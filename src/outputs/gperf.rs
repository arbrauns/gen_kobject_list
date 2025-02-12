use std::{collections::BTreeMap, io::Write, iter::repeat_n, ops::Range};

use color_eyre::{
    eyre::{Context as _, OptionExt as _},
    Result,
};
use tracing::instrument;

use crate::{types::KobjectInstance, AnalyzerCounters, FileMetadata};

const HEADER: &str = "%compare-lengths
%define lookup-function-name z_object_lookup
%language=ANSI-C
%global-table
%struct-type
%{
#include <zephyr/kernel.h>
#include <zephyr/toolchain.h>
#include <zephyr/internal/syscall_handler.h>
#include <string.h>
%}
struct k_object;
";

/// Different versions of gperf have different prototypes for the lookup
/// function, best to implement the wrapper here. The pointer value itself is
/// turned into a string, we told gperf to expect binary strings that are not
/// NULL-terminated.
const FOOTER: &str = "%%
struct k_object *z_object_gperf_find(const void *obj)
{
    return z_object_lookup((const char *)obj, sizeof(void *));
}

void z_object_gperf_wordlist_foreach(_wordlist_cb_func_t func, void *context)
{
    int i;

    for (i = MIN_HASH_VALUE; i <= MAX_HASH_VALUE; i++) {
        if (wordlist[i].name != NULL) {
            func(&wordlist[i], context);
        }
    }
}

#ifndef CONFIG_DYNAMIC_OBJECTS
struct k_object *k_object_find(const void *obj)
	ALIAS_OF(z_object_gperf_find);

void k_object_wordlist_foreach(_wordlist_cb_func_t func, void *context)
	ALIAS_OF(z_object_gperf_wordlist_foreach);
#endif
";

#[instrument(skip_all)]
pub fn write_gperf_table(
    mut outfile: impl Write,
    meta: &FileMetadata,
    mut objs: BTreeMap<u64, KobjectInstance>,
    counters: AnalyzerCounters,
    static_kernel_objects: Range<u64>,
) -> Result<()> {
    write!(outfile, "{}", HEADER)?;
    if counters.sys_mutexes != 0 {
        writeln!(
            outfile,
            "static struct k_mutex kernel_mutexes[{}] = {{",
            counters.sys_mutexes
        )?;
        for i in 0..counters.sys_mutexes {
            if i != 0 {
                write!(outfile, ", ")?;
            }
            write!(outfile, "Z_MUTEX_INITIALIZER(kernel_mutexes[{i}])")?;
        }
        writeln!(outfile, "}};")?;
    }

    if counters.futexes != 0 {
        writeln!(
            outfile,
            "static struct z_futex_data futex_data[{}] = {{",
            counters.futexes
        )?;
        for i in 0..counters.sys_mutexes {
            if i != 0 {
                write!(outfile, ", ")?;
            }
            write!(outfile, "Z_FUTEX_DATA_INITIALIZER(futex_data[{i}])")?;
        }
        writeln!(outfile, "}};")?;
    }

    fn get_metadata_field(gen_priv_stacks: bool, ko: &KobjectInstance) -> (&str, String) {
        let data = ko.data;

        match ko.type_name.as_deref() {
            Some("K_OBJ_THREAD_STACK_ELEMENT") => {
                if gen_priv_stacks {
                    ("stack_data", format!("&stack_data[{data}]"))
                } else {
                    ("stack_size", data.to_string())
                }
            }
            Some("K_OBJ_THREAD") => ("thread_id", data.to_string()),
            Some("K_OBJ_SYS_MUTEX") => ("mutex", format!("&kernel_mutexes[{data}]")),
            Some("K_OBJ_FUTEX") => ("futex_data", format!("&futex_data[{data}]")),
            _ => ("unused", "0".to_owned()),
        }
    }

    let gen_priv_stacks = meta.syms.contains_key("CONFIG_GEN_PRIV_STACKS");
    if gen_priv_stacks && counters.stacks != 0 {
        // Same as K_KERNEL_STACK_ARRAY_DEFINE, but routed to a different memory section.
        writeln!(
            outfile,
            "static uint8_t Z_GENERIC_SECTION(.priv_stacks.noinit)  \
                  __aligned(Z_KERNEL_STACK_OBJ_ALIGN) \
                  priv_stacks[{}][K_KERNEL_STACK_LEN(CONFIG_PRIVILEGED_STACK_SIZE)];",
            counters.stacks
        )?;

        writeln!(
            outfile,
            "static const struct z_stack_data stack_data[{}] = {{",
            counters.stacks
        )?;

        for (i, ko) in objs
            .values_mut()
            .filter(|ko| ko.type_name.as_deref() == Some("K_OBJ_THREAD_STACK_ELEMENT"))
            .enumerate()
        {
            // ko.data currently has the stack size. fetch the value to populate the appropriate
            // entry in stack_data, and put a reference to the entry in stack_data into the data
            // value instead
            let size = ko.data;
            ko.data = i;
            writeln!(outfile, "\t{{ {size}, (uint8_t *)(&priv_stacks[{i}]) }},")?;
        }

        writeln!(outfile, "}};")?;
    }

    writeln!(outfile, "%%")?;

    // Setup variables for mapping thread indexes
    let mut thread_idx_map: Vec<_> = repeat_n(true, meta.max_threads).collect();

    for (obj_addr, ko) in objs {
        let obj_type = ko
            .type_name
            .as_deref()
            .ok_or_eyre("Object did not get type name assigned")?;
        // pre-initialized objects fall within this memory range, they are either completely
        // initialized at build time, or done automatically at boot during some `PRE_KERNEL_*` phase
        let initialized = static_kernel_objects.contains(&obj_addr);
        let is_driver = obj_type.starts_with("K_OBJ_DRIVER_");

        let mut write_addr = |bytes: &[u8]| -> Result<()> {
            write!(outfile, "\"")?;
            for b in bytes {
                write!(outfile, "\\x{b:02x}")?;
            }
            write!(outfile, "\", ")?;
            Ok(())
        };

        if meta.is_64bit {
            if meta.is_little_endian {
                write_addr(&obj_addr.to_le_bytes())?;
            } else {
                write_addr(&obj_addr.to_be_bytes())?;
            }
        } else {
            let obj_addr = u32::try_from(obj_addr)
                .wrap_err("object address does not fit in environment's 32-bit addresses")?;
            if meta.is_little_endian {
                write_addr(&obj_addr.to_le_bytes())?;
            } else {
                write_addr(&obj_addr.to_be_bytes())?;
            }
        }

        let mut flags = "0".to_owned();

        if initialized {
            flags.push_str(" | K_OBJ_FLAG_INITIALIZED");
        }
        if is_driver {
            flags.push_str(" | K_OBJ_FLAG_DRIVER");
        }

        let (md_field, md_value) = get_metadata_field(gen_priv_stacks, &ko);

        writeln!(
            outfile,
            "{{0}}, {obj_type}, {flags}, {{ .{md_field} = {md_value} }}",
        )?;

        if obj_type == "K_OBJ_THREAD" {
            thread_idx_map[ko.data] = false;
        }
    }

    write!(outfile, "{}", FOOTER)?;

    // Generate the array of already mapped thread indexes
    writeln!(outfile)?;
    writeln!(outfile, "Z_GENERIC_DOT_SECTION(data)")?;
    write!(
        outfile,
        "uint8_t _thread_idx_map[{}] = {{",
        meta.max_threads / 8
    )?;
    for bits in thread_idx_map.chunks(8) {
        let byte: u8 = bits
            .iter()
            .enumerate()
            .map(|(i, b)| u8::from(*b) << i)
            .sum();
        write!(outfile, " 0x{byte:x}, ")?;
    }
    writeln!(outfile, "}};")?;

    Ok(())
}
