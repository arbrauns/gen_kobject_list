use std::{
    borrow::Cow,
    collections::HashMap,
    fs::OpenOptions,
    io::{self, BufWriter},
    ops::Range,
    path::{Path, PathBuf},
};

use analyze::{find_kobjects, FileMetadata};
use clap::Parser;
use color_eyre::{
    eyre::{bail, eyre, Context, OptionExt},
    Result,
};
use fallible_iterator::{FallibleIterator, IteratorExt};
use hifijson::token::Lex as _;
use object::{Object as _, ObjectSymbol as _};
use phf::phf_ordered_map;
use tracing::{instrument, warn};
use tracing_subscriber::{layer::SubscriberExt as _, util::SubscriberInitExt as _};

use crate::outputs::{
    write_gperf_table, write_kobj_otype_output, write_kobj_size_output, write_kobj_types_output,
    write_validation_output,
};

mod analyze;
mod outputs;

pub type Symbols<'name> = HashMap<&'name str, u64>;

pub struct Kobject {
    /// The name of a Kconfig that indicates the presence of this object's definition in
    /// case it is not available in all configurations.
    pub kconfig: Option<&'static str>,
    /// Whether it is permissible for the object to be located in user-accessible memory.
    pub user_accessible: bool,
    /// Whether this item can be dynamically allocated with `k_object_alloc()`. Keep this in sync
    /// with the switch statement in `z_impl_k_object_alloc()`.
    pub dyn_allocatable: bool,
}
macro_rules! kobject {
    (None, $user_accessible:expr, $dyn_allocatable:expr) => {
        Kobject {
            kconfig: None,
            user_accessible: $user_accessible,
            dyn_allocatable: $dyn_allocatable,
        }
    };
    ($kconfig:expr, $user_accessible:expr, $dyn_allocatable:expr) => {
        Kobject {
            kconfig: Some($kconfig),
            user_accessible: $user_accessible,
            dyn_allocatable: $dyn_allocatable,
        }
    };
}
/// Keys in this map are structs which should be recognized as kernel objects.
/// Key names in all caps do not correspond to a specific data type but instead indicate that
/// objects of its type are of a family of compatible data structures.
pub const KOBJECTS: phf::OrderedMap<&str, Kobject> = phf_ordered_map! {
    "k_mem_slab" => kobject!(None, false, true),
    "k_msgq" => kobject!(None, false, true),
    "k_mutex" => kobject!(None, false, true),
    "k_pipe" => kobject!(None, false, true),
    "k_queue" => kobject!(None, false, true),
    "k_poll_signal" => kobject!(None, false, true),
    "k_sem" => kobject!(None, false, true),
    "k_stack" => kobject!(None, false, true),
    "k_thread" => kobject!(None, false, true),
    "k_timer" => kobject!(None, false, true),
    "z_thread_stack_element" => kobject!(None, false, false),
    "device" => kobject!(None, false, false),
    "NET_SOCKET" => kobject!(None, false, false),
    "net_if" => kobject!(None, false, false),
    "sys_mutex" => kobject!(None, true, false),
    "k_futex" => kobject!(None, true, false),
    "k_condvar" => kobject!(None, false, true),
    "k_event" => kobject!("CONFIG_EVENTS", false, true),
    "ztest_suite_node" => kobject!("CONFIG_ZTEST", true, false),
    "ztest_suite_stats" => kobject!("CONFIG_ZTEST", true, false),
    "ztest_unit_test" => kobject!("CONFIG_ZTEST", true, false),
    "ztest_test_rule" => kobject!("CONFIG_ZTEST", true, false),
    "rtio" => kobject!("CONFIG_RTIO", false, false),
    "rtio_iodev" => kobject!("CONFIG_RTIO", false, false),
    "sensor_decoder_api" => kobject!("CONFIG_SENSOR_ASYNC_API", true, false),
};

#[derive(Debug, Clone)]
pub struct KobjectInstance {
    pub name: String,
    pub api: bool,
    pub type_name: Option<String>,
    /// A numeric data field used during output generation. Semantics depend on the object type:
    /// - For `k_thread`, the thread ID
    /// - For `sys_mutex`, the mutex ID
    /// - For `k_futex`, the futex ID
    /// - For `k_thread_stack_element`, initially the stack size, and if `CONFIG_GEN_PRIV_STACKS`
    ///   is enabled, later the stack object ID
    pub data: usize,
}

impl KobjectInstance {
    pub fn new(name: String, api: bool) -> Self {
        Self {
            name,
            api,
            // Type name determined later since drivers needs to look at the API struct address
            type_name: None,
            data: 0,
        }
    }
}

fn kobject_to_enum(kobj: &str) -> String {
    format!(
        "K_OBJ_{}",
        kobj.trim_start_matches("k_")
            .trim_start_matches("z_")
            .to_uppercase()
    )
}

fn subsystem_to_enum(subsystem: &str) -> String {
    let subsys = subsystem
        .strip_suffix("_driver_api")
        .expect("should have _driver_api suffix")
        .to_uppercase();
    format!("K_OBJ_DRIVER_{subsys}")
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct StructTags {
    subsystems: Vec<String>,
    net_sockets: Vec<String>,
}

impl Extend<StructTags> for StructTags {
    fn extend<T: IntoIterator<Item = StructTags>>(&mut self, iter: T) {
        for StructTags {
            subsystems,
            net_sockets,
        } in iter
        {
            self.subsystems.extend(subsystems);
            self.net_sockets.extend(net_sockets);
        }
    }
}

impl FromIterator<StructTags> for StructTags {
    fn from_iter<T: IntoIterator<Item = StructTags>>(iter: T) -> Self {
        let mut ret = Self::default();
        ret.extend(iter);
        ret
    }
}

fn parse_subsystems_list_file(path: &Path) -> Result<StructTags> {
    use hifijson::value::Value;

    let content = std::fs::read(path).wrap_err("Failed to read file")?;

    let mut lexer = hifijson::SliceLexer::new(&content);

    let value = lexer
        .exactly_one(hifijson::value::parse_unbounded)
        .wrap_err("Failed to parse JSON")?;

    let Value::Object(o) = value else {
        bail!("Expected object at top level");
    };

    let mut subsystems: Option<Vec<String>> = None;
    let mut net_sockets: Option<Vec<String>> = None;

    for (k, v) in o {
        fn handle_field<T>(
            field: &mut Option<Vec<T>>,
            key: &str,
            arr: Value<&str, Cow<str>>,
        ) -> Result<()>
        where
            T: TryFrom<String>,
            color_eyre::Report: From<<T as TryFrom<String>>::Error>,
        {
            if field.is_some() {
                bail!("Duplicate key {key}");
            }

            let Value::Array(arr) = arr else {
                bail!("Expected array for key {key}");
            };

            let items = arr
                .into_iter()
                .map(|v| {
                    let Value::String(s) = v else {
                        bail!("Expected string value, got {v}");
                    };
                    Ok(s.into_owned().try_into()?)
                })
                .collect::<Result<Vec<T>, _>>()?;

            *field = Some(items);

            Ok(())
        }

        match k.as_ref() {
            "__subsystem" => handle_field(&mut subsystems, &k, v)?,
            "__net_socket" => handle_field(&mut net_sockets, &k, v)?,
            _ => {
                bail!("Unknown key {k}")
            }
        }
    }

    let subsystems = subsystems.ok_or_eyre("Missing __subsystem field")?;
    for value in &subsystems {
        if !value.ends_with("_driver_api") {
            bail!("__subsystem is missing _driver_api suffix: {value}")
        }
    }
    let net_sockets = net_sockets.ok_or_eyre("Missing __subsystem field")?;

    Ok(StructTags {
        subsystems,
        net_sockets,
    })
}

#[instrument(skip(syms))]
fn get_symbol(syms: &Symbols, symbol: &str) -> Result<u64> {
    syms.get(symbol)
        .copied()
        .ok_or_else(|| eyre!("Missing {symbol} symbol"))
}

#[instrument(skip(syms))]
fn get_symbol_range(syms: &Symbols, prefix: &str) -> Result<Range<u64>> {
    let start = get_symbol(syms, &format!("{prefix}_start"))
        .or_else(|_e| get_symbol(syms, &format!("{prefix}_begin")))
        .map_err(|_e| eyre!("No _start/_begin symbol found for {prefix}"))?;
    let end = get_symbol(syms, &format!("{prefix}_end"))?;
    Ok(start..end)
}

#[derive(Parser)]
struct Args {
    /// Print extra debugging information
    #[arg(short, long)]
    verbose: bool,

    /// Input zephyr ELF binary
    #[arg(short, long)]
    kernel: Option<PathBuf>,
    /// Specifies a file with a JSON encoded list of subsystem names to append to the driver
    /// subsystems list. Can be specified multiple times: -i file1 -i file2 ...
    #[arg(short, long)]
    include_subsystem_list: Vec<PathBuf>,

    /// Output list of kernel object addresses for gperf use
    #[arg(short, long, requires = "kernel")]
    gperf_output: Option<PathBuf>,
    /// Output driver validation macros
    #[arg(short = 'V', long)]
    validation_output: Option<PathBuf>,
    /// Output k_object enum constants
    #[arg(short = 'K', long)]
    kobj_types_output: Option<PathBuf>,
    /// Output case statements for otype_to_str()
    #[arg(short = 'S', long)]
    kobj_otype_output: Option<PathBuf>,
    /// Output case statements for obj_size_get()
    #[arg(short = 'Z', long)]
    kobj_size_output: Option<PathBuf>,
}

fn open_outfile(outfile: PathBuf) -> Result<BufWriter<std::fs::File>, color_eyre::eyre::Error> {
    Ok(BufWriter::new(
        OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open(outfile)?,
    ))
}

pub fn main() -> Result<()> {
    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer().with_writer(io::stderr))
        .with(tracing_subscriber::EnvFilter::from_default_env())
        .with(tracing_error::ErrorLayer::default())
        .init();

    color_eyre::install()?;

    let args = Args::parse();

    let struct_tags: StructTags = args
        .include_subsystem_list
        .iter()
        .into_fallible()
        .map_err(|e| match e {})
        .map(|list_file| {
            parse_subsystems_list_file(list_file).wrap_err_with(|| {
                format!(
                    "Failed to load subsystem list file {}",
                    list_file.to_string_lossy()
                )
            })
        })
        .collect::<StructTags>()?;

    if let Some(outfile) = args.gperf_output {
        let kernel_path = args.kernel.expect("--gperf-output should require --kernel");
        let kernel_bytes = std::fs::read(&kernel_path).wrap_err_with(|| {
            format!(
                "Failed to read kernel image from {}",
                kernel_path.to_string_lossy()
            )
        })?;
        let elf = object::File::parse(&*kernel_bytes).wrap_err("Failed to parse kernel ELF")?;
        let syms = elf
            .symbols()
            .map(|sym| Ok((sym.name()?, sym.address())))
            .collect::<Result<HashMap<_, _>>>()
            .wrap_err("Failed to read symbols from kernel ELF")?;

        let meta = FileMetadata {
            max_threads: usize::try_from(8 * get_symbol(&syms, "CONFIG_MAX_THREAD_BYTES")?)
                .wrap_err("CONFIG_MAX_THREAD_BYTES should fit in usize")?,
            is_64bit: syms.contains_key("CONFIG_64BIT"),
            is_little_endian: elf.is_little_endian(),
            syms,
        };
        assert_eq!(elf.is_64(), meta.is_64bit);

        let (objs, counters) = find_kobjects(&elf, &meta, &struct_tags)?;

        write_gperf_table(
            open_outfile(outfile)?,
            &meta,
            objs,
            counters,
            get_symbol_range(&meta.syms, "_static_kernel_objects")?,
        )?;
    }

    if let Some(outfile) = args.validation_output {
        write_validation_output(open_outfile(outfile)?, &struct_tags)?;
    }

    if let Some(outfile) = args.kobj_types_output {
        write_kobj_types_output(open_outfile(outfile)?, &struct_tags)?;
    }

    if let Some(outfile) = args.kobj_otype_output {
        write_kobj_otype_output(open_outfile(outfile)?, &struct_tags)?;
    }

    if let Some(outfile) = args.kobj_size_output {
        write_kobj_size_output(open_outfile(outfile)?)?;
    }

    Ok(())
}
