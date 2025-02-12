use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap},
    fmt::Write as _,
    fs::OpenOptions,
    io::{self, BufWriter},
    num::NonZeroUsize,
    ops::Range,
    path::{Path, PathBuf},
};

use clap::Parser;
use color_eyre::{
    eyre::{bail, eyre, Context, OptionExt},
    Result, Section as _,
};
use fallible_iterator::{FallibleIterator, IteratorExt};
use gimli::{
    constants::{
        self, DW_AT_byte_size, DW_AT_count, DW_AT_data_member_location, DW_AT_decl_file,
        DW_AT_decl_line, DW_AT_location, DW_AT_upper_bound, DW_TAG_member, DW_TAG_subrange_type,
    },
    Dwarf, Operation, Unit,
};
use hifijson::token::Lex as _;
use object::{Object as _, ObjectSection as _, ObjectSymbol as _};
use tracing::{debug, info, instrument, trace, warn};
use tracing_subscriber::{layer::SubscriberExt as _, util::SubscriberInitExt as _};

use crate::{
    consts::{KOBJECTS, STACK_TYPE},
    outputs::{
        write_gperf_table, write_kobj_otype_output, write_kobj_size_output,
        write_kobj_types_output, write_validation_output,
    },
    types::{
        AggregateMember, Die, DieRef, KobjectInstance, MemoryRanges, OwnedDieRef, Reader, Symbols,
        Type, TypeEnv,
    },
};

mod consts;
mod outputs;
mod types;

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

fn get_die_location(dwarf: &Dwarf<Reader<'_>>, die: DieRef<'_, '_, '_>) -> Result<String> {
    let decl_file: usize = die
        .attr(DW_AT_decl_file)?
        .ok_or_eyre("Missing DW_AT_decl_file attribute")?
        .udata_value()
        .ok_or_eyre("Unsupported form for DW_AT_decl_file attribute")?
        .try_into()?;
    let line_program = die
        .unit
        .line_program
        .as_ref()
        .ok_or_eyre("Unit has no line program")?;

    let header = line_program.header();
    let fileinfo = header
        .file_names()
        .get(decl_file - 1)
        .ok_or_eyre("Invalid file name index")?;
    let filename = dwarf
        .attr_string(die.unit, fileinfo.path_name())?
        .to_string_lossy();
    let filedir = dwarf
        .attr_string(
            die.unit,
            fileinfo
                .directory(header)
                .ok_or_eyre("Invalid directory index")?,
        )?
        .to_string_lossy();

    let path = format!("{filedir}/{filename}");
    let lineno = die
        .attr_value(DW_AT_decl_line)?
        .ok_or_eyre("Missing DW_AT_decl_line attribute")?
        .udata_value()
        .ok_or_eyre("Unsupported form for DW_AT_decl_line attribute")?;

    Ok(format!("File {}, line {}", path, lineno))
}

fn debug_die<'input>(dwarf: &Dwarf<Reader<'input>>, die: DieRef<'_, 'input, '_>) -> String {
    let mut s = String::new();
    match get_die_location(dwarf, die) {
        Ok(loc) => writeln!(&mut s, "{loc}").unwrap(),
        Err(err) => writeln!(&mut s, "[failed to get DIE location: {err}]").unwrap(),
    }

    let name = match die.get_name(dwarf) {
        Ok(Some(s)) => s,
        Ok(None) => "[no name]",
        Err(_) => "[error getting name]",
    };
    writeln!(
        &mut s,
        r#"DIE "{name}" ({}), has children: {}"#,
        &die.tag(),
        die.has_children()
    )
    .unwrap();

    let mut attrs = die.attrs();
    loop {
        match attrs.next() {
            Ok(Some(attr)) => {
                writeln!(&mut s, "   - {:-20}={:?}", attr.name(), attr.raw_value()).unwrap()
            }
            Ok(None) => break,
            Err(err) => writeln!(&mut s, "  - [failed to parse attributes: {err}]").unwrap(),
        }
    }

    s
}

#[derive(Debug, Default)]
struct AnalyzerCounters {
    threads: usize,
    sys_mutexes: usize,
    futexes: usize,
    stacks: usize,
}

#[instrument(skip_all)]
fn analyze_units<'input>(
    elf: &object::File,
    dwarf: &Dwarf<Reader<'input>>,
    units: &[Unit<Reader<'input>>],
    struct_tags: &StructTags,
    ranges: MemoryRanges,
) -> Result<(BTreeMap<u64, KobjectInstance>, AnalyzerCounters)> {
    let mut type_env = TypeEnv::default();

    info!("Collecting Debugging Information Entries");

    // Step 1: collect all type information.
    let mut variables = Vec::new();
    for unit in units {
        let unit_analyzer = UnitAnalyzer {
            dwarf,
            struct_tags,
            unit,
            type_env: &mut type_env,
        };
        unit_analyzer.analyze_unit(&mut variables)?;
    }

    // Step 2: filter type_env to only contain kernel objects, or structs and arrays of kernel
    // objects
    info!(
        num_variables = variables.len(),
        num_types = type_env.len(),
        "Done collecting DIEs, starting post-processing"
    );
    type_env.remove_non_kobjects();
    info!(num_types = type_env.len(), "Pruned types to kobjects");

    /*
    {
        let mut types: Vec<_> = type_env.inner.clone().into_iter().collect();
        types.sort_by_key(|(off, _)| *off);
        for (off, typ) in types {
            println!("{}: {typ}", off.as_debug_info_offset().unwrap().0);
        }
    }
    */

    // Step 3: Now that we know all the types we are looking for, examine all variables
    let mut all_objs = BTreeMap::new();
    for die in variables {
        let Some(name) = die.get_name(dwarf)? else {
            continue;
        };

        if name.starts_with("__init_sys_init") {
            // Boot-time initialization function; not an actual device
            continue;
        }

        let typ = die.get_type()?.ok_or_eyre("Missing DW_AT_type attribute")?;

        // Is this a kernel object, or a structure containing kernel
        // objects?
        let Some(type_obj) = type_env.get(typ.as_die_ref()) else {
            continue;
        };

        let Some(loc) = die.attr_value(DW_AT_location)? else {
            debug!(
                name,
                die = debug_die(dwarf, die.as_die_ref()),
                "No location information for object; possibly stack allocated"
            );
            continue;
        };
        let Some(locval) = loc.exprloc_value() else {
            debug!(
                name,
                die = debug_die(dwarf, die.as_die_ref()),
                attr = ?loc,
                "kernel object unexpected location format"
            );
            continue;
        };
        let mut loc_ops = locval.operations(die.unit.header.encoding());
        let expr = loc_ops.next()?;
        let Some(Operation::Address { mut address }) = expr else {
            // Check if frame pointer offset DW_OP_fbreg
            if let Some(Operation::FrameOffset { .. }) = expr {
                debug!(
                    name,
                    die = debug_die(dwarf, die.as_die_ref()),
                    "kernel object found on stack"
                );
            } else {
                debug!(
                    name,
                    die = debug_die(dwarf, die.as_die_ref()),
                    ?expr,
                    "kernel object has unexpected exprloc opcode"
                );
            }
            continue;
        };
        if let Some(Operation::PlusConstant { value }) = loc_ops.next()? {
            // Handle a DW_FORM_exprloc that contains a DW_OP_addr, followed immediately by a
            // DW_OP_plus_uconst.
            address += value;
        }
        if address == 0 {
            // Never linked; gc-sections deleted it
            continue;
        }

        let mut num_objs = 0;
        let objs = type_obj
            .get_kobjects(&type_env, address)
            .inspect(|_| num_objs += 1);
        all_objs.extend(objs);

        debug!(name, address, num_objects = num_objs, "processed variable");
    }

    // Step 4: all_objs is a dictionary mapping variable memory addresses to their associated
    // type objects. Now that we have seen all variables and can properly look up API structs,
    // convert this into a dictionary mapping variables to the C enumeration of what kernel
    // object type it is.

    let mut counters = AnalyzerCounters::default();
    // BTreeMap so it's sorted (by address)
    let mut ret = BTreeMap::new();
    for (&addr, ko) in &all_objs {
        if ko.api {
            // API structs don't get into the gperf table
            continue;
        }

        let user_ram_allowed = KOBJECTS
            .get(&ko.name)
            .ok_or_else(|| eyre!("Unknown kobject name {}", &ko.name))?
            .user_accessible;

        if !user_ram_allowed
            && (ranges.app_smem.contains(&addr) || ranges.app_smem_pinned.contains(&addr))
        {
            warn!(name = ko.name, addr, "object found in invalid location");
            continue;
        }

        if ko.name == STACK_TYPE && !ranges.user_stack.contains(&addr) {
            debug!(addr, "skip kernel-only stack");
            continue;
        }

        let mut ko = ko.clone();

        // At this point we know the object will be included in the gperf table
        match ko.name.as_str() {
            "k_thread" => {
                // Assign an ID for this thread object, used to track its
                // permissions to other kernel objects
                ko.data = counters.threads;
                counters.threads += 1;
            }
            "sys_mutex" => {
                ko.data = counters.sys_mutexes;
                counters.sys_mutexes += 1;
            }
            "k_futex" => {
                ko.data = counters.futexes;
                counters.futexes += 1;
            }
            STACK_TYPE => {
                counters.stacks += 1;
            }
            _ => {}
        }

        if ko.name != "device" {
            // Not a device struct so we immediately know its type
            ko.type_name = Some(kobject_to_enum(&ko.name));
            ret.insert(addr, ko);
            continue;
        }

        // Device struct. Need to get the address of its API struct, if it has one.
        let apiaddr = device_get_api_addr(elf, addr)?;
        let Some(apiobj) = all_objs.get(&apiaddr) else {
            if apiaddr == 0 {
                warn!(addr, "device instance has no associated subsystem");
            } else {
                warn!(addr, apiaddr, "device instance has unknown API");
            }
            // API struct does not correspond to a known subsystem, skip it
            continue;
        };

        ko.type_name = Some(subsystem_to_enum(&apiobj.name));
        ret.insert(addr, ko);
    }

    debug!(num_objs = ret.len(), "resolved kernel objects");

    Ok((ret, counters))
}

#[derive(Debug)]
enum DieResult<'input, 'unit> {
    Variable,
    Type(Type<'input, 'unit>),
    None,
}

struct UnitAnalyzer<'parent, 'input, 'unit> {
    dwarf: &'parent Dwarf<Reader<'input>>,
    struct_tags: &'parent StructTags,
    unit: &'unit Unit<Reader<'input>>,
    type_env: &'parent mut TypeEnv<'input, 'unit>,
}

impl<'input, 'unit> UnitAnalyzer<'_, 'input, 'unit> {
    #[instrument(
        skip(self, variables),
        fields(
            unit = ?self.unit.name.map(|b| String::from_utf8_lossy(b.as_ref()).into_owned()),
        ),
    )]
    pub fn analyze_unit(mut self, variables: &mut Vec<OwnedDieRef<'input, 'unit>>) -> Result<()> {
        let mut dies = self.unit.entries();
        while let Some((_depth_delta, die)) = dies.next_dfs()? {
            match self.analyze_die(die)? {
                DieResult::Variable => variables.push(self.owned_die_ref(die.clone())),
                DieResult::Type(typ) => self.type_env.insert(self.die_ref(die), typ),
                DieResult::None => {}
            }
        }
        Ok(())
    }

    #[instrument(
        level = "DEBUG",
        skip(self, die),
        fields(
            die = debug_die(self.dwarf, self.die_ref(die)),
        ),
    )]
    fn analyze_die(&mut self, die: &Die<'input, 'unit>) -> Result<DieResult<'input, 'unit>> {
        trace!("Analyzing DIE");
        // Unions are disregarded, kernel objects should never be union
        // members since the memory is not dedicated to that object and
        // could be something else
        let typ = match die.tag() {
            constants::DW_TAG_structure_type => self.analyze_die_struct(die)?,
            constants::DW_TAG_const_type => self.analyze_die_const(die)?,
            constants::DW_TAG_array_type => self.analyze_die_array(die)?,
            constants::DW_TAG_typedef => self.analyze_die_typedef(die)?,
            constants::DW_TAG_variable => {
                return Ok(DieResult::Variable);
            }
            _ => return Ok(DieResult::None),
        };

        if let Some(typ) = typ {
            Ok(DieResult::Type(typ))
        } else {
            Ok(DieResult::None)
        }
    }

    fn die_ref<'die>(&self, die: &'die Die<'input, 'unit>) -> DieRef<'die, 'input, 'unit> {
        DieRef {
            die,
            unit: self.unit,
        }
    }

    fn owned_die_ref(&self, die: Die<'input, 'unit>) -> OwnedDieRef<'input, 'unit> {
        OwnedDieRef {
            die,
            unit: self.unit,
        }
    }

    fn get_die_name(&self, die: &Die<'input, 'unit>) -> Result<Option<&'input str>> {
        self.die_ref(die).get_name(self.dwarf)
    }

    fn get_die_type(&self, die: &Die<'input, 'unit>) -> Result<Option<Die<'input, 'unit>>> {
        self.die_ref(die).get_type()
    }

    fn analyze_die_struct(
        &mut self,
        die: &Die<'input, 'unit>,
    ) -> Result<Option<Type<'input, 'unit>>> {
        let name = self.get_die_name(die)?.unwrap_or("<anon>");
        let Some(size_attr) = die.attr_value(DW_AT_byte_size)? else {
            // Incomplete type
            return Ok(None);
        };

        let Some(size) = size_attr.udata_value() else {
            bail!("Unsupported DW_AT_byte_size form: {size_attr:?}");
        };
        let size = usize::try_from(size).wrap_err("Struct size should fit in usize")?;
        let Some(size) = NonZeroUsize::new(size) else {
            // Incomplete type
            return Ok(None);
        };

        let typ = if KOBJECTS.contains_key(name) {
            Type::Kobject {
                name: name.to_owned(),
                size,
                api: false,
            }
        } else if self.struct_tags.subsystems.iter().any(|x| x == name) {
            Type::Kobject {
                name: name.to_owned(),
                size,
                api: true,
            }
        } else if self.struct_tags.net_sockets.iter().any(|x| x == name) {
            Type::Kobject {
                name: "NET_SOCKET".to_owned(),
                size,
                api: false,
            }
        } else {
            let mut members = Vec::new();
            let mut tree = self.unit.entries_tree(Some(die.offset()))?;
            let mut children = tree.root()?.children();
            while let Some(child_node) = children.next()? {
                let child = child_node.entry();
                if child.tag() != DW_TAG_member {
                    continue;
                }

                let Some(data_member_location) = child.attr_value(DW_AT_data_member_location)?
                else {
                    continue;
                };

                // FIXME DW_OP_plus_uconst in member offset
                let Some(data_member_location) = data_member_location.udata_value() else {
                    bail!("Unsupported DW_AT_data_member_location form: {data_member_location:?}");
                };

                let member_type = self
                    .get_die_type(child)?
                    .ok_or_eyre("Missing DW_AT_type attribute")?;
                members.push(AggregateMember {
                    name: self.get_die_name(child)?.unwrap_or("<anon>").to_owned(),
                    typ: self.owned_die_ref(member_type),
                    offset: data_member_location,
                })
            }

            Type::Aggregate {
                name: name.to_owned(),
                size,
                members,
            }
        };

        Ok(Some(typ))
    }

    fn analyze_die_const(
        &mut self,
        die: &Die<'input, 'unit>,
    ) -> Result<Option<Type<'input, 'unit>>> {
        let Some(typ) = self.get_die_type(die)? else {
            debug!("Const type DIE without child type");
            return Ok(None);
        };
        Ok(Some(Type::Const {
            child_type: self.owned_die_ref(typ),
        }))
    }

    fn analyze_die_array(
        &mut self,
        die: &Die<'input, 'unit>,
    ) -> Result<Option<Type<'input, 'unit>>> {
        let typ = self
            .get_die_type(die)?
            .ok_or_eyre("Missing DW_AT_type attribute")?;

        let mut elements = Vec::new();
        let mut tree = self.unit.entries_tree(Some(die.offset()))?;
        let mut children = tree.root()?.children();
        while let Some(child_node) = children.next()? {
            let child = child_node.entry();
            if child.tag() != DW_TAG_subrange_type {
                continue;
            }

            let ub = if let Some(ub) = child.attr_value(DW_AT_upper_bound)? {
                ub
            } else if let Some(count) = child.attr_value(DW_AT_count)? {
                // in DWARF 4, e.g. ARC Metaware toolchain, DW_AT_count is used
                // not DW_AT_upper_bound
                count
            } else {
                continue;
            };

            let Some(value) = ub.udata_value() else {
                //warn!(attribute_value = ?ub, "Unexpected form for DW_AT_upper_bound/DW_AT_count");
                continue;
            };

            let value = usize::try_from(value).wrap_err("Failed to fit array length into usize")?;
            elements.push(value + 1);
        }

        if elements.is_empty() {
            let Some(mt) = self.type_env.get(self.die_ref(&typ)) else {
                debug!(
                    typ = debug_die(self.dwarf, self.die_ref(&typ)),
                    "member type not in type env"
                );
                return Ok(None);
            };
            let Type::Kobject { name, .. } = mt else {
                return Ok(None);
            };
            if name != STACK_TYPE {
                return Ok(None);
            }

            elements.push(1);
            Ok(Some(Type::Array {
                elements,
                member_type: self.owned_die_ref(typ),
            }))
        } else {
            Ok(Some(Type::Array {
                elements,
                member_type: self.owned_die_ref(typ),
            }))
        }
    }

    fn analyze_die_typedef(
        &mut self,
        die: &Die<'input, 'unit>,
    ) -> Result<Option<Type<'input, 'unit>>> {
        let Some(typ) = self.get_die_type(die)? else {
            debug!("Typedef DIE without target type");
            return Ok(None);
        };
        let Some(typ) = self.type_env.get(self.die_ref(&typ)) else {
            return Ok(None);
        };
        Ok(Some(typ.clone()))
    }
}

#[instrument(skip(elf))]
fn device_get_api_addr(elf: &object::File, addr: u64) -> Result<u64> {
    // See include/device.h for a description of struct device
    let api_member_offset = if elf.is_64() { 16 } else { 8 };
    let addr = addr + api_member_offset;

    for sect in elf.sections() {
        let start = sect.address();
        let end = start + sect.size();
        if start <= addr && addr < end {
            let offset = usize::try_from(addr - start).expect("u64 should fit in usize");
            let data = sect.uncompressed_data()?;
            return Ok(if elf.is_64() {
                (if elf.is_little_endian() {
                    u64::from_le_bytes
                } else {
                    u64::from_be_bytes
                })(data[offset..offset + 8].try_into().unwrap())
            } else {
                (if elf.is_little_endian() {
                    u32::from_le_bytes
                } else {
                    u32::from_be_bytes
                })(data[offset..offset + 4].try_into().unwrap())
                .into()
            });
        }
    }

    bail!("device struct at address 0x{addr:016x} not found in ELF")
}

struct FileMetadata<'input> {
    syms: Symbols<'input>,
    max_threads: usize,
    is_64bit: bool,
    is_little_endian: bool,
}

#[instrument(skip(elf, meta, struct_tags))]
fn find_kobjects<'input>(
    elf: &object::File<'input>,
    meta: &FileMetadata<'input>,
    struct_tags: &StructTags,
) -> Result<(BTreeMap<u64, KobjectInstance>, AnalyzerCounters)> {
    let ranges = MemoryRanges::from_symbols(&meta.syms)?;

    let dwarf_sections = gimli::DwarfSections::load(|id| {
        elf.section_by_name(id.name())
            .map(|section| section.uncompressed_data())
            .unwrap_or(Ok(Cow::Borrowed([].as_slice())))
    })
    .wrap_err("Failed to load DWARF sections")?;

    let endian = if meta.is_little_endian {
        gimli::RunTimeEndian::Little
    } else {
        gimli::RunTimeEndian::Big
    };

    let dwarf = dwarf_sections.borrow(|section| gimli::EndianSlice::new(section.as_ref(), endian));
    let units: Vec<_> = dwarf
        .units()
        .map(|unit_header| dwarf.unit(unit_header))
        .collect()?;
    for unit in &units {
        assert_eq!(
            unit.header.encoding().address_size,
            if meta.is_64bit { 8 } else { 4 }
        );
    }

    let (objs, counters) = analyze_units(elf, &dwarf, &units, struct_tags, ranges)?;

    if objs.is_empty() {
        warn!("Zero kobjects found");
    }

    if counters.threads > meta.max_threads {
        return Err(
            eyre!("Too many thread objects ({})", counters.threads).suggestion(format!(
                "Increase CONFIG_MAX_THREAD_BYTES to {}",
                counters.threads.div_ceil(8)
            )),
        );
    }

    Ok((objs, counters))
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
