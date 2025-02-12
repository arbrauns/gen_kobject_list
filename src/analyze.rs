use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap},
    fmt::{Display, Write as _},
    num::{NonZero, NonZeroUsize},
    ops::{Deref, Range},
};

use color_eyre::{
    eyre::{bail, eyre, Context, OptionExt},
    Result, Section as _,
};
use fallible_iterator::FallibleIterator;
use gimli::{
    constants::{
        self, DW_AT_byte_size, DW_AT_count, DW_AT_data_member_location, DW_AT_decl_file,
        DW_AT_decl_line, DW_AT_location, DW_AT_name, DW_AT_specification, DW_AT_type,
        DW_AT_upper_bound, DW_TAG_member, DW_TAG_subrange_type,
    },
    AttributeValue, DebuggingInformationEntry, DwAt, Dwarf, EndianSlice, Operation, RunTimeEndian,
    Unit, UnitSectionOffset,
};
use object::{Object as _, ObjectSection as _};
use tracing::{debug, info, instrument, trace, warn};

use crate::{
    get_symbol_range, kobject_to_enum, subsystem_to_enum, KobjectInstance, StructTags, Symbols,
    KOBJECTS,
};

type Reader<'input> = EndianSlice<'input, RunTimeEndian>;
type Die<'input, 'unit> = DebuggingInformationEntry<'unit, 'unit, Reader<'input>>;

const STACK_TYPE: &str = "z_thread_stack_element";

/// `DebuggingInformationEntry` is only meaningful with its associated `Unit`, so this type tracks
/// both at the same time.
#[derive(Debug, Clone, Copy)]
struct DieRef<'die, 'input, 'unit> {
    die: &'die Die<'input, 'unit>,
    unit: &'unit Unit<Reader<'input>>,
}

impl<'input, 'unit> DieRef<'_, 'input, 'unit> {
    #[instrument(skip(self))]
    fn get_attr_or_follow_spec(self, attr: DwAt) -> Result<Option<AttributeValue<Reader<'input>>>> {
        if let Some(attr_val) = self.die.attr_value(attr)? {
            return Ok(Some(attr_val));
        };

        let Some(spec_ref) = self.die.attr_value(DW_AT_specification)? else {
            return Ok(None);
        };

        let AttributeValue::UnitRef(spec_offset) = spec_ref else {
            bail!("Unexpected form for DW_AT_specification: {spec_ref:?}")
        };

        Ok(Some(
            self.unit
                .entry(spec_offset)?
                .attr_value(attr)?
                .ok_or_eyre("spec DIE should have requested attribute")?,
        ))
    }

    pub fn unit_section_offset(self) -> UnitSectionOffset {
        self.die.offset().to_unit_section_offset(self.unit)
    }

    #[instrument(skip(self, dwarf))]
    pub fn get_name(self, dwarf: &Dwarf<Reader<'input>>) -> Result<Option<&'input str>> {
        let Some(attr_val) = self.get_attr_or_follow_spec(DW_AT_name)? else {
            return Ok(None);
        };

        Ok(Some(dwarf.attr_string(self.unit, attr_val)?.to_string()?))
    }

    #[instrument(skip(self))]
    pub fn get_type(self) -> Result<Option<Die<'input, 'unit>>> {
        let Some(attr_val) = self.get_attr_or_follow_spec(DW_AT_type)? else {
            return Ok(None);
        };

        let AttributeValue::UnitRef(entry_offset) = attr_val else {
            bail!("Unexpected form for DW_AT_type: {attr_val:?}")
        };

        Ok(Some(self.unit.entry(entry_offset)?))
    }
}

impl<'input, 'unit> Deref for DieRef<'_, 'input, 'unit> {
    type Target = Die<'input, 'unit>;

    fn deref(&self) -> &Self::Target {
        self.die
    }
}

/// Equivalent of `DieRef` which owns the `DebuggingInformationEntry`.
#[derive(Debug, Clone)]
struct OwnedDieRef<'input, 'unit> {
    die: Die<'input, 'unit>,
    unit: &'unit Unit<Reader<'input>>,
}

impl<'input, 'unit> OwnedDieRef<'input, 'unit> {
    fn as_die_ref(&self) -> DieRef<'_, 'input, 'unit> {
        DieRef {
            die: &self.die,
            unit: self.unit,
        }
    }

    fn get_name(&self, dwarf: &Dwarf<Reader<'input>>) -> Result<Option<&'input str>> {
        self.as_die_ref().get_name(dwarf)
    }

    fn get_type(&self) -> Result<Option<OwnedDieRef<'input, 'unit>>> {
        if let Some(die) = self.as_die_ref().get_type()? {
            Ok(Some(OwnedDieRef {
                die,
                unit: self.unit,
            }))
        } else {
            Ok(None)
        }
    }
}

impl<'input, 'unit> Deref for OwnedDieRef<'input, 'unit> {
    type Target = Die<'input, 'unit>;

    fn deref(&self) -> &Self::Target {
        &self.die
    }
}

#[derive(Debug, Clone)]
enum Type<'input, 'unit> {
    Kobject {
        name: String,
        size: NonZero<usize>,
        api: bool,
    },
    Aggregate {
        name: String,
        size: NonZero<usize>,
        members: Vec<AggregateMember<'input, 'unit>>,
    },
    Const {
        child_type: OwnedDieRef<'input, 'unit>,
    },
    Array {
        /// Number of elements, one entry per array dimension
        elements: Vec<usize>,
        member_type: OwnedDieRef<'input, 'unit>,
    },
}

impl Display for Type<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn debug_info_offset(die: &OwnedDieRef) -> usize {
            let UnitSectionOffset::DebugInfoOffset(dio) = die.as_die_ref().unit_section_offset()
            else {
                panic!("DIE should be in .debug_info");
            };

            dio.0
        }

        match self {
            Type::Kobject { name, .. } => write!(f, "<kobject {name}>"),
            Type::Aggregate { name, members, .. } => {
                write!(f, "<struct {name}, with [")?;
                for (i, AggregateMember { name, typ, offset }) in members.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(
                        f,
                        "<member {name}, type {}, offset {offset}>",
                        debug_info_offset(typ)
                    )?;
                }
                write!(f, "]>")
            }
            Type::Const { child_type } => write!(f, "<const {}>", debug_info_offset(child_type)),
            Type::Array { member_type, .. } => {
                write!(f, "<array of {}>", debug_info_offset(member_type))
            }
        }
    }
}

impl Type<'_, '_> {
    #[instrument(skip(self, type_env))]
    fn has_kobject(&self, type_env: &TypeEnv) -> bool {
        match self {
            Type::Kobject { .. } => true,
            Type::Aggregate { members, .. } => members.iter().any(|m| {
                type_env
                    .get(m.typ.as_die_ref())
                    .is_some_and(|t| t.has_kobject(type_env))
            }),
            Type::Const { child_type: typ }
            | Type::Array {
                member_type: typ, ..
            } => type_env
                .get(typ.as_die_ref())
                .is_some_and(|t| t.has_kobject(type_env)),
        }
    }

    #[instrument(skip(self, type_env))]
    fn clone_kobjects_only(&self, type_env: &TypeEnv) -> Option<Self> {
        // FIXME: terrible unnecessary recursion, do it in-place somehow
        let die_has_kobject = |dieref: &OwnedDieRef| {
            type_env
                .get(dieref.as_die_ref())
                .is_some_and(|t| t.has_kobject(type_env))
        };

        match self {
            Type::Kobject { .. } => Some(self.clone()),
            &Type::Aggregate {
                ref name,
                size,
                ref members,
            } => {
                let members: Vec<_> = members
                    .iter()
                    .filter(|m| die_has_kobject(&m.typ))
                    .cloned()
                    .collect();

                if members.is_empty() {
                    None
                } else {
                    Some(Type::Aggregate {
                        name: name.clone(),
                        size,
                        members,
                    })
                }
            }
            Type::Const { child_type: typ }
            | Type::Array {
                member_type: typ, ..
            } => {
                if die_has_kobject(typ) {
                    Some(self.clone())
                } else {
                    None
                }
            }
        }
    }

    #[instrument(skip(self, type_env))]
    fn get_kobjects<'a>(
        &'a self,
        type_env: &'a TypeEnv,
        addr: u64,
    ) -> Box<dyn Iterator<Item = (u64, KobjectInstance)> + 'a> {
        match self {
            Type::Kobject { name, api, .. } => Box::new(std::iter::once((
                addr,
                KobjectInstance::new(name.clone(), *api),
            ))),
            Type::Aggregate { members, .. } => Box::new(members.iter().flat_map(move |m| {
                let mt = type_env
                    .get(m.typ.as_die_ref())
                    .expect("Child type should be in type env");
                mt.get_kobjects(type_env, addr + m.offset)
            })),
            Type::Const { child_type } => type_env
                .get(child_type.as_die_ref())
                .expect("Child type should be in type env")
                .get_kobjects(type_env, addr),
            Type::Array {
                elements,
                member_type,
                ..
            } => {
                let mt = type_env
                    .get(member_type.as_die_ref())
                    .expect("Child type should be in type env");

                // Stacks are arrays of _k_stack_element_t but we want to treat
                // the whole array as one kernel object (a thread stack)
                // Data value gets set to size of entire region
                let (element_size, dimensions, is_stacks) = match mt {
                    Type::Kobject { name, .. } if name == STACK_TYPE => {
                        // An array of stacks appears as a multi-dimensional array.
                        // The last size is the size of each stack. We need to track
                        // each stack within the array, not as one huge stack object.
                        let (stacksize, dimensions) = elements
                            .split_last()
                            .expect("Array should have at least one dimension");

                        (*stacksize, dimensions, true)
                    }
                    Type::Kobject { size, .. } | Type::Aggregate { size, .. } => {
                        (size.get(), elements.as_slice(), false)
                    }
                    Type::Const { .. } | Type::Array { .. } => {
                        panic!("Unexpected array member type {mt:?}")
                    }
                };

                let num_members: usize = dimensions.iter().product();

                Box::new((0..num_members).flat_map(move |i| {
                    let member_addr =
                        addr + u64::try_from(i * element_size).expect("usize should fit in u64");

                    mt.get_kobjects(type_env, member_addr)
                        .map(move |(addr, mut inst)| {
                            if is_stacks && addr == member_addr {
                                inst.data = element_size;
                            }
                            (addr, inst)
                        })
                }))
            }
        }
    }
}

#[derive(Debug, Clone)]
struct AggregateMember<'input, 'unit> {
    name: String,
    typ: OwnedDieRef<'input, 'unit>,
    offset: u64,
}

#[derive(Debug)]
struct MemoryRanges {
    app_smem: Range<u64>,
    app_smem_pinned: Range<u64>,
    user_stack: Range<u64>,
}

impl MemoryRanges {
    fn from_symbols(syms: &Symbols) -> Result<Self> {
        let app_smem = get_symbol_range(syms, "_app_smem")?;
        let app_smem_pinned = if syms.contains_key("CONFIG_LINKER_USE_PINNED_SECTION")
            && syms.contains_key("_app_smem_pinned_start")
        {
            get_symbol_range(syms, "_app_smem_pinned")?
        } else {
            app_smem.clone()
        };
        let user_stack = get_symbol_range(syms, "z_user_stacks")?;

        Ok(Self {
            app_smem,
            app_smem_pinned,
            user_stack,
        })
    }
}

#[derive(Default)]
struct TypeEnv<'input, 'unit> {
    inner: HashMap<UnitSectionOffset, Type<'input, 'unit>>,
}

impl<'input, 'unit> TypeEnv<'input, 'unit> {
    #[inline]
    fn get(&self, die: DieRef<'_, '_, '_>) -> Option<&Type<'input, 'unit>> {
        self.inner.get(&die.unit_section_offset())
    }

    #[inline]
    fn insert(&mut self, die: DieRef<'_, '_, '_>, typ: Type<'input, 'unit>) {
        self.inner.insert(die.unit_section_offset(), typ);
    }

    #[inline]
    fn len(&self) -> usize {
        self.inner.len()
    }

    #[instrument(skip(self))]
    fn remove_non_kobjects(&mut self) {
        let clone = self
            .inner
            .iter()
            .filter_map(|(addr, t)| t.clone_kobjects_only(self).map(|new_t| (*addr, new_t)))
            .collect();
        self.inner = clone;
    }
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
pub struct AnalyzerCounters {
    pub threads: usize,
    pub sys_mutexes: usize,
    pub futexes: usize,
    pub stacks: usize,
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
    fn analyze_unit(mut self, variables: &mut Vec<OwnedDieRef<'input, 'unit>>) -> Result<()> {
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

pub struct FileMetadata<'input> {
    pub syms: Symbols<'input>,
    pub max_threads: usize,
    pub is_64bit: bool,
    pub is_little_endian: bool,
}

#[instrument(skip(elf, meta, struct_tags))]
pub fn find_kobjects<'input>(
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
