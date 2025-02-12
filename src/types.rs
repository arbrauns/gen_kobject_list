use std::{
    collections::HashMap,
    fmt::Display,
    num::NonZero,
    ops::{Deref, Range},
};

use color_eyre::{
    eyre::{bail, OptionExt as _},
    Result,
};
use gimli::{
    constants::{DW_AT_name, DW_AT_specification, DW_AT_type},
    AttributeValue, DebuggingInformationEntry, DwAt, Dwarf, EndianSlice, RunTimeEndian, Unit,
    UnitSectionOffset,
};
use tracing::instrument;

use crate::{consts::STACK_TYPE, get_symbol_range};

pub type Reader<'input> = EndianSlice<'input, RunTimeEndian>;
pub type Die<'input, 'unit> = DebuggingInformationEntry<'unit, 'unit, Reader<'input>>;
pub type Symbols<'name> = HashMap<&'name str, u64>;

/// `DebuggingInformationEntry` is only meaningful with its associated `Unit`, so this type tracks
/// both at the same time.
#[derive(Debug, Clone, Copy)]
pub struct DieRef<'die, 'input, 'unit> {
    pub die: &'die Die<'input, 'unit>,
    pub unit: &'unit Unit<Reader<'input>>,
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
pub struct OwnedDieRef<'input, 'unit> {
    pub die: Die<'input, 'unit>,
    pub unit: &'unit Unit<Reader<'input>>,
}

impl<'input, 'unit> OwnedDieRef<'input, 'unit> {
    pub fn as_die_ref(&self) -> DieRef<'_, 'input, 'unit> {
        DieRef {
            die: &self.die,
            unit: self.unit,
        }
    }

    pub fn get_name(&self, dwarf: &Dwarf<Reader<'input>>) -> Result<Option<&'input str>> {
        self.as_die_ref().get_name(dwarf)
    }

    pub fn get_type(&self) -> Result<Option<OwnedDieRef<'input, 'unit>>> {
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
    fn new(name: String, api: bool) -> Self {
        Self {
            name,
            api,
            // Type name determined later since drivers needs to look at the API struct address
            type_name: None,
            data: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Type<'input, 'unit> {
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
    pub fn get_kobjects<'a>(
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
pub struct AggregateMember<'input, 'unit> {
    pub name: String,
    pub typ: OwnedDieRef<'input, 'unit>,
    pub offset: u64,
}

#[derive(Debug)]
pub struct MemoryRanges {
    pub app_smem: Range<u64>,
    pub app_smem_pinned: Range<u64>,
    pub user_stack: Range<u64>,
}

impl MemoryRanges {
    pub fn from_symbols(syms: &Symbols) -> Result<Self> {
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
pub struct TypeEnv<'input, 'unit> {
    inner: HashMap<UnitSectionOffset, Type<'input, 'unit>>,
}

impl<'input, 'unit> TypeEnv<'input, 'unit> {
    #[inline]
    pub fn get(&self, die: DieRef<'_, '_, '_>) -> Option<&Type<'input, 'unit>> {
        self.inner.get(&die.unit_section_offset())
    }

    #[inline]
    pub fn insert(&mut self, die: DieRef<'_, '_, '_>, typ: Type<'input, 'unit>) {
        self.inner.insert(die.unit_section_offset(), typ);
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    #[instrument(skip(self))]
    pub fn remove_non_kobjects(&mut self) {
        let clone = self
            .inner
            .iter()
            .filter_map(|(addr, t)| t.clone_kobjects_only(self).map(|new_t| (*addr, new_t)))
            .collect();
        self.inner = clone;
    }
}
