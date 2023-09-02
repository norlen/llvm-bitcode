use llvm_bitstream::{BitstreamReader, ReaderError};
use num_enum::TryFromPrimitive;
use smallvec::SmallVec;
use tracing::{info, warn};

use crate::{
    bitcodes::{AttributeCode, AttributeKindCode},
    context::Context,
    ir::{
        Attribute, AttributeGroup, EnumAttribute, IntAttribute, MemoryEffects, TypeAttribute,
        UnwindTableKind,
    },
};

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum AttributeGroupError {
    #[error("TypeNotFound: {0}")]
    TypeNotFound(u64),

    /// Failed to parse a group record.
    #[error("Invalid group record")]
    InvalidGroupRecord,

    /// Got an enum record, but the parsed attribute wasn't an enum attribute.
    #[error("Unexpected attribute found in enum record")]
    InvalidEnumAttribute,

    /// Got an int record, but the parsed attribute wasn't an int attribute.
    #[error("Unexpected attribute found in int record")]
    InvalidIntAttribute,

    /// Got a type record, but the parsed attribute wasn't a type attribute.
    #[error("Unexpected attribute found in type record")]
    InvalidTypeAttribute,

    /// Unknown record attribute code
    #[error("Unknown record attribute code: {0}")]
    UnknownRecordAttributeCode(u64),

    /// Error from the underlying [`BitstreamReader`].
    #[error("{0}")]
    ReaderError(#[from] ReaderError),
}

/// Attribute kind.
///
/// Each attribute kind is either a well-known LLVM attribute or an arbitrary string. Both of these
/// can have either an integer respectively a string value associated with it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum RecordAttributeCode {
    /// Well-known attribute.
    ///
    /// `[key#]`.
    Enum = 0,

    /// Well-known attribute with an integer value.
    ///
    /// `[key#, value#]`.
    Int = 1,

    /// String attribute.
    ///
    /// `[ch x N]`.
    String = 3,

    /// String attribute with a string value.
    ///
    /// `[ch x N, ch x N]`.
    StringWithValue = 4,

    /// Type attribute.
    ///
    ///
    Type = 5,

    /// Type attribute with value.
    ///
    ///
    TypeWithValue = 6,
}

impl RecordAttributeCode {
    pub fn parse(code: u64) -> Result<RecordAttributeCode, AttributeGroupError> {
        let c: u8 = code
            .try_into()
            .map_err(|_ignore| AttributeGroupError::UnknownRecordAttributeCode(code))?;
        Self::try_from_primitive(c)
            .map_err(|_ignore| AttributeGroupError::UnknownRecordAttributeCode(code))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AttributeGroupType {
    /// Attribute group for the return value.
    Return,

    /// Attribute group for a function.
    Function,

    /// Attribute group for the Nth function parameter starting at 0.
    Parameter(u64),
}

impl From<u64> for AttributeGroupType {
    fn from(value: u64) -> Self {
        match value {
            0 => AttributeGroupType::Return,
            0xFFFFFFFF => AttributeGroupType::Function,
            n => AttributeGroupType::Parameter(n - 1),
        }
    }
}

/// Parse an attributes groups block.
///
///
pub fn parse_attribute_groups_block<T: AsRef<[u8]>>(
    bitstream: &mut BitstreamReader<T>,
    ctx: &Context,
) -> Result<Vec<AttributeGroup>, AttributeGroupError> {
    let mut record: SmallVec<[u64; 64]> = SmallVec::new();

    let mut attribute_groups = Vec::new();

    while let Some(entry) = bitstream.records()? {
        let code = bitstream.read_record(entry, &mut record)?;
        let Some(AttributeCode::GroupEntry) = AttributeCode::from_code(code) else {
            warn!("Unknown code {code}, skipping");
            continue;
        };

        if record.len() < 3 {
            return Err(AttributeGroupError::InvalidGroupRecord);
        }

        // Each record is encoded as
        // - PARAMATTR_GRP_CODE_ENTRY: [group_id, index, [$attribute]+]
        // - ATTRIBUTE: [ kind, key+, value* ]
        // Some keys and values can be encoded as strings, the rest are encoded as integers.
        let group_id = record[0];
        // let group_kind: AttributeGroupType = record[1].into();

        let mut memory_effects = MemoryEffects::unknown();
        let mut attribute_group = AttributeGroup::new(group_id);

        // attributes are of variable size.
        let mut index = 2;
        while index < record.len() {
            let attribute_kind = record[index];
            index += 1;

            let attribute_kind = RecordAttributeCode::parse(attribute_kind)?;
            let attribute = match attribute_kind {
                RecordAttributeCode::Enum => {
                    let Some(kind) = AttributeKindCode::from_code(record[index] as u32) else {
                        return Err(AttributeGroupError::InvalidGroupRecord);
                    };
                    index += 1;

                    // if matches!(group_kind, AttributeGroupType::Function) {
                    if let Some(me) = upgrade_old_memory_attribute(memory_effects, kind) {
                        memory_effects = me;
                        continue;
                    }
                    // }

                    // Old attribute may be present as enums, when they are type attributes in newer
                    // versions. Ensure these are parsed correctly, and ignore that they are enum
                    // attributes here.
                    match kind {
                        AttributeKindCode::ByVal => Attribute::Type(TypeAttribute::ByVal, None),
                        AttributeKindCode::StructRet => {
                            Attribute::Type(TypeAttribute::StructRet, None)
                        }
                        AttributeKindCode::InAlloca => {
                            Attribute::Type(TypeAttribute::InAlloca, None)
                        }
                        AttributeKindCode::UWTable => Attribute::Integer(
                            IntAttribute::UnwindTable,
                            UnwindTableKind::Default as u64,
                        ),
                        n => match parse_enum_attribute(n) {
                            Some(enum_attribute) => Attribute::Enum(enum_attribute),
                            None => return Err(AttributeGroupError::InvalidEnumAttribute),
                        },
                    }
                }
                RecordAttributeCode::Int => {
                    let Some(kind) = AttributeKindCode::from_code(record[index] as u32) else {
                        return Err(AttributeGroupError::InvalidGroupRecord);
                    };
                    index += 1;

                    let attribute = parse_int_attribute(kind)
                        .ok_or(AttributeGroupError::InvalidIntAttribute)?;
                    let value = record[index];
                    index += 1;

                    Attribute::Integer(attribute, value)
                }
                RecordAttributeCode::String | RecordAttributeCode::StringWithValue => {
                    let (key, size) = take_cstring(&record, index)?;
                    index += size;

                    let value = if matches!(attribute_kind, RecordAttributeCode::StringWithValue) {
                        let (value, size) = take_cstring(&record, index)?;
                        index += size;
                        Some(value)
                    } else {
                        None
                    };

                    Attribute::String(key, value)
                }
                RecordAttributeCode::Type | RecordAttributeCode::TypeWithValue => {
                    let Some(kind) = AttributeKindCode::from_code(record[index] as u32) else {
                        return Err(AttributeGroupError::InvalidGroupRecord);
                    };
                    index += 1;

                    let attribute = parse_type_attribute(kind)
                        .ok_or(AttributeGroupError::InvalidTypeAttribute)?;

                    let associated_type =
                        if matches!(attribute_kind, RecordAttributeCode::TypeWithValue) {
                            let tid = record[index];
                            index += 1;

                            let ty = ctx
                                .types
                                .get(tid)
                                .ok_or(AttributeGroupError::TypeNotFound(tid))?;

                            Some(ty)
                        } else {
                            None
                        };

                    Attribute::Type(attribute, associated_type)
                }
            };

            attribute_group.attributes.push(attribute);
        }

        if memory_effects != MemoryEffects::unknown() {
            attribute_group.attributes.push(Attribute::Integer(
                IntAttribute::Memory,
                memory_effects.data() as u64,
            ));
        }

        // TODO: check auto-upgrade of attributes.
        // It probobably shouldn't be required since the target is LLVM 15+, but it doesn't hurt
        // to perform the steps anyway. Logic for the auto upgrade is in `AutoUpgrade.cpp:5034`.

        info!("parsed_attributes: {attribute_group}");
        attribute_groups.push(attribute_group);
    }

    Ok(attribute_groups)
}

fn take_cstring<const N: usize>(
    record: &SmallVec<[u64; N]>,
    start: usize,
) -> Result<(String, usize), AttributeGroupError> {
    let string = record[start..]
        .iter()
        .copied()
        .take_while(|&b| b != 0)
        .map(u8::try_from)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|_ignore| AttributeGroupError::InvalidGroupRecord)?;

    let string =
        String::from_utf8(string).map_err(|_ignore| AttributeGroupError::InvalidGroupRecord)?;

    let size = string.len() + 1; // Include null-terminator.
    Ok((string, size))
}

fn upgrade_old_memory_attribute(
    me: MemoryEffects,
    kind: AttributeKindCode,
) -> Option<MemoryEffects> {
    match kind {
        AttributeKindCode::ReadNone => Some(me.intersect(MemoryEffects::none())),
        AttributeKindCode::ReadOnly => Some(me.intersect(MemoryEffects::read_only())),
        AttributeKindCode::ArgMemOnly => Some(me.intersect(MemoryEffects::arg_mem_only())),
        AttributeKindCode::InaccessibleMemOnly => {
            Some(me.intersect(MemoryEffects::inacessible_mem_only()))
        }
        AttributeKindCode::InaccessibleMemOrArgMemOnly => {
            Some(me.intersect(MemoryEffects::arg_or_inaccessible_mem_only()))
        }
        AttributeKindCode::WriteOnly => Some(me.intersect(MemoryEffects::write_only())),
        _ => None,
    }
}

/// Convert from a bitcode attribute to an enum IR attribute.
///
/// # Panics
///
/// If the following attribute codes haven't been handled beforehand this will function panic
///
/// - [`AttributeKindCode::ReadNone`]
/// - [`AttributeKindCode::ReadOnly`]
/// - [`AttributeKindCode::ArgMemOnly`]
/// - [`AttributeKindCode::InaccessibleMemOnly`]
/// - [`AttributeKindCode::InaccessibleMemOrArgMemOnly`]
///
/// These should be converted to [`MemoryEffects`].
fn parse_enum_attribute(code: AttributeKindCode) -> Option<EnumAttribute> {
    match code {
        AttributeKindCode::AlwaysInline => Some(EnumAttribute::AlwaysInline),
        AttributeKindCode::InlineHint => Some(EnumAttribute::InlineHint),
        AttributeKindCode::InReg => Some(EnumAttribute::InReg),
        AttributeKindCode::MinSize => Some(EnumAttribute::MinSize),
        AttributeKindCode::Naked => Some(EnumAttribute::Naked),
        AttributeKindCode::Nest => Some(EnumAttribute::Nest),
        AttributeKindCode::NoAlias => Some(EnumAttribute::NoAlias),
        AttributeKindCode::NoBuiltin => Some(EnumAttribute::NoBuiltin),
        AttributeKindCode::NoCapture => Some(EnumAttribute::NoCapture),
        AttributeKindCode::NoDuplicate => Some(EnumAttribute::NoDuplicate),
        AttributeKindCode::NoImplicitFloat => Some(EnumAttribute::NoImplicitFloat),
        AttributeKindCode::NoInline => Some(EnumAttribute::NoInline),
        AttributeKindCode::NonLazyBind => Some(EnumAttribute::NonLazyBind),
        AttributeKindCode::NoRedZone => Some(EnumAttribute::NoRedZone),
        AttributeKindCode::NoReturn => Some(EnumAttribute::NoReturn),
        AttributeKindCode::NoUnwind => Some(EnumAttribute::NoUnwind),
        AttributeKindCode::OptimizeForSize => Some(EnumAttribute::OptimizeForSize),
        AttributeKindCode::Returned => Some(EnumAttribute::Returned),
        AttributeKindCode::ReturnsTwice => Some(EnumAttribute::ReturnsTwice),
        AttributeKindCode::SExt => Some(EnumAttribute::SExt),
        AttributeKindCode::StackProtect => Some(EnumAttribute::StackProtect),
        AttributeKindCode::StackProtectReq => Some(EnumAttribute::StackProtectReq),
        AttributeKindCode::StackProtectStrong => Some(EnumAttribute::StackProtectStrong),
        AttributeKindCode::SanitizeAddress => Some(EnumAttribute::SanitizeAddress),
        AttributeKindCode::SanitizeThread => Some(EnumAttribute::SanitizeThread),
        AttributeKindCode::SanitizeMemory => Some(EnumAttribute::SanitizeMemory),
        AttributeKindCode::ZExt => Some(EnumAttribute::ZExt),
        AttributeKindCode::Builtin => Some(EnumAttribute::Builtin),
        AttributeKindCode::Cold => Some(EnumAttribute::Cold),
        AttributeKindCode::OptimizeNone => Some(EnumAttribute::OptimizeNone),
        AttributeKindCode::NonNull => Some(EnumAttribute::NonNull),
        AttributeKindCode::JumpTable => Some(EnumAttribute::JumpTable),
        AttributeKindCode::Convergent => Some(EnumAttribute::Convergent),
        AttributeKindCode::SafeStack => Some(EnumAttribute::SafeStack),
        AttributeKindCode::SwiftSelf => Some(EnumAttribute::SwiftSelf),
        AttributeKindCode::SwiftError => Some(EnumAttribute::SwiftError),
        AttributeKindCode::NoRecurse => Some(EnumAttribute::NoRecurse),
        AttributeKindCode::Speculatable => Some(EnumAttribute::Speculatable),
        AttributeKindCode::StrictFp => Some(EnumAttribute::StrictFp),
        AttributeKindCode::SanitizeHwAddress => Some(EnumAttribute::SanitizeHwAddress),
        AttributeKindCode::NoCfCheck => Some(EnumAttribute::NoCfCheck),
        AttributeKindCode::OptForFuzzing => Some(EnumAttribute::OptForFuzzing),
        AttributeKindCode::ShadowCallStack => Some(EnumAttribute::ShadowCallStack),
        AttributeKindCode::SpeculativeLoadHardening => {
            Some(EnumAttribute::SpeculativeLoadHardening)
        }
        AttributeKindCode::ImmArg => Some(EnumAttribute::ImmArg),
        AttributeKindCode::WillReturn => Some(EnumAttribute::WillReturn),
        AttributeKindCode::NoFree => Some(EnumAttribute::NoFree),
        AttributeKindCode::NoSync => Some(EnumAttribute::NoSync),
        AttributeKindCode::SanitizeMemTag => Some(EnumAttribute::SanitizeMemTag),
        AttributeKindCode::NoMerge => Some(EnumAttribute::NoMerge),
        AttributeKindCode::NullPointerIsValid => Some(EnumAttribute::NullPointerIsValid),
        AttributeKindCode::NoUndef => Some(EnumAttribute::NoUndef),
        AttributeKindCode::MustProgress => Some(EnumAttribute::MustProgress),
        AttributeKindCode::NoCallback => Some(EnumAttribute::NoCallback),
        AttributeKindCode::Hot => Some(EnumAttribute::Hot),
        AttributeKindCode::NoProfile => Some(EnumAttribute::NoProfile),
        AttributeKindCode::SwiftAsync => Some(EnumAttribute::SwiftAsync),
        AttributeKindCode::NoSanitizeCoverage => Some(EnumAttribute::NoSanitizeCoverage),
        AttributeKindCode::DisableSanitizerInstrumentation => {
            Some(EnumAttribute::DisableSanitizerInstrumentation)
        }
        AttributeKindCode::NoSanitizeBounds => Some(EnumAttribute::NoSanitizeBounds),
        AttributeKindCode::AllocAlign => Some(EnumAttribute::AllocAlign),
        AttributeKindCode::AllocatedPointer => Some(EnumAttribute::AllocatedPointer),
        AttributeKindCode::PresplitCoroutine => Some(EnumAttribute::PresplitCoroutine),
        AttributeKindCode::FnRetThunkExtern => Some(EnumAttribute::FnRetThunkExtern),
        AttributeKindCode::SkipProfile => Some(EnumAttribute::SkipProfile),

        AttributeKindCode::ReadNone
        | AttributeKindCode::ReadOnly
        | AttributeKindCode::WriteOnly
        | AttributeKindCode::ArgMemOnly
        | AttributeKindCode::InaccessibleMemOnly
        | AttributeKindCode::InaccessibleMemOrArgMemOnly => {
            panic!("Memory attributes should have been handled by the memory effects upgrade")
        }

        _ => None,
    }
}

/// Convert from a bitcode attribute to an int IR attribute.
fn parse_int_attribute(code: AttributeKindCode) -> Option<IntAttribute> {
    match code {
        AttributeKindCode::Alignment => Some(IntAttribute::Alignment),
        AttributeKindCode::StackAlignment => Some(IntAttribute::StackAlignment),
        AttributeKindCode::UWTable => Some(IntAttribute::UnwindTable),
        AttributeKindCode::Dereferenceable => Some(IntAttribute::Dereferenceable),
        AttributeKindCode::DereferenceableOrNull => Some(IntAttribute::DereferenceableOrNull),
        AttributeKindCode::AllocSize => Some(IntAttribute::AllocSize),
        AttributeKindCode::VScaleRange => Some(IntAttribute::VScaleRange),
        AttributeKindCode::AllocKind => Some(IntAttribute::AllocKind),
        AttributeKindCode::Memory => Some(IntAttribute::Memory),
        AttributeKindCode::NoFpClass => Some(IntAttribute::NoFpClass),
        _ => None,
    }
}

/// Convert from a bitcode attribute to a type IR attribute.
fn parse_type_attribute(code: AttributeKindCode) -> Option<TypeAttribute> {
    match code {
        AttributeKindCode::ByVal => Some(TypeAttribute::ByVal),
        AttributeKindCode::StructRet => Some(TypeAttribute::StructRet),
        AttributeKindCode::InAlloca => Some(TypeAttribute::InAlloca),
        AttributeKindCode::Preallocated => Some(TypeAttribute::Preallocated),
        AttributeKindCode::ByRef => Some(TypeAttribute::ByRef),
        AttributeKindCode::ElementType => Some(TypeAttribute::ElementType),
        _ => None,
    }
}
