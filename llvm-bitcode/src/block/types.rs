use std::{borrow::Borrow, collections::HashMap, rc::Rc};

use llvm_bitstream::{BitstreamReader, Entry, ReaderError};
use num_enum::{TryFromPrimitive, TryFromPrimitiveError};
use smallvec::SmallVec;
use tracing::{error, info, warn};

use crate::{
    bitcodes::{SyncScopeNameCode, TypeCode},
    util::types::{FloatingPointType, IntegerType, Structure, StructureType, Type},
    Fields,
};

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum TypesError {
    /// Failed to parse the num entries. This should always be the first record in a types block.
    #[error("Failed to parse the num entries record")]
    InvalidNumEntries,

    /// Parsed more types than the `NUM_ENTRIES` record specified.
    #[error("Found more records than NUM_ENTRIES specified")]
    TooManyRecords,

    /// Expected a struct name record to be parsed before parsing a subsequent record requiring it.
    ///
    /// Struct names are consumed for e.g. named structs and extensions types. A struct name must
    /// be before these records.
    #[error("Failed to parse record, a type name record was expected to have been parsed")]
    NoTypeNameForRecord,

    /// Failed to parse a struct name record, could not convert the record into a string.
    #[error("Failed to convert record into struct for struct name record")]
    InvalidStructNameRecord,

    /// Failed to get the bit-width from an integer record.
    #[error("Failed to get the bit-width from an integer record")]
    InvalidIntegerRecord,

    /// Bit-width of an integer is outside the allowed range.
    #[error("Bit-width of the integer is outside of the allowed range")]
    BitwidthOutOfRange,

    /// Failed to get the address space from an opaque pointer record.
    #[error("Failed to get the address space from an opaque pointer record")]
    InvalidOpaquePointerRecord,

    /// Failed to parse an array record.
    #[error("Failed to parse an array record")]
    InvalidArrayRecord,

    /// Failed to parse a vector record.
    #[error("Failed to parse a vector record")]
    InvalidVectorRecord,

    /// Failed to parse a anonymous struct record.
    #[error("Failed to parse a anonymous struct record")]
    InvalidStructAnonRecord,

    /// Failed to parse a named struct record.
    #[error("Failed to parse a named struct record")]
    InvalidStructNamedRecord,

    /// Failed to parse an opaque struct record.
    #[error("Failed to parse an opaque struct record")]
    InvalidOpaqueStructRecord,

    /// Failed to parse a function record. Either the record does not hold enough elements, or
    /// the contained parameter types could not be found.
    #[error("Failed to parse a function type record")]
    InvalidFunctionRecord,

    /// Failed to parse a target extension type record.
    #[error("Failed to parse a target extension type record")]
    InvalidTargetTypeRecord,

    /// Encountered a currently unsupported record, cannot continue parsing safely since types
    /// are referenced by index.
    #[error("Encountered an unsupported record: {0}. Cannot safely continue parsing")]
    UnsupportedRecord(TypeCode),

    /// Unexpected [`TypeCode`], cannot safely continue parsing as types are by their occurence in
    /// the type block.
    #[error("Unexpected type code: {0}")]
    UnexpectedTypeCode(u32),

    /// Error from the underlying [`BitstreamReader`].
    #[error("{0}")]
    ReaderError(#[from] ReaderError),
}

/// Tracks [`Type`]s from a type block.
///
///
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct TypeList {
    pub(crate) types: Vec<Rc<Type>>,
    pub(crate) contained_type_ids: HashMap<u64, SmallVec<[u32; 16]>>,
}

impl TypeList {
    fn with_capacity(capacity: usize) -> Self {
        Self {
            types: Vec::with_capacity(capacity),
            contained_type_ids: HashMap::new(),
        }
    }

    pub fn get(&self, tid: u64) -> Option<&Rc<Type>> {
        self.types.get(tid as usize)
    }

    pub fn add(&mut self, ty: Type) {
        info!("Add type: {ty}");
        self.types.push(Rc::new(ty));
    }

    pub fn add_with_contained_ids(&mut self, ty: Type, contained_ids: SmallVec<[u32; 16]>) {
        info!("Add type: {ty} with contained ids: {contained_ids:?}");
        let index = self.types.len();
        self.types.push(Rc::new(ty));
        self.contained_type_ids.insert(index as u64, contained_ids);
    }
}

/// Parse a function block.
///
/// # Errors
///
/// Types are referenced by index, so unknown records *cannot* be skipped, if found [`TypesError::UnexpectedTypeCode`]
/// is returned.
pub fn parse_type_block<T: AsRef<[u8]>>(
    bitstream: &mut BitstreamReader<T>,
) -> Result<TypeList, TypesError> {
    // Note about forwards references:
    //
    // LLVM talks about forward referenced structs. However, looking through the source I cannot
    // see how it's possible. The only parts where their type list is reference is inside the
    // struct records; and they never create types past the type index. I may be wrong and it can
    // somehow create forward referenced structs, I don't know how that's possible to forward
    // referenced structs are skipped when parsing a type block here.

    let mut record = Fields::<64>::new();

    // Get the number of entries (types) in the type block.
    let num_entries = {
        const NUM_ENTRIES_CODE: u64 = 1;

        let Some(Entry::Record(entry)) = bitstream.advance()? else {
            return Err(TypesError::InvalidNumEntries)?;
        };

        let code = bitstream.read_record(entry, &mut record)?;
        let Some(TypeCode::NumEntries) = TypeCode::from_code(code) else {
            return Err(TypesError::InvalidNumEntries)?;
        };

        record
            .first()
            .copied()
            .ok_or(TypesError::InvalidNumEntries)? as usize
    };

    let mut type_list = TypeList::with_capacity(num_entries);
    let mut contained_ids: SmallVec<[u32; 16]> = SmallVec::new();

    // Some records set the name of a struct that is used in subsequent structs.
    let mut type_name: Option<String> = None;

    // The type block does not contain any subblocks itself, only records.
    let mut processed_records = 0;
    while let Some(entry) = bitstream.records()? {
        let code = bitstream.read_record(entry, &mut record)?;
        let Some(code) = TypeCode::from_code(code) else {
            return Err(TypesError::UnexpectedTypeCode(code));
        };

        let ty = match code {
            // These are old records. Either a deprecated function record, or an old style pointer
            // record. Currently, only opaque pointers are supported (LLVM 15+).
            TypeCode::Pointer | TypeCode::FunctionOld => {
                return Err(TypesError::UnsupportedRecord(code));
            }
            // Should already have been handled above; should only exist one in the types block.
            TypeCode::NumEntries => {
                warn!("Found more than one NumEntries record");
                continue;
            }
            TypeCode::StructName => {
                let name = record
                    .to_string(0)
                    .ok_or(TypesError::InvalidStructNameRecord)?;
                type_name = Some(name);
                continue;
            }
            TypeCode::Void => Type::Void,
            TypeCode::Half => Type::FloatingPoint(FloatingPointType::Half),
            TypeCode::BFloat => Type::FloatingPoint(FloatingPointType::BFloat),
            TypeCode::Float => Type::FloatingPoint(FloatingPointType::Float),
            TypeCode::Double => Type::FloatingPoint(FloatingPointType::Double),
            TypeCode::Fp128 => Type::FloatingPoint(FloatingPointType::Fp128),
            TypeCode::X86Fp80 => Type::FloatingPoint(FloatingPointType::X86Fp80),
            TypeCode::PpcFp128 => Type::FloatingPoint(FloatingPointType::PpcFp128),
            TypeCode::X86Mmx => Type::FloatingPoint(FloatingPointType::X86Fp80),
            TypeCode::X86Amx => Type::FloatingPoint(FloatingPointType::X86Amx),
            TypeCode::Label => Type::Label,
            TypeCode::Token => Type::Token,
            TypeCode::Metadata => Type::Metadata,
            TypeCode::Integer => {
                let bits = record.get_u32(0).ok_or(TypesError::InvalidIntegerRecord)?;
                if !(IntegerType::MIN_BITS..IntegerType::MAX_BITS).contains(&bits) {
                    return Err(TypesError::BitwidthOutOfRange);
                }
                Type::Integer(IntegerType { bits })
            }
            TypeCode::OpaquePointer => {
                let address_space = record
                    .first()
                    .copied()
                    .ok_or(TypesError::InvalidOpaquePointerRecord)?;

                Type::Pointer { address_space }
            }
            TypeCode::Array => {
                if record.len() < 2 {
                    return Err(TypesError::InvalidArrayRecord);
                }

                let num_elements = record[0];
                let ty = type_list
                    .get(record[1])
                    .cloned()
                    .ok_or(TypesError::InvalidArrayRecord)?;

                if !ty.is_valid_element_type() {
                    return Err(TypesError::InvalidArrayRecord);
                }

                Type::Array { num_elements, ty }
            }
            TypeCode::Vector => {
                // Must contain num elements and type id, and num elements must be greater than 0.
                if record.len() < 2 || record[0] == 0 {
                    return Err(TypesError::InvalidVectorRecord);
                }
                let num_elements = record[0];
                let ty = type_list
                    .get(record[1])
                    .cloned()
                    .ok_or(TypesError::InvalidVectorRecord)?;
                let is_scalable = record.get(2).map(|v| *v > 0).unwrap_or(false);

                Type::Vector {
                    num_elements,
                    ty,
                    is_scalable,
                }
            }
            TypeCode::StructAnon => {
                // LLVM only checks that it's not empty, but `is_packed` is then retrieve by index
                // which asserts that the index is larger than the current size.
                if record.len() < 1 {
                    return Err(TypesError::InvalidStructAnonRecord);
                }

                let is_packed = record[0] > 0;
                let fields: SmallVec<[Rc<Type>; 8]> = record
                    .iter()
                    .skip(1)
                    .map(|&tid| {
                        type_list
                            .get(tid)
                            .cloned()
                            .ok_or(TypesError::InvalidStructAnonRecord)
                    })
                    .collect::<Result<_, _>>()?;

                // We never expect more than 2^32 type ids, so this coercion should be fine.
                contained_ids.extend(record.iter().skip(1).map(|&v| v as u32));

                Type::Structure(Structure::Literal(StructureType { fields, is_packed }))
            }
            TypeCode::StructNamed => {
                // LLVM only checks that it's not empty, but `is_packed` is then retrieve by index
                // which asserts that the index is larger than the current size.
                if record.len() < 1 {
                    return Err(TypesError::InvalidStructNamedRecord);
                }
                let name = type_name.take().ok_or(TypesError::NoTypeNameForRecord)?;
                let is_packed = record[0] > 0;
                let fields: SmallVec<[Rc<Type>; 8]> = record
                    .iter()
                    .skip(1)
                    .map(|&tid| {
                        type_list
                            .get(tid)
                            .cloned()
                            .ok_or(TypesError::InvalidStructAnonRecord)
                    })
                    .collect::<Result<_, _>>()?;

                // We never expect more than 2^32 type ids, so this coercion should be fine.
                contained_ids.extend(record.iter().skip(1).map(|&v| v as u32));

                Type::Structure(Structure::Identified(
                    name,
                    StructureType { fields, is_packed },
                ))
            }
            TypeCode::Opaque => {
                // This one is weird, LLVM checks that the size *must* be one, but it never reads
                // the value from the record.
                if record.len() != 1 {
                    return Err(TypesError::InvalidOpaqueStructRecord);
                }
                let name = type_name.take().ok_or(TypesError::NoTypeNameForRecord)?;
                Type::Structure(Structure::Opaque(name))
            }
            TypeCode::Function => {
                if record.len() < 2 {
                    return Err(TypesError::InvalidFunctionRecord);
                }
                let is_var_arg = record[0] > 0;
                let return_ty = type_list
                    .get(record[1])
                    .cloned()
                    .ok_or(TypesError::InvalidFunctionRecord)?;

                let parameters: SmallVec<[Rc<Type>; 8]> = record
                    .iter()
                    .copied()
                    .skip(2)
                    .map(|param_tid| {
                        let ty = type_list
                            .get(param_tid)
                            .cloned()
                            .ok_or(TypesError::InvalidFunctionRecord)?;

                        // Only first class types are valid as arguments.
                        if !ty.is_first_class() {
                            return Err(TypesError::InvalidFunctionRecord);
                        }
                        Ok(ty)
                    })
                    .collect::<Result<_, _>>()?;

                Type::Function {
                    parameters,
                    return_ty,
                    is_var_arg,
                }
            }
            TypeCode::TargetType => {
                if record.len() < 1 {
                    return Err(TypesError::InvalidTargetTypeRecord);
                }
                let name = type_name.take().ok_or(TypesError::NoTypeNameForRecord)?;

                let num_types = record[0] as usize;
                if num_types > num_entries {
                    return Err(TypesError::InvalidTargetTypeRecord);
                }

                let type_parameters: SmallVec<[Rc<Type>; 4]> = record
                    .iter()
                    .skip(1)
                    .take(num_types)
                    .map(|tid: &u64| {
                        type_list
                            .get(*tid)
                            .cloned()
                            .ok_or(TypesError::InvalidTargetTypeRecord)
                    })
                    .collect::<Result<_, _>>()?;

                let int_parameters: SmallVec<[u32; 8]> = record
                    .iter()
                    .skip(1 + num_types)
                    .map(|&value| {
                        value
                            .try_into()
                            .map_err(|_ignore| TypesError::InvalidTargetTypeRecord)
                    })
                    .collect::<Result<_, _>>()?;

                Type::TargetExtension {
                    name,
                    type_parameters,
                    int_parameters,
                }
            }
        };

        // If we processed more than the number of type entries we expected, the type table is
        // invalid.
        if processed_records >= num_entries {
            return Err(TypesError::TooManyRecords);
        }
        processed_records += 1;

        if contained_ids.is_empty() {
            type_list.add(ty);
        } else {
            type_list.add_with_contained_ids(ty, contained_ids);
            contained_ids = SmallVec::new();
        }
    }

    Ok(type_list)
}
