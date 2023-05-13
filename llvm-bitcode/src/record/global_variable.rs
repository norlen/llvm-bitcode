use std::rc::Rc;

use smallvec::SmallVec;

use crate::{
    context::Context,
    ir::{GlobalVariable, SanitizerMetadata},
    record::util::{
        decode_linkage, get_attributes_opt, get_dll_storage_class, get_preemption_specifier,
        get_section_opt, get_thread_local, get_unnamed_addr, get_visibility_opt,
        has_implicit_comdat,
    },
    util::{parse_alignment, types::Type},
};

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum GlobalVariableError {
    /// Not enough fields in the record.
    #[error("not enough fields in the record")]
    IncompleteRecord,

    /// Failed to parse record.
    #[error("Failed to parse record: {0}")]
    InvalidRecord(&'static str),

    /// Currently, only explicit types are supported when parsing global variable records.
    #[error("Non-explicit types are not supported")]
    NonExplicitTypesUnsupported,

    /// Implicit comdat is not supported.
    #[error("Implicit comdat is not supported")]
    ImplicitComdatUnsupported,
}

struct PackedValues {
    is_constant: bool,
    explicit_type: bool,
    address_space: u64,
}

impl From<u64> for PackedValues {
    fn from(value: u64) -> Self {
        PackedValues {
            is_constant: value & 0x1 > 0,
            explicit_type: value & 0x2 > 0,
            address_space: value >> 2,
        }
    }
}

/// Parse a global variable record.
///
/// On success the parsed global variable, its type, and the initializers id will be returned.
pub fn parse_global_variable<const N: usize>(
    record: &SmallVec<[u64; N]>,
    ctx: &Context,
) -> Result<(GlobalVariable, Rc<Type>, Option<u64>), GlobalVariableError> {
    // Global variable structure:
    //
    // | index | field                            |
    // |-------|----------------------------------|
    // |   0,1 | strtab_offset, strtab_size       |
    // |     2 | type_id                          |
    // |     3 | packed_values                    |
    // |     4 | init_id                          |
    // |     5 | linkage                          |
    // |     6 | alignment                        |
    // |     7 | section                          |
    // |     8 | visibility                       |
    // |     9 | thread_local_mode                |
    // |    10 | unnamed_addr                     |
    // |    11 | externally_initialized           |
    // |    12 | dll_storage_class                |
    // |    13 | comdat                           |
    // |    14 | attributes                       |
    // |    15 | dso_local                        |
    // | 16,17 | partition_offset, partition_size |
    // |    18 | sanitizer_metadata               |
    //
    // All fields with an index >= 7 are optional.
    //
    // Packed values: [is_constant (bit 0), explicit_type (bit 1), addrspace (rest)]
    const MIN_FIELDS: usize = 7;
    if record.len() < MIN_FIELDS {
        return Err(GlobalVariableError::IncompleteRecord);
    }

    let name = ctx
        .strtab()
        .get(record[0], record[1])
        .ok_or(GlobalVariableError::InvalidRecord("could not get name"))?;

    let ty = ctx
        .type_list
        .get(record[2])
        .cloned()
        .ok_or(GlobalVariableError::InvalidRecord("unknown type"))?;

    let PackedValues {
        is_constant,
        address_space,
        explicit_type,
    } = record[3].into();

    if !explicit_type {
        return Err(GlobalVariableError::NonExplicitTypesUnsupported);
    }

    // If the global does not have an initialiazing value, this is zeroed out. The value that
    // iniitializes the global has most likely not been parsed yet, so we just return this to the
    // caller so they can fix the link later.
    let init_id = if record[4] == 0 {
        None
    } else {
        Some(record[4] - 1)
    };

    let raw_linkage_value = record[5]; // Needed to check for implicit comdat.
    let linkage = decode_linkage(raw_linkage_value);
    let alignment = parse_alignment(record[6]);

    let section = get_section_opt(record.get(7).copied(), ctx)
        .map_err(|_e| GlobalVariableError::InvalidRecord("section name not found"))?;

    let visibility = get_visibility_opt(record.get(8).copied(), linkage);
    let thread_local = get_thread_local(record.get(9).copied());
    let unnamed_addr = get_unnamed_addr(record.get(10).copied());
    let is_externally_initialized = record.get(11).copied().map_or(false, |v| v > 0);

    let dll_storage_class =
        get_dll_storage_class(record.get(12).copied(), linkage, raw_linkage_value);

    let comdat = match record.get(13).copied() {
        Some(n) if n != 0 => {
            let comdat = ctx
                .comdat
                .get((n - 1) as usize)
                .cloned()
                .ok_or(GlobalVariableError::InvalidRecord("comdat not found"))?;

            Some(comdat)
        }
        None if has_implicit_comdat(raw_linkage_value) => {
            return Err(GlobalVariableError::ImplicitComdatUnsupported);
        }
        Some(_) | None => None,
    };

    let attributes = get_attributes_opt(record.get(14).copied(), ctx)
        .map_err(|_e| GlobalVariableError::InvalidRecord("could not get attributes"))?;

    let preemption_specifier =
        get_preemption_specifier(record.get(15).copied(), visibility, linkage);

    let partition = if let (Some(&offset), Some(&size)) = (record.get(16), record.get(17)) {
        if size == 0 {
            None
        } else {
            let partition =
                ctx.strtab()
                    .get(offset, size)
                    .ok_or(GlobalVariableError::InvalidRecord(
                        "failed to get partition name",
                    ))?;
            Some(partition)
        }
    } else {
        None
    };

    let sanitizer_metadata = decode_sanitizer_metadata(record.get(18).copied().unwrap_or(0));

    let global_variable = GlobalVariable {
        name,
        ty: ty.clone(),
        is_constant,
        address_space,
        init_id: None,
        linkage,
        alignment,
        section,
        visibility,
        thread_local,
        unnamed_addr,
        is_externally_initialized,
        dll_storage_class,
        comdat,
        attributes,
        preemption_specifier,
        partition,
        sanitizer_metadata,
    };

    Ok((global_variable, ty, init_id))
}

fn decode_sanitizer_metadata(value: u64) -> SanitizerMetadata {
    SanitizerMetadata {
        no_address: (value & (1 << 0)) > 0,
        no_hw_address: (value & (1 << 1)) > 0,
        memtag: (value & (1 << 2)) > 0,
        is_dyn_init: (value & (1 << 3)) > 0,
    }
}
