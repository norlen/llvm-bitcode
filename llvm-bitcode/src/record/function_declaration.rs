use smallvec::SmallVec;

use crate::{
    context::Context,
    ir::Function,
    record::util::{
        decode_linkage, get_attributes, get_dll_storage_class, get_gc_name_opt,
        get_preemption_specifier, get_section, get_unnamed_addr, get_visibility_opt,
        has_implicit_comdat,
    },
    util::{parse_alignment, types::Type},
};

use super::util::decode_calling_convention;

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum FunctionRecordError {
    /// Not enough fields in the record.
    #[error("not enough fields in the record")]
    IncompleteRecord,

    /// Failed to parse record.
    #[error("Failed to parse record: {0}")]
    InvalidRecord(&'static str),

    /// Implicit comdat is not supported.
    #[error("Implicit comdat is not supported")]
    ImplicitComdatUnsupported,
}

/// Parse a global variable record.
///
/// On success the parsed global variable, its type, and the initializers id will be returned.
pub fn parse_function_record<const N: usize>(
    record: &SmallVec<[u64; N]>,
    ctx: &Context,
) -> Result<Function, FunctionRecordError> {
    // Global variable structure:
    //
    // | index | field                            |
    // |-------|----------------------------------|
    // |   0,1 | strtab_offset, strtab_size       |
    // |     2 | type_id                          |
    // |     3 | calling_conv                     |
    // |     4 | is_proto                         |
    // |     5 | linkage                          |
    // |     6 | param_attr                       |
    // |     7 | alignment                        |
    // |     8 | section                          |
    // |     9 | visibility                       |
    // |    10 | gc                               |
    // |    11 | unnamed_addr                     |
    // |    12 | prologue_data                    |
    // |    13 | dll_storage_class                |
    // |    14 | comdat                           |
    // |    15 | prefix_data                      |
    // |    16 | personality_fn                   |
    // |    17 | preemption_specifier             |
    // |    18 | addrspace                        |
    // | 19,20 | parition_offset, partition_size  |
    //
    // All fields with an index >= 10 are optional.
    if record.len() < 10 {
        return Err(FunctionRecordError::IncompleteRecord)?;
    }

    let name = ctx
        .strtab()
        .get(record[0], record[1])
        .ok_or(FunctionRecordError::InvalidRecord("invalid name reference"))?;

    let ty = {
        let ty = ctx
            .types
            .get(record[2])
            .ok_or(FunctionRecordError::InvalidRecord("invalid type id"))?;

        if matches!(ty.as_ref(), Type::Pointer { address_space: _ }) {
            todo!("opaque ptr");
        }

        if !ty.is_function() {
            return Err(FunctionRecordError::InvalidRecord("not a function type"));
        }

        ty
    };

    let calling_convention = decode_calling_convention(record[3]).ok_or(
        FunctionRecordError::InvalidRecord("invalid calling convention id"),
    )?;

    let is_proto = record[4] > 0;

    let raw_linkage_value = record[5]; // Needed to check for implicit comdat.
    let linkage = decode_linkage(raw_linkage_value);

    let attributes = get_attributes(record[6], ctx)
        .map_err(|_e| FunctionRecordError::InvalidRecord("attributes not found"))?;

    let alignment = parse_alignment(record[7]);
    let section = get_section(record[8], ctx)
        .map_err(|_e| FunctionRecordError::InvalidRecord("section name not found"))?;
    let visibility = get_visibility_opt(Some(record[9]), linkage);
    let gc_name = get_gc_name_opt(record.get(10).copied(), ctx)
        .map_err(|_e| FunctionRecordError::InvalidRecord("gc name not found"))?;

    let unnamed_addr = get_unnamed_addr(record.get(11).copied());

    let prologue = record.get(12).copied();

    let dll_storage_class =
        get_dll_storage_class(record.get(13).copied(), linkage, raw_linkage_value);

    let comdat = match record.get(14).copied() {
        Some(n) if n != 0 => {
            let comdat = ctx
                .comdat
                .get((n - 1) as usize)
                .cloned()
                .ok_or(FunctionRecordError::InvalidRecord("comdat not found"))?;

            Some(comdat)
        }
        None if has_implicit_comdat(raw_linkage_value) => {
            return Err(FunctionRecordError::ImplicitComdatUnsupported);
        }
        Some(_) | None => None,
    };

    let prefix = record.get(15).copied();
    let personality_fn = record.get(16).copied();
    let preemption_specifier =
        get_preemption_specifier(record.get(17).copied(), visibility, linkage);

    // TODO: Use default address space if missing.
    let address_space = record.get(18).copied();

    let partition = if let (Some(&offset), Some(&size)) = (record.get(19), record.get(20)) {
        if size == 0 {
            None
        } else {
            let partition =
                ctx.strtab()
                    .get(offset, size)
                    .ok_or(FunctionRecordError::InvalidRecord(
                        "failed to get partition name",
                    ))?;
            Some(partition)
        }
    } else {
        None
    };

    let function = Function {
        name,
        ty,
        calling_convention,
        is_proto,
        linkage,
        attributes,
        alignment,
        visibility,
        unnamed_addr,
        dll_storage_class,
        preemption_specifier,
        section,
        gc: gc_name,
        prologue_data: prologue,
        comdat,
        prefix_data: prefix,
        personality_fn,
        addrspace: address_space,
        partition,
    };

    Ok(function)
}
