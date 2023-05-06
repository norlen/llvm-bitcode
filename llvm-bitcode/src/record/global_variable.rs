use std::rc::Rc;

use smallvec::SmallVec;

use crate::{
    context::Context,
    ir::{
        DllStorageClass, GlobalVariable, Linkage, PreemptionSpecifier, SanitizerMetadata,
        ThreadLocalMode, UnnamedAddr, Visibility,
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

    // Sections are 1-indexed if available.
    let section = match record.get(7).copied() {
        Some(n) if n > 0 => {
            let section = ctx
                .section_table
                .get((n - 1) as usize)
                .cloned()
                .ok_or(GlobalVariableError::InvalidRecord("section name not found"))?;

            Some(section)
        }
        _ => None,
    };

    // Local linkage must have default visibility. Upgrade `Hidden` and `Protected` for old bitcode.
    let visibility = match record.get(8).copied() {
        Some(visibility) if !linkage.is_local() => decode_visibility(visibility),
        _ => Visibility::Default,
    };

    let thread_local = record
        .get(9)
        .copied()
        .map_or(ThreadLocalMode::NotThreadLocal, decode_thread_local);

    let unnamed_addr = record
        .get(10)
        .copied()
        .map_or(UnnamedAddr::None, decode_unnamed_addr);

    let is_externally_initialized = record.get(11).copied().map_or(false, |v| v > 0);

    let dll_storage_class = if linkage.is_local() {
        // Global value with local linkage cannot have a dll storage class.
        DllStorageClass::Default
    } else {
        match record.get(12).copied() {
            Some(value) => decode_dll(value),
            None => upgrade_dll_import_export_linkage(raw_linkage_value),
        }
    };

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

    let attributes = match record.get(14).copied() {
        Some(0) | None => None,
        Some(n) => {
            let attributes = ctx.attributes.get((n - 1) as usize).cloned().ok_or(
                GlobalVariableError::InvalidRecord("could not get attributes"),
            )?;

            Some(attributes)
        }
    };

    let preemption_specifier = {
        let preemption_specifier = record
            .get(15)
            .copied()
            .map_or(PreemptionSpecifier(false), decode_preemption_specifier);

        let has_default_visibility = matches!(visibility, Visibility::Default);
        let has_external_weak_linkage = matches!(linkage, Linkage::ExternalWeak);
        if linkage.is_local() || !(has_default_visibility || has_external_weak_linkage) {
            PreemptionSpecifier(true)
        } else {
            preemption_specifier
        }
    };

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

fn has_implicit_comdat(raw_linkage_value: u64) -> bool {
    // These values match
    // - Old WeakAnyLinkage (1).
    // - Old LinkOnceAnyLinkage (4).
    // - Old WeakODRLinkage (10).
    // - Old LinkOnceODRLinkage (11).
    matches!(raw_linkage_value, 1 | 4 | 10 | 11)
}

fn upgrade_dll_import_export_linkage(value: u64) -> DllStorageClass {
    match value {
        5 => DllStorageClass::Import,
        6 => DllStorageClass::Export,
        _ => DllStorageClass::Default,
    }
}

fn decode_sanitizer_metadata(value: u64) -> SanitizerMetadata {
    SanitizerMetadata {
        no_address: (value & (1 << 0)) > 0,
        no_hw_address: (value & (1 << 1)) > 0,
        memtag: (value & (1 << 2)) > 0,
        is_dyn_init: (value & (1 << 3)) > 0,
    }
}

/// Convert an integer value to a [Linkage].
///
/// There are remapped obselete linkages
///
/// - `DllImportLinkage` (5) -> `External`.
/// - `DllExportLinkage` (6) -> `External`.
/// - `LinkerPrivateLinkage` (13) -> `Private`.
/// - `LinkerPrivateWeakLinkage` (14) -> `Private`.
fn decode_linkage(value: u64) -> Linkage {
    #[allow(clippy::match_same_arms)]
    match value {
        0 | 5 | 6 | 15 => Linkage::External,
        2 => Linkage::Appending,
        3 => Linkage::Internal,
        7 => Linkage::ExternalWeak,
        8 => Linkage::Common,
        9 | 13 | 14 => Linkage::Private,
        12 => Linkage::AvailableExternally,
        1 | 16 => Linkage::WeakAny,
        10 | 17 => Linkage::WeakOdr,
        4 | 18 => Linkage::LinkOnceAny,
        11 | 19 => Linkage::LinkOnceOdr,

        // Map unknown values to `External`.
        _ => Linkage::External,
    }
}

fn decode_visibility(value: u64) -> Visibility {
    #[allow(clippy::match_same_arms)]
    match value {
        0 => Visibility::Default,
        1 => Visibility::Hidden,
        2 => Visibility::Protected,

        // Map unknown values to `Default`.
        _ => Visibility::Default,
    }
}

fn decode_dll(value: u64) -> DllStorageClass {
    #[allow(clippy::match_same_arms)]
    match value {
        0 => DllStorageClass::Default,
        1 => DllStorageClass::Import,
        2 => DllStorageClass::Export,

        // Map unknown values to `Default`.
        _ => DllStorageClass::Default,
    }
}

fn decode_unnamed_addr(value: u64) -> UnnamedAddr {
    #[allow(clippy::match_same_arms)]
    match value {
        0 => UnnamedAddr::None,
        1 => UnnamedAddr::Global,
        2 => UnnamedAddr::Local,

        // Map unknown values to `None`.
        _ => UnnamedAddr::None,
    }
}

fn decode_preemption_specifier(value: u64) -> PreemptionSpecifier {
    #[allow(clippy::match_same_arms)]
    match value {
        0 => PreemptionSpecifier(false),
        1 => PreemptionSpecifier(true),

        // Map unknown values to `true`.
        _ => PreemptionSpecifier(true),
    }
}

fn decode_thread_local(value: u64) -> ThreadLocalMode {
    #[allow(clippy::match_same_arms)]
    match value {
        0 => ThreadLocalMode::NotThreadLocal,
        1 => ThreadLocalMode::GeneralDynamic,
        2 => ThreadLocalMode::LocalDynamic,
        3 => ThreadLocalMode::InitialExec,
        4 => ThreadLocalMode::LocalExec,

        // Map unknown values to `GeneralDynamic`.
        _ => ThreadLocalMode::GeneralDynamic,
    }
}
