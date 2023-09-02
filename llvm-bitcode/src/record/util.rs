use std::rc::Rc;

use num_enum::TryFromPrimitive;

use crate::{
    context::{Context, ContextError},
    ir::{
        AttributeList, CallingConvention, DllStorageClass, Linkage, PreemptionSpecifier,
        ThreadLocalMode, UnnamedAddr, Visibility,
    },
    util::{fields::IncompleteRecordError, types::Type, value::Value},
    FieldsIter,
};

/// Returns `true` if the given linkage has implicit comdat.
pub(super) fn has_implicit_comdat(raw_linkage_value: u64) -> bool {
    // These values match
    // - Old WeakAnyLinkage (1).
    // - Old LinkOnceAnyLinkage (4).
    // - Old WeakODRLinkage (10).
    // - Old LinkOnceODRLinkage (11).
    matches!(raw_linkage_value, 1 | 4 | 10 | 11)
}

/// Convert an integer value to a [Linkage].
///
/// Remaps the following obselete linkages
///
/// - `DllImportLinkage` (5) -> `External`.
/// - `DllExportLinkage` (6) -> `External`.
/// - `LinkerPrivateLinkage` (13) -> `Private`.
/// - `LinkerPrivateWeakLinkage` (14) -> `Private`.
pub(super) fn decode_linkage(value: u64) -> Linkage {
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

/// Decode `value` to [`Visibility`].
///
/// Missing values, or if linkage is local, are mapped to [`Visibility::Default`].
pub(super) fn get_visibility_opt(value: Option<u64>, linkage: Linkage) -> Visibility {
    // Local linkage must have default visibility. Upgrade `Hidden` and `Protected` for old bitcode.
    match value {
        Some(visibility) if !linkage.is_local() => decode_visibility(visibility),
        _ => Visibility::Default,
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

/// Decode `value` to a [`DllStorageClass`].
///
/// Sets default for local linkage and updrages missing values.
pub(super) fn get_dll_storage_class(
    value: Option<u64>,
    linkage: Linkage,
    raw_linkage_value: u64,
) -> DllStorageClass {
    if linkage.is_local() {
        // Global value with local linkage cannot have a dll storage class.
        return DllStorageClass::Default;
    }

    match value {
        Some(value) => decode_dll(value),
        None => upgrade_dll_import_export_linkage(raw_linkage_value),
    }
}

fn upgrade_dll_import_export_linkage(value: u64) -> DllStorageClass {
    match value {
        5 => DllStorageClass::Import,
        6 => DllStorageClass::Export,
        _ => DllStorageClass::Default,
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

/// Decode `value` to an [`UnnamedAddr`].
///
/// If the value does not exist, or contains an unknown value the default [`UnnamedAddr::None`] is returned.
pub(super) fn get_unnamed_addr(value: Option<u64>) -> UnnamedAddr {
    value.map_or(UnnamedAddr::None, decode_unnamed_addr)
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

/// Decode `value` to [`ThreadLocalMode`].
///
/// - Missing values are mapped to [`ThreadLocalMode::NotThreadLocal`].
/// - Unknown values are mapped to [`ThreadLocalMode::GeneralDynamic`].
pub(super) fn get_thread_local(value: Option<u64>) -> ThreadLocalMode {
    value.map_or(ThreadLocalMode::NotThreadLocal, decode_thread_local)
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

#[derive(Debug, TryFromPrimitive)]
#[repr(u64)]
#[allow(non_camel_case_types)]
enum BitcodeCallingConvention {
    C = 0,
    Fast = 8,
    Cold = 9,
    GHC = 10,
    HiPE = 11,
    WebKit_JS = 12,
    AnyReg = 13,
    PreserveMost = 14,
    PreserveAll = 15,
    Swift = 16,
    CXX_FAST_TLS = 17,
    Tail = 18,
    CFGuard_Check = 19,
    SwiftTail = 20,
    X86_StdCall = 64,
    X86_FastCall = 65,
    ARM_APCS = 66,
    ARM_AAPCS = 67,
    ARM_AAPCS_VFP = 68,
    MSP430_INTR = 69,
    X86_ThisCall = 70,
    PTX_Kernel = 71,
    PTX_Device = 72,
    SPIR_FUNC = 75,
    SPIR_KERNEL = 76,
    Intel_OCL_BI = 77,
    X86_64_SysV = 78,
    Win64 = 79,
    X86_VectorCall = 80,
    DUMMY_HHVM = 81,
    DUMMY_HHVM_C = 82,
    X86_INTR = 83,
    AVR_INTR = 84,
    AVR_SIGNAL = 85,
    AVR_BUILTIN = 86,
    AMDGPU_VS = 87,
    AMDGPU_GS = 88,
    AMDGPU_PS = 89,
    AMDGPU_CS = 90,
    AMDGPU_KERNEL = 91,
    X86_RegCall = 92,
    AMDGPU_HS = 93,
    MSP430_BUILTIN = 94,
    AMDGPU_LS = 95,
    AMDGPU_ES = 96,
    AArch64_VectorCall = 97,
    AArch64_SVE_VectorCall = 98,
    WASM_EmscriptenInvoke = 99,
    AMDGPU_Gfx = 100,
    M68k_INTR = 101,
    AArch64_SME_ABI_Support_Routines_PreserveMost_From_X0 = 102,
    AArch64_SME_ABI_Support_Routines_PreserveMost_From_X2 = 103,
}

pub(super) fn decode_calling_convention(value: u64) -> Option<CallingConvention> {
    if value > CallingConvention::MAX_ID {
        return None;
    }

    let cc = match BitcodeCallingConvention::try_from(value) {
        Ok(value) => match value {
            BitcodeCallingConvention::C => CallingConvention::C,
            BitcodeCallingConvention::Fast => CallingConvention::Fast,
            BitcodeCallingConvention::Cold => CallingConvention::Cold,
            BitcodeCallingConvention::GHC => CallingConvention::GHC,
            BitcodeCallingConvention::HiPE => CallingConvention::HiPE,
            BitcodeCallingConvention::WebKit_JS => CallingConvention::WebkitJs,
            BitcodeCallingConvention::AnyReg => CallingConvention::AnyReg,
            BitcodeCallingConvention::PreserveMost => CallingConvention::PreserveMost,
            BitcodeCallingConvention::PreserveAll => CallingConvention::PreserveAll,
            BitcodeCallingConvention::Swift => CallingConvention::Swift,
            BitcodeCallingConvention::CXX_FAST_TLS => CallingConvention::CxxFastTls,
            BitcodeCallingConvention::Tail => CallingConvention::Tail,
            BitcodeCallingConvention::CFGuard_Check => CallingConvention::CfGuardCheck,
            BitcodeCallingConvention::SwiftTail => CallingConvention::SwiftTail,
            BitcodeCallingConvention::X86_StdCall => CallingConvention::X86StdCall,
            BitcodeCallingConvention::X86_FastCall => CallingConvention::X86FastCall,
            BitcodeCallingConvention::ARM_APCS => CallingConvention::ArmApcs,
            BitcodeCallingConvention::ARM_AAPCS => CallingConvention::ArmAapcs,
            BitcodeCallingConvention::ARM_AAPCS_VFP => CallingConvention::ArmAapcsVfp,
            BitcodeCallingConvention::MSP430_INTR => CallingConvention::Msp430Intr,
            BitcodeCallingConvention::X86_ThisCall => CallingConvention::X86ThisCall,
            BitcodeCallingConvention::PTX_Kernel => CallingConvention::PtxKernel,
            BitcodeCallingConvention::PTX_Device => CallingConvention::PtxDevice,
            BitcodeCallingConvention::SPIR_FUNC => CallingConvention::SpirFunc,
            BitcodeCallingConvention::SPIR_KERNEL => CallingConvention::SpirKernel,
            BitcodeCallingConvention::Intel_OCL_BI => CallingConvention::IntelOclBi,
            BitcodeCallingConvention::X86_64_SysV => CallingConvention::X8664SysV,
            BitcodeCallingConvention::Win64 => CallingConvention::Win64,
            BitcodeCallingConvention::X86_VectorCall => CallingConvention::X86VectorCall,
            BitcodeCallingConvention::DUMMY_HHVM => CallingConvention::DummyHhvm,
            BitcodeCallingConvention::DUMMY_HHVM_C => CallingConvention::DummyHhvmC,
            BitcodeCallingConvention::X86_INTR => CallingConvention::X86Intr,
            BitcodeCallingConvention::AVR_INTR => CallingConvention::AvrIntr,
            BitcodeCallingConvention::AVR_SIGNAL => CallingConvention::AvrSignal,
            BitcodeCallingConvention::AVR_BUILTIN => CallingConvention::AvrBuiltin,
            BitcodeCallingConvention::AMDGPU_VS => CallingConvention::AmdGpuVs,
            BitcodeCallingConvention::AMDGPU_GS => CallingConvention::AmdGpuGs,
            BitcodeCallingConvention::AMDGPU_PS => CallingConvention::AmdGpuPs,
            BitcodeCallingConvention::AMDGPU_CS => CallingConvention::AmdGpuCs,
            BitcodeCallingConvention::AMDGPU_KERNEL => CallingConvention::AmdGpuKernel,
            BitcodeCallingConvention::X86_RegCall => CallingConvention::X86RegCall,
            BitcodeCallingConvention::AMDGPU_HS => CallingConvention::AmdGpuHs,
            BitcodeCallingConvention::MSP430_BUILTIN => CallingConvention::Msp430Builtin,
            BitcodeCallingConvention::AMDGPU_LS => CallingConvention::AmgGpuLs,
            BitcodeCallingConvention::AMDGPU_ES => CallingConvention::AmdGpuEs,
            BitcodeCallingConvention::AArch64_VectorCall => CallingConvention::Aarch64VectorCall,
            BitcodeCallingConvention::AArch64_SVE_VectorCall => {
                CallingConvention::AArch64SveVectorCall
            }
            BitcodeCallingConvention::WASM_EmscriptenInvoke => {
                CallingConvention::WasmEmscriptenInvoke
            }
            BitcodeCallingConvention::AMDGPU_Gfx => CallingConvention::AmgGpuGfx,
            BitcodeCallingConvention::M68k_INTR => CallingConvention::M68kIntr,
            BitcodeCallingConvention::AArch64_SME_ABI_Support_Routines_PreserveMost_From_X0 => {
                CallingConvention::AarchSmeAbiSupportRoutinesPreserveMostFromX0
            }
            BitcodeCallingConvention::AArch64_SME_ABI_Support_Routines_PreserveMost_From_X2 => {
                CallingConvention::AarchSmeAbiSupportRoutinesPreserveMostFromX2
            }
        },
        Err(_) => CallingConvention::Other(value as u32),
    };

    Some(cc)
}

/// Returns the attributes from `value` if they exists, otherwise `None`.
///
/// # Errors
///
/// If a section index is given, but the section does not exist `()` is returned.
pub(super) fn get_attributes_opt(
    value: Option<u64>,
    ctx: &Context,
) -> Result<Option<Rc<AttributeList>>, ()> {
    match value {
        Some(value) => get_attributes(value, ctx),
        None => Ok(None),
    }
}

/// Returns the attributes from `value` if they exists, otherwise `None`.
///
/// # Errors
///
/// If a section index is given, but the section does not exist `()` is returned.
pub(super) fn get_attributes(value: u64, ctx: &Context) -> Result<Option<Rc<AttributeList>>, ()> {
    // Sections are 1-indexed if available.
    Ok(match value {
        0 => None,
        n => {
            let attributes = ctx.attributes.get((n - 1) as usize).cloned().ok_or(())?;
            Some(attributes)
        }
    })
}

/// Returns the section string from `value` if it exists, otherwise `None`.
///
/// # Errors
///
/// If a section index is given, but the section does not exist `()` is returned.
pub(super) fn get_section_opt(value: Option<u64>, ctx: &Context) -> Result<Option<String>, ()> {
    match value {
        Some(value) => get_section(value, ctx),
        None => Ok(None),
    }
}

/// Returns the section string from `value` if it exists, otherwise `None`.
///
/// # Errors
///
/// If a section index is given, but the section does not exist `()` is returned.
pub(super) fn get_section(value: u64, ctx: &Context) -> Result<Option<String>, ()> {
    // Sections are 1-indexed if available.
    Ok(match value {
        0 => None,
        n => {
            let section = ctx.section_table.get((n - 1) as usize).cloned().ok_or(())?;
            Some(section)
        }
    })
}

/// Returns the gc string from `value` if it exists, otherwise `None`.
///
/// # Errors
///
/// If a section index is given, but the section does not exist `()` is returned.
pub(super) fn get_gc_name_opt(value: Option<u64>, ctx: &Context) -> Result<Option<String>, ()> {
    Ok(match value {
        Some(0) | None => None,
        Some(n) => {
            let gc_name = ctx.gc_table.get((n - 1) as usize).cloned().ok_or(())?;
            Some(gc_name)
        }
    })
}

/// Decode `value` to [`PreemptionSpecifier`].
pub(super) fn get_preemption_specifier(
    value: Option<u64>,
    visibility: Visibility,
    linkage: Linkage,
) -> PreemptionSpecifier {
    let has_default_visibility = matches!(visibility, Visibility::Default);
    let has_external_weak_linkage = matches!(linkage, Linkage::ExternalWeak);

    if linkage.is_local() || !(has_default_visibility || has_external_weak_linkage) {
        PreemptionSpecifier(true)
    } else {
        value.map_or(PreemptionSpecifier(false), decode_preemption_specifier)
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

const USE_RELATIVE_IDS: bool = true;

/// Returns the value and type from the given record.
///
///
pub fn get_value_type_pair<'a, const N: usize, E>(
    record: &mut FieldsIter<'a, N>,
    next_value_number: u64,
    ctx: &Context,
) -> Result<(Rc<Value>, Rc<Type>), E>
where
    E: From<ContextError> + From<IncompleteRecordError>,
{
    let value_id = record.next_or_err()?;
    let value_id = if USE_RELATIVE_IDS {
        next_value_number - value_id
    } else {
        value_id
    };

    // If the value is not a forward reference just return it.
    let (value, ty) = if value_id < next_value_number {
        let ty = ctx.get_value_type(value_id)?;
        let value = ctx.get_value(value_id)?;
        (value, ty)
    } else {
        let tid = record.next_or_err()?;
        let ty = ctx.get_ty(tid)?;
        let value = ctx.get_value(value_id)?;
        (value, ty)
    };

    Ok((value, ty))
}

pub fn get_value(
    id: u64,
    next_value_number: u64,
    ctx: &Context,
) -> Result<Rc<Value>, ContextError> {
    let id = id as u32;
    let next_value_number = next_value_number as u32;

    let id = if USE_RELATIVE_IDS {
        next_value_number.wrapping_sub(id)
    } else {
        id
    };
    ctx.get_value(id as u64)
}
