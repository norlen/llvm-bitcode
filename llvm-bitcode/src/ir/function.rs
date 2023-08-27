use std::rc::Rc;

use crate::util::types::Type;

use super::{
    AttributeList, Comdat, DllStorageClass, Linkage, PreemptionSpecifier, UnnamedAddr, Visibility,
};

/// IR Function.
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: String,

    pub ty: Rc<Type>,

    pub calling_convention: CallingConvention,

    pub is_proto: bool,
    pub linkage: Linkage,
    pub attributes: Option<Rc<AttributeList>>,
    pub alignment: Option<u64>,

    pub visibility: Visibility,
    pub unnamed_addr: UnnamedAddr,
    pub dll_storage_class: DllStorageClass,
    pub preemption_specifier: PreemptionSpecifier,

    pub section: Option<String>,
    pub gc: Option<String>,

    pub prologue_data: Option<u64>,
    pub comdat: Option<Comdat>,
    pub prefix_data: Option<u64>,
    pub personality_fn: Option<u64>,
    pub addrspace: Option<u64>,
    pub partition: Option<String>,
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "define @{}", self.name)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CallingConvention {
    C,
    Fast,
    Cold,
    GHC,
    HiPE,
    WebkitJs,
    AnyReg,
    PreserveMost,
    PreserveAll,
    Swift,
    CxxFastTls,
    Tail,
    CfGuardCheck,
    SwiftTail,
    X86StdCall,
    X86FastCall,
    ArmApcs,
    ArmAapcs,
    ArmAapcsVfp,
    Msp430Intr,
    X86ThisCall,
    PtxKernel,
    PtxDevice,
    SpirFunc,
    SpirKernel,
    IntelOclBi,
    X8664SysV,
    Win64,
    X86VectorCall,
    DummyHhvm,
    DummyHhvmC,
    X86Intr,
    AvrIntr,
    AvrSignal,
    AvrBuiltin,
    AmdGpuVs,
    AmdGpuGs,
    AmdGpuPs,
    AmdGpuCs,
    AmdGpuKernel,
    X86RegCall,
    AmdGpuHs,
    Msp430Builtin,
    AmgGpuLs,
    AmdGpuEs,
    Aarch64VectorCall,
    AArch64SveVectorCall,
    WasmEmscriptenInvoke,
    AmgGpuGfx,
    M68kIntr,
    AarchSmeAbiSupportRoutinesPreserveMostFromX0,
    AarchSmeAbiSupportRoutinesPreserveMostFromX2,
    Other(u32),
}

impl CallingConvention {
    /// Highest possible id for a calling convention.
    pub const MAX_ID: u64 = 1023;
}
