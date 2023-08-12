mod attribute;
mod basic_block;
mod float;
mod function;
mod global_variable;
mod int;

pub use attribute::{
    Attribute, AttributeGroup, AttributeList, EnumAttribute, IntAttribute, Location, MemoryEffects,
    MemoryEffectsFlags, TypeAttribute, UnwindTableKind,
};
pub use function::{CallingConvention, Function};
pub use global_variable::{GlobalVariable, SanitizerMetadata};
pub use int::APInt;

#[derive(Debug, Clone, PartialEq)]
pub struct Comdat;

/// Linkage for global values.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Linkage {
    /// Externally visible function
    External,

    /// Available for inspection, not emission.
    AvailableExternally,

    /// Keep one copy of the function when linking (inline).
    LinkOnceAny,

    /// Same as `LinkOnceAny`, but only replaced by something equivalent.
    LinkOnceOdr,

    /// Keep one copy of named function when linking (weak).
    WeakAny,

    /// Same as `WeakAny`, but only replaced by something equivalent.
    WeakOdr,

    /// Special purpose, only applies to global arrays.
    Appending,

    /// Rename collisions when linking (static functions).
    Internal,

    /// Like `Internal`, but omit from symbol table.
    Private,

    /// todo
    ExternalWeak,

    /// Tentative definitions.
    Common,
}

impl Linkage {
    /// Returns `true` if `Linkage` is local.
    ///
    /// Local linkage is for `Internal` and `Private`.
    pub fn is_local(&self) -> bool {
        matches!(self, Linkage::Internal | Linkage::Private)
    }

    pub fn is_valid_declaration(&self) -> bool {
        matches!(self, Linkage::ExternalWeak | Linkage::External)
    }

    /// Returns `true` if the defintion may be replaced by something non-equivalent at link time.
    ///
    /// For example, if a function has weak linkage then the code defining it may be replaced by
    /// different code.
    pub fn is_interposable(&self) -> bool {
        match self {
            Linkage::WeakAny |
            Linkage::LinkOnceAny |
            Linkage::ExternalWeak |
            Linkage::Common => true,

            // These first three can be overriden, but not re-defined.
            Linkage::AvailableExternally |
            Linkage::LinkOnceOdr |
            Linkage::WeakOdr |

            // Others.
            Linkage::External |
            Linkage::Appending |
            Linkage::Internal |
            Linkage::Private => false,
        }
    }
}

impl std::fmt::Display for Linkage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Linkage::External => write!(f, "External"),
            Linkage::AvailableExternally => write!(f, "AvailableExternally"),
            Linkage::LinkOnceAny => write!(f, "LinkOnceAny"),
            Linkage::LinkOnceOdr => write!(f, "linkonce_odr"),
            Linkage::WeakAny => write!(f, "WeakAny"),
            Linkage::WeakOdr => write!(f, "WeakOdr"),
            Linkage::Appending => write!(f, "Appending"),
            Linkage::Internal => write!(f, "internal"),
            Linkage::Private => write!(f, "private"),
            Linkage::ExternalWeak => write!(f, "ExternalWeak"),
            Linkage::Common => write!(f, "Common"),
        }
    }
}

/// Visibility of global values.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Visibility {
    /// Global value is visible.
    Default,

    /// Global value is hidden.
    Hidden,

    /// Global value is protected.
    Protected,
}

impl std::fmt::Display for Visibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Visibility::Default => write!(f, "Default"),
            Visibility::Hidden => write!(f, "hidden"),
            Visibility::Protected => write!(f, "Protected"),
        }
    }
}

/// Storage classes of global values for PE targets.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum DllStorageClass {
    Default,

    /// Function can be imported from DLL.
    Import,

    /// Function can be accessible from DLL.
    Export,
}

impl std::fmt::Display for DllStorageClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DllStorageClass::Default => write!(f, "Default"),
            DllStorageClass::Import => write!(f, "Import"),
            DllStorageClass::Export => write!(f, "Export"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum ThreadLocalMode {
    NotThreadLocal,
    GeneralDynamic,
    LocalDynamic,
    InitialExec,
    LocalExec,
}

impl std::fmt::Display for ThreadLocalMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ThreadLocalMode::NotThreadLocal => write!(f, "NotThreadLocal"),
            ThreadLocalMode::GeneralDynamic => write!(f, "GeneralDynamic"),
            ThreadLocalMode::LocalDynamic => write!(f, "LocalDynamic"),
            ThreadLocalMode::InitialExec => write!(f, "InitialExec"),
            ThreadLocalMode::LocalExec => write!(f, "LocalExec"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum UnnamedAddr {
    None,
    Local,
    Global,
}

impl UnnamedAddr {
    pub fn at_least_local(&self) -> bool {
        !matches!(self, Self::None)
    }
}

impl std::fmt::Display for UnnamedAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnnamedAddr::None => write!(f, "none"),
            UnnamedAddr::Local => write!(f, "local"),
            UnnamedAddr::Global => write!(f, "unnamed_addr"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct PreemptionSpecifier(pub bool);

impl std::fmt::Display for PreemptionSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            false => write!(f, "false"),
            true => write!(f, "true"),
        }
    }
}
