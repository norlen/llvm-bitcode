use std::rc::Rc;

use crate::util::types::Type;

use super::{
    attribute::AttributeList, Comdat, DllStorageClass, Linkage, PreemptionSpecifier,
    ThreadLocalMode, UnnamedAddr, Visibility,
};

#[derive(Debug, Clone, PartialEq)]
pub struct SanitizerMetadata {
    pub no_address: bool,
    pub no_hw_address: bool,
    pub memtag: bool,
    pub is_dyn_init: bool,
}

/// Global variable.
///
///
#[derive(Debug, Clone, PartialEq)]
pub struct GlobalVariable {
    pub name: String,

    pub ty: Rc<Type>,

    pub is_constant: bool,

    pub address_space: u64,

    pub init_id: Option<()>,

    pub linkage: Linkage,

    pub alignment: Option<u64>,

    pub section: Option<String>,

    pub visibility: Visibility,

    pub thread_local: ThreadLocalMode,

    pub unnamed_addr: UnnamedAddr,

    pub is_externally_initialized: bool,

    pub dll_storage_class: DllStorageClass,

    pub comdat: Option<Comdat>,

    pub attributes: Option<Rc<AttributeList>>,

    pub preemption_specifier: PreemptionSpecifier,

    pub partition: Option<String>,

    pub sanitizer_metadata: SanitizerMetadata,
}

impl std::fmt::Display for GlobalVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let is_constant = if self.is_constant { "constant" } else { "" };
        write!(
            f,
            "@{} = {} {} {} {}",
            self.name, self.linkage, self.unnamed_addr, is_constant, self.ty
        )
    }
}
