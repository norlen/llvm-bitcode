mod attribute;
mod attribute_group;
mod constants;
mod function;
mod global_value_summary;
mod identification;
mod module;
mod operand_bundle_tags;
mod string_table;
mod symbol_table;
mod sync_scope_names;
mod types;
mod vst;

pub use attribute::{parse_attribute_block, AttributeError};
pub use attribute_group::{parse_attribute_groups_block, AttributeGroupError};
pub use constants::{parse_constant_block, ConstantError};
pub use identification::{Identification, IdentificationError};
pub use module::{parse_module, ModuleError, ModuleInfo};
pub use operand_bundle_tags::{
    parse_operand_bundle_tags_block, OperandBundleTags, OperandBundleTagsError,
};
pub use string_table::{StringTable, StringTableError};
pub use symbol_table::{SymbolTable, SymbolTableError};
pub use sync_scope_names::{parse_sync_scope_names_block, SyncScopeNames, SyncScopeNamesError};
pub use types::{parse_type_block, TypeList, TypesError};
