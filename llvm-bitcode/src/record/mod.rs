mod function_declaration;
mod global_variable;
mod instruction;
mod util;

pub use function_declaration::{parse_function_record, FunctionRecordError};
pub use global_variable::{parse_global_variable, GlobalVariableError};
