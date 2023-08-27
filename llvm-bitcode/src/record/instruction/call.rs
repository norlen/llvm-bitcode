use std::rc::Rc;

use crate::{
    context::Context,
    util::{types::Type, value::Value},
    Fields,
};

use super::Instruction;

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum CallError {
    /// Not enough fields in the record.
    #[error("not enough fields in the record")]
    IncompleteRecord,

    /// Failed to parse record.
    #[error("Failed to parse record: {0}")]
    InvalidRecord(&'static str),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub attributes: u64,
    pub cc: u64,
    pub function_ty: Rc<Type>,
    pub function: Rc<Value>,
    pub arguments: Vec<u64>,
}

impl std::fmt::Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // <result> = [tail | musttail | notail ] call [fast-math flags] [cconv] [ret attrs] [addrspace(<num>)]
        //   <ty>|<fnty> <fnptrval>(<function args>) [fn attrs] [ operand bundles ]
        let rty = match self.function_ty.as_ref() {
            Type::Function(f) => format!("{}", f.return_ty),
            _ => "not_fn_ty".to_owned(),
        };
        let fn_name = match self.function.as_ref() {
            Value::Function(f) => f.name.to_owned(),
            _ => "not_fn_value".to_owned(),
        };

        write!(
            f,
            "call {} {} {:?} ({:?}) {}",
            self.cc, rty, fn_name, self.arguments, self.attributes
        )
    }
}

pub fn parse_instruction_call<const N: usize>(
    record: &Fields<N>,
    ctx: &Context,
) -> Result<Instruction, CallError> {
    if record.len() < 5 {
        return Err(CallError::IncompleteRecord);
    }
    todo!()
}
