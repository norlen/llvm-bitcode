use std::rc::Rc;

use crate::{
    context::{Context, ContextError},
    record::util::get_value_type_pair,
    util::{fields::IncompleteRecordError, parse_alignment, types::Type, value::Value},
    Fields, FieldsIter,
};

use super::Instruction;

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum StoreInstructionError {
    #[error(transparent)]
    IncompleteRecord(#[from] IncompleteRecordError),

    #[error(transparent)]
    ContextError(#[from] ContextError),

    /// Failed to parse record.
    #[error("Failed to parse record: {0}")]
    InvalidRecord(&'static str),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Store {
    pub pointer_ty: Rc<Type>,
    pub pointer: Rc<Value>,
    pub value_ty: Rc<Type>,
    pub value: Rc<Value>,
    pub alignment: Option<u64>,
    pub volatile: u64,
}

impl std::fmt::Display for Store {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "store {} to {}", self.value, self.pointer)
    }
}

pub fn parse_instruction_store<const N: usize>(
    record: &Fields<N>,
    ctx: &Context,
    next_value_number: u64,
) -> Result<Instruction, StoreInstructionError> {
    // STORE_OLD: [ptr_ty, ptr_val, value, alignment, volatile]
    // STORE:     [ptr_ty, ptr_val, value_ty, value, alignment, volatile]
    let mut record = FieldsIter::from(record);

    let (pointer, pointer_ty) =
        get_value_type_pair::<N, StoreInstructionError>(&mut record, next_value_number, ctx)?;
    let (value, value_ty) =
        get_value_type_pair::<N, StoreInstructionError>(&mut record, next_value_number, ctx)?;
    let alignment = parse_alignment(record.next_or_err()?);
    let volatile = record.next_or_err()?;

    Ok(Instruction::Store(Store {
        pointer_ty,
        pointer,
        value_ty,
        value,
        alignment,
        volatile,
    }))
}
