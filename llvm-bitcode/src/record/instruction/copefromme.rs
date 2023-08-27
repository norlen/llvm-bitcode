use smallvec::SmallVec;
use std::rc::Rc;

use crate::{
    context::Context,
    util::{types::Type, value::Value},
};

use super::Instruction;

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum MyError {
    /// Not enough fields in the record.
    #[error("not enough fields in the record")]
    IncompleteRecord,

    /// Failed to parse record.
    #[error("Failed to parse record: {0}")]
    InvalidRecord(&'static str),
}

pub fn parse_instruction_my<const N: usize>(
    record: &SmallVec<[u64; N]>,
    ctx: &Context,
) -> Result<Instruction, MyError> {
    todo!()
}
