use smallvec::SmallVec;
use std::rc::Rc;

use crate::{
    context::Context,
    util::{types::Type, value::Value},
};

use super::Instruction;

#[derive(Debug, Clone, PartialEq)]
pub struct Add {
    pub return_ty: Rc<Type>,
    pub lhs: Rc<Value>,
    pub rhs: Rc<Value>,
    pub no_signed_wrap: bool,
    pub no_unsigned_wrap: bool,
}

impl Add {
    pub fn new(
        return_ty: Rc<Type>,
        lhs: Rc<Value>,
        rhs: Rc<Value>,
        no_signed_wrap: bool,
        no_unsigned_wrap: bool,
    ) -> Self {
        Self {
            return_ty,
            lhs,
            rhs,
            no_signed_wrap,
            no_unsigned_wrap,
        }
    }
}

impl std::fmt::Display for Add {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let nsw = if self.no_signed_wrap { "nsw " } else { "" };
        let nuw = if self.no_unsigned_wrap { "nuw " } else { "" };
        write!(f, "add {}{}{:?} {:?}", nsw, nuw, self.lhs, self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Sub {
    pub return_ty: Rc<Type>,
    pub lhs: Rc<Value>,
    pub rhs: Rc<Value>,
    pub no_signed_wrap: bool,
    pub no_unsigned_wrap: bool,
}

impl Sub {
    pub fn new(
        return_ty: Rc<Type>,
        lhs: Rc<Value>,
        rhs: Rc<Value>,
        no_signed_wrap: bool,
        no_unsigned_wrap: bool,
    ) -> Self {
        Self {
            return_ty,
            lhs,
            rhs,
            no_signed_wrap,
            no_unsigned_wrap,
        }
    }
}

impl std::fmt::Display for Sub {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let nsw = if self.no_signed_wrap { "nsw " } else { "" };
        let nuw = if self.no_unsigned_wrap { "nuw " } else { "" };
        write!(f, "sub {}{}{:?} {:?}", nsw, nuw, self.lhs, self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Mul {
    pub return_ty: Rc<Type>,
    pub lhs: Rc<Value>,
    pub rhs: Rc<Value>,
    pub no_signed_wrap: bool,
    pub no_unsigned_wrap: bool,
}

impl Mul {
    pub fn new(
        return_ty: Rc<Type>,
        lhs: Rc<Value>,
        rhs: Rc<Value>,
        no_signed_wrap: bool,
        no_unsigned_wrap: bool,
    ) -> Self {
        Self {
            return_ty,
            lhs,
            rhs,
            no_signed_wrap,
            no_unsigned_wrap,
        }
    }
}

impl std::fmt::Display for Mul {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let nsw = if self.no_signed_wrap { "nsw " } else { "" };
        let nuw = if self.no_unsigned_wrap { "nuw " } else { "" };
        write!(f, "mul {}{}{:?} {:?}", nsw, nuw, self.lhs, self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UDiv {
    pub return_ty: Rc<Type>,
    pub lhs: Rc<Value>,
    pub rhs: Rc<Value>,
    pub exact: bool,
}

impl UDiv {
    pub fn new(return_ty: Rc<Type>, lhs: Rc<Value>, rhs: Rc<Value>, exact: bool) -> Self {
        Self {
            return_ty,
            lhs,
            rhs,
            exact,
        }
    }
}

impl std::fmt::Display for UDiv {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let exact = if self.exact { "exact " } else { "" };
        write!(f, "sub {exact}{:?} {:?}", self.lhs, self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SDiv {
    pub return_ty: Rc<Type>,
    pub lhs: Rc<Value>,
    pub rhs: Rc<Value>,
    pub exact: bool,
}

impl SDiv {
    pub fn new(return_ty: Rc<Type>, lhs: Rc<Value>, rhs: Rc<Value>, exact: bool) -> Self {
        Self {
            return_ty,
            lhs,
            rhs,
            exact,
        }
    }
}

impl std::fmt::Display for SDiv {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let exact = if self.exact { "exact " } else { "" };
        write!(f, "sub {exact}{:?} {:?}", self.lhs, self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct URem {
    pub return_ty: Rc<Type>,
    pub lhs: Rc<Value>,
    pub rhs: Rc<Value>,
}

impl URem {
    pub fn new(return_ty: Rc<Type>, lhs: Rc<Value>, rhs: Rc<Value>) -> Self {
        Self {
            return_ty,
            lhs,
            rhs,
        }
    }
}

impl std::fmt::Display for URem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "sub {:?} {:?}", self.lhs, self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SRem {
    pub return_ty: Rc<Type>,
    pub lhs: Rc<Value>,
    pub rhs: Rc<Value>,
}

impl SRem {
    pub fn new(return_ty: Rc<Type>, lhs: Rc<Value>, rhs: Rc<Value>) -> Self {
        Self {
            return_ty,
            lhs,
            rhs,
        }
    }
}

impl std::fmt::Display for SRem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "sub {:?} {:?}", self.lhs, self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Shl {
    pub return_ty: Rc<Type>,
    pub lhs: Rc<Value>,
    pub rhs: Rc<Value>,
    pub no_signed_wrap: bool,
    pub no_unsigned_wrap: bool,
}

impl Shl {
    pub fn new(
        return_ty: Rc<Type>,
        lhs: Rc<Value>,
        rhs: Rc<Value>,
        no_signed_wrap: bool,
        no_unsigned_wrap: bool,
    ) -> Self {
        Self {
            return_ty,
            lhs,
            rhs,
            no_signed_wrap,
            no_unsigned_wrap,
        }
    }
}

impl std::fmt::Display for Shl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let nsw = if self.no_signed_wrap { "nsw " } else { "" };
        let nuw = if self.no_unsigned_wrap { "nuw " } else { "" };
        write!(f, "sub {}{}{:?} {:?}", nsw, nuw, self.lhs, self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LShr {
    pub return_ty: Rc<Type>,
    pub lhs: Rc<Value>,
    pub rhs: Rc<Value>,
    pub exact: bool,
}

impl LShr {
    pub fn new(return_ty: Rc<Type>, lhs: Rc<Value>, rhs: Rc<Value>, exact: bool) -> Self {
        Self {
            return_ty,
            lhs,
            rhs,
            exact,
        }
    }
}

impl std::fmt::Display for LShr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let exact = if self.exact { "exact " } else { "" };
        write!(f, "sub {exact}{:?} {:?}", self.lhs, self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AShr {
    pub return_ty: Rc<Type>,
    pub lhs: Rc<Value>,
    pub rhs: Rc<Value>,
    pub exact: bool,
}

impl AShr {
    pub fn new(return_ty: Rc<Type>, lhs: Rc<Value>, rhs: Rc<Value>, exact: bool) -> Self {
        Self {
            return_ty,
            lhs,
            rhs,
            exact,
        }
    }
}

impl std::fmt::Display for AShr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let exact = if self.exact { "exact " } else { "" };
        write!(f, "sub {exact}{:?} {:?}", self.lhs, self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct And {
    pub return_ty: Rc<Type>,
    pub lhs: Rc<Value>,
    pub rhs: Rc<Value>,
}

impl And {
    pub fn new(return_ty: Rc<Type>, lhs: Rc<Value>, rhs: Rc<Value>) -> Self {
        Self {
            return_ty,
            lhs,
            rhs,
        }
    }
}

impl std::fmt::Display for And {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "sub {:?} {:?}", self.lhs, self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Or {
    pub return_ty: Rc<Type>,
    pub lhs: Rc<Value>,
    pub rhs: Rc<Value>,
}

impl Or {
    pub fn new(return_ty: Rc<Type>, lhs: Rc<Value>, rhs: Rc<Value>) -> Self {
        Self {
            return_ty,
            lhs,
            rhs,
        }
    }
}

impl std::fmt::Display for Or {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "sub {:?} {:?}", self.lhs, self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Xor {
    pub return_ty: Rc<Type>,
    pub lhs: Rc<Value>,
    pub rhs: Rc<Value>,
}

impl Xor {
    pub fn new(return_ty: Rc<Type>, lhs: Rc<Value>, rhs: Rc<Value>) -> Self {
        Self {
            return_ty,
            lhs,
            rhs,
        }
    }
}

impl std::fmt::Display for Xor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "sub {:?} {:?}", self.lhs, self.rhs)
    }
}

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum BinopError {
    /// Not enough fields in the record.
    #[error("not enough fields in the record")]
    IncompleteRecord,

    /// Failed to parse record.
    #[error("Failed to parse record: {0}")]
    InvalidRecord(&'static str),
}

pub fn parse_instruction_binop<const N: usize>(
    record: &SmallVec<[u64; N]>,
    ctx: &Context,
) -> Result<Instruction, BinopError> {
    todo!()
}
