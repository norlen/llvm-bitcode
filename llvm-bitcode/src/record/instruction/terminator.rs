use std::rc::Rc;

use crate::util::{types::Type, value::Value};

#[derive(Debug, Clone, PartialEq)]
pub struct Ret {
    pub ty: Option<Rc<Type>>,
    pub value: Option<Rc<Value>>,
}

impl Ret {
    pub fn new(ty: Rc<Type>, value: Rc<Value>) -> Self {
        Ret {
            ty: Some(ty),
            value: Some(value),
        }
    }

    pub fn void() -> Self {
        Ret {
            ty: None,
            value: None,
        }
    }
}

impl std::fmt::Display for Ret {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match (&self.ty, &self.value) {
            (Some(ty), Some(value)) => write!(f, "ret {ty} {value:?}"),
            _ => write!(f, "ret void"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Br {
    Unconditonal(UnconditionalBr),
    ConditionalBr(ConditionalBr),
}

impl std::fmt::Display for Br {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Br::Unconditonal(br) => write!(f, "{br}"),
            Br::ConditionalBr(br) => write!(f, "{br}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnconditionalBr {
    pub true_dest_id: u64,
}

impl std::fmt::Display for UnconditionalBr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "br label {}", self.true_dest_id)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalBr {
    pub true_dest_id: u64,
    pub false_dest_id: u64,
    pub condition: Rc<Value>,
}

impl std::fmt::Display for ConditionalBr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "br {:?}, label {}, label {}",
            self.condition, self.true_dest_id, self.false_dest_id
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Resume {
    pub ty: Rc<Type>,
    pub value: Rc<Value>,
}

impl Resume {
    pub fn new(ty: Rc<Type>, value: Rc<Value>) -> Self {
        Self { ty, value }
    }
}

impl std::fmt::Display for Resume {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "resume {} {:?}", self.ty, self.value)
    }
}
