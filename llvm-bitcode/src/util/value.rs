use std::{collections::HashMap, rc::Rc};

/// Values used throughout the bitcode.
///
///
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    /// Constant value, can either be at the module scope or constant inside a function block.
    Constant,

    /// Global variable at the module scope.
    GlobalVariable,

    /// Function declaration.
    Function,

    /// Argument passed to a function, should only exist in a function block scope.
    Argument,

    /// Instructions inside a function block, the resulting value comes from evaluating the
    /// instruction.
    Instruction,
}

/// Values currently in use by the parser.
///
///
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ValueList {
    /// Maping of value index to a value, a map is used since values can be forward declared, i.e.,
    /// parts of the IR can reference later values.
    values: HashMap<u64, (Rc<Value>, u64)>,

    /// Keep track of our current size.
    size: u64,

    /// Keep track of certain sizes at different points in time, e.g., after a function block has
    /// been parsed the value list should be reset to the size it was beforehand.
    scope_indices: Vec<u64>,
}

impl ValueList {
    /// Create an empty value list.
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            size: 0,
            scope_indices: Vec::new(),
        }
    }

    ///
    pub fn push(&mut self, value: Value) {
        let index = self.size;
        // self.values.insert(index, Rc::new(value));
        todo!()
    }

    pub fn get(&self, index: u64) -> Option<&Rc<Value>> {
        // self.values.get(&index)
        todo!()
    }

    pub fn push_scope(&mut self) {
        self.scope_indices.push(self.size);
    }

    pub fn pop_scope(&mut self) {
        let pop_to = self.scope_indices.pop().expect("at the outermost scope");
        self.values.retain(|&k, _| k < pop_to);
    }
}

pub struct Values(Vec<Rc<Value>>);

impl Values {
    pub fn get(&self, vid: usize) -> Option<&Rc<Value>> {
        self.0.get(vid)
    }

    pub fn add(&mut self, value: Value) {
        self.0.push(Rc::new(value));
    }
}
