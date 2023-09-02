use std::{collections::HashMap, rc::Rc};

use tracing::warn;

use super::util::value::ValueList;
use crate::{
    block::{Identification, StringTable, SymbolTable, TypeList},
    ir::{AttributeGroup, AttributeList, Comdat},
    util::{types::Type, value::Value},
};

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ContextError {
    /// Invalid type id.
    #[error("Invalid type id: {0}")]
    InvalidTypeId(u64),

    /// Invalid value id.
    #[error("Invalid value id: {0}")]
    InvalidValueId(u64),
}

/// Context used while parsing the bitstream.
///
#[derive(Debug)]
pub struct Context {
    // Identification block.
    identification: Option<Identification>,

    // String table if parsed, otherwise empty.
    string_table: StringTable,

    // Symbol table if parsed, otherwise empty.
    symbol_table: SymbolTable,

    // Store attribute groups, keyed by their group id.
    attribute_groups: HashMap<u64, Rc<AttributeGroup>>,

    pub attributes: Vec<Rc<AttributeList>>,

    pub section_table: Vec<String>,

    pub gc_table: Vec<String>,

    pub comdat: Vec<Comdat>,

    // Parsed type list if parsed, otherwise empty.
    pub types: TypeList,

    pub values: ValueList,
}

impl Context {
    pub fn new() -> Self {
        Self {
            identification: None,
            string_table: StringTable::default(),
            symbol_table: SymbolTable::default(),
            attribute_groups: HashMap::new(),
            attributes: Vec::new(),
            section_table: Vec::new(),
            gc_table: Vec::new(),
            comdat: Vec::new(),
            types: TypeList::default(),
            values: ValueList::new(),
        }
    }

    pub fn get_ty(&self, tid: u64) -> Result<Rc<Type>, ContextError> {
        self.types.get(tid).ok_or(ContextError::InvalidTypeId(tid))
    }

    pub fn get_value(&self, vid: u64) -> Result<Rc<Value>, ContextError> {
        self.values
            .get(vid)
            .cloned()
            .ok_or(ContextError::InvalidValueId(vid))
    }

    pub fn get_value_type(&self, vid: u64) -> Result<Rc<Type>, ContextError> {
        self.values
            .get_ty(vid)
            .cloned()
            .ok_or(ContextError::InvalidValueId(vid))
    }

    pub fn set_identification(&mut self, identification: Identification) {
        self.identification = Some(identification);
    }

    pub fn strtab(&self) -> &StringTable {
        &self.string_table
    }

    pub fn set_string_table(&mut self, string_table: StringTable) {
        self.string_table = string_table;
    }

    pub fn set_symbol_table(&mut self, symbol_table: SymbolTable) {
        self.symbol_table = symbol_table;
    }

    pub fn set_attribute_groups(&mut self, attribute_groups: Vec<AttributeGroup>) {
        for group in attribute_groups {
            warn!(
                "Adding attribute group id: {}, group: {}",
                group.group_id, group
            );
            self.attribute_groups.insert(group.group_id, Rc::new(group));
        }
    }

    pub fn get_attribute_group(&self, group_id: u64) -> Option<&Rc<AttributeGroup>> {
        self.attribute_groups.get(&group_id)
    }

    pub fn get_attributes(&self, index: u64) -> Option<Rc<AttributeList>> {
        self.attributes.get(index as usize).cloned()
    }
}
