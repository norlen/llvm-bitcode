use std::{collections::HashMap, rc::Rc};

use crate::{
    block::{Identification, StringTable, SymbolTable, TypeList},
    ir::AttributeGroup,
};

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

    // Parsed type list if parsed, otherwise empty.
    pub type_list: TypeList,
}

impl Context {
    pub fn new() -> Self {
        Self {
            identification: None,
            string_table: StringTable::default(),
            symbol_table: SymbolTable::default(),
            attribute_groups: HashMap::new(),
            type_list: TypeList::default(),
        }
    }

    pub fn set_identification(&mut self, identification: Identification) {
        self.identification = Some(identification);
    }

    pub fn set_string_table(&mut self, string_table: StringTable) {
        self.string_table = string_table;
    }

    pub fn set_symbol_table(&mut self, symbol_table: SymbolTable) {
        self.symbol_table = symbol_table;
    }

    pub fn set_attribute_groups(&mut self, attribute_groups: Vec<AttributeGroup>) {
        for group in attribute_groups {
            self.attribute_groups.insert(group.group_id, Rc::new(group));
        }
    }

    pub fn get_attribute_group(&self, group_id: u64) -> Option<&Rc<AttributeGroup>> {
        self.attribute_groups.get(&group_id)
    }
}
