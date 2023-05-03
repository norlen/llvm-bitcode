use std::process::id;

use crate::block::{Identification, StringTable, SymbolTable};

/// Context used while parsing the bitstream.
///
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Context {
    identification: Option<Identification>,
    string_table: Option<StringTable>,
    symbol_table: Option<SymbolTable>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            identification: None,
            string_table: None,
            symbol_table: None,
        }
    }

    pub fn set_identification(&mut self, identification: Identification) {
        self.identification = Some(identification);
    }

    pub fn set_string_table(&mut self, string_table: StringTable) {
        self.string_table = Some(string_table);
    }

    pub fn set_symbol_table(&mut self, symbol_table: SymbolTable) {
        self.symbol_table = Some(symbol_table);
    }
}
