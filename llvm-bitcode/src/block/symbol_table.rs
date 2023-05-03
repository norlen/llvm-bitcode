use num_enum::TryFromPrimitiveError;

use llvm_bitstream::{BitstreamReader, ReaderError};
use tracing::warn;

use crate::{
    bitcodes::{IdentificationCode, SymbolTableCode},
    Fields,
};

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum SymbolTableError {
    /// Failed to parse the symbol table.
    #[error("Invalid symbol table data")]
    InvalidSymbolTable,

    /// Error from the underlying [`BitstreamReader`].
    #[error("{0}")]
    ReaderError(#[from] ReaderError),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SymbolTable(Vec<u8>);

impl SymbolTable {
    /// Parse string table block.
    ///
    /// Should only contain a single blob of string data.
    pub fn parse<T: AsRef<[u8]>>(
        bitstream: &mut BitstreamReader<T>,
    ) -> Result<Self, SymbolTableError> {
        let mut data = None;

        let mut record = Fields::<32>::new();
        while let Some(entry) = bitstream.records()? {
            let code = bitstream.read_record(entry, &mut record)?;
            let Some(SymbolTableCode::Blob) = SymbolTableCode::from_code(code) else {
                warn!("Unknown code in SymbolTable: {code}, skipping");
                continue;
            };
            data = record.to_blob(0);
        }

        let data = data.ok_or(SymbolTableError::InvalidSymbolTable)?;
        Ok(SymbolTable(data))
    }
}
