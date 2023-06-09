use llvm_bitstream::{BitstreamReader, ReaderError};
use tracing::warn;

use crate::{bitcodes::StringTableCode, Fields};

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum StringTableError {
    /// Failed to parse the string table.
    #[error("Invalid string table data")]
    InvalidStringTable,

    /// Error from the underlying [`BitstreamReader`].
    #[error("{0}")]
    ReaderError(#[from] ReaderError),
}

/// String table containing all strings used in a module.
///
/// Essentially an LLVM `STRTAB` block, where a string can be retrieved by providing an offset
/// into the table and the size.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct StringTable(Vec<u8>);

impl StringTable {
    /// Parse string table block.
    ///
    /// Should only contain a single blob of string data.
    pub fn parse<T: AsRef<[u8]>>(
        bitstream: &mut BitstreamReader<T>,
    ) -> Result<Self, StringTableError> {
        let mut data = None;

        let mut record = Fields::<32>::new();
        while let Some(entry) = bitstream.records()? {
            let code = bitstream.read_record(entry, &mut record)?;
            let Some(StringTableCode::Blob) = StringTableCode::from_code(code) else {
                warn!("Unknown code in StringTable: {code}, skipping");
                continue;
            };
            data = record.to_blob(0);
        }

        let data = data.ok_or(StringTableError::InvalidStringTable)?;
        Ok(StringTable(data))
    }

    /// Get a an owned string from the string table.
    pub fn get(&self, offset: u64, size: u64) -> Option<String> {
        let offset = offset as usize;
        let size = size as usize;

        if size == 0 || offset + size >= self.0.len() {
            None
        } else {
            let start = offset;
            let end = offset + size;

            let bytes: &[u8] = self.0.as_ref();
            core::str::from_utf8(&bytes[start..end])
                .ok()
                .map(|s| s.to_owned())
        }
    }
}
