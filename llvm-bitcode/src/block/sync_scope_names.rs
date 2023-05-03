use llvm_bitstream::{BitstreamReader, ReaderError};
use num_enum::{TryFromPrimitive, TryFromPrimitiveError};
use tracing::{error, info, warn};

use crate::{bitcodes::SyncScopeNameCode, Fields};

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum SyncScopeNamesError {
    /// Failed to convert the record into a string.
    #[error("Could not parse string from SyncScopeName record")]
    InvalidSyncScopeNameRecord,

    /// Error from the underlying [`BitstreamReader`].
    #[error("{0}")]
    ReaderError(#[from] ReaderError),
}

/// Synchronization scope names.
///
/// Names are implicitly mapped to ids by their name.
#[derive(Debug, Default)]
pub struct SyncScopeNames {
    pub names: Vec<String>,
}

/// Parse a `SyncScopeNames` block.
///
/// The block itself is pretty simple, it only contains record. Each record is a single string
/// containing the a single sync scope name.
pub fn parse_sync_scope_names_block<T: AsRef<[u8]>>(
    bitstream: &mut BitstreamReader<T>,
) -> Result<SyncScopeNames, SyncScopeNamesError> {
    let mut sync_scope_names = SyncScopeNames::default();

    let mut record = Fields::<32>::new();
    while let Some(entry) = bitstream.records()? {
        let code = bitstream.read_record(entry, &mut record)?;
        let Some(SyncScopeNameCode::Name) = SyncScopeNameCode::from_code(code) else {
            warn!("Unkonwn SyncScopeNameCode: {code}, skipping");
            continue;
        };

        if record.is_empty() {
            warn!("Empty sync_scope_name");
            sync_scope_names.names.push(String::new());
        } else {
            let sync_scope_name = record
                .to_string(0)
                .ok_or(SyncScopeNamesError::InvalidSyncScopeNameRecord)?;

            info!(sync_scope_name = sync_scope_name);
            sync_scope_names.names.push(sync_scope_name);
        }
    }

    Ok(sync_scope_names)
}
