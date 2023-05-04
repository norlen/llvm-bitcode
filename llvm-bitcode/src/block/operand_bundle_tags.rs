use llvm_bitstream::{BitstreamReader, ReaderError};
use tracing::{error, info, warn};

use crate::{bitcodes::OperandBundleTagCode, Fields};

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum OperandBundleTagsError {
    /// Failed to convert the record into a string.
    #[error("Could not parse string from OperandBundleTags record")]
    InvalidOperandBundleTagsRecord,

    /// Error from the underlying [`BitstreamReader`].
    #[error("{0}")]
    ReaderError(#[from] ReaderError),
}

/// Operand bundle tags.
#[derive(Debug, Default)]
pub struct OperandBundleTags {
    pub tags: Vec<String>,
}

/// Parse an `OperandBundleTags` block.
///
/// The block itself is pretty simple, it only contains records. Each record is a single string
/// containing an operand bundle tag.
pub fn parse_operand_bundle_tags_block<T: AsRef<[u8]>>(
    bitstream: &mut BitstreamReader<T>,
) -> Result<OperandBundleTags, OperandBundleTagsError> {
    let mut operand_bundle_tags = OperandBundleTags::default();

    let mut record = Fields::<32>::new();
    while let Some(entry) = bitstream.records()? {
        let code = bitstream.read_record(entry, &mut record)?;
        let Some(OperandBundleTagCode::Tag) = OperandBundleTagCode::from_code(code) else {
            warn!("Unkonwn OperandBundleTagCode: {code}, skipping");
            continue;
        };

        let tag = record
            .to_string(0)
            .ok_or(OperandBundleTagsError::InvalidOperandBundleTagsRecord)?;

        info!(tag = tag);
        operand_bundle_tags.tags.push(tag);
    }

    Ok(operand_bundle_tags)
}
