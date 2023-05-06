use std::rc::Rc;

use llvm_bitstream::{BitstreamReader, ReaderError};
use smallvec::SmallVec;
use tracing::{info, warn};

use crate::{bitcodes::AttributeCode, context::Context, ir::AttributeList};

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum AttributeError {
    /// Failed to get the attribute group id for the attribute list.
    #[error("Invalid attribute group id: {0}")]
    InvalidAttributeGroupId(u64),

    /// Error from the underlying [`BitstreamReader`].
    #[error("{0}")]
    ReaderError(#[from] ReaderError),
}

pub fn parse_attribute_block<T: AsRef<[u8]>>(
    bitstream: &mut BitstreamReader<T>,
    ctx: &Context,
) -> Result<Vec<Rc<AttributeList>>, AttributeError> {
    let mut record: SmallVec<[u64; 64]> = SmallVec::new();

    let mut attributes = Vec::new();
    while let Some(entry) = bitstream.records()? {
        let code = bitstream.read_record(entry, &mut record)?;
        let Some(AttributeCode::Entry) = AttributeCode::from_code(code) else {
            warn!("Unsupported or unknown code: {code}, skipping");
            continue;
        };

        let mut current_attributes = SmallVec::new();
        for group_id in record.iter().copied() {
            let attribute_group = ctx
                .get_attribute_group(group_id)
                .cloned()
                .ok_or(AttributeError::InvalidAttributeGroupId(group_id))?;

            current_attributes.push(attribute_group);
        }

        let current_attributes = AttributeList::new(current_attributes);
        info!("Attribute !{}: {current_attributes}", attributes.len());
        attributes.push(Rc::new(current_attributes));
    }

    Ok(attributes)
}
