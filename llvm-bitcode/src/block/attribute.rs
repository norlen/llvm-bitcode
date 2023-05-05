use std::rc::Rc;

use llvm_bitstream::{BitstreamReader, ReaderError};
use smallvec::SmallVec;
use tracing::{info, warn};

use crate::{bitcodes::AttributeCode, context::Context, ir::AttributeGroup};

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum AttributeError {
    /// Failed to get the attribute group id for the attribute list.
    #[error("Invalid attribute group id: {0}")]
    InvalidAttributeGroupId(u64),

    /// Error from the underlying [`BitstreamReader`].
    #[error("{0}")]
    ReaderError(#[from] ReaderError),
}

/// Collection of attributes groups.
pub struct Attributes {
    attributes: SmallVec<[Rc<AttributeGroup>; 8]>,
}

impl std::fmt::Display for Attributes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "!{{")?;
        for (i, attribute) in self.attributes.iter().enumerate() {
            if i + 1 == self.attributes.len() {
                write!(f, "!{}", attribute.group_id)?;
            } else {
                write!(f, "!{} ", attribute.group_id)?;
            }
        }
        write!(f, "}}")
    }
}

impl Attributes {
    pub fn new(attributes: SmallVec<[Rc<AttributeGroup>; 8]>) -> Self {
        Self { attributes }
    }
}

pub fn parse_attribute_block<T: AsRef<[u8]>>(
    bitstream: &mut BitstreamReader<T>,
    ctx: &Context,
) -> Result<Vec<Attributes>, AttributeError> {
    let mut record: SmallVec<[u64; 64]> = SmallVec::new();

    let mut attributes = Vec::new();
    while let Some(entry) = bitstream.records()? {
        let code = bitstream.read_record(entry, &mut record)?;
        let Some(AttributeCode::Entry) = AttributeCode::from_code(code) else {
            warn!("Unsupported or unknown code: {code}, skipping");
            continue;
        };

        let mut current_attributes: SmallVec<[Rc<AttributeGroup>; 8]> = SmallVec::new();
        for group_id in record.iter().copied() {
            let attribute_group = ctx
                .get_attribute_group(group_id)
                .cloned()
                .ok_or(AttributeError::InvalidAttributeGroupId(group_id))?;

            current_attributes.push(attribute_group);
        }

        let current_attributes = Attributes::new(current_attributes);
        info!("Attribute !{}: {current_attributes}", attributes.len());
        attributes.push(current_attributes);
    }

    Ok(attributes)
}
