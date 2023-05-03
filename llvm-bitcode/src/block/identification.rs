use num_enum::TryFromPrimitiveError;

use llvm_bitstream::{BitstreamReader, ReaderError};

use crate::{bitcodes::IdentificationCode, Fields};

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum IdentificationError {
    /// Couldn't find a producer record in the block.
    #[error("Missing producer")]
    MissingProducer,

    /// Couldn't find an epoch record in the block.
    #[error("Missing epoch")]
    MissingEpoch,

    /// Failed to parse the producer as a string.
    #[error("Bad Producer")]
    BadProducer,

    /// Failed to get an epoch from the record.
    #[error("Bad Epoch")]
    BadEpoch,

    /// Got an incomaptible epoch.
    #[error("incompatible epoch, got: {0}")]
    IncompatibleEpoch(u8),

    /// Failed to parse the code to a valid [`IdentificationCode`].
    #[error("No discriminant in enum `IdentificationCode` matches the value `{0}`")]
    InvalidIdentificationCode(u8),

    /// Error from the underlying [`BitstreamReader`].
    #[error("{0}")]
    ReaderError(#[from] ReaderError),
}

impl From<TryFromPrimitiveError<IdentificationCode>> for IdentificationError {
    fn from(value: TryFromPrimitiveError<IdentificationCode>) -> Self {
        IdentificationError::InvalidIdentificationCode(value.number)
    }
}

/// Identification details.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Identification {
    /// Describes producer details.
    pub producer: String,

    /// Epoch defins the auto-upgrade capability.
    pub epoch: u64,
}

impl Identification {
    /// Creates a new `Identification`.
    pub fn new(producer: String, epoch: u64) -> Self {
        Self { producer, epoch }
    }

    /// Parse an `IdentificationBlock`.
    ///
    /// If multiple producer and epoch records exist, the last ones will be used.
    ///
    /// # Errors
    ///
    /// ## Producer
    ///
    /// - If the `Producer` string cannot be parse [`IdentificationError::BadProducer`] is returned.
    /// - If no `Producer` record is found [`IdentificationError::MissingProducer`] is returned.
    ///
    /// ## Epoch
    ///
    /// - If an `epoch` isn't found in the `Epoch` record [`IdentificationError::BadEpoch`] is returned.
    /// - If the `epoch` isn't compatible with the this parser, e.g., it matches [`IdentificationCode::BITCODE_CURRENT_EPOCH`]
    ///   [`IdentificationError::IncompatibleEpoch`] is returned.
    /// - If no `Epoch` record is found [`IdentificationError::MissingEpoch`] is returned.
    pub fn parse<T: AsRef<[u8]>>(
        bitstream: &mut BitstreamReader<T>,
    ) -> Result<Identification, IdentificationError> {
        let mut producer: Option<String> = None;
        let mut epoch: Option<u64> = None;

        let mut record = Fields::<32>::new();
        while let Some(entry) = bitstream.records()? {
            let code = bitstream.read_record(entry, &mut record)?;
            let code = IdentificationCode::try_from(code as u8)?;
            match code {
                IdentificationCode::Producer => {
                    producer = Some(record.string(0).ok_or(IdentificationError::BadProducer)?);
                }
                IdentificationCode::Epoch => {
                    let e = *record.get(0).ok_or(IdentificationError::BadEpoch)?;
                    if e != IdentificationCode::BITCODE_CURRENT_EPOCH {
                        return Err(IdentificationError::IncompatibleEpoch(e as u8));
                    }
                    epoch = Some(e);
                }
            }
        }

        Ok(Identification::new(
            producer.ok_or(IdentificationError::MissingProducer)?,
            epoch.ok_or(IdentificationError::MissingEpoch)?,
        ))
    }
}
