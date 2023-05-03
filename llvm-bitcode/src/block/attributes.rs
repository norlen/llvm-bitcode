use llvm_bitstream::ReaderError;
use num_enum::{TryFromPrimitive, TryFromPrimitiveError};

use crate::bitcodes::AttributeKindCode;

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum AttributeGroupError {
    #[error("TypeNotFound: {0}")]
    TypeNotFound(u64),

    /// Failed to parse the code to a valid [`RecordAttributeKind`].
    #[error("No discriminant in enum `RecordAttributeKind` matches the value `{0}`")]
    InvalidRecordAttributeKind(u8),

    /// Failed to parse the code to a valid [`AttributeKindCode`].
    #[error("No discriminant in enum `AttributeKindCode` matches the value `{0}`")]
    InvalidAttributeKindCode(u8),

    /// Error from the underlying [`BitstreamReader`].
    #[error("{0}")]
    ReaderError(#[from] ReaderError),
}

impl From<TryFromPrimitiveError<RecordAttributeKind>> for AttributeGroupError {
    fn from(value: TryFromPrimitiveError<RecordAttributeKind>) -> Self {
        AttributeGroupError::InvalidRecordAttributeKind(value.number)
    }
}

impl From<TryFromPrimitiveError<AttributeKindCode>> for AttributeGroupError {
    fn from(value: TryFromPrimitiveError<AttributeKindCode>) -> Self {
        AttributeGroupError::InvalidAttributeKindCode(value.number)
    }
}

/// Attribute kind.
///
/// Each attribute kind is either a well-known LLVM attribute or an arbitrary string. Both of these
/// can have either an integer respectively a string value associated with it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, TryFromPrimitive)]
#[repr(u8)]
pub enum RecordAttributeKind {
    /// Well-known attribute.
    ///
    /// [key#].
    Enum = 0,

    /// Well-known attribute with an integer value.
    ///
    /// [key#, value#].
    Int = 1,

    /// String attribute.
    ///
    /// [strchr x N].
    String = 3,

    /// String attribute with a string value.
    ///
    /// [strchr x N, strchr x N].
    StringWithValue = 4,

    /// Type attribute.
    ///
    ///
    Type = 5,
    TypeWithValue = 6,
}
