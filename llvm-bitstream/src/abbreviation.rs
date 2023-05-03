use smallvec::SmallVec;

use crate::bitstream::ReaderError;

/// Abbreviation for the first operand, a code.
///
/// The code can only be a subset of all [`AbbreviationOperand`]s. `Array` and `Blob` are not valid
/// as the first operand in an abbreviation.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum CodeAbbreviation {
    /// Fixed value.
    Literal(u64),

    /// Fixed width field, value specifies the number of bits.
    Fixed(u32),

    /// VBR field where the value specifies the width of each chunk.
    Vbr(u32),

    /// 6-bit fixed field which maps to [a-zA-Z0-9._].
    Char6,
}

/// Descriptions for operands for [`AbbreviationRecord`].
///
/// An operand is a union of two things
///
/// 1. Literal values, e.g., the operand is always `N`.
/// 2. Encoding specificiations, e.g., the operand is always encoded like *that*.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum AbbreviationOperand {
    /// Fixed value.
    Literal(u64),

    /// Fixed width field, value specifies the number of bits.
    Fixed(u32),

    /// VBR field where the value specifies the width of each chunk.
    Vbr(u32),

    /// Sequence of fields.
    Array(ArrayOperand),

    /// 6-bit fixed field which maps to [a-zA-Z0-9._].
    Char6,

    /// 32-bit aligned array of 8-bit characters.
    Blob,
}

/// [`AbbreviationOperand::Array`] can only hold `Fixed`, `Vbr` and `Char6` as its elements.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum ArrayOperand {
    /// For zero-sized elements (`Fixed` and `Vbr` with a bit-width of zero) we stored this right
    /// away as zero.
    Zero,

    /// Fixed width field, value specifies the number of bits.
    Fixed(u32),

    /// VBR field where the value specifies the width of each chunk.
    Vbr(u32),

    /// 6-bit fixed field which maps to [a-zA-Z0-9._].
    Char6,
}

/// Abbreviation operand encoding values.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
#[repr(u8)]
pub(crate) enum OperandEncoding {
    Fixed = 1,
    Vbr,
    Array,
    Char6,
    Blob,
}

impl TryFrom<u64> for OperandEncoding {
    type Error = ReaderError;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(OperandEncoding::Fixed),
            2 => Ok(OperandEncoding::Vbr),
            3 => Ok(OperandEncoding::Array),
            4 => Ok(OperandEncoding::Char6),
            5 => Ok(OperandEncoding::Blob),
            _ => Err(ReaderError::InvalidAbbreviationRecord(
                "invalid abbreviation operand encoding",
            )),
        }
    }
}

/// Abbreviation record.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct AbbreviationRecord {
    pub(crate) code: CodeAbbreviation,
    pub(crate) operands: SmallVec<[AbbreviationOperand; 32]>,
}

impl AbbreviationRecord {
    pub(crate) fn from_code(code: CodeAbbreviation) -> Self {
        Self {
            code,
            operands: SmallVec::new(),
        }
    }

    pub(crate) fn push(&mut self, v: AbbreviationOperand) {
        self.operands.push(v);
    }
}
