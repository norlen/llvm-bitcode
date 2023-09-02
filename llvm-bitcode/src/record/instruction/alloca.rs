use bitflags::bitflags;
use smallvec::SmallVec;
use std::rc::Rc;

use crate::{
    context::Context,
    util::{types::Type, value::Value},
};

use super::Instruction;

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum AllocaError {
    /// Not enough fields in the record.
    #[error("not enough fields in the record")]
    IncompleteRecord,

    /// Failed to parse record.
    #[error("Failed to parse record: {0}")]
    InvalidRecord(&'static str),

    /// Failed to get type from type list.
    #[error("Uknown type")]
    UnknownType,

    /// Failed to get value from value list.
    #[error("Unknown value")]
    UnknownValue,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Alloca {
    pub dest_ty: Rc<Type>,
    pub num_elements_ty: Rc<Type>,
    pub num_elements: Rc<Value>,
    pub alignment: usize,
    pub flags: AllocaFlags,
    pub addrspace: Option<u64>,
}

impl Alloca {
    #[allow(unused)]
    pub fn get_swift_error(&self) -> bool {
        self.flags.contains(AllocaFlags::SWIFT_ERROR)
    }

    #[allow(unused)]
    pub fn get_in_alloca(&self) -> bool {
        self.flags.contains(AllocaFlags::IN_ALLOCA)
    }
}

impl std::fmt::Display for Alloca {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // <result> = alloca [inalloca] <type> [, <ty> <NumElements>] [, align <alignment>] [, addrspace(<num>)]     ; yields type addrspace(num)*:result
        let in_alloca = if self.get_in_alloca() {
            "inalloca "
        } else {
            ""
        };

        let num_elements = { format!("{} {:?}", self.num_elements_ty, self.num_elements) };

        write!(f, "alloca {in_alloca}{} {num_elements}", self.dest_ty)
    }
}

bitflags! {
    #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub struct AllocaFlags: u8 {
        const SWIFT_ERROR = 0b0000_0001;
        const IN_ALLOCA   = 0b0000_0010;
    }
}

struct AllocaPackedValues(u64);

impl AllocaPackedValues {
    // All constants are of (offset, size).
    const ALIGN_LOWER_DATA: (u64, u64) = (0, 5);
    const USED_WITH_IN_ALLOCA_DATA: (u64, u64) = (Self::next_bit(Self::ALIGN_LOWER_DATA), 1);
    const EXPLICIT_TYPE_DATA: (u64, u64) = (Self::next_bit(Self::USED_WITH_IN_ALLOCA_DATA), 1);
    const SWIFT_ERROR_DATA: (u64, u64) = (Self::next_bit(Self::EXPLICIT_TYPE_DATA), 1);
    const ALIGN_UPPER_DATA: (u64, u64) = (Self::next_bit(Self::SWIFT_ERROR_DATA), 3);

    // All constants are of (offset, mask)
    const ALIGN_LOWER: (u64, u64) = Self::generate(Self::ALIGN_LOWER_DATA);
    const USED_WITH_IN_ALLOCA: (u64, u64) = Self::generate(Self::USED_WITH_IN_ALLOCA_DATA);
    const EXPLICIT_TYPE: (u64, u64) = Self::generate(Self::EXPLICIT_TYPE_DATA);
    const SWIFT_ERROR: (u64, u64) = Self::generate(Self::SWIFT_ERROR_DATA);
    const ALIGN_UPPER: (u64, u64) = Self::generate(Self::ALIGN_UPPER_DATA);

    const fn next_bit((offset, size): (u64, u64)) -> u64 {
        offset + size
    }

    /// Generates the constants offset and mask, in that order.
    const fn generate((offset, size): (u64, u64)) -> (u64, u64) {
        let mask = ((1 << size) - 1) << offset;
        (offset, mask)
    }

    fn extract(&self, (offset, mask): (u64, u64)) -> u64 {
        (self.0 & mask) >> offset
    }

    fn alignment(&self) -> u64 {
        let align_lower = self.extract(Self::ALIGN_LOWER);
        let align_upper = self.extract(Self::ALIGN_UPPER);
        let align = align_lower | (align_upper << Self::ALIGN_LOWER.1);
        1 << (align - 1)
    }

    fn used_with_in_alloca(&self) -> bool {
        self.extract(Self::USED_WITH_IN_ALLOCA) > 0
    }

    fn explicit_type(&self) -> bool {
        self.extract(Self::EXPLICIT_TYPE) > 0
    }

    fn swift_error(&self) -> bool {
        self.extract(Self::SWIFT_ERROR) > 0
    }
}

pub fn parse_instruction_alloca<const N: usize>(
    record: &SmallVec<[u64; N]>,
    ctx: &Context,
) -> Result<Instruction, AllocaError> {
    if record.len() != 4 && record.len() != 5 {
        return Err(AllocaError::IncompleteRecord);
    }

    let dest_ty = ctx.types.get(record[0]).ok_or(AllocaError::UnknownType)?;
    let num_elements_ty = ctx.types.get(record[1]).ok_or(AllocaError::UnknownType)?;

    let num_elements = ctx
        .values
        .get(record[2])
        .cloned()
        .ok_or(AllocaError::UnknownValue)?;

    let packed = AllocaPackedValues(record[3]);
    if !packed.explicit_type() {
        todo!("TODO: Implicit type not supported (yet)");
    }

    // TODO: Default datalayout from context should be used if not present.
    let addrspace = record.get(5).copied();

    let mut flags = AllocaFlags::empty();
    flags.set(AllocaFlags::SWIFT_ERROR, packed.swift_error());
    flags.set(AllocaFlags::IN_ALLOCA, packed.used_with_in_alloca());

    let alignment = packed.alignment() as usize;
    // if !(alignment > 0 && dest_ty.is_sized()) {
    //     panic!("Alloca of unsized type")
    // }

    Ok(Instruction::Alloca(Alloca {
        dest_ty,
        num_elements_ty,
        num_elements,
        alignment,
        flags,
        addrspace,
    }))
}
