use std::rc::Rc;

use llvm_bitstream::{BitstreamReader, ReaderError};
use smallvec::SmallVec;
use tracing::error;

use crate::{
    bitcodes::{ConstantsCode, UnaryOperationCode},
    context::Context,
    ir::APInt,
    util::types::{FloatingPointType, IntegerType, Type},
    Fields,
};

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ConstantError {
    /// Failed to parse record in constant block.
    #[error("Invalid constant record")]
    InvalidRecord,

    /// Unexpected [`ConstantsCode`], cannot safely continue parsing as constants a referenced
    /// by index in the value list.
    #[error("Unexpected constant code: {0}")]
    UnexpectedCode(u32),

    #[error("Invalid type for constant null value")]
    InvalidNullType,

    /// Invalid integer record.
    #[error("Invalid integer record")]
    InvalidIntegerRecord,

    /// Invalid wide integer record.
    #[error("Invalid wide integer record")]
    InvalidWideIntegerRecord,

    /// Invalid aggregate record.
    #[error("Invalid aggregate record")]
    InvalidAggregateRecord,

    /// Invalid string record.
    #[error("Invalid string record")]
    InvalidStringRecord,

    /// Invalid data record.
    #[error("Invalid data record")]
    InvalidDataRecord,

    /// Invalid unary op record.
    #[error("Invalid unary op record")]
    InvalidUnaryOpRecord,

    /// Error from the underlying [`BitstreamReader`].
    #[error("{0}")]
    ReaderError(#[from] ReaderError),
}

/// Constants internal to the bitcode reader.
///
/// These contants will later be materialized into IR constants.
#[derive(Clone, Debug)]
pub enum BitcodeConstant {
    // Null(Rc<Type>),
    /// Undefined constant value.
    Undef(Rc<Type>),

    /// Poision constant value.
    Poison(Rc<Type>),

    /// ...
    Data(Rc<Type>, SmallVec<[u64; 16]>),

    /// Contain both integer and boolean constants.
    Integer(Rc<Type>, APInt),

    /// Floating point values (float, doubles)
    FloatingPoint(Rc<Type>, f64),

    /// Constant pointer value that points to `null`.
    PointerNull(Rc<Type>),

    /// All aggregate zero values.
    AggregateZero(Rc<Type>),

    /// Array containing references to other constants.
    Array(Rc<Type>, SmallVec<[u32; 16]>),

    /// Array containing constant data.
    DataArray(Rc<Type>, SmallVec<[u64; 16]>),

    /// Vector containing references to other constants.
    Vector(Rc<Type>, SmallVec<[u32; 16]>),

    /// Vector containing constant data.
    DataVector(Rc<Type>, SmallVec<[u64; 16]>),

    /// ...
    Struct(Rc<Type>, SmallVec<[u32; 16]>),

    /// Constant expression.
    Expr(BitcodeConstantExpr),
}

#[derive(Clone, Debug)]
pub enum BitcodeConstantExpr {
    FpNeg(Rc<Type>, u64),
}

fn get_null_value(ty: Rc<Type>) -> BitcodeConstant {
    match ty.as_ref() {
        Type::Void => unreachable!(),
        Type::Integer(IntegerType { bits }) => {
            let value = APInt::from_u64(*bits, 0);
            BitcodeConstant::Integer(ty, value)
        }
        Type::FloatingPoint(fp) => match fp {
            FloatingPointType::Half
            | FloatingPointType::BFloat
            | FloatingPointType::Float
            | FloatingPointType::Double
            | FloatingPointType::Fp128
            | FloatingPointType::X86Fp80
            | FloatingPointType::PpcFp128 => BitcodeConstant::FloatingPoint(ty, 0.0),
            FloatingPointType::X86Amx => todo!(),
            FloatingPointType::X86Mmx => todo!(),
        },
        Type::Pointer { address_space: _ } => BitcodeConstant::PointerNull(ty),
        Type::Vector(_)
        | Type::Array {
            num_elements: _,
            ty: _,
        }
        | Type::Structure(_) => BitcodeConstant::AggregateZero(ty),
        Type::Function {
            parameters: _,
            return_ty: _,
            is_var_arg: _,
        } => todo!(),
        Type::Label => todo!(),
        Type::Token => todo!(),
        Type::Metadata => todo!(),
        Type::TargetExtension {
            name: _,
            type_parameters: _,
            int_parameters: _,
        } => todo!(),
    }
}

/// Parse a constants block.
///
/// # Errors
///
///
pub fn parse_constant_block<T: AsRef<[u8]>>(
    bitstream: &mut BitstreamReader<T>,
    ctx: &Context,
) -> Result<Vec<BitcodeConstant>, ConstantError> {
    let mut record = Fields::<64>::new();
    let mut current_ty = None;

    let mut constants = Vec::new();
    while let Some(entry) = bitstream.records()? {
        let code = bitstream.read_record(entry, &mut record)?;
        let Some(code) = ConstantsCode::from_code(code) else {
            return Err(ConstantError::UnexpectedCode(code));
        };

        // SETTYPE: [typeid]
        //
        // Set a type that will be used by subsequent records. There can be more than one that use
        // this set type.
        if matches!(code, ConstantsCode::SetType) {
            let tid = record.get(0).copied().ok_or(ConstantError::InvalidRecord)?;
            let ty = ctx
                .type_list
                .get(tid)
                .cloned()
                .ok_or(ConstantError::InvalidRecord)?;

            if matches!(ty.as_ref(), Type::Void) {
                return Err(ConstantError::InvalidRecord);
            }

            // This does not produce a constant, so skip to the next iteration.
            current_ty = Some(ty);
            continue;
        }

        let ty = current_ty.clone().ok_or(ConstantError::InvalidRecord)?;
        let constant = match code {
            // Already handled.
            ConstantsCode::SetType => unreachable!(),
            ConstantsCode::Undef => BitcodeConstant::Undef(ty),
            ConstantsCode::Poison => BitcodeConstant::Poison(ty),
            ConstantsCode::Null => get_null_value(ty),
            ConstantsCode::Integer => {
                if record.is_empty() {
                    return Err(ConstantError::InvalidIntegerRecord);
                }

                // TODO: Handle vector types.
                let bits = if let Type::Integer(IntegerType { bits }) = ty.as_ref() {
                    *bits
                } else {
                    return Err(ConstantError::InvalidIntegerRecord);
                };
                let value = decode_sign_rotated_value_u(record[0]);
                BitcodeConstant::Integer(ty, APInt::from_u64(bits, value))
            }
            ConstantsCode::WideInteger => {
                if record.is_empty() {
                    return Err(ConstantError::InvalidWideIntegerRecord);
                }

                let bits = if let Type::Integer(IntegerType { bits }) = ty.as_ref() {
                    *bits
                } else {
                    return Err(ConstantError::InvalidIntegerRecord);
                };
                let values: SmallVec<[u64; 8]> = record
                    .iter()
                    .map(|v| decode_sign_rotated_value_u(*v))
                    .collect();

                BitcodeConstant::Integer(ty, APInt::from_arr(bits, &values))
            }
            ConstantsCode::Float => todo!("ConstantsCode::Float"),
            ConstantsCode::Aggregate => {
                if record.is_empty() {
                    return Err(ConstantError::InvalidAggregateRecord);
                }

                let values: SmallVec<[u32; 16]> = record.iter().map(|v| *v as u32).collect();
                match ty.as_ref() {
                    Type::Vector(_) => BitcodeConstant::Vector(ty, values),
                    Type::Array {
                        num_elements: _,
                        ty: _,
                    } => BitcodeConstant::Array(ty, values),
                    Type::Structure(_) => BitcodeConstant::Struct(ty, values),
                    _ => BitcodeConstant::Undef(ty),
                }
            }
            ConstantsCode::String | ConstantsCode::CString => {
                if record.is_empty() {
                    return Err(ConstantError::InvalidStringRecord);
                }
                let null_terminated = matches!(code, ConstantsCode::CString);
                let values_to_take = if null_terminated {
                    record.len() - 1
                } else {
                    record.len()
                };

                // TODO: Allow this to be `u8`.
                let values: SmallVec<[u64; 16]> = convert_record(&record, values_to_take)
                    .ok_or(ConstantError::InvalidStringRecord)?;

                BitcodeConstant::Data(ty, values)
            }
            ConstantsCode::Data => {
                if record.is_empty() {
                    return Err(ConstantError::InvalidDataRecord);
                }

                let (element_ty, is_vec) = match ty.as_ref() {
                    Type::Vector(_) => (ty.clone(), true),
                    Type::Array {
                        num_elements: _,
                        ty,
                    } => (ty.clone(), false),
                    _ => {
                        error!("Invalid type for constant DATA: {:?}", ty.as_ref());
                        return Err(ConstantError::InvalidDataRecord);
                    }
                };

                match element_ty.as_ref() {
                    Type::Integer(IntegerType { bits: 8 })
                    | Type::Integer(IntegerType { bits: 16 })
                    | Type::Integer(IntegerType { bits: 32 })
                    | Type::Integer(IntegerType { bits: 64 }) => {
                        // TODO: Allow elements to be different sizes.
                        let values: SmallVec<[u64; 16]> = convert_record(&record, record.len())
                            .ok_or(ConstantError::InvalidStringRecord)?;

                        if is_vec {
                            BitcodeConstant::DataVector(ty, values)
                        } else {
                            BitcodeConstant::DataArray(ty, values)
                        }
                    }
                    Type::FloatingPoint(FloatingPointType::Half)
                    | Type::FloatingPoint(FloatingPointType::BFloat)
                    | Type::FloatingPoint(FloatingPointType::Float)
                    | Type::FloatingPoint(FloatingPointType::Double) => {
                        todo!("Floating point constant DATA")
                    }
                    _ => {
                        error!("invalid element type: {:?}", element_ty.as_ref());
                        return Err(ConstantError::InvalidDataRecord);
                    }
                }
            }

            ConstantsCode::BlockAddress => todo!(),
            ConstantsCode::DsoLocalEquivalent => todo!(),
            ConstantsCode::NoCfiValue => todo!(),

            ConstantsCode::InlineAsm => todo!(),

            // Constant expressions
            ConstantsCode::ConstexprBinop => todo!(),
            ConstantsCode::ConstexprCast => todo!(),
            ConstantsCode::ConstexprGep => todo!(),
            ConstantsCode::ConstexprSelect => todo!(),
            ConstantsCode::ConstexprExtractElement => todo!(),
            ConstantsCode::ConstexprInsertElement => todo!(),
            ConstantsCode::ConstexprShuffleVector => todo!(),
            ConstantsCode::ConstexprCompare => todo!(),
            ConstantsCode::ConstexprShuffleVectorEx => todo!(),
            ConstantsCode::ConstexprInboundsGep => todo!(),
            ConstantsCode::ConstexprGepWithInrangeIndex => todo!(),
            ConstantsCode::ConstexprUnaryOp => {
                // Unary ops are only valid for integers or floating point values.
                if record.len() < 2
                    || !matches!(ty.as_ref(), Type::Integer(_) | Type::FloatingPoint(_))
                {
                    return Err(ConstantError::InvalidUnaryOpRecord);
                }
                let op_code = UnaryOperationCode::from_code(record[0] as u32)
                    .ok_or(ConstantError::InvalidUnaryOpRecord)?;

                match op_code {
                    UnaryOperationCode::FloatingPointNegation => {
                        BitcodeConstant::Expr(BitcodeConstantExpr::FpNeg(ty.clone(), record[1]))
                    }
                }
            }

            // Unsupported codes.
            ConstantsCode::InlineAsmOld => todo!(),
            ConstantsCode::InlineAsmOld2 => todo!(),
            ConstantsCode::InlineAsmOld3 => todo!(),
        };

        constants.push(constant);
    }

    Ok(constants)
}

fn convert_record<T, const N: usize>(values: &[u64], len: usize) -> Option<SmallVec<[T; N]>>
where
    T: TryFrom<u64>,
{
    values
        .iter()
        .take(len)
        .map(|v| T::try_from(*v))
        .collect::<Result<SmallVec<[T; N]>, _>>()
        .ok()
}

/// Decode a sign rotated value.
///
/// Integers in LLVM stored the sign bit as the LSB for a dense VBR encoding. This function
/// parses these encoded integers.
fn decode_sign_rotated_value(value: i64) -> i64 {
    let is_positive = (value & 0b1) == 0;
    if is_positive {
        value >> 1
    } else if value != 1 {
        -(value >> 1)
    } else {
        // Where value == 1, it does not represent -0 as that does not make sense for integers.
        // Instead this represents the lowest integer value.
        i64::MIN
    }
}

/// Decode a sign rotated value.
///
/// Sign rotated values are signed, but here an unsigned value is treated as it is signed. Interally
/// all integers used are unsigned, even though they may be signed.
fn decode_sign_rotated_value_u(value: u64) -> u64 {
    decode_sign_rotated_value(value as i64) as u64
}
