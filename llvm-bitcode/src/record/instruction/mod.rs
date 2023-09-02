mod alloca;
mod binop;
mod call;
mod store;
mod terminator;

use std::rc::Rc;

pub use alloca::*;
pub use binop::*;
pub use call::*;
pub use store::*;
pub use terminator::*;

use crate::util::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // // Terminator instructions.
    // Ret(Ret),
    // Br(Br),
    // Switch,
    // IndirectBr,
    // Invoke,
    // CallBr,
    // Resume(Resume),
    // CatchSwitch,
    // CatchRet,
    // CleanupRet,
    // Unreachable,

    // // Unary operations.
    // FNeg,

    // // Binary operations.
    // Add(Add),
    // Sub(Sub),
    // Mul(Mul),
    // UDiv(UDiv),
    // SDiv(SDiv),
    // URem(URem),
    // SRem(SRem),

    // FAdd,
    // FSub,
    // FMul,
    // FDiv,
    // FRem,

    // // Bitwse binary operations.
    // Shl(Shl),
    // LShr(LShr),
    // AShr(AShr),
    // And(And),
    // Or(Or),
    // Xor(Xor),

    // Vector operations.
    // ExtractElement,
    // InsertElement,
    // ShuffleVector,
    // // Aggregate operations.
    // ExtractValue(ExtractValue),
    // InsertValue(InsertValue),

    // // Memory access and addressing operations.
    Alloca(Alloca),
    // Load(Load),
    Store(Store),
    // Fence,
    // CmpXchg,
    // AtomicRmw,
    // GetElementPtr(Gep),

    // // Conversion operations
    // Trunc(Truncate),
    // Zext(ZeroExtend),
    // Sext(SignExtend),
    // FpTrunc(FloatingPointTruncate),
    // FpExt(FloatingPointExtend),
    // FpToUi(FloatingPointToUnsignedInt),
    // FpToSi(FloatingPointToSignedInt),
    // SiToFp(SignedIntToFloatingPoint),
    // UiToFp(UnsignedIntToFloatingPoint),
    // PtrToInt(PointerToInt),
    // IntToPtr(IntToPointer),
    // Bitcast(Bitcast),
    // AddrspaceCast(AddressSpaceCast),

    // // Other operations.
    // Icmp(Icmp),
    // Fcmp(Fcmp),
    // Phi,
    // Select,
    // Freeze,
    Call(Call),
    // VaArg,
    // LandingPad,
    // CatchPad,
    // CleanupPad,
}

impl Instruction {
    pub fn get_type(&self) -> Option<Rc<Type>> {
        match self {
            Instruction::Alloca(i) => Some(i.dest_ty.clone()),
            Instruction::Call(i) => match i.function_ty.as_ref() {
                Type::Function(f) => Some(f.return_ty.clone()),
                _ => todo!(),
            },
            Instruction::Store(i) => None,
        }
    }
}

//     pub fn is_terminator(&self) -> bool {
//         matches!(
//             self,
//             Instruction::Ret(_)
//                 | Instruction::Br(_)
//                 | Instruction::Switch
//                 | Instruction::IndirectBr
//                 | Instruction::Invoke
//                 | Instruction::CallBr
//                 | Instruction::Resume(_)
//                 | Instruction::CatchSwitch
//                 | Instruction::CatchRet
//                 | Instruction::CleanupRet
//                 | Instruction::Unreachable
//         )
//     }
// }

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Alloca(alloca) => write!(f, "{alloca}"),
            Instruction::Call(call) => write!(f, "{call}"),
            Instruction::Store(i) => write!(f, "{i}"),
        }
    }
}
