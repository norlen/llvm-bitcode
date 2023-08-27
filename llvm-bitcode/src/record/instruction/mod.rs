mod alloca;
mod binop;
mod call;
mod terminator;

pub use alloca::*;
pub use binop::*;
pub use call::*;
pub use terminator::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // Terminator instructions.
    Ret(Ret),
    Br(Br),
    Switch,
    IndirectBr,
    Invoke,
    CallBr,
    Resume(Resume),
    CatchSwitch,
    CatchRet,
    CleanupRet,
    Unreachable,

    // Unary operations.
    FNeg,

    // Binary operations.
    Add(Add),
    Sub(Sub),
    Mul(Mul),
    UDiv(UDiv),
    SDiv(SDiv),
    URem(URem),
    SRem(SRem),

    FAdd,
    FSub,
    FMul,
    FDiv,
    FRem,

    // Bitwse binary operations.
    Shl(Shl),
    LShr(LShr),
    AShr(AShr),
    And(And),
    Or(Or),
    Xor(Xor),

    // Vector operations.
    ExtractElement,
    InsertElement,
    ShuffleVector,
    // // Aggregate operations.
    // ExtractValue(ExtractValue),
    // InsertValue(InsertValue),

    // // Memory access and addressing operations.
    Alloca(Alloca),
    // Load(Load),
    // Store(Store),
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

// impl Instruction {
//     pub fn get_type(&self, ctx: &Context) -> Option<&Rc<Type>> {
//         match self {
//             Instruction::Alloca(i) => Some(i.dest_ty),
//             Instruction::Call(i) => match i.function_ty {
//                 Type::Function(f) => Some(f.return_ty),
//                 _ => None,
//             },
//             Instruction::Store(_) => None,
//             Instruction::Br(_) => None,
//             Instruction::Ret(r) => r.ty,
//             Instruction::Load(i) => Some(i.ty),
//             Instruction::GetElementPtr(i) => Some(i.ty),
//             Instruction::InsertValue(i) => Some(i.aggregate_ty),
//             Instruction::Resume(_) => None,
//             Instruction::Switch => todo!(),
//             Instruction::IndirectBr => todo!(),
//             Instruction::Invoke => todo!(),
//             Instruction::CallBr => todo!(),
//             Instruction::CatchSwitch => todo!(),
//             Instruction::CatchRet => todo!(),
//             Instruction::CleanupRet => todo!(),
//             Instruction::Unreachable => None,
//             Instruction::FNeg => todo!(),
//             Instruction::Add(i) => Some(i.return_ty),
//             Instruction::Sub(i) => Some(i.return_ty),
//             Instruction::Mul(i) => Some(i.return_ty),
//             Instruction::UDiv(i) => Some(i.return_ty),
//             Instruction::SDiv(i) => Some(i.return_ty),
//             Instruction::URem(i) => Some(i.return_ty),
//             Instruction::SRem(i) => Some(i.return_ty),
//             Instruction::FAdd => todo!(),
//             Instruction::FSub => todo!(),
//             Instruction::FMul => todo!(),
//             Instruction::FDiv => todo!(),
//             Instruction::FRem => todo!(),
//             Instruction::Shl(i) => Some(i.return_ty),
//             Instruction::LShr(i) => Some(i.return_ty),
//             Instruction::AShr(i) => Some(i.return_ty),
//             Instruction::And(i) => Some(i.return_ty),
//             Instruction::Or(i) => Some(i.return_ty),
//             Instruction::Xor(i) => Some(i.return_ty),
//             Instruction::ExtractElement => todo!(),
//             Instruction::InsertElement => todo!(),
//             Instruction::ShuffleVector => todo!(),
//             Instruction::ExtractValue(i) => Some(i.return_ty),
//             Instruction::Fence => todo!(),
//             Instruction::CmpXchg => todo!(),
//             Instruction::AtomicRmw => todo!(),
//             Instruction::Trunc(i) => Some(i.result_ty),
//             Instruction::Zext(i) => Some(i.result_ty),
//             Instruction::Sext(i) => Some(i.result_ty),
//             Instruction::FpTrunc(i) => Some(i.result_ty),
//             Instruction::FpExt(i) => Some(i.result_ty),
//             Instruction::FpToUi(i) => Some(i.result_ty),
//             Instruction::FpToSi(i) => Some(i.result_ty),
//             Instruction::SiToFp(i) => Some(i.result_ty),
//             Instruction::UiToFp(i) => Some(i.result_ty),
//             Instruction::PtrToInt(i) => Some(i.result_ty),
//             Instruction::IntToPtr(i) => Some(i.result_ty),
//             Instruction::Bitcast(i) => Some(i.result_ty),
//             Instruction::AddrspaceCast(i) => Some(i.result_ty),
//             Instruction::Icmp(_) => Some(ctx.bool_type()),
//             Instruction::Fcmp(_) => Some(ctx.bool_type()),
//             Instruction::Phi => todo!(),
//             Instruction::Select => todo!(),
//             Instruction::Freeze => todo!(),
//             Instruction::VaArg => todo!(),
//             Instruction::LandingPad => todo!(),
//             Instruction::CatchPad => todo!(),
//             Instruction::CleanupPad => todo!(), // Not really sure about this...
//         }
//     }

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
            Instruction::Ret(_) => todo!(),
            Instruction::Br(_) => todo!(),
            Instruction::Switch => todo!(),
            Instruction::IndirectBr => todo!(),
            Instruction::Invoke => todo!(),
            Instruction::CallBr => todo!(),
            Instruction::Resume(_) => todo!(),
            Instruction::CatchSwitch => todo!(),
            Instruction::CatchRet => todo!(),
            Instruction::CleanupRet => todo!(),
            Instruction::Unreachable => todo!(),
            Instruction::FNeg => todo!(),
            Instruction::Add(_) => todo!(),
            Instruction::Sub(_) => todo!(),
            Instruction::Mul(_) => todo!(),
            Instruction::UDiv(_) => todo!(),
            Instruction::SDiv(_) => todo!(),
            Instruction::URem(_) => todo!(),
            Instruction::SRem(_) => todo!(),
            Instruction::FAdd => todo!(),
            Instruction::FSub => todo!(),
            Instruction::FMul => todo!(),
            Instruction::FDiv => todo!(),
            Instruction::FRem => todo!(),
            Instruction::Shl(_) => todo!(),
            Instruction::LShr(_) => todo!(),
            Instruction::AShr(_) => todo!(),
            Instruction::And(_) => todo!(),
            Instruction::Or(_) => todo!(),
            Instruction::Xor(_) => todo!(),
            Instruction::ExtractElement => todo!(),
            Instruction::InsertElement => todo!(),
            Instruction::ShuffleVector => todo!(),
            Instruction::Alloca(alloca) => write!(f, "{alloca}"),
            Instruction::Call(call) => write!(f, "{call}"),
        }
    }
}
