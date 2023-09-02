use std::rc::Rc;

use smallvec::SmallVec;
use tracing::warn;

use crate::{
    context::{Context, ContextError},
    ir::AttributeList,
    record::util::{get_value, get_value_type_pair},
    util::{fields::IncompleteRecordError, types::Type, value::Value},
    Fields, FieldsIter,
};
use bitflags::bitflags;

use super::Instruction;

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum CallError {
    #[error(transparent)]
    IncompleteRecord(#[from] IncompleteRecordError),

    /// Failed to parse record.
    #[error("Failed to parse record: {0}")]
    InvalidRecord(&'static str),

    /// Callee is not a pointer type.
    #[error("Callee is not a pointer type")]
    CalleeNotPointer,

    #[error(transparent)]
    ContextError(#[from] ContextError),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub attributes: Rc<AttributeList>,
    pub cc: u64,
    pub function_ty: Rc<Type>,
    pub function: Rc<Value>,
    pub arguments: SmallVec<[Rc<Value>; 16]>,
}

impl std::fmt::Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // <result> = [tail | musttail | notail ] call [fast-math flags] [cconv] [ret attrs] [addrspace(<num>)]
        //   <ty>|<fnty> <fnptrval>(<function args>) [fn attrs] [ operand bundles ]
        let rty = match self.function_ty.as_ref() {
            Type::Function(f) => format!("{}", f.return_ty),
            _ => "not_fn_ty".to_owned(),
        };
        let fn_name = match self.function.as_ref() {
            Value::Function(f) => f.name.to_owned(),
            _ => "not_fn_value".to_owned(),
        };

        write!(
            f,
            "call {} {} {:?} ({:?}) {}",
            self.cc, rty, fn_name, self.arguments, self.attributes
        )
    }
}

bitflags! {
    /// Flags that indicate if a memory access modifies or references memory.
    #[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
    pub struct CallingConvention: u64 {
        const TAIL = (1 << 0);
        const CALL_CONV = 0b0011_1111_1111_1111 << 1;
        const MUSTTAIL = (1 << 14);
        const EXPLICIT_TYPE = (1 << 15);
        const NOTAIL = (1 << 16);
        const FAST_MATH_FLAGS = (1 << 17);
    }
}

bitflags! {
    /// Flags that indicate if a memory access modifies or references memory.
    #[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
    pub struct FastMathFlags: u8 {
        const UnsafeAlgebra = (1 << 0); // Legacy
        const NoNaNs = (1 << 1);
        const NoInfs = (1 << 2);
        const NoSignedZeros = (1 << 3);
        const AllowReciprocal = (1 << 4);
        const AllowContract = (1 << 5);
        const ApproxFunc = (1 << 6);
        const AllowReassoc = (1 << 7);
    }
}

fn decode_fast_math_flags(value: u64) -> FastMathFlags {
    FastMathFlags::from_bits_truncate(value as u8)
}

// pub fn parse_fmf(record: u64) -> FastMathFlags {
//     if record
// }

/// Parse call instruction record.
///
///
pub fn parse_instruction_call<const N: usize>(
    record: &Fields<N>,
    ctx: &Context,
    next_value_number: u64,
) -> Result<Instruction, CallError> {
    //     // CALL: [paramattrs, cc, fmf, fnty, fnid, arg0, arg1...]
    //     if (Record.size() < 3)
    //     return error("Invalid record");
    let mut record = FieldsIter::from(record);
    record.require_min_size(3)?;

    //   unsigned OpNum = 0;
    //   AttributeList PAL = getAttributes(Record[OpNum++]);
    //   unsigned CCInfo = Record[OpNum++];

    let attr_grp = record.next_or_err()?;
    println!("attr_grp: {:?}", attr_grp);
    let attributes = ctx
        .get_attributes(attr_grp)
        .ok_or(CallError::InvalidRecord("Failed to get attributes"))?;
    println!("attributes: {:?}", attributes);

    let cc = CallingConvention::from_bits_truncate(record.next_or_err()?);

    //   FastMathFlags FMF;
    //   if ((CCInfo >> bitc::CALL_FMF) & 1) {
    //     FMF = getDecodedFastMathFlags(Record[OpNum++]);
    //     if (!FMF.any())
    //       return error("Fast math flags indicator set for call with no FMF");
    //   }

    let fast_math_flags = if cc.contains(CallingConvention::FAST_MATH_FLAGS) {
        let fast_math_flags = decode_fast_math_flags(record.next_or_err()?);
        if fast_math_flags.is_empty() {
            warn!("Fast math flags indicator set for call with no FMF");
        }
        Some(fast_math_flags)
    } else {
        None
    };

    //   unsigned FTyID = InvalidTypeID;
    //   FunctionType *FTy = nullptr;
    //   if ((CCInfo >> bitc::CALL_EXPLICIT_TYPE) & 1) {
    //     FTyID = Record[OpNum++];
    //     FTy = dyn_cast_or_null<FunctionType>(getTypeByID(FTyID));
    //     if (!FTy)
    //       return error("Explicit call type is not a function type");
    //   }

    let (fun_tid, fun_ty_rc) = if cc.contains(CallingConvention::EXPLICIT_TYPE) {
        let tid = record.next().unwrap();
        let ty = ctx.get_ty(tid)?;
        if !ty.is_function() {
            return Err(CallError::InvalidRecord(
                "Explicit call type is not a function type",
            ));
        }

        (tid, ty)
    } else {
        todo!("TODO: Implicit type not supported (yet)");
    };

    //   Value *Callee;
    //   unsigned CalleeTypeID;
    //   if (getValueTypePair(Record, OpNum, NextValueNo, Callee, CalleeTypeID,
    //                        CurBB))
    //     return error("Invalid record");

    let (callee, callee_ty) =
        get_value_type_pair::<N, CallError>(&mut record, next_value_number, ctx)?;

    //   PointerType *OpTy = dyn_cast<PointerType>(Callee->getType());
    //   if (!OpTy)
    //     return error("Callee is not a pointer type");
    //   if (!FTy) {
    //     FTyID = getContainedTypeID(CalleeTypeID);
    //     FTy = dyn_cast_or_null<FunctionType>(getTypeByID(FTyID));
    //     if (!FTy)
    //       return error("Callee is not of pointer to function type");
    //   } else if (!OpTy->isOpaqueOrPointeeTypeMatches(FTy))
    //     return error("Explicit call type does not match pointee type of "
    //                  "callee operand");
    //   if (Record.size() < FTy->getNumParams() + OpNum)
    //     return error("Insufficient operands to call");

    // if !callee_ty.is_pointer() {
    //     return Err(CallError::CalleeNotPointer);
    // }
    // TODO: Validate types and that the number of arguments is correct.
    let Type::Function(fun_ty) = fun_ty_rc.as_ref() else {
        panic!("TODO: Not a function type");
    };

    //   SmallVector<Value*, 16> Args;
    //   SmallVector<unsigned, 16> ArgTyIDs;
    //   // Read the fixed params.
    //   for (unsigned i = 0, e = FTy->getNumParams(); i != e; ++i, ++OpNum) {
    //     unsigned ArgTyID = getContainedTypeID(FTyID, i + 1);
    //     if (FTy->getParamType(i)->isLabelTy())
    //       Args.push_back(getBasicBlock(Record[OpNum]));
    //     else
    //       Args.push_back(getValue(Record, OpNum, NextValueNo,
    //                               FTy->getParamType(i), ArgTyID, CurBB));
    //     ArgTyIDs.push_back(ArgTyID);
    //     if (!Args.back())
    //       return error("Invalid record");
    //   }

    // Read fixed arguments.
    let mut arguments = SmallVec::<[Rc<Value>; 16]>::new();
    // let mut argument_types = SmallVec::<[u64; 16]>::new();
    for parameter in fun_ty.parameters.iter() {
        let vid = record.next_or_err()?;
        if matches!(parameter.as_ref(), Type::Label) {
            todo!("Label types in call");
            // let bb = ctx.get_basic_block(vid)?;
            // arguments.push(bb);
        } else if matches!(parameter.as_ref(), Type::Metadata) {
            warn!("Metadata types in call");
            arguments.push(Rc::new(Value::Metadata));
        } else {
            let value = get_value(vid, next_value_number, ctx)?;
            arguments.push(value);
            // argument_types.push(ty.id());
        }
    }

    //   // Read type/value pairs for varargs params.
    //   if (!FTy->isVarArg()) {
    //     if (OpNum != Record.size())
    //       return error("Invalid record");
    //   } else {
    //     while (OpNum != Record.size()) {
    //       Value *Op;
    //       unsigned OpTypeID;
    //       if (getValueTypePair(Record, OpNum, NextValueNo, Op, OpTypeID, CurBB))
    //         return error("Invalid record");
    //       Args.push_back(Op);
    //       ArgTyIDs.push_back(OpTypeID);
    //     }
    //   }

    if fun_ty.is_var_arg {
        todo!("Var args in call");
    }

    //   // Upgrade the bundles if needed.
    //   if (!OperandBundles.empty())
    //     UpgradeOperandBundles(OperandBundles);

    // TODO: Updgrade operand bundles.

    //   I = CallInst::Create(FTy, Callee, Args, OperandBundles);
    //   ResTypeID = getContainedTypeID(FTyID);
    //   OperandBundles.clear();
    //   InstructionList.push_back(I);
    //   cast<CallInst>(I)->setCallingConv(
    //       static_cast<CallingConv::ID>((0x7ff & CCInfo) >> bitc::CALL_CCONV));
    //   CallInst::TailCallKind TCK = CallInst::TCK_None;
    //   if (CCInfo & 1 << bitc::CALL_TAIL)
    //     TCK = CallInst::TCK_Tail;
    //   if (CCInfo & (1 << bitc::CALL_MUSTTAIL))
    //     TCK = CallInst::TCK_MustTail;
    //   if (CCInfo & (1 << bitc::CALL_NOTAIL))
    //     TCK = CallInst::TCK_NoTail;
    //   cast<CallInst>(I)->setTailCallKind(TCK);
    //   cast<CallInst>(I)->setAttributes(PAL);
    //   if (Error Err = propagateAttributeTypes(cast<CallBase>(I), ArgTyIDs)) {
    //     I->deleteValue();
    //     return Err;
    //   }
    //   if (FMF.any()) {
    //     if (!isa<FPMathOperator>(I))
    //       return error("Fast-math-flags specified for call without "
    //                    "floating-point scalar or vector return type");
    //     I->setFastMathFlags(FMF);
    //   }

    let calling_convention = 0;

    Ok(Instruction::Call(Call {
        attributes,
        cc: calling_convention,
        function_ty: fun_ty_rc,
        function: callee,
        arguments,
    }))
}
