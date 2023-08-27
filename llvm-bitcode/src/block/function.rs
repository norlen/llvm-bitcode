use llvm_bitstream::{BitstreamReader, Entry, ReaderError};
use tracing::{debug, error, info, warn};

use crate::{
    bitcodes::{BlockId, FunctionCode},
    block::parse_constant_block,
    context::Context,
    ir::Function,
    record::{parse_instruction_alloca, parse_instruction_call, AllocaError, CallError},
    util::{types::Type, value::Value},
    Fields,
};

use super::ConstantError;

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum FunctionBlockError {
    /// Failed to parse function block.
    #[error("Failed to parse function block")]
    InvalidFunctionBlock,

    /// Failed to parse constant sub-block in function block.
    #[error("Invalid constant block")]
    InvalidConstantBlock(#[from] ConstantError),

    /// Function block should start with a `DECLAREBLOCKS` record.
    #[error("Function block should start with a DECLAREBLOCKS record")]
    BlockShouldStartWithDeclareBlocks,

    /// Unexpected DECLARE_BLOCK record.
    #[error("Unexpected DECLARE_BLOCK record")]
    UnexpectedDeclareBlock,

    /// Failed to parse declare block record in function block.
    #[error("Failed to parse declare block record in function block")]
    InvalidDeclareBlockRecord,

    /// Failed to parse alloca instruction.
    #[error("Failed to parse alloca instruction")]
    InvalidAllocaRecord(#[from] AllocaError),

    /// Failed to parse call instruction.
    #[error("Failed to parse call instruction")]
    InvalidCallRecord(#[from] CallError),

    /// Error from the underlying [`BitstreamReader`].
    #[error("{0}")]
    ReaderError(#[from] ReaderError),
}

#[derive(Debug, Default)]
pub struct FnBlock {
    pub names: Vec<String>,
}

/// Parse a `Function` block.
///
/// Parsing a function block will temporarily add values to the context. These values will be
/// then be removed before the parsing exits.
pub fn parse_function_block<T: AsRef<[u8]>>(
    bitstream: &mut BitstreamReader<T>,
    function_declaration: &Function,
    ctx: &mut Context,
) -> Result<FnBlock, FunctionBlockError> {
    info!("Parsing body for function: {:?}", function_declaration);
    let mut record = Fields::<32>::new();

    // To keep indices consistent, we need to add the function arguments to the context. Instruction
    // records will then be able to reference them by index.
    match function_declaration.ty.as_ref() {
        Type::Function(fty) => {
            for parameter in &fty.parameters {
                info!("Adding parameter with type: {}", parameter);
                ctx.values.push(Value::Argument(parameter.clone()));
            }
        }
        _ => todo!(),
    }

    // A function block always begin with a `DECLAREBLOCKS` record.
    let num_basic_blocks = {
        let entry = bitstream
            .records()?
            .ok_or(FunctionBlockError::BlockShouldStartWithDeclareBlocks)?;

        let code = bitstream.read_record(entry, &mut record)?;
        match FunctionCode::from_code(code) {
            Some(FunctionCode::DeclareBlocks) => {}
            _ => {
                return Err(FunctionBlockError::BlockShouldStartWithDeclareBlocks);
            }
        }

        // The record itself contains a single integer; the number of blocks.
        record
            .get(0)
            .copied()
            .ok_or(FunctionBlockError::InvalidDeclareBlockRecord)? as usize
    };

    info!("Function should have {} basic blocks", num_basic_blocks);

    // let mut fn_block = FnBlock::default();

    while let Some(entry) = bitstream.advance()? {
        match entry {
            Entry::SubBlock(block) => {
                // When parsing a function block LLVM checks for the following blocks
                // - CONSTANTS_BLOCK_ID
                // - VALUE_SYMTAB_BLOCK_ID
                // - METADATA_ATTACHMENT_ID
                // - METADATA_BLOCK_ID
                // - USELIST_BLOCK_ID
                let Some(id) = BlockId::from_id(block.id) else {
                    warn!("Skipping unknown block: {:?}", block.id);
                    bitstream.skip_block()?;
                    continue;
                };

                match id {
                    BlockId::Constants => {
                        warn!("Skipping BlockId::Constants");
                        bitstream.enter_block(block)?;
                        let constants = parse_constant_block(bitstream, ctx)?;

                        for constant in constants {
                            ctx.values.push(Value::Constant(constant));
                        }
                    }
                    BlockId::ValueSymtab => {
                        warn!("Skipping BlockId::ValueSymtab");
                        bitstream.skip_block()?;
                    }
                    BlockId::Metadata => {
                        warn!("Skipping BlockId::Metadata");
                        bitstream.skip_block()?;
                    }
                    BlockId::MetadataAttachment => {
                        warn!("Skipping BlockId::MetadataAttachment");
                        bitstream.skip_block()?;
                    }
                    BlockId::Uselist => {
                        warn!("Skipping BlockId::Uselist");
                        bitstream.skip_block()?;
                    }
                    _ => {
                        warn!("Unexpected block in function block: {id:?}");
                        bitstream.skip_block()?;
                    }
                }
            }
            Entry::Record(entry) => {
                let code = bitstream.read_record(entry, &mut record)?;
                let Some(code) = FunctionCode::from_code(code) else {
                    warn!("Unknown function code: {code}, skipping");
                    continue;
                };

                let instruction = match code {
                    FunctionCode::DeclareBlocks => {
                        error!("Encountered another DECLAREBLOCKS record");
                        return Err(FunctionBlockError::UnexpectedDeclareBlock);
                    }
                    FunctionCode::InstBinop => {
                        info!("InstBinop");
                        todo!("InstBinop")
                    }
                    FunctionCode::InstCast => {
                        info!("InstCast");
                        todo!("InstCast")
                    }
                    FunctionCode::InstGepOld => {
                        info!("InstGepOld");
                        todo!("InstGepOld")
                    }
                    FunctionCode::InstSelect => {
                        info!("InstSelect");
                        todo!("InstSelect")
                    }
                    FunctionCode::InstExtractElt => {
                        info!("InstExtractElt");
                        todo!("InstExtractElt")
                    }
                    FunctionCode::IntInsertElt => {
                        info!("IntInsertElt");
                        todo!("IntInsertElt")
                    }
                    FunctionCode::IsntShuffleVec => {
                        info!("IsntShuffleVec");
                        todo!("IsntShuffleVec")
                    }
                    FunctionCode::InstCmp => {
                        info!("InstCmp");
                        todo!("InstCmp")
                    }
                    FunctionCode::InstRet => {
                        info!("InstRet");
                        todo!("InstRet")
                    }
                    FunctionCode::InstBr => {
                        info!("InstBr");
                        todo!("InstBr")
                    }
                    FunctionCode::InstSwitch => {
                        info!("InstSwitch");
                        todo!("InstSwitch")
                    }
                    FunctionCode::InstInvoke => {
                        info!("InstInvoke");
                        todo!("InstInvoke")
                    }
                    FunctionCode::InstUnreachable => {
                        info!("InstUnreachable");
                        todo!("InstUnreachable")
                    }
                    FunctionCode::InstPhi => {
                        info!("InstPhi");
                        todo!("InstPhi")
                    }
                    FunctionCode::InstAlloca => parse_instruction_alloca(&record, ctx)?,
                    FunctionCode::InstLoad => {
                        info!("InstLoad");
                        todo!("InstLoad")
                    }
                    FunctionCode::InstVaArg => {
                        info!("InstVaArg");
                        todo!("InstVaArg")
                    }
                    FunctionCode::InstStoreOld => {
                        info!("InstStoreOld");
                        todo!("InstStoreOld")
                    }
                    FunctionCode::InstExtractVal => {
                        info!("InstExtractVal");
                        todo!("InstExtractVal")
                    }
                    FunctionCode::InstInsertVal => {
                        info!("InstInsertVal");
                        todo!("InstInsertVal")
                    }
                    FunctionCode::InstCmp2 => {
                        info!("InstCmp2");
                        todo!("InstCmp2")
                    }
                    FunctionCode::InstVSelect => {
                        info!("InstVSelect");
                        todo!("InstVSelect")
                    }
                    FunctionCode::InstInboundsGepOld => {
                        info!("InstInboundsGepOld");
                        todo!("InstInboundsGepOld")
                    }
                    FunctionCode::InstIndirectBr => {
                        info!("InstIndirectBr");
                        todo!("InstIndirectBr")
                    }
                    FunctionCode::DebugLocAgain => {
                        info!("DebugLocAgain");
                        todo!("DebugLocAgain")
                    }
                    FunctionCode::InstCall => parse_instruction_call(&record, ctx)?,
                    FunctionCode::DebugLoc => {
                        info!("DebugLoc");
                        todo!("DebugLoc")
                    }
                    FunctionCode::InstFence => {
                        info!("InstFence");
                        todo!("InstFence")
                    }
                    FunctionCode::InstCmpxchgOld => {
                        info!("InstCmpxchgOld");
                        todo!("InstCmpxchgOld")
                    }
                    FunctionCode::InstAtomicRMWOld => {
                        info!("InstAtomicRMWOld");
                        todo!("InstAtomicRMWOld")
                    }
                    FunctionCode::InstResume => {
                        info!("InstResume");
                        todo!("InstResume")
                    }
                    FunctionCode::InstLandingpadOld => {
                        info!("InstLandingpadOld");
                        todo!("InstLandingpadOld")
                    }
                    FunctionCode::InstLoadAtomic => {
                        info!("InstLoadAtomic");
                        todo!("InstLoadAtomic")
                    }
                    FunctionCode::InstStoreAtomicOld => {
                        info!("InstStoreAtomicOld");
                        todo!("InstStoreAtomicOld")
                    }
                    FunctionCode::InstGep => {
                        info!("InstGep");
                        todo!("InstGep")
                    }
                    FunctionCode::InstStore => {
                        info!("InstStore");
                        todo!("InstStore")
                    }
                    FunctionCode::InstStoreAtomic => {
                        info!("InstStoreAtomic");
                        todo!("InstStoreAtomic")
                    }
                    FunctionCode::InstCmpxchg => {
                        info!("InstCmpxchg");
                        todo!("InstCmpxchg")
                    }
                    FunctionCode::InstLandingpad => {
                        info!("InstLandingpad");
                        todo!("InstLandingpad")
                    }
                    FunctionCode::InstCleanupret => {
                        info!("InstCleanupret");
                        todo!("InstCleanupret")
                    }
                    FunctionCode::InstCatchret => {
                        info!("InstCatchret");
                        todo!("InstCatchret")
                    }
                    FunctionCode::InstCatchpad => {
                        info!("InstCatchpad");
                        todo!("InstCatchpad")
                    }
                    FunctionCode::InstCleanuppad => {
                        info!("InstCleanuppad");
                        todo!("InstCleanuppad")
                    }
                    FunctionCode::InstCatchSwitch => {
                        info!("InstCatchSwitch");
                        todo!("InstCatchSwitch")
                    }
                    FunctionCode::OperandBundle => {
                        info!("OperandBundle");
                        todo!("OperandBundle")
                    }
                    FunctionCode::InstUnop => {
                        info!("InstUnop");
                        todo!("InstUnop")
                    }
                    FunctionCode::InstCallBr => {
                        info!("InstCallBr");
                        todo!("InstCallBr")
                    }
                    FunctionCode::InstFreeze => {
                        info!("InstFreeze");
                        todo!("InstFreeze")
                    }
                    FunctionCode::InstAtomicRMW => {
                        info!("InstAtomicRMW");
                        todo!("InstAtomicRMW")
                    }
                    FunctionCode::BlockaddrUsers => {
                        info!("BlockaddrUsers");
                        todo!("BlockaddrUsers")
                    }
                };

                info!("Parsed instruction: {instruction}");
                debug!("Parsed instruction: {:?}", instruction);
            }
        }
    }

    // Ok(fn_block)
    todo!("------ remove me --------");
}
