use llvm_bitstream::{BitstreamReader, Entry, ReaderError};
use tracing::{error, info, warn};

use crate::{
    bitcodes::{BlockId, ModuleCode},
    block::{parse_attribute_groups_block, parse_sync_scope_names_block, parse_type_block},
    context::Context,
    record::{
        parse_function_record, parse_global_variable, FunctionRecordError, GlobalVariableError,
    },
    Fields,
};

use super::{
    parse_attribute_block, parse_operand_bundle_tags_block, AttributeError, AttributeGroupError,
    OperandBundleTagsError, SyncScopeNamesError, TypesError,
};

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ModuleError {
    /// Did not process enough records to complete the module.
    #[error("Could not populate module")]
    InvalidModuleBlock,

    /// Couldn't parse a `Types` block.
    #[error("Failed to parse a Types block")]
    InvalidTypes(#[from] TypesError),

    /// Couldn't parse a `AttributeGroups` block.
    #[error("Failed to parse a AttributeGroups block")]
    InvalidAttributeGroups(#[from] AttributeGroupError),

    /// Couldn't parse a `Attribute` block.
    #[error("Failed to parse a Attribute block")]
    InvalidAttributeError(#[from] AttributeError),

    /// Couldn't parse a `SyncScopeNames` block.
    #[error("Failed to parse a SyncScopeNames block")]
    InvalidSyncScopeNames(#[from] SyncScopeNamesError),

    /// Couldn't parse an `OperandBundleTags` block.
    #[error("Failed to parse a OperandBundleTags block")]
    InvalidOperandBundleTags(#[from] OperandBundleTagsError),

    /// Failed to parse version record.
    #[error("Failed to parse version record")]
    InvalidVersionRecord,

    /// Failed to parse triple record.
    #[error("Failed to parse triple record")]
    InvalidTripleRecord,

    /// Failed to parse datalayout record.
    #[error("Failed to parse datalayout record")]
    InvalidDatalayoutRecord,

    /// Failed to parse asm record.
    #[error("Failed to parse asm record")]
    InvalidAsmRecord,

    /// Failed to parse section name record.
    #[error("Failed to parse section name record")]
    InvalidSectionNameRecord,

    /// Failed to parse gc name record.
    #[error("Failed to parse gc name record")]
    InvalidGcNameRecord,

    /// Failed to parse deplib record.
    #[error("Failed to parse deplib record")]
    InvalidDeplibRecord,

    /// Failed to parse vst offset record.
    #[error("Failed to parse vst offset record")]
    InvalidVstOffsetRecord,

    /// Failed to parse filename record.
    #[error("Failed to parse filename record")]
    InvalidFilenameRecord,

    /// Failed to parse global variable record.
    #[error("Failed to parse global variable record")]
    InvalidGlobalVariableRecord(#[from] GlobalVariableError),

    /// Failed to parse function record.
    #[error("Failed to parse function record")]
    InvalidFunctionRecord(#[from] FunctionRecordError),

    /// Failed to parse hash record.
    #[error("Failed to parse hash record")]
    InvalidHashRecord,

    /// Error from the underlying [`BitstreamReader`].
    #[error("{0}")]
    ReaderError(#[from] ReaderError),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleInfo {
    version: u64,
    triple: String,
    datalayout: String,
    asm: Option<String>,
    gc_name: Option<String>,
    deplib: Option<String>,
    filename: String,
    vst_offset: u64,
    hash: Option<[u32; 5]>,
}

#[derive(Debug, Default)]
struct ModuleInfoBuilder {
    version: Option<u64>,
    triple: Option<String>,
    datalayout: Option<String>,
    asm: Option<String>,
    gc_name: Option<String>,
    deplib: Option<String>,
    filename: Option<String>,
    vst_offset: Option<u64>,
    hash: Option<[u32; 5]>,
}

impl ModuleInfoBuilder {
    fn build(self) -> Result<ModuleInfo, ModuleError> {
        Ok(ModuleInfo {
            version: self.version.ok_or(ModuleError::InvalidModuleBlock)?,
            triple: self.triple.ok_or(ModuleError::InvalidModuleBlock)?,
            datalayout: self.datalayout.ok_or(ModuleError::InvalidModuleBlock)?,
            asm: self.asm,
            gc_name: self.gc_name,
            deplib: self.deplib,
            filename: self.filename.ok_or(ModuleError::InvalidModuleBlock)?,
            vst_offset: self.vst_offset.ok_or(ModuleError::InvalidModuleBlock)?,
            hash: self.hash,
        })
    }

    fn version(&mut self, version: u64) -> &mut Self {
        info!(version = version);
        self.version = Some(version);
        self
    }

    fn triple(&mut self, triple: String) -> &mut Self {
        info!(triple = triple);
        self.triple = Some(triple);
        self
    }

    fn datalayout(&mut self, datalayout: String) -> &mut Self {
        info!(datalayout = datalayout);
        self.datalayout = Some(datalayout);
        self
    }

    fn asm(&mut self, asm: String) -> &mut Self {
        info!(asm = asm);
        self.asm = Some(asm);
        self
    }

    fn gc_name(&mut self, gc_name: String) -> &mut Self {
        info!(gc_name = gc_name);
        self.gc_name = Some(gc_name);
        self
    }

    fn deplib(&mut self, deplib: String) -> &mut Self {
        info!(deplib = deplib);
        self.deplib = Some(deplib);
        self
    }

    fn filename(&mut self, filename: String) -> &mut Self {
        info!(filename = filename);
        self.filename = Some(filename);
        self
    }

    fn vst_offset(&mut self, vst_offset: u64) -> &mut Self {
        info!(vst_offset = vst_offset);
        self.vst_offset = Some(vst_offset);
        self
    }

    fn hash(&mut self, hash: [u32; 5]) -> &mut Self {
        info!("hash = {hash:x?}");
        self.hash = Some(hash);
        self
    }
}

pub fn parse_module<T: AsRef<[u8]>>(
    bitstream: &mut BitstreamReader<T>,
    ctx: &mut Context,
) -> Result<ModuleInfo, ModuleError> {
    let mut module_info = ModuleInfoBuilder::default();

    let mut record = Fields::<32>::new();
    while let Some(entry) = bitstream.advance()? {
        match entry {
            Entry::SubBlock(block) => {
                let Some(id) = BlockId::from_id(block.id) else {
                    warn!("Unknown module block id: {}, skipping", block.id);
                    bitstream.skip_block()?;
                    continue;
                };

                // These are the blocks that LLVM handle when parsing the module block.
                match id {
                    BlockId::ParameterAttributes => {
                        bitstream.enter_block(block)?;
                        let attributes = parse_attribute_block(bitstream, ctx)?;
                        ctx.attributes = attributes;
                    }
                    BlockId::ParameterAttributeGroups => {
                        bitstream.enter_block(block)?;
                        let attribute_groups = parse_attribute_groups_block(bitstream, ctx)?;
                        ctx.set_attribute_groups(attribute_groups);
                    }
                    BlockId::Constants => {
                        bitstream.skip_block()?;
                        info!("Constants block");
                    }
                    BlockId::Function => {
                        bitstream.skip_block()?;
                        info!("Function block");
                    }
                    BlockId::ValueSymtab => {
                        bitstream.skip_block()?;
                        info!("ValueSymtab block");
                    }
                    BlockId::Metadata => {
                        bitstream.skip_block()?;
                        info!("Skipping: Metadata block");
                    }
                    BlockId::Types => {
                        bitstream.enter_block(block)?;
                        ctx.type_list = parse_type_block(bitstream)?;
                    }
                    BlockId::Uselist => {
                        bitstream.skip_block()?;
                        info!("Uselist block");
                    }
                    BlockId::ModuleStrtab => {
                        bitstream.skip_block()?;
                        info!("ModuleStrtab block");
                    }
                    BlockId::GlobalValueSummary => {
                        bitstream.skip_block()?;
                        info!("GlobalValueSummary block");
                    }
                    BlockId::OperandBundleTags => {
                        bitstream.enter_block(block)?;
                        let _operand_bundle_tags = parse_operand_bundle_tags_block(bitstream)?;
                    }
                    BlockId::MetadataKind => {
                        bitstream.skip_block()?;
                        info!("Skipping: MetadataKind block");
                    }
                    BlockId::FullLtoGlobalValueSummary => {
                        bitstream.skip_block()?;
                        info!("FullLtoGlobalValueSummary block");
                    }
                    BlockId::SyncScopeNames => {
                        bitstream.enter_block(block)?;
                        let _sync_scope_names = parse_sync_scope_names_block(bitstream)?;
                    }
                    _ => {
                        warn!(
                            "Unexpected block id: {} in module block, skipping",
                            block.id
                        );
                        bitstream.skip_block()?;
                        continue;
                    }
                }
            }
            Entry::Record(entry) => {
                let code = bitstream.read_record(entry, &mut record)?;
                let Some(code) = ModuleCode::from_code(code) else {
                    warn!("Unknown module code: {code}, skipping");
                    continue;
                };

                match code {
                    ModuleCode::Version => {
                        let version = *record.first().ok_or(ModuleError::InvalidVersionRecord)?;
                        module_info.version(version);
                    }
                    ModuleCode::Triple => {
                        let triple = record
                            .to_string(0)
                            .ok_or(ModuleError::InvalidTripleRecord)?;

                        module_info.triple(triple);
                    }
                    ModuleCode::Datalayout => {
                        let datalayout = record
                            .to_string(0)
                            .ok_or(ModuleError::InvalidDatalayoutRecord)?;

                        module_info.datalayout(datalayout);
                    }
                    ModuleCode::Asm => {
                        let asm = record.to_string(0).ok_or(ModuleError::InvalidAsmRecord)?;

                        module_info.asm(asm);
                    }
                    ModuleCode::SectionName => {
                        let section_name = record
                            .to_string(0)
                            .ok_or(ModuleError::InvalidSectionNameRecord)?;

                        info!(section_name = section_name);
                        ctx.section_table.push(section_name);
                    }
                    ModuleCode::DepLib => {
                        let deplib = record
                            .to_string(0)
                            .ok_or(ModuleError::InvalidDeplibRecord)?;

                        module_info.deplib(deplib);
                    }
                    ModuleCode::GcName => {
                        let gc_name = record
                            .to_string(0)
                            .ok_or(ModuleError::InvalidGcNameRecord)?;

                        module_info.gc_name(gc_name);
                    }
                    ModuleCode::Comdat => info!("Comdat record"),
                    ModuleCode::VstOffset => {
                        let vst_offset =
                            *record.first().ok_or(ModuleError::InvalidVstOffsetRecord)?;

                        // LLVM decrements the vst offset by 1, noting that because the offset is
                        // relative to one word before the start of the identification or module
                        // block. For, of course, historical reasons, this was always the start of
                        // the regular bitcode header.
                        module_info.vst_offset(vst_offset - 1);
                    }
                    ModuleCode::Filename => {
                        let filename = record
                            .to_string(0)
                            .ok_or(ModuleError::InvalidFilenameRecord)?;

                        module_info.filename(filename);
                    }
                    ModuleCode::Hash => {
                        if record.len() != 5 {
                            return Err(ModuleError::InvalidHashRecord);
                        }
                        let mut hash = [0u32; 5];
                        for (i, v) in record.iter().copied().enumerate() {
                            hash[i] = v.try_into().map_err(|_ignored| {
                                error!("Unexpected high bits set in hash");
                                ModuleError::InvalidHashRecord
                            })?;
                        }
                        module_info.hash(hash);
                    }
                    ModuleCode::GlobalVariable => {
                        let (global_variable, _ty, _init_id) = parse_global_variable(&record, ctx)?;
                        info!("parse global: {global_variable}");
                    }
                    ModuleCode::Function => {
                        let function = parse_function_record(&record, ctx)?;
                        info!("parse function: {function}");
                    }
                    ModuleCode::AliasOld => info!("AliasOld record"),
                    ModuleCode::Alias => info!("Alias record"),
                    ModuleCode::IFunc => info!("IFunc record"),
                    ModuleCode::MetadataValuesUnused => info!("MetadataValuesUnused record"),
                }
            }
        }
    }

    module_info.build()
}
