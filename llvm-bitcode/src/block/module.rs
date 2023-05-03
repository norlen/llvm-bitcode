use llvm_bitstream::{BitstreamReader, Entry, ReaderError};
use tracing::{info, warn};

use crate::{
    bitcodes::{BlockId, ModuleCode},
    Fields,
};

#[derive(Clone, Copy, Debug, thiserror::Error, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ModuleError {
    /// Did not process enough records to complete the module.
    #[error("Could not populate module")]
    InvalidModuleBlock,

    /// Error from the underlying [`BitstreamReader`].
    #[error("{0}")]
    ReaderError(#[from] ReaderError),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleInfo {
    // version: u64,
    // triple: String,
    // data_layout: String,
    // section_name: String,
    // filename: String,
    // vst_offset: u64,
}

#[derive(Debug, Default)]
struct ModuleInfoBuilder {
    version: Option<u64>,
    triple: Option<String>,
    data_layout: Option<String>,
    section_name: Option<String>,
    filename: Option<String>,
    vst_offset: Option<u64>,
}

impl ModuleInfoBuilder {
    fn build(self) -> Result<ModuleInfo, ModuleError> {
        Ok(ModuleInfo {
            // version: self.version.ok_or(ModuleError::InvalidModuleBlock)?,
            // triple: self.triple.ok_or(ModuleError::InvalidModuleBlock)?,
            // data_layout: self.data_layout.ok_or(ModuleError::InvalidModuleBlock)?,
            // section_name: self.section_name.ok_or(ModuleError::InvalidModuleBlock)?,
            // filename: self.filename.ok_or(ModuleError::InvalidModuleBlock)?,
            // vst_offset: self.vst_offset.ok_or(ModuleError::InvalidModuleBlock)?,
        })
    }

    fn version(&mut self, version: u64) -> &mut Self {
        self.version = Some(version);
        self
    }

    fn triple(&mut self, triple: String) -> &mut Self {
        self.triple = Some(triple);
        self
    }

    fn data_layout(&mut self, data_layout: String) -> &mut Self {
        self.data_layout = Some(data_layout);
        self
    }

    fn section_name(&mut self, section_name: String) -> &mut Self {
        self.section_name = Some(section_name);
        self
    }

    fn filename(&mut self, filename: String) -> &mut Self {
        self.filename = Some(filename);
        self
    }

    fn vst_offset(&mut self, vst_offset: u64) -> &mut Self {
        self.vst_offset = Some(vst_offset);
        self
    }
}

pub fn parse_module<T: AsRef<[u8]>>(
    bitstream: &mut BitstreamReader<T>,
) -> Result<ModuleInfo, ModuleError> {
    let mut module_info = ModuleInfoBuilder::default();

    let mut record = Fields::<32>::new();
    while let Some(entry) = bitstream.advance()? {
        match entry {
            Entry::SubBlock(block) => {
                let Some(id) = BlockId::try_from(block.id as u8).ok() else {
                    warn!("Unknown module block id: {}, skipping", block.id);
                    bitstream.skip_block()?;
                    continue;
                };

                // These are the blocks that LLVM handle when parsing the module block.
                match id {
                    BlockId::ParameterAttributes => {
                        bitstream.skip_block()?;
                        info!("ParameterAttributes block");
                    }
                    BlockId::ParameterAttributeGroups => {
                        bitstream.skip_block()?;
                        info!("ParameterAttributeGroups block");
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
                        info!("Metadata block");
                    }
                    BlockId::Types => {
                        bitstream.skip_block()?;
                        info!("Types block");
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
                        bitstream.skip_block()?;
                        info!("OperandBundleTags block");
                    }
                    BlockId::MetadataKind => {
                        bitstream.skip_block()?;
                        info!("MetadataKind block");
                    }
                    BlockId::FullLtoGlobalValueSummary => {
                        bitstream.skip_block()?;
                        info!("FullLtoGlobalValueSummary block");
                    }
                    BlockId::SyncScopeNames => {
                        bitstream.skip_block()?;
                        info!("SyncScopeNames block");
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
                let Some(code) = ModuleCode::try_from(code as u8).ok() else {
                    warn!("Unknown module code: {code}, skipping");
                    continue;
                };
                match code {
                    ModuleCode::Version => info!("Version record"),
                    ModuleCode::Triple => info!("Triple record"),
                    ModuleCode::Datalayout => info!("Datalayout record"),
                    ModuleCode::Asm => info!("Asm record"),
                    ModuleCode::SectionName => info!("SectionName record"),
                    ModuleCode::DepLib => info!("DepLib record"),
                    ModuleCode::GlobalVariable => info!("GlobalVariable record"),
                    ModuleCode::Function => info!("Function record"),
                    ModuleCode::AliasOld => info!("AliasOld record"),
                    ModuleCode::GcName => info!("GcName record"),
                    ModuleCode::Comdat => info!("Comdat record"),
                    ModuleCode::VstOffset => info!("VstOffset record"),
                    ModuleCode::Alias => info!("Alias record"),
                    ModuleCode::MetadataValuesUnused => info!("MetadataValuesUnused record"),
                    ModuleCode::Filename => info!("Filename record"),
                    ModuleCode::Hash => info!("Hash record"),
                    ModuleCode::IFunc => info!("IFunc record"),
                }
            }
        }
    }

    module_info.build()
}
