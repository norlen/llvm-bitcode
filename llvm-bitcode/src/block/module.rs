use llvm_bitstream::{BitstreamReader, Entry, ReaderError};
use tracing::info;

use crate::Fields;

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
                info!("found block");
                bitstream.skip_block()?;
            }
            Entry::Record(entry) => {
                let code = bitstream.read_record(entry, &mut record)?;
            }
        }
    }

    module_info.build()
}
