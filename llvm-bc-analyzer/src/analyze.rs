use anyhow::anyhow;
use llvm_bitstream::{BitstreamReader, Block, Entry};
use smallvec::SmallVec;

use crate::llvm_bitcodes::{
    AttributeCode, BlockId, ConstantsCode, FunctionCode, GlobalValueSummarySymbolTableCode,
    IdentificationCode, MetadataCode, ModuleCode, ModulePathSymbolTableCode, OperandBundleTagCode,
    StringTableCode, SymbolTableCode, SyncScopeNameCode, TypeCode, UseListCode,
    ValueSymbolTableCode,
};

pub fn parse_bitcode<T: AsRef<[u8]>>(bytes: T) -> anyhow::Result<()> {
    let mut bitstream = BitstreamReader::from_bytes(bytes)?;

    // Go through the top-level blocks.
    while !bitstream.cursor().is_empty() {
        let entry = bitstream
            .advance()?
            .ok_or_else(|| anyhow!("Encountered end block at the top-level"))?;

        match entry {
            Entry::SubBlock(block) => {
                bitstream.enter_block(block)?;
                parse_block(&mut bitstream, block, 0)?;
            }
            Entry::Record(_) => return Err(anyhow!("Found record at the top-level")),
        };
    }

    Ok(())
}

trait Id {
    type Value;

    fn id(&self) -> Option<Self::Value>;
}

impl Id for Block {
    type Value = BlockId;

    fn id(&self) -> Option<BlockId> {
        let id: u8 = self.id.try_into().ok()?;
        BlockId::try_from(id).ok()
    }
}

fn get_record_name(block_id: BlockId, code: u32) -> Option<&'static str> {
    let code: u8 = code.try_into().ok()?;

    match block_id {
        BlockId::Module => ModuleCode::try_from(code).ok().map(|code| code.name()),
        BlockId::ParameterAttributes | BlockId::ParameterAttributeGroups => {
            AttributeCode::try_from(code).ok().map(|code| code.name())
        }
        BlockId::Constants => ConstantsCode::try_from(code).ok().map(|code| code.name()),
        BlockId::Function => FunctionCode::try_from(code).ok().map(|code| code.name()),
        BlockId::Identification => IdentificationCode::try_from(code)
            .ok()
            .map(|code| code.name()),
        BlockId::ValueSymtab => ValueSymbolTableCode::try_from(code)
            .ok()
            .map(|code| code.name()),
        BlockId::Metadata | BlockId::MetadataAttachment => {
            MetadataCode::try_from(code).ok().map(|code| code.name())
        }
        BlockId::Types => TypeCode::try_from(code).ok().map(|code| code.name()),
        BlockId::Uselist => UseListCode::try_from(code).ok().map(|code| code.name()),
        BlockId::ModuleStrtab => ModulePathSymbolTableCode::try_from(code)
            .ok()
            .map(|code| code.name()),
        BlockId::GlobalValueSummary | BlockId::FullLtoGlobalValueSummary => {
            GlobalValueSummarySymbolTableCode::try_from(code)
                .ok()
                .map(|code| code.name())
        }
        BlockId::OperandBundleTags => OperandBundleTagCode::try_from(code)
            .ok()
            .map(|code| code.name()),
        BlockId::MetadataKind => MetadataCode::try_from(code).ok().map(|code| code.name()),
        BlockId::StringTable => StringTableCode::try_from(code).ok().map(|code| code.name()),
        BlockId::Symtab => SymbolTableCode::try_from(code).ok().map(|code| code.name()),
        BlockId::SyncScopeNames => SyncScopeNameCode::try_from(code)
            .ok()
            .map(|code| code.name()),
    }
}

fn parse_block<T: AsRef<[u8]>>(
    bitstream: &mut BitstreamReader<T>,
    block: Block,
    level: usize,
) -> anyhow::Result<()> {
    let name = match block.id() {
        Some(id) => id.name(),
        None => "UNKNOWN_BLOCK",
    };
    println!("{:width$}<{name}>", "", width = level * 4);

    let mut record: SmallVec<[u64; 32]> = SmallVec::new();
    while let Some(entry) = bitstream.advance()? {
        match entry {
            Entry::SubBlock(block) => {
                bitstream.enter_block(block)?;
                parse_block(bitstream, block, level + 1)?;
            }
            Entry::Record(entry) => {
                let code = bitstream.read_record(entry, &mut record)?;

                if let Some(block_id) = block.id() {
                    let abbrev_id = match entry.abbreviation {
                        Some(abbreviation) => format!(" abbrevid={abbreviation}"),
                        None => String::new(),
                    };

                    let mut fields = String::new();
                    for (i, f) in record.iter().enumerate() {
                        fields.push_str(&format!(" op{}={}", i, f));
                    }

                    let name = get_record_name(block_id, code).unwrap_or("UNKNOWN_RECORD");

                    println!(
                        "{:width$}<{name}{abbrev_id}{fields} />",
                        "",
                        width = (level + 1) * 4
                    );
                }
            }
        }
    }
    println!("{:width$}</{name}>", "", width = level * 4);

    Ok(())
}
