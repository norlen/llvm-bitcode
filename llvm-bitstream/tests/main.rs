use llvm_bitstream::{BitstreamReader, Entry};
use smallvec::SmallVec;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn read_bitcode<T: AsRef<[u8]>>(bitstream: &mut BitstreamReader<T>) -> Result<()> {
    while !bitstream.cursor().is_empty() {
        let entry = bitstream.advance()?.expect("Should never end");

        match entry {
            Entry::SubBlock(_) => parse_block(bitstream)?,
            Entry::Record(_) => panic!("no records here"),
        };
    }
    Ok(())
}

fn parse_block<T: AsRef<[u8]>>(bitstream: &mut BitstreamReader<T>) -> Result<()> {
    let mut record: SmallVec<[u64; 32]> = SmallVec::new();

    while let Some(entry) = bitstream.advance()? {
        match entry {
            Entry::SubBlock(_) => {
                parse_block(bitstream)?;
            }
            Entry::Record(entry) => {
                let _code = bitstream.read_record(entry, &mut record)?;
            }
        }
    }
    Ok(())
}

#[test]
fn running() {
    // let bytes = include_bytes!("../../sample/initial-1f15a977ee5b5e38.bc");
    let bytes = include_bytes!("../../sample/symex-f4b190effaa6c89b.bc");

    let mut bitstream = BitstreamReader::from_bytes(bytes).unwrap();
    read_bitcode(&mut bitstream).unwrap();
}
