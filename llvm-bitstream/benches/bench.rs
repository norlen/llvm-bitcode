use criterion::{black_box, criterion_group, criterion_main, Criterion};

use llvm_bitstream::{BitstreamReader, Entry};
use smallvec::SmallVec;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn read_bitcode<T: AsRef<[u8]>>(bitstream: &mut BitstreamReader<T>) -> Result<()> {
    while !bitstream.cursor().is_empty() {
        let entry = bitstream.advance()?.expect("Should never end");

        match entry {
            Entry::SubBlock(block) => {
                bitstream.enter_block(block)?;
                parse_block(bitstream)?;
            }
            Entry::Record(_) => panic!("no records here"),
        };
    }
    Ok(())
}

fn parse_block<T: AsRef<[u8]>>(bitstream: &mut BitstreamReader<T>) -> Result<()> {
    let mut record: SmallVec<[u64; 64]> = SmallVec::new();

    while let Some(entry) = bitstream.advance()? {
        match entry {
            Entry::SubBlock(block) => {
                bitstream.enter_block(block)?;
                parse_block(bitstream)?;
            }
            Entry::Record(entry) => {
                let code = bitstream.read_record(entry, &mut record)?;
                black_box(code);
                black_box(&record);
            }
        }
    }
    Ok(())
}

fn parse<T: AsRef<[u8]>>(bytes: T) -> Result<()> {
    let mut bitstream = BitstreamReader::from_bytes(bytes)?;
    read_bitcode(&mut bitstream)?;
    Ok(())
}

fn criterion_benchmark(c: &mut Criterion) {
    let bitcode_small = include_bytes!("../../sample/initial-1f15a977ee5b5e38.bc");
    let bitcode_large = include_bytes!("../../sample/symex-f4b190effaa6c89b.bc");

    c.bench_function("bitcode small", |b| {
        b.iter(|| parse(black_box(bitcode_small)))
    });

    c.bench_function("bitcode large", |b| {
        b.iter(|| parse(black_box(bitcode_large)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
