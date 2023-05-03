use criterion::{black_box, criterion_group, criterion_main, Criterion};

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
                let code = bitstream.read_record(entry, &mut record)?;
                black_box(code);
                black_box(&record);
            }
        }
    }
    Ok(())
}

fn criterion_benchmark(c: &mut Criterion) {
    let bitcode_small = include_bytes!("../../sample/initial-1f15a977ee5b5e38.bc");
    let bitcode_large = include_bytes!("../../sample/symex-f4b190effaa6c89b.bc");

    c.bench_function("bitcode small", |b| {
        let mut bitstream = BitstreamReader::from_bytes(bitcode_small).unwrap();
        b.iter(|| read_bitcode(black_box(&mut bitstream)))
    });

    c.bench_function("bitcode large", |b| {
        let mut bitstream = BitstreamReader::from_bytes(bitcode_large).unwrap();
        b.iter(|| read_bitcode(black_box(&mut bitstream)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
