use criterion::{black_box, criterion_group, criterion_main, Criterion};

use llvm_bitcode::parse;

fn criterion_benchmark(c: &mut Criterion) {
    let bitcode_small = include_bytes!("../../sample/initial-1f15a977ee5b5e38.bc");
    let bitcode_large = include_bytes!("../../sample/symex-f4b190effaa6c89b.bc");

    c.bench_function("parse bitcode small", |b| {
        b.iter(|| parse(black_box(bitcode_small)))
    });

    c.bench_function("parse bitcode large", |b| {
        b.iter(|| parse(black_box(bitcode_large)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
