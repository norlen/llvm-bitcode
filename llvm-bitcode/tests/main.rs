#[test]
fn parse() {
    tracing_subscriber::fmt::init();

    let bytes = include_bytes!("../../sample/initial-1f15a977ee5b5e38.bc");
    // let bytes = include_bytes!("../../sample/symex-f4b190effaa6c89b.bc");
    llvm_bitcode::parse(bytes).unwrap();
}
