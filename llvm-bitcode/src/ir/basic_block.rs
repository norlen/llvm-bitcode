#![allow(unused)]

// move
pub struct Instruction;
pub struct Terminator;
pub struct Identifier;

/// LLVM Basic block.
///
///
pub struct BasicBlock {
    name: Identifier,
    instructions: Vec<Instruction>,
    terminator: Terminator,
}
