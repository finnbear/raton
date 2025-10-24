//! Compile a program abstract-syntax-tree to bytecode.

mod bytecode_generator;
pub use bytecode_generator::*;
mod parser;
pub use parser::*;
