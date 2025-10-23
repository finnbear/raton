#![cfg_attr(test, feature(test))]

pub mod ast;
mod value;
pub use value::*;
mod parser;
pub use parser::*;
pub mod bytecode;
mod bytecode_generator;
pub use bytecode_generator::*;
mod bytecode_vm;
pub use bytecode_vm::*;
mod linker;
pub use linker::*;
#[cfg(test)]
mod benches;
