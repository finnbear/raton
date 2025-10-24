#![cfg_attr(test, feature(test))]
#![forbid(unsafe_code)]

pub mod ast;
pub mod bytecode;
mod common;
pub mod compiler;
pub mod runtime;
pub use common::*;
pub mod prelude;

#[cfg(all(test, not(miri)))]
mod benches;
