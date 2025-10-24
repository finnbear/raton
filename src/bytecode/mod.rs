//! Bytecode type definitions.

use crate::{BinaryOperator, UnaryOperator, Value};
use std::collections::HashMap;

/// A single bytecode instruction, which may contain arguments.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "bitcode", derive(bitcode::Encode, bitcode::Decode))]
#[non_exhaustive]
pub enum Instruction {
    /// Allocate this many variables on the stack.
    AllocVars(u8),
    /// Push the constant onto the stack.
    LoadConst(Value),
    /// Push the indexed variable's value onto the stack.
    LoadVar(u8),
    /// Store a value popped from the stack into the indexed variable.
    StoreVar(u8),
    /// Push the result of the unary operation on a value popped from the stack.
    UnaryOp(UnaryOperator),
    /// Push the result of the binary operation on two values popped from the stack.
    BinaryOp(BinaryOperator),
    /// Jump to the indexed instruction.
    Jump(u32),
    /// Jump to the indexed instruction if a value peeked from the stack is the bool 'false'.
    #[cfg(feature = "bool_type")]
    JumpIfFalse(u32),
    /// Call the named function with the given number of arguments.
    CallByName(String, u8),
    /// Call the function at the given instruction pointer with the given number of arguments.
    CallByIp(u32, u8),
    /// Return from the current function.
    Return,
    /// Discard an item popped from the stack.
    Pop,
}

/// A bytecode instruction stream for a whole program, with known entry
/// points for public functions.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "bitcode", derive(bitcode::Encode, bitcode::Decode))]
#[allow(unused)]
#[non_exhaustive]
pub struct ProgramBytecode {
    pub public_functions: HashMap<String, PublicFunction>,
    pub instructions: Vec<Instruction>,
}

/// Information on a public function, allowing it to be called.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "bitcode", derive(bitcode::Encode, bitcode::Decode))]
#[allow(unused)]
#[non_exhaustive]
pub struct PublicFunction {
    pub ip: u32,
    pub arguments: u8,
}
