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
    AllocVariables(u16),
    /// Push the constant onto the stack.
    LoadConstant(Value),
    /// Push the indexed variable's value onto the stack.
    LoadVariable(u16),
    /// Store a value popped from the stack into the indexed variable.
    StoreVariable(u16),
    /// Push the result of the unary operation on a value popped from the stack.
    UnaryOperator(UnaryOperator),
    /// Push the result of the binary operation on two values popped from the stack.
    BinaryOperator(BinaryOperator),
    /// Jump to the indexed instruction.
    Jump(u32),
    /// Jump to the indexed instruction if a value peeked from the stack is the bool 'false'.
    #[cfg(feature = "bool_type")]
    JumpIfFalse(u32),
    /// Call the named function with the given number of arguments.
    CallByName(String, u8),
    /// Call the function at the given address with the given number of arguments.
    CallByAddress(u32, u8),
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
    /// Information on public functions, callable from outside the program.
    pub public_functions: HashMap<String, PublicFunction>,
    /// The bytecode instruction stream of the program.
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
    /// The address of the first instruction of the function.
    pub address: u32,
    /// The number of arguments of the function.
    pub arguments: u8,
}
