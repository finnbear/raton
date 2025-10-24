use std::collections::HashMap;

use crate::{
    Value,
    ast::{BinaryOperator, UnaryOperator},
};

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "bitcode", derive(bitcode::Encode, bitcode::Decode))]
#[non_exhaustive]
pub enum Instruction {
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
    Call(String, u8),
    /// Return from the current function.
    Return,
    /// Discard an item popped from the stack.
    Pop,
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "bitcode", derive(bitcode::Encode, bitcode::Decode))]
#[non_exhaustive]
pub struct FunctionBytecode {
    pub instructions: Vec<Instruction>,
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "bitcode", derive(bitcode::Encode, bitcode::Decode))]
#[allow(unused)]
#[non_exhaustive]
pub struct ProgramBytecode {
    pub functions: HashMap<String, u32>,
    pub instructions: Vec<Instruction>,
}
