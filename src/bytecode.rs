use crate::{
    Value,
    ast::{BinaryOp, UnaryOp},
};

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum Instruction {
    /// Push the constant onto the stack.
    LoadConst(Value),
    /// Push the indexed variable's value onto the stack.
    LoadVar(u8),
    /// Store a value popped from the stack into the indexed variable.
    StoreVar(u8),
    /// Push the result of the unary operation on a value popped from the stack.
    UnaryOp(UnaryOp),
    /// Push the result of the binary operation on two values popped from the stack.
    BinaryOp(BinaryOp),
    /// Jump to the indexed instruction.
    Jump(u32),
    /// Jump to the indexed instruction if a value peeked from the stack is the bool 'false'.
    JumpIfFalse(u32),
    /// Call the named function with the given number of arguments.
    Call(String, u8),
    /// Return from the current function.
    Return,
    /// Discard an item popped from the stack.
    Pop,
}
