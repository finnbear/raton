use crate::{
    Value,
    ast::{BinaryOp, UnaryOp},
};

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum Instruction {
    LoadConst(Value),
    LoadVar(u8),
    StoreVar(u8),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    Jump(usize),
    JumpIfFalse(usize),
    Call(String, usize),
    Return,
    Pop,
}
