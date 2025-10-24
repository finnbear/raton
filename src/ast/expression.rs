use super::{Identifier, Statement};
use crate::{BinaryOperator, UnaryOperator, Value};

/// An expresssion that may be evaluated to produce a value.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub enum Expression {
    /// `null`
    /// `true`
    /// `42`
    /// `3.14`
    /// `"hello"`
    Literal(Value),
    /// `argname`
    /// `varname`
    Variable(Identifier),
    /// See [`UnaryExpression`].
    Unary(UnaryExpression),
    /// See [`BinaryExpression`].
    Binary(BinaryExpression),
    /// See [`CallExpression`].
    Call(CallExpression),
    #[cfg(feature = "if_expression")]
    /// See [`IfExpression`].
    If(IfExpression),
    /// See [`BlockExpression`].
    Block(BlockExpression),
}

/// `-operand`
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub operand: Box<Expression>,
}

/// `lhs * rhs`
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
}

/// `identifier(arg1, arg2)`
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct CallExpression {
    pub identifier: Identifier,
    pub arguments: Vec<Expression>,
}

/// `if cond { then_branch }`
/// `if cond { then_branch } else { else_branch }`
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg(feature = "if_expression")]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub then_branch: BlockExpression,
    /// If absent, a false condition means the [`IfExpression`]
    /// implicitly evaluates to [`Value::Null`].
    pub else_branch: Option<BlockExpression>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
/// A sequence of statements, possibly followed by an expression,
/// in curly brackets.
///
/// `{ stmt1; stmt2; }`
/// `{ stmt1; stmt2; value }`
pub struct BlockExpression {
    /// Statements to execute sequentially.
    pub statements: Vec<Statement>,
    /// Expression to evaluate to produce a value, the value of the block.
    pub value: Option<Box<Expression>>,
}
