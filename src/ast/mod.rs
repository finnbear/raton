//! Abstract syntax tree type definitions.

mod expression;
pub use expression::*;
mod statement;
pub use statement::*;

pub type Identifier = String;

/// A function with a name, parameters, and a body.
///
/// fn identifier(params) { body }
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct Function {
    /// The name that can be used to call the function.
    pub identifier: Identifier,
    /// The names of the function parameters.
    pub arguments: Vec<Identifier>,
    /// The statementse that will be executed when the function is called.
    pub body: BlockExpression,
}

/// A abstract syntax tree representing a program.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct Program {
    pub functions: Vec<Function>,
}
