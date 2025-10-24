use super::{Expression, Identifier};

/// A statement, ending in a semicolon.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub enum Statement {
    /// See [`LetStatement`].
    Let(LetStatement),
    /// See [`AssignStatement`].
    Assign(AssignStatement),
    /// `expression;`
    Expression(Expression),
    /// See [`WhileLoop`].
    #[cfg(feature = "while_loop")]
    While(WhileLoop),
    /// `break;`
    #[cfg(feature = "while_loop")]
    Break,
    /// `continue;`
    #[cfg(feature = "while_loop")]
    Continue,
    /// See [`ReturnStatement`].
    Return(ReturnStatement),
}

/// `let identifier = expression;`
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct LetStatement {
    /// Variable to store the value in.
    pub identifier: Identifier,
    /// Expression to evaluate to produce a value.
    pub expression: Expression,
}

/// `identifier = expression;`
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct AssignStatement {
    /// Variable to store the value in.
    pub identifier: Identifier,
    /// Expression to evaluate to produce a value.
    pub expression: Expression,
}

/// `return;`
/// `return value;`
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct ReturnStatement {
    /// Return value.
    pub value: Option<Expression>,
}

/// `while condition { statement1; statement2; }`
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
#[cfg(feature = "while_loop")]
pub struct WhileLoop {
    /// Continue the loop until this evaluates to `false`.
    pub condition: Expression,
    /// Statements to repeat each iteration.
    pub body: Vec<Statement>,
}
