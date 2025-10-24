use super::{Expression, Identifier};

/// A statement, ending in a semicolon.
#[derive(Debug, Clone, PartialEq)]
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
    /// `return;`
    /// `return value;`
    Return(Option<Expression>),
}

/// `let identifier = expression;`
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct LetStatement {
    pub identifier: Identifier,
    pub expression: Expression,
}

/// `identifier = expression;`
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct AssignStatement {
    pub identifier: Identifier,
    pub expression: Expression,
}

/// `while condition { statement1; statement2; }`
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
#[cfg(feature = "while_loop")]
pub struct WhileLoop {
    pub condition: Expression,
    pub body: Vec<Statement>,
}
