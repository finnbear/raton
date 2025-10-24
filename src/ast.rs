use crate::Value;

type Identifier = String;

/// An operator that takes one operand.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "bitcode", derive(bitcode::Encode, bitcode::Decode))]
#[non_exhaustive]
pub enum UnaryOperator {
    /// !
    #[cfg(feature = "bool_type")]
    Not,
    /// -
    Neg,
}

/// An operator that takes two operands.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "bitcode", derive(bitcode::Encode, bitcode::Decode))]
#[non_exhaustive]
pub enum BinaryOperator {
    /// +
    Add,
    /// -
    Subtract,
    /// *
    Multiply,
    /// /
    Divide,
    /// %
    Modulo,
    /// ==
    #[cfg(feature = "bool_type")]
    Equal,
    /// !=
    #[cfg(feature = "bool_type")]
    NotEqual,
    /// <
    #[cfg(feature = "bool_type")]
    LessThan,
    /// <=
    #[cfg(feature = "bool_type")]
    LessThanOrEqual,
    /// >
    #[cfg(feature = "bool_type")]
    GreaterThan,
    /// >=
    #[cfg(feature = "bool_type")]
    GreaterThanOrEqual,
    /// &&
    ///
    /// This is short-circuiting.
    #[cfg(feature = "bool_type")]
    And,
    /// ||
    ///
    /// This is short-circuiting.
    #[cfg(feature = "bool_type")]
    Or,
}

/// An expresssion that may be evaluated to produce a value.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub enum Expression {
    /// null
    /// true
    /// 42
    /// 3.14
    /// "hello"
    Literal(Value),
    /// argname
    /// varname
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

/// -operand
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub operand: Box<Expression>,
}

/// lhs * rhs
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
}

/// identifier(arg1, arg2)
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct CallExpression {
    pub identifier: Identifier,
    pub arguments: Vec<Expression>,
}

/// if cond { then_branch }
/// if cond { then_branch } else { else_branch }
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg(feature = "if_expression")]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub then_branch: BlockExpression,
    pub else_branch: Option<BlockExpression>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
/// A sequence of statements, possibly followed by an expression,
/// in curly brackets.
///
/// { stmt1; stmt2; }
/// { stmt1; stmt2; value }
pub struct BlockExpression {
    /// Statements to execute sequentially.
    pub statements: Vec<Statement>,
    /// Expression to evaluate to produce a value, the value of the block.
    pub value: Option<Box<Expression>>,
}

/// A statement, ending in a semicolon.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub enum Statement {
    /// See [`LetStatement`].
    Let(LetStatement),
    /// See [`AssignStatement`].
    Assign(AssignStatement),
    /// expression;
    Expression(Expression),
    /// See [`WhileLoop`].
    #[cfg(feature = "while_loop")]
    While(WhileLoop),
    /// break;
    #[cfg(feature = "while_loop")]
    Break,
    /// continue;
    #[cfg(feature = "while_loop")]
    Continue,
    /// return;
    /// return value;
    Return(Option<Expression>),
}

/// let identifier = expression;
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct LetStatement {
    pub identifier: Identifier,
    pub expression: Expression,
}

/// identifier = expression;
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct AssignStatement {
    pub identifier: Identifier,
    pub expression: Expression,
}

/// while condition { statement1; statement2; }
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
#[cfg(feature = "while_loop")]
pub struct WhileLoop {
    pub condition: Expression,
    pub body: Vec<Statement>,
}

/// A function with a name, parameters, and a body.
///
/// fn identifier(params) { body }
#[derive(Debug, Clone, PartialEq)]
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
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct Program {
    pub functions: Vec<Function>,
}
