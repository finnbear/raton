use crate::Value;

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum UnaryOp {
    /// !
    Not,
    /// -
    Neg,
}

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum BinaryOp {
    /// +
    Add,
    /// -
    Sub,
    /// *
    Mul,
    /// /
    Div,
    /// %
    Mod,
    /// ==
    Eq,
    /// !=
    Ne,
    /// <
    Lt,
    /// <=
    Le,
    /// >
    Gt,
    /// >=
    Ge,
    /// &&
    ///
    /// This is short-circuiting.
    And,
    /// ||
    ///
    /// This is short-circuiting.
    Or,
}

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum Expr {
    Literal(Value),
    Variable(String),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    If {
        cond: Box<Expr>,
        then_branch: Block,
        else_branch: Option<Block>,
    },
    Block(Block),
}

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub value: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum Stmt {
    Let(String, Expr),
    Assign(String, Expr),
    Expr(Expr),
    While { cond: Expr, body: Vec<Stmt> },
    Break,
    Continue,
    Return(Option<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub struct Program {
    pub functions: Vec<Function>,
}
