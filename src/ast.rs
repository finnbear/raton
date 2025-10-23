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
    /// null
    /// true
    /// 42
    /// 3.14
    /// "hello"
    Literal(Value),
    /// argname
    /// varname
    Variable(String),
    /// -e
    Unary(UnaryOp, Box<Expr>),
    /// lhs * rhs
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    /// func(arg1, arg2)
    Call(String, Vec<Expr>),
    /// if cond { block }
    /// if cond { block } else { block }
    If {
        cond: Box<Expr>,
        then_branch: Block,
        else_branch: Option<Block>,
    },
    Block(Block),
}

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
/// { stmt1; stmt2; }
/// { stmt1; stmt2; value }
pub struct Block {
    pub statements: Vec<Stmt>,
    pub value: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum Stmt {
    /// let name = expr;
    Let(String, Expr),
    /// name = expr;
    Assign(String, Expr),
    /// expr;
    Expr(Expr),
    /// while cond { stmt1; stmt2; }
    While { cond: Expr, body: Vec<Stmt> },
    /// break;
    Break,
    /// continue;
    Continue,
    /// return;
    /// return value;
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
