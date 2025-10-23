use crate::Value;

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "bitcode", derive(bitcode::Encode, bitcode::Decode))]
#[non_exhaustive]
pub enum UnaryOp {
    /// !
    #[cfg(feature = "bool_type")]
    Not,
    /// -
    Neg,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "bitcode", derive(bitcode::Encode, bitcode::Decode))]
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
    #[cfg(feature = "bool_type")]
    Eq,
    /// !=
    #[cfg(feature = "bool_type")]
    Ne,
    /// <
    #[cfg(feature = "bool_type")]
    Lt,
    /// <=
    #[cfg(feature = "bool_type")]
    Le,
    /// >
    #[cfg(feature = "bool_type")]
    Gt,
    /// >=
    #[cfg(feature = "bool_type")]
    Ge,
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

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
    #[cfg(feature = "if_expression")]
    If {
        cond: Box<Expr>,
        then_branch: Block,
        else_branch: Option<Block>,
    },
    Block(Block),
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
/// { stmt1; stmt2; }
/// { stmt1; stmt2; value }
pub struct Block {
    pub statements: Vec<Stmt>,
    pub value: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub enum Stmt {
    /// let name = expr;
    Let(String, Expr),
    /// name = expr;
    Assign(String, Expr),
    /// expr;
    Expr(Expr),
    /// while cond { stmt1; stmt2; }
    #[cfg(feature = "while_loop")]
    While { cond: Expr, body: Vec<Stmt> },
    /// break;
    #[cfg(feature = "while_loop")]
    Break,
    /// continue;
    #[cfg(feature = "while_loop")]
    Continue,
    /// return;
    /// return value;
    Return(Option<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct Program {
    pub functions: Vec<Function>,
}
