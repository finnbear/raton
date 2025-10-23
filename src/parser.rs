use crate::Value;
use crate::ast::*;
use nom::character::complete::multispace1;
use nom::{
    IResult, Parser as NomParser,
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0},
    combinator::{map, opt, recognize, value},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, terminated},
};
use nom_language::precedence::{Assoc, Operation, binary_op, precedence, unary_op};
use std::fmt::{self, Debug, Display, Write};
use std::marker::PhantomData;
use std::ops::Range;
use thiserror::Error;

#[non_exhaustive]
pub struct Parser {
    _hidden: PhantomData<()>,
}

impl Debug for Parser {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Parser").finish_non_exhaustive()
    }
}

#[derive(Clone, Debug, Error)]
#[error("parse error from {} to {}: {reason}", span.start, span.end)]
pub struct ParseError {
    pub span: Range<usize>,
    pub reason: ParseErrorReason,
}

#[derive(Clone, Debug)]
pub enum ParseErrorReason {
    Unexpected {
        expected: Vec<String>,
        found: Option<char>,
    },
    Other(String),
}

impl Display for ParseErrorReason {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unexpected { expected, found } => {
                write!(f, "expected one of [{expected:?}], found ")?;
                if let Some(found) = found {
                    f.write_char(*found)
                } else {
                    f.write_str("none")
                }
            }
            Self::Other(other) => f.write_str(other),
        }
    }
}

mod depth_limiter {
    use std::sync::atomic::{AtomicU32, Ordering};

    thread_local! {
        static DEPTH: AtomicU32 = AtomicU32::new(0);
    }
    #[must_use]
    pub struct DepthGuard {}

    pub fn dive(i: &str) -> Result<DepthGuard, nom::Err<nom::error::Error<&str>>> {
        DEPTH.with(|depth| {
            let depth = depth.fetch_add(1, Ordering::Relaxed);
            if depth > 20 {
                return Err(nom::Err::Failure(nom::error::Error::new(
                    i,
                    nom::error::ErrorKind::TooLarge,
                )));
            }
            Ok(DepthGuard {})
        })
    }

    impl Drop for DepthGuard {
        fn drop(&mut self) {
            DEPTH.with(|depth| {
                depth.fetch_sub(1, Ordering::Relaxed);
            })
        }
    }
}

impl Parser {
    pub fn new() -> Self {
        Self {
            _hidden: PhantomData,
        }
    }

    pub fn parse<'src>(&self, src: &'src str) -> Result<Program, Vec<ParseError>> {
        let ret = parse_program.parse(src);

        match ret {
            Ok((i, p)) => {
                if i.is_empty() {
                    return Ok(p);
                } else {
                    return Err(Vec::new());
                }
            }
            Err(_) => return Err(Vec::new()),
        }
    }
}

// Whitespace helper
fn ws<'a, F, O>(inner: F) -> impl NomParser<&'a str, Output = O, Error = nom::error::Error<&'a str>>
where
    F: NomParser<&'a str, Output = O, Error = nom::error::Error<&'a str>>,
{
    delimited(multispace0, inner, multispace0)
}

fn identifier(i: &str) -> IResult<&str, String> {
    let (i, _) = multispace0(i)?;
    let (i, first) = alt((alpha1, tag("_"))).parse(i)?;
    let (i, rest) = recognize(many0(alt((alphanumeric1, tag("_"))))).parse(i)?;
    let (i, _) = multispace0(i)?;

    let ident = format!("{}{}", first, rest);

    // Check for keywords
    match ident.as_str() {
        "let" | "if" | "else" | "while" | "break" | "continue" | "return" | "fn" | "true"
        | "false" | "null" => Err(nom::Err::Error(nom::error::Error::new(
            i,
            nom::error::ErrorKind::Tag,
        ))),
        _ => Ok((i, ident)),
    }
}

fn parse_null(i: &str) -> IResult<&str, Value> {
    value(Value::Null, ws(tag("null"))).parse(i)
}

fn parse_bool(i: &str) -> IResult<&str, Value> {
    alt((
        value(Value::Bool(true), ws(tag("true"))),
        value(Value::Bool(false), ws(tag("false"))),
    ))
    .parse(i)
}

fn parse_f32(i: &str) -> IResult<&str, Value> {
    let (i, _) = multispace0.parse(i)?;
    let (i, num_str) = recognize((opt(char('-')), digit1, char('.'), digit1)).parse(i)?;
    let (i, _) = multispace0.parse(i)?;

    match num_str.parse::<f32>() {
        Ok(f) => Ok((i, Value::F32(f))),
        Err(_) => Err(nom::Err::Error(nom::error::Error::new(
            i,
            nom::error::ErrorKind::Verify,
        ))),
    }
}

fn parse_i32(i: &str) -> IResult<&str, Value> {
    let (i, _) = multispace0.parse(i)?;
    let (i, num_str) = recognize(pair(opt(char('-')), digit1)).parse(i)?;
    let (i, _) = multispace0.parse(i)?;

    match num_str.parse::<i32>() {
        Ok(n) => Ok((i, Value::I32(n))),
        Err(_) => Err(nom::Err::Error(nom::error::Error::new(
            i,
            nom::error::ErrorKind::Verify,
        ))),
    }
}

fn parse_string(i: &str) -> IResult<&str, Value> {
    let (i, _) = multispace0.parse(i)?;
    let (i, _) = char('"').parse(i)?;
    let (i, content) = take_while1(|c| c != '"').parse(i)?;
    let (i, _) = char('"').parse(i)?;
    let (i, _) = multispace0.parse(i)?;

    Ok((i, Value::String(content.to_string())))
}

fn parse_literal(i: &str) -> IResult<&str, Value> {
    alt((parse_null, parse_bool, parse_f32, parse_i32, parse_string)).parse(i)
}

// Prefix operators for precedence parser
#[derive(Copy, Clone)]
enum PrefixOp {
    Not,
    Neg,
}

// Postfix operators for precedence parser
#[derive(Clone)]
enum PostfixOp {
    Call(Vec<Expr>),
}

// Binary operators for precedence parser
#[derive(Copy, Clone)]
enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

fn function_call(i: &str) -> IResult<&str, PostfixOp> {
    map(
        delimited(
            ws(tag("(")),
            separated_list0(ws(tag(",")), expression),
            ws(tag(")")),
        ),
        PostfixOp::Call,
    )
    .parse(i)
}

fn parse_block(i: &str) -> IResult<&str, Block> {
    let (mut i, _) = ws(tag("{")).parse(i)?;

    let mut statements = Vec::new();
    let mut value = None;

    loop {
        let (i2, soe) = opt(parse_stmt_or_expr).parse(i)?;
        i = i2;

        if let Some(soe) = soe {
            if value.is_some() {
                return Err(nom::Err::Failure(nom::error::Error::new(
                    i,
                    nom::error::ErrorKind::Verify,
                )));
            }

            match soe {
                StmtOrExpr::Expr(expr) => {
                    value = Some(expr);
                }
                StmtOrExpr::Stmt(stmt) => {
                    statements.push(stmt);
                }
            }
        } else {
            break;
        }
    }

    let (i, _) = ws(tag("}")).parse(i)?;

    Ok((i, Block {
        statements,
        value: value.map(Box::new),
    }))
}

// If expression parser
fn parse_if(i: &str) -> IResult<&str, Expr> {
    let (i, _) = ws(terminated(tag("if"), multispace1)).parse(i)?;
    let (i, cond) = expression.parse(i)?;
    let (i, then_branch) = parse_block.parse(i)?;
    let (i, else_branch) = opt(preceded(ws(tag("else")), parse_block)).parse(i)?;
    Ok((i, Expr::If {
        cond: Box::new(cond),
        then_branch,
        else_branch,
    }))
}

// Primary expression (atom)
fn primary_expr(i: &str) -> IResult<&str, Expr> {
    alt((
        map(parse_literal, Expr::Literal),
        parse_if,
        map(parse_block, Expr::Block),
        map(identifier, Expr::Variable),
        delimited(ws(tag("(")), expression, ws(tag(")"))),
    ))
    .parse(i)
}

// Main expression parser using precedence
fn expression(i: &str) -> IResult<&str, Expr> {
    let _guard = depth_limiter::dive(i)?;

    precedence(
        // Prefix operators
        alt((
            unary_op(2, value(PrefixOp::Not, ws(tag("!")))),
            unary_op(2, value(PrefixOp::Neg, ws(tag("-")))),
        )),
        // Postfix operators (function calls)
        unary_op(1, function_call),
        // Binary operators with precedence levels
        alt((
            // Level 3: Multiplicative
            binary_op(
                3,
                Assoc::Left,
                alt((
                    value(BinOp::Mul, ws(tag("*"))),
                    value(BinOp::Div, ws(tag("/"))),
                    value(BinOp::Mod, ws(tag("%"))),
                )),
            ),
            // Level 4: Additive
            binary_op(
                4,
                Assoc::Left,
                alt((
                    value(BinOp::Add, ws(tag("+"))),
                    value(BinOp::Sub, ws(tag("-"))),
                )),
            ),
            // Level 5: Comparison
            binary_op(
                5,
                Assoc::Left,
                alt((
                    value(BinOp::Le, ws(tag("<="))),
                    value(BinOp::Ge, ws(tag(">="))),
                    value(BinOp::Lt, ws(tag("<"))),
                    value(BinOp::Gt, ws(tag(">"))),
                )),
            ),
            // Level 6: Equality
            binary_op(
                6,
                Assoc::Left,
                alt((
                    value(BinOp::Eq, ws(tag("=="))),
                    value(BinOp::Ne, ws(tag("!="))),
                )),
            ),
            // Level 7: Logical AND
            binary_op(7, Assoc::Left, value(BinOp::And, ws(tag("&&")))),
            // Level 8: Logical OR
            binary_op(8, Assoc::Left, value(BinOp::Or, ws(tag("||")))),
        )),
        primary_expr,
        |op: Operation<PrefixOp, PostfixOp, BinOp, Expr>| -> Result<Expr, ()> {
            use Operation::*;
            match op {
                Prefix(PrefixOp::Not, e) => Ok(Expr::Unary(UnaryOp::Not, Box::new(e))),
                Prefix(PrefixOp::Neg, e) => Ok(Expr::Unary(UnaryOp::Neg, Box::new(e))),
                Postfix(Expr::Variable(name), PostfixOp::Call(args)) => Ok(Expr::Call(name, args)),
                Postfix(_, PostfixOp::Call(_)) => Err(()),
                Binary(lhs, BinOp::Add, rhs) => {
                    Ok(Expr::Binary(BinaryOp::Add, Box::new(lhs), Box::new(rhs)))
                }
                Binary(lhs, BinOp::Sub, rhs) => {
                    Ok(Expr::Binary(BinaryOp::Sub, Box::new(lhs), Box::new(rhs)))
                }
                Binary(lhs, BinOp::Mul, rhs) => {
                    Ok(Expr::Binary(BinaryOp::Mul, Box::new(lhs), Box::new(rhs)))
                }
                Binary(lhs, BinOp::Div, rhs) => {
                    Ok(Expr::Binary(BinaryOp::Div, Box::new(lhs), Box::new(rhs)))
                }
                Binary(lhs, BinOp::Mod, rhs) => {
                    Ok(Expr::Binary(BinaryOp::Mod, Box::new(lhs), Box::new(rhs)))
                }
                Binary(lhs, BinOp::Eq, rhs) => {
                    Ok(Expr::Binary(BinaryOp::Eq, Box::new(lhs), Box::new(rhs)))
                }
                Binary(lhs, BinOp::Ne, rhs) => {
                    Ok(Expr::Binary(BinaryOp::Ne, Box::new(lhs), Box::new(rhs)))
                }
                Binary(lhs, BinOp::Lt, rhs) => {
                    Ok(Expr::Binary(BinaryOp::Lt, Box::new(lhs), Box::new(rhs)))
                }
                Binary(lhs, BinOp::Le, rhs) => {
                    Ok(Expr::Binary(BinaryOp::Le, Box::new(lhs), Box::new(rhs)))
                }
                Binary(lhs, BinOp::Gt, rhs) => {
                    Ok(Expr::Binary(BinaryOp::Gt, Box::new(lhs), Box::new(rhs)))
                }
                Binary(lhs, BinOp::Ge, rhs) => {
                    Ok(Expr::Binary(BinaryOp::Ge, Box::new(lhs), Box::new(rhs)))
                }
                Binary(lhs, BinOp::And, rhs) => {
                    Ok(Expr::Binary(BinaryOp::And, Box::new(lhs), Box::new(rhs)))
                }
                Binary(lhs, BinOp::Or, rhs) => {
                    Ok(Expr::Binary(BinaryOp::Or, Box::new(lhs), Box::new(rhs)))
                }
            }
        },
    )(i)
}

fn parse_let(i: &str) -> IResult<&str, Stmt> {
    let (i, _) = ws(terminated(tag("let"), multispace1)).parse(i)?;
    let (i, name) = identifier.parse(i)?;
    let (i, _) = ws(tag("=")).parse(i)?;
    let (i, expr) = expression.parse(i)?;
    let (i, _) = ws(tag(";")).parse(i)?;

    Ok((i, Stmt::Let(name, expr)))
}

fn parse_assign(i: &str) -> IResult<&str, Stmt> {
    let (i, name) = identifier.parse(i)?;
    let (i, _) = ws(tag("=")).parse(i)?;
    let (i, expr) = expression.parse(i)?;
    let (i, _) = ws(tag(";")).parse(i)?;

    Ok((i, Stmt::Assign(name, expr)))
}

fn parse_while(i: &str) -> IResult<&str, Stmt> {
    let (i, _) = ws(terminated(tag("while"), multispace1)).parse(i)?;
    let (i, cond) = expression.parse(i)?;
    let (i, _) = ws(tag("{")).parse(i)?;
    let (i, body) = many0(parse_stmt).parse(i)?;
    let (i, _) = ws(tag("}")).parse(i)?;
    Ok((i, Stmt::While { cond, body }))
}

fn parse_break(i: &str) -> IResult<&str, Stmt> {
    let (i, _) = ws(tag("break")).parse(i)?;
    let (i, _) = ws(tag(";")).parse(i)?;
    Ok((i, Stmt::Break))
}

fn parse_continue(i: &str) -> IResult<&str, Stmt> {
    let (i, _) = ws(tag("continue")).parse(i)?;
    let (i, _) = ws(tag(";")).parse(i)?;
    Ok((i, Stmt::Continue))
}

fn parse_return(i: &str) -> IResult<&str, Stmt> {
    let (i, _) = ws(terminated(tag("return"), multispace1)).parse(i)?;
    let (i, expr) = opt(expression).parse(i)?;
    let (i, _) = ws(tag(";")).parse(i)?;
    Ok((i, Stmt::Return(expr)))
}

fn parse_expr_stmt(i: &str) -> IResult<&str, Stmt> {
    let (i, expr) = expression.parse(i)?;
    let (i, _) = ws(tag(";")).parse(i)?;
    Ok((i, Stmt::Expr(expr)))
}

fn parse_stmt(i: &str) -> IResult<&str, Stmt> {
    alt((
        parse_let,
        parse_while,
        parse_break,
        parse_continue,
        parse_return,
        parse_assign,
        parse_expr_stmt,
    ))
    .parse(i)
}

enum StmtOrExpr {
    Stmt(Stmt),
    Expr(Expr),
}

fn parse_stmt_or_expr(i: &str) -> IResult<&str, StmtOrExpr> {
    let (i, expr) = opt(expression).parse(i)?;

    if let Some(expr) = expr {
        let (i, semi) = opt(ws(tag(";"))).parse(i)?;
        return Ok((
            i,
            if semi.is_some() {
                StmtOrExpr::Stmt(Stmt::Expr(expr))
            } else {
                StmtOrExpr::Expr(expr)
            },
        ));
    }

    let (i, stmt) = alt((
        parse_let,
        parse_while,
        parse_break,
        parse_continue,
        parse_return,
        parse_assign,
    ))
    .parse(i)?;

    Ok((i, StmtOrExpr::Stmt(stmt)))
}

fn parse_function(i: &str) -> IResult<&str, Function> {
    let (i, _) = ws(terminated(tag("fn"), multispace1)).parse(i)?;
    let (i, name) = identifier.parse(i)?;
    let (i, _) = ws(tag("(")).parse(i)?;
    let (i, params) = separated_list0(ws(tag(",")), identifier).parse(i)?;
    let (i, _) = ws(tag(")")).parse(i)?;
    let (i, block) = parse_block(i)?;

    let mut body = block.statements;
    if let Some(value) = block.value {
        body.push(Stmt::Return(Some(*value)));
    }

    Ok((i, Function { name, params, body }))
}

pub fn parse_program(i: &str) -> IResult<&str, Program> {
    let (i, functions) = many0(parse_function).parse(i)?;
    let (i, _) = multispace0(i)?;
    Ok((i, Program { functions }))
}
