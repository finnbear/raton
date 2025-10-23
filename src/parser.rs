use crate::Value;
use crate::ast::*;
#[allow(unused_imports)]
use nom::{
    IResult, Parser as NomParser,
    branch::alt,
    bytes::complete::{tag, take_until, take_while1},
    character::complete::{alpha1, alphanumeric1, anychar, char, digit1, multispace0},
    combinator::{cut, eof, map, not, opt, peek, recognize, value, verify},
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
            if depth > 50 {
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

/// Parse a single comment.
#[cfg(any(feature = "single_line_comment", feature = "multi_line_comment"))]
pub fn comment(i: &str) -> IResult<&str, &str> {
    preceded(
        char('/'),
        alt((
            #[cfg(feature = "single_line_comment")]
            preceded(
                char('/'),
                terminated(take_until("\n"), alt((tag("\n"), eof))),
            ),
            #[cfg(feature = "multi_line_comment")]
            preceded(char('*'), cut(terminated(take_until("*/"), tag("*/")))),
        )),
    )
    .parse(i)
}

/// Parse several comments.
#[cfg(any(feature = "single_line_comment", feature = "multi_line_comment"))]
pub fn comments(i: &str) -> IResult<&str, &str> {
    recognize(many0(terminated(comment, multispace0))).parse(i)
}

/// In-between token parser (spaces and comments).
fn blank(i: &str) -> IResult<&str, ()> {
    #[cfg(any(feature = "single_line_comment", feature = "multi_line_comment"))]
    return value((), preceded(multispace0, comments)).parse(i);
    #[cfg(not(any(feature = "single_line_comment", feature = "multi_line_comment")))]
    return value((), multispace0).parse(i);
}

// Whitespace helper
fn ws<'a, F, O>(inner: F) -> impl NomParser<&'a str, Output = O, Error = nom::error::Error<&'a str>>
where
    F: NomParser<&'a str, Output = O, Error = nom::error::Error<&'a str>>,
{
    delimited(blank, inner, blank)
}

// Keyword that must not be the prefix of an ident.
fn keyword<'a>(
    k: &'a str,
) -> impl NomParser<&'a str, Output = &'a str, Error = nom::error::Error<&'a str>> {
    terminated(
        tag(k),
        not(verify(peek(anychar), |&c: &char| {
            c.is_ascii_alphanumeric() || c == '_'
        })),
    )
}

fn identifier(i: &str) -> IResult<&str, String> {
    let (i, _) = blank(i)?;
    let (i, ident) = recognize((
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))
    .parse(i)?;
    let (i, _) = blank(i)?;

    let err = || {
        Err(nom::Err::Error(nom::error::Error::new(
            i,
            nom::error::ErrorKind::Tag,
        )))
    };

    // Check for keywords
    match ident {
        "let" | "if" | "else" | "while" | "break" | "continue" | "return" | "fn" | "null" => err(),
        #[cfg(feature = "bool_type")]
        "true" | "false" => err(),
        _ => Ok((i, ident.to_owned())),
    }
}

fn parse_null(i: &str) -> IResult<&str, Value> {
    value(Value::Null, ws(keyword("null"))).parse(i)
}

#[cfg(feature = "bool_type")]
fn parse_bool(i: &str) -> IResult<&str, Value> {
    alt((
        value(Value::Bool(true), ws(keyword("true"))),
        value(Value::Bool(false), ws(keyword("false"))),
    ))
    .parse(i)
}

#[cfg(feature = "f32_type")]
fn parse_f32(i: &str) -> IResult<&str, Value> {
    let (i, _) = blank(i)?;
    let (i, num_str) = recognize((opt(char('-')), digit1, char('.'), digit1)).parse(i)?;
    let (i, _) = blank(i)?;

    match num_str.parse::<f32>() {
        Ok(f) => Ok((i, Value::F32(f))),
        Err(_) => Err(nom::Err::Error(nom::error::Error::new(
            i,
            nom::error::ErrorKind::Verify,
        ))),
    }
}

#[cfg(feature = "i32_type")]
fn parse_i32(i: &str) -> IResult<&str, Value> {
    let (i, _) = blank(i)?;
    let (i, num_str) = recognize(pair(opt(char('-')), digit1)).parse(i)?;
    let (i, _) = blank(i)?;

    match num_str.parse::<i32>() {
        Ok(n) => Ok((i, Value::I32(n))),
        Err(_) => Err(nom::Err::Error(nom::error::Error::new(
            i,
            nom::error::ErrorKind::Verify,
        ))),
    }
}

#[cfg(feature = "string_type")]
fn parse_string(i: &str) -> IResult<&str, Value> {
    let (i, _) = blank.parse(i)?;
    let (i, _) = char('"').parse(i)?;
    let (i, content) = take_while1(|c| c != '"').parse(i)?;
    let (i, _) = char('"').parse(i)?;
    let (i, _) = blank.parse(i)?;

    Ok((i, Value::String(content.to_string())))
}

fn parse_literal(i: &str) -> IResult<&str, Value> {
    alt((
        parse_null,
        #[cfg(feature = "bool_type")]
        parse_bool,
        #[cfg(feature = "f32_type")]
        parse_f32,
        #[cfg(feature = "i32_type")]
        parse_i32,
        #[cfg(feature = "string_type")]
        parse_string,
    ))
    .parse(i)
}

// Postfix operators for precedence parser
#[derive(Clone)]
enum PostfixOp {
    Call(Vec<Expr>),
}

fn function_call(i: &str) -> IResult<&str, PostfixOp> {
    map(
        delimited(
            ws(char('(')),
            separated_list0(ws(char(',')), expression),
            ws(cut(char(')'))),
        ),
        PostfixOp::Call,
    )
    .parse(i)
}

fn parse_block(i: &str) -> IResult<&str, Block> {
    let (mut i, _) = ws(char('{')).parse(i)?;

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

    let (i, _) = ws(cut(char('}'))).parse(i)?;

    Ok((i, Block {
        statements,
        value: value.map(Box::new),
    }))
}

// If expression parser
#[cfg(feature = "if_expression")]
fn parse_if(i: &str) -> IResult<&str, Expr> {
    let (i, _) = ws(keyword("if")).parse(i)?;
    let (i, cond) = cut(expression).parse(i)?;
    let (i, then_branch) = cut(parse_block).parse(i)?;
    let (i, else_branch) = opt(preceded(ws(keyword("else")), cut(parse_block))).parse(i)?;
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
        #[cfg(feature = "if_expression")]
        parse_if,
        map(parse_block, Expr::Block),
        map(identifier, Expr::Variable),
        delimited(ws(char('(')), expression, ws(cut(char(')')))),
    ))
    .parse(i)
}

// Main expression parser using precedence
fn expression(i: &str) -> IResult<&str, Expr> {
    let _guard = depth_limiter::dive(i)?;

    precedence(
        // Prefix operators
        alt((
            #[cfg(feature = "bool_type")]
            unary_op(2, value(UnaryOp::Not, ws(tag("!")))),
            unary_op(2, value(UnaryOp::Neg, ws(tag("-")))),
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
                    value(BinaryOp::Mul, ws(tag("*"))),
                    value(BinaryOp::Div, ws(tag("/"))),
                    value(BinaryOp::Mod, ws(tag("%"))),
                )),
            ),
            // Level 4: Additive
            binary_op(
                4,
                Assoc::Left,
                alt((
                    value(BinaryOp::Add, ws(tag("+"))),
                    value(BinaryOp::Sub, ws(tag("-"))),
                )),
            ),
            // Level 5: Comparison
            #[cfg(feature = "bool_type")]
            binary_op(
                5,
                Assoc::Left,
                alt((
                    value(BinaryOp::Le, ws(tag("<="))),
                    value(BinaryOp::Ge, ws(tag(">="))),
                    value(BinaryOp::Lt, ws(tag("<"))),
                    value(BinaryOp::Gt, ws(tag(">"))),
                )),
            ),
            // Level 6: Equality
            #[cfg(feature = "bool_type")]
            binary_op(
                6,
                Assoc::Left,
                alt((
                    value(BinaryOp::Eq, ws(tag("=="))),
                    value(BinaryOp::Ne, ws(tag("!="))),
                )),
            ),
            // Level 7: Logical AND
            #[cfg(feature = "bool_type")]
            binary_op(7, Assoc::Left, value(BinaryOp::And, ws(tag("&&")))),
            // Level 8: Logical OR
            #[cfg(feature = "bool_type")]
            binary_op(8, Assoc::Left, value(BinaryOp::Or, ws(tag("||")))),
        )),
        primary_expr,
        |op: Operation<UnaryOp, PostfixOp, BinaryOp, Expr>| -> Result<Expr, ()> {
            use Operation::*;
            match op {
                Prefix(op, e) => Ok(Expr::Unary(op, Box::new(e))),
                Postfix(Expr::Variable(name), PostfixOp::Call(args)) => Ok(Expr::Call(name, args)),
                Postfix(_, PostfixOp::Call(_)) => Err(()),
                Binary(lhs, op, rhs) => Ok(Expr::Binary(op, Box::new(lhs), Box::new(rhs))),
            }
        },
    )(i)
}

fn parse_let(i: &str) -> IResult<&str, Stmt> {
    let (i, _) = ws(keyword("let")).parse(i)?;
    let (i, name) = cut(identifier).parse(i)?;
    let (i, _) = ws(cut(char('='))).parse(i)?;
    let (i, expr) = cut(expression).parse(i)?;
    let (i, _) = ws(cut(char(';'))).parse(i)?;

    Ok((i, Stmt::Let(name, expr)))
}

fn parse_assign(i: &str) -> IResult<&str, Stmt> {
    let (i, name) = identifier.parse(i)?;
    let (i, _) = ws(char('=')).parse(i)?;
    let (i, expr) = cut(expression).parse(i)?;
    let (i, _) = ws(cut(char(';'))).parse(i)?;

    Ok((i, Stmt::Assign(name, expr)))
}

#[cfg(feature = "while_loop")]
fn parse_while(i: &str) -> IResult<&str, Stmt> {
    let (i, _) = ws(keyword("while")).parse(i)?;
    let (i, cond) = cut(expression).parse(i)?;
    let (i, _) = ws(cut(char('{'))).parse(i)?;
    let (i, body) = many0(parse_stmt).parse(i)?;
    let (i, _) = ws(cut(char('}'))).parse(i)?;
    Ok((i, Stmt::While { cond, body }))
}

#[cfg(feature = "while_loop")]
fn parse_break(i: &str) -> IResult<&str, Stmt> {
    let (i, _) = ws(keyword("break")).parse(i)?;
    let (i, _) = ws(cut(char(';'))).parse(i)?;
    Ok((i, Stmt::Break))
}

#[cfg(feature = "while_loop")]
fn parse_continue(i: &str) -> IResult<&str, Stmt> {
    let (i, _) = ws(keyword("continue")).parse(i)?;
    let (i, _) = ws(cut(char(';'))).parse(i)?;
    Ok((i, Stmt::Continue))
}

fn parse_return(i: &str) -> IResult<&str, Stmt> {
    let (i, _) = ws(keyword("return")).parse(i)?;
    let (i, expr) = opt(expression).parse(i)?;
    let (i, _) = ws(cut(char(';'))).parse(i)?;
    Ok((i, Stmt::Return(expr)))
}

#[cfg(feature = "while_loop")]
fn parse_expr_stmt(i: &str) -> IResult<&str, Stmt> {
    let (i, expr) = expression.parse(i)?;
    let (i, _) = ws(char(';')).parse(i)?;
    Ok((i, Stmt::Expr(expr)))
}

#[cfg(feature = "while_loop")]
fn parse_stmt(i: &str) -> IResult<&str, Stmt> {
    let _guard = depth_limiter::dive(i)?;

    alt((
        parse_let,
        #[cfg(feature = "while_loop")]
        parse_while,
        #[cfg(feature = "while_loop")]
        parse_break,
        #[cfg(feature = "while_loop")]
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
        let (i, semi) = opt(ws(char(';'))).parse(i)?;
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
        #[cfg(feature = "while_loop")]
        parse_while,
        #[cfg(feature = "while_loop")]
        parse_break,
        #[cfg(feature = "while_loop")]
        parse_continue,
        parse_return,
        parse_assign,
    ))
    .parse(i)?;

    Ok((i, StmtOrExpr::Stmt(stmt)))
}

fn parse_function(i: &str) -> IResult<&str, Function> {
    let (i, _) = ws(keyword("fn")).parse(i)?;
    let (i, name) = cut(identifier).parse(i)?;
    let (i, _) = ws(cut(char('('))).parse(i)?;
    let (i, params) = separated_list0(ws(char(',')), identifier).parse(i)?;
    let (i, _) = ws(cut(char(')'))).parse(i)?;
    let (i, block) = parse_block(i)?;

    let mut body = block.statements;
    if let Some(value) = block.value {
        body.push(Stmt::Return(Some(*value)));
    }

    Ok((i, Function { name, params, body }))
}

pub fn parse_program(i: &str) -> IResult<&str, Program> {
    let (i, functions) = many0(parse_function).parse(i)?;
    let (i, _) = blank(i)?;
    Ok((i, Program { functions }))
}
