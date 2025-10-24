use crate::{ast::*, BinaryOperator, UnaryOperator, Value};
#[allow(unused_imports)]
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while1},
    character::complete::{alpha1, alphanumeric1, anychar, char, digit1, multispace0},
    combinator::{cut, eof, map, not, opt, peek, recognize, value, verify},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, terminated},
    IResult, Parser as NomParser,
};
use nom_language::precedence::{binary_op, precedence, unary_op, Assoc, Operation};
use std::fmt::{self, Debug, Display, Write};
use std::ops::Range;
use thiserror::Error;

/// Parses source code into an abstract syntax tree.
#[non_exhaustive]
pub struct Parser {
    max_depth: u32,
}

impl Debug for Parser {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Parser").finish_non_exhaustive()
    }
}

/// A parse error at the given location in the source code.
#[derive(Clone, Debug, Error)]
#[error("parse error from {} to {}: {reason}", span.start, span.end)]
pub struct ParseError {
    pub span: Range<usize>,
    pub reason: ParseErrorReason,
}

/// The reason for a [`ParseError`] at a given location.
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

    pub fn reset(max: u32) {
        DEPTH.with(|depth| {
            depth.store(max, Ordering::Relaxed);
        });
    }

    pub fn dive(i: &str) -> Result<DepthGuard, nom::Err<nom::error::Error<&str>>> {
        DEPTH.with(|depth| {
            let depth = depth.fetch_sub(1, Ordering::Relaxed);
            if depth == 0 {
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
                depth.fetch_add(1, Ordering::Relaxed);
            })
        }
    }
}

impl Parser {
    pub fn new() -> Self {
        Self { max_depth: 50 }
    }

    pub fn with_max_depth(mut self, max: u32) -> Self {
        self.max_depth = max;
        self
    }

    pub fn parse<'src>(&self, src: &'src str) -> Result<Program, Vec<ParseError>> {
        depth_limiter::reset(self.max_depth);

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
fn comment(i: &str) -> IResult<&str, &str> {
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
fn comments(i: &str) -> IResult<&str, &str> {
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

fn parse_identifier(i: &str) -> IResult<&str, String> {
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
    Call(Vec<Expression>),
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

fn parse_block(i: &str) -> IResult<&str, BlockExpression> {
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

    Ok((
        i,
        BlockExpression {
            statements,
            value: value.map(Box::new),
        },
    ))
}

// If expression parser
#[cfg(feature = "if_expression")]
fn parse_if(i: &str) -> IResult<&str, Expression> {
    let (i, _) = ws(keyword("if")).parse(i)?;
    let (i, cond) = cut(expression).parse(i)?;
    let (i, then_branch) = cut(parse_block).parse(i)?;
    let (i, else_branch) = opt(preceded(ws(keyword("else")), cut(parse_block))).parse(i)?;
    Ok((
        i,
        Expression::If(IfExpression {
            condition: Box::new(cond),
            then_branch,
            else_branch,
        }),
    ))
}

// Primary expression (atom)
fn primary_expr(i: &str) -> IResult<&str, Expression> {
    alt((
        map(parse_literal, Expression::Literal),
        #[cfg(feature = "if_expression")]
        parse_if,
        map(parse_block, Expression::Block),
        map(parse_identifier, Expression::Variable),
        delimited(ws(char('(')), expression, ws(cut(char(')')))),
    ))
    .parse(i)
}

// Main expression parser using precedence
fn expression(i: &str) -> IResult<&str, Expression> {
    let _guard = depth_limiter::dive(i)?;

    precedence(
        // Prefix operators
        alt((
            #[cfg(feature = "bool_type")]
            unary_op(2, value(UnaryOperator::Not, ws(tag("!")))),
            unary_op(2, value(UnaryOperator::Negate, ws(tag("-")))),
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
                    value(BinaryOperator::Multiply, ws(tag("*"))),
                    value(BinaryOperator::Divide, ws(tag("/"))),
                    value(BinaryOperator::Modulo, ws(tag("%"))),
                )),
            ),
            // Level 4: Additive
            binary_op(
                4,
                Assoc::Left,
                alt((
                    value(BinaryOperator::Add, ws(tag("+"))),
                    value(BinaryOperator::Subtract, ws(tag("-"))),
                )),
            ),
            // Level 5: Comparison
            #[cfg(feature = "bool_type")]
            binary_op(
                5,
                Assoc::Left,
                alt((
                    value(BinaryOperator::LessThanOrEqual, ws(tag("<="))),
                    value(BinaryOperator::GreaterThanOrEqual, ws(tag(">="))),
                    value(BinaryOperator::LessThan, ws(tag("<"))),
                    value(BinaryOperator::GreaterThan, ws(tag(">"))),
                )),
            ),
            // Level 6: Equality
            #[cfg(feature = "bool_type")]
            binary_op(
                6,
                Assoc::Left,
                alt((
                    value(BinaryOperator::Equal, ws(tag("=="))),
                    value(BinaryOperator::NotEqual, ws(tag("!="))),
                )),
            ),
            // Level 7: Logical AND
            #[cfg(feature = "bool_type")]
            binary_op(7, Assoc::Left, value(BinaryOperator::And, ws(tag("&&")))),
            // Level 8: Logical OR
            #[cfg(feature = "bool_type")]
            binary_op(8, Assoc::Left, value(BinaryOperator::Or, ws(tag("||")))),
        )),
        primary_expr,
        |op: Operation<UnaryOperator, PostfixOp, BinaryOperator, Expression>| -> Result<Expression, ()> {
            use Operation::*;
            match op {
                Prefix(operator, operand) => Ok(Expression::Unary(UnaryExpression { operator, operand: Box::new(operand) })),
                Postfix(Expression::Variable(name), PostfixOp::Call(arguments)) => Ok(Expression::Call(CallExpression{identifier: name, arguments})),
                Postfix(_, PostfixOp::Call(_)) => Err(()),
                Binary(left, operator, right) => Ok(Expression::Binary(BinaryExpression{operator, left: Box::new(left), right: Box::new(right)})),
            }
        },
    )(i)
}

fn parse_let(i: &str) -> IResult<&str, Statement> {
    let (i, _) = ws(keyword("let")).parse(i)?;
    let (i, identifier) = cut(parse_identifier).parse(i)?;
    let (i, _) = ws(cut(char('='))).parse(i)?;
    let (i, expression) = cut(expression).parse(i)?;
    let (i, _) = ws(cut(char(';'))).parse(i)?;
    Ok((
        i,
        Statement::Let(LetStatement {
            identifier,
            expression,
        }),
    ))
}

fn parse_assign(i: &str) -> IResult<&str, Statement> {
    let (i, identifier) = parse_identifier.parse(i)?;
    let (i, _) = ws(char('=')).parse(i)?;
    let (i, expression) = cut(expression).parse(i)?;
    let (i, _) = ws(cut(char(';'))).parse(i)?;
    Ok((
        i,
        Statement::Assign(AssignStatement {
            identifier,
            expression,
        }),
    ))
}

#[cfg(feature = "while_loop")]
fn parse_while(i: &str) -> IResult<&str, Statement> {
    let (i, _) = ws(keyword("while")).parse(i)?;
    let (i, condition) = cut(expression).parse(i)?;
    let (i, _) = ws(cut(char('{'))).parse(i)?;

    // While can be nested without expressions.
    let _guard = depth_limiter::dive(i)?;

    let (i, body) = many0(parse_stmt).parse(i)?;
    let (i, _) = ws(cut(char('}'))).parse(i)?;
    Ok((i, Statement::While(WhileLoop { condition, body })))
}

#[cfg(feature = "while_loop")]
fn parse_break(i: &str) -> IResult<&str, Statement> {
    let (i, _) = ws(keyword("break")).parse(i)?;
    let (i, _) = ws(cut(char(';'))).parse(i)?;
    Ok((i, Statement::Break))
}

#[cfg(feature = "while_loop")]
fn parse_continue(i: &str) -> IResult<&str, Statement> {
    let (i, _) = ws(keyword("continue")).parse(i)?;
    let (i, _) = ws(cut(char(';'))).parse(i)?;
    Ok((i, Statement::Continue))
}

fn parse_return(i: &str) -> IResult<&str, Statement> {
    let (i, _) = ws(keyword("return")).parse(i)?;
    let (i, expr) = opt(expression).parse(i)?;
    let (i, _) = ws(cut(char(';'))).parse(i)?;
    Ok((i, Statement::Return(expr)))
}

#[cfg(feature = "while_loop")]
fn parse_expr_stmt(i: &str) -> IResult<&str, Statement> {
    let (i, expr) = expression.parse(i)?;
    let (i, _) = ws(char(';')).parse(i)?;
    Ok((i, Statement::Expression(expr)))
}

#[cfg(feature = "while_loop")]
fn parse_stmt(i: &str) -> IResult<&str, Statement> {
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
    Stmt(Statement),
    Expr(Expression),
}

fn parse_stmt_or_expr(i: &str) -> IResult<&str, StmtOrExpr> {
    let (i, expr) = opt(expression).parse(i)?;

    if let Some(expr) = expr {
        let (i, semi) = opt(ws(char(';'))).parse(i)?;
        return Ok((
            i,
            if semi.is_some() {
                StmtOrExpr::Stmt(Statement::Expression(expr))
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
    let (i, identifier) = cut(parse_identifier).parse(i)?;
    let (i, _) = ws(cut(char('('))).parse(i)?;
    let (i, arguments) = separated_list0(ws(char(',')), parse_identifier).parse(i)?;
    let (i, _) = ws(cut(char(')'))).parse(i)?;
    let (i, body) = parse_block(i)?;
    Ok((
        i,
        Function {
            identifier,
            arguments,
            body,
        },
    ))
}

fn parse_program(i: &str) -> IResult<&str, Program> {
    let (i, functions) = many0(parse_function).parse(i)?;
    let (i, _) = blank(i)?;
    Ok((i, Program { functions }))
}
