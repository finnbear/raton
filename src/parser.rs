use crate::Value;
use crate::ast::*;
use chumsky::prelude::*;

pub fn parser<'src>() -> impl Parser<'src, &'src str, Program, extra::Err<Rich<'src, char>>> {
    let ident = text::ident().padded().boxed();

    /*
        let type_parser = choice((
            just("bool").to(Type::Bool),
            just("i32").to(Type::I32),
            just("f32").to(Type::F32),
            just("string").to(Type::String),
            just("()").to(Type::Unit),
        ))
        .padded().boxed();
    */

    let value = choice((
        just("true").to(Value::Bool(true)),
        just("false").to(Value::Bool(false)),
        text::int(10)
            .map(|s: &str| Value::I32(s.parse().unwrap()))
            .labelled("integer"),
        text::int(10)
            .then_ignore(just('.'))
            .then(text::digits(10).collect::<String>())
            .map(|(i, d)| Value::F32(format!("{i}.{d}").parse().unwrap()))
            .labelled("float"),
        just('"')
            .ignore_then(any().filter(|c| *c != '"').repeated().collect::<String>())
            .then_ignore(just('"'))
            .map(Value::String)
            .labelled("string"),
        just("()").to(Value::Unit),
    ))
    .padded()
    .boxed();

    let mut stmt = Recursive::declare();
    let mut expr = Recursive::declare();

    let bracketed_statements = stmt
        .clone()
        .repeated()
        .collect::<Vec<_>>()
        .delimited_by(just('{').padded(), just('}').padded());

    let block = stmt
        .clone()
        .repeated()
        .collect::<Vec<_>>()
        .then(expr.clone().map(Box::new).or_not())
        .delimited_by(just('{').padded(), just('}').padded())
        .map(|(statements, value)| Block { statements, value })
        .padded();

    stmt.define({
        let let_stmt = just("let")
            .padded()
            .ignore_then(ident.clone())
            .then_ignore(just('=').padded())
            .then(expr.clone())
            .then_ignore(just(';').padded())
            .map(|(name, expr): (&str, Expr)| Stmt::Let(name.to_owned(), expr));

        let assign_stmt = ident
            .clone()
            .then_ignore(just('=').padded())
            .then(expr.clone())
            .then_ignore(just(';').padded())
            .map(|(name, expr): (&str, _)| Stmt::Assign(name.to_owned(), expr));

        let while_stmt = just("while")
            .padded()
            .ignore_then(expr.clone())
            .then(bracketed_statements.clone())
            .map(|(cond, body)| Stmt::While { cond, body });

        let break_stmt = just("break")
            .padded()
            .then_ignore(just(';').padded())
            .to(Stmt::Break);

        let continue_stmt = just("continue")
            .padded()
            .then_ignore(just(';').padded())
            .to(Stmt::Continue);

        let return_stmt = just("return")
            .padded()
            .ignore_then(expr.clone().or_not())
            .then_ignore(just(';').padded())
            .map(Stmt::Return);

        let expr_stmt = expr.clone().then_ignore(just(';').padded()).map(Stmt::Expr);

        choice((
            let_stmt,
            while_stmt,
            break_stmt,
            continue_stmt,
            return_stmt,
            assign_stmt,
            expr_stmt,
        ))
    });

    expr.define({
        let atom = choice((
            value.map(Expr::Literal),
            ident.clone().map(|s: &str| Expr::Variable(s.to_owned())),
            expr.clone()
                .delimited_by(just('(').padded(), just(')').padded()),
        ))
        .padded();

        let call_or_atom = ident
            .clone()
            .then(
                expr.clone()
                    .separated_by(just(',').padded())
                    .collect::<Vec<_>>()
                    .delimited_by(just('(').padded(), just(')').padded()),
            )
            .map(|(name, args): (&str, _)| Expr::Call(name.to_owned(), args))
            .or(atom.clone());

        let unary = choice((just('!').to(UnaryOp::Not), just('-').to(UnaryOp::Neg)))
            .padded()
            .then(call_or_atom.clone())
            .map(|(op, expr)| Expr::Unary(op, Box::new(expr)))
            .padded();

        let binary = call_or_atom
            .clone()
            .then(
                choice((
                    just('+').to(BinaryOp::Add),
                    just('-').to(BinaryOp::Sub),
                    just('*').to(BinaryOp::Mul),
                    just('/').to(BinaryOp::Div),
                    just('%').to(BinaryOp::Mod),
                    just("==").to(BinaryOp::Eq),
                    just("!=").to(BinaryOp::Ne),
                    just("<=").to(BinaryOp::Le),
                    just('<').to(BinaryOp::Lt),
                    just(">=").to(BinaryOp::Ge),
                    just('>').to(BinaryOp::Gt),
                    just("&&").to(BinaryOp::And),
                    just("||").to(BinaryOp::Or),
                ))
                .padded()
                .then(call_or_atom.clone()),
            )
            .map(|(lhs, (op, rhs))| Expr::Binary(op, Box::new(lhs), Box::new(rhs)));

        let if_expr = just("if")
            .padded()
            .ignore_then(expr.clone())
            .then(block.clone())
            .then(
                just("else")
                    .padded()
                    .ignore_then(
                        just("if")
                            .padded()
                            .ignore_then(expr.clone())
                            .then(block.clone())
                            .then(just("else").padded().ignore_then(block.clone()).or_not())
                            .map(|((cond, then_branch), else_branch)| Block {
                                statements: vec![],
                                value: Some(Box::new(Expr::If {
                                    cond: Box::new(cond),
                                    then_branch,
                                    else_branch,
                                })),
                            })
                            .or(block.clone()),
                    )
                    .or_not(),
            )
            .map(|((cond, then_branch), else_branch)| Expr::If {
                cond: Box::new(cond),
                then_branch,
                else_branch,
            });

        let block_expr = block.clone().map(Expr::Block);

        choice((unary, binary, if_expr, block_expr, call_or_atom))
    });

    let function = just("fn")
        .padded()
        .ignore_then(ident.clone())
        .then(
            ident
                .clone()
                .separated_by(just(',').padded())
                .collect::<Vec<_>>()
                .delimited_by(just('(').padded(), just(')').padded()),
        )
        .then(
            stmt.clone()
                .repeated()
                .collect::<Vec<_>>()
                .then(expr.or_not())
                .delimited_by(just('{').padded(), just('}').padded())
                .map(|(mut stmts, ret_expr)| {
                    if let Some(e) = ret_expr {
                        stmts.push(Stmt::Return(Some(e)));
                    }
                    stmts
                }),
        )
        .map(|((name, params), body): ((&str, Vec<&str>), _)| Function {
            name: name.to_owned(),
            params: params.into_iter().map(|s| s.to_owned()).collect(),
            body,
        })
        .boxed();

    function
        .padded()
        .repeated()
        .collect::<Vec<_>>()
        .then_ignore(end())
        .map(|functions| Program { functions })
}
