use chumsky::prelude::*;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Bool,
    I32,
    F32,
    String,
}

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum Value {
    Unit,
    Bool(bool),
    I32(i32),
    F32(f32),
    String(String),
}

impl Value {
    pub fn type_of(&self) -> Type {
        match self {
            Value::Unit => Type::Unit,
            Value::Bool(_) => Type::Bool,
            Value::I32(_) => Type::I32,
            Value::F32(_) => Type::F32,
            Value::String(_) => Type::String,
        }
    }

    fn as_bool(&self) -> Result<bool, String> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(format!("Expected bool, got {:?}", self.type_of())),
        }
    }

    #[allow(unused)]
    fn as_i32(&self) -> Result<i32, String> {
        match self {
            Value::I32(i) => Ok(*i),
            _ => Err(format!("Expected i32, got {:?}", self.type_of())),
        }
    }

    #[allow(unused)]
    fn as_f32(&self) -> Result<f32, String> {
        match self {
            Value::F32(f) => Ok(*f),
            _ => Err(format!("Expected f32, got {:?}", self.type_of())),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::I32(i) => write!(f, "{}", i),
            Value::F32(fl) => write!(f, "{}", fl),
            Value::String(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
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

#[derive(Debug, Clone, PartialEq)]
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
pub struct Block {
    statements: Vec<Stmt>,
    value: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
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
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub functions: Vec<Function>,
}

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

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    LoadConst(Value),
    LoadVar(u8),
    StoreVar(u8),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    Jump(usize),
    JumpIfFalse(usize),
    Call(String, usize),
    Return,
    Pop,
}

pub struct BytecodeGenerator {
    instructions: Vec<Instruction>,
    variable_stack: Vec<Vec<String>>,
    /// (start, breaks, continues)
    loop_stack: Vec<(usize, Vec<usize>, Vec<usize>)>,
}

impl BytecodeGenerator {
    fn new() -> Self {
        Self {
            instructions: Vec::new(),
            variable_stack: Vec::new(),
            loop_stack: Vec::new(),
        }
    }

    fn emit(&mut self, inst: Instruction) -> usize {
        let addr = self.instructions.len();
        self.instructions.push(inst);
        addr
    }

    fn current_addr(&self) -> usize {
        self.instructions.len()
    }

    fn patch_jump(&mut self, addr: usize, target: usize) {
        match &mut self.instructions[addr] {
            Instruction::Jump(t) | Instruction::JumpIfFalse(t) => *t = target,
            _ => panic!("Attempted to patch non-jump instruction"),
        }
    }

    fn generate_block(&mut self, block: &Block) {
        for stmt in &block.statements {
            self.generate_stmt(stmt);
        }
        if let Some(value) = &block.value {
            self.generate_expr(value);
        } else {
            self.emit(Instruction::LoadConst(Value::Unit));
        }
    }

    fn generate_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal(val) => {
                self.emit(Instruction::LoadConst(val.clone()));
            }
            Expr::Variable(name) => {
                if let Some(last) = self.variable_stack.last_mut() {
                    // TODO.
                    let index = last.iter().rposition(|v| v == name).unwrap_or(0);
                    self.emit(Instruction::LoadVar(index as u8));
                }
            }
            Expr::Unary(op, operand) => {
                self.generate_expr(operand);
                self.emit(Instruction::UnaryOp(op.clone()));
            }
            Expr::Binary(BinaryOp::And, left, right) => {
                self.generate_expr(left);
                let jump_addr = self.emit(Instruction::JumpIfFalse(0));
                self.emit(Instruction::Pop);
                self.generate_expr(right);
                let end_addr = self.current_addr();
                self.patch_jump(jump_addr, end_addr);
            }
            Expr::Binary(BinaryOp::Or, left, right) => {
                self.generate_expr(left);
                let jump_addr = self.emit(Instruction::JumpIfFalse(0));
                let jump_end = self.emit(Instruction::Jump(0));
                self.current_addr();
                let else_addr = self.emit(Instruction::Pop);
                self.patch_jump(jump_addr, else_addr);
                self.generate_expr(right);
                let end_addr = self.current_addr();
                self.patch_jump(jump_end, end_addr);
            }
            Expr::Binary(op, left, right) => {
                self.generate_expr(left);
                self.generate_expr(right);
                self.emit(Instruction::BinaryOp(op.clone()));
            }
            Expr::Call(name, args) => {
                for arg in args {
                    self.generate_expr(arg);
                }
                self.emit(Instruction::Call(name.clone(), args.len()));
            }
            Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.generate_expr(cond);
                let jump_else = self.emit(Instruction::JumpIfFalse(0));
                self.emit(Instruction::Pop);

                self.generate_block(then_branch);

                let jump_end = self.emit(Instruction::Jump(0));
                let else_addr = self.current_addr();
                self.patch_jump(jump_else, else_addr);
                self.emit(Instruction::Pop);

                if let Some(else_stmts) = else_branch {
                    self.generate_block(else_stmts);
                } else {
                    self.emit(Instruction::LoadConst(Value::Unit));
                }

                let end_addr = self.current_addr();
                self.patch_jump(jump_end, end_addr);
            }
            Expr::Block(block) => {
                self.generate_block(block);
            }
        }
    }

    fn generate_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let(name, expr) => {
                if let Some(last) = self.variable_stack.last_mut() {
                    let index = last.len();
                    // TODO.
                    last.push(name.clone());

                    self.generate_expr(expr);
                    self.emit(Instruction::StoreVar(index as u8));
                }
            }
            Stmt::Assign(name, expr) => {
                if let Some(last) = self.variable_stack.last_mut() {
                    // TODO.
                    let index = last.iter().rposition(|v| v == name).unwrap_or(0);
                    self.generate_expr(expr);
                    self.emit(Instruction::StoreVar(index as u8));
                }
            }
            Stmt::Expr(expr) => {
                self.generate_expr(expr);
                self.emit(Instruction::Pop);
            }
            Stmt::While { cond, body } => {
                let loop_start = self.current_addr();
                self.loop_stack.push((loop_start, Vec::new(), Vec::new()));

                self.generate_expr(cond);
                let jump_end = self.emit(Instruction::JumpIfFalse(0));
                self.emit(Instruction::Pop);

                for stmt in body {
                    self.generate_stmt(stmt);
                }

                self.emit(Instruction::Jump(loop_start));
                let end_addr = self.current_addr();
                self.patch_jump(jump_end, end_addr);
                self.emit(Instruction::Pop);
                let break_target_addr = self.current_addr();

                let (_, breaks, continues) = self.loop_stack.pop().unwrap();
                for break_addr in breaks {
                    self.patch_jump(break_addr, break_target_addr);
                }
                for continue_addr in continues {
                    self.patch_jump(continue_addr, loop_start);
                }
            }
            Stmt::Break => {
                let addr = self.emit(Instruction::Jump(0));
                if let Some((_, breaks, _)) = self.loop_stack.last_mut() {
                    breaks.push(addr);
                }
            }
            Stmt::Continue => {
                let addr = self.emit(Instruction::Jump(0));
                if let Some((_, _, continues)) = self.loop_stack.last_mut() {
                    continues.push(addr);
                }
            }
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    self.generate_expr(expr);
                } else {
                    self.emit(Instruction::LoadConst(Value::Unit));
                }
                self.emit(Instruction::Return);
            }
        }
    }

    pub fn generate(func: &Function) -> Vec<Instruction> {
        let mut generator = BytecodeGenerator::new();
        generator.variable_stack.push(Vec::new());
        for arg in &func.params {
            generator
                .variable_stack
                .last_mut()
                .unwrap()
                .push(arg.clone());
        }
        for stmt in &func.body {
            generator.generate_stmt(stmt);
        }
        if !matches!(generator.instructions.last(), Some(Instruction::Return)) {
            generator.emit(Instruction::LoadConst(Value::Unit));
            generator.emit(Instruction::Return);
        }
        generator.instructions
    }
}

pub type HostFunction = Box<dyn Fn(&[Value]) -> Result<Value, String>>;

pub struct Vm {
    functions: HashMap<String, Vec<Instruction>>,
    host_functions: HashMap<String, HostFunction>,
}

impl Vm {
    pub fn new() -> Self {
        let mut vm = Self {
            functions: HashMap::new(),
            host_functions: HashMap::new(),
        };

        vm.register_host_function(
            "bool",
            Box::new(|args| {
                if args.len() != 1 {
                    return Err("bool() takes exactly 1 argument".to_string());
                }
                match &args[0] {
                    Value::Bool(b) => Ok(Value::Bool(*b)),
                    Value::I32(i) => Ok(Value::Bool(*i != 0)),
                    Value::F32(f) => Ok(Value::Bool(*f != 0.0)),
                    Value::String(s) => Ok(Value::Bool(!s.is_empty())),
                    Value::Unit => Ok(Value::Bool(false)),
                }
            }),
        );

        vm.register_host_function(
            "i32",
            Box::new(|args| {
                if args.len() != 1 {
                    return Err("i32() takes exactly 1 argument".to_string());
                }
                match &args[0] {
                    Value::I32(i) => Ok(Value::I32(*i)),
                    Value::Bool(b) => Ok(Value::I32(if *b { 1 } else { 0 })),
                    Value::F32(f) => Ok(Value::I32(*f as i32)),
                    Value::String(s) => s
                        .parse::<i32>()
                        .map(Value::I32)
                        .map_err(|_| "Failed to parse string as i32".to_string()),
                    _ => Err("Cannot cast to i32".to_string()),
                }
            }),
        );

        vm.register_host_function(
            "f32",
            Box::new(|args| {
                if args.len() != 1 {
                    return Err("f32() takes exactly 1 argument".to_string());
                }
                match &args[0] {
                    Value::F32(f) => Ok(Value::F32(*f)),
                    Value::I32(i) => Ok(Value::F32(*i as f32)),
                    Value::Bool(b) => Ok(Value::F32(if *b { 1.0 } else { 0.0 })),
                    Value::String(s) => s
                        .parse::<f32>()
                        .map(Value::F32)
                        .map_err(|_| "Failed to parse string as f32".to_string()),
                    _ => Err("Cannot cast to f32".to_string()),
                }
            }),
        );

        vm.register_host_function(
            "string",
            Box::new(|args| {
                if args.len() != 1 {
                    return Err("string() takes exactly 1 argument".to_string());
                }
                Ok(Value::String(args[0].to_string()))
            }),
        );

        vm
    }

    pub fn load_program(&mut self, program: &Program) {
        for func in &program.functions {
            let bytecode = BytecodeGenerator::generate(func);
            self.functions.insert(func.name.clone(), bytecode);
        }
    }

    pub fn register_host_function(&mut self, name: &str, func: HostFunction) {
        self.host_functions.insert(name.to_string(), func);
    }

    pub fn execute(&self, func_name: &str, args: Vec<Value>) -> Result<Value, String> {
        let mut stack: Vec<Value> = Vec::new();
        let mut variables = args;
        let mut pc = 0;

        if let Some(instructions) = self.functions.get(func_name) {
            while pc < instructions.len() {
                match &instructions[pc] {
                    Instruction::LoadConst(val) => {
                        stack.push(val.clone());
                        pc += 1;
                    }
                    Instruction::LoadVar(name) => {
                        let val = variables
                            .get(*name as usize)
                            .ok_or_else(|| format!("Undefined load variable: {}", name))?;
                        stack.push(val.clone());
                        pc += 1;
                    }
                    Instruction::StoreVar(name) => {
                        let val = stack.pop().ok_or("Stack underflow")?;
                        while variables.len() < *name as usize + 1 {
                            variables.push(Value::Unit);
                        }
                        *variables
                            .get_mut(*name as usize)
                            .ok_or_else(|| format!("Undefined store variable: {}", name))? = val;
                        pc += 1;
                    }
                    Instruction::UnaryOp(op) => {
                        let operand = stack.pop().ok_or("Stack underflow")?;
                        let result = match op {
                            UnaryOp::Not => {
                                let b = operand.as_bool()?;
                                Value::Bool(!b)
                            }
                            UnaryOp::Neg => match operand {
                                Value::I32(i) => {
                                    Value::I32(i.checked_neg().ok_or("Integer overflow")?)
                                }
                                Value::F32(f) => Value::F32(-f),
                                _ => return Err("Invalid operand for negation".to_string()),
                            },
                        };
                        stack.push(result);
                        pc += 1;
                    }
                    Instruction::BinaryOp(op) => {
                        let right = stack.pop().ok_or("Stack underflow")?;
                        let left = stack.pop().ok_or("Stack underflow")?;
                        let result = match op {
                            BinaryOp::Add => match (left, right) {
                                (Value::I32(l), Value::I32(r)) => {
                                    Value::I32(l.checked_add(r).ok_or("Integer overflow")?)
                                }
                                (Value::F32(l), Value::F32(r)) => Value::F32(l + r),
                                (Value::String(l), Value::String(r)) => {
                                    Value::String(format!("{}{}", l, r))
                                }
                                _ => return Err("Type mismatch in addition".to_string()),
                            },
                            BinaryOp::Sub => match (left, right) {
                                (Value::I32(l), Value::I32(r)) => {
                                    Value::I32(l.checked_sub(r).ok_or("Integer overflow")?)
                                }
                                (Value::F32(l), Value::F32(r)) => Value::F32(l - r),
                                _ => return Err("Type mismatch in subtraction".to_string()),
                            },
                            BinaryOp::Mul => match (left, right) {
                                (Value::I32(l), Value::I32(r)) => {
                                    Value::I32(l.checked_mul(r).ok_or("Integer overflow")?)
                                }
                                (Value::F32(l), Value::F32(r)) => Value::F32(l * r),
                                _ => return Err("Type mismatch in multiplication".to_string()),
                            },
                            BinaryOp::Div => match (left, right) {
                                (Value::I32(l), Value::I32(r)) => {
                                    if r == 0 {
                                        return Err("Division by zero".to_string());
                                    }
                                    Value::I32(l.checked_div(r).ok_or("Integer overflow")?)
                                }
                                (Value::F32(l), Value::F32(r)) => Value::F32(l / r),
                                _ => return Err("Type mismatch in division".to_string()),
                            },
                            BinaryOp::Mod => match (left, right) {
                                (Value::I32(l), Value::I32(r)) => {
                                    if r == 0 {
                                        return Err("Modulo by zero".to_string());
                                    }
                                    Value::I32(l % r)
                                }
                                _ => return Err("Type mismatch in modulo".to_string()),
                            },
                            BinaryOp::Eq => Value::Bool(left == right),
                            BinaryOp::Ne => Value::Bool(left != right),
                            BinaryOp::Lt => match (left, right) {
                                (Value::I32(l), Value::I32(r)) => Value::Bool(l < r),
                                (Value::F32(l), Value::F32(r)) => Value::Bool(l < r),
                                _ => return Err("Type mismatch in comparison".to_string()),
                            },
                            BinaryOp::Le => match (left, right) {
                                (Value::I32(l), Value::I32(r)) => Value::Bool(l <= r),
                                (Value::F32(l), Value::F32(r)) => Value::Bool(l <= r),
                                _ => return Err("Type mismatch in comparison".to_string()),
                            },
                            BinaryOp::Gt => match (left, right) {
                                (Value::I32(l), Value::I32(r)) => Value::Bool(l > r),
                                (Value::F32(l), Value::F32(r)) => Value::Bool(l > r),
                                _ => return Err("Type mismatch in comparison".to_string()),
                            },
                            BinaryOp::Ge => match (left, right) {
                                (Value::I32(l), Value::I32(r)) => Value::Bool(l >= r),
                                (Value::F32(l), Value::F32(r)) => Value::Bool(l >= r),
                                _ => return Err("Type mismatch in comparison".to_string()),
                            },
                            BinaryOp::And | BinaryOp::Or => {
                                return Err("And/Or should have been desugared".to_string());
                            }
                        };
                        stack.push(result);
                        pc += 1;
                    }
                    Instruction::Jump(target) => {
                        pc = *target;
                    }
                    Instruction::JumpIfFalse(target) => {
                        let cond = stack.last().ok_or("Stack underflow")?;
                        let cond_val = cond.as_bool()?;
                        if !cond_val {
                            pc = *target;
                        } else {
                            pc += 1;
                        }
                    }
                    Instruction::Call(name, arg_count) => {
                        let mut call_args = Vec::new();
                        for _ in 0..*arg_count {
                            call_args.push(stack.pop().ok_or("Stack underflow")?);
                        }
                        call_args.reverse();

                        let result = if let Some(host_fn) = self.host_functions.get(name) {
                            host_fn(&call_args)?
                        } else if self.functions.contains_key(name) {
                            // TODO: don't use host stack(?)
                            self.execute(name, call_args)?
                        } else {
                            return Err(format!("Undefined function: {}", name));
                        };

                        stack.push(result);
                        pc += 1;
                    }
                    Instruction::Return => {
                        return stack
                            .pop()
                            .ok_or_else(|| "Stack underflow on return".to_string());
                    }
                    Instruction::Pop => {
                        stack.pop().ok_or("Stack underflow")?;
                        pc += 1;
                    }
                }
            }

            Ok(Value::Unit)
        } else {
            Err(format!("Function not found: {}", func_name))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple() {
        let source = r#"
            fn hello(a) {
                return f32(a);
            }
        "#;

        let ast = parser().parse(source).unwrap();
        let mut vm = Vm::new();
        vm.load_program(&ast);

        let result = vm.execute("hello", vec![Value::I32(5)]).unwrap();
        assert_eq!(result, Value::F32(5.0));
    }

    #[test]
    fn test_simple_math() {
        let source = r#"
            fn add(a, b) {
                return a + b;
            }
        "#;

        let ast = parser().parse(source).unwrap();
        let mut vm = Vm::new();
        vm.load_program(&ast);

        let result = vm
            .execute("add", vec![Value::I32(5), Value::I32(3)])
            .unwrap();
        assert_eq!(result, Value::I32(8));
    }

    #[test]
    fn test_if_expression() {
        let source = r#"
            fn max(a, b) {
                if (a > b) {
                    return a;
                } else {
                    return b;
                }
            }
        "#;

        let ast = parser().parse(source).unwrap();
        let mut vm = Vm::new();
        vm.load_program(&ast);

        let result = vm
            .execute("max", vec![Value::I32(10), Value::I32(5)])
            .unwrap();
        assert_eq!(result, Value::I32(10));
    }

    #[test]
    fn test_while_loop() {
        let source = r#"
            fn sum_to_n(n) {
                let i = 0;
                let sum = 0;
                while (i < n) {
                    i = i + 1;
                    sum = sum + i;
                }
                return sum;
            }
        "#;

        let ast = parser().parse(source).unwrap();
        let mut vm = Vm::new();
        vm.load_program(&ast);

        let result = vm.execute("sum_to_n", vec![Value::I32(5)]).unwrap();
        assert_eq!(result, Value::I32(15));
    }
}
