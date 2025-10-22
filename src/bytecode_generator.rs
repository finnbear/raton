use crate::{Value, ast::*, bytecode::*};
use thiserror::Error;

pub struct BytecodeGenerator {
    instructions: Vec<Instruction>,
    variable_stack: Vec<Vec<String>>,
    /// (start, breaks, continues)
    loop_stack: Vec<(u32, Vec<u32>, Vec<u32>)>,
    max_instructions: u32,
}

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("max instructions exceeded")]
    MaxInstructionsExceeded,
    #[error("internal compiler error")]
    Internal,
}

impl BytecodeGenerator {
    fn new() -> Self {
        Self {
            instructions: Vec::new(),
            variable_stack: Vec::new(),
            loop_stack: Vec::new(),
            max_instructions: u32::MAX - 1,
        }
    }

    pub fn with_max_instructions(mut self, max: u32) -> Self {
        self.max_instructions = max.min(u32::MAX - 1);
        self
    }

    fn emit(&mut self, inst: Instruction) -> Result<u32, CompileError> {
        let addr = self.instructions.len() as u32;
        if addr >= self.max_instructions {
            return Err(CompileError::MaxInstructionsExceeded);
        }
        self.instructions.push(inst);
        Ok(addr)
    }

    fn current_addr(&self) -> u32 {
        self.instructions.len() as u32
    }

    fn patch_jump(&mut self, addr: u32, target: u32) -> Result<(), CompileError> {
        match &mut self.instructions[addr as usize] {
            Instruction::Jump(t) | Instruction::JumpIfFalse(t) => {
                *t = target;
                Ok(())
            }
            _ => {
                debug_assert!(false);
                Err(CompileError::Internal)
            }
        }
    }

    fn generate_block(&mut self, block: &Block) -> Result<(), CompileError> {
        for stmt in &block.statements {
            self.generate_stmt(stmt)?;
        }
        if let Some(value) = &block.value {
            self.generate_expr(value)?;
        } else {
            self.emit(Instruction::LoadConst(Value::Unit))?;
        }
        Ok(())
    }

    fn generate_expr(&mut self, expr: &Expr) -> Result<(), CompileError> {
        match expr {
            Expr::Literal(val) => {
                self.emit(Instruction::LoadConst(val.clone()))?;
            }
            Expr::Variable(name) => {
                if let Some(last) = self.variable_stack.last_mut() {
                    // TODO.
                    let index = last.iter().rposition(|v| v == name).unwrap_or(0);
                    self.emit(Instruction::LoadVar(index as u8))?;
                }
            }
            Expr::Unary(op, operand) => {
                self.generate_expr(operand)?;
                self.emit(Instruction::UnaryOp(op.clone()))?;
            }
            Expr::Binary(BinaryOp::And, left, right) => {
                self.generate_expr(left)?;
                let jump_addr = self.emit(Instruction::JumpIfFalse(0))?;
                self.emit(Instruction::Pop)?;
                self.generate_expr(right)?;
                let end_addr = self.current_addr();
                self.patch_jump(jump_addr, end_addr)?;
            }
            Expr::Binary(BinaryOp::Or, left, right) => {
                self.generate_expr(left)?;
                let jump_addr = self.emit(Instruction::JumpIfFalse(0))?;
                let jump_end = self.emit(Instruction::Jump(0))?;
                self.current_addr();
                let else_addr = self.emit(Instruction::Pop)?;
                self.patch_jump(jump_addr, else_addr)?;
                self.generate_expr(right)?;
                let end_addr = self.current_addr();
                self.patch_jump(jump_end, end_addr)?;
            }
            Expr::Binary(op, left, right) => {
                self.generate_expr(left)?;
                self.generate_expr(right)?;
                self.emit(Instruction::BinaryOp(op.clone()))?;
            }
            Expr::Call(name, args) => {
                for arg in args {
                    self.generate_expr(arg)?;
                }
                self.emit(Instruction::Call(name.clone(), args.len() as u8))?;
            }
            Expr::If {
                cond,
                then_branch,
                else_branch,
            } => {
                self.generate_expr(cond)?;
                let jump_else = self.emit(Instruction::JumpIfFalse(0))?;
                self.emit(Instruction::Pop)?;

                self.generate_block(then_branch)?;

                let jump_end = self.emit(Instruction::Jump(0))?;
                let else_addr = self.current_addr();
                self.patch_jump(jump_else, else_addr)?;
                self.emit(Instruction::Pop)?;

                if let Some(else_stmts) = else_branch {
                    self.generate_block(else_stmts)?;
                } else {
                    self.emit(Instruction::LoadConst(Value::Unit))?;
                }

                let end_addr = self.current_addr();
                self.patch_jump(jump_end, end_addr)?;
            }
            Expr::Block(block) => {
                self.generate_block(block)?;
            }
        }
        Ok(())
    }

    fn generate_stmt(&mut self, stmt: &Stmt) -> Result<(), CompileError> {
        match stmt {
            Stmt::Let(name, expr) => {
                if let Some(last) = self.variable_stack.last_mut() {
                    let index = last.len();
                    // TODO.
                    last.push(name.clone());

                    self.generate_expr(expr)?;
                    self.emit(Instruction::StoreVar(index as u8))?;
                }
            }
            Stmt::Assign(name, expr) => {
                if let Some(last) = self.variable_stack.last_mut() {
                    // TODO.
                    let index = last.iter().rposition(|v| v == name).unwrap_or(0);
                    self.generate_expr(expr)?;
                    self.emit(Instruction::StoreVar(index as u8))?;
                }
            }
            Stmt::Expr(expr) => {
                self.generate_expr(expr)?;
                self.emit(Instruction::Pop)?;
            }
            Stmt::While { cond, body } => {
                let loop_start = self.current_addr();
                self.loop_stack.push((loop_start, Vec::new(), Vec::new()));

                self.generate_expr(cond)?;
                let jump_end = self.emit(Instruction::JumpIfFalse(0))?;
                self.emit(Instruction::Pop)?;

                for stmt in body {
                    self.generate_stmt(stmt)?;
                }

                self.emit(Instruction::Jump(loop_start))?;
                let end_addr = self.current_addr();
                self.patch_jump(jump_end, end_addr)?;
                self.emit(Instruction::Pop)?;
                let break_target_addr = self.current_addr();

                let (_, breaks, continues) = self.loop_stack.pop().unwrap();
                for break_addr in breaks {
                    self.patch_jump(break_addr, break_target_addr)?;
                }
                for continue_addr in continues {
                    self.patch_jump(continue_addr, loop_start)?;
                }
            }
            Stmt::Break => {
                let addr = self.emit(Instruction::Jump(0))?;
                if let Some((_, breaks, _)) = self.loop_stack.last_mut() {
                    breaks.push(addr);
                }
            }
            Stmt::Continue => {
                let addr = self.emit(Instruction::Jump(0))?;
                if let Some((_, _, continues)) = self.loop_stack.last_mut() {
                    continues.push(addr);
                }
            }
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    self.generate_expr(expr)?;
                } else {
                    self.emit(Instruction::LoadConst(Value::Unit))?;
                }
                self.emit(Instruction::Return)?;
            }
        }
        Ok(())
    }

    pub fn generate(func: &Function) -> Result<Vec<Instruction>, CompileError> {
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
            generator.generate_stmt(stmt)?;
        }
        if !matches!(generator.instructions.last(), Some(Instruction::Return)) {
            generator.emit(Instruction::LoadConst(Value::Unit))?;
            generator.emit(Instruction::Return)?;
        }
        Ok(generator.instructions)
    }
}
