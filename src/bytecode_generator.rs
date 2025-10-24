use crate::{Value, ast::*, bytecode::*};
use thiserror::Error;

pub struct BytecodeGenerator {
    instructions: Vec<Instruction>,
    variable_stack: Vec<Vec<String>>,
    /// (start, breaks, continues)
    #[cfg(feature = "while_loop")]
    loop_stack: Vec<(u32, Vec<u32>, Vec<u32>)>,
    max_instructions: u32,
}

/// An error produced when compiling a program into bytecode.
#[derive(Debug, Error)]
pub enum CompileError {
    #[error("max instructions exceeded")]
    MaxInstructionsExceeded,
    #[error("internal compiler error")]
    Internal,
}

impl BytecodeGenerator {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            variable_stack: Vec::new(),
            #[cfg(feature = "while_loop")]
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

    #[allow(unused)]
    fn current_addr(&self) -> u32 {
        self.instructions.len() as u32
    }

    #[allow(unused)]
    fn patch_jump(&mut self, addr: u32, target: u32) -> Result<(), CompileError> {
        match &mut self.instructions[addr as usize] {
            Instruction::Jump(t) => {
                *t = target;
                Ok(())
            }
            #[cfg(feature = "bool_type")]
            Instruction::JumpIfFalse(t) => {
                *t = target;
                Ok(())
            }
            _ => {
                debug_assert!(false);
                Err(CompileError::Internal)
            }
        }
    }

    fn generate_block(&mut self, block: &BlockExpression) -> Result<(), CompileError> {
        for stmt in &block.statements {
            self.generate_stmt(stmt)?;
        }
        if let Some(value) = &block.value {
            self.generate_expr(value)?;
        } else {
            self.emit(Instruction::LoadConst(Value::Null))?;
        }
        Ok(())
    }

    fn generate_expr(&mut self, expr: &Expression) -> Result<(), CompileError> {
        match expr {
            Expression::Literal(val) => {
                self.emit(Instruction::LoadConst(val.clone()))?;
            }
            Expression::Variable(name) => {
                if let Some(last) = self.variable_stack.last_mut() {
                    // TODO.
                    let index = last.iter().rposition(|v| v == name).unwrap_or(0);
                    self.emit(Instruction::LoadVar(index as u8))?;
                }
            }
            Expression::Unary(UnaryExpression { operand, operator }) => {
                self.generate_expr(operand)?;
                self.emit(Instruction::UnaryOp(operator.clone()))?;
            }
            #[cfg(feature = "bool_type")]
            Expression::Binary(BinaryExpression {
                operator: BinaryOperator::And,
                left,
                right,
            }) => {
                self.generate_expr(left)?;
                let jump_addr = self.emit(Instruction::JumpIfFalse(0))?;
                self.emit(Instruction::Pop)?;
                self.generate_expr(right)?;
                let end_addr = self.current_addr();
                self.patch_jump(jump_addr, end_addr)?;
            }
            #[cfg(feature = "bool_type")]
            Expression::Binary(BinaryExpression {
                operator: BinaryOperator::Or,
                left,
                right,
            }) => {
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
            Expression::Binary(BinaryExpression {
                left,
                right,
                operator,
            }) => {
                self.generate_expr(left)?;
                self.generate_expr(right)?;
                self.emit(Instruction::BinaryOp(operator.clone()))?;
            }
            Expression::Call(CallExpression {
                identifier: name,
                arguments,
            }) => {
                for argument in arguments {
                    self.generate_expr(argument)?;
                }
                self.emit(Instruction::Call(name.clone(), arguments.len() as u8))?;
            }
            #[cfg(feature = "if_expression")]
            Expression::If(IfExpression {
                condition: cond,
                then_branch,
                else_branch,
            }) => {
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
                    self.emit(Instruction::LoadConst(Value::Null))?;
                }

                let end_addr = self.current_addr();
                self.patch_jump(jump_end, end_addr)?;
            }
            Expression::Block(block) => {
                self.generate_block(block)?;
            }
        }
        Ok(())
    }

    fn generate_stmt(&mut self, stmt: &Statement) -> Result<(), CompileError> {
        match stmt {
            Statement::Let(LetStatement {
                identifier,
                expression,
            }) => {
                if let Some(last) = self.variable_stack.last_mut() {
                    let index = last.len();
                    // TODO.
                    last.push(identifier.clone());

                    self.generate_expr(expression)?;
                    self.emit(Instruction::StoreVar(index as u8))?;
                }
            }
            Statement::Assign(AssignStatement {
                identifier,
                expression,
            }) => {
                if let Some(last) = self.variable_stack.last_mut() {
                    // TODO.
                    let index = last.iter().rposition(|v| v == identifier).unwrap_or(0);
                    self.generate_expr(expression)?;
                    self.emit(Instruction::StoreVar(index as u8))?;
                }
            }
            Statement::Expression(expr) => {
                self.generate_expr(expr)?;
                self.emit(Instruction::Pop)?;
            }
            #[cfg(feature = "while_loop")]
            Statement::While(WhileLoop { condition, body }) => {
                let loop_start = self.current_addr();
                self.loop_stack.push((loop_start, Vec::new(), Vec::new()));

                self.generate_expr(condition)?;
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
            #[cfg(feature = "while_loop")]
            Statement::Break => {
                let addr = self.emit(Instruction::Jump(0))?;
                if let Some((_, breaks, _)) = self.loop_stack.last_mut() {
                    breaks.push(addr);
                }
            }
            #[cfg(feature = "while_loop")]
            Statement::Continue => {
                let addr = self.emit(Instruction::Jump(0))?;
                if let Some((_, _, continues)) = self.loop_stack.last_mut() {
                    continues.push(addr);
                }
            }
            Statement::Return(expr) => {
                if let Some(expr) = expr {
                    self.generate_expr(expr)?;
                } else {
                    self.emit(Instruction::LoadConst(Value::Null))?;
                }
                self.emit(Instruction::Return)?;
            }
        }
        Ok(())
    }

    pub fn generate_function(mut self, func: &Function) -> Result<FunctionBytecode, CompileError> {
        self.variable_stack.push(Vec::new());
        for arg in &func.arguments {
            self.variable_stack.last_mut().unwrap().push(arg.clone());
        }
        self.generate_block(&func.body)?;
        for stmt in &func.body.statements {
            self.generate_stmt(stmt)?;
        }
        if !matches!(self.instructions.last(), Some(Instruction::Return)) {
            self.emit(Instruction::Return)?;
        }
        Ok(FunctionBytecode {
            instructions: self.instructions,
        })
    }
}
