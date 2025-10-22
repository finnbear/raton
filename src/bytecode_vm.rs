use crate::{
    BytecodeGenerator, Type, Value,
    ast::{BinaryOp, Program, UnaryOp},
    bytecode::*,
};
use std::collections::HashMap;
use thiserror::Error;

pub type HostFunction = Box<dyn Fn(&[Value]) -> Result<Value, RuntimeError>>;

/// Executes bytecode.
pub struct Vm {
    functions: HashMap<String, Vec<Instruction>>,
    host_functions: HashMap<String, HostFunction>,
}

#[derive(Debug, Error)]
pub enum RuntimeError {
    #[error("type mismatch ({expected} expected {actual} actual)")]
    TypeMismatch { expected: Type, actual: Type },
    #[error("undefined function ({name})")]
    UndefinedFunction { name: String },
    #[error("undefined variable (index {index})")]
    UndefinedVariable { index: u8 },
    #[error("invalid operand ({actual})")]
    InvalidOperand { actual: Type },
    #[error("invalid argument(s)")]
    InvalidArgument,
    #[error("integer division by zero")]
    IntegerDivisionByZero,
    #[error("integer overflow")]
    IntegerOverflow,
    /// This indicates a bug in Ratón.
    #[error("stack underflow (bug in Ratón)")]
    StackUnderflow,
    #[error("stack overflow")]
    StackOverflow,
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
                    return Err(RuntimeError::InvalidArgument);
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
                    return Err(RuntimeError::InvalidArgument);
                }
                match &args[0] {
                    Value::I32(i) => Ok(Value::I32(*i)),
                    Value::Bool(b) => Ok(Value::I32(if *b { 1 } else { 0 })),
                    Value::F32(f) => Ok(Value::I32(*f as i32)),
                    Value::String(s) => Ok(s.parse::<i32>().map(Value::I32).unwrap_or(Value::Unit)),
                    _ => Err(RuntimeError::InvalidArgument),
                }
            }),
        );

        vm.register_host_function(
            "f32",
            Box::new(|args| {
                if args.len() != 1 {
                    return Err(RuntimeError::InvalidArgument);
                }
                match &args[0] {
                    Value::F32(f) => Ok(Value::F32(*f)),
                    Value::I32(i) => Ok(Value::F32(*i as f32)),
                    Value::Bool(b) => Ok(Value::F32(if *b { 1.0 } else { 0.0 })),
                    Value::String(s) => Ok(s.parse::<f32>().map(Value::F32).unwrap_or(Value::Unit)),
                    _ => Err(RuntimeError::InvalidArgument),
                }
            }),
        );

        vm.register_host_function(
            "string",
            Box::new(|args| {
                if args.len() != 1 {
                    return Err(RuntimeError::InvalidArgument);
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

    pub fn execute(&self, func_name: &str, args: Vec<Value>) -> Result<Value, RuntimeError> {
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
                    &Instruction::LoadVar(index) => {
                        let val = variables
                            .get(index as usize)
                            .ok_or_else(|| RuntimeError::UndefinedVariable { index })?;
                        stack.push(val.clone());
                        pc += 1;
                    }
                    &Instruction::StoreVar(index) => {
                        let val = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                        while variables.len() < index as usize + 1 {
                            variables.push(Value::Unit);
                        }
                        *variables
                            .get_mut(index as usize)
                            .ok_or_else(|| RuntimeError::UndefinedVariable { index })? = val;
                        pc += 1;
                    }
                    Instruction::UnaryOp(op) => {
                        let operand = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                        let result = match op {
                            UnaryOp::Not => {
                                let b = operand.as_bool()?;
                                Value::Bool(!b)
                            }
                            UnaryOp::Neg => match operand {
                                Value::I32(i) => Value::I32(
                                    i.checked_neg().ok_or(RuntimeError::IntegerOverflow)?,
                                ),
                                Value::F32(f) => Value::F32(-f),
                                _ => {
                                    return Err(RuntimeError::InvalidOperand {
                                        actual: operand.type_of(),
                                    });
                                }
                            },
                        };
                        stack.push(result);
                        pc += 1;
                    }
                    Instruction::BinaryOp(op) => {
                        let right = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                        let left = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                        let result = match op {
                            BinaryOp::Add => match (&left, &right) {
                                (&Value::I32(l), &Value::I32(r)) => Value::I32(
                                    l.checked_add(r).ok_or(RuntimeError::IntegerOverflow)?,
                                ),
                                (&Value::F32(l), &Value::F32(r)) => Value::F32(l + r),
                                (Value::String(l), Value::String(r)) => {
                                    Value::String(format!("{l}{r}"))
                                }
                                _ => {
                                    return Err(RuntimeError::TypeMismatch {
                                        expected: left.type_of(),
                                        actual: right.type_of(),
                                    });
                                }
                            },
                            BinaryOp::Sub => match (&left, &right) {
                                (&Value::I32(l), &Value::I32(r)) => Value::I32(
                                    l.checked_sub(r).ok_or(RuntimeError::IntegerOverflow)?,
                                ),
                                (&Value::F32(l), &Value::F32(r)) => Value::F32(l - r),
                                _ => {
                                    return Err(RuntimeError::TypeMismatch {
                                        expected: left.type_of(),
                                        actual: right.type_of(),
                                    });
                                }
                            },
                            BinaryOp::Mul => match (&left, &right) {
                                (&Value::I32(l), &Value::I32(r)) => Value::I32(
                                    l.checked_mul(r).ok_or(RuntimeError::IntegerOverflow)?,
                                ),
                                (&Value::F32(l), &Value::F32(r)) => Value::F32(l * r),
                                _ => {
                                    return Err(RuntimeError::TypeMismatch {
                                        expected: left.type_of(),
                                        actual: right.type_of(),
                                    });
                                }
                            },
                            BinaryOp::Div => match (&left, &right) {
                                (&Value::I32(l), &Value::I32(r)) => {
                                    if r == 0 {
                                        return Err(RuntimeError::IntegerDivisionByZero);
                                    }
                                    Value::I32(
                                        l.checked_div(r).ok_or(RuntimeError::IntegerOverflow)?,
                                    )
                                }
                                (&Value::F32(l), &Value::F32(r)) => Value::F32(l / r),
                                _ => {
                                    return Err(RuntimeError::TypeMismatch {
                                        expected: left.type_of(),
                                        actual: right.type_of(),
                                    });
                                }
                            },
                            BinaryOp::Mod => match (&left, &right) {
                                (&Value::I32(l), &Value::I32(r)) => {
                                    if r == 0 {
                                        return Err(RuntimeError::IntegerDivisionByZero);
                                    }
                                    Value::I32(l % r)
                                }
                                (&Value::F32(l), &Value::F32(r)) => Value::F32(l % r),
                                _ => {
                                    return Err(RuntimeError::TypeMismatch {
                                        expected: left.type_of(),
                                        actual: right.type_of(),
                                    });
                                }
                            },
                            BinaryOp::Eq => Value::Bool(left == right),
                            BinaryOp::Ne => Value::Bool(left != right),
                            BinaryOp::Lt => match (&left, &right) {
                                (&Value::I32(l), &Value::I32(r)) => Value::Bool(l < r),
                                (&Value::F32(l), &Value::F32(r)) => Value::Bool(l < r),
                                _ => {
                                    return Err(RuntimeError::TypeMismatch {
                                        expected: left.type_of(),
                                        actual: right.type_of(),
                                    });
                                }
                            },
                            BinaryOp::Le => match (&left, &right) {
                                (&Value::I32(l), &Value::I32(r)) => Value::Bool(l <= r),
                                (&Value::F32(l), &Value::F32(r)) => Value::Bool(l <= r),
                                _ => {
                                    return Err(RuntimeError::TypeMismatch {
                                        expected: left.type_of(),
                                        actual: right.type_of(),
                                    });
                                }
                            },
                            BinaryOp::Gt => match (&left, &right) {
                                (&Value::I32(l), &Value::I32(r)) => Value::Bool(l > r),
                                (&Value::F32(l), &Value::F32(r)) => Value::Bool(l > r),
                                _ => {
                                    return Err(RuntimeError::TypeMismatch {
                                        expected: left.type_of(),
                                        actual: right.type_of(),
                                    });
                                }
                            },
                            BinaryOp::Ge => match (&left, &right) {
                                (&Value::I32(l), &Value::I32(r)) => Value::Bool(l >= r),
                                (&Value::F32(l), &Value::F32(r)) => Value::Bool(l >= r),
                                _ => {
                                    return Err(RuntimeError::TypeMismatch {
                                        expected: left.type_of(),
                                        actual: right.type_of(),
                                    });
                                }
                            },
                            BinaryOp::And | BinaryOp::Or => {
                                unreachable!("And/Or should have been desugared");
                            }
                        };
                        stack.push(result);
                        pc += 1;
                    }
                    Instruction::Jump(target) => {
                        pc = *target;
                    }
                    Instruction::JumpIfFalse(target) => {
                        let cond = stack.last().ok_or(RuntimeError::StackUnderflow)?;
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
                            call_args.push(stack.pop().ok_or(RuntimeError::StackUnderflow)?);
                        }
                        call_args.reverse();

                        let result = if let Some(host_fn) = self.host_functions.get(name) {
                            host_fn(&call_args)?
                        } else if self.functions.contains_key(name) {
                            // TODO: don't use host stack(?)
                            self.execute(name, call_args)?
                        } else {
                            return Err(RuntimeError::UndefinedFunction { name: name.clone() });
                        };

                        stack.push(result);
                        pc += 1;
                    }
                    Instruction::Return => {
                        return stack.pop().ok_or(RuntimeError::StackUnderflow);
                    }
                    Instruction::Pop => {
                        stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                        pc += 1;
                    }
                }
            }

            Ok(Value::Unit)
        } else {
            Err(RuntimeError::UndefinedFunction {
                name: func_name.to_owned(),
            })
        }
    }
}
