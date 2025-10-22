use crate::{
    BytecodeGenerator, Value,
    ast::{BinaryOp, Program, UnaryOp},
    bytecode::*,
};
use std::collections::HashMap;

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
