use std::fmt::{self, Display};

use crate::RuntimeError;

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum Type {
    Unit,
    Bool,
    I32,
    F32,
    String,
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Unit => "()",
            Self::Bool => "bool",
            Self::I32 => "i32",
            Self::F32 => "f32",
            Self::String => "string",
        })
    }
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

    pub fn as_bool(&self) -> Result<bool, RuntimeError> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(RuntimeError::TypeMismatch {
                expected: Type::Bool,
                actual: self.type_of(),
            }),
        }
    }

    #[allow(unused)]
    fn as_i32(&self) -> Result<i32, RuntimeError> {
        match self {
            Value::I32(i) => Ok(*i),
            _ => Err(RuntimeError::TypeMismatch {
                expected: Type::I32,
                actual: self.type_of(),
            }),
        }
    }

    #[allow(unused)]
    fn as_f32(&self) -> Result<f32, RuntimeError> {
        match self {
            Value::F32(f) => Ok(*f),
            _ => Err(RuntimeError::TypeMismatch {
                expected: Type::F32,
                actual: self.type_of(),
            }),
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
