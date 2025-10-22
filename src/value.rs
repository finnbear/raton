use std::fmt;

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
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

    pub fn as_bool(&self) -> Result<bool, String> {
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
