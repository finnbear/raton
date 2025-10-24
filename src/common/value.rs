#[allow(unused_imports)]
use crate::runtime::RuntimeError;
use std::fmt::{self, Display};

/// The type of a [`Value`].
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "bitcode", derive(bitcode::Encode, bitcode::Decode))]
#[non_exhaustive]
pub enum Type {
    Null,
    #[cfg(feature = "bool_type")]
    Bool,
    #[cfg(feature = "i32_type")]
    I32,
    #[cfg(feature = "f32_type")]
    F32,
    #[cfg(feature = "string_type")]
    String,
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Null => "null",
            #[cfg(feature = "bool_type")]
            Self::Bool => "bool",
            #[cfg(feature = "i32_type")]
            Self::I32 => "i32",
            #[cfg(feature = "f32_type")]
            Self::F32 => "f32",
            #[cfg(feature = "string_type")]
            Self::String => "string",
        })
    }
}

/// A value, used an argument, operand, or return value.
///
/// It cannot be evaluated any further.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "bitcode", derive(bitcode::Encode, bitcode::Decode))]
#[non_exhaustive]
pub enum Value {
    Null,
    #[cfg(feature = "bool_type")]
    Bool(bool),
    #[cfg(feature = "i32_type")]
    I32(i32),
    #[cfg(feature = "f32_type")]
    F32(f32),
    #[cfg(feature = "string_type")]
    String(String),
}

impl Value {
    /// Get the [`Type`] of the value.
    pub fn type_of(&self) -> Type {
        match self {
            Value::Null => Type::Null,
            #[cfg(feature = "bool_type")]
            Value::Bool(_) => Type::Bool,
            #[cfg(feature = "i32_type")]
            Value::I32(_) => Type::I32,
            #[cfg(feature = "f32_type")]
            Value::F32(_) => Type::F32,
            #[cfg(feature = "string_type")]
            Value::String(_) => Type::String,
        }
    }

    #[cfg(feature = "bool_type")]
    pub(crate) fn as_bool(&self) -> Result<bool, RuntimeError> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(RuntimeError::TypeMismatch {
                expected: Type::Bool,
                actual: self.type_of(),
            }),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Null => write!(f, "()"),
            #[cfg(feature = "bool_type")]
            Value::Bool(b) => write!(f, "{}", b),
            #[cfg(feature = "i32_type")]
            Value::I32(i) => write!(f, "{}", i),
            #[cfg(feature = "f32_type")]
            Value::F32(fl) => write!(f, "{}", fl),
            #[cfg(feature = "string_type")]
            Value::String(s) => write!(f, "{}", s),
        }
    }
}
