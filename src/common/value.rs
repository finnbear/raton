#[allow(unused_imports)]
use std::fmt::{self, Display};

use crate::runtime::Type;

/// A value, used an argument, operand, or return value.
///
/// It cannot be evaluated any further.
#[derive(Default, Debug, Clone, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "bitcode", derive(bitcode::Encode, bitcode::Decode))]
#[non_exhaustive]
pub enum Value {
    /// A missing value.
    #[default]
    Null,
    /// A boolean, [`true`] or [`false`].
    #[cfg(feature = "bool_type")]
    Bool(bool),
    /// A signed 32-bit integer.
    #[cfg(feature = "i32_type")]
    I32(i32),
    /// An IEEE-754 32-bit float.
    #[cfg(feature = "f32_type")]
    F32(f32),
    /// A UTF-8 string.
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
