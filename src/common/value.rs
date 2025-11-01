#[allow(unused_imports)]
use std::fmt::{self, Display};
use std::hash::Hash;

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

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Null => {
                state.write_u8(2);
            }
            #[cfg(feature = "bool_type")]
            &Self::Bool(b) => {
                // 0 or 1.
                state.write_u8(b as u8);
            }
            #[cfg(feature = "i32_type")]
            &Self::I32(i) => {
                state.write_i32(i);
            }
            #[cfg(feature = "f32_type")]
            &Self::F32(f) => {
                // `f == 0.0` is so `0.0` and `-0.0` hash the same.
                //
                // The NaN check limits cross-platform non-determinism.
                if f == 0.0 || f.is_nan() {
                    state.write_u8(3u8);
                } else {
                    state.write_u32(f.to_bits());
                }
            }
            #[cfg(feature = "string_type")]
            Self::String(s) => {
                s.hash(state);
            }
        }
    }
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
