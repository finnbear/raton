#[cfg(doc)]
use super::RuntimeValue;
#[cfg(doc)]
use crate::Value;
use std::fmt::{self, Display};

/// The type of a [`Value`].
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "bitcode", derive(bitcode::Encode, bitcode::Decode))]
#[non_exhaustive]
pub enum Type {
    /// The type of [`Value::Null`] in [`RuntimeValue::Value`].
    Null,
    /// The type of [`Value::Bool`] in [`RuntimeValue::Value`].
    #[cfg(feature = "bool_type")]
    Bool,
    /// The type of [`Value::I32`] in [`RuntimeValue::Value`].
    #[cfg(feature = "i32_type")]
    I32,
    /// The type of [`Value::F32`] in [`RuntimeValue::Value`].
    #[cfg(feature = "f32_type")]
    F32,
    /// The type of [`Value::String`] in [`RuntimeValue::Value`].
    #[cfg(feature = "string_type")]
    String,
    /// The type of [`Extern::Value`] in [`RuntimeValue::Extern`].
    #[cfg(feature = "extern_value_type")]
    ExternValue,
    /// The type of [`Extern::Ref`] in [`RuntimeValue::Extern`].
    ExternRef,
    /// The type of [`Extern::Mut`] in [`RuntimeValue::Extern`].
    ExternMut,
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
            #[cfg(feature = "extern_value_type")]
            Self::ExternValue => "extern_value",
            Self::ExternRef => "extern_ref",
            Self::ExternMut => "extern_mut",
        })
    }
}
