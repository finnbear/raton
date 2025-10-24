/// An operator that takes one operand.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "bitcode", derive(bitcode::Encode, bitcode::Decode))]
#[non_exhaustive]
pub enum UnaryOperator {
    /// `!`
    #[cfg(feature = "bool_type")]
    Not,
    /// `-`
    Neg,
}

/// An operator that takes two operands.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "bitcode", derive(bitcode::Encode, bitcode::Decode))]
#[non_exhaustive]
pub enum BinaryOperator {
    /// `+`
    Add,
    /// `-`
    Subtract,
    /// `*`
    Multiply,
    /// `/`
    Divide,
    /// `%`
    Modulo,
    /// `==`
    #[cfg(feature = "bool_type")]
    Equal,
    /// `!=`
    #[cfg(feature = "bool_type")]
    NotEqual,
    /// `<`
    #[cfg(feature = "bool_type")]
    LessThan,
    /// `<=`
    #[cfg(feature = "bool_type")]
    LessThanOrEqual,
    /// `>`
    #[cfg(feature = "bool_type")]
    GreaterThan,
    /// `>=`
    #[cfg(feature = "bool_type")]
    GreaterThanOrEqual,
    /// `&&`
    ///
    /// This is short-circuiting.
    #[cfg(feature = "bool_type")]
    And,
    /// `||`
    ///
    /// This is short-circuiting.
    #[cfg(feature = "bool_type")]
    Or,
}
