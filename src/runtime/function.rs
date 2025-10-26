use super::{Extern, RuntimeError, RuntimeValue, Type};
use crate::Value;

/// Type-erased function, callable by a script.
pub(crate) struct ErasedFunction<'a> {
    // TODO: SmallBox?
    #[allow(clippy::type_complexity)]
    inner: Box<dyn FnMut(&mut [RuntimeValue<'a>]) -> Result<RuntimeValue<'a>, RuntimeError> + 'a>,
}

impl<'a> ErasedFunction<'a> {
    /// Erase the type of a function.
    pub(crate) fn new<A, R, F: Function<'a, A, R> + 'a>(mut inner: F) -> Self {
        Self {
            inner: Box::new(move |arguments| inner.call(arguments)),
        }
    }

    /// Call the type-erased function.
    pub(crate) fn call(
        &mut self,
        arguments: &mut [RuntimeValue<'a>],
    ) -> Result<RuntimeValue<'a>, RuntimeError> {
        (self.inner)(arguments)
    }
}

/// A Rust function callable by a script.
pub trait Function<'a, A, R>: Send + Sync {
    /// The number of arguments.
    const ARGS: usize;

    /// Call the function.
    fn call(
        &mut self,
        arguments: &mut [RuntimeValue<'a>],
    ) -> Result<RuntimeValue<'a>, RuntimeError>;
}

/// Types that can be converted to a [`RuntimeValue`], allowing them to
/// be passed as arguments from the host to a script function.
pub trait ToRuntimeValue<'a>: Sized {
    /// Perform the conversion.
    fn to_value(self) -> RuntimeValue<'a>;
}

/// Types that can be converted from a [`RuntimeValue`], allowing them to
/// be passed as arguments from a script function to the host.
pub trait FromRuntimeValue<'a>: Sized {
    /// The specific type this is looking for, if any. This is used to
    /// improve errors.
    const TYPE: Option<Type> = None;

    /// Perform the conversion, returning [`None`] if the [`RuntimeValue`]
    /// is of an incompatible type.
    fn from_value(value: RuntimeValue<'a>) -> Option<Self>;
}

impl<'a> ToRuntimeValue<'a> for RuntimeValue<'a> {
    fn to_value(self) -> RuntimeValue<'a> {
        self
    }
}

impl<'a> FromRuntimeValue<'a> for RuntimeValue<'a> {
    fn from_value(value: RuntimeValue<'a>) -> Option<Self> {
        Some(value)
    }
}

macro_rules! both_ways {
    ($v:ident, $t:path) => {
        impl<'a> ToRuntimeValue<'a> for $t {
            fn to_value(self) -> RuntimeValue<'a> {
                RuntimeValue::$v(self)
            }
        }

        impl<'a> FromRuntimeValue<'a> for $t {
            fn from_value(value: RuntimeValue<'a>) -> Option<Self> {
                if let RuntimeValue::$v(v) = value {
                    Some(v)
                } else {
                    None
                }
            }
        }
    };
}

both_ways!(Value, Value);
both_ways!(Extern, Extern<'a>);

/// A shared [`std::rc::Rc`]-reference to a host value, which may be cheaply copied in a script.
///
/// Since [`std::rc::Rc`] is heap-allocated, requiring no value to reference it's easier to
/// create this type of extern value in a host function called by the script.
#[cfg(feature = "extern_value_type")]
pub struct ExternValue<T>(pub std::rc::Rc<T>);

/// An immutable reference to a host value, which may be freely copied in a script.
pub struct ExternRef<'a, T>(pub &'a T);

/// A mutable reference to a host value, which is taken when used in a script.
pub struct ExternMut<'a, T>(pub &'a mut T);

#[cfg(feature = "extern_value_type")]
impl<'a, T: 'static> ToRuntimeValue<'a> for ExternValue<T> {
    fn to_value(self) -> RuntimeValue<'a> {
        RuntimeValue::Extern(Extern::Value(self.0))
    }
}

#[cfg(feature = "extern_value_type")]
impl<'a, T: 'static> FromRuntimeValue<'a> for ExternValue<T> {
    const TYPE: Option<Type> = Some(Type::ExternValue);

    fn from_value(value: RuntimeValue<'a>) -> Option<Self> {
        if let RuntimeValue::Extern(Extern::Value(value)) = value {
            value.downcast().ok().map(ExternValue)
        } else {
            None
        }
    }
}

impl<'a, T: 'static> ToRuntimeValue<'a> for ExternRef<'a, T> {
    fn to_value(self) -> RuntimeValue<'a> {
        RuntimeValue::Extern(Extern::Ref(self.0))
    }
}

impl<'a, T: 'static> FromRuntimeValue<'a> for ExternRef<'a, T> {
    const TYPE: Option<Type> = Some(Type::ExternRef);

    fn from_value(value: RuntimeValue<'a>) -> Option<Self> {
        if let RuntimeValue::Extern(Extern::Ref(value)) = value {
            value.downcast_ref().map(ExternRef)
        } else {
            None
        }
    }
}

impl<'a, T: 'static> ToRuntimeValue<'a> for ExternMut<'a, T> {
    fn to_value(self) -> RuntimeValue<'a> {
        RuntimeValue::Extern(Extern::Mut(self.0))
    }
}

impl<'a, T: 'static> FromRuntimeValue<'a> for ExternMut<'a, T> {
    const TYPE: Option<Type> = Some(Type::ExternMut);

    fn from_value(value: RuntimeValue<'a>) -> Option<Self> {
        if let RuntimeValue::Extern(Extern::Mut(value)) = value {
            value.downcast_mut().map(ExternMut)
        } else {
            None
        }
    }
}

#[allow(unused)]
macro_rules! impl_convert_argument {
    ($v:ident, $t:ident) => {
        impl<'a> ToRuntimeValue<'a> for $t {
            fn to_value(self) -> RuntimeValue<'a> {
                RuntimeValue::Value(Value::$v(self))
            }
        }

        impl<'a> FromRuntimeValue<'a> for $t {
            const TYPE: Option<Type> = Some(Type::$v);

            fn from_value(value: RuntimeValue<'a>) -> Option<Self> {
                if let RuntimeValue::Value(Value::$v(v)) = value {
                    Some(v)
                } else {
                    None
                }
            }
        }
    };
}

#[cfg(feature = "bool_type")]
impl_convert_argument!(Bool, bool);
#[cfg(feature = "i32_type")]
impl_convert_argument!(I32, i32);
#[cfg(feature = "f32_type")]
impl_convert_argument!(F32, f32);
#[cfg(feature = "string_type")]
impl_convert_argument!(String, String);

macro_rules! impl_function {
    ($($a: ident),*) => {
        impl<'a, $($a,)* R, FUNC: FnMut($($a),*) -> Result<R, RuntimeError> + Send + Sync> Function<'a, ($($a,)*), R> for FUNC
            where $($a: FromRuntimeValue<'a>,)*
                R: ToRuntimeValue<'a> {
            const ARGS: usize = 0 $(
                + {
                    let _ = std::mem::size_of::<$a>();
                    1
                }
            )*;

            fn call(
                    &mut self,
                    arguments: &mut [RuntimeValue<'a>],
                ) -> Result<RuntimeValue<'a>, RuntimeError> {
                if arguments.len() != Self::ARGS {
                    return Err(RuntimeError::WrongNumberOfArguments{expected: Self::ARGS as u16, actual: arguments.len() as u16});
                }
                let mut _i = 0;
                (self)($({
                    let arg = &mut arguments[{
                        let n = _i;
                        _i += 1;
                        n
                    }];
                    let type_of = arg.type_of();
                    <$a>::from_value(std::mem::take(arg)).ok_or(RuntimeError::InvalidArgument{
                        expected: $a::TYPE,
                        actual: type_of
                    })?
                }),*).map(|v| v.to_value())
            }
        }
    };
}

impl_function!();
impl_function!(A);
impl_function!(A, B);
impl_function!(A, B, C);
impl_function!(A, B, C, D);
impl_function!(A, B, C, D, E);
impl_function!(A, B, C, D, E, F);
impl_function!(A, B, C, D, E, F, G);
impl_function!(A, B, C, D, E, F, G, H);
impl_function!(A, B, C, D, E, F, G, H, I);
impl_function!(A, B, C, D, E, F, G, H, I, J);
