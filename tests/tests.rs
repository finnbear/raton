use raton::prelude::*;
#[allow(unused_imports)]
use std::time::Instant;

fn assert_execute<'a>(src: &str, func: &str, args: &'a mut [RuntimeValue<'a>], expected: Value) {
    let ast = Parser::new().parse(src).unwrap();
    let program = CodeGenerator::new()
        .with_max_depth(100)
        .generate_program(&ast)
        .unwrap();
    let mut vm = VirtualMachine::new(&program).with_type_casting();

    let result = vm.call(func, args).unwrap();
    assert_eq!(result, RuntimeValue::Value(expected));
}

#[test]
fn minimal() {
    let src = r#"
        fn hello() {}
    "#;

    assert_execute(src, "hello", &mut [], Value::Null);
}

#[test]
#[cfg(all(feature = "i32_type", feature = "f32_type"))]
fn simple_function2() {
    let src = r#"
        fn to_float(a) {
            return f32(a);
        }

        fn to_float2(a) {
            return to_float(a);
        }
    "#;

    assert_execute(
        src,
        "to_float2",
        &mut [RuntimeValue::Value(Value::I32(5))],
        Value::F32(5.0),
    );
}

#[test]
#[cfg(feature = "i32_type")]
fn simple_math() {
    let src = r#"
        fn add(a, b) {
            return a + b;
        }
    "#;

    assert_execute(
        src,
        "add",
        &mut [
            RuntimeValue::Value(Value::I32(5)),
            RuntimeValue::Value(Value::I32(3)),
        ],
        Value::I32(8),
    );
}

#[test]
#[cfg(all(feature = "if_expression", feature = "i32_type"))]
fn simple_if_expression() {
    let src = r#"
        fn max(a, b) {
            if (a > b) {
                return a;
            } else {
                return b;
            }
        }
    "#;

    assert_execute(
        src,
        "max",
        &mut [
            RuntimeValue::Value(Value::I32(5)),
            RuntimeValue::Value(Value::I32(3)),
        ],
        Value::I32(5),
    );
    assert_execute(
        src,
        "max",
        &mut [
            RuntimeValue::Value(Value::I32(3)),
            RuntimeValue::Value(Value::I32(5)),
        ],
        Value::I32(5),
    );
}

#[test]
#[cfg(all(feature = "if_expression", feature = "i32_type"))]
fn argument_order() {
    let src = r#"
        fn first(a, b) {
            a
        }

        fn call_first(a, b) {
            first(a, b)
        }
    "#;

    assert_execute(
        src,
        "call_first",
        &mut [
            RuntimeValue::Value(Value::I32(5)),
            RuntimeValue::Value(Value::I32(3)),
        ],
        Value::I32(5),
    );
    assert_execute(
        src,
        "call_first",
        &mut [
            RuntimeValue::Value(Value::I32(3)),
            RuntimeValue::Value(Value::I32(5)),
        ],
        Value::I32(3),
    );
}

#[test]
#[cfg(all(feature = "while_loop", feature = "i32_type"))]
fn simple_while_loop() {
    let src = r#"
        fn sum_to_n(n) {
            let i = 0;
            let sum = 0;
            while (i < n) {
                i = i + 1;
                sum = sum + i;
            }
            return sum;
        }
    "#;

    assert_execute(
        src,
        "sum_to_n",
        &mut [RuntimeValue::Value(Value::I32(5))],
        Value::I32(15),
    );
}

#[test]
#[cfg(feature = "extern_value_type")]
fn extern_types() {
    use std::rc::Rc;

    struct Foo {
        contents: u8,
    }

    let src = r#"
        fn call_on_extern(a, b, c) {
            host_call(a, b, c)
        }
    "#;

    let ast = Parser::new().parse(src).unwrap();
    let program = CodeGenerator::new()
        .with_max_depth(100)
        .generate_program(&ast)
        .unwrap();

    let mut a = Foo { contents: 2 };
    let mut other = 0;

    {
        let mut vm = VirtualMachine::new(&program)
            .with_type_casting()
            .with_host_function(
                "host_call",
                |ExternValue(a): ExternValue<Foo>,
                 ExternRef(b): ExternRef<'_, Foo>,
                 ExternMut(c): ExternMut<'_, Foo>| {
                    c.contents *= a.contents * b.contents;
                    other += 1;
                    Ok(RuntimeValue::default())
                },
            );

        let result = vm
            .call3(
                "call_on_extern",
                ExternValue(Rc::new(Foo { contents: 3 })),
                ExternRef(&Foo { contents: 5 }),
                ExternMut(&mut a),
            )
            .unwrap();
        assert_eq!(result, Default::default());
    }

    assert_eq!(a.contents, 30);
    assert_eq!(other, 1);
}

#[test]
#[cfg(all(feature = "extern_value_type", feature = "method_call_expression"))]
fn method_call_on_value() {
    struct Foo {
        contents: u8,
    }

    let src = r#"
        fn call_on_receiver(a, b) {
            a.host_call(b);
            a.host_call(b);
        }
    "#;

    let ast = Parser::new().parse(src).unwrap();
    let program = CodeGenerator::new()
        .with_max_depth(100)
        .generate_program(&ast)
        .unwrap();

    let a = std::rc::Rc::new(Foo { contents: 2 });
    let mut other = 0;

    {
        let mut vm = VirtualMachine::new(&program)
            .with_type_casting()
            .with_host_function(
                "host_call",
                |_receiver: Receiver<ExternValue<Foo>>, ExternRef(_b): ExternRef<'_, Foo>| {
                    //receiver.contents *= b.contents;
                    other += 1;
                    Ok(RuntimeValue::default())
                },
            )
            .with_host_function(
                "host_call",
                |_: Receiver<Value>| -> Result<Value, RuntimeError> {
                    panic!("wrong method called");
                },
            );

        let result = vm
            .call2(
                "call_on_receiver",
                ExternValue(std::rc::Rc::clone(&a)),
                ExternRef(&Foo { contents: 5 }),
            )
            .unwrap();
        assert_eq!(result, Default::default());
    }

    assert_eq!(a.contents, 2);
    assert_eq!(other, 2);
}

#[test]
#[cfg(feature = "method_call_expression")]
fn method_call_on_ref() {
    struct Foo {
        contents: u8,
    }

    let src = r#"
        fn call_on_receiver(a, b, c) {
            a.host_call(b);
            a.host_call(c);
        }
    "#;

    let ast = Parser::new().parse(src).unwrap();
    let program = CodeGenerator::new()
        .with_max_depth(100)
        .generate_program(&ast)
        .unwrap();

    let mut b = Foo { contents: 2 };
    let mut c = Foo { contents: 3 };
    let mut other = 0;

    {
        let mut vm = VirtualMachine::new(&program)
            .with_type_casting()
            .with_host_function(
                "host_call",
                |receiver: Receiver<ExternRef<Foo>>, ExternMut(b): ExternMut<Foo>| {
                    b.contents *= receiver.contents;
                    other += 1;
                    Ok(Value::Null)
                },
            )
            .with_host_function(
                "host_call",
                |_: Receiver<Value>| -> Result<Value, RuntimeError> {
                    panic!("wrong method called");
                },
            );

        let result = vm
            .call3(
                "call_on_receiver",
                ExternRef(&Foo { contents: 5 }),
                ExternMut(&mut b),
                ExternMut(&mut c),
            )
            .unwrap();
        assert_eq!(result, Default::default());
    }

    assert_eq!(b.contents, 10);
    assert_eq!(c.contents, 15);
    assert_eq!(other, 2);
}

#[test]
#[cfg(feature = "method_call_expression")]
fn method_call_on_mut() {
    struct Foo {
        contents: u8,
    }

    let src = r#"
        fn call_on_receiver(a, b) {
            a.host_call(b);
            a.host_call(b);
        }
    "#;

    let ast = Parser::new().parse(src).unwrap();
    let program = CodeGenerator::new()
        .with_max_depth(100)
        .generate_program(&ast)
        .unwrap();

    let mut a = Foo { contents: 2 };
    let mut other = 0;

    {
        let mut vm = VirtualMachine::new(&program)
            .with_type_casting()
            .with_host_function(
                "host_call",
                |mut receiver: Receiver<ExternMut<Foo>>, ExternRef(b): ExternRef<Foo>| {
                    receiver.contents *= b.contents;
                    other += 1;
                    Ok(RuntimeValue::default())
                },
            )
            .with_host_function(
                "host_call",
                |_: Receiver<Value>| -> Result<Value, RuntimeError> {
                    panic!("wrong method called");
                },
            );

        let result = vm
            .call2(
                "call_on_receiver",
                ExternMut(&mut a),
                ExternRef(&Foo { contents: 5 }),
            )
            .unwrap();
        assert_eq!(result, Default::default());
    }

    assert_eq!(a.contents, 50);
    assert_eq!(other, 2);
}

#[test]
#[cfg(feature = "f32_type")]
fn error_on_nan() {
    use core::f32;

    let src = r#"
        fn sub(a, b) {
            a - b
        }
    "#;

    let ast = Parser::new().parse(src).unwrap();
    let program = CodeGenerator::new().generate_program(&ast).unwrap();

    let mut vm = VirtualMachine::new(&program).with_error_on_nan(true);

    let result = vm
        .call2("sub", Value::F32(f32::INFINITY), Value::F32(f32::INFINITY))
        .unwrap_err();
    assert!(matches!(result, RuntimeError::ProducedNan));
}

#[test]
fn undefined() {
    let src = r#"
        fn undefined() {
            return a;
        }
    "#;

    let ast = Parser::new().parse(src).unwrap();
    let result = CodeGenerator::new().generate_program(&ast).unwrap_err();
    assert!(matches!(result, CompileError::UndefinedVariable { .. }));
}

#[test]
fn scope() {
    let src = r#"
        fn scope() {
            {
                let a = null;
            };
            return a;
        }
    "#;

    let ast = Parser::new().parse(src).unwrap();
    let result = CodeGenerator::new().generate_program(&ast).unwrap_err();
    assert!(matches!(result, CompileError::UndefinedVariable { .. }));
}

#[test]
#[cfg(feature = "single_line_comment")]
fn single_line_comments() {
    let src = r#"
        fn enterprise_grade() {
            // hello
            let a = null;
            // foo bar
        }
    "#;

    assert_execute(src, "enterprise_grade", &mut [], Value::Null);
}

#[test]
#[cfg(feature = "multi_line_comment")]
fn multi_line_comments() {
    let src = r#"
        fn enterprise_grade() {
            /* hello */
            let /* hi */ a /* hi */ = /* hi */ null;
            /* foo
               bar */
        }
    "#;

    assert_execute(src, "enterprise_grade", &mut [], Value::Null);
}

#[test]
#[cfg(all(feature = "single_line_comment", feature = "multi_line_comment"))]
fn comments() {
    let src = r#"
        fn enterprise_grade() {
            // hello
            /* world */
            let a = null;
            /* foo
               bar */
        }
    "#;

    assert_execute(src, "enterprise_grade", &mut [], Value::Null);
}

#[test]
#[cfg(feature = "i32_type")]
fn deep() {
    for n in (1..=50).step_by(3) {
        let mut deep = String::new();
        deep.push_str(&"{".repeat(n));
        deep.push_str("42");
        deep.push_str(&"}".repeat(n));
        let src = format!(
            r#"
            fn deep() {{
                {deep}
            }}
        "#
        );

        let start = Instant::now();
        assert_execute(&src, "deep", &mut [], Value::I32(42));
        let time = start.elapsed();
        println!("{n}, {:.2}", time.as_secs_f32());
    }
}

#[test]
fn parse_error() {
    let src = r#"
        fn foobar(a, b, c) {
            baz(a, b, c
        }
    "#;

    let expected = r#"
0: at line 4:
        }
        ^
expected ')', found }
    "#;

    let err = Parser::new().parse(src).unwrap_err();
    assert_eq!(err.to_string().trim(), expected.trim());
}
