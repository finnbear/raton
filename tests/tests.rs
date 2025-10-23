use raton::*;
#[allow(unused_imports)]
use std::time::Instant;

fn assert_execute(src: &str, func: &str, args: Vec<Value>, expected: Value) {
    let ast = Parser::new().parse(src).unwrap();
    let mut vm = Vm::new().with_type_casting();
    vm.load_program(&ast).unwrap();

    let result = vm.execute(func, args).unwrap();
    assert_eq!(result, expected);
}

#[test]
fn minimal() {
    let src = r#"
        fn hello() {}
    "#;

    assert_execute(src, "hello", vec![], Value::Null);
}

#[test]
#[cfg(feature = "f32_type")]
fn simple_function() {
    let src = r#"
        fn to_float(a) {
            return f32(a);
        }
    "#;

    assert_execute(src, "to_float", vec![Value::I32(5)], Value::F32(5.0));
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
        vec![Value::I32(5), Value::I32(3)],
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
        vec![Value::I32(5), Value::I32(3)],
        Value::I32(5),
    );
    assert_execute(
        src,
        "max",
        vec![Value::I32(3), Value::I32(5)],
        Value::I32(5),
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

    assert_execute(src, "sum_to_n", vec![Value::I32(5)], Value::I32(15));
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

    assert_execute(src, "enterprise_grade", vec![], Value::Null);
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

    assert_execute(src, "enterprise_grade", vec![], Value::Null);
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

    assert_execute(src, "enterprise_grade", vec![], Value::Null);
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
        assert_execute(&src, "deep", vec![], Value::I32(42));
        let time = start.elapsed();
        println!("{n}, {:.2}", time.as_secs_f32());
    }
}
