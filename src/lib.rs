pub mod ast;
mod value;
pub use value::*;
mod parser;
pub use parser::*;
pub mod bytecode;
mod bytecode_generator;
pub use bytecode_generator::*;
mod bytecode_vm;
pub use bytecode_vm::*;

#[cfg(test)]
mod tests {
    use std::time::Instant;

    use super::*;

    #[test]
    fn test_simpler() {
        let source = r#"
            fn hello(a) { }
        "#;

        let ast = Parser::new().parse(source).unwrap();
        let mut vm = Vm::new().with_type_casting();
        vm.load_program(&ast).unwrap();

        let result = vm.execute("hello", vec![Value::Null]).unwrap();
        assert_eq!(result, Value::Null);
    }

    #[test]
    fn test_simple() {
        let source = r#"
            fn hello(a) {
                return f32(a);
            }
        "#;

        let ast = Parser::new().parse(source).unwrap();
        let mut vm = Vm::new().with_type_casting();
        vm.load_program(&ast).unwrap();

        let result = vm.execute("hello", vec![Value::I32(5)]).unwrap();
        assert_eq!(result, Value::F32(5.0));
    }

    /*
    #[test]
    fn test_linear_time() {
        let source = include_str!("../fuzz/artifacts/fuzz/timeout-992fbd5b80652eec3a99fdbac2b76916d7a8df34");

        let _ = Parser::new().parse(source);
    }
    */

    #[test]
    fn test_simple_math() {
        let source = r#"
            fn add(a, b) {
                return a + b;
            }
        "#;

        let ast = Parser::new().parse(source).unwrap();
        let mut vm = Vm::new();
        vm.load_program(&ast).unwrap();

        let result = vm
            .execute("add", vec![Value::I32(5), Value::I32(3)])
            .unwrap();
        assert_eq!(result, Value::I32(8));
    }

    #[test]
    fn test_if_expression() {
        let source = r#"
            fn max(a, b) {
                if (a > b) {
                    return /*bigger*/ a;
                } else {
                    return /* smaller */ b;
                }
            }
        "#;

        let ast = Parser::new().parse(source).unwrap();
        let mut vm = Vm::new();
        vm.load_program(&ast).unwrap();

        let result = vm
            .execute("max", vec![Value::I32(10), Value::I32(5)])
            .unwrap();
        assert_eq!(result, Value::I32(10));
    }

    #[test]
    fn test_while_loop() {
        let source = r#"
            fn sum_to_n(n) {
                let i = 0;
                let sum = 0;
                // iterate n times
                while (i < n) {
                    i = i + 1;
                    sum = sum + i;
                }
                return sum;
            }
        "#;

        let ast = Parser::new().parse(source).unwrap();
        let mut vm = Vm::new();
        vm.load_program(&ast).unwrap();

        let result = vm.execute("sum_to_n", vec![Value::I32(5)]).unwrap();
        assert_eq!(result, Value::I32(15));
    }

    #[test]
    fn test_deep() {
        for n in (1..=19).step_by(3) {
            let mut deep = String::new();
            deep.push_str(&"{".repeat(n));
            deep.push_str("42");
            deep.push_str(&"}".repeat(n));
            let source = format!(
                r#"
                fn deep() {{
                    // deep
                    {deep}
                }}
            "#
            );

            let start = Instant::now();
            let ast = Parser::new().parse(&source).unwrap();
            let time = start.elapsed();
            println!("{deep} {n}, {:.2}", time.as_secs_f32());

            let mut vm = Vm::new();
            vm.load_program(&ast).unwrap();

            let result = vm.execute("deep", vec![Value::I32(5)]).unwrap();
            assert_eq!(result, Value::I32(42));
        }
    }

    #[test]
    fn test_threading() {
        let source = r#"
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

        let mut threads = Vec::new();
        for _ in 0..5 {
            threads.push(std::thread::spawn(move || {
                let ast = Parser::new().parse(source).unwrap();
                let mut vm = Vm::new();
                vm.load_program(&ast).unwrap();

                let result = vm.execute("sum_to_n", vec![Value::I32(5)]).unwrap();
                assert_eq!(result, Value::I32(15));
            }));
        }

        for thread in threads {
            thread.join().unwrap();
        }
    }
}
