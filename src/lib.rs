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
    use super::*;

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
                    return a;
                } else {
                    return b;
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
}
