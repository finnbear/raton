use crate::prelude::*;
extern crate test;
use test::Bencher;

// Oct 23 2025
// test benches::fib_28             ... bench: 149,497,373.70 ns/iter (+/- 9,979,934.13)
// test benches::million_iterations ... bench:  44,039,739.20 ns/iter (+/- 10,533,766.51)

// Oct 24 2025
// test benches::fib_28             ... bench: 137,125,511 ns/iter (+/- 19,572,563)
// test benches::million_iterations ... bench:  44,711,787 ns/iter (+/- 5,496,146)

#[allow(unused)]
fn bench_execute(b: &mut Bencher, src: &str, func: &str, args: Vec<Value>, expected: Value) {
    let ast = Parser::new().parse(src).unwrap();
    let program = CodeGenerator::new().generate_program(&ast).unwrap();
    let mut vm = VirtualMachine::new(&program).with_type_casting();

    b.iter(|| {
        let result = vm.execute(func, args.clone()).unwrap();
        assert_eq!(result, expected);
    })
}

#[bench]
#[cfg(all(feature = "while_loop", feature = "i32_type"))]
fn million_iterations(b: &mut Bencher) {
    let src = r#"
        fn million() {
            let x = 1000000;
            while x > 0 {
                x = x - 1;
            }
        }
    "#;

    bench_execute(b, src, "million", vec![], Value::Null);
}

#[bench]
#[cfg(all(feature = "if_expression", feature = "i32_type"))]
fn fib_28(b: &mut test::Bencher) {
    let src = r#"
        fn fib(n) {
            if n < 2 {
                n
            } else {
                fib(n-1) + fib(n-2)
            }
        }
    "#;

    bench_execute(b, src, "fib", vec![Value::I32(28)], Value::I32(317811));
}
