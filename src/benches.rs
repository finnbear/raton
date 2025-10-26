use crate::prelude::*;
extern crate test;
use test::{black_box, Bencher};

// Oct 23 2025
// test benches::fib_28             ... bench: 149,497,373.70 ns/iter (+/- 9,979,934.13)
// test benches::million_iterations ... bench:  44,039,739.20 ns/iter (+/- 10,533,766.51)

// Oct 24 2025
// test benches::fib_28             ... bench: 137,125,511 ns/iter (+/- 19,572,563)
// test benches::million_iterations ... bench:  44,711,787 ns/iter (+/- 5,496,146)

// Oct 24 2025 (after custom stack)
// test benches::fib_28             ... bench:  80,010,209.00 ns/iter (+/- 13,744,648.44)
// test benches::million_iterations ... bench:  44,463,605.80 ns/iter (+/- 5,599,272.26)

// Oct 24 2025 (after custom stack optimized)
// test benches::fib_28             ... bench:  58,545,572.00 ns/iter (+/- 10,114,819.82)
// test benches::million_iterations ... bench:  42,496,642.50 ns/iter (+/- 8,920,702.95)

// Oct 24 2025 (after custom stack further optimized)
// test benches::fib_28             ... bench:  57,441,287.50 ns/iter (+/- 10,581,056.86)
// test benches::million_iterations ... bench:  39,619,527.30 ns/iter (+/- 7,884,078.85)

// Oct 24 2025 (after looping fib_28 5 times)
// test benches::fib_28             ... bench: 305,698,143.70 ns/iter (+/- 88,536,045.25)
// test benches::million_iterations ... bench:  41,642,978.90 ns/iter (+/- 9,144,916.79)

// Oct 25 2025 (after adding extern)
// test benches::fib_28             ... bench: 391,909,564.60 ns/iter (+/- 48,836,435.46)
// test benches::million_iterations ... bench:  54,902,119.30 ns/iter (+/- 6,247,312.49)

// Oct 25 2025 (after optimizing)
// test benches::fib_28             ... bench: 372,114,652.20 ns/iter (+/- 33,355,530.29)
// test benches::million_iterations ... bench:  53,791,145.80 ns/iter (+/- 4,036,985.22)

#[allow(unused)]
fn bench_execute<'a>(
    b: &mut Bencher,
    src: &str,
    func: &str,
    args: &'a mut [RuntimeValue<'a>],
    expected: RuntimeValue<'a>,
) {
    let ast = Parser::new().parse(src).unwrap();
    let program = CodeGenerator::new().generate_program(&ast).unwrap();

    b.iter(move || {
        // Hack.
        let mut values = args
            .iter()
            .map(|v| {
                if let RuntimeValue::Value(v) = v {
                    RuntimeValue::<'static>::Value(v.clone())
                } else {
                    unimplemented!();
                }
            })
            .collect::<Vec<_>>();
        let mut vm = VirtualMachine::new(&program).with_type_casting();

        let result = black_box(&mut vm)
            .call(black_box(func), black_box(&mut values))
            .unwrap();
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

    bench_execute(b, src, "million", &mut [], RuntimeValue::Value(Value::Null));
}

#[bench]
#[cfg(all(
    feature = "if_expression",
    feature = "while_loop",
    feature = "i32_type"
))]
fn fib_28(b: &mut test::Bencher) {
    let src = r#"
        fn fib(n) {
            if n < 2 {
                n
            } else {
                fib(n-1) + fib(n-2)
            }
        }

        fn five(n) {
            let i = 0;
            let ret = 0;
            while (i < 5) {
                ret = fib(n);
                i = i + 1;
            }
            ret
        }
    "#;

    bench_execute(
        b,
        src,
        "five",
        &mut [RuntimeValue::Value(Value::I32(28))],
        RuntimeValue::Value(Value::I32(317811)),
    );
}
