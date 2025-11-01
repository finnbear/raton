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
// test benches::overhead           ... bench:          45.71 ns/iter (+/- 2.51)
// test benches::fib_28             ... bench: 372,114,652.20 ns/iter (+/- 33,355,530.29)
// test benches::million_iterations ... bench:  53,791,145.80 ns/iter (+/- 4,036,985.22)

// Oct 29 2025 (after adding receivers)
// test benches::fib_28             ... bench: 397,696,071.90 ns/iter (+/- 35,402,116.10)
// test benches::million_iterations ... bench:  57,300,537.00 ns/iter (+/- 9,191,647.60)
// test benches::overhead           ... bench:          45.19 ns/iter (+/- 6.66)

// Oct 31 2025 (after finishing receivers)
// test benches::fib_28             ... bench: 388,137,459.00 ns/iter (+/- 61,098,549.25)
// test benches::fn_call_overhead   ... bench:          44.78 ns/iter (+/- 27.50)
// test benches::million_iterations ... bench:  55,008,289.20 ns/iter (+/- 5,612,928.22)
// test benches::new_vm_overhead    ... bench:         201.80 ns/iter (+/- 7.73)

// Oct 31 2025 (after using BTreeMap for ProgramBytecode)
// test benches::fib_28             ... bench: 408,173,838.70 ns/iter (+/- 53,866,307.14)
// test benches::fn_call_overhead   ... bench:          40.12 ns/iter (+/- 0.29)
// test benches::host_call_overhead ... bench:          58.48 ns/iter (+/- 0.37)
// test benches::million_iterations ... bench:  56,130,105.00 ns/iter (+/- 3,751,664.67)
// test benches::new_vm_overhead    ... bench:         182.75 ns/iter (+/- 12.17)

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
    let mut vm = VirtualMachine::new(&program)
        .with_type_casting()
        .with_max_instructions(None)
        .with_max_stack_depth(None);

    b.iter(move || {
        let result = black_box(&mut vm)
            .call(black_box(func), black_box(args))
            .unwrap();
        assert_eq!(result, expected);
    })
}

#[bench]
#[cfg(feature = "bool_type")]
fn fn_call_overhead(b: &mut Bencher) {
    // Make sure there are multiple items in the public function map,
    // so the lookup isn't trivial.
    let src = r#"
        fn simple(a) {
            a
        }

        fn decoy1(a) {
            !a
        }

        fn decoy2(a) {
            !a
        }
    "#;

    bench_execute(
        b,
        src,
        "simple",
        &mut [RuntimeValue::Value(Value::Bool(true))],
        RuntimeValue::Value(Value::Bool(true)),
    );
}

#[bench]
#[cfg(all(feature = "bool_type", feature = "i32_type"))]
fn host_call_overhead(b: &mut Bencher) {
    let src = r#"
        fn simple(a) {
            i32(a)
        }
    "#;

    bench_execute(
        b,
        src,
        "simple",
        &mut [RuntimeValue::Value(Value::Bool(true))],
        RuntimeValue::Value(Value::I32(1)),
    );
}

#[bench]
#[cfg(feature = "bool_type")]
fn new_vm_overhead(b: &mut Bencher) {
    let src = r#"
        fn simple(a) {
            a
        }
    "#;

    let ast = Parser::new().parse(src).unwrap();
    let program = CodeGenerator::new().generate_program(&ast).unwrap();

    b.iter(move || {
        let mut vm = VirtualMachine::new(&program)
            .with_type_casting()
            .with_max_instructions(None)
            .with_max_stack_depth(None);

        let result = black_box(&mut vm)
            .call(
                black_box("simple"),
                black_box(&mut [RuntimeValue::Value(Value::Bool(true))]),
            )
            .unwrap();
        assert_eq!(result, RuntimeValue::Value(Value::Bool(true)));
    });
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
