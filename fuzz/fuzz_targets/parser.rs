#![no_main]
use std::rc::Rc;

use libfuzzer_sys::fuzz_target;
use raton::prelude::*;

fuzz_target!(|src: &str| {
    target(src);
});

fn target(src: &str) {
    let ast = match Parser::new().parse(src) {
        Ok(ast) => ast,
        Err(_errs) => {
            // TODO.
            return;
        }
    };
    let program = match CodeGenerator::new()
        .with_max_instructions(10_000)
        .generate_program(&ast)
    {
        Ok(p) => p,
        Err(err) => {
            if matches!(err, CompileError::Internal) {
                panic!("ICE");
            }
            return;
        }
    };

    let a = Rc::new(10u8);
    let b = 5u16;
    let mut c = 3u32;

    if let Some(func) = ast.functions.iter().map(|f| f.identifier.clone()).next() {
        let mut vm = VirtualMachine::new(&program)
            .with_type_casting()
            .with_max_instructions(10000)
            .with_max_stack_depth(10)
            .with_host_function(
                "host_call",
                |_: ExternValue<u8>, _: ExternRef<u16>, _: ExternMut<u32>| Ok(Value::Null),
            )
            .with_host_function(
                "host_call2",
                |_: ExternValue<u16>, _: ExternRef<u32>, _: ExternMut<u64>| Ok(Value::Null),
            )
            .with_host_function("host_call", |_: Receiver<ExternValue<u8>>| Ok(Value::Null))
            .with_host_function("host_call", |_: Receiver<ExternValue<u16>>| Ok(Value::Null))
            .with_host_function("host_call", |_: Receiver<ExternRef<u8>>| Ok(Value::Null))
            .with_host_function("host_call", |_: Receiver<ExternRef<u16>>| Ok(Value::Null))
            .with_host_function("host_call", |_: Receiver<ExternMut<u32>>| Ok(Value::Null))
            .with_host_function("host_call", |_: Receiver<ExternMut<u16>>| Ok(Value::Null));

        let result = vm.call3(
            &func,
            ExternValue(Rc::clone(&a)),
            ExternRef(&b),
            ExternMut(&mut c),
        );
        if matches!(
            result,
            Err(RuntimeError::StackUnderflow
                | RuntimeError::IllegalInstruction
                | RuntimeError::BytecodeEndedAbruptly)
        ) {
            panic!("IRE {:?}", result);
        }
    }
}
