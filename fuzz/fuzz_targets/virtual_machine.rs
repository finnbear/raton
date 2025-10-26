#![no_main]
use libfuzzer_sys::fuzz_target;
use raton::{bytecode::ProgramBytecode, prelude::*};

fuzz_target!(|program: ProgramBytecode| {
    target(program);
});

fn target(program: ProgramBytecode) {
    let mut vm = VirtualMachine::new(&program)
        .with_type_casting()
        .with_max_instructions(10000)
        .with_max_stack_depth(10);

    for func in program
        .public_functions
        .keys()
        .cloned()
        .collect::<Vec<String>>()
    {
        let _ = vm.call1(&func, Value::I32(5));
    }
}
