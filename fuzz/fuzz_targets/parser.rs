#![no_main]
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
        .with_max_instructions(1000)
        .generate_program(&ast)
    {
        Ok(p) => p,
        Err(_err) => {
            // TODO.
            return;
        }
    };
    let mut vm = VirtualMachine::new(&program)
        .with_type_casting()
        .with_instruction_budget(200)
        .with_max_stack_depth(10);

    for func in ast
        .functions
        .iter()
        .map(|f| f.identifier.clone())
        .collect::<Vec<_>>()
    {
        let _ = vm.execute(&func, &[Value::I32(5)]);
    }
}
