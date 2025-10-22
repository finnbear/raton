#![no_main]
use libfuzzer_sys::fuzz_target;
use raton::*;
use std::sync::LazyLock;

fuzz_target!(|src: &str| {
    target(src);
});

fn target(src: &str) {
    static PARSER: LazyLock<Parser> = LazyLock::new(|| Parser::new());
    let ast = match PARSER.parse(src) {
        Ok(ast) => ast,
        Err(_errs) => {
            // TODO.
            return;
        }
    };
    let mut vm = Vm::new()
        .with_type_casting()
        .with_instruction_budget(200)
        .with_max_stack_depth(10);
    match vm.load_program(&ast) {
        Ok(_) => {}
        Err(_err) => {
            // TODO.
            return;
        }
    }

    for func in ast
        .functions
        .iter()
        .map(|f| f.name.clone())
        .collect::<Vec<_>>()
    {
        let _ = vm.execute(&func, vec![Value::I32(5)]);
    }
}
