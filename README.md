# Ratón 🐁

[![Documentation](https://docs.rs/raton/badge.svg)](https://docs.rs/raton)
[![crates.io](https://img.shields.io/crates/v/raton.svg)](https://crates.io/crates/raton)
[![Build](https://github.com/finnbear/raton/actions/workflows/build.yml/badge.svg)](https://github.com/finnbear/raton/actions/workflows/build.yml)
[![unsafe forbidden](https://img.shields.io/badge/unsafe-forbidden-success.svg)](https://github.com/rust-secure-code/safety-dance/)

A tiny, highly modular, embeddable, dynamically typed scripting language with a bytecode VM, intended for use in games.

## Example

```toml
[dependences]
raton = {
  version = "0",
  features = ["i32_type", "while_loop", "single_line_comment"],
  default-features = false
}
```
```rust
// Add up all integers from 1 to n
fn sum_to_n(n) {
    let i = 0;
    let sum = 0;
    while (i < n) {
        i = i + 1;
        sum = sum + i;
    }
    return sum;
}
```

## Components
- [x] Parser
- [x] Bytecode generator
- [x] Bytecode VM
- [ ] Optimizer
- [ ] Debugger
- [ ] Modular standard library

## Modular type system
- [x] `null`
- [x] `bool` (optional `bool_type` feature)
- [x] `i32` (optional `i32_type` feature)
- [x] `f32` (optional `f32_type` feature)
- [x] `string` (optional `string_type` feature)
- [x] Host value (optional `extern_value` feature)
- [x] Host immutable/mutable reference

## Modular language features
- [x] `if`, `else` (optional `if_expression` feature)
- [x] `.method()` (optional `method_call_expression` feature)
- [x] `while`, `break`, `continue` (optional `while_loop` feature)
- [x] `//` comments (optional `single_line_comment` feature)
- [x] `/* */` comments (optional `multi_line_comment` feature)

## Serialization
- [x] `serde` (ast and bytecode)
- [x] `bitcode` (bytecode only)

## Other features

- [x] Portable to any platform, 32 bits or higher, supported by Rust
- [x] Parsing, code generation, and runtime have configurable limits
- [x] Error handling and reporting
- [x] Optional error if an operation produces `f32::NaN`
- [ ] `no_std`

## Performance

Creating a new VM with several host functions requires ~2 heap allocations and has
an overhead on the order of 150ns.

Calling a function on a created VM requires ~0 heap allocatinos and has an
overhead on the order of 50ns.

Ratón takes ~0.4s on `Fibonacci` and ~0.05s on `1M Loop` on an i7-7700k (see [Rhai benchmarks](https://rhai.rs/book/about/benchmarks.html)).

## Security

Ratón is designed to handle untrusted or malicious source code, asts, or bytecode
without panicking, exhausting memory, memory unsafety, exponential time complexity,
or infinite loop. Unsafe code is forbidden, and each component has a fuzzer that
tests it against arbitrary inputs.

You are responsible for using the limits, such as on instructions and call stack
depth, that it provides.

## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.