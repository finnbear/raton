# Ratón 🐁

[![Documentation](https://docs.rs/raton/badge.svg)](https://docs.rs/raton)
[![crates.io](https://img.shields.io/crates/v/raton.svg)](https://crates.io/crates/raton)
[![Build](https://github.com/finnbear/raton/actions/workflows/build.yml/badge.svg)](https://github.com/finnbear/raton/actions/workflows/build.yml) 

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

## Status

Unfinished, do not use in production.

## Components
- [x] Parser
- [x] Bytecode generator
- [ ] Bytecode linker
- [x] Bytecode VM
- [ ] Optimizer
- [ ] Modular standard library

## Modular type system
- [x] `null`
- [x] `bool` (optional `bool_type` feature)
- [x] `i32` (optional `i32_type` feature)
- [x] `f32` (optional `f32_type` feature)
- [x] `string` (optional `string_type` feature)
- [ ] Host value (optional)
- [ ] Host reference (optional)

## Modular language features
- [x] `if`, `else` (optional `if_expression` feature)
- [x] `while`, `break`, `continue` (optional `while_loop` feature)
- [x] `//` comments (optional `single_line_comment` feature)
- [x] `/* */` comments (optional `multi_line_comment` feature)

## Serialization
- [x] `serde` (ast and bytecode)
- [x] `bitcode` (bytecode only)

## Other features

- [x] Portable to any platform, 32 bits or higher, supported by Rust
- [x] Fuzzing (`cargo fuzz run fuzz -- -timeout=5`)
- [x] `#![forbid(unsafe_code)]`, also validated in Miri
- [ ] `no_std`
- [ ] Denial of service prevention
- [ ] Proper error handling
- [ ] Proper error messages

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