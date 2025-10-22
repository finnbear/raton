# Ratón 🐁

[![Build](https://github.com/finnbear/raton/actions/workflows/build.yml/badge.svg)](https://github.com/finnbear/raton/actions/workflows/build.yml) 

A tiny, embeddable, dynamically typed scripting language with a bytecode VM, intended for use in games.

## Example

```rust
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
- [x] Bytecode VM
- [ ] Optimizer
- [ ] Standard library

## Types
- [x] `()`
- [x] `bool`
- [x] `i32`
- [x] `f32`
- [x] `string`
- [ ] Host value
- [ ] Host reference

## Other features

- [ ] Denial of service prevention
- [ ] Proper error handling
- [ ] Proper error messages
- [ ] Fuzzing

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