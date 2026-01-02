# ToyBEAM

A toy BEAM virtual machine in Rust that compiles to WebAssembly.

## Goal

Build a Rust-like syntax language that compiles to ToyBEAM bytecode, combining Rust's syntax with Erlang's concurrency semantics (processes, message passing, pattern matching).

## Architecture

```
Source Code → Lexer → Parser → AST → Codegen → ToyBEAM Bytecode
                                                      ↓
                                              ToyBEAM VM (Rust/WASM)
```

## Current VM Features

- **Processes**: spawn, spawn_link, parent tracking
- **Message Passing**: send, receive, receive_timeout, receive_match (selective)
- **Links & Monitors**: bidirectional crash notification, one-way DOWN messages
- **Process Registry**: register/unregister/whereis named processes
- **Data Types**: integers, atoms, tuples, lists, strings, PIDs
- **Pattern Matching**: wildcards, variables, literals, tuple/list destructuring
- **Control Flow**: jump, jump_if, jump_unless, call, return
- **Stack**: push, pop operations
- **Arithmetic**: add, sub, mul, div, mod
- **Comparisons**: eq, ne, lt, lte, gt, gte

## Building

```bash
# Run tests
cargo test

# Build WASM
wasm-pack build --target web
```

## Commands

- `cargo test` - run all tests
- `cargo check` - type check
- `wasm-pack build --target web` - build WASM package

## Code Structure

- `src/lib.rs` - public API exports
- `src/instruction.rs` - bytecode instruction definitions
- `src/value.rs` - runtime value types
- `src/process.rs` - process state
- `src/scheduler.rs` - VM execution engine
- `src/message.rs` - message types (user and system)
- `src/pid.rs` - process identifier
- `src/wasm.rs` - WASM/JS bindings
