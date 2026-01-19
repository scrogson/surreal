# Dream

A programming language with Rust-like syntax and Erlang-style concurrency.

## Goal

Combine Rust's familiar syntax with Erlang's battle-tested concurrency model (processes, message passing, pattern matching).

## Compilation Targets

### BEAM (via Core Erlang)
```
Source → Lexer → Parser → AST → Core Erlang → BEAM
```

### WebAssembly (via Dream VM)
```
Source → Lexer → Parser → AST → Dream Bytecode → Dream VM (Rust/WASM)
```

## Language Features

- **Processes**: spawn, spawn_link, self()
- **Message Passing**: send (`!`), receive, receive with timeout
- **Links & Monitors**: bidirectional crash notification, one-way DOWN messages
- **Process Registry**: register/unregister/whereis named processes
- **Data Types**: integers, atoms, tuples, lists, strings, PIDs, binaries
- **Pattern Matching**: wildcards, variables, literals, tuple/list/struct destructuring, guards
- **Structs & Enums**: Rust-style type definitions
- **Traits**: trait definitions and implementations
- **Pipe Operator**: `|>` for function chaining

## Building

```bash
# Run tests
cargo test

# Build the compiler
cargo build --release

# Build WASM target
wasm-pack build --target web
```

## Commands

- `cargo test` - run all tests
- `cargo check` - type check
- `cargo build --release` - build compiler
- `wasm-pack build --target web` - build WASM package

## Dependency Bindings System

Dream can interoperate with Erlang and Elixir libraries through **bindings** - type stub files that declare external module interfaces.

### Why Bindings Are Needed

When Dream code calls external Erlang/Elixir functions like `jason::encode(data, [])`, the compiler needs to know:
1. That `jason` is an external module (not a Dream module)
2. The actual Erlang/Elixir module name to call (e.g., `'Elixir.Jason'`)
3. The function signatures for type checking

Without bindings, calls like `jason::encode` would compile to `'surreal::project::jason':encode/2` which doesn't exist.

### Generating Bindings

After fetching dependencies with `surrealc deps get`, generate bindings:

```bash
surrealc deps bindgen
```

This creates `.surt` files in `_build/bindings/` by scanning dependency source files (`.erl` and `.ex` files) and extracting:
- Module names
- Function specs/signatures
- Type definitions
- Struct definitions

### The `.surt` File Format

Generated bindings use `extern mod` declarations with a `#[name]` attribute:

```surreal
// _build/bindings/jason.surt
#[name = "Elixir.Jason"]
extern mod jason {
    fn decode(arg0: Any, arg1: [Any]) -> Result<Any, Any>;
    fn encode(arg0: Any, arg1: [Any]) -> Result<String, Any>;
    #[name = "encode!"]
    fn encode_bang(arg0: Any, arg1: [Any]) -> Any;
}
```

Key elements:
- `#[name = "Elixir.Jason"]` - The actual Erlang atom for the module
- `extern mod jason` - The Dream name used in code (`jason::encode`)
- `#[name = "encode!"]` - Maps Dream-safe names to Erlang names with special chars

### Using Bindings in Dream Code

1. **Declare the module** in your `lib.sur`:
   ```surreal
   mod jason;  // Loads from _build/bindings/jason.surt
   ```

2. **Call functions** using the Dream module name:
   ```surreal
   // Compiles to 'Elixir.Jason':encode(data, [])
   let result = jason::encode(data, []);
   ```

### Troubleshooting

**"undefined function 'surreal::project::module':function/N"**
- Bindings weren't generated. Run `surrealc deps bindgen`

**"cannot find module `module`"**
- The `.surt` file doesn't exist in `_build/bindings/`
- Run `surrealc deps bindgen` to generate it

### Manual Extern Declarations

For one-off external calls without generating full bindings, use the `:` prefix:

```surreal
// Direct extern call - no binding needed
let result = :erlang::system_info(:process_count);
let json = :"Elixir.Jason"::encode(data);
```

---

# Compiler Internals

## Module Resolution: Stdlib vs Extern

Dream has two types of module calls that look similar but compile differently:

1. **Stdlib module calls** (`io::println`, `logger::info`) - Compile to `'surreal::io':'println'`
2. **Direct extern calls** (`:io::format`, `:logger::info`) - Compile to `'io':'format'` (Erlang directly)

### How It Works

The typechecker's `annotate_expr` function (in `typeck.rs`) transforms calls based on these rules:

```
module::func(args)    →  If module is in STDLIB_MODULES: stays as Path, gets surreal:: prefix
                         If module is extern mod: transforms to ExternCall

:module::func(args)   →  Always ExternCall (direct Erlang/Elixir call)
```

### The STDLIB_MODULES List

Both `typeck.rs` and `core_erlang.rs` have a `STDLIB_MODULES` constant that lists Dream stdlib modules:

```rust
const STDLIB_MODULES: &'static [&'static str] = &[
    "io", "list", "enumerable", "iterator", "option", "result",
    "string", "map", "file", "timer", "display", "convert",
    "process", "genserver", "supervisor", "application", "logger",
];
```

**When adding a new stdlib module**, update BOTH files.

### Common Bug Pattern

If a stdlib module also has an extern binding (e.g., `stdlib/erlang/std/io.sur` defines `extern mod io`), the typechecker might incorrectly treat `io::println` as an extern call. The fix is to check `is_stdlib_module()` BEFORE checking `is_extern_module()`.

### Debugging Module Resolution

1. Build with `--target core` to see generated Core Erlang:
   ```bash
   surrealc build project --target core
   ```

2. Check the output for correct module prefixes:
   - `call 'surreal::io':'println'` ✓ (stdlib call)
   - `call 'io':'println'` ✗ (wrong - would fail at runtime)

3. Add debug output to `core_erlang.rs` emit functions to trace code paths.

## Testing Compiler Changes

Tests for code generation live in `src/compiler/core_erlang.rs` in the `mod tests` section:

```rust
#[test]
fn test_stdlib_module_call_gets_surreal_prefix() {
    let source = r#"
        mod test {
            pub fn log(msg: String) -> Atom {
                io::println(msg)
            }
        }
    "#;
    let result = emit_core_erlang(source).unwrap();
    assert!(result.contains("call 'surreal::io':'println'"));
}
```

Use `emit_core_erlang()` for simple tests, `emit_core_erlang_with_typecheck()` when you need type information.

## Stdlib Build Artifacts

The stdlib is precompiled to `target/stdlib/` when the compiler is built. **These files can become stale** if you modify stdlib source files.

### Rebuilding Stdlib

After modifying any stdlib source files, rebuild the stdlib:

```bash
just stdlib
```

This command:
1. Builds all `stdlib/*.sur` files
2. Copies the resulting `.beam` files to `target/stdlib/`

**IMPORTANT**: The typechecker loads stdlib source files directly for type information, but runtime execution uses the `.beam` files in `target/stdlib/`. Both must be in sync.

### Key Points

1. **Macro expander uses `target/stdlib/`**: When derive macros run, they load modules like `surreal::syn` from `target/stdlib/`. If these .beam files are stale, macros will malfunction.

2. **Building standalone stdlib files outputs to current directory**: Running `surrealc build stdlib/syn.sur` creates `surreal::syn.beam` in the *current directory*, NOT in `target/stdlib/`.

3. **After modifying stdlib, use `just stdlib`** (preferred) or manually copy .beam files:
   ```bash
   surrealc build stdlib/syn.sur
   cp "surreal::syn.beam" target/stdlib/
   ```

### Symptoms of Stale Stdlib

- Derive macros return unexpected values (e.g., `unknown` instead of struct name)
- Runtime errors from stdlib functions that should work
- Debug output in stdlib code doesn't appear (old .beam doesn't have it)

### Debugging Stale Stdlib Issues

1. Check timestamps:
   ```bash
   ls -la target/stdlib.sur::syn.beam stdlib/syn.sur
   ```

2. Force rebuild and copy:
   ```bash
   touch stdlib/syn.sur
   surrealc build stdlib/syn.sur
   cp "surreal::syn.beam" target/stdlib/
   ```

3. Test directly with erl:
   ```bash
   erl -noshell -pa target/stdlib -eval "
   {module, _} = code:load_file('surreal::syn'),
   % test your function
   halt()."
   ```

---

# Agent Instructions

This project uses **bd** (beads) for issue tracking. Run `bd onboard` to get started.

## Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --status in_progress  # Claim work
bd close <id>         # Complete work
bd sync               # Sync with git
```

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd sync
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds

## Committing Code

**ALWAYS get user approval before committing.** The user wants to validate code changes before they are committed.

1. After making changes, show the user what changed (git status, git diff summary)
2. Ask the user to review and approve before committing
3. Only commit after explicit user approval
4. NEVER auto-commit without asking first

## Commit Signing

**NEVER bypass commit signing.** All commits in this repository must be signed.

- Always use `git commit -S` to sign commits
- If the signing process times out or hangs (e.g., waiting for 1Password), **WAIT for the user** - do NOT use `--no-gpg-sign` or any other workaround
- The user will resolve signing issues manually if needed
- An unsigned commit is worse than waiting - it breaks the trust chain
