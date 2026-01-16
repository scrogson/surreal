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

Without bindings, calls like `jason::encode` would compile to `'dream::project::jason':encode/2` which doesn't exist.

### Generating Bindings

After fetching dependencies with `dream deps get`, generate bindings:

```bash
dream deps bindgen
```

This creates `.dreamt` files in `_build/bindings/` by scanning dependency source files (`.erl` and `.ex` files) and extracting:
- Module names
- Function specs/signatures
- Type definitions
- Struct definitions

### The `.dreamt` File Format

Generated bindings use `extern mod` declarations with a `#[name]` attribute:

```dream
// _build/bindings/jason.dreamt
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

1. **Declare the module** in your `lib.dream`:
   ```dream
   mod jason;  // Loads from _build/bindings/jason.dreamt
   ```

2. **Call functions** using the Dream module name:
   ```dream
   // Compiles to 'Elixir.Jason':encode(data, [])
   let result = jason::encode(data, []);
   ```

### Troubleshooting

**"undefined function 'dream::project::module':function/N"**
- Bindings weren't generated. Run `dream deps bindgen`

**"cannot find module `module`"**
- The `.dreamt` file doesn't exist in `_build/bindings/`
- Run `dream deps bindgen` to generate it

### Manual Extern Declarations

For one-off external calls without generating full bindings, use the `:` prefix:

```dream
// Direct extern call - no binding needed
let result = :erlang::system_info(:process_count);
let json = :"Elixir.Jason"::encode(data);
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
