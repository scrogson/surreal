# Dream Language Justfile

# Default recipe - show available commands
default:
    @just --list

# Build the compiler (release mode)
build:
    cargo build --release

# Build the compiler (debug mode)
build-debug:
    cargo build

# Run all tests
test:
    cargo test

# Type check without building
check:
    cargo check

# Run a Dream file
run file:
    cargo run --release -- run {{file}}

# Generate stubs for a single Erlang module
bindgen file output:
    cargo run --release -- bindgen {{file}} --output {{output}}

# Erlang source directories
erl_root := `erl -noshell -eval 'io:format("~s", [code:root_dir()])' -s init stop`
stdlib_src := erl_root / "lib/stdlib-*/src"
erts_src := erl_root / "lib/erts-*/src"

# Regenerate all stubs from Erlang source
stubs: build
    #!/usr/bin/env bash
    set -euo pipefail
    ERL_ROOT=$(erl -noshell -eval 'io:format("~s", [code:root_dir()])' -s init stop)
    STDLIB=$(echo "$ERL_ROOT"/lib/stdlib-*/src)
    ERTS=$(echo "$ERL_ROOT"/lib/erts-*/src)
    KERNEL=$(echo "$ERL_ROOT"/lib/kernel-*/src)

    echo "Generating stubs from $ERL_ROOT..."
    ./target/release/dream bindgen "$ERTS/erlang.erl" --output stubs/erlang.dreamt
    ./target/release/dream bindgen "$STDLIB/lists.erl" --output stubs/lists.dreamt
    ./target/release/dream bindgen "$STDLIB/maps.erl" --output stubs/maps.dreamt
    ./target/release/dream bindgen "$STDLIB/io.erl" --output stubs/io.dreamt
    ./target/release/dream bindgen "$KERNEL/file.erl" --output stubs/file.dreamt
    echo "Generated stubs:"
    wc -l stubs/*.dreamt

# Clean build artifacts
clean:
    cargo clean
    rm -rf target/stdlib

# Format code
fmt:
    cargo fmt

# Lint code
lint:
    cargo clippy

# Watch for changes and run tests
watch:
    cargo watch -x test

# Build and run a project-style example (debug mode for fast iteration)
example name:
    cd examples/{{name}} && cargo run --manifest-path ../../Cargo.toml -- run

# Run a single-file example
example-run name:
    cargo run --release -- run examples/{{name}}.dream

# Rebuild all stdlib modules (compiles all files in a single pass)
stdlib:
    cargo run --release -- stdlib --force
