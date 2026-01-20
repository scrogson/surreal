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
    ./target/release/surreal bindgen "$ERTS/erlang.erl" --output stubs/erlang.surreal
    ./target/release/surreal bindgen "$STDLIB/lists.erl" --output stubs/lists.surreal
    ./target/release/surreal bindgen "$STDLIB/maps.erl" --output stubs/maps.surreal
    ./target/release/surreal bindgen "$STDLIB/io.erl" --output stubs/io.surreal
    ./target/release/surreal bindgen "$KERNEL/file.erl" --output stubs/file.surreal
    echo "Generated stubs:"
    wc -l stubs/*.surreal

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
    cargo run --release -- run examples/{{name}}.surreal

# Rebuild all stdlib modules (compiles all files in a single pass)
stdlib:
    cargo run --release -- stdlib --force

# Build the NIF library
nif:
    #!/usr/bin/env bash
    set -euo pipefail
    cd native/surreal_nif
    cargo build --release
    # On macOS, Erlang expects .so but Rust produces .dylib
    if [[ "$(uname)" == "Darwin" ]]; then
        ln -sf libsurreal_nif.dylib target/release/libsurreal_nif.so
    fi
    erlc -o . surreal_nif.erl
    echo "NIF built: native/surreal_nif/target/release/libsurreal_nif.{so,dylib}"

# Build NIF in debug mode
nif-debug:
    #!/usr/bin/env bash
    set -euo pipefail
    cd native/surreal_nif
    cargo build
    if [[ "$(uname)" == "Darwin" ]]; then
        ln -sf libsurreal_nif.dylib target/debug/libsurreal_nif.so
    fi
    erlc -o . surreal_nif.erl
    echo "NIF built (debug): native/surreal_nif/target/debug/libsurreal_nif.{so,dylib}"
