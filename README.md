# Dream

**A programming language with Rust-like syntax and Erlang-style concurrency.**

Dream brings Rust's expressive type system and familiar syntax to the BEAM virtual machine. Write concurrent, fault-tolerant applications with the ergonomics of Rust and the battle-tested runtime of Erlang/OTP.

```rust
use process::send;

pub fn main() {
    // Spawn a lightweight process
    let pid = spawn(worker());
    send(pid, (:greet, "World", self()));

    receive {
        (:response, msg) => println(msg)
    }
}

fn worker() {
    receive {
        (:greet, name, sender) => {
            let msg = "Hello, " + name + "!";
            send(sender, (:response, msg))
        }
    }
}
```

## Why Dream?

Dream combines the best of two worlds:

| From Rust | From Erlang/OTP |
|-----------|-----------------|
| `struct`, `enum`, `impl` blocks | Lightweight processes |
| `trait` definitions and bounds | Message passing |
| Generics with type parameters | Preemptive scheduling |
| Pattern matching with guards | Hot code reloading |
| `Result<T, E>` and `?` operator | Fault tolerance |
| Module system with `mod`/`use` | Supervisor trees |
| Derive macros | Hex package ecosystem |

## Quick Start

```bash
# Create a new project
surrealc new my_app
cd my_app

# Build the project
surrealc build

# Run the project
surrealc run

# Run tests
surrealc test

# Start the REPL
surrealc shell
```

## Compilation

```
Source → Lexer → Parser → AST → Core Erlang → BEAM
```

Dream compiles to Core Erlang, which is then compiled to BEAM bytecode. This gives you full access to the Erlang/OTP ecosystem, including OTP behaviors, Hex packages, and interop with Erlang and Elixir libraries.

## Language Features

### Concurrency

```rust
use process::send;

pub fn main() {
    let pid = spawn(worker());
    send(pid, (:hello, self()));

    receive {
        (:reply, msg) => println(msg)
    }
}

fn worker() {
    receive {
        (:hello, sender) => send(sender, (:reply, "Hello from worker!"))
    }
}
```

- **Processes**: lightweight, isolated units of execution
- **Message Passing**: async send, receive with timeout, selective receive
- **Links**: bidirectional crash notification between processes
- **Monitors**: one-way crash notification (DOWN messages)
- **Process Registry**: register/unregister/whereis for named processes

### Data Types

```rust
// Primitives
let i: int = 42;                    // Arbitrary precision integer
let f: float = 3.14;                // 64-bit float
let b: bool = true;                 // Boolean
let a: Atom = :ok;                  // Atom (interned string)

// Strings and Binaries
let s: String = "hello";            // UTF-8 string (list of codepoints)
let bin: Binary = "hello";          // Binary (byte sequence)

// Collections
let list: [int] = [1, 2, 3];        // Linked list
let tuple: (int, String) = (1, "a"); // Fixed-size tuple
let map: Map = {key: "value"};      // Hash map

// Process types
let pid: Pid = self();              // Process identifier
let r: Ref = make_ref();            // Unique reference

// Generic types
let opt: Option<int> = Some(42);    // Optional value
let res: Result<int, String> = Ok(1); // Result type

// Any type (dynamic)
let x: any = "anything";            // Accepts any value
```

**Type Aliases**

```rust
type UserId = int;
type Handler = fn(Map) -> Map;
type Cache<T> = Map;  // Generic type alias
```

### Control Flow

```rust
// If expressions
let status = if count > 0 { :active } else { :empty };

// If-else chains
let grade = if score >= 90 {
    "A"
} else if score >= 80 {
    "B"
} else if score >= 70 {
    "C"
} else {
    "F"
};

// For loops with ranges
for i <- 1..10 {
    println(i)
}

// For loops with lists
for item <- items {
    process(item)
}

// While loops (via recursion)
fn countdown(n: int) {
    if n > 0 {
        println(n);
        countdown(n - 1)
    }
}
```

### Pattern Matching

```rust
// Match expressions
fn describe(value: any) -> string {
    match value {
        0 => "zero",
        n if n > 0 => "positive",
        n if n < 0 => "negative",
        (:ok, result) => "success",
        (:error, _) => "failure",
        [head, ..tail] => "non-empty list",
        [] => "empty list",
        _ => "unknown"
    }
}

// Destructuring in let bindings
let (x, y) = point;
let [first, second, ..rest] = items;
let User { name, email, .. } = user;

// Pattern matching in function arguments
fn handle_result(result: Result<int, string>) -> int {
    match result {
        Ok(value) => value,
        Err(msg) => {
            println("Error: " + msg);
            0
        }
    }
}
```

### Error Handling

```rust
fn might_fail(x: int) -> Result<int, string> {
    if x < 0 {
        Err("negative number")
    } else {
        Ok(x * 2)
    }
}

fn chain_operations(x: int) -> Result<int, string> {
    let a = might_fail(x)?;    // Returns Err early if x < 0
    let b = might_fail(a)?;    // Propagates error
    Ok(b)
}
```

### Structs and Enums

```rust
struct User {
    id: int,
    name: string,
    email: Option<string>,
}

impl User {
    pub fn new(id: int, name: string) -> User {
        User { id, name, email: None }
    }
}

enum Status {
    Active,
    Inactive,
    Pending { reason: string },
}
```

### Traits

```rust
trait Display {
    fn display(self) -> String;
}

trait Default {
    fn default() -> Self;
}

impl Display for User {
    pub fn display(self) -> String {
        self.name
    }
}

impl Default for User {
    pub fn default() -> User {
        User { id: 0, name: "anonymous", email: None }
    }
}
```

### Generics

```rust
// Generic functions
fn first<T>(list: [T]) -> Option<T> {
    match list {
        [head, ..] => Some(head),
        [] => None,
    }
}

fn map<T, U>(list: [T], f: fn(T) -> U) -> [U] {
    match list {
        [] => [],
        [head, ..tail] => [f(head), ..map(tail, f)],
    }
}

// Generic structs
struct Stack<T> {
    items: [T],
}

impl<T> Stack<T> {
    pub fn new() -> Stack<T> {
        Stack { items: [] }
    }

    pub fn push(self, item: T) -> Stack<T> {
        Stack { items: [item, ..self.items] }
    }

    pub fn pop(self) -> (Option<T>, Stack<T>) {
        match self.items {
            [head, ..tail] => (Some(head), Stack { items: tail }),
            [] => (None, self),
        }
    }
}

// Trait bounds
fn print_all<T: Display>(items: [T]) {
    for item <- items {
        println(item.display())
    }
}
```

### Derive Macros

```rust
use serde::Serialize;
use serde::Deserialize;

#[derive(Serialize, Deserialize)]
pub struct User {
    id: int,
    name: String,
    email: Option<String>,
}

// Now you can serialize/deserialize:
let json = serde_json::to_string(user);
let user: User = serde_json::from_str_typed(json);
```

### Attributes

```rust
#[test]
fn test_addition() {
    assert_eq(1 + 1, 2)
}

#[cfg(feature = "json")]
pub fn parse_json(s: string) -> any {
    :jason::decode(s)
}

#[cfg(test)]
mod tests {
    // Only compiled during testing
}
```

### OTP Integration

Implement OTP behaviors using traits:

```rust
use application::Application;

impl Application {
    pub fn start(_type: atom, _args: any) -> Result<pid, any> {
        // Start your application
        let pid = spawn(my_server());
        Ok(pid)
    }

    pub fn stop(_state: pid) -> atom {
        :ok
    }
}
```

### Closures

```rust
// Anonymous functions with closure capture
let multiplier = 2;
let double = |x| x * multiplier;

// Multi-argument closures
let add = |a, b| a + b;

// Closures as arguments
let numbers = [1, 2, 3, 4, 5];
let doubled = numbers |> map(|x| x * 2);
let evens = numbers |> filter(|x| x % 2 == 0);
```

### List Comprehensions

```rust
// Basic comprehension
let squares = for x <- [1, 2, 3, 4, 5] { x * x };

// With filter
let even_squares = for x <- 1..10, x % 2 == 0 { x * x };

// Nested comprehension
let pairs = for x <- [1, 2], y <- [:a, :b] { (x, y) };

// With pattern matching
let names = for (id, name) <- users { name };
```

### Receive with Timeout

```rust
// Wait for message with timeout
receive {
    (:data, value) => handle_data(value),
    (:done,) => :ok
} after 5000 {
    // Timeout after 5 seconds
    :timeout
}

// Selective receive
fn wait_for_ack(ref: Ref) -> Result<any, atom> {
    receive {
        (:ack, r, result) if r == ref => Ok(result)
    } after 10000 {
        Err(:timeout)
    }
}
```

### Links and Monitors

```rust
// Bidirectional link - if either process crashes, both are notified
let pid = spawn_link(worker());

// One-way monitor - only the caller receives DOWN message
let ref = monitor(pid);

receive {
    (:DOWN, r, :process, p, reason) if r == ref => {
        println("Process crashed: " + reason)
    }
}

// Unlink/demonitor
unlink(pid);
demonitor(ref);
```

### Pipe Operator

```rust
let result = data
    |> transform()
    |> filter(predicate)
    |> map(func);
```

### Binaries

```rust
// Binary literals
let bin: Binary = "hello";

// Binary pattern matching
fn parse_header(data: Binary) -> (int, int, Binary) {
    match data {
        <<version:8, flags:8, rest:binary>> => (version, flags, rest),
        _ => (0, 0, <<>>)
    }
}

// Binary construction
let packet = <<1:8, 0:8, "payload":binary>>;
```

### Erlang Interop

Call Erlang/Elixir functions directly:

```rust
// Call Erlang module
let result = :lists::reverse([1, 2, 3]);

// Call Elixir module
let json = :"Elixir.Jason"::encode(data);
```

## Project Structure

A Dream project looks like this:

```
my_app/
├── surreal.toml           # Project configuration
├── src/
│   ├── lib.sur        # Root module
│   ├── app.sur        # Application entry point
│   └── handlers/
│       ├── mod.sur    # Submodule declarations
│       └── api.sur    # Handler implementation
└── _build/              # Build artifacts
```

### surreal.toml

```toml
[package]
name = "my_app"
version = "0.1.0"

[application]
mod = "my_app::app"      # OTP application module

[dependencies]
cowboy = "2.12.0"        # Hex packages
jason = "1.4.4"

[features]
json = []
full = ["json"]
```

### Module System

Dream uses a Rust-like module system:

```rust
// src/lib.sur - root module
mod app;           // Loads src/app.sur
mod handlers;      // Loads src/handlers/mod.sur

pub fn version() -> string {
    "1.0.0"
}
```

```rust
// src/handlers/mod.sur
mod api;           // Loads src/handlers/api.sur
mod websocket;
```

## CLI Commands

| Command | Description |
|---------|-------------|
| `surrealc new <name>` | Create a new project |
| `surrealc build` | Build the project |
| `surrealc run` | Build and run |
| `surrealc test` | Run tests |
| `surrealc test "pattern"` | Run tests matching pattern |
| `surrealc shell` | Interactive REPL |
| `surrealc deps get` | Fetch dependencies |
| `surrealc deps update` | Update dependencies |
| `surrealc bindgen` | Generate type stubs from Erlang |

### Build Options

```bash
surrealc build --features json      # Enable features
surrealc test --features json       # Test with features
```

## Building from Source

```bash
# Clone the repository
git clone https://github.com/scrogson/dream
cd dream

# Build the compiler
cargo build --release

# Run tests
cargo test

# Install locally
cargo install --path .
```

## Examples

See the `examples/` directory for complete examples:

- `http_api/` - JSON API server using Cowboy
- `concurrency/` - Process spawning and message passing
- `genserver/` - OTP GenServer implementation

## Related Projects

- [tree-sitter-dream](https://github.com/scrogson/tree-sitter-dream) - Tree-sitter grammar for editor support

## License

MIT
