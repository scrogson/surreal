//! Dream interactive shell (REPL)
//!
//! Provides an interactive environment for evaluating Dream expressions
//! using the BEAM runtime with a persistent process for fast evaluation.

use std::io::{BufRead, BufReader, Write};
use std::process::{Child, Command, ExitCode, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use dream::compiler::{BinOp, Expr, Parser};

/// Counter for generating unique module names
static EVAL_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Erlang eval server code - runs in a loop reading filenames and evaluating them
const EVAL_SERVER: &str = r#"Loop = fun Loop() -> case io:get_line("") of eof -> ok; {error, _} -> ok; Line -> Filename = string:trim(Line), case Filename of "" -> Loop(); _ -> Result = try ModName = list_to_atom(filename:basename(Filename, ".core")), case compile:file(Filename, [from_core, binary, return_errors]) of {ok, ModName, Binary} -> code:purge(ModName), case code:load_binary(ModName, Filename, Binary) of {module, ModName} -> Val = ModName:'__eval__'(), code:purge(ModName), code:delete(ModName), {ok, Val}; {error, What} -> {error, {load_failed, What}} end; {error, Errors, _Warnings} -> {error, {compile_failed, Errors}} end catch Class:Reason:Stack -> {error, {Class, Reason, Stack}} end, case Result of {ok, Value} -> io:format("~s~p~n", [<<1>>, Value]); {error, Err} -> io:format("~s~p~n", [<<2>>, Err]) end, Loop() end end end, Loop()"#;

/// Binding stored from a let statement
#[derive(Clone, Debug)]
struct Binding {
    name: String,
    /// The Core Erlang expression for this binding's value
    core_expr: String,
}

/// REPL state
struct ReplState {
    /// Accumulated bindings from let statements
    bindings: Vec<Binding>,
    /// The running BEAM process
    beam_process: Option<Child>,
    /// Stdin handle for the BEAM process
    beam_stdin: Option<std::process::ChildStdin>,
    /// Stdout reader for the BEAM process
    beam_stdout: Option<BufReader<std::process::ChildStdout>>,
    /// Path to stdlib beam files
    stdlib_path: Option<String>,
    /// Temp directory for this session
    temp_dir: std::path::PathBuf,
}

impl ReplState {
    fn new() -> Self {
        let stdlib_path = find_stdlib_path();
        let temp_dir = std::env::temp_dir();

        Self {
            bindings: Vec::new(),
            beam_process: None,
            beam_stdin: None,
            beam_stdout: None,
            stdlib_path,
            temp_dir,
        }
    }

    /// Start the BEAM process if not already running
    fn ensure_beam_running(&mut self) -> Result<(), String> {
        if self.beam_process.is_some() {
            return Ok(());
        }

        // The eval server code is self-contained
        let eval_code = format!("{}.", EVAL_SERVER);

        let mut cmd = Command::new("erl");
        cmd.arg("-noshell");

        // Add temp dir to code path
        cmd.arg("-pa").arg(&self.temp_dir);

        // Add stdlib to code path if available
        if let Some(ref stdlib) = self.stdlib_path {
            cmd.arg("-pa").arg(stdlib);
        }

        cmd.arg("-eval").arg(&eval_code);

        cmd.stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit());

        let mut child = cmd
            .spawn()
            .map_err(|e| format!("Failed to start BEAM: {}", e))?;

        let stdin = child.stdin.take().ok_or("Failed to get stdin")?;
        let stdout = child.stdout.take().ok_or("Failed to get stdout")?;

        self.beam_process = Some(child);
        self.beam_stdin = Some(stdin);
        self.beam_stdout = Some(BufReader::new(stdout));

        Ok(())
    }

    /// Evaluate an expression and return the result as a string
    fn eval_expr(&mut self, expr: &Expr) -> Result<String, String> {
        // Ensure BEAM is running
        self.ensure_beam_running()?;

        // Generate a unique module name
        let counter = EVAL_COUNTER.fetch_add(1, Ordering::SeqCst);
        let module_name = format!("dream_repl_{}", counter);

        // Generate Core Erlang for the expression wrapped in a module
        let core_erlang = self.generate_core_erlang(&module_name, expr)?;

        // Write to temp file
        let core_file = self.temp_dir.join(format!("{}.core", module_name));

        std::fs::write(&core_file, &core_erlang)
            .map_err(|e| format!("Failed to write Core Erlang: {}", e))?;

        // Send filename to BEAM process
        let stdin = self.beam_stdin.as_mut().ok_or("BEAM stdin not available")?;
        writeln!(stdin, "{}", core_file.display())
            .map_err(|e| format!("Failed to send to BEAM: {}", e))?;
        stdin
            .flush()
            .map_err(|e| format!("Failed to flush: {}", e))?;

        // Read result from BEAM process
        let stdout = self
            .beam_stdout
            .as_mut()
            .ok_or("BEAM stdout not available")?;
        let mut line = String::new();
        stdout
            .read_line(&mut line)
            .map_err(|e| format!("Failed to read from BEAM: {}", e))?;

        // Clean up temp file
        let _ = std::fs::remove_file(&core_file);

        // Parse result - first byte is status (1 = ok, 2 = error)
        if line.is_empty() {
            return Err("No response from BEAM".to_string());
        }

        let first_byte = line.as_bytes().first().copied().unwrap_or(0);
        let result = line[1..].trim().to_string();

        if first_byte == 1 {
            Ok(result)
        } else {
            Err(format!("Evaluation error: {}", result))
        }
    }

    /// Generate Core Erlang for an expression wrapped in a module
    fn generate_core_erlang(&self, module_name: &str, expr: &Expr) -> Result<String, String> {
        let mut output = String::new();

        // Module header
        output.push_str(&format!(
            "module '{}' ['__eval__'/0]\n    attributes []\n\n",
            module_name
        ));

        // Generate the eval function
        output.push_str("'__eval__'/0 =\nfun () ->\n");

        // Add bindings as let expressions
        for binding in &self.bindings {
            output.push_str(&format!(
                "    let <{}> =\n    {}\n    in ",
                capitalize_first(&binding.name),
                binding.core_expr
            ));
        }

        // Generate the expression
        let expr_core = self.expr_to_core(expr)?;
        output.push_str(&expr_core);
        output.push_str("\nend\n");

        Ok(output)
    }

    /// Convert an expression to Core Erlang
    fn expr_to_core(&self, expr: &Expr) -> Result<String, String> {
        match expr {
            Expr::Int(n) => Ok(n.to_string()),
            Expr::Bool(b) => Ok(if *b { "'true'" } else { "'false'" }.to_string()),
            Expr::String(s) => {
                // Convert string to list of integers (Erlang string representation)
                let chars: Vec<String> = s.bytes().map(|b| b.to_string()).collect();
                Ok(format!("[{}]", chars.join(", ")))
            }
            Expr::Atom(a) => Ok(format!("'{}'", a)),
            Expr::Ident(name) => {
                // Check if it's a binding
                if self.bindings.iter().any(|b| &b.name == name) {
                    Ok(capitalize_first(name))
                } else {
                    Err(format!("Undefined variable: {}", name))
                }
            }
            Expr::Binary { op, left, right } => {
                let left_core = self.expr_to_core(left)?;
                let right_core = self.expr_to_core(right)?;
                let op_str = match op {
                    BinOp::Add => "call 'erlang':'+'",
                    BinOp::Sub => "call 'erlang':'-'",
                    BinOp::Mul => "call 'erlang':'*'",
                    BinOp::Div => "call 'erlang':'div'",
                    BinOp::Mod => "call 'erlang':'rem'",
                    BinOp::Eq => "call 'erlang':'=:='",
                    BinOp::Ne => "call 'erlang':'=/='",
                    BinOp::Lt => "call 'erlang':'<'",
                    BinOp::Le => "call 'erlang':'=<'",
                    BinOp::Gt => "call 'erlang':'>'",
                    BinOp::Ge => "call 'erlang':'>='",
                    BinOp::And => "call 'erlang':'and'",
                    BinOp::Or => "call 'erlang':'or'",
                };
                Ok(format!("{}({}, {})", op_str, left_core, right_core))
            }
            Expr::Tuple(elems) => {
                let elem_strs: Result<Vec<_>, _> =
                    elems.iter().map(|e| self.expr_to_core(e)).collect();
                Ok(format!("{{{}}}", elem_strs?.join(", ")))
            }
            Expr::List(elems) => {
                let elem_strs: Result<Vec<_>, _> =
                    elems.iter().map(|e| self.expr_to_core(e)).collect();
                Ok(format!("[{}]", elem_strs?.join(", ")))
            }
            Expr::Call {
                func,
                args,
                type_args: _,
                inferred_type_args: _,
            } => {
                // Handle qualified calls like module::func
                if let Expr::Path { segments } = func.as_ref() {
                    if segments.len() == 2 {
                        let module = &segments[0];
                        let func_name = &segments[1];
                        let arg_strs: Result<Vec<_>, _> =
                            args.iter().map(|a| self.expr_to_core(a)).collect();
                        return Ok(format!(
                            "call 'dream::{}'  :'{}'({})",
                            module,
                            func_name,
                            arg_strs?.join(", ")
                        ));
                    }
                }

                // Simple function call
                if let Expr::Ident(name) = func.as_ref() {
                    let arg_strs: Result<Vec<_>, _> =
                        args.iter().map(|a| self.expr_to_core(a)).collect();
                    return Ok(format!("apply '{}'({})", name, arg_strs?.join(", ")));
                }

                Err(format!("Unsupported call expression: {:?}", func))
            }
            _ => Err(format!("Unsupported expression type in REPL: {:?}", expr)),
        }
    }

    /// Add a binding
    fn add_binding(&mut self, name: String, expr: &Expr) -> Result<(), String> {
        let core_expr = self.expr_to_core(expr)?;
        // Remove existing binding with same name (shadowing)
        self.bindings.retain(|b| b.name != name);
        self.bindings.push(Binding { name, core_expr });
        Ok(())
    }

    /// Clear all bindings
    fn clear_bindings(&mut self) {
        self.bindings.clear();
    }
}

impl Drop for ReplState {
    fn drop(&mut self) {
        // Close stdin to signal EOF to the eval loop
        self.beam_stdin.take();

        // Kill the BEAM process if running
        if let Some(ref mut child) = self.beam_process {
            let _ = child.kill();
            let _ = child.wait();
        }
    }
}

/// Find the stdlib beam files
fn find_stdlib_path() -> Option<String> {
    // Try relative to executable
    if let Ok(exe_path) = std::env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            let stdlib = exe_dir.join("../stdlib");
            if stdlib.exists() {
                return stdlib
                    .canonicalize()
                    .ok()
                    .map(|p| p.to_string_lossy().into_owned());
            }
        }
    }

    // Try target/stdlib
    let target_stdlib = std::path::Path::new("target/stdlib");
    if target_stdlib.exists() {
        return target_stdlib
            .canonicalize()
            .ok()
            .map(|p| p.to_string_lossy().into_owned());
    }

    None
}

/// Capitalize the first character of a string (for Erlang variable names)
fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
    }
}

/// Print the welcome banner
fn print_banner() {
    println!("Dream {} (BEAM backend)", env!("CARGO_PKG_VERSION"));
    println!("Type :help for commands, :quit to exit");
    println!();
}

/// Print help information
fn print_help() {
    println!("Commands:");
    println!("  :help          Show this help message");
    println!("  :quit, :q      Exit the shell");
    println!("  :clear         Clear all bindings");
    println!("  :bindings      Show current bindings");
    println!();
    println!("Enter Dream expressions to evaluate them.");
    println!("Use 'let x = expr' to create bindings.");
}

/// Run the interactive shell
pub fn run_shell() -> ExitCode {
    print_banner();

    let mut rl = match DefaultEditor::new() {
        Ok(editor) => editor,
        Err(e) => {
            eprintln!("Failed to initialize readline: {}", e);
            return ExitCode::from(1);
        }
    };

    let mut state = ReplState::new();

    loop {
        let readline = rl.readline("dream> ");
        match readline {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }

                // Add to history
                let _ = rl.add_history_entry(line);

                // Handle special commands (only known commands, not atoms)
                if line.starts_with(':') {
                    match line {
                        ":quit" | ":q" => {
                            println!("Goodbye!");
                            break;
                        }
                        ":help" | ":h" => {
                            print_help();
                            continue;
                        }
                        ":clear" => {
                            state.clear_bindings();
                            println!("Bindings cleared.");
                            continue;
                        }
                        ":bindings" | ":b" => {
                            if state.bindings.is_empty() {
                                println!("No bindings.");
                            } else {
                                for binding in &state.bindings {
                                    println!("  {} = <expr>", binding.name);
                                }
                            }
                            continue;
                        }
                        // If not a known command, treat as an expression (atom)
                        _ => {}
                    }
                }

                // Parse and evaluate
                match parse_and_eval(&mut state, line) {
                    Ok(result) => println!("{}", result),
                    Err(e) => eprintln!("Error: {}", e),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!("Goodbye!");
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }

    ExitCode::SUCCESS
}

/// Parse input and evaluate it
fn parse_and_eval(state: &mut ReplState, input: &str) -> Result<String, String> {
    // Try to parse as a let statement first
    if input.trim_start().starts_with("let ") {
        return parse_and_eval_let(state, input);
    }

    // Parse as an expression
    let mut parser = Parser::new(input);
    let expr = parser
        .parse_expr()
        .map_err(|e| format!("Parse error: {:?}", e))?;

    // Evaluate
    state.eval_expr(&expr)
}

/// Parse and evaluate a let statement
fn parse_and_eval_let(state: &mut ReplState, input: &str) -> Result<String, String> {
    // Simple parsing: "let name = expr"
    let input = input.trim_start().strip_prefix("let ").unwrap();

    // Find the = sign
    let eq_pos = input
        .find('=')
        .ok_or_else(|| "Expected '=' in let statement".to_string())?;

    let name = input[..eq_pos].trim().to_string();
    let expr_str = input[eq_pos + 1..].trim();

    // Validate name
    if name.is_empty() || !name.chars().next().unwrap().is_alphabetic() {
        return Err("Invalid variable name".to_string());
    }

    // Parse the expression
    let mut parser = Parser::new(expr_str);
    let expr = parser
        .parse_expr()
        .map_err(|e| format!("Parse error: {:?}", e))?;

    // Add binding
    state.add_binding(name, &expr)?;

    Ok(":ok".to_string())
}
