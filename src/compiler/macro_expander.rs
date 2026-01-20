//! Macro expander that executes user-defined macros on BEAM.
//!
//! This module handles the BEAM communication needed for running user-defined
//! macros at compile time. Macros are Dream functions marked with `#[macro]`
//! that take AST data and return transformed AST.
//!
//! ## Architecture
//!
//! ```text
//! Compiler                          BEAM Process
//! ─────────                         ────────────
//! 1. Serialize AST to Erlang term
//!                     ──────────────►
//! 2.                                 Call macro function
//! 3.                                 Return expanded AST
//!                     ◄──────────────
//! 4. Deserialize result to AST
//! ```

use std::io::{BufRead, BufReader, Write};
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::sync::mpsc::{self, Receiver};
use std::thread;

use crate::compiler::lexer::Span;

/// Error type for macro expansion failures.
#[derive(Debug)]
pub struct MacroError {
    pub message: String,
    /// Optional span in the source where the error occurred.
    pub span: Option<Span>,
    /// Optional file path where the error occurred.
    pub file: Option<PathBuf>,
    /// Optional macro name that failed.
    pub macro_name: Option<String>,
}

impl MacroError {
    /// Create a new macro error without location information.
    pub fn new(msg: impl Into<String>) -> Self {
        MacroError {
            message: msg.into(),
            span: None,
            file: None,
            macro_name: None,
        }
    }

    /// Create a macro error with source location.
    pub fn with_span(msg: impl Into<String>, span: Span) -> Self {
        MacroError {
            message: msg.into(),
            span: Some(span),
            file: None,
            macro_name: None,
        }
    }

    /// Add file path information to the error.
    pub fn in_file(mut self, path: impl Into<PathBuf>) -> Self {
        self.file = Some(path.into());
        self
    }

    /// Add macro name information to the error.
    pub fn in_macro(mut self, name: impl Into<String>) -> Self {
        self.macro_name = Some(name.into());
        self
    }
}

impl std::fmt::Display for MacroError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Macro error")?;

        if let Some(ref name) = self.macro_name {
            write!(f, " in `{}`", name)?;
        }

        if let Some(ref file) = self.file {
            write!(f, " ({})", file.display())?;
        }

        if let Some(ref span) = self.span {
            write!(f, " at {}..{}", span.start, span.end)?;
        }

        write!(f, ": {}", self.message)
    }
}

impl std::error::Error for MacroError {}

/// Result type for macro operations.
pub type MacroResult<T> = Result<T, MacroError>;

const RESULT_MARKER: &str = "\x00MACRO_RESULT\x00";

/// Erlang macro server for executing macro functions.
///
/// Protocol (pipe-delimited to avoid conflicts with :: in module names):
///   expand|<module>|<function>|<ast_term> - call macro function with AST
///   quit - shutdown the server
const MACRO_SERVER: &str = r#"
Loop = fun Loop() ->
    case io:get_line("") of
        eof -> ok;
        {error, _} -> ok;
        Line ->
            Cmd = string:trim(Line),
            case Cmd of
                "" -> Loop();
                "quit" -> ok;
                _ ->
                    case string:prefix(Cmd, "expand|") of
                        nomatch ->
                            io:format("~s~nerr:unknown command~n", [<<0, "MACRO_RESULT", 0>>]),
                            Loop();
                        Rest ->
                            case string:split(Rest, "|", all) of
                                [ModStr, FunStr | AstParts] ->
                                    try
                                        Mod = list_to_atom(ModStr),
                                        Fun = list_to_atom(FunStr),
                                        AstStr = lists:flatten(lists:join("|", AstParts)),
                                        {ok, Tokens, _} = erl_scan:string(AstStr ++ "."),
                                        {ok, Ast} = erl_parse:parse_term(Tokens),
                                        Result = Mod:Fun(Ast),
                                        io:format("~s~nok:~w~n", [<<0, "MACRO_RESULT", 0>>, Result]),
                                        Loop()
                                    catch
                                        Class:Reason:Stack ->
                                            Err = io_lib:format("~p:~p~n~p", [Class, Reason, Stack]),
                                            io:format("~s~nerr:~s~n", [<<0, "MACRO_RESULT", 0>>, Err]),
                                            Loop()
                                    end;
                                _ ->
                                    io:format("~s~nerr:invalid expand command~n", [<<0, "MACRO_RESULT", 0>>]),
                                    Loop()
                            end
                    end
            end
    end
end,
Loop()
"#;

/// Macro expander that communicates with a BEAM process.
pub struct MacroExpander {
    beam_process: Option<Child>,
    beam_stdin: Option<std::process::ChildStdin>,
    result_rx: Option<Receiver<Result<String, String>>>,
    /// Paths to add to BEAM code path (for loading macro modules).
    beam_paths: Vec<PathBuf>,
}

impl MacroExpander {
    /// Create a new macro expander with the given BEAM paths.
    pub fn new(beam_paths: Vec<PathBuf>) -> Self {
        MacroExpander {
            beam_process: None,
            beam_stdin: None,
            result_rx: None,
            beam_paths,
        }
    }

    /// Start the BEAM process if not already running.
    pub fn ensure_running(&mut self) -> MacroResult<()> {
        if self.beam_process.is_some() {
            return Ok(());
        }

        // Flatten the Erlang code to a single line
        let eval_code = MACRO_SERVER
            .lines()
            .map(|l| l.trim())
            .collect::<Vec<_>>()
            .join(" ");
        let eval_code = format!("{}.", eval_code);

        let mut cmd = Command::new("erl");
        cmd.arg("-noshell");

        // Add paths for loading macro modules
        for path in &self.beam_paths {
            cmd.arg("-pa").arg(path);
        }

        cmd.arg("-eval").arg(&eval_code);
        cmd.stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit());

        let mut child = cmd
            .spawn()
            .map_err(|e| MacroError::new(format!("Failed to start BEAM: {}", e)))?;

        let stdin = child
            .stdin
            .take()
            .ok_or_else(|| MacroError::new("Failed to get stdin"))?;
        let stdout = child
            .stdout
            .take()
            .ok_or_else(|| MacroError::new("Failed to get stdout"))?;

        // Create channel for results
        let (tx, rx) = mpsc::channel::<Result<String, String>>();

        // Spawn background thread to read stdout
        thread::spawn(move || {
            let mut reader = BufReader::new(stdout);
            let mut waiting_for_result = false;

            loop {
                let mut line = String::new();
                match reader.read_line(&mut line) {
                    Ok(0) => break, // EOF
                    Ok(_) => {
                        let trimmed = line.trim();

                        if trimmed == RESULT_MARKER {
                            waiting_for_result = true;
                            continue;
                        }

                        if waiting_for_result {
                            if let Some(value) = trimmed.strip_prefix("ok:") {
                                let _ = tx.send(Ok(value.to_string()));
                                waiting_for_result = false;
                            } else if let Some(err) = trimmed.strip_prefix("err:") {
                                let _ = tx.send(Err(err.to_string()));
                                waiting_for_result = false;
                            }
                            // Ignore other lines while waiting for result
                        }
                    }
                    Err(_) => break,
                }
            }
        });

        self.beam_process = Some(child);
        self.beam_stdin = Some(stdin);
        self.result_rx = Some(rx);

        Ok(())
    }

    /// Send a command to BEAM and get the response.
    fn send_command(&mut self, cmd: &str) -> MacroResult<String> {
        let stdin = self
            .beam_stdin
            .as_mut()
            .ok_or_else(|| MacroError::new("BEAM not running"))?;

        writeln!(stdin, "{}", cmd)
            .map_err(|e| MacroError::new(format!("Failed to send: {}", e)))?;
        stdin
            .flush()
            .map_err(|e| MacroError::new(format!("Failed to flush: {}", e)))?;

        // Wait for result with timeout
        let rx = self
            .result_rx
            .as_ref()
            .ok_or_else(|| MacroError::new("No result channel"))?;

        match rx.recv_timeout(std::time::Duration::from_secs(30)) {
            Ok(Ok(result)) => Ok(result),
            Ok(Err(err)) => Err(MacroError::new(err)),
            Err(_) => Err(MacroError::new("Timeout waiting for macro result")),
        }
    }

    /// Expand a derive macro by calling a macro function on BEAM.
    ///
    /// # Arguments
    ///
    /// * `module` - The Surreal module containing the macro (e.g., "surreal::my_macros")
    /// * `function` - The macro function name (e.g., "my_debug")
    /// * `ast_term` - The AST serialized as an Erlang term string
    ///
    /// # Returns
    ///
    /// The expanded AST as an Erlang term string, which can be deserialized
    /// back to Dream AST nodes.
    pub fn expand_macro(
        &mut self,
        module: &str,
        function: &str,
        ast_term: &str,
    ) -> MacroResult<String> {
        self.ensure_running()?;

        // Format: expand|module|function|ast_term (pipe-delimited)
        let cmd = format!("expand|{}|{}|{}", module, function, ast_term);
        self.send_command(&cmd)
    }

    /// Shutdown the BEAM process.
    pub fn shutdown(&mut self) {
        if let Some(ref mut stdin) = self.beam_stdin {
            let _ = writeln!(stdin, "quit");
            let _ = stdin.flush();
        }
        // Drop stdin to signal EOF to the process
        self.beam_stdin.take();
        self.result_rx.take();

        if let Some(ref mut child) = self.beam_process {
            // Try to wait with a short timeout, then kill if necessary
            for _ in 0..10 {
                match child.try_wait() {
                    Ok(Some(_)) => break, // Process exited
                    Ok(None) => {
                        // Still running, wait a bit
                        std::thread::sleep(std::time::Duration::from_millis(50));
                    }
                    Err(_) => break,
                }
            }
            // If still running, force kill
            let _ = child.kill();
            let _ = child.wait();
        }
        self.beam_process.take();
    }

    /// Check if the expander is running.
    pub fn is_running(&self) -> bool {
        self.beam_process.is_some()
    }
}

impl Drop for MacroExpander {
    fn drop(&mut self) {
        self.shutdown();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_macro_error_display() {
        let err = MacroError::new("test error");
        assert_eq!(format!("{}", err), "Macro error: test error");
    }

    #[test]
    fn test_macro_error_with_span() {
        let err = MacroError::with_span("type mismatch", 10..25);
        assert_eq!(format!("{}", err), "Macro error at 10..25: type mismatch");
    }

    #[test]
    fn test_macro_error_with_full_context() {
        let err = MacroError::with_span("expected struct", 100..150)
            .in_file("src/lib.surreal")
            .in_macro("my_derive");
        assert_eq!(
            format!("{}", err),
            "Macro error in `my_derive` (src/lib.surreal) at 100..150: expected struct"
        );
    }

    #[test]
    fn test_macro_expander_new() {
        let expander = MacroExpander::new(vec![]);
        assert!(!expander.is_running());
    }

    // Integration test that requires BEAM - run manually
    #[test]
    #[ignore]
    fn test_macro_expander_start() {
        let mut expander = MacroExpander::new(vec![]);
        expander.ensure_running().expect("Failed to start BEAM");
        assert!(expander.is_running());
        expander.shutdown();
        assert!(!expander.is_running());
    }

    // Integration test with a real macro - run with: cargo test test_macro_expand_simple --ignored
    #[test]
    #[ignore]
    fn test_macro_expand_simple() {
        use crate::compiler::ast_serde;
        use crate::compiler::ast::*;

        // Path to the compiled test_macro.beam
        let beam_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures");

        let mut expander = MacroExpander::new(vec![beam_path]);
        expander.ensure_running().expect("Failed to start BEAM");

        // Create a simple struct definition
        let struct_def = StructDef {
            is_pub: true,
            name: "Point".to_string(),
            type_params: vec![],
            fields: vec![
                ("x".to_string(), SpannedType::unspanned(Type::Int)),
                ("y".to_string(), SpannedType::unspanned(Type::Int)),
            ],
            attrs: vec![],
            span: 0..0,
        };

        // Serialize to Erlang term
        let ast_term = ast_serde::struct_def_to_erlang_term(&struct_def);
        println!("Input term: {}", ast_term);

        // Call the macro
        let result = expander.expand_macro("test_macro", "simple_derive", &ast_term);
        println!("Result: {:?}", result);

        assert!(result.is_ok(), "Macro call failed: {:?}", result.err());

        let result_str = result.unwrap();
        println!("Result term: {}", result_str);

        // Parse the result
        let term = ast_serde::parse_term(&result_str).expect("Failed to parse result");
        println!("Parsed term: {:?}", term);

        // Convert to ImplBlock
        let impl_block = ast_serde::term_to_impl_block(&term).expect("Failed to convert to impl");
        println!("ImplBlock: {:?}", impl_block);

        assert_eq!(impl_block.type_name, "Point");
        assert_eq!(impl_block.methods.len(), 1);
        assert_eq!(impl_block.methods[0].name, "type_name");

        expander.shutdown();
    }
}
