//! Dream interactive shell (REPL)
//!
//! Provides an interactive environment for evaluating Dream expressions
//! using the BEAM runtime with a persistent process for fast evaluation.
//! Uses runtime introspection for module discovery and method dispatch.

use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Write};
use std::process::{Child, Command, ExitCode, Stdio};
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::mpsc::{self, Receiver};
use std::thread;

use rustyline::completion::{Completer, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::Validator;
use rustyline::{Context, Editor, ExternalPrinter, Helper};

use surreal::compiler::{
    check_modules, resolve_stdlib_methods, CompilerError, CoreErlangEmitter,
    GenericFunctionRegistry, Item, ModuleContext, Parser,
};
use miette::{NamedSource, SourceSpan};
use std::sync::{Arc, RwLock};

/// Counter for generating unique module names
static EVAL_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Format a REPL error showing only the user's input, not the internal wrapper.
/// Adjusts the error span to be relative to the user's expression.
fn format_repl_error(
    user_expr: &str,
    expr_offset: usize,
    message: &str,
    span: SourceSpan,
    help: Option<&str>,
) -> String {
    // Adjust span to be relative to user expression
    let span_start = span.offset();
    let span_len = span.len();

    // Check if span is within the user expression
    let adjusted_span = if span_start >= expr_offset {
        let relative_start = span_start - expr_offset;
        let expr_len = user_expr.len();

        // Clamp to user expression bounds
        if relative_start < expr_len {
            let adjusted_len = span_len.min(expr_len - relative_start);
            Some(SourceSpan::new(relative_start.into(), adjusted_len))
        } else {
            // Span is after the expression (in the closing braces)
            // Point to the end of the expression
            Some(SourceSpan::new(expr_len.saturating_sub(1).into(), 1))
        }
    } else {
        // Span is before the user expression (in the wrapper header)
        // This shouldn't normally happen for user input errors
        None
    };

    // Build the error message
    if let Some(span) = adjusted_span {
        // Show the user's input with the error location
        let src = NamedSource::new("<repl>", user_expr.to_string());
        let diag = if let Some(h) = help {
            miette::miette!(
                labels = vec![miette::LabeledSpan::at(span, "here")],
                help = h,
                "{}",
                message
            )
            .with_source_code(src)
        } else {
            miette::miette!(
                labels = vec![miette::LabeledSpan::at(span, "here")],
                "{}",
                message
            )
            .with_source_code(src)
        };
        format!("{:?}", diag)
    } else {
        // No span to display, just show the message
        let mut error = message.to_string();
        if let Some(h) = help {
            error.push_str(&format!("\nhelp: {}", h));
        }
        error
    }
}

/// Result marker for protocol
const RESULT_MARKER: &str = "\x00DREAM_RESULT\x00";

/// Erlang eval server with introspection support
/// Protocol:
///   eval:<filename> - evaluate a Core Erlang file (temp module, purged after)
///   bind:<name>:<filename> - evaluate and store result in process dictionary
///   load:<filename> - load a Core Erlang file persistently
///   call:<module>:<function> - call a function in a loaded module
///   exports:<module> - get exports for a loaded module (in-memory)
///   introspect:list_modules - list all surreal:: modules
///   introspect:exports:<module> - get exports for a module (from .beam file)
const EVAL_SERVER: &str = r#"
Loop = fun Loop() ->
    case io:get_line("") of
        eof -> ok;
        {error, _} -> ok;
        Line ->
            Cmd = string:trim(Line),
            case Cmd of
                "" -> Loop();
                "introspect:list_modules" ->
                    Paths = code:get_path(),
                    Mods = lists:usort(lists:flatten([
                        [begin
                            Base = filename:basename(F, ".beam"),
                            list_to_atom(Base)
                        end || F <- filelib:wildcard("surreal::*.beam", Dir)]
                    || Dir <- Paths])),
                    io:format("~s~nok:~w~n", [<<0, "DREAM_RESULT", 0>>, Mods]),
                    Loop();
                _ ->
                    case string:prefix(Cmd, "exports:") of
                        nomatch ->
                            case string:prefix(Cmd, "introspect:exports:") of
                                nomatch ->
                                    case string:prefix(Cmd, "load:") of
                                        nomatch ->
                                            case string:prefix(Cmd, "call:") of
                                                nomatch ->
                                                    case string:prefix(Cmd, "bind:") of
                                                        nomatch ->
                                                            case string:prefix(Cmd, "eval:") of
                                                                nomatch ->
                                                                    io:format("~s~nerr:unknown_command~n", [<<0, "DREAM_RESULT", 0>>]),
                                                                    Loop();
                                                                Filename ->
                                                                    Result = try
                                                                        ModName = list_to_atom(filename:basename(Filename, ".core")),
                                                                        case compile:file(Filename, [from_core, binary, return_errors]) of
                                                                            {ok, ModName, Binary} ->
                                                                                code:purge(ModName),
                                                                                case code:load_binary(ModName, Filename, Binary) of
                                                                                    {module, ModName} ->
                                                                                        Val = ModName:'__eval__'(),
                                                                                        code:purge(ModName),
                                                                                        code:delete(ModName),
                                                                                        {ok, Val};
                                                                                    {error, What} ->
                                                                                        {error, {load_failed, What}}
                                                                                end;
                                                                            {error, Errors, _Warnings} ->
                                                                                {error, {compile_failed, Errors}}
                                                                        end
                                                                    catch
                                                                        Class:Reason:Stack ->
                                                                            {error, {Class, Reason, Stack}}
                                                                    end,
                                                                    io:format("~s~n", [<<0, "DREAM_RESULT", 0>>]),
                                                                    case Result of
                                                                        {ok, Value} -> io:format("ok:~p~n", [Value]);
                                                                        {error, Err} -> io:format("err:~p~n", [Err])
                                                                    end,
                                                                    Loop()
                                                            end;
                                                        BindRest ->
                                                            [NameStr, Filename] = string:split(BindRest, ":", leading),
                                                            BindKey = list_to_atom("repl_" ++ NameStr),
                                                            Result = try
                                                                ModName = list_to_atom(filename:basename(Filename, ".core")),
                                                                case compile:file(Filename, [from_core, binary, return_errors]) of
                                                                    {ok, ModName, Binary} ->
                                                                        code:purge(ModName),
                                                                        case code:load_binary(ModName, Filename, Binary) of
                                                                            {module, ModName} ->
                                                                                Val = ModName:'__eval__'(),
                                                                                code:purge(ModName),
                                                                                code:delete(ModName),
                                                                                erlang:put(BindKey, Val),
                                                                                {ok, Val};
                                                                            {error, What} ->
                                                                                {error, {load_failed, What}}
                                                                        end;
                                                                    {error, Errors, _Warnings} ->
                                                                        {error, {compile_failed, Errors}}
                                                                end
                                                            catch
                                                                Class:Reason:Stack ->
                                                                    {error, {Class, Reason, Stack}}
                                                            end,
                                                            io:format("~s~n", [<<0, "DREAM_RESULT", 0>>]),
                                                            case Result of
                                                                {ok, Value} -> io:format("ok:~p~n", [Value]);
                                                                {error, Err} -> io:format("err:~p~n", [Err])
                                                            end,
                                                            Loop()
                                                    end;
                                                CallSpec ->
                                                    Result = try
                                                        [ModStr, FuncStr] = string:split(CallSpec, ":"),
                                                        ModName = list_to_atom(ModStr),
                                                        FuncName = list_to_atom(FuncStr),
                                                        Val = erlang:apply(ModName, FuncName, []),
                                                        {ok, Val}
                                                    catch
                                                        Class:Reason:Stack ->
                                                            {error, {Class, Reason, Stack}}
                                                    end,
                                                    io:format("~s~n", [<<0, "DREAM_RESULT", 0>>]),
                                                    case Result of
                                                        {ok, Value} -> io:format("ok:~p~n", [Value]);
                                                        {error, Err} -> io:format("err:~p~n", [Err])
                                                    end,
                                                    Loop()
                                            end;
                                        Filename ->
                                            Result = try
                                                case compile:file(Filename, [from_core, binary, return_errors]) of
                                                    {ok, CompiledMod, Binary} ->
                                                        code:purge(CompiledMod),
                                                        case code:load_binary(CompiledMod, Filename, Binary) of
                                                            {module, CompiledMod} ->
                                                                {ok, CompiledMod};
                                                            {error, What} ->
                                                                {error, {load_failed, What}}
                                                        end;
                                                    {error, Errors, _Warnings} ->
                                                        {error, {compile_failed, Errors}}
                                                end
                                            catch
                                                Class:Reason:Stack ->
                                                    {error, {Class, Reason, Stack}}
                                            end,
                                            io:format("~s~n", [<<0, "DREAM_RESULT", 0>>]),
                                            case Result of
                                                {ok, Value} -> io:format("ok:~p~n", [Value]);
                                                {error, Err} -> io:format("err:~p~n", [Err])
                                            end,
                                            Loop()
                                    end;
                                ModStr ->
                                    Paths = code:get_path(),
                                    BeamFile = ModStr ++ ".beam",
                                    FindFile = fun FindFile([]) -> not_found;
                                                   FindFile([Dir|Rest]) ->
                                                       Path = filename:join(Dir, BeamFile),
                                                       case filelib:is_file(Path) of
                                                           true -> {ok, Path};
                                                           false -> FindFile(Rest)
                                                       end
                                               end,
                                    case FindFile(Paths) of
                                        not_found ->
                                            io:format("~s~nerr:module_not_found~n", [<<0, "DREAM_RESULT", 0>>]);
                                        {ok, FullPath} ->
                                            case beam_lib:chunks(FullPath, [exports]) of
                                                {ok, {_, [{exports, Exports}]}} ->
                                                    io:format("~s~nok:~w~n", [<<0, "DREAM_RESULT", 0>>, Exports]);
                                                {error, _, _} ->
                                                    io:format("~s~nerr:beam_read_failed~n", [<<0, "DREAM_RESULT", 0>>])
                                            end
                                    end,
                                    Loop()
                            end;
                        ModStr ->
                            Result = try
                                ModName = list_to_atom(ModStr),
                                IsLoaded = code:is_loaded(ModName),
                                case IsLoaded of
                                    {file, _} ->
                                        Exports = ModName:module_info(exports),
                                        {ok, Exports};
                                    false ->
                                        {error, {not_loaded, ModName, IsLoaded}}
                                end
                            catch
                                Class:Reason:Stack -> {error, {Class, Reason, Stack}}
                            end,
                            io:format("~s~n", [<<0, "DREAM_RESULT", 0>>]),
                            case Result of
                                {ok, Value} -> io:format("ok:~w~n", [Value]);
                                {error, Err} -> io:format("err:~p~n", [Err])
                            end,
                            Loop()
                    end
            end
    end
end,
Loop()
"#;

/// Keywords for completion
const KEYWORDS: &[&str] = &[
    "let", "if", "else", "match", "fn", "pub", "mod", "use", "struct", "enum",
    "trait", "impl", "type", "extern", "spawn", "send", "receive", "self",
    "true", "false",
];

/// REPL commands for completion
const COMMANDS: &[&str] = &[":help", ":quit", ":q", ":clear", ":bindings", ":b", ":h", ":reload", ":edit", ":e", ":load"];

/// Information about a module's exports
#[derive(Clone, Debug, Default)]
struct ModuleInfo {
    /// Module-level functions: name -> arity
    functions: HashMap<String, u8>,
    /// Struct impl methods: method_name -> (struct_name, arity)
    /// e.g., "put" -> ("Map", 3) means Map_put/3
    struct_methods: HashMap<String, (String, u8)>,
}

/// Registry of all known modules and their exports
#[derive(Clone, Debug, Default)]
struct ModuleRegistry {
    /// Map from module name (e.g., "map", "string") to its info
    modules: HashMap<String, ModuleInfo>,
}

impl ModuleRegistry {
    fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }

    /// Parse exports and categorize into functions vs struct methods
    fn add_module(&mut self, module_name: &str, exports: &[(String, u8)]) {
        let mut info = ModuleInfo::default();

        // Extract the short module name (e.g., "surreal::map" -> "map")
        let short_name = module_name
            .strip_prefix("surreal::")
            .unwrap_or(module_name);

        // Expected struct name for this module (e.g., "map" -> "Map")
        let expected_struct = capitalize_first(short_name);
        let struct_prefix = format!("{}_", expected_struct);

        for (func_name, arity) in exports {
            // Skip internal functions
            if func_name == "module_info" {
                continue;
            }

            if let Some(method_name) = func_name.strip_prefix(&struct_prefix) {
                // This is a struct method: Map_put -> ("Map", "put", arity)
                info.struct_methods
                    .insert(method_name.to_string(), (expected_struct.clone(), *arity));
            } else {
                // This is a module-level function
                info.functions.insert(func_name.clone(), *arity);
            }
        }

        self.modules.insert(short_name.to_string(), info);
    }

    /// Get all function names for a module (for completion)
    fn get_completions(&self, module: &str) -> Vec<String> {
        let mut completions = Vec::new();
        if let Some(info) = self.modules.get(module) {
            completions.extend(info.functions.keys().cloned());
            completions.extend(info.struct_methods.keys().cloned());
        }
        completions.sort();
        completions
    }

    /// Get all module names (for completion)
    fn get_module_names(&self) -> Vec<String> {
        let mut names: Vec<_> = self.modules.keys().cloned().collect();
        names.sort();
        names
    }
}

/// Shared state type
type SharedBindings = Rc<RefCell<Vec<Binding>>>;
type SharedRegistry = Rc<RefCell<ModuleRegistry>>;

/// Binding stored from a let statement
/// Values are stored in the BEAM process dictionary under key `repl_<name>`
#[derive(Clone, Debug)]
struct Binding {
    name: String,
}

/// REPL helper for rustyline
struct ReplHelper {
    bindings: SharedBindings,
    registry: SharedRegistry,
}

impl ReplHelper {
    fn new(bindings: SharedBindings, registry: SharedRegistry) -> Self {
        Self { bindings, registry }
    }
}

impl Completer for ReplHelper {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Pair>)> {
        let mut completions = Vec::new();
        let (start, word) = find_word_start(line, pos);

        // Complete REPL commands
        if word.starts_with(':') {
            for cmd in COMMANDS {
                if cmd.starts_with(word) {
                    completions.push(Pair {
                        display: cmd.to_string(),
                        replacement: cmd.to_string(),
                    });
                }
            }
            return Ok((start, completions));
        }

        // Complete module paths (e.g., "string::" -> "string::reverse")
        if let Some(colon_pos) = word.find("::") {
            let module = &word[..colon_pos];
            let func_prefix = &word[colon_pos + 2..];

            let registry = self.registry.borrow();
            for func in registry.get_completions(module) {
                if func.starts_with(func_prefix) {
                    let full = format!("{}::{}", module, func);
                    completions.push(Pair {
                        display: full.clone(),
                        replacement: full,
                    });
                }
            }
            return Ok((start, completions));
        }

        // Complete keywords
        for kw in KEYWORDS {
            if kw.starts_with(word) && !word.is_empty() {
                completions.push(Pair {
                    display: kw.to_string(),
                    replacement: kw.to_string(),
                });
            }
        }

        // Complete binding names
        let bindings = self.bindings.borrow();
        for binding in bindings.iter() {
            if binding.name.starts_with(word) && !word.is_empty() {
                completions.push(Pair {
                    display: binding.name.clone(),
                    replacement: binding.name.clone(),
                });
            }
        }

        // Complete module names (with :: suffix)
        let registry = self.registry.borrow();
        for module in registry.get_module_names() {
            if module.starts_with(word) && !word.is_empty() {
                completions.push(Pair {
                    display: format!("{}::", module),
                    replacement: format!("{}::", module),
                });
            }
        }

        // Also suggest struct names (capitalized modules) for Struct::method syntax
        for module in registry.get_module_names() {
            let struct_name = capitalize_first(&module);
            if struct_name.starts_with(word) && !word.is_empty() {
                completions.push(Pair {
                    display: format!("{}::", struct_name),
                    replacement: format!("{}::", struct_name),
                });
            }
        }

        Ok((start, completions))
    }
}

impl Hinter for ReplHelper {
    type Hint = String;
}

impl Highlighter for ReplHelper {}
impl Validator for ReplHelper {}
impl Helper for ReplHelper {}

/// Find the start of the word at position
fn find_word_start(line: &str, pos: usize) -> (usize, &str) {
    let line = &line[..pos];
    let start = line
        .rfind(|c: char| c.is_whitespace() || c == '(' || c == '[' || c == '{' || c == ',')
        .map(|i| i + 1)
        .unwrap_or(0);
    (start, &line[start..])
}

/// Result from the BEAM eval server (ok: or err: line)
type BeamResult = Result<String, String>;

/// A simple printer that writes directly to stdout
/// Used as fallback when ExternalPrinter is not available (e.g., non-TTY)
struct StdoutPrinter;

impl ExternalPrinter for StdoutPrinter {
    fn print(&mut self, msg: String) -> rustyline::Result<()> {
        // In non-TTY mode, just print directly
        // This may interleave with prompts but works for testing
        print!("{}", msg);
        std::io::stdout().flush().ok();
        Ok(())
    }
}

/// REPL state
struct ReplState {
    bindings: SharedBindings,
    registry: SharedRegistry,
    beam_process: Option<Child>,
    beam_stdin: Option<std::process::ChildStdin>,
    /// Channel receiver for command results only
    result_rx: Option<Receiver<BeamResult>>,
    stdlib_path: Option<String>,
    temp_dir: std::path::PathBuf,
    /// Last edited content from :edit command (for iterating)
    last_edit: Option<String>,
    /// Extra code paths (for deps, project beam files, etc.)
    extra_paths: Vec<std::path::PathBuf>,
    /// Application to start (if running with -S)
    app_name: Option<String>,
}

impl ReplState {
    fn new() -> Self {
        let stdlib_path = find_stdlib_path();
        let temp_dir = std::env::temp_dir();

        Self {
            bindings: Rc::new(RefCell::new(Vec::new())),
            registry: Rc::new(RefCell::new(ModuleRegistry::new())),
            beam_process: None,
            beam_stdin: None,
            result_rx: None,
            stdlib_path,
            temp_dir,
            last_edit: None,
            extra_paths: Vec::new(),
            app_name: None,
        }
    }

    fn with_app(app_name: String, beam_dir: std::path::PathBuf, deps_dirs: Vec<std::path::PathBuf>) -> Self {
        let stdlib_path = find_stdlib_path();
        let temp_dir = std::env::temp_dir();

        let mut extra_paths = vec![beam_dir];
        extra_paths.extend(deps_dirs);

        Self {
            bindings: Rc::new(RefCell::new(Vec::new())),
            registry: Rc::new(RefCell::new(ModuleRegistry::new())),
            beam_process: None,
            beam_stdin: None,
            result_rx: None,
            stdlib_path,
            temp_dir,
            last_edit: None,
            extra_paths,
            app_name: Some(app_name),
        }
    }

    /// Start the BEAM process if not already running
    /// The printer is used by the reader thread to print output while readline is blocking
    fn ensure_beam_running<P: ExternalPrinter + Send + 'static>(
        &mut self,
        mut printer: P,
    ) -> Result<(), String> {
        if self.beam_process.is_some() {
            return Ok(());
        }

        // Minify the eval server code (remove newlines)
        let eval_code = EVAL_SERVER
            .lines()
            .map(|l| l.trim())
            .collect::<Vec<_>>()
            .join(" ");

        // If we have an app to start, add that to the eval code
        let eval_code = if let Some(ref app_name) = self.app_name {
            format!(
                "{}, application:ensure_all_started({}).",
                eval_code, app_name
            )
        } else {
            format!("{}.", eval_code)
        };

        let mut cmd = Command::new("erl");
        cmd.arg("-noshell");
        cmd.arg("-pa").arg(&self.temp_dir);

        if let Some(ref stdlib) = self.stdlib_path {
            cmd.arg("-pa").arg(stdlib);
        }

        // Add extra paths (project beam files, deps, etc.)
        for path in &self.extra_paths {
            cmd.arg("-pa").arg(path);
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

        // Create channel for results only
        let (tx, rx) = mpsc::channel::<BeamResult>();

        // Spawn background thread to read stdout
        // - Output lines are printed immediately via ExternalPrinter
        // - Result lines (after RESULT_MARKER) are sent through channel
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
                            // Next line(s) will contain result
                            waiting_for_result = true;
                            continue;
                        }

                        if waiting_for_result {
                            // Check for result lines
                            if let Some(value) = trimmed.strip_prefix("ok:") {
                                let _ = tx.send(Ok(value.to_string()));
                                waiting_for_result = false;
                            } else if let Some(err) = trimmed.strip_prefix("err:") {
                                let _ = tx.send(Err(err.to_string()));
                                waiting_for_result = false;
                            } else if !trimmed.is_empty() {
                                // Output during result waiting (from spawned process)
                                let _ = printer.print(line);
                            }
                        } else if !trimmed.is_empty() {
                            // Regular output (from io::println, logger, etc.)
                            let _ = printer.print(line);
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

    /// Send a command to the BEAM server and get the response
    /// BEAM must be running (call ensure_beam_running first)
    fn send_command(&mut self, cmd: &str) -> Result<String, String> {
        let stdin = self.beam_stdin.as_mut().ok_or("BEAM not running")?;
        writeln!(stdin, "{}", cmd).map_err(|e| format!("Failed to send: {}", e))?;
        stdin.flush().map_err(|e| format!("Failed to flush: {}", e))?;

        // Wait for result from channel (output is printed by reader thread)
        let rx = self.result_rx.as_ref().ok_or("BEAM not running")?;
        rx.recv().map_err(|_| "BEAM process terminated".to_string())?
    }

    /// Load module registry by introspecting the BEAM
    fn load_registry(&mut self) -> Result<(), String> {
        // Get list of surreal:: modules
        let modules_str = self.send_command("introspect:list_modules")?;

        // Parse the module list: ['surreal::map', 'surreal::string', ...]
        let modules = parse_atom_list(&modules_str);

        for module in modules {
            // Get exports for each module
            let cmd = format!("introspect:exports:{}", module);
            if let Ok(exports_str) = self.send_command(&cmd) {
                let exports = parse_exports(&exports_str);
                self.registry.borrow_mut().add_module(&module, &exports);
            }
        }

        Ok(())
    }

    /// Compile an expression to Core Erlang, injecting binding fetches
    fn compile_expr(&mut self, expr_source: &str) -> Result<(String, String), String> {
        let counter = EVAL_COUNTER.fetch_add(1, Ordering::SeqCst);
        let module_name = format!("__repl_{}", counter);

        // Generate Dream source (bindings will be injected into Core Erlang)
        let (surreal_source, expr_offset) = self.generate_surreal_source(&module_name, expr_source);

        // Parse
        let mut parser = Parser::new(&surreal_source);
        let modules = parser.parse_file_modules(&module_name).map_err(|e| {
            format_repl_error(expr_source, expr_offset, &e.message, e.span, e.help.as_deref())
        })?;

        if modules.is_empty() {
            return Err("No module parsed".to_string());
        }

        // Type check
        let type_results = check_modules(&modules);
        let mut annotated_modules = Vec::new();
        for (_mod_name, result) in type_results {
            match result {
                Ok(annotated) => annotated_modules.push(annotated),
                Err(e) => {
                    return Err(format_repl_error(
                        expr_source,
                        expr_offset,
                        &e.message,
                        e.span.unwrap_or_else(|| SourceSpan::new(0.into(), 0)),
                        e.help.as_deref(),
                    ));
                }
            }
        }

        // Resolve stdlib method calls (e.g., Some(1).map(f) -> option::map(Some(1), f))
        let mut module = annotated_modules.remove(0);
        resolve_stdlib_methods(&mut module);

        // Compile to Core Erlang
        let registry = Arc::new(RwLock::new(GenericFunctionRegistry::new()));

        // Prefix with surreal::
        let mut prefixed_module = module;
        prefixed_module.name = format!("surreal::{}", prefixed_module.name);

        let module_context = ModuleContext::default();
        let mut emitter = CoreErlangEmitter::with_registry_and_context(registry, module_context);

        let core_erlang = emitter
            .emit_module(&prefixed_module)
            .map_err(|e| format!("Codegen error: {}", e))?;

        // Inject binding fetches into Core Erlang
        let core_erlang = self.inject_bindings_into_core_erlang(&core_erlang);

        Ok((prefixed_module.name, core_erlang))
    }

    /// Evaluate an expression using the full compiler
    fn eval_expr(&mut self, expr_source: &str) -> Result<String, String> {
        let (module_name, core_erlang) = self.compile_expr(expr_source)?;

        // Write and evaluate
        let core_file = self.temp_dir.join(format!("{}.core", module_name));
        std::fs::write(&core_file, &core_erlang)
            .map_err(|e| format!("Failed to write Core Erlang: {}", e))?;

        let cmd = format!("eval:{}", core_file.display());
        let result = self.send_command(&cmd);

        let _ = std::fs::remove_file(&core_file);

        result.map(|v| format_surreal_value(&v))
    }

    /// Evaluate an expression and bind the result to a name
    fn eval_and_bind(&mut self, name: &str, expr_source: &str) -> Result<String, String> {
        let (module_name, core_erlang) = self.compile_expr(expr_source)?;

        // Write Core Erlang file
        let core_file = self.temp_dir.join(format!("{}.core", module_name));
        std::fs::write(&core_file, &core_erlang)
            .map_err(|e| format!("Failed to write Core Erlang: {}", e))?;

        // Use bind: command to evaluate and store in process dictionary
        let cmd = format!("bind:{}:{}", name, core_file.display());
        let result = self.send_command(&cmd);

        let _ = std::fs::remove_file(&core_file);

        // If successful, record the binding
        if result.is_ok() {
            self.add_binding(name.to_string());
        }

        result.map(|v| format_surreal_value(&v))
    }

    /// Generate Dream source code wrapping an expression
    /// Uses an extern declaration for binding retrieval to get proper `any` type
    /// Returns (source, expr_offset) where expr_offset is the byte offset of the user expression
    fn generate_surreal_source(&self, module_name: &str, expr_source: &str) -> (String, usize) {
        let bindings = self.bindings.borrow();

        let mut source = format!("mod {} {{\n", module_name);

        // Declare extern function for binding retrieval if we have bindings
        // This gives us proper `any` return type that the type checker respects
        if !bindings.is_empty() {
            source.push_str("    extern mod __repl_bindings {\n");
            for binding in bindings.iter() {
                source.push_str(&format!(
                    "        fn __get_{}__() -> any;\n",
                    binding.name
                ));
            }
            source.push_str("    }\n\n");
        }

        source.push_str("    pub fn __eval__() -> any {\n");

        // Bind each variable by calling the extern function
        for binding in bindings.iter() {
            source.push_str(&format!(
                "        let {} = __repl_bindings::__get_{}__();\n",
                binding.name, binding.name
            ));
        }

        // Track where the user expression starts (after 8 spaces of indentation)
        let expr_offset = source.len() + 8; // 8 spaces of indentation

        source.push_str(&format!("        {}\n", expr_source));
        source.push_str("    }\n");
        source.push_str("}\n");
        (source, expr_offset)
    }

    /// Replace extern binding calls with erlang:get in Core Erlang
    /// Transforms: call '__repl_bindings':'__get_x__'() -> call 'erlang':'get'('repl_x')
    fn inject_bindings_into_core_erlang(&self, core_erlang: &str) -> String {
        let bindings = self.bindings.borrow();
        if bindings.is_empty() {
            return core_erlang.to_string();
        }

        let mut result = core_erlang.to_string();

        // Replace each extern call with an erlang:get call
        for binding in bindings.iter() {
            let extern_call = format!("call '__repl_bindings':'__get_{}__'()", binding.name);
            let replacement = format!("call 'erlang':'get'('repl_{}')", binding.name);
            result = result.replace(&extern_call, &replacement);
        }

        result
    }

    fn add_binding(&mut self, name: String) {
        let mut bindings = self.bindings.borrow_mut();
        bindings.retain(|b| b.name != name);
        bindings.push(Binding { name });
    }

    fn clear_bindings(&mut self) {
        self.bindings.borrow_mut().clear();
    }
}

impl Drop for ReplState {
    fn drop(&mut self) {
        self.beam_stdin.take();
        if let Some(ref mut child) = self.beam_process {
            let _ = child.kill();
            let _ = child.wait();
        }
    }
}

/// Parse a list of atoms from Erlang format: ['atom1', 'atom2', ...]
fn parse_atom_list(s: &str) -> Vec<String> {
    let s = s.trim();
    if s == "[]" {
        return Vec::new();
    }

    let inner = s.strip_prefix('[').and_then(|s| s.strip_suffix(']')).unwrap_or(s);

    inner
        .split(',')
        .filter_map(|atom| {
            let atom = atom.trim();
            atom.strip_prefix('\'')
                .and_then(|a| a.strip_suffix('\''))
                .map(|a| a.to_string())
        })
        .collect()
}

/// Parse exports from Erlang format: [{func, arity}, ...]
fn parse_exports(s: &str) -> Vec<(String, u8)> {
    let s = s.trim();
    if s == "[]" {
        return Vec::new();
    }

    let mut exports = Vec::new();
    let inner = s.strip_prefix('[').and_then(|s| s.strip_suffix(']')).unwrap_or(s);

    // Simple parser for {name, arity} tuples
    let mut i = 0;
    let chars: Vec<char> = inner.chars().collect();

    while i < chars.len() {
        if chars[i] == '{' {
            let start = i + 1;
            while i < chars.len() && chars[i] != '}' {
                i += 1;
            }
            let tuple_content = &inner[start..i];
            if let Some((name, arity)) = parse_export_tuple(tuple_content) {
                exports.push((name, arity));
            }
        }
        i += 1;
    }

    exports
}

fn parse_export_tuple(s: &str) -> Option<(String, u8)> {
    let parts: Vec<&str> = s.split(',').collect();
    if parts.len() != 2 {
        return None;
    }

    let name = parts[0].trim();
    let name = name.strip_prefix('\'').and_then(|n| n.strip_suffix('\''))
        .unwrap_or(name);

    let arity: u8 = parts[1].trim().parse().ok()?;

    Some((name.to_string(), arity))
}

/// Find stdlib path
fn find_stdlib_path() -> Option<String> {
    if let Ok(exe_path) = std::env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            let stdlib = exe_dir.join("../stdlib");
            if stdlib.exists() {
                return stdlib.canonicalize().ok().map(|p| p.to_string_lossy().into_owned());
            }
        }
    }

    let target_stdlib = std::path::Path::new("target/stdlib");
    if target_stdlib.exists() {
        return target_stdlib.canonicalize().ok().map(|p| p.to_string_lossy().into_owned());
    }

    None
}

fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
    }
}

/// Format Erlang value as Dream syntax
fn format_surreal_value(value: &str) -> String {
    let value = value.trim();

    if value == "none" {
        return "None".to_string();
    }
    if let Some(inner) = value.strip_prefix("{some,").and_then(|s| s.strip_suffix('}')) {
        return format!("Some({})", format_surreal_value(inner.trim()));
    }
    if let Some(inner) = value.strip_prefix("{ok,").and_then(|s| s.strip_suffix('}')) {
        return format!("Ok({})", format_surreal_value(inner.trim()));
    }
    if let Some(inner) = value.strip_prefix("{error,").and_then(|s| s.strip_suffix('}')) {
        return format!("Err({})", format_surreal_value(inner.trim()));
    }

    if let Some(inner) = value.strip_prefix('\'').and_then(|s| s.strip_suffix('\'')) {
        if inner == "true" || inner == "false" || inner == "ok" || inner == "error" {
            return inner.to_string();
        }
        return format!(":{}", inner);
    }

    if let Some(inner) = value.strip_prefix("<<\"").and_then(|s| s.strip_suffix("\">>")) {
        return format!("\"{}\"", inner);
    }

    if value.starts_with("#{") && value.contains("'__struct__'") {
        if let Some(struct_start) = value.find("'__struct__' => '") {
            let after_struct = &value[struct_start + 17..];
            if let Some(struct_end) = after_struct.find('\'') {
                let struct_path = &after_struct[..struct_end];
                let struct_name = struct_path.split("::").last().unwrap_or(struct_path);

                // Check for wrapped struct format: data => #{...}
                if let Some(data_start) = value.find("data => ") {
                    let after_data = &value[data_start + 8..];
                    if let Some(data_content) = extract_map_content(after_data) {
                        if struct_name == "Map" {
                            if data_content == "#{}" {
                                return "{}".to_string();
                            }
                            return format!("{{ {} }}", format_map_fields(&data_content));
                        }
                        if data_content == "#{}" {
                            return format!("{}()", struct_name);
                        }
                        return format!("{}({})", struct_name, format_map_fields(&data_content));
                    }
                } else {
                    // Direct struct format: #{field => val, '__struct__' => 'Type', ...}
                    // Extract fields excluding __struct__
                    let fields = format_struct_fields(value, struct_name);
                    if fields.is_empty() {
                        return format!("{} {{}}", struct_name);
                    }
                    return format!("{} {{ {} }}", struct_name, fields);
                }
            }
        }
    }

    value.to_string()
}

fn extract_map_content(s: &str) -> Option<String> {
    if !s.starts_with("#{") {
        return None;
    }
    let mut depth = 0;
    let mut end_idx = 0;
    for (i, c) in s.chars().enumerate() {
        match c {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    end_idx = i + 1;
                    break;
                }
            }
            _ => {}
        }
    }
    if end_idx > 0 { Some(s[..end_idx].to_string()) } else { None }
}

fn format_map_fields(map_str: &str) -> String {
    let inner = map_str.strip_prefix("#{").and_then(|s| s.strip_suffix('}'));
    if let Some(inner) = inner {
        if inner.is_empty() { return String::new(); }
        inner.replace(" => ", ": ")
    } else {
        map_str.to_string()
    }
}

/// Format struct fields from an Erlang map, excluding __struct__
/// Input: #{name => <<"Sonny">>,'__struct__' => 'repl_edit::User',age => 45}
/// Output: name: "Sonny", age: 45
fn format_struct_fields(map_str: &str, _struct_name: &str) -> String {
    let inner = map_str
        .strip_prefix("#{")
        .and_then(|s| s.strip_suffix('}'));

    let Some(inner) = inner else {
        return String::new();
    };

    if inner.is_empty() {
        return String::new();
    }

    // Parse the key => value pairs, handling nested structures
    // We need to handle Erlang binaries like <<"text">> specially
    let mut fields = Vec::new();
    let mut current = String::new();
    let mut depth = 0;
    let mut in_binary = false;
    let chars: Vec<char> = inner.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let c = chars[i];

        // Check for binary start: <<
        if c == '<' && i + 1 < chars.len() && chars[i + 1] == '<' {
            in_binary = true;
            current.push('<');
            current.push('<');
            i += 2;
            continue;
        }

        // Check for binary end: >>
        if c == '>' && i + 1 < chars.len() && chars[i + 1] == '>' && in_binary {
            in_binary = false;
            current.push('>');
            current.push('>');
            i += 2;
            continue;
        }

        match c {
            '{' | '[' | '(' => {
                depth += 1;
                current.push(c);
            }
            '}' | ']' | ')' => {
                depth -= 1;
                current.push(c);
            }
            ',' if depth == 0 && !in_binary => {
                let field = current.trim();
                if !field.is_empty() && !field.contains("'__struct__'") {
                    fields.push(format_single_field(field));
                }
                current.clear();
            }
            _ => current.push(c),
        }
        i += 1;
    }

    // Don't forget the last field
    let field = current.trim();
    if !field.is_empty() && !field.contains("'__struct__'") {
        fields.push(format_single_field(field));
    }

    fields.join(", ")
}

/// Format a single field: "name => <<"Sonny">>" -> "name: \"Sonny\""
fn format_single_field(field: &str) -> String {
    if let Some(arrow_pos) = field.find(" => ") {
        let key = field[..arrow_pos].trim();
        let value = field[arrow_pos + 4..].trim();

        // Clean up atom keys (remove quotes)
        let clean_key = key.trim_matches('\'');

        // Format the value
        let formatted_value = format_surreal_value(value);

        format!("{}: {}", clean_key, formatted_value)
    } else {
        field.to_string()
    }
}

fn print_banner() {
    println!("Dream {} (BEAM backend)", env!("CARGO_PKG_VERSION"));
    println!("Type :help for commands, :quit to exit");
    println!();
}

fn print_help() {
    println!("Commands:");
    println!("  :help           Show this help message");
    println!("  :quit, :q       Exit the shell");
    println!("  :clear          Clear all bindings");
    println!("  :bindings       Show current bindings");
    println!("  :reload         Reload module registry");
    println!("  :edit, :e       Open $EDITOR to write Dream code");
    println!("  :load <file>    Compile and load a .sur file");
    println!();
    println!("Enter Dream expressions to evaluate them.");
    println!("Use 'let x = expr' to create bindings.");
    println!("Press TAB for completion.");
}

/// Run the interactive shell
pub fn run_shell() -> ExitCode {
    print_banner();

    let mut state = ReplState::new();

    // Create Editor first so we can get an ExternalPrinter for BEAM output
    let helper = ReplHelper::new(Rc::clone(&state.bindings), Rc::clone(&state.registry));
    let mut rl = match Editor::new() {
        Ok(editor) => editor,
        Err(e) => {
            eprintln!("Failed to initialize readline: {}", e);
            return ExitCode::from(1);
        }
    };
    rl.set_helper(Some(helper));

    // Start BEAM with a printer for real-time output from spawned processes
    // Try ExternalPrinter first (works with TTY), fall back to StdoutPrinter
    print!("Loading modules...");
    std::io::stdout().flush().ok();

    let start_result = if let Ok(printer) = rl.create_external_printer() {
        state.ensure_beam_running(printer)
    } else {
        // Fallback for non-TTY mode (e.g., piped input for testing)
        state.ensure_beam_running(StdoutPrinter)
    };

    if let Err(e) = start_result {
        eprintln!(" failed to start BEAM: {}", e);
        return ExitCode::from(1);
    }

    if let Err(e) = state.load_registry() {
        println!(" failed: {}", e);
    } else {
        let count = state.registry.borrow().modules.len();
        println!(" {} modules loaded.", count);
    }
    println!();

    loop {
        let readline = rl.readline("surreal> ");
        match readline {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }

                let _ = rl.add_history_entry(line);

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
                            let bindings = state.bindings.borrow();
                            if bindings.is_empty() {
                                println!("No bindings.");
                            } else {
                                for binding in bindings.iter() {
                                    println!("  {}", binding.name);
                                }
                            }
                            continue;
                        }
                        ":reload" => {
                            print!("Reloading modules...");
                            std::io::stdout().flush().ok();
                            state.registry.borrow_mut().modules.clear();
                            if let Err(e) = state.load_registry() {
                                println!(" failed: {}", e);
                            } else {
                                let count = state.registry.borrow().modules.len();
                                println!(" {} modules loaded.", count);
                            }
                            continue;
                        }
                        ":edit" | ":e" => {
                            match edit_and_eval(&mut state) {
                                Ok(Some(result)) => println!("{}", result),
                                Ok(None) => {} // Empty file or cancelled
                                Err(e) => eprintln!("Error: {}", e),
                            }
                            continue;
                        }
                        cmd if cmd.starts_with(":load ") => {
                            let file = cmd.strip_prefix(":load ").unwrap().trim();
                            if file.is_empty() {
                                eprintln!("Usage: :load <file.sur>");
                            } else {
                                match load_and_compile(&mut state, file) {
                                    Ok(msg) => println!("{}", msg),
                                    Err(e) => eprintln!("Error: {}", e),
                                }
                            }
                            continue;
                        }
                        _ => {
                            eprintln!("Unknown command: {}", line);
                            eprintln!("Type :help for available commands.");
                            continue;
                        }
                    }
                }

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

/// Run the interactive shell with an application loaded (like `iex -S mix`)
pub fn run_shell_with_app(
    app_name: String,
    beam_dir: std::path::PathBuf,
    deps_dirs: Vec<std::path::PathBuf>,
) -> ExitCode {
    println!("Dream {} (BEAM backend)", env!("CARGO_PKG_VERSION"));
    println!("Starting application '{}'...", app_name);
    println!();

    let mut state = ReplState::with_app(app_name.clone(), beam_dir, deps_dirs);

    // Create Editor first so we can get an ExternalPrinter for BEAM output
    let helper = ReplHelper::new(Rc::clone(&state.bindings), Rc::clone(&state.registry));
    let mut rl = match Editor::new() {
        Ok(editor) => editor,
        Err(e) => {
            eprintln!("Failed to initialize readline: {}", e);
            return ExitCode::from(1);
        }
    };
    rl.set_helper(Some(helper));

    // Start BEAM with the app loaded
    print!("Loading modules and starting application...");
    std::io::stdout().flush().ok();

    let start_result = if let Ok(printer) = rl.create_external_printer() {
        state.ensure_beam_running(printer)
    } else {
        state.ensure_beam_running(StdoutPrinter)
    };

    if let Err(e) = start_result {
        eprintln!(" failed: {}", e);
        return ExitCode::from(1);
    }

    if let Err(e) = state.load_registry() {
        println!(" failed to load registry: {}", e);
    } else {
        let count = state.registry.borrow().modules.len();
        println!(" {} modules loaded.", count);
    }

    println!();
    println!("Application '{}' started. Type :help for commands, :quit to exit.", app_name);
    println!();

    // Main REPL loop (same as run_shell)
    loop {
        let readline = rl.readline("surreal> ");
        match readline {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }

                let _ = rl.add_history_entry(line);

                if line.starts_with(':') {
                    match line {
                        ":quit" | ":q" => {
                            println!("Stopping application and exiting...");
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
                            let bindings = state.bindings.borrow();
                            if bindings.is_empty() {
                                println!("No bindings.");
                            } else {
                                for binding in bindings.iter() {
                                    println!("  {}", binding.name);
                                }
                            }
                            continue;
                        }
                        ":reload" => {
                            print!("Reloading modules...");
                            std::io::stdout().flush().ok();
                            state.registry.borrow_mut().modules.clear();
                            if let Err(e) = state.load_registry() {
                                println!(" failed: {}", e);
                            } else {
                                let count = state.registry.borrow().modules.len();
                                println!(" {} modules loaded.", count);
                            }
                            continue;
                        }
                        ":edit" | ":e" => {
                            match edit_and_eval(&mut state) {
                                Ok(Some(result)) => println!("{}", result),
                                Ok(None) => {}
                                Err(e) => eprintln!("Error: {}", e),
                            }
                            continue;
                        }
                        cmd if cmd.starts_with(":load ") => {
                            let file = cmd.strip_prefix(":load ").unwrap().trim();
                            if file.is_empty() {
                                eprintln!("Usage: :load <file.sur>");
                            } else {
                                match load_and_compile(&mut state, file) {
                                    Ok(msg) => println!("{}", msg),
                                    Err(e) => eprintln!("Error: {}", e),
                                }
                            }
                            continue;
                        }
                        _ => {
                            eprintln!("Unknown command: {}", line);
                            eprintln!("Type :help for available commands.");
                            continue;
                        }
                    }
                }

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
                println!("Stopping application and exiting...");
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

/// Default template for :edit when no previous edit exists
const EDIT_TEMPLATE: &str = r#"// Define modules here. All code must be inside mod blocks.
// After saving, call functions from the REPL prompt.

mod repl_edit {
    pub fn add(a: int, b: int) -> int {
        a + b
    }
}
"#;

/// Open a temp file in the user's editor and compile/run it
fn edit_and_eval(state: &mut ReplState) -> Result<Option<String>, String> {
    // Create a temporary file with .sur extension
    let temp_file = state.temp_dir.join("surreal_repl_edit.sur");

    // Use last edit if available, otherwise use template
    let initial_content = state.last_edit.as_deref().unwrap_or(EDIT_TEMPLATE);
    std::fs::write(&temp_file, initial_content)
        .map_err(|e| format!("Failed to create temp file: {}", e))?;

    // Get editor from EDITOR env var, default to vim
    let editor = std::env::var("EDITOR").unwrap_or_else(|_| "vim".to_string());

    // Open the editor
    let status = Command::new(&editor)
        .arg(&temp_file)
        .status()
        .map_err(|e| format!("Failed to open editor '{}': {}", editor, e))?;

    if !status.success() {
        let _ = std::fs::remove_file(&temp_file);
        return Err(format!("Editor '{}' exited with error", editor));
    }

    // Read the file contents
    let content = std::fs::read_to_string(&temp_file)
        .map_err(|e| format!("Failed to read temp file: {}", e))?;

    // Debug: show what we're about to parse if DREAM_DEBUG is set
    if std::env::var("DREAM_DEBUG").is_ok() {
        eprintln!("=== DEBUG: Content to parse ({} bytes) ===", content.len());
        eprintln!("{:?}", content);
        eprintln!("=== END DEBUG ===");
    }

    // Clean up temp file
    let _ = std::fs::remove_file(&temp_file);

    let content = content.trim();
    if content.is_empty() {
        return Ok(None);
    }

    // Store the content for next edit (even if compilation fails, so user can fix errors)
    state.last_edit = Some(content.to_string());

    // Compile and run
    compile_and_run_source(state, content, "repl_edit")
}

/// Load and compile a .sur file
fn load_and_compile(state: &mut ReplState, path: &str) -> Result<String, String> {
    let path = std::path::Path::new(path);
    if !path.exists() {
        return Err(format!("File not found: {}", path.display()));
    }

    let content = std::fs::read_to_string(path)
        .map_err(|e| format!("Failed to read file: {}", e))?;

    let module_name = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("loaded");

    compile_and_run_source(state, &content, module_name)?;
    Ok(format!("Loaded {}", path.display()))
}

/// Compile Dream source code and load modules into BEAM
fn compile_and_run_source(
    state: &mut ReplState,
    source: &str,
    fallback_name: &str,
) -> Result<Option<String>, String> {
    // Parse the source
    let mut parser = Parser::new(source);
    let modules = parser.parse_file_modules(fallback_name).map_err(|e| {
        let err = CompilerError::parse(fallback_name, source, e);
        format!("{:?}", miette::Report::new(err))
    })?;

    if modules.is_empty() {
        return Ok(None);
    }

    // Type check
    let type_results = check_modules(&modules);
    let mut annotated_modules = Vec::new();
    for (_module_name, result) in type_results {
        match result {
            Ok(annotated) => annotated_modules.push(annotated),
            Err(e) => {
                let err = CompilerError::type_error(fallback_name, source, e);
                return Err(format!("{:?}", miette::Report::new(err)));
            }
        }
    }

    // Compile each module to Core Erlang and load into BEAM
    let registry = Arc::new(RwLock::new(GenericFunctionRegistry::new()));
    let mut loaded_modules = Vec::new();

    for module in &annotated_modules {
        // Don't prefix REPL-edited modules - let users call them directly
        let prefixed_module = module.clone();

        let mut module_context = ModuleContext::default();
        module_context.skip_stdlib_prefix = true;  // REPL modules don't get surreal:: prefix
        let mut emitter = CoreErlangEmitter::with_registry_and_context(registry.clone(), module_context);

        let core_erlang = emitter
            .emit_module(&prefixed_module)
            .map_err(|e| format!("Codegen error: {}", e))?;

        // Write Core Erlang to temp file
        let core_file = state.temp_dir.join(format!("{}.core", prefixed_module.name));
        std::fs::write(&core_file, &core_erlang)
            .map_err(|e| format!("Failed to write Core Erlang: {}", e))?;

        // Load into BEAM (persistent load)
        let cmd = format!("load:{}", core_file.display());
        match state.send_command(&cmd) {
            Ok(_) => {
                loaded_modules.push(module.name.clone());

                // Extract exports from AST for tab completion (pub functions)
                let mut exports: Vec<(String, u8)> = Vec::new();
                for item in &module.items {
                    if let Item::Function(func) = item {
                        if func.is_pub {
                            exports.push((func.name.clone(), func.params.len() as u8));
                        }
                    }
                }
                state.registry.borrow_mut().add_module(&prefixed_module.name, &exports);
            }
            Err(e) => {
                let _ = std::fs::remove_file(&core_file);
                return Err(format!("BEAM compile error: {}", e));
            }
        }

        let _ = std::fs::remove_file(&core_file);
    }

    Ok(Some(format!("Loaded: {}", loaded_modules.join(", "))))
}

fn parse_and_eval(state: &mut ReplState, input: &str) -> Result<String, String> {
    let input = input.trim();

    if input.starts_with("let ") {
        return parse_and_eval_let(state, input);
    }

    // Evaluate the expression using the full compiler
    state.eval_expr(input)
}

fn parse_and_eval_let(state: &mut ReplState, input: &str) -> Result<String, String> {
    let input = input.trim().strip_prefix("let ").unwrap();
    let eq_pos = input.find('=').ok_or("Expected '=' in let statement")?;

    let name = input[..eq_pos].trim().to_string();
    let expr_source = input[eq_pos + 1..].trim().to_string();

    if name.is_empty() || !name.chars().next().unwrap().is_alphabetic() {
        return Err("Invalid variable name".to_string());
    }

    // Evaluate and store the value in BEAM process dictionary
    state.eval_and_bind(&name, &expr_source)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_exports() {
        // Test parsing Erlang export list format
        let exports = parse_exports("[{add,2},{subtract,2},{main,0}]");
        assert_eq!(exports.len(), 3);
        assert!(exports.contains(&("add".to_string(), 2)));
        assert!(exports.contains(&("subtract".to_string(), 2)));
        assert!(exports.contains(&("main".to_string(), 0)));
    }

    #[test]
    fn test_parse_exports_empty() {
        let exports = parse_exports("[]");
        assert!(exports.is_empty());
    }

    #[test]
    fn test_parse_exports_includes_module_info() {
        // parse_exports doesn't filter - that happens in add_module
        let exports = parse_exports("[{add,2},{module_info,0},{module_info,1}]");
        assert_eq!(exports.len(), 3);
    }

    #[test]
    fn test_module_registry_filters_module_info() {
        // add_module filters out module_info
        let mut registry = ModuleRegistry::new();
        let exports = vec![
            ("add".to_string(), 2),
            ("module_info".to_string(), 0),
            ("module_info".to_string(), 1),
        ];
        registry.add_module("surreal::math", &exports);

        let info = registry.modules.get("math").unwrap();
        assert!(info.functions.contains_key("add"));
        assert!(!info.functions.contains_key("module_info"));
    }

    #[test]
    fn test_module_registry_add_module() {
        let mut registry = ModuleRegistry::new();
        let exports = vec![
            ("add".to_string(), 2),
            ("multiply".to_string(), 2),
        ];
        registry.add_module("surreal::math", &exports);

        // Should be stored under short name "math"
        assert!(registry.modules.contains_key("math"));
        let info = registry.modules.get("math").unwrap();
        assert!(info.functions.contains_key("add"));
        assert!(info.functions.contains_key("multiply"));
    }

    #[test]
    fn test_module_registry_get_completions() {
        let mut registry = ModuleRegistry::new();
        let exports = vec![
            ("add".to_string(), 2),
            ("add_all".to_string(), 1),
            ("multiply".to_string(), 2),
        ];
        registry.add_module("surreal::math", &exports);

        let completions = registry.get_completions("math");
        assert_eq!(completions.len(), 3);
        assert!(completions.contains(&"add".to_string()));
        assert!(completions.contains(&"add_all".to_string()));
        assert!(completions.contains(&"multiply".to_string()));
    }

    #[test]
    fn test_module_registry_get_module_names() {
        let mut registry = ModuleRegistry::new();
        registry.add_module("surreal::string", &[("reverse".to_string(), 1)]);
        registry.add_module("surreal::io", &[("println".to_string(), 1)]);

        let names = registry.get_module_names();
        assert!(names.contains(&"string".to_string()));
        assert!(names.contains(&"io".to_string()));
    }

    #[test]
    fn test_capitalize_first() {
        assert_eq!(capitalize_first("hello"), "Hello");
        assert_eq!(capitalize_first("WORLD"), "WORLD");
        assert_eq!(capitalize_first("a"), "A");
        assert_eq!(capitalize_first(""), "");
    }

    #[test]
    fn test_format_surreal_value_quoted_atoms() {
        // Atoms come from Erlang with single quotes
        assert_eq!(format_surreal_value("'ok'"), "ok");
        assert_eq!(format_surreal_value("'error'"), "error");
        assert_eq!(format_surreal_value("'true'"), "true");
        assert_eq!(format_surreal_value("'false'"), "false");
        assert_eq!(format_surreal_value("'my_atom'"), ":my_atom");
    }

    #[test]
    fn test_format_surreal_value_numbers() {
        assert_eq!(format_surreal_value("42"), "42");
        assert_eq!(format_surreal_value("-123"), "-123");
    }

    #[test]
    fn test_format_surreal_value_strings() {
        assert_eq!(format_surreal_value("<<\"hello\">>"), "\"hello\"");
    }

    #[test]
    fn test_format_surreal_value_result_types() {
        // {ok, value} and {error, value} are formatted as Ok/Err
        assert_eq!(format_surreal_value("{ok,42}"), "Ok(42)");
        assert_eq!(format_surreal_value("{error,42}"), "Err(42)");
    }

    #[test]
    fn test_format_surreal_value_option_types() {
        assert_eq!(format_surreal_value("none"), "None");
        assert_eq!(format_surreal_value("{some,42}"), "Some(42)");
    }

    #[test]
    fn test_format_surreal_value_lists() {
        // Lists pass through as-is
        assert_eq!(format_surreal_value("[1,2,3]"), "[1,2,3]");
        assert_eq!(format_surreal_value("[]"), "[]");
    }
}
