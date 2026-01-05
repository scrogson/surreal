//! Dream CLI - Build and run Dream programs.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};

use clap::{Parser, Subcommand};

use dream::{
    compiler::{check_modules, resolve_stdlib_methods, CompilerError, CoreErlangEmitter, Module, ModuleLoader},
    config::{generate_dream_toml, generate_main_dream, ProjectConfig},
};

#[derive(Parser)]
#[command(name = "dream")]
#[command(author, version, about = "Dream programming language", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Create a new Dream project
    New {
        /// Name of the project
        name: String,
    },
    /// Build the project or a single file
    Build {
        /// Source file to compile (optional, uses project if not specified)
        file: Option<PathBuf>,
        /// Target: beam, core, or vm
        #[arg(long, short, default_value = "beam")]
        target: String,
        /// Output directory
        #[arg(long, short)]
        output: Option<PathBuf>,
    },
    /// Compile the project or a single file (alias for build)
    Compile {
        /// Source file to compile (optional, uses project if not specified)
        file: Option<PathBuf>,
        /// Target: beam, core, or vm
        #[arg(long, short, default_value = "beam")]
        target: String,
        /// Output directory
        #[arg(long, short)]
        output: Option<PathBuf>,
    },
    /// Build and run the project or a single file
    Run {
        /// Source file to run (optional, uses project if not specified)
        file: Option<PathBuf>,
        /// Function to call (default: main)
        #[arg(short, long, default_value = "main")]
        function: String,
        /// Arguments to pass to the function
        args: Vec<String>,
    },
    /// Show version information
    Version,
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    match cli.command {
        Commands::New { name } => cmd_new(&name),
        Commands::Build { file, target, output } | Commands::Compile { file, target, output } => {
            cmd_build(file.as_deref(), &target, output.as_deref())
        }
        Commands::Run {
            file,
            function,
            args,
        } => cmd_run(file.as_deref(), &function, &args),
        Commands::Version => {
            println!("dream {}", env!("CARGO_PKG_VERSION"));
            ExitCode::SUCCESS
        }
    }
}

/// Create a new Dream project.
fn cmd_new(name: &str) -> ExitCode {
    let project_dir = Path::new(name);

    if project_dir.exists() {
        eprintln!("Error: directory '{}' already exists", name);
        return ExitCode::from(1);
    }

    // Create directory structure
    let src_dir = project_dir.join("src");
    if let Err(e) = fs::create_dir_all(&src_dir) {
        eprintln!("Error creating directories: {}", e);
        return ExitCode::from(1);
    }

    // Write dream.toml
    let toml_path = project_dir.join("dream.toml");
    if let Err(e) = fs::write(&toml_path, generate_dream_toml(name)) {
        eprintln!("Error writing dream.toml: {}", e);
        return ExitCode::from(1);
    }

    // Write src/main.dream
    let main_path = src_dir.join("main.dream");
    if let Err(e) = fs::write(&main_path, generate_main_dream(name)) {
        eprintln!("Error writing main.dream: {}", e);
        return ExitCode::from(1);
    }

    println!("Created project '{}'", name);
    println!();
    println!("  cd {}", name);
    println!("  dream build");
    println!("  dream run");

    ExitCode::SUCCESS
}

/// Build the project or a standalone file.
fn cmd_build(file: Option<&Path>, target: &str, output: Option<&Path>) -> ExitCode {
    // Determine if we're building a standalone file or a project
    if let Some(source_file) = file {
        return build_standalone_file(source_file, target, output);
    }

    // Project mode: find project root and load config
    let (project_root, config) = match ProjectConfig::from_project_root() {
        Ok(result) => result,
        Err(e) => {
            eprintln!("Error: {}", e);
            return ExitCode::from(1);
        }
    };

    let src_dir = config.src_dir(&project_root);
    let build_dir = output
        .map(|p| p.to_path_buf())
        .unwrap_or_else(|| config.beam_dir(&project_root));

    // Create build directory
    if let Err(e) = fs::create_dir_all(&build_dir) {
        eprintln!("Error creating build directory: {}", e);
        return ExitCode::from(1);
    }

    println!("Compiling {}...", config.package.name);

    // Load all .dream files in src/ directory
    let mut loader = ModuleLoader::new();
    if let Err(e) = loader.load_all_in_dir(&src_dir) {
        eprintln!("Error loading modules: {}", e);
        return ExitCode::from(1);
    }

    // Compile all loaded modules
    compile_modules(loader.into_modules(), &build_dir, target)
}

/// Build a standalone .dream file.
fn build_standalone_file(source_file: &Path, target: &str, output: Option<&Path>) -> ExitCode {
    if !source_file.exists() {
        eprintln!("Error: file not found: {}", source_file.display());
        return ExitCode::from(1);
    }

    // Default output directory is current directory
    let build_dir = output
        .map(|p| p.to_path_buf())
        .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

    // Create build directory if needed
    if let Err(e) = fs::create_dir_all(&build_dir) {
        eprintln!("Error creating build directory: {}", e);
        return ExitCode::from(1);
    }

    println!("Compiling {}...", source_file.display());

    compile_and_emit(source_file, &build_dir, target)
}

/// Compile source file(s) and emit to build directory.
fn compile_and_emit(entry_file: &Path, build_dir: &Path, target: &str) -> ExitCode {
    // Load modules
    let mut loader = ModuleLoader::new();
    if let Err(e) = loader.load_project(entry_file) {
        eprintln!("Error loading project: {}", e);
        return ExitCode::from(1);
    }

    compile_modules(loader.into_modules(), build_dir, target)
}

/// Compile modules to Core Erlang and optionally BEAM.
fn compile_modules(
    modules: Vec<Module>,
    build_dir: &Path,
    target: &str,
) -> ExitCode {
    let mut modules = modules;

    if modules.is_empty() {
        eprintln!("No modules to compile");
        return ExitCode::from(1);
    }

    // Type check all modules together (allows cross-module type references)
    for (module_name, result) in check_modules(&modules) {
        if let Err(e) = result {
            // Find the module to get source for error display
            if let Some(module) = modules.iter().find(|m| m.name == module_name) {
                if let Some(ref source) = module.source {
                    let err = CompilerError::type_error(&module_name, source, e);
                    eprintln!("  Type warning in {}:\n{:?}", module_name, miette::Report::new(err));
                } else {
                    eprintln!("  Type warning in {}: {:?}", module_name, miette::Report::new(e));
                }
            }
        }
    }

    // Resolve stdlib method calls (e.g., s.trim() -> string::trim(s))
    for module in &mut modules {
        resolve_stdlib_methods(module);
    }

    // Compile each module to Core Erlang
    let mut core_files = Vec::new();
    for module in &modules {
        let mut emitter = CoreErlangEmitter::new();
        let core_erlang = match emitter.emit_module(module) {
            Ok(c) => c,
            Err(e) => {
                eprintln!("Compile error in {}: {}", module.name, e);
                return ExitCode::from(1);
            }
        };

        // Use prefixed module name for output files (e.g., dream::io.core)
        let beam_name = CoreErlangEmitter::beam_module_name(&module.name);
        let core_file = build_dir.join(format!("{}.core", beam_name));
        if let Err(e) = fs::write(&core_file, &core_erlang) {
            eprintln!("Error writing {}: {}", core_file.display(), e);
            return ExitCode::from(1);
        }

        println!("  Compiled {}.core", beam_name);
        core_files.push(core_file);
    }

    // If target is "core", we're done
    if target == "core" {
        println!();
        println!("Build complete. Core Erlang files in {}", build_dir.display());
        return ExitCode::SUCCESS;
    }

    // For "beam" target, invoke erlc
    if target == "beam" {
        // Check if erlc is available
        if !command_exists("erlc") {
            eprintln!();
            eprintln!("Warning: erlc not found in PATH");
            eprintln!("Install Erlang/OTP to compile to BEAM bytecode.");
            eprintln!();
            eprintln!("Core Erlang files are in {}", build_dir.display());
            eprintln!("You can compile manually with: erlc +from_core *.core");
            return ExitCode::from(1);
        }

        for core_file in &core_files {
            let status = Command::new("erlc")
                .arg("+from_core")
                .arg("-o")
                .arg(build_dir)
                .arg(core_file)
                .status();

            match status {
                Ok(s) if s.success() => {
                    let beam_name = core_file.file_stem().unwrap().to_string_lossy();
                    println!("  Compiled {}.beam", beam_name);
                    // Clean up intermediate .core file
                    let _ = fs::remove_file(core_file);
                }
                Ok(s) => {
                    eprintln!("erlc failed with exit code {:?}", s.code());
                    return ExitCode::from(1);
                }
                Err(e) => {
                    eprintln!("Error running erlc: {}", e);
                    return ExitCode::from(1);
                }
            }
        }
    }

    println!();
    println!("Build complete.");

    ExitCode::SUCCESS
}

/// Find the stdlib directory relative to the executable or current directory.
fn find_stdlib_dir() -> Option<PathBuf> {
    // Try relative to executable first
    if let Ok(exe_path) = std::env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            // Check ../stdlib (for target/debug/dream -> stdlib/)
            let stdlib = exe_dir.join("../../stdlib");
            if stdlib.exists() {
                return Some(stdlib.canonicalize().unwrap_or(stdlib));
            }
            // Check alongside executable
            let stdlib = exe_dir.join("stdlib");
            if stdlib.exists() {
                return Some(stdlib);
            }
        }
    }

    // Try current directory
    let stdlib = PathBuf::from("stdlib");
    if stdlib.exists() {
        return Some(stdlib.canonicalize().unwrap_or(stdlib));
    }

    None
}

/// Get the stdlib output directory.
fn stdlib_beam_dir() -> PathBuf {
    // Use target/stdlib for compiled stdlib .beam files
    PathBuf::from("target/stdlib")
}

/// Compile the stdlib to target/stdlib/ if needed.
fn compile_stdlib() -> Result<PathBuf, String> {
    let stdlib_dir = find_stdlib_dir().ok_or("Could not find stdlib directory")?;
    let output_dir = stdlib_beam_dir();

    // Create output directory
    fs::create_dir_all(&output_dir)
        .map_err(|e| format!("Failed to create stdlib output directory: {}", e))?;

    // Find all .dream files in stdlib
    let entries = fs::read_dir(&stdlib_dir)
        .map_err(|e| format!("Failed to read stdlib directory: {}", e))?;

    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("dream") {
            // Check if .beam file exists and is newer than source
            let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("");
            let beam_name = CoreErlangEmitter::beam_module_name(stem);
            let beam_file = output_dir.join(format!("{}.beam", beam_name));

            let needs_compile = if beam_file.exists() {
                // Check if source is newer than beam
                let src_modified = path.metadata().and_then(|m| m.modified()).ok();
                let beam_modified = beam_file.metadata().and_then(|m| m.modified()).ok();
                match (src_modified, beam_modified) {
                    (Some(src), Some(beam)) => src > beam,
                    _ => true,
                }
            } else {
                true
            };

            if needs_compile {
                let result = compile_and_emit(&path, &output_dir, "beam");
                if result != ExitCode::SUCCESS {
                    // Log warning but continue with other modules
                    eprintln!("Warning: Failed to compile stdlib module: {}", path.display());
                }
            }
        }
    }

    Ok(output_dir)
}

/// Build and run the project or a standalone file.
fn cmd_run(file: Option<&Path>, function: &str, args: &[String]) -> ExitCode {
    // Compile stdlib first
    let stdlib_dir = match compile_stdlib() {
        Ok(dir) => Some(dir),
        Err(e) => {
            eprintln!("Warning: {}", e);
            None
        }
    };

    // Determine beam directory and module name based on mode
    let (beam_dir, module_name) = if let Some(source_file) = file {
        // Standalone file mode
        let build_dir = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));

        // Build the standalone file
        let build_result = cmd_build(Some(source_file), "beam", Some(&build_dir));
        if build_result != ExitCode::SUCCESS {
            return build_result;
        }

        // Module name comes from the file name (with dream:: prefix)
        let base_name = source_file
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("main");
        let module_name = CoreErlangEmitter::beam_module_name(base_name);

        (build_dir, module_name)
    } else {
        // Project mode
        let build_result = cmd_build(None, "beam", None);
        if build_result != ExitCode::SUCCESS {
            return build_result;
        }

        // Find project root and load config
        let (project_root, config) = match ProjectConfig::from_project_root() {
            Ok(result) => result,
            Err(e) => {
                eprintln!("Error: {}", e);
                return ExitCode::from(1);
            }
        };

        (config.beam_dir(&project_root), CoreErlangEmitter::beam_module_name("main"))
    };

    // Check if erl is available
    if !command_exists("erl") {
        eprintln!("Error: erl not found in PATH");
        eprintln!("Install Erlang/OTP to run on the BEAM.");
        return ExitCode::from(1);
    }

    // Format arguments for Erlang
    let args_str = if args.is_empty() {
        String::new()
    } else {
        args.join(", ")
    };

    println!();
    println!("Running '{}':{}({})...", module_name, function, args_str);
    println!();

    // Run with erl (module name must be quoted for special chars like ::)
    let eval_expr = format!(
        "io:format(\"~p~n\", ['{}':{}({})]), halt().",
        module_name, function, args_str
    );

    let mut cmd = Command::new("erl");
    cmd.arg("-pa").arg(&beam_dir);

    // Add stdlib to code path if available
    if let Some(ref stdlib) = stdlib_dir {
        cmd.arg("-pa").arg(stdlib);
    }

    cmd.arg("-noshell").arg("-eval").arg(&eval_expr);

    let status = cmd.status();

    match status {
        Ok(s) if s.success() => ExitCode::SUCCESS,
        Ok(s) => ExitCode::from(s.code().unwrap_or(1) as u8),
        Err(e) => {
            eprintln!("Error running erl: {}", e);
            ExitCode::from(1)
        }
    }
}

/// Check if a command exists in PATH.
fn command_exists(cmd: &str) -> bool {
    Command::new("which")
        .arg(cmd)
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}
