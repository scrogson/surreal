//! Dream CLI - Build and run Dream programs.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};

use clap::{Parser, Subcommand};

use dream::{
    compiler::{CoreErlangEmitter, ModuleLoader},
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
    /// Build the project
    Build {
        /// Target: beam, core, or vm
        #[arg(long, short, default_value = "beam")]
        target: String,
        /// Output directory
        #[arg(long, short)]
        output: Option<PathBuf>,
    },
    /// Compile the project (alias for build)
    Compile {
        /// Target: beam, core, or vm
        #[arg(long, short, default_value = "beam")]
        target: String,
        /// Output directory
        #[arg(long, short)]
        output: Option<PathBuf>,
    },
    /// Build and run the project
    Run {
        /// Module to run (default: project name)
        #[arg(short, long)]
        module: Option<String>,
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
        Commands::Build { target, output } | Commands::Compile { target, output } => {
            cmd_build(&target, output.as_deref())
        }
        Commands::Run {
            module,
            function,
            args,
        } => cmd_run(module.as_deref(), &function, &args),
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

/// Build the project.
fn cmd_build(target: &str, output: Option<&Path>) -> ExitCode {
    // Find project root and load config
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

    // Find entry point
    let entry_file = find_entry_file(&src_dir);
    let entry_file = match entry_file {
        Some(f) => f,
        None => {
            eprintln!("Error: no main.dream or lib.dream found in {}", src_dir.display());
            return ExitCode::from(1);
        }
    };

    // Load modules
    let mut loader = ModuleLoader::new();
    if let Err(e) = loader.load_project(&entry_file) {
        eprintln!("Error loading project: {}", e);
        return ExitCode::from(1);
    }

    // Compile each module to Core Erlang
    let mut core_files = Vec::new();
    for module in loader.modules() {
        let mut emitter = CoreErlangEmitter::new();
        let core_erlang = match emitter.emit_module(module) {
            Ok(c) => c,
            Err(e) => {
                eprintln!("Compile error in {}: {}", module.name, e);
                return ExitCode::from(1);
            }
        };

        let core_file = build_dir.join(format!("{}.core", module.name));
        if let Err(e) = fs::write(&core_file, &core_erlang) {
            eprintln!("Error writing {}: {}", core_file.display(), e);
            return ExitCode::from(1);
        }

        println!("  Compiled {}.core", module.name);
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
                .arg(&build_dir)
                .arg(core_file)
                .status();

            match status {
                Ok(s) if s.success() => {
                    let beam_name = core_file.file_stem().unwrap().to_string_lossy();
                    println!("  Compiled {}.beam", beam_name);
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

/// Build and run the project.
fn cmd_run(module: Option<&str>, function: &str, _args: &[String]) -> ExitCode {
    // First, build the project
    let build_result = cmd_build("beam", None);
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

    let beam_dir = config.beam_dir(&project_root);

    // Default module is "main" (from main.dream), not the package name
    let module_name = module.unwrap_or("main");

    // Check if erl is available
    if !command_exists("erl") {
        eprintln!("Error: erl not found in PATH");
        eprintln!("Install Erlang/OTP to run on the BEAM.");
        return ExitCode::from(1);
    }

    println!();
    println!("Running {}:{}()...", module_name, function);
    println!();

    // Run with erl
    let eval_expr = format!(
        "io:format(\"~p~n\", [{}:{}()]), halt().",
        module_name, function
    );

    let status = Command::new("erl")
        .arg("-pa")
        .arg(&beam_dir)
        .arg("-noshell")
        .arg("-eval")
        .arg(&eval_expr)
        .status();

    match status {
        Ok(s) if s.success() => ExitCode::SUCCESS,
        Ok(s) => ExitCode::from(s.code().unwrap_or(1) as u8),
        Err(e) => {
            eprintln!("Error running erl: {}", e);
            ExitCode::from(1)
        }
    }
}

/// Find the entry file (main.dream or lib.dream) in the source directory.
fn find_entry_file(src_dir: &Path) -> Option<PathBuf> {
    let main = src_dir.join("main.dream");
    if main.exists() {
        return Some(main);
    }

    let lib = src_dir.join("lib.dream");
    if lib.exists() {
        return Some(lib);
    }

    None
}

/// Check if a command exists in PATH.
fn command_exists(cmd: &str) -> bool {
    Command::new("which")
        .arg(cmd)
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}
