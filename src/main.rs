//! Dream CLI - Build and run Dream programs.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};

use clap::{Parser, Subcommand};

use dream::{
    compiler::{
        check_modules, resolve_stdlib_methods, CompilerError, CoreErlangEmitter,
        GenericFunctionRegistry, Item, Module, ModuleContext, ModuleLoader, Parser as DreamParser,
        SharedGenericRegistry,
    },
    config::{generate_dream_toml, generate_main_dream, ApplicationConfig, ProjectConfig},
    deps::DepsManager,
};
use std::sync::{Arc, RwLock};

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
        /// Function to call (default: main, or starts application if configured)
        #[arg(short, long)]
        function: Option<String>,
        /// Just evaluate the function and exit (don't start application)
        #[arg(long)]
        eval: bool,
        /// Keep the BEAM running after function returns (for non-application mode)
        #[arg(long)]
        no_halt: bool,
        /// Environment: dev, test, prod (default: dev)
        #[arg(short, long, default_value = "dev")]
        env: String,
        /// Arguments to pass to the function
        args: Vec<String>,
    },
    /// Generate .dreamt type stubs from Erlang source files
    Bindgen {
        /// Erlang source files (.erl) to parse
        #[arg(required = true)]
        files: Vec<PathBuf>,
        /// Output file (default: stdout)
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Module name override (default: derived from filename)
        #[arg(short, long)]
        module: Option<String>,
    },
    /// Show version information
    Version,
    /// Start an interactive Dream shell (REPL)
    Shell,
    /// Manage dependencies
    Deps {
        #[command(subcommand)]
        action: DepsAction,
    },
}

#[derive(Subcommand)]
enum DepsAction {
    /// Fetch all dependencies
    Get,
    /// Compile all dependencies
    Compile,
}

mod bindgen;
mod repl;

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
            eval,
            no_halt,
            env,
            args,
        } => cmd_run(file.as_deref(), function.as_deref(), eval, no_halt, &env, &args),
        Commands::Bindgen {
            files,
            output,
            module,
        } => bindgen::cmd_bindgen(&files, output.as_deref(), module.as_deref()),
        Commands::Version => {
            println!("dream {}", env!("CARGO_PKG_VERSION"));
            ExitCode::SUCCESS
        }
        Commands::Shell => repl::run_shell(),
        Commands::Deps { action } => cmd_deps(action),
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

/// Handle dependency management commands.
fn cmd_deps(action: DepsAction) -> ExitCode {
    // Find project root and load config
    let (project_root, config) = match ProjectConfig::from_project_root() {
        Ok(result) => result,
        Err(e) => {
            eprintln!("Error: {}", e);
            return ExitCode::from(1);
        }
    };

    let deps_manager = DepsManager::new(project_root, config);

    match action {
        DepsAction::Get => {
            // Use tokio runtime for async deps fetching
            let rt = match tokio::runtime::Runtime::new() {
                Ok(rt) => rt,
                Err(e) => {
                    eprintln!("Error creating runtime: {}", e);
                    return ExitCode::from(1);
                }
            };

            if let Err(e) = rt.block_on(deps_manager.fetch_all()) {
                eprintln!("Error fetching dependencies: {}", e);
                return ExitCode::from(1);
            }

            ExitCode::SUCCESS
        }
        DepsAction::Compile => {
            if let Err(e) = deps_manager.compile_deps() {
                eprintln!("Error compiling dependencies: {}", e);
                return ExitCode::from(1);
            }

            ExitCode::SUCCESS
        }
    }
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

    // Load all .dream files in src/ directory with package context
    // This enables Rust-style module naming (e.g., my_app::users::auth)
    let mut loader = ModuleLoader::with_package(
        config.package.name.clone(),
        src_dir.clone(),
    );
    if let Err(e) = loader.load_all_in_dir(&src_dir) {
        eprintln!("Error loading modules: {}", e);
        return ExitCode::from(1);
    }

    // Collect module names before compilation
    let modules = loader.into_modules();
    let module_names: Vec<String> = modules.iter().map(|m| m.name.clone()).collect();

    // Compile all loaded modules with package name for module resolution
    let result = compile_modules(
        modules,
        &build_dir,
        target,
        Some(&config.package.name),
    );

    // Generate .app file if compilation succeeded
    if result == ExitCode::SUCCESS && target == "beam" {
        if let Err(e) = generate_app_file(&build_dir, &config, &module_names) {
            eprintln!("Warning: Failed to generate .app file: {}", e);
        }
    }

    result
}

/// Build a standalone .dream file.
fn build_standalone_file(source_file: &Path, target: &str, output: Option<&Path>) -> ExitCode {
    if !source_file.exists() {
        eprintln!("Error: file not found: {}", source_file.display());
        return ExitCode::from(1);
    }

    // Check if this file is part of a project (dream.toml in parent directories)
    if let Some(project_root) = find_project_root(source_file) {
        if let Ok(config) = ProjectConfig::load(&project_root.join("dream.toml")) {
            // This is a project file - use project mode
            let src_dir = config.src_dir(&project_root);
            let build_dir = output
                .map(|p| p.to_path_buf())
                .unwrap_or_else(|| config.beam_dir(&project_root));

            if let Err(e) = fs::create_dir_all(&build_dir) {
                eprintln!("Error creating build directory: {}", e);
                return ExitCode::from(1);
            }

            println!("Compiling {}...", config.package.name);

            let mut loader = ModuleLoader::with_package(
                config.package.name.clone(),
                src_dir.clone(),
            );
            if let Err(e) = loader.load_all_in_dir(&src_dir) {
                eprintln!("Error loading modules: {}", e);
                return ExitCode::from(1);
            }

            return compile_modules(
                loader.into_modules(),
                &build_dir,
                target,
                Some(&config.package.name),
            );
        }
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

/// Find the project root by looking for dream.toml in parent directories.
fn find_project_root(start: &Path) -> Option<PathBuf> {
    let mut current = start.canonicalize().ok()?;
    while let Some(parent) = current.parent() {
        current = parent.to_path_buf();
        if current.join("dream.toml").exists() {
            return Some(current);
        }
    }
    None
}

/// Compile source file(s) and emit to build directory.
fn compile_and_emit(entry_file: &Path, build_dir: &Path, target: &str) -> ExitCode {
    // Load modules
    let mut loader = ModuleLoader::new();
    if let Err(e) = loader.load_project(entry_file) {
        eprintln!("Error loading project: {}", e);
        return ExitCode::from(1);
    }

    // Standalone files don't have a package context
    compile_modules(loader.into_modules(), build_dir, target, None)
}

/// Compile modules to Core Erlang and optionally BEAM.
fn compile_modules(
    modules: Vec<Module>,
    build_dir: &Path,
    target: &str,
    package_name: Option<&str>,
) -> ExitCode {
    // Use stdlib generics registry if available
    let stdlib_registry = load_stdlib_generics();
    compile_modules_with_registry(modules, build_dir, target, stdlib_registry, package_name)
}

/// Compile modules to Core Erlang and optionally BEAM, with a pre-populated registry.
fn compile_modules_with_registry(
    modules: Vec<Module>,
    build_dir: &Path,
    target: &str,
    external_registry: Option<SharedGenericRegistry>,
    package_name: Option<&str>,
) -> ExitCode {
    if modules.is_empty() {
        eprintln!("No modules to compile");
        return ExitCode::from(1);
    }

    // Load stub modules for FFI type checking
    let stub_modules = load_stub_modules();

    // Check if we're compiling stdlib itself (by checking if any module shares a name with stdlib)
    let stdlib_module_names_raw: std::collections::HashSet<_> = load_stdlib_modules()
        .iter()
        .map(|m| m.name.clone())
        .collect();

    let user_module_names: std::collections::HashSet<_> = modules.iter()
        .map(|m| m.name.clone())
        .collect();

    let is_compiling_stdlib = user_module_names.iter().any(|n| stdlib_module_names_raw.contains(n));

    // Only load stdlib for type checking when compiling user code (not stdlib itself)
    let stdlib_modules = if is_compiling_stdlib {
        Vec::new()
    } else {
        load_stdlib_modules()
    };

    // Combine user modules with stub modules and stdlib for type checking
    let mut all_modules_for_typeck: Vec<Module> = stub_modules;
    all_modules_for_typeck.extend(stdlib_modules.iter().cloned());
    all_modules_for_typeck.extend(modules.iter().cloned());

    // Type check all modules together (allows cross-module type references)
    // This also annotates the AST with inferred type arguments
    let mut has_errors = false;

    let mut annotated_modules: Vec<Module> = Vec::new();
    let type_results = check_modules(&all_modules_for_typeck);

    // List of stdlib module names for filtering
    let stdlib_module_names: std::collections::HashSet<_> = stdlib_modules.iter()
        .map(|m| m.name.clone())
        .collect();

    for (module_name, result) in type_results {
        // Skip stub modules (they don't have function bodies)
        if module_name.ends_with("_stubs") || module_name == "erlang" {
            continue;
        }

        // Skip stdlib modules for error reporting (but still process them for annotation)
        let is_stdlib = stdlib_module_names.contains(&module_name);

        match result {
            Ok(annotated) => {
                // Only keep user modules, not stdlib modules that were added for type checking
                if modules.iter().any(|m| m.name == module_name) {
                    annotated_modules.push(annotated);
                }
            }
            Err(e) => {
                // Only report errors for user modules, not stdlib
                if !is_stdlib {
                    has_errors = true;
                    // Find the module to get source for error display
                    if let Some(module) = modules.iter().find(|m| m.name == module_name) {
                        if let Some(ref source) = module.source {
                            let err = CompilerError::type_error(&module_name, source, e);
                            eprintln!("  Type error in {}:\n{:?}", module_name, miette::Report::new(err));
                        } else {
                            eprintln!("  Type error in {}: {:?}", module_name, miette::Report::new(e));
                        }
                    } else {
                        // Module not found in user modules - this shouldn't happen
                        eprintln!("  Type error in {}: {:?}", module_name, e);
                    }
                }
            }
        }
    }

    if has_errors {
        eprintln!("\nCompilation failed due to type errors.");
        return ExitCode::from(1);
    }

    // Use annotated modules for code generation
    let mut modules = annotated_modules;

    // Resolve stdlib method calls (e.g., s.trim() -> string::trim(s))
    for module in &mut modules {
        resolve_stdlib_methods(module);
    }

    // Create a shared registry for cross-module generic functions
    // Start with external (stdlib) generics if available
    let generic_registry: SharedGenericRegistry = external_registry
        .unwrap_or_else(|| Arc::new(RwLock::new(GenericFunctionRegistry::new())));

    // Collect local module short names for module resolution
    // These are the short names (e.g., "hello_handler") that can be referenced
    // as atoms like :hello_handler and should resolve to dream::package::hello_handler
    let local_module_names: std::collections::HashSet<String> = if let Some(pkg) = package_name {
        modules
            .iter()
            .filter_map(|m| {
                // Extract the short name from the full module name
                // e.g., "http_api::hello_handler" -> "hello_handler"
                let prefix = format!("{}::", pkg);
                if let Some(suffix) = m.name.strip_prefix(&prefix) {
                    // Get the last segment (the actual module name)
                    suffix.split("::").last().map(|s| s.to_string())
                } else if m.name == pkg {
                    // Root module - use package name
                    Some(pkg.to_string())
                } else {
                    // Module name doesn't have package prefix, use as-is
                    Some(m.name.clone())
                }
            })
            .collect()
    } else {
        std::collections::HashSet::new()
    };

    // Compile each module to Core Erlang
    let mut core_files = Vec::new();
    for module in &modules {
        // Create module-specific context for path resolution (crate::/super::/self::)
        let module_context = match package_name {
            Some(pkg) => ModuleContext::for_module(pkg, &module.name)
                .with_local_modules(local_module_names.clone()),
            None => ModuleContext::default(),
        };

        let mut emitter = CoreErlangEmitter::with_registry_and_context(
            generic_registry.clone(),
            module_context,
        );
        let core_erlang = match emitter.emit_module(module) {
            Ok(c) => c,
            Err(e) => {
                eprintln!("Compile error in {}: {}", module.name, e);
                return ExitCode::from(1);
            }
        };

        // Register this module's generic functions for cross-module use
        {
            let mut registry = generic_registry.write().unwrap();
            emitter.register_generics(&mut registry);
        }

        // All Dream modules are prefixed with dream:: (like Elixir uses Elixir.)
        let beam_module_name = if module.name.starts_with("dream::") {
            module.name.clone()
        } else {
            format!("dream::{}", module.name)
        };

        let core_file = build_dir.join(format!("{}.core", &beam_module_name));
        if let Err(e) = fs::write(&core_file, &core_erlang) {
            eprintln!("Error writing {}: {}", core_file.display(), e);
            return ExitCode::from(1);
        }

        println!("  Compiled {}.core", &beam_module_name);
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

/// Generate an OTP .app file for the Dream application.
fn generate_app_file(
    build_dir: &Path,
    config: &ProjectConfig,
    module_names: &[String],
) -> Result<(), String> {
    // OTP application name is just the package name (no dream:: prefix)
    let app_name = &config.package.name;
    let version = &config.package.version;

    // Format module list as Erlang atoms with dream:: prefix
    // Module names are fully qualified (e.g., dream::http_api::hello_handler)
    let modules_str = module_names
        .iter()
        .map(|m| {
            if m.starts_with("dream::") {
                format!("'{}'", m)
            } else {
                format!("'dream::{}'", m)
            }
        })
        .collect::<Vec<_>>()
        .join(", ");

    // Get dependency application names
    let deps: Vec<String> = config
        .dependencies
        .keys()
        .map(|k| k.clone())
        .collect();

    let deps_str = deps
        .iter()
        .map(|d| d.as_str())
        .collect::<Vec<_>>()
        .join(", ");

    // Build the .app file content
    // Standard OTP applications: kernel and stdlib
    let applications = if deps_str.is_empty() {
        "kernel, stdlib".to_string()
    } else {
        format!("kernel, stdlib, {}", deps_str)
    };

    let app_content = format!(
        r#"{{application, {app_name}, [
  {{description, "A Dream application"}},
  {{vsn, "{version}"}},
  {{modules, [{modules}]}},
  {{registered, []}},
  {{applications, [{applications}]}}
]}}.
"#,
        app_name = app_name,
        version = version,
        modules = modules_str,
        applications = applications,
    );

    let app_file = build_dir.join(format!("{}.app", app_name));
    fs::write(&app_file, app_content)
        .map_err(|e| format!("Failed to write {}: {}", app_file.display(), e))?;

    println!("  Generated {}.app", app_name);
    Ok(())
}

/// Find the stdlib directory relative to the executable or current directory.
fn find_stdlib_dir() -> Option<PathBuf> {
    // Try relative to executable first
    if let Ok(exe_path) = std::env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            // Check ../../stdlib (for target/debug/dream -> stdlib/)
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

    // Search up from current directory (for running from subdirectories)
    if let Ok(mut current) = std::env::current_dir() {
        loop {
            let stdlib = current.join("stdlib");
            if stdlib.exists() {
                return Some(stdlib.canonicalize().unwrap_or(stdlib));
            }
            if !current.pop() {
                break;
            }
        }
    }

    None
}

/// Find the stubs directory relative to the executable or current directory.
fn find_stubs_dir() -> Option<PathBuf> {
    // Try relative to executable first
    if let Ok(exe_path) = std::env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            // Check ../../stubs (for target/debug/dream -> stubs/)
            let stubs = exe_dir.join("../../stubs");
            if stubs.exists() {
                return Some(stubs.canonicalize().unwrap_or(stubs));
            }
            // Check alongside executable
            let stubs = exe_dir.join("stubs");
            if stubs.exists() {
                return Some(stubs);
            }
        }
    }

    // Try current directory
    let stubs = PathBuf::from("stubs");
    if stubs.exists() {
        return Some(stubs.canonicalize().unwrap_or(stubs));
    }

    // Search up from current directory (for running from subdirectories)
    if let Ok(mut current) = std::env::current_dir() {
        loop {
            let stubs = current.join("stubs");
            if stubs.exists() {
                return Some(stubs.canonicalize().unwrap_or(stubs));
            }
            if !current.pop() {
                break;
            }
        }
    }

    None
}

/// Load .dreamt stub files and parse them into modules.
/// These provide type information for FFI calls to external libraries.
fn load_stub_modules() -> Vec<Module> {
    let stubs_dir = match find_stubs_dir() {
        Some(dir) => dir,
        None => return Vec::new(),
    };

    let entries = match fs::read_dir(&stubs_dir) {
        Ok(entries) => entries,
        Err(_) => return Vec::new(),
    };

    let mut stub_modules = Vec::new();

    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("dreamt") {
            continue;
        }

        // Read and parse the stub file
        let source = match fs::read_to_string(&path) {
            Ok(s) => s,
            Err(_) => continue,
        };

        // Wrap in a module since parser expects that
        // Add _stubs suffix to avoid name conflicts with Dream stdlib modules
        // (stubs are for FFI to external BEAM modules, stdlib compiles to dream::*)
        let stub_name = path.file_stem().and_then(|s| s.to_str()).unwrap_or("stubs");
        let stub_module_name = format!("{}_stubs", stub_name);
        let wrapped = format!("mod {} {{\n{}\n}}", stub_module_name, source);

        let mut parser = dream::compiler::Parser::new(&wrapped);
        match parser.parse_module() {
            Ok(module) => stub_modules.push(module),
            Err(e) => {
                eprintln!("Warning: Failed to parse stub file {}: {:?}", path.display(), e);
            }
        }
    }

    stub_modules
}

/// Load stdlib modules for type checking.
/// This allows the type checker to see function signatures from stdlib.
fn load_stdlib_modules() -> Vec<Module> {
    let stdlib_dir = match find_stdlib_dir() {
        Some(dir) => dir,
        None => return Vec::new(),
    };

    // Use package-aware loader with "dream" as the package name
    // This enables implicit modules: stdlib/io.dream -> dream::io
    let mut loader = ModuleLoader::with_package("dream".to_string(), stdlib_dir.clone());

    if let Err(_) = loader.load_all_in_dir(&stdlib_dir) {
        return Vec::new();
    }

    loader.into_modules()
}

/// Get the stdlib output directory.
fn stdlib_beam_dir() -> PathBuf {
    // Use target/stdlib relative to executable for compiled stdlib .beam files
    // This ensures stdlib is found regardless of working directory
    if let Ok(exe_path) = std::env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            // Executable is at target/release/dream or target/debug/dream
            // Stdlib should be at target/stdlib
            return exe_dir.join("../stdlib");
        }
    }
    // Fallback to relative path
    PathBuf::from("target/stdlib")
}

/// Load stdlib modules and extract their generic functions into a registry.
/// This enables cross-module monomorphization of stdlib generic functions.
fn load_stdlib_generics() -> Option<SharedGenericRegistry> {
    let stdlib_dir = find_stdlib_dir()?;
    let registry = Arc::new(RwLock::new(GenericFunctionRegistry::new()));

    // Use package-aware loader with "dream" as the package name
    let mut loader = ModuleLoader::with_package("dream".to_string(), stdlib_dir.clone());
    if loader.load_all_in_dir(&stdlib_dir).is_err() {
        return Some(registry);
    }

    // Extract generic functions from each loaded module
    for module in loader.modules() {
        let mut reg = registry.write().unwrap();
        for item in &module.items {
            if let Item::Function(func) = item {
                if !func.type_params.is_empty() {
                    reg.register(&module.name, func);
                }
            }
        }
    }

    Some(registry)
}

/// Compile the stdlib to target/stdlib/ if needed.
fn compile_stdlib() -> Result<PathBuf, String> {
    let stdlib_dir = find_stdlib_dir().ok_or("Could not find stdlib directory")?;
    let output_dir = stdlib_beam_dir();

    // Create output directory
    fs::create_dir_all(&output_dir)
        .map_err(|e| format!("Failed to create stdlib output directory: {}", e))?;

    // Check if any stdlib files need recompilation
    let entries = fs::read_dir(&stdlib_dir)
        .map_err(|e| format!("Failed to read stdlib directory: {}", e))?;

    let mut needs_compile = false;
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("dream") {
            let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("");
            // Stdlib modules are named dream::<stem>, beam files use the full name
            let beam_file = output_dir.join(format!("dream::{}.beam", stem));

            if !beam_file.exists() {
                needs_compile = true;
                break;
            }

            // Check if source is newer than beam
            let src_modified = path.metadata().and_then(|m| m.modified()).ok();
            let beam_modified = beam_file.metadata().and_then(|m| m.modified()).ok();
            if let (Some(src), Some(beam)) = (src_modified, beam_modified) {
                if src > beam {
                    needs_compile = true;
                    break;
                }
            }
        }
    }

    if needs_compile {
        // Load all stdlib modules with "dream" as the package name
        let mut loader = ModuleLoader::with_package("dream".to_string(), stdlib_dir.clone());
        if let Err(e) = loader.load_all_in_dir(&stdlib_dir) {
            return Err(format!("Failed to load stdlib modules: {}", e));
        }

        // Compile all stdlib modules
        let result = compile_modules(loader.into_modules(), &output_dir, "beam", Some("dream"));
        if result != ExitCode::SUCCESS {
            return Err("Failed to compile stdlib".to_string());
        }
    }

    Ok(output_dir)
}

/// Extract the module name(s) from a Dream source file.
/// Returns the name of the first module declared in the file.
fn extract_module_name(source_file: &Path) -> Option<String> {
    let source = fs::read_to_string(source_file).ok()?;
    let mut parser = DreamParser::new(&source);

    // Use a dummy fallback name - we'll get the real name from the parsed result
    let fallback = source_file
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown");

    match parser.parse_file_modules(fallback) {
        Ok(modules) if !modules.is_empty() => Some(modules[0].name.clone()),
        _ => None,
    }
}

/// Build and run the project or a standalone file.
fn cmd_run(
    file: Option<&Path>,
    function: Option<&str>,
    eval_mode: bool,
    no_halt: bool,
    env: &str,
    args: &[String],
) -> ExitCode {
    // Compile stdlib first
    let stdlib_dir = match compile_stdlib() {
        Ok(dir) => Some(dir),
        Err(e) => {
            eprintln!("Warning: {}", e);
            None
        }
    };

    // Determine beam directory, module name, and application config based on mode
    let (beam_dir, module_name, app_config) = if let Some(source_file) = file {
        // Standalone file mode - no application support
        let build_dir = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));

        // Extract actual module name from the source file before building
        let base_name = match extract_module_name(source_file) {
            Some(name) => name,
            None => {
                // Fall back to filename if parsing fails
                source_file
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or("main")
                    .to_string()
            }
        };

        // Build the standalone file
        let build_result = cmd_build(Some(source_file), "beam", Some(&build_dir));
        if build_result != ExitCode::SUCCESS {
            return build_result;
        }

        // Module name from parsed AST (use directly - explicit names now)
        let module_name = base_name;

        (build_dir, module_name, None)
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

        let beam_dir = config.beam_dir_for_env(&project_root, env);
        let app_config = config.application.clone();

        // Determine module name: use application module or package name
        // Module names are prefixed with dream:: and package:: (e.g., "dream::http_api::http_api")
        let base_module = if let Some(ref app) = app_config {
            app.module.clone().unwrap_or_else(|| config.package.name.clone())
        } else {
            // Use package name as the default module (corresponds to lib.dream)
            config.package.name.clone()
        };

        // Build the full module path: dream::package::module
        let package_name = &config.package.name;
        let module_name = if base_module.starts_with("dream::") {
            base_module
        } else if base_module.starts_with(&format!("{}::", package_name)) {
            format!("dream::{}", base_module)
        } else {
            format!("dream::{}::{}", package_name, base_module)
        };

        (beam_dir, module_name, app_config)
    };

    // Check if erl is available
    if !command_exists("erl") {
        eprintln!("Error: erl not found in PATH");
        eprintln!("Install Erlang/OTP to run on the BEAM.");
        return ExitCode::from(1);
    }

    // Determine run mode:
    // 1. If --eval flag is set, always use eval mode (call function and exit)
    // 2. If function is explicitly specified, use eval mode
    // 3. If application is configured and no function specified, use application mode
    // 4. If __script__ module exists and no function specified, run the script
    // 5. Otherwise, use eval mode with main()
    let use_app_mode = !eval_mode && function.is_none() && app_config.is_some();

    // Check if __script__ module exists (for script files with top-level expressions)
    let script_beam = beam_dir.join("__script__.beam");
    let has_script_module = script_beam.exists();

    // Get deps ebin paths if in project mode
    let deps_dirs: Vec<PathBuf> = if file.is_none() {
        // Re-load config to get project root for DepsManager
        if let Ok((project_root, config)) = ProjectConfig::from_project_root() {
            let deps_manager = DepsManager::new(project_root, config);
            deps_manager.dep_ebin_paths()
        } else {
            Vec::new()
        }
    } else {
        Vec::new()
    };

    if use_app_mode {
        run_application(&beam_dir, &module_name, &app_config.unwrap(), stdlib_dir.as_ref(), &deps_dirs)
    } else if has_script_module && function.is_none() && !eval_mode {
        // Run script mode: execute __script__:__main__()
        run_function(
            &beam_dir,
            "__script__",
            "__main__",
            args,
            no_halt,
            stdlib_dir.as_ref(),
            &deps_dirs,
        )
    } else {
        let func = function.unwrap_or("main");
        run_function(&beam_dir, &module_name, func, args, no_halt, stdlib_dir.as_ref(), &deps_dirs)
    }
}

/// Run in application mode - start the supervision tree and keep running.
fn run_application(
    beam_dir: &Path,
    module_name: &str,
    app_config: &ApplicationConfig,
    stdlib_dir: Option<&PathBuf>,
    deps_dirs: &[PathBuf],
) -> ExitCode {
    println!();
    println!("Starting application '{}'...", module_name);
    println!();

    // Build the eval expression for application mode:
    // 1. Set environment variables from config
    // 2. Call the Application's start/2 function
    // 3. Block forever (receive loop)
    let mut eval_parts = Vec::new();

    // Set application environment from config
    for (key, value) in &app_config.env {
        let erlang_value = toml_to_erlang(value);
        eval_parts.push(format!(
            "application:set_env('{}', '{}', {})",
            module_name, key, erlang_value
        ));
    }

    // Start the application
    eval_parts.push(format!(
        "case '{}':start(normal, []) of \
            {{ok, _Pid}} -> ok; \
            {{error, Reason}} -> io:format(\"Failed to start: ~p~n\", [Reason]), halt(1) \
        end",
        module_name
    ));

    // Print startup message
    eval_parts.push(format!(
        "io:format(\"Application '{}' started. Press Ctrl+C to stop.~n\", [])",
        module_name
    ));

    // Block forever - the supervision tree handles everything
    eval_parts.push("receive stop -> ok end".to_string());

    let eval_expr = eval_parts.join(", ") + ".";

    let mut cmd = Command::new("erl");
    cmd.arg("-pa").arg(beam_dir);

    // Add stdlib to code path if available
    if let Some(stdlib) = stdlib_dir {
        cmd.arg("-pa").arg(stdlib);
    }

    // Add deps ebin directories to code path
    for dep_dir in deps_dirs {
        cmd.arg("-pa").arg(dep_dir);
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

/// Run in eval mode - call a function and optionally exit.
fn run_function(
    beam_dir: &Path,
    module_name: &str,
    function: &str,
    args: &[String],
    no_halt: bool,
    stdlib_dir: Option<&PathBuf>,
    deps_dirs: &[PathBuf],
) -> ExitCode {
    // Format arguments for Erlang
    let args_str = if args.is_empty() {
        String::new()
    } else {
        args.join(", ")
    };

    println!();
    println!("Running '{}':{}({})...", module_name, function, args_str);
    println!();

    // Build eval expression
    // Quote both module and function names to handle atoms like __main__
    let eval_expr = if no_halt {
        // Print result but don't halt - keep BEAM running
        format!(
            "io:format(\"~p~n\", ['{}':'{}'({})]).",
            module_name, function, args_str
        )
    } else {
        // Print result and halt
        format!(
            "io:format(\"~p~n\", ['{}':'{}'({})]), halt().",
            module_name, function, args_str
        )
    };

    let mut cmd = Command::new("erl");
    cmd.arg("-pa").arg(beam_dir);

    // Add stdlib to code path if available
    if let Some(stdlib) = stdlib_dir {
        cmd.arg("-pa").arg(stdlib);
    }

    // Add deps ebin directories to code path
    for dep_dir in deps_dirs {
        cmd.arg("-pa").arg(dep_dir);
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

/// Convert a TOML value to an Erlang term string.
fn toml_to_erlang(value: &toml::Value) -> String {
    match value {
        toml::Value::String(s) => format!("<<\"{}\">>", s), // Binary string
        toml::Value::Integer(i) => i.to_string(),
        toml::Value::Float(f) => f.to_string(),
        toml::Value::Boolean(b) => if *b { "true" } else { "false" }.to_string(),
        toml::Value::Array(arr) => {
            let items: Vec<String> = arr.iter().map(toml_to_erlang).collect();
            format!("[{}]", items.join(", "))
        }
        toml::Value::Table(tbl) => {
            let items: Vec<String> = tbl
                .iter()
                .map(|(k, v)| format!("{{'{}', {}}}", k, toml_to_erlang(v)))
                .collect();
            format!("[{}]", items.join(", "))
        }
        toml::Value::Datetime(dt) => format!("<<\"{}\">>", dt),
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
