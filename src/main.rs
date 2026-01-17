//! Dream CLI - Build and run Dream programs.

use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};

use clap::{Parser, Subcommand};

use dream::{
    compiler::{
        cfg, check_modules_with_metadata, expand_derives_with_registry, expand_quotes,
        get_derive_macro_name, is_derive_macro, is_macro, resolve_stdlib_methods,
        CompilerError, CoreErlangEmitter, GenericFunctionRegistry, Item, MacroRegistry,
        Module, ModuleContext, ModuleLoader, Parser as DreamParser, SharedGenericRegistry,
    },
    config::{generate_dream_toml, generate_main_dream, ApplicationConfig, CompileOptions, ProjectConfig},
    deps::DepsManager,
};
use std::collections::HashSet;
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
        /// Enable features for conditional compilation (comma-separated)
        #[arg(long, value_delimiter = ',')]
        features: Vec<String>,
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
        /// Enable features for conditional compilation (comma-separated)
        #[arg(long, value_delimiter = ',')]
        features: Vec<String>,
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
        /// Start with interactive Erlang shell (like iex -S mix)
        #[arg(short = 'S', long)]
        shell: bool,
        /// Environment: dev, test, prod (default: dev)
        #[arg(short, long, default_value = "dev")]
        env: String,
        /// Enable features for conditional compilation (comma-separated)
        #[arg(long, value_delimiter = ',')]
        features: Vec<String>,
        /// Arguments to pass to the function
        args: Vec<String>,
    },
    /// Run tests
    Test {
        /// Filter tests by name (substring match)
        filter: Option<String>,
        /// Enable features for conditional compilation (comma-separated)
        #[arg(long, value_delimiter = ',')]
        features: Vec<String>,
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
    /// Generate bindings for dependencies
    Bindgen,
}

mod bindgen;
mod repl;

fn main() -> ExitCode {
    let cli = Cli::parse();

    match cli.command {
        Commands::New { name } => cmd_new(&name),
        Commands::Build { file, target, output, features } | Commands::Compile { file, target, output, features } => {
            cmd_build(file.as_deref(), &target, output.as_deref(), &features)
        }
        Commands::Run {
            file,
            function,
            eval,
            no_halt,
            shell,
            env,
            features,
            args,
        } => cmd_run(file.as_deref(), function.as_deref(), eval, no_halt, shell, &env, &features, &args),
        Commands::Test { filter, features } => cmd_test(filter.as_deref(), &features),
        Commands::Bindgen {
            files,
            output,
            module,
        } => bindgen::cmd_bindgen(&files, output.as_deref(), module.as_deref()),
        Commands::Version => {
            println!("dream {}", env!("CARGO_PKG_VERSION"));
            ExitCode::SUCCESS
        }
        Commands::Shell => {
            // Compile stdlib first so it's available in the shell
            if let Err(e) = compile_stdlib() {
                eprintln!("Warning: Failed to compile stdlib: {}", e);
            }
            repl::run_shell()
        }
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

            // Auto-generate bindings after fetching
            if let Err(e) = deps_manager.generate_bindings() {
                eprintln!("Warning: Failed to generate bindings: {}", e);
                // Don't fail the command, just warn
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
        DepsAction::Bindgen => {
            if let Err(e) = deps_manager.generate_bindings() {
                eprintln!("Error generating bindings: {}", e);
                return ExitCode::from(1);
            }

            ExitCode::SUCCESS
        }
    }
}

/// Build the project or a standalone file.
fn cmd_build(file: Option<&Path>, target: &str, output: Option<&Path>, features: &[String]) -> ExitCode {
    // Determine if we're building a standalone file or a project
    if let Some(source_file) = file {
        return build_standalone_file(source_file, target, output, features);
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

    // Compile stdlib first
    if let Err(e) = compile_stdlib() {
        eprintln!("Warning: {}", e);
    }

    println!("Compiling {}...", config.package.name);

    // Load all .dream files in src/ directory with package context
    // This enables Rust-style module naming (e.g., my_app::users::auth)
    let mut loader = ModuleLoader::with_package(
        config.package.name.clone(),
        src_dir.clone(),
    );

    // Add _build/bindings/ to search path for auto-generated dependency bindings
    let bindings_dir = project_root.join("_build").join("bindings");
    loader.add_bindings_dir(bindings_dir);

    if let Err(e) = loader.load_all_in_dir(&src_dir) {
        eprintln!("Error loading modules: {}", e);
        return ExitCode::from(1);
    }

    // Collect module names before compilation
    let modules = loader.into_modules();
    let module_names: Vec<String> = modules.iter().map(|m| m.name.clone()).collect();

    // Resolve features (CLI features + their dependencies from config)
    let resolved_features = config.resolve_features(features);
    let compile_options = CompileOptions::with_features(resolved_features);

    // Get dependency ebin paths for loading macros from dependencies
    let deps_manager = DepsManager::new(project_root.clone(), config.clone());
    let dep_ebin_paths = deps_manager.dep_ebin_paths();

    // Compile all loaded modules with package name for module resolution
    let result = compile_modules_with_options(
        modules,
        &build_dir,
        target,
        Some(&config.package.name),
        &compile_options,
        &dep_ebin_paths,
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
fn build_standalone_file(source_file: &Path, target: &str, output: Option<&Path>, features: &[String]) -> ExitCode {
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

            // Compile stdlib first
            if let Err(e) = compile_stdlib() {
                eprintln!("Warning: {}", e);
            }

            println!("Compiling {}...", config.package.name);

            let mut loader = ModuleLoader::with_package(
                config.package.name.clone(),
                src_dir.clone(),
            );

            // Add _build/bindings/ to search path for auto-generated dependency bindings
            let bindings_dir = project_root.join("_build").join("bindings");
            loader.add_bindings_dir(bindings_dir);

            if let Err(e) = loader.load_all_in_dir(&src_dir) {
                eprintln!("Error loading modules: {}", e);
                return ExitCode::from(1);
            }

            // Collect module names before compilation
            let modules = loader.into_modules();
            let module_names: Vec<String> = modules.iter().map(|m| m.name.clone()).collect();

            // Resolve features
            let resolved_features = config.resolve_features(features);
            let compile_options = CompileOptions::with_features(resolved_features);

            // Get dependency ebin paths for loading macros from dependencies
            let deps_manager = DepsManager::new(project_root.clone(), config.clone());
            let dep_ebin_paths = deps_manager.dep_ebin_paths();

            let result = compile_modules_with_options(
                modules,
                &build_dir,
                target,
                Some(&config.package.name),
                &compile_options,
                &dep_ebin_paths,
            );

            // Generate .app file if compilation succeeded
            if result == ExitCode::SUCCESS && target == "beam" {
                if let Err(e) = generate_app_file(&build_dir, &config, &module_names) {
                    eprintln!("Warning: Failed to generate .app file: {}", e);
                }
            }

            return result;
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

    compile_and_emit(source_file, &build_dir, target, features)
}

/// Find the project root by looking for dream.toml in current and parent directories.
fn find_project_root(start: &Path) -> Option<PathBuf> {
    let mut current = start.canonicalize().ok()?;

    // If start is a directory, check it first
    if current.is_dir() && current.join("dream.toml").exists() {
        return Some(current);
    }

    // Then check parent directories
    while let Some(parent) = current.parent() {
        current = parent.to_path_buf();
        if current.join("dream.toml").exists() {
            return Some(current);
        }
    }
    None
}

/// Compile source file(s) and emit to build directory.
fn compile_and_emit(entry_file: &Path, build_dir: &Path, target: &str, features: &[String]) -> ExitCode {
    // Load modules
    let mut loader = ModuleLoader::new();
    if let Err(e) = loader.load_project(entry_file) {
        eprintln!("Error loading project: {}", e);
        return ExitCode::from(1);
    }

    // Create compile options from features (standalone files have no feature resolution)
    let resolved_features: HashSet<String> = features.iter().cloned().collect();
    let compile_options = CompileOptions::with_features(resolved_features);

    // Standalone files don't have a package context or dependencies
    compile_modules_with_options(loader.into_modules(), build_dir, target, None, &compile_options, &[])
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
    compile_modules_with_registry(modules, build_dir, target, stdlib_registry, package_name, &[])
}

/// Compile modules to Core Erlang and optionally BEAM, with compile options for cfg filtering.
fn compile_modules_with_options(
    modules: Vec<Module>,
    build_dir: &Path,
    target: &str,
    package_name: Option<&str>,
    compile_options: &CompileOptions,
    dep_ebin_paths: &[PathBuf],
) -> ExitCode {
    // Use stdlib generics registry if available
    let stdlib_registry = load_stdlib_generics();
    compile_modules_with_registry_and_options(
        modules,
        build_dir,
        target,
        stdlib_registry,
        package_name,
        compile_options,
        dep_ebin_paths,
    )
}

/// Compile modules to Core Erlang and optionally BEAM, with a pre-populated registry.
fn compile_modules_with_registry(
    modules: Vec<Module>,
    build_dir: &Path,
    target: &str,
    external_registry: Option<SharedGenericRegistry>,
    package_name: Option<&str>,
    dep_ebin_paths: &[PathBuf],
) -> ExitCode {
    if modules.is_empty() {
        eprintln!("No modules to compile");
        return ExitCode::from(1);
    }

    // Load stub modules for FFI type checking
    let stub_modules = load_stub_modules();

    // Load all stdlib modules for type checking
    // This is needed even when compiling stdlib itself, because stdlib modules
    // may depend on extern modules defined in other stdlib files
    let stdlib_modules_full = load_stdlib_modules();

    // Check if we're compiling stdlib itself (by checking if any module shares a name with stdlib)
    let stdlib_module_names_raw: std::collections::HashSet<_> = stdlib_modules_full
        .iter()
        .map(|m| m.name.clone())
        .collect();

    let user_module_names: std::collections::HashSet<_> = modules.iter()
        .map(|m| m.name.clone())
        .collect();

    let is_compiling_stdlib = user_module_names.iter().any(|n| stdlib_module_names_raw.contains(n));

    // When compiling stdlib itself, filter out modules being compiled to avoid duplicates
    // but keep other stdlib modules for type checking (e.g., extern module definitions)
    let stdlib_modules: Vec<Module> = if is_compiling_stdlib {
        stdlib_modules_full
            .into_iter()
            .filter(|m| !user_module_names.contains(&m.name))
            .collect()
    } else {
        stdlib_modules_full
    };

    // Combine user modules with stub modules and stdlib for type checking
    let mut all_modules_for_typeck: Vec<Module> = stub_modules;
    all_modules_for_typeck.extend(stdlib_modules.iter().cloned());
    all_modules_for_typeck.extend(modules.iter().cloned());

    // Type check all modules together (allows cross-module type references)
    // This also annotates the AST with inferred type arguments
    let mut has_errors = false;

    let mut annotated_modules: Vec<Module> = Vec::new();
    let type_check_result = check_modules_with_metadata(&all_modules_for_typeck);
    let extern_module_names = type_check_result.extern_module_names.clone();

    // List of stdlib module names for filtering
    let stdlib_module_names: std::collections::HashSet<_> = stdlib_modules.iter()
        .map(|m| m.name.clone())
        .collect();

    for (module_name, result) in type_check_result.modules {
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

    // Create macro registry for user-defined derives
    // Include build_dir, stdlib, and dependency ebin paths for loading macro modules
    let mut beam_paths = vec![build_dir.to_path_buf()];
    // Add stdlib so macros can use syn, proc_macro, etc.
    let stdlib_dir = stdlib_beam_dir();
    if stdlib_dir.exists() {
        beam_paths.push(stdlib_dir);
    }
    beam_paths.extend(dep_ebin_paths.iter().cloned());
    let mut macro_registry = MacroRegistry::with_paths(beam_paths);

    // Load macros from dependencies (from .macros metadata files)
    load_dependency_macros(&mut macro_registry, dep_ebin_paths);

    // Track macros defined in this project for .macros file generation
    let mut project_macros: Vec<(String, String, String)> = Vec::new();

    // Identify and compile macro modules first
    let macro_module_names: Vec<String> = modules
        .iter()
        .filter(|m| has_macro_functions(m))
        .map(|m| m.name.clone())
        .collect();

    if !macro_module_names.is_empty() {
        // Compile macro modules to BEAM first so they can be used by other modules
        for module_name in &macro_module_names {
            if let Some(module) = modules.iter().find(|m| &m.name == module_name) {
                // Clone and expand only built-in derives for macro modules
                let mut macro_module = module.clone();
                if let Err(errors) = expand_derives_with_registry(&mut macro_module, &mut MacroRegistry::new()) {
                    for err in errors {
                        eprintln!("Derive error in macro module {}: {}", module_name, err.message);
                    }
                    return ExitCode::from(1);
                }

                // Expand quote expressions (quote { ... } -> tuple construction)
                expand_quotes(&mut macro_module);

                // Resolve stdlib methods
                resolve_stdlib_methods(&mut macro_module);

                // Compile to BEAM
                match compile_module_to_beam(
                    &macro_module,
                    build_dir,
                    &generic_registry,
                    &extern_module_names,
                    package_name,
                    &local_module_names,
                ) {
                    Ok(_) => {
                        // Get the BEAM module name
                        let beam_module_name = if module.name.starts_with("dream::") {
                            module.name.clone()
                        } else {
                            format!("dream::{}", module.name)
                        };
                        println!("  Compiled macro module {}.beam", beam_module_name);

                        // Register macros from this module and track for .macros file
                        for (derive_name, func_name) in get_macro_functions(module) {
                            macro_registry.register(&derive_name, &beam_module_name, &func_name);
                            project_macros.push((derive_name, beam_module_name.clone(), func_name));
                        }
                    }
                    Err(e) => {
                        eprintln!("Error compiling macro module {}: {}", module_name, e);
                        return ExitCode::from(1);
                    }
                }
            }
        }
    }

    // Expand derive macros (e.g., #[derive(Debug, Clone)]) with macro registry
    for module in &mut modules {
        if let Err(errors) = expand_derives_with_registry(module, &mut macro_registry) {
            for err in errors {
                eprintln!("Derive error: {}", err.message);
            }
            return ExitCode::from(1);
        }
    }

    // Expand quote expressions in all modules (quote { ... } -> tuple construction)
    for module in &mut modules {
        expand_quotes(module);
    }

    // Resolve stdlib method calls (e.g., s.trim() -> string::trim(s))
    for module in &mut modules {
        resolve_stdlib_methods(module);
    }

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
        // Set extern module name mappings for #[name = "..."] attribute support
        emitter.set_extern_module_names(extern_module_names.clone());

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

    // Generate .macros file if we have macros and a package name
    if !project_macros.is_empty() {
        if let Some(pkg_name) = package_name {
            if let Err(e) = generate_macros_file(build_dir, pkg_name, &project_macros) {
                eprintln!("Warning: Failed to generate .macros file: {}", e);
            }
        }
    }

    println!();
    println!("Build complete.");

    ExitCode::SUCCESS
}

/// Compile modules to Core Erlang and optionally BEAM, with registry and compile options.
fn compile_modules_with_registry_and_options(
    modules: Vec<Module>,
    build_dir: &Path,
    target: &str,
    external_registry: Option<SharedGenericRegistry>,
    package_name: Option<&str>,
    compile_options: &CompileOptions,
    dep_ebin_paths: &[PathBuf],
) -> ExitCode {
    if modules.is_empty() {
        eprintln!("No modules to compile");
        return ExitCode::from(1);
    }

    // Load stub modules for FFI type checking
    let stub_modules = load_stub_modules();

    // Load all stdlib modules for type checking
    // This is needed even when compiling stdlib itself, because stdlib modules
    // may depend on extern modules defined in other stdlib files
    let stdlib_modules_full = load_stdlib_modules();

    // Check if we're compiling stdlib itself (by checking if any module shares a name with stdlib)
    let stdlib_module_names_raw: std::collections::HashSet<_> = stdlib_modules_full
        .iter()
        .map(|m| m.name.clone())
        .collect();

    let user_module_names: std::collections::HashSet<_> = modules.iter()
        .map(|m| m.name.clone())
        .collect();

    let is_compiling_stdlib = user_module_names.iter().any(|n| stdlib_module_names_raw.contains(n));

    // When compiling stdlib itself, filter out modules being compiled to avoid duplicates
    // but keep other stdlib modules for type checking (e.g., extern module definitions)
    let stdlib_modules: Vec<Module> = if is_compiling_stdlib {
        stdlib_modules_full
            .into_iter()
            .filter(|m| !user_module_names.contains(&m.name))
            .collect()
    } else {
        stdlib_modules_full
    };

    // Combine user modules with stub modules and stdlib for type checking
    let mut all_modules_for_typeck: Vec<Module> = stub_modules;
    all_modules_for_typeck.extend(stdlib_modules.iter().cloned());
    all_modules_for_typeck.extend(modules.iter().cloned());

    // Type check all modules together (allows cross-module type references)
    // This also annotates the AST with inferred type arguments
    let mut has_errors = false;

    let mut annotated_modules: Vec<Module> = Vec::new();
    let type_check_result = check_modules_with_metadata(&all_modules_for_typeck);
    let extern_module_names = type_check_result.extern_module_names.clone();

    // List of stdlib module names for filtering
    let stdlib_module_names: std::collections::HashSet<_> = stdlib_modules.iter()
        .map(|m| m.name.clone())
        .collect();

    for (module_name, result) in type_check_result.modules {
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

    // Create macro registry for user-defined derives
    // Include build_dir, stdlib, and dependency ebin paths for loading macro modules
    let mut beam_paths = vec![build_dir.to_path_buf()];
    // Add stdlib so macros can use syn, proc_macro, etc.
    let stdlib_dir = stdlib_beam_dir();
    if stdlib_dir.exists() {
        beam_paths.push(stdlib_dir);
    }
    beam_paths.extend(dep_ebin_paths.iter().cloned());
    let mut macro_registry = MacroRegistry::with_paths(beam_paths);

    // Load macros from dependencies (from .macros metadata files)
    load_dependency_macros(&mut macro_registry, dep_ebin_paths);

    // Track macros defined in this project for .macros file generation
    let mut project_macros: Vec<(String, String, String)> = Vec::new();

    // Identify and compile macro modules first
    let macro_module_names: Vec<String> = modules
        .iter()
        .filter(|m| has_macro_functions(m))
        .map(|m| m.name.clone())
        .collect();

    if !macro_module_names.is_empty() {
        // Compile macro modules to BEAM first so they can be used by other modules
        for module_name in &macro_module_names {
            if let Some(module) = modules.iter().find(|m| &m.name == module_name) {
                // Clone and expand only built-in derives for macro modules
                let mut macro_module = module.clone();
                if let Err(errors) = expand_derives_with_registry(&mut macro_module, &mut MacroRegistry::new()) {
                    for err in errors {
                        eprintln!("Derive error in macro module {}: {}", module_name, err.message);
                    }
                    return ExitCode::from(1);
                }

                // Expand quote expressions (quote { ... } -> tuple construction)
                expand_quotes(&mut macro_module);

                // Resolve stdlib methods
                resolve_stdlib_methods(&mut macro_module);

                // Compile to BEAM
                match compile_module_to_beam(
                    &macro_module,
                    build_dir,
                    &generic_registry,
                    &extern_module_names,
                    package_name,
                    &local_module_names,
                ) {
                    Ok(_) => {
                        // Get the BEAM module name
                        let beam_module_name = if module.name.starts_with("dream::") {
                            module.name.clone()
                        } else {
                            format!("dream::{}", module.name)
                        };
                        println!("  Compiled macro module {}.beam", beam_module_name);

                        // Register macros from this module and track for .macros file
                        for (derive_name, func_name) in get_macro_functions(module) {
                            macro_registry.register(&derive_name, &beam_module_name, &func_name);
                            project_macros.push((derive_name, beam_module_name.clone(), func_name));
                        }
                    }
                    Err(e) => {
                        eprintln!("Error compiling macro module {}: {}", module_name, e);
                        return ExitCode::from(1);
                    }
                }
            }
        }
    }

    // Expand derive macros (e.g., #[derive(Debug, Clone)]) with macro registry
    for module in &mut modules {
        if let Err(errors) = expand_derives_with_registry(module, &mut macro_registry) {
            for err in errors {
                eprintln!("Derive error: {}", err.message);
            }
            return ExitCode::from(1);
        }
    }

    // Expand quote expressions in all modules (quote { ... } -> tuple construction)
    for module in &mut modules {
        expand_quotes(module);
    }

    // Resolve stdlib method calls (e.g., s.trim() -> string::trim(s))
    for module in &mut modules {
        resolve_stdlib_methods(module);
    }

    // Compile each module to Core Erlang (with incremental compilation)
    let mut core_files = Vec::new();
    let mut skipped_count = 0;

    for module in &modules {
        // All Dream modules are prefixed with dream:: (like Elixir uses Elixir.)
        let beam_module_name = if module.name.starts_with("dream::") {
            module.name.clone()
        } else {
            format!("dream::{}", module.name)
        };

        let beam_file = build_dir.join(format!("{}.beam", &beam_module_name));

        // Check if module needs recompilation
        if !needs_recompilation(module, &beam_file) {
            skipped_count += 1;
            continue;
        }

        // Create module-specific context for path resolution (crate::/super::/self::)
        let module_context = match package_name {
            Some(pkg) => ModuleContext::for_module(pkg, &module.name)
                .with_local_modules(local_module_names.clone()),
            None => ModuleContext::default(),
        };

        // Use with_all to include compile options for cfg filtering
        let mut emitter = CoreErlangEmitter::with_all(
            generic_registry.clone(),
            module_context,
            compile_options.clone(),
        );
        // Set extern module name mappings for #[name = "..."] attribute support
        emitter.set_extern_module_names(extern_module_names.clone());

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
        if skipped_count > 0 {
            println!("Build complete. {} module(s) up to date, {} recompiled.", skipped_count, core_files.len());
        } else {
            println!("Build complete. Core Erlang files in {}", build_dir.display());
        }
        return ExitCode::SUCCESS;
    }

    // For "beam" target, invoke erlc
    if target == "beam" && !core_files.is_empty() {
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

        // Batch compile all .core files in a single erlc invocation
        let mut cmd = Command::new("erlc");
        cmd.arg("+from_core").arg("-o").arg(build_dir);
        for core_file in &core_files {
            cmd.arg(core_file);
        }

        let status = cmd.status();
        match status {
            Ok(s) if s.success() => {
                for core_file in &core_files {
                    let beam_name = core_file.file_stem().unwrap().to_string_lossy();
                    println!("  Compiled {}.beam", beam_name);
                    // Clean up intermediate .core file
                    let _ = fs::remove_file(core_file);
                }
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

    // Generate .macros file if we have macros and a package name
    if !project_macros.is_empty() {
        if let Some(pkg_name) = package_name {
            if let Err(e) = generate_macros_file(build_dir, pkg_name, &project_macros) {
                eprintln!("Warning: Failed to generate .macros file: {}", e);
            }
        }
    }

    println!();
    if skipped_count > 0 && core_files.is_empty() {
        println!("Build complete. All {} module(s) up to date.", skipped_count);
    } else if skipped_count > 0 {
        println!("Build complete. {} module(s) up to date, {} recompiled.", skipped_count, core_files.len());
    } else {
        println!("Build complete.");
    }

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

    // Build the mod entry if an application module is configured
    let mod_entry = if let Some(ref app_config) = config.application {
        if let Some(ref module) = app_config.module {
            // Build fully qualified module name: dream::module
            // The module name in config already includes the package prefix (e.g., "http_api::app")
            let full_module = format!("dream::{}", module);
            format!("  {{mod, {{'{}', []}}}},\n", full_module)
        } else {
            String::new()
        }
    } else {
        String::new()
    };

    let app_content = format!(
        r#"{{application, {app_name}, [
  {{description, "A Dream application"}},
  {{vsn, "{version}"}},
{mod_entry}  {{modules, [{modules}]}},
  {{registered, []}},
  {{applications, [{applications}]}}
]}}.
"#,
        app_name = app_name,
        version = version,
        mod_entry = mod_entry,
        modules = modules_str,
        applications = applications,
    );

    let app_file = build_dir.join(format!("{}.app", app_name));
    fs::write(&app_file, app_content)
        .map_err(|e| format!("Failed to write {}: {}", app_file.display(), e))?;

    println!("  Generated {}.app", app_name);
    Ok(())
}

/// Macro metadata for serialization
#[derive(serde::Serialize, serde::Deserialize, Debug)]
struct MacroMetadata {
    /// Derive name (e.g., "Serialize")
    derive_name: String,
    /// Module containing the macro (e.g., "dream::serde::serde")
    module: String,
    /// Function name (e.g., "serialize_derive")
    function: String,
}

/// Macros file format
#[derive(serde::Serialize, serde::Deserialize, Debug)]
struct MacrosFile {
    macros: Vec<MacroMetadata>,
}

/// Generate a .macros metadata file listing exported macros.
fn generate_macros_file(
    build_dir: &Path,
    app_name: &str,
    macros: &[(String, String, String)], // (derive_name, module, function)
) -> Result<(), String> {
    if macros.is_empty() {
        return Ok(());
    }

    let macros_file = MacrosFile {
        macros: macros
            .iter()
            .map(|(derive_name, module, function)| MacroMetadata {
                derive_name: derive_name.clone(),
                module: module.clone(),
                function: function.clone(),
            })
            .collect(),
    };

    let content = serde_json::to_string_pretty(&macros_file)
        .map_err(|e| format!("Failed to serialize macros: {}", e))?;

    let macros_path = build_dir.join(format!("{}.macros", app_name));
    fs::write(&macros_path, content)
        .map_err(|e| format!("Failed to write {}: {}", macros_path.display(), e))?;

    println!("  Generated {}.macros", app_name);
    Ok(())
}

/// Load macros from dependency .macros files and register them for qualified path lookups.
/// Macros are registered under their package name (e.g., "serde::Serialize").
/// This does NOT register them globally - they must be either:
/// - Imported via `use serde::Serialize;` (for unqualified `#[derive(Serialize)]`)
/// - Used with qualified path `#[derive(serde::Serialize)]`
fn load_dependency_macros(
    macro_registry: &mut MacroRegistry,
    dep_ebin_paths: &[PathBuf],
) {
    for ebin_dir in dep_ebin_paths {
        // Look for .macros files in the ebin directory
        if let Ok(entries) = fs::read_dir(ebin_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.extension().map_or(false, |ext| ext == "macros") {
                    // Extract package name from filename (e.g., "serde.macros" -> "serde")
                    let package_name = path
                        .file_stem()
                        .and_then(|s| s.to_str())
                        .unwrap_or("");

                    if package_name.is_empty() {
                        continue;
                    }

                    if let Ok(content) = fs::read_to_string(&path) {
                        match serde_json::from_str::<MacrosFile>(&content) {
                            Ok(macros_file) => {
                                for m in macros_file.macros {
                                    // Register under package name for qualified lookups
                                    macro_registry.register_package_macro(
                                        package_name,
                                        &m.derive_name,
                                        &m.module,
                                        &m.function,
                                    );
                                }
                            }
                            Err(e) => {
                                eprintln!(
                                    "Warning: Failed to parse {}: {}",
                                    path.display(),
                                    e
                                );
                            }
                        }
                    }
                }
            }
        }
    }
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

/// Find Elixir's ebin directories for stdlib support.
/// This is needed when using Elixir dependencies that require Elixir.Enum, etc.
fn find_elixir_ebin_dirs() -> Vec<PathBuf> {
    // Try to run elixir to get its lib directory
    let output = Command::new("elixir")
        .arg("-e")
        .arg("IO.puts(:code.lib_dir(:elixir))")
        .output();

    let elixir_lib = match output {
        Ok(out) if out.status.success() => {
            let path_str = String::from_utf8_lossy(&out.stdout).trim().to_string();
            PathBuf::from(path_str)
        }
        _ => return Vec::new(), // Elixir not found
    };

    // The lib directory structure is: elixir_root/lib/{app}/ebin
    // elixir_lib is like: /path/to/elixir/lib/elixir
    // We want: /path/to/elixir/lib/*/ebin
    let lib_root = match elixir_lib.parent() {
        Some(p) => p,
        None => return Vec::new(),
    };

    // Collect ebin directories for all Elixir stdlib apps
    let mut ebin_dirs = Vec::new();
    if let Ok(entries) = fs::read_dir(lib_root) {
        for entry in entries.flatten() {
            let ebin_path = entry.path().join("ebin");
            if ebin_path.is_dir() {
                ebin_dirs.push(ebin_path);
            }
        }
    }

    ebin_dirs
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
        None => {
            return Vec::new();
        }
    };

    // Use package-aware loader with "dream" as the package name
    // This enables implicit modules: stdlib/io.dream -> dream::io
    let mut loader = ModuleLoader::with_package("dream".to_string(), stdlib_dir.clone());

    // Load all Dream files in the stdlib directory
    // Silently ignore errors - some stdlib files may have parse errors during development
    let _ = loader.load_all_in_dir(&stdlib_dir);

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
    shell_mode: bool,
    env: &str,
    features: &[String],
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
        let build_result = cmd_build(Some(source_file), "beam", Some(&build_dir), features);
        if build_result != ExitCode::SUCCESS {
            return build_result;
        }

        // Module name from parsed AST (use directly - explicit names now)
        let module_name = base_name;

        (build_dir, module_name, None)
    } else {
        // Project mode
        let build_result = cmd_build(None, "beam", None, features);
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
    let mut deps_dirs: Vec<PathBuf> = if file.is_none() {
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

    // Add Elixir stdlib paths if Elixir is installed
    // This enables Elixir dependencies that require Elixir.Enum, etc.
    deps_dirs.extend(find_elixir_ebin_dirs());

    if use_app_mode {
        run_application(&beam_dir, &module_name, &app_config.unwrap(), stdlib_dir.as_ref(), &deps_dirs, shell_mode)
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
    _module_name: &str,
    app_config: &ApplicationConfig,
    stdlib_dir: Option<&PathBuf>,
    deps_dirs: &[PathBuf],
    shell_mode: bool,
) -> ExitCode {
    // Get the OTP application name from config (not the module name)
    let app_name = if let Ok((_, config)) = ProjectConfig::from_project_root() {
        config.package.name
    } else {
        return ExitCode::from(1);
    };

    // Shell mode: use the Dream REPL with the application loaded
    if shell_mode {
        let mut all_paths = vec![beam_dir.to_path_buf()];
        if let Some(stdlib) = stdlib_dir {
            all_paths.push(stdlib.clone());
        }
        all_paths.extend(deps_dirs.iter().cloned());

        return repl::run_shell_with_app(app_name, beam_dir.to_path_buf(), all_paths);
    }

    // Non-shell mode: run erl directly
    println!();
    println!("Starting application '{}'...", app_name);
    println!();

    // Build the eval expression for application mode:
    // 1. Set environment variables from config
    // 2. Use application:ensure_all_started/1 to start deps and our app
    // 3. Block forever (receive loop)
    let mut eval_parts = Vec::new();

    // Set application environment from config
    for (key, value) in &app_config.env {
        let erlang_value = toml_to_erlang(value);
        eval_parts.push(format!(
            "application:set_env('{}', '{}', {})",
            app_name, key, erlang_value
        ));
    }

    // Start all dependencies and then our application using OTP
    // This reads the .app file's {applications, [...]} and {mod, {...}} entries
    eval_parts.push(format!(
        "case application:ensure_all_started({}) of \
            {{ok, _Started}} -> ok; \
            {{error, {{_App, Reason}}}} -> io:format(\"Failed to start: ~p~n\", [Reason]), halt(1) \
        end",
        app_name
    ));

    // Print startup message and block forever
    eval_parts.push(format!(
        "io:format(\"Application '{}' started. Press Ctrl+C to stop.~n\", [])",
        app_name
    ));
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

    // Save terminal state before running erl
    let saved_term = save_terminal_state();

    // Run erl with proper signal handling - let erl handle Ctrl+C (BREAK menu)
    let status = run_with_signal_handling(cmd);

    // Restore terminal state after erl exits
    restore_terminal_state(saved_term);

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

    // Save terminal state before running erl
    let saved_term = save_terminal_state();

    // Run erl with proper signal handling - let erl handle Ctrl+C (BREAK menu)
    let status = run_with_signal_handling(cmd);

    // Restore terminal state after erl exits
    restore_terminal_state(saved_term);

    match status {
        Ok(s) if s.success() => ExitCode::SUCCESS,
        Ok(s) => ExitCode::from(s.code().unwrap_or(1) as u8),
        Err(e) => {
            eprintln!("Error running erl: {}", e);
            ExitCode::from(1)
        }
    }
}

/// Run a command while ignoring Ctrl+C in the parent process.
/// This allows the child (erl) to handle SIGINT itself (showing BREAK menu).
fn run_with_signal_handling(cmd: Command) -> io::Result<std::process::ExitStatus> {
    use tokio::runtime::Runtime;

    // Create a runtime for async signal handling
    let rt = Runtime::new()?;

    rt.block_on(async {
        let mut child = tokio::process::Command::from(cmd).spawn()?;

        // Wait for child to exit, ignoring Ctrl+C in parent
        // The child receives SIGINT directly from the terminal
        loop {
            tokio::select! {
                // Ignore Ctrl+C - let the child handle it
                _ = tokio::signal::ctrl_c() => {
                    // Do nothing - erl will show BREAK menu
                    continue;
                }
                // Child exited
                status = child.wait() => {
                    return status;
                }
            }
        }
    })
}

/// Reset the terminal to a sane state.
/// This is necessary after running erl because it may leave the terminal in raw mode
/// Save terminal state before running a subprocess that might modify it.
/// Returns saved state string on Unix.
#[cfg(unix)]
fn save_terminal_state() -> Option<String> {
    std::process::Command::new("stty")
        .arg("-g")
        .stdin(std::process::Stdio::inherit())
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
}

#[cfg(not(unix))]
fn save_terminal_state() -> Option<String> {
    None
}

/// Restore terminal state after subprocess exits.
fn restore_terminal_state(saved: Option<String>) {
    #[cfg(unix)]
    if let Some(state) = saved {
        let _ = std::process::Command::new("stty")
            .arg(&state)
            .stdin(std::process::Stdio::inherit())
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status();
    }

    // Drain any pending input
    while crossterm::event::poll(std::time::Duration::ZERO).unwrap_or(false) {
        let _ = crossterm::event::read();
    }

    let _ = io::stdout().flush();
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

/// Check if a module contains any macro functions.
/// Supports both `#[macro]` and `#[derive(Name)]` attributes.
fn has_macro_functions(module: &Module) -> bool {
    for item in &module.items {
        if let Item::Function(func) = item {
            if is_macro(&func.attrs) || is_derive_macro(&func.attrs) {
                return true;
            }
        }
    }
    false
}

/// Get macro function info from a module: Vec<(derive_name, func_name)>.
/// For `#[macro]` functions, the derive name equals the function name.
/// For `#[proc_macro_derive(Name)]` functions, the derive name is extracted from the attribute.
fn get_macro_functions(module: &Module) -> Vec<(String, String)> {
    let mut macros = Vec::new();
    for item in &module.items {
        if let Item::Function(func) = item {
            // Check for #[proc_macro_derive(Name)] first (preferred Rust-style syntax)
            if let Some(derive_name) = get_derive_macro_name(&func.attrs) {
                macros.push((derive_name, func.name.clone()));
            } else if is_macro(&func.attrs) {
                // Fall back to #[macro] where derive name = function name
                macros.push((func.name.clone(), func.name.clone()));
            }
        }
    }
    macros
}

/// Compile a single module to BEAM (for macro modules).
/// Returns the path to the compiled .beam file on success.
fn compile_module_to_beam(
    module: &Module,
    build_dir: &Path,
    generic_registry: &SharedGenericRegistry,
    extern_module_names: &std::collections::HashMap<String, String>,
    package_name: Option<&str>,
    local_module_names: &std::collections::HashSet<String>,
) -> Result<PathBuf, String> {
    // Create module-specific context for path resolution
    let module_context = match package_name {
        Some(pkg) => ModuleContext::for_module(pkg, &module.name)
            .with_local_modules(local_module_names.clone()),
        None => ModuleContext::default(),
    };

    let mut emitter = CoreErlangEmitter::with_registry_and_context(
        generic_registry.clone(),
        module_context,
    );
    emitter.set_extern_module_names(extern_module_names.clone());

    let core_erlang = emitter.emit_module(module)
        .map_err(|e| format!("Compile error in {}: {}", module.name, e))?;

    // Register this module's generic functions
    {
        let mut registry = generic_registry.write().unwrap();
        emitter.register_generics(&mut registry);
    }

    // All Dream modules are prefixed with dream::
    let beam_module_name = if module.name.starts_with("dream::") {
        module.name.clone()
    } else {
        format!("dream::{}", module.name)
    };

    let core_file = build_dir.join(format!("{}.core", &beam_module_name));
    fs::write(&core_file, &core_erlang)
        .map_err(|e| format!("Error writing {}: {}", core_file.display(), e))?;

    // Compile to BEAM using erlc
    let status = Command::new("erlc")
        .arg("+from_core")
        .arg("-o")
        .arg(build_dir)
        .arg(&core_file)
        .status()
        .map_err(|e| format!("Error running erlc: {}", e))?;

    if !status.success() {
        return Err(format!("erlc failed for {}", beam_module_name));
    }

    // Clean up .core file (temporarily disabled for debugging)
    // let _ = fs::remove_file(&core_file);

    let beam_file = build_dir.join(format!("{}.beam", &beam_module_name));
    Ok(beam_file)
}

/// Check if a module needs recompilation.
/// Returns true if the source file is newer than the beam file, or beam doesn't exist.
fn needs_recompilation(module: &Module, beam_file: &Path) -> bool {
    // If beam file doesn't exist, needs compilation
    if !beam_file.exists() {
        return true;
    }

    // If no source path, always recompile (can't check)
    let source_path = match &module.source_path {
        Some(p) => p,
        None => return true,
    };

    // Compare modification times
    let src_modified = source_path.metadata().and_then(|m| m.modified()).ok();
    let beam_modified = beam_file.metadata().and_then(|m| m.modified()).ok();

    match (src_modified, beam_modified) {
        (Some(src), Some(beam)) => src > beam,
        _ => true, // If we can't check, recompile to be safe
    }
}

/// Run tests in the project.
fn cmd_test(filter: Option<&str>, features: &[String]) -> ExitCode {
    // Find project root and load config
    let (project_root, config) = match ProjectConfig::from_project_root() {
        Ok(result) => result,
        Err(e) => {
            eprintln!("Error: {}", e);
            return ExitCode::from(1);
        }
    };

    let src_dir = config.src_dir(&project_root);
    let build_dir = config.beam_dir_for_env(&project_root, "test");

    // Create build directory
    if let Err(e) = fs::create_dir_all(&build_dir) {
        eprintln!("Error creating build directory: {}", e);
        return ExitCode::from(1);
    }

    println!("Compiling {} in test mode...", config.package.name);

    // Load all .dream files in src/ directory with package context
    let mut loader = ModuleLoader::with_package(
        config.package.name.clone(),
        src_dir.clone(),
    );

    // Add _build/bindings/ to search path for auto-generated dependency bindings
    let bindings_dir = project_root.join("_build").join("bindings");
    loader.add_bindings_dir(bindings_dir);

    if let Err(e) = loader.load_all_in_dir(&src_dir) {
        eprintln!("Error loading modules: {}", e);
        return ExitCode::from(1);
    }

    let modules = loader.into_modules();

    // Discover test functions before compilation
    let mut test_functions: Vec<(String, String)> = Vec::new(); // (module_name, function_name)
    for module in &modules {
        for item in &module.items {
            if let Item::Function(func) = item {
                if cfg::is_test(&func.attrs) {
                    // Apply filter if provided
                    if let Some(pattern) = filter {
                        if !func.name.contains(pattern) {
                            continue;
                        }
                    }
                    test_functions.push((module.name.clone(), func.name.clone()));
                }
            }
        }
    }

    if test_functions.is_empty() {
        println!();
        if filter.is_some() {
            println!("No tests match the filter.");
        } else {
            println!("No tests found.");
        }
        return ExitCode::SUCCESS;
    }

    // Resolve features (CLI features + their dependencies from config)
    let resolved_features = config.resolve_features(features);
    let compile_options = CompileOptions::for_testing_with_features(resolved_features);

    // Get dependency ebin paths for loading macros from dependencies
    let deps_manager = DepsManager::new(project_root.clone(), config.clone());
    let dep_ebin_paths = deps_manager.dep_ebin_paths();

    // Compile all modules in test mode
    let result = compile_modules_with_options(
        modules,
        &build_dir,
        "beam",
        Some(&config.package.name),
        &compile_options,
        &dep_ebin_paths,
    );

    if result != ExitCode::SUCCESS {
        return result;
    }

    // Compile stdlib if needed
    let stdlib_dir = match compile_stdlib() {
        Ok(dir) => Some(dir),
        Err(e) => {
            eprintln!("Warning: {}", e);
            None
        }
    };

    // Check if erl is available
    if !command_exists("erl") {
        eprintln!("Error: erl not found in PATH");
        eprintln!("Install Erlang/OTP to run tests.");
        return ExitCode::from(1);
    }

    // Get deps ebin paths
    let mut deps_dirs: Vec<PathBuf> = {
        let deps_manager = DepsManager::new(project_root, config);
        deps_manager.dep_ebin_paths()
    };

    // Add Elixir stdlib paths if available
    deps_dirs.extend(find_elixir_ebin_dirs());

    // Run tests
    println!();
    println!("Running {} test{}...", test_functions.len(), if test_functions.len() == 1 { "" } else { "s" });
    println!();

    let mut passed = 0;
    let mut failed = 0;
    let mut failures: Vec<(String, String, String)> = Vec::new(); // (module, function, error)

    for (module_name, func_name) in &test_functions {
        // Build the full module name with dream:: prefix
        let beam_module = if module_name.starts_with("dream::") {
            module_name.clone()
        } else {
            format!("dream::{}", module_name)
        };

        // Build eval expression to run the test and catch any errors
        let eval_expr = format!(
            "try '{}':'{}'() of _ -> io:format(\"ok~n\"), halt(0) catch Class:Reason:Stack -> io:format(\"~p:~p~n~p~n\", [Class, Reason, Stack]), halt(1) end.",
            beam_module, func_name
        );

        let mut cmd = Command::new("erl");
        cmd.arg("-pa").arg(&build_dir);

        // Add stdlib to code path if available
        if let Some(ref stdlib) = stdlib_dir {
            cmd.arg("-pa").arg(stdlib);
        }

        // Add deps ebin directories to code path
        for dep_dir in &deps_dirs {
            cmd.arg("-pa").arg(dep_dir);
        }

        cmd.arg("-noshell").arg("-eval").arg(&eval_expr);

        let output = cmd.output();

        match output {
            Ok(out) => {
                if out.status.success() {
                    passed += 1;
                    println!("  {} {}::{} ... ok", "\u{2713}", module_name, func_name);
                } else {
                    failed += 1;
                    let stderr = String::from_utf8_lossy(&out.stdout).to_string();
                    failures.push((module_name.clone(), func_name.clone(), stderr));
                    println!("  {} {}::{} ... FAILED", "\u{2717}", module_name, func_name);
                }
            }
            Err(e) => {
                failed += 1;
                failures.push((module_name.clone(), func_name.clone(), e.to_string()));
                println!("  {} {}::{} ... FAILED ({})", "\u{2717}", module_name, func_name, e);
            }
        }
    }

    // Print summary
    println!();
    if !failures.is_empty() {
        println!("Failures:");
        println!();
        for (module, func, error) in &failures {
            println!("  {}::{}", module, func);
            for line in error.lines() {
                println!("    {}", line);
            }
            println!();
        }
    }

    let total = passed + failed;
    if failed == 0 {
        println!("{} test{} passed.", total, if total == 1 { "" } else { "s" });
        ExitCode::SUCCESS
    } else {
        println!("{} passed, {} failed.", passed, failed);
        ExitCode::from(1)
    }
}
