//! Analysis wrapper for compiler APIs.
//!
//! This module provides a clean interface between the LSP server and
//! the Dream compiler's parser and type checker.

use std::path::Path;
use std::sync::OnceLock;

use crate::compiler::{
    check_modules_with_metadata, Module, ModuleLoader, ParseError, Parser, TypeError,
    TypeCheckResult, Warning,
};

/// Cached stdlib data for LSP analysis.
struct StdlibData {
    modules: Vec<Module>,
}

static LSP_STDLIB_CACHE: OnceLock<StdlibData> = OnceLock::new();

/// Load stdlib modules, caching the result.
fn get_stdlib() -> &'static StdlibData {
    LSP_STDLIB_CACHE.get_or_init(load_stdlib)
}

fn load_stdlib() -> StdlibData {
    let stdlib_dir = match find_stdlib_dir() {
        Some(dir) => dir,
        None => return StdlibData { modules: Vec::new() },
    };

    let mut loader = ModuleLoader::with_package("surreal".to_string(), stdlib_dir.clone());
    if loader.load_all_in_dir(&stdlib_dir).is_err() {
        return StdlibData { modules: Vec::new() };
    }

    StdlibData {
        modules: loader.into_modules(),
    }
}

fn find_stdlib_dir() -> Option<std::path::PathBuf> {
    // Try relative to executable
    if let Ok(exe) = std::env::current_exe() {
        let stdlib = exe.parent()?.join("stdlib");
        if stdlib.exists() {
            return Some(stdlib);
        }
        // Try going up from target/debug or target/release
        let stdlib = exe.parent()?.parent()?.parent()?.join("stdlib");
        if stdlib.exists() {
            return Some(stdlib);
        }
    }

    // Try from cwd
    let cwd_stdlib = std::path::Path::new("stdlib");
    if cwd_stdlib.exists() {
        return Some(cwd_stdlib.to_path_buf());
    }

    None
}

/// Result of analyzing a single file.
pub struct AnalysisResult {
    /// Successfully parsed module (may still have type errors)
    pub module: Option<Module>,
    /// Parse errors encountered
    pub parse_errors: Vec<ParseError>,
    /// Type errors encountered
    pub type_errors: Vec<TypeError>,
    /// Warnings collected
    pub warnings: Vec<Warning>,
    /// Type check metadata (if type checking succeeded)
    pub metadata: Option<TypeCheckResult>,
}

impl Default for AnalysisResult {
    fn default() -> Self {
        Self {
            module: None,
            parse_errors: Vec::new(),
            type_errors: Vec::new(),
            warnings: Vec::new(),
            metadata: None,
        }
    }
}

/// Analyzer wrapping compiler APIs for LSP use.
pub struct Analyzer {
    // Could hold state for caching, etc.
}

impl Analyzer {
    /// Create a new analyzer.
    pub fn new() -> Self {
        // Pre-warm stdlib cache
        let _ = get_stdlib();
        Self {}
    }

    /// Analyze a single file.
    pub fn analyze(&self, source: &str, path: Option<&Path>) -> AnalysisResult {
        let mut result = AnalysisResult::default();

        // Determine module name from path
        let module_name = path
            .and_then(|p| p.file_stem())
            .and_then(|s| s.to_str())
            .unwrap_or("main")
            .to_string();

        // Parse
        let mut parser = Parser::new(source);
        let module = match parser.parse_file(&module_name) {
            Ok(module) => module,
            Err(e) => {
                result.parse_errors.push(e);
                return result;
            }
        };

        // Build module list: stdlib + user module
        let stdlib = get_stdlib();
        let mut modules: Vec<Module> = stdlib.modules.clone();
        modules.push(module.clone());

        // Type check
        let type_result = check_modules_with_metadata(&modules);

        // Extract results for the user's module
        for (name, module_result) in &type_result.modules {
            if name == &module_name {
                match module_result {
                    Ok(checked_module) => {
                        result.module = Some(checked_module.clone());
                    }
                    Err(e) => {
                        result.type_errors.push(e.clone());
                        // Still store the module for partial analysis
                        result.module = Some(module.clone());
                    }
                }
                break;
            }
        }

        // If we didn't find our module in results, store the unchecked version
        if result.module.is_none() {
            result.module = Some(module);
        }

        // Collect warnings for this module
        result.warnings = type_result
            .warnings
            .iter()
            .filter(|w| w.module.as_ref().map_or(true, |m| m == &module_name))
            .cloned()
            .collect();

        result.metadata = Some(type_result);

        result
    }

    /// Get the stdlib modules (for completions, etc.).
    pub fn stdlib_modules(&self) -> &[Module] {
        &get_stdlib().modules
    }
}

impl Default for Analyzer {
    fn default() -> Self {
        Self::new()
    }
}
