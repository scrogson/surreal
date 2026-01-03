//! Module loader for file-based module resolution.
//!
//! Resolves `mod foo;` declarations to files and loads them recursively.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use crate::compiler::ast::{Item, ModDecl, Module};
use crate::compiler::parser::Parser;

/// Error during module loading.
#[derive(Debug, Clone)]
pub struct LoadError {
    pub message: String,
    pub path: Option<PathBuf>,
}

impl std::fmt::Display for LoadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(path) = &self.path {
            write!(f, "{}: {}", path.display(), self.message)
        } else {
            write!(f, "{}", self.message)
        }
    }
}

impl std::error::Error for LoadError {}

impl LoadError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            path: None,
        }
    }

    pub fn with_path(message: impl Into<String>, path: PathBuf) -> Self {
        Self {
            message: message.into(),
            path: Some(path),
        }
    }

    pub fn module_not_found(name: &str, searched: &[PathBuf]) -> Self {
        let paths = searched
            .iter()
            .map(|p| format!("  {}", p.display()))
            .collect::<Vec<_>>()
            .join("\n");
        Self {
            message: format!("cannot find module `{}`\nsearched:\n{}", name, paths),
            path: None,
        }
    }

    pub fn circular_dependency(chain: &[String]) -> Self {
        Self {
            message: format!("circular module dependency: {}", chain.join(" -> ")),
            path: None,
        }
    }
}

/// Result type for module loading operations.
pub type LoadResult<T> = Result<T, LoadError>;

/// Loads and resolves modules from the filesystem.
pub struct ModuleLoader {
    /// Cache of loaded modules by canonical path.
    loaded: HashMap<PathBuf, Module>,
    /// Paths currently being loaded (for cycle detection).
    loading: Vec<PathBuf>,
}

impl ModuleLoader {
    /// Create a new module loader.
    pub fn new() -> Self {
        Self {
            loaded: HashMap::new(),
            loading: Vec::new(),
        }
    }

    /// Resolve module path for a mod declaration.
    /// Looks for `<dir>/<name>.tb` or `<dir>/<name>/mod.tb`.
    pub fn resolve_module_path(&self, name: &str, from: &Path) -> LoadResult<PathBuf> {
        let parent = from.parent().unwrap_or(Path::new("."));

        // Try <dir>/<name>.tb
        let file_path = parent.join(format!("{}.tb", name));
        if file_path.exists() {
            return Ok(file_path);
        }

        // Try <dir>/<name>/mod.tb
        let dir_path = parent.join(name).join("mod.tb");
        if dir_path.exists() {
            return Ok(dir_path);
        }

        Err(LoadError::module_not_found(name, &[file_path, dir_path]))
    }

    /// Load a module and all its dependencies.
    /// The module name is derived from the filename.
    pub fn load(&mut self, path: &Path) -> LoadResult<Module> {
        // Derive module name from filename
        let module_name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown")
            .to_string();

        self.load_with_name(path, &module_name)
    }

    /// Load a module with an explicit name.
    /// Used when loading via `mod foo;` where the name should be `foo`.
    fn load_with_name(&mut self, path: &Path, module_name: &str) -> LoadResult<Module> {
        let canonical = path
            .canonicalize()
            .map_err(|e| LoadError::with_path(format!("cannot access file: {}", e), path.to_path_buf()))?;

        // Check cache
        if let Some(module) = self.loaded.get(&canonical) {
            return Ok(module.clone());
        }

        // Check for cycles
        if self.loading.contains(&canonical) {
            let chain: Vec<_> = self.loading
                .iter()
                .map(|p| {
                    p.file_stem()
                        .and_then(|s| s.to_str())
                        .unwrap_or("?")
                        .to_string()
                })
                .collect();
            return Err(LoadError::circular_dependency(&chain));
        }

        self.loading.push(canonical.clone());

        // Read and parse
        let source = fs::read_to_string(&canonical)
            .map_err(|e| LoadError::with_path(format!("cannot read file: {}", e), canonical.clone()))?;

        let mut parser = Parser::new(&source);
        let module = parser
            .parse_file(module_name)
            .map_err(|e| LoadError::with_path(e.to_string(), canonical.clone()))?;

        // Load dependencies (recursive)
        let mod_decls: Vec<ModDecl> = module
            .items
            .iter()
            .filter_map(|item| match item {
                Item::ModDecl(decl) => Some(decl.clone()),
                _ => None,
            })
            .collect();

        for decl in mod_decls {
            let dep_path = self.resolve_module_path(&decl.name, &canonical)?;
            // Use the declared name, not the filename
            self.load_with_name(&dep_path, &decl.name)?;
        }

        self.loading.pop();
        self.loaded.insert(canonical, module.clone());

        Ok(module)
    }

    /// Load a project from an entry point.
    /// If path is a directory, looks for main.tb or lib.tb.
    pub fn load_project(&mut self, path: &Path) -> LoadResult<Module> {
        let entry = if path.is_dir() {
            let main_path = path.join("main.tb");
            let lib_path = path.join("lib.tb");

            if main_path.exists() {
                main_path
            } else if lib_path.exists() {
                lib_path
            } else {
                return Err(LoadError::with_path(
                    "no main.tb or lib.tb found in directory",
                    path.to_path_buf(),
                ));
            }
        } else {
            path.to_path_buf()
        };

        self.load(&entry)
    }

    /// Get all loaded modules.
    pub fn modules(&self) -> impl Iterator<Item = &Module> {
        self.loaded.values()
    }

    /// Get all loaded modules as a vector (for codegen).
    pub fn into_modules(self) -> Vec<Module> {
        self.loaded.into_values().collect()
    }

    /// Get the set of module names that have been loaded.
    pub fn loaded_module_names(&self) -> HashSet<String> {
        self.loaded.values().map(|m| m.name.clone()).collect()
    }
}

impl Default for ModuleLoader {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::TempDir;

    fn create_temp_file(dir: &Path, name: &str, content: &str) -> PathBuf {
        let path = dir.join(name);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        let mut file = fs::File::create(&path).unwrap();
        file.write_all(content.as_bytes()).unwrap();
        path
    }

    #[test]
    fn test_load_single_file() {
        let dir = TempDir::new().unwrap();
        let path = create_temp_file(
            dir.path(),
            "math.tb",
            "pub fn add(x: int, y: int) -> int { x + y }",
        );

        let mut loader = ModuleLoader::new();
        let module = loader.load(&path).unwrap();

        assert_eq!(module.name, "math");
        assert_eq!(module.items.len(), 1);
    }

    #[test]
    fn test_load_with_mod_decl() {
        let dir = TempDir::new().unwrap();

        // Create lib.tb that depends on math.tb
        create_temp_file(
            dir.path(),
            "lib.tb",
            "mod math;\npub fn main() -> int { math::add(1, 2) }",
        );
        create_temp_file(
            dir.path(),
            "math.tb",
            "pub fn add(x: int, y: int) -> int { x + y }",
        );

        let mut loader = ModuleLoader::new();
        let lib = loader.load_project(dir.path()).unwrap();

        assert_eq!(lib.name, "lib");
        assert_eq!(loader.loaded_module_names().len(), 2);
        assert!(loader.loaded_module_names().contains("lib"));
        assert!(loader.loaded_module_names().contains("math"));
    }

    #[test]
    fn test_load_nested_module() {
        let dir = TempDir::new().unwrap();

        // Create lib.tb -> utils/mod.tb structure
        create_temp_file(
            dir.path(),
            "lib.tb",
            "mod utils;",
        );
        create_temp_file(
            dir.path(),
            "utils/mod.tb",
            "pub fn helper() -> int { 42 }",
        );

        let mut loader = ModuleLoader::new();
        loader.load_project(dir.path()).unwrap();

        assert_eq!(loader.loaded_module_names().len(), 2);
        assert!(loader.loaded_module_names().contains("lib"));
        assert!(loader.loaded_module_names().contains("utils")); // Module name comes from mod declaration
    }

    #[test]
    fn test_circular_dependency_detection() {
        let dir = TempDir::new().unwrap();

        create_temp_file(dir.path(), "a.tb", "mod b;");
        create_temp_file(dir.path(), "b.tb", "mod a;");

        let mut loader = ModuleLoader::new();
        let result = loader.load(&dir.path().join("a.tb"));

        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.message.contains("circular"));
    }

    #[test]
    fn test_module_not_found() {
        let dir = TempDir::new().unwrap();
        create_temp_file(dir.path(), "lib.tb", "mod missing;");

        let mut loader = ModuleLoader::new();
        let result = loader.load(&dir.path().join("lib.tb"));

        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.message.contains("cannot find module"));
    }
}
