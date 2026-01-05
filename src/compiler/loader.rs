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
    /// Cache of loaded modules by synthetic key (path#name for multi-module files).
    loaded: HashMap<PathBuf, Module>,
    /// Files that have been fully processed.
    processed_files: HashSet<PathBuf>,
    /// Paths currently being loaded (for cycle detection).
    loading: Vec<PathBuf>,
}

impl ModuleLoader {
    /// Create a new module loader.
    pub fn new() -> Self {
        Self {
            loaded: HashMap::new(),
            processed_files: HashSet::new(),
            loading: Vec::new(),
        }
    }

    /// Resolve module path for a mod declaration.
    /// Looks for `<dir>/<name>.dream` or `<dir>/<name>/mod.dream`.
    pub fn resolve_module_path(&self, name: &str, from: &Path) -> LoadResult<PathBuf> {
        let parent = from.parent().unwrap_or(Path::new("."));

        // Try <dir>/<name>.dream
        let file_path = parent.join(format!("{}.dream", name));
        if file_path.exists() {
            return Ok(file_path);
        }

        // Try <dir>/<name>/mod.dream
        let dir_path = parent.join(name).join("mod.dream");
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
        // Load all modules from this file
        let modules = self.load_file_modules(path, module_name)?;

        // Return the first module (or find the one matching the name)
        modules
            .into_iter()
            .find(|m| m.name == module_name)
            .or_else(|| self.loaded.values().find(|m| m.name == module_name).cloned())
            .ok_or_else(|| LoadError::new(format!("module '{}' not found in file", module_name)))
    }

    /// Load all modules from a file.
    /// Handles files with multiple `mod name { }` blocks.
    fn load_file_modules(&mut self, path: &Path, fallback_name: &str) -> LoadResult<Vec<Module>> {
        let canonical = path
            .canonicalize()
            .map_err(|e| LoadError::with_path(format!("cannot access file: {}", e), path.to_path_buf()))?;

        // Check if file was already processed
        if self.processed_files.contains(&canonical) {
            return Ok(vec![]);
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
        let modules = parser
            .parse_file_modules(fallback_name)
            .map_err(|e| LoadError::with_path(e.to_string(), canonical.clone()))?;

        // Load dependencies for each module (recursive)
        for module in &modules {
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
                self.load_file_modules(&dep_path, &decl.name)?;
            }
        }

        self.loading.pop();

        // Mark file as processed
        self.processed_files.insert(canonical.clone());

        // Store all modules with a synthetic key (canonical path + module name)
        // Use unique keys to avoid duplicate entries in into_modules()
        for module in &modules {
            let key = canonical.join(format!("#{}", module.name));
            self.loaded.insert(key, module.clone());
        }

        Ok(modules)
    }

    /// Load a project from an entry point.
    /// If path is a directory, looks for main.dream or lib.dream.
    pub fn load_project(&mut self, path: &Path) -> LoadResult<Module> {
        let entry = if path.is_dir() {
            let main_path = path.join("main.dream");
            let lib_path = path.join("lib.dream");

            if main_path.exists() {
                main_path
            } else if lib_path.exists() {
                lib_path
            } else {
                return Err(LoadError::with_path(
                    "no main.dream or lib.dream found in directory",
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

    /// Load all .dream files in a directory (recursively).
    /// This is used for Elixir-style project compilation where all files
    /// in src/ are compiled automatically.
    pub fn load_all_in_dir(&mut self, dir: &Path) -> LoadResult<Vec<Module>> {
        let files = Self::find_dream_files(dir)?;

        for file in &files {
            // Skip if already loaded
            if let Ok(canonical) = file.canonicalize() {
                if self.loaded.contains_key(&canonical) {
                    continue;
                }
            }

            // Load the file (this will also load its mod dependencies)
            self.load(file)?;
        }

        Ok(self.loaded.values().cloned().collect())
    }

    /// Find all .dream files in a directory recursively.
    fn find_dream_files(dir: &Path) -> LoadResult<Vec<PathBuf>> {
        let mut files = Vec::new();
        Self::find_dream_files_recursive(dir, &mut files)?;
        // Sort for deterministic ordering
        files.sort();
        Ok(files)
    }

    fn find_dream_files_recursive(dir: &Path, files: &mut Vec<PathBuf>) -> LoadResult<()> {
        if !dir.is_dir() {
            return Ok(());
        }

        let entries = fs::read_dir(dir)
            .map_err(|e| LoadError::with_path(format!("cannot read directory: {}", e), dir.to_path_buf()))?;

        for entry in entries {
            let entry = entry
                .map_err(|e| LoadError::with_path(format!("cannot read entry: {}", e), dir.to_path_buf()))?;
            let path = entry.path();

            if path.is_dir() {
                Self::find_dream_files_recursive(&path, files)?;
            } else if path.extension().and_then(|s| s.to_str()) == Some("dream") {
                files.push(path);
            }
        }

        Ok(())
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
            "math.dream",
            "pub fn add(x: int, y: int) -> int { x + y }",
        );

        let mut loader = ModuleLoader::new();
        let module = loader.load(&path).unwrap();

        assert_eq!(module.name, "math");
        // 3 items: 2 prelude (Option, Result) + 1 user function
        assert_eq!(module.items.len(), 3);
    }

    #[test]
    fn test_load_with_mod_decl() {
        let dir = TempDir::new().unwrap();

        // Create lib.tb that depends on math.tb
        create_temp_file(
            dir.path(),
            "lib.dream",
            "mod math;\npub fn main() -> int { math::add(1, 2) }",
        );
        create_temp_file(
            dir.path(),
            "math.dream",
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
            "lib.dream",
            "mod utils;",
        );
        create_temp_file(
            dir.path(),
            "utils/mod.dream",
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

        create_temp_file(dir.path(), "a.dream", "mod b;");
        create_temp_file(dir.path(), "b.dream", "mod a;");

        let mut loader = ModuleLoader::new();
        let result = loader.load(&dir.path().join("a.dream"));

        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.message.contains("circular"));
    }

    #[test]
    fn test_module_not_found() {
        let dir = TempDir::new().unwrap();
        create_temp_file(dir.path(), "lib.dream", "mod missing;");

        let mut loader = ModuleLoader::new();
        let result = loader.load(&dir.path().join("lib.dream"));

        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.message.contains("cannot find module"));
    }
}
