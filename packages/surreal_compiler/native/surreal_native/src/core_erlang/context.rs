//! Build context for Core Erlang AST generation.

use std::collections::HashMap;

/// Context for building Core Erlang AST.
/// Tracks local function names so we can distinguish function calls from variable calls.
pub struct BuildContext {
    /// Map of function name -> arity for functions defined in this module
    local_functions: HashMap<String, usize>,
}

impl BuildContext {
    /// Create a new BuildContext from a Surreal module.
    pub fn new(module: &surreal::compiler::Module) -> Self {
        let mut local_functions = HashMap::new();
        for item in &module.items {
            if let surreal::compiler::Item::Function(f) = item {
                local_functions.insert(f.name.clone(), f.params.len());
            }
        }
        BuildContext { local_functions }
    }

    /// Get the arity of a local function by name.
    /// Returns None if the name is not a local function.
    pub fn get_function_arity(&self, name: &str) -> Option<usize> {
        self.local_functions.get(name).copied()
    }
}
