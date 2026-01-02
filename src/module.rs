//! Module system for organizing functions.

use std::collections::{HashMap, HashSet};

use crate::Instruction;

/// A compiled module containing functions.
#[derive(Debug, Clone)]
pub struct Module {
    /// Module name (e.g., "math", "lists").
    pub name: String,

    /// All bytecode for this module, concatenated.
    /// Functions reference ranges within this code.
    pub code: Vec<Instruction>,

    /// Function table: maps (name, arity) -> function metadata.
    pub functions: HashMap<(String, u8), FunctionDef>,

    /// Exported functions (subset of functions callable from outside).
    pub exports: HashSet<(String, u8)>,
}

/// Definition of a function within a module.
#[derive(Debug, Clone)]
pub struct FunctionDef {
    /// Function name.
    pub name: String,

    /// Number of arguments.
    pub arity: u8,

    /// Index into module's code where function starts.
    pub entry: usize,
}

impl Module {
    /// Create a new empty module.
    pub fn new(name: String) -> Self {
        Self {
            name,
            code: Vec::new(),
            functions: HashMap::new(),
            exports: HashSet::new(),
        }
    }

    /// Add a function to this module.
    ///
    /// The function's code is appended to the module's code vector,
    /// and a FunctionDef is created pointing to the entry point.
    pub fn add_function(&mut self, name: String, arity: u8, code: Vec<Instruction>) {
        let entry = self.code.len();

        self.code.extend(code);
        self.functions.insert(
            (name.clone(), arity),
            FunctionDef { name, arity, entry },
        );
    }

    /// Mark a function as exported (callable from other modules).
    pub fn export(&mut self, name: &str, arity: u8) {
        self.exports.insert((name.to_string(), arity));
    }

    /// Look up a function by name and arity.
    pub fn get_function(&self, name: &str, arity: u8) -> Option<&FunctionDef> {
        self.functions.get(&(name.to_string(), arity))
    }

    /// Check if a function is exported.
    pub fn is_exported(&self, name: &str, arity: u8) -> bool {
        self.exports.contains(&(name.to_string(), arity))
    }
}
