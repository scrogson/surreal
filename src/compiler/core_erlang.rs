//! Core Erlang code generator.
//!
//! This module generates Core Erlang source code from the AST,
//! which can then be compiled to BEAM bytecode using `erlc +from_core`.

use crate::compiler::cfg;
use crate::config::CompileOptions;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};

/// Registry for cross-module generic functions.
/// Stores generic function ASTs keyed by (module_name, function_name).
/// This allows monomorphization of generic functions from other modules.
#[derive(Debug, Default, Clone)]
pub struct GenericFunctionRegistry {
    /// Generic functions: (module_name, func_name) → Function AST
    functions: HashMap<(String, String), Function>,
}

impl GenericFunctionRegistry {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    /// Register a generic function from a module.
    pub fn register(&mut self, module_name: &str, func: &Function) {
        if !func.type_params.is_empty() {
            self.functions
                .insert((module_name.to_string(), func.name.clone()), func.clone());
        }
    }

    /// Look up a generic function by module and name.
    pub fn get(&self, module_name: &str, func_name: &str) -> Option<&Function> {
        self.functions.get(&(module_name.to_string(), func_name.to_string()))
    }

    /// Check if a function exists in the registry.
    pub fn contains(&self, module_name: &str, func_name: &str) -> bool {
        self.functions.contains_key(&(module_name.to_string(), func_name.to_string()))
    }

    /// Batch register generic functions from multiple modules.
    /// This is more efficient than calling register() in a loop with locks.
    pub fn register_batch(&mut self, modules: Vec<(String, Vec<Function>)>) {
        for (module_name, funcs) in modules {
            for func in funcs {
                if !func.type_params.is_empty() {
                    // Register with full module name
                    self.functions
                        .insert((module_name.clone(), func.name.clone()), func.clone());
                    // Also register without the dream:: prefix for easier lookup
                    let short_name = module_name
                        .strip_prefix("dream::")
                        .unwrap_or(&module_name);
                    self.functions
                        .insert((short_name.to_string(), func.name.clone()), func);
                }
            }
        }
    }
}

/// Thread-safe wrapper for GenericFunctionRegistry.
pub type SharedGenericRegistry = Arc<RwLock<GenericFunctionRegistry>>;

use crate::compiler::ast::{
    BinOp, BitEndianness, BitSegmentType, BitSignedness, BitStringSegment, Block,
    EnumPatternFields, EnumVariant, EnumVariantArgs, Expr, ForClause, Function, Item, MatchArm,
    Module, ModuleContext, ModulePath, PathPrefix, Pattern, SpannedExpr, SpannedType, Stmt, StringPart, TraitDef, TraitImpl,
    Type, UnaryOp, UseDecl, UseTree, VariantKind,
};
use crate::compiler::typeck::StructInfo;

/// Core Erlang emitter error.
#[derive(Debug, Clone)]
pub struct CoreErlangError {
    pub message: String,
}

impl CoreErlangError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl std::fmt::Display for CoreErlangError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Core Erlang error: {}", self.message)
    }
}

impl std::error::Error for CoreErlangError {}

pub type CoreErlangResult<T> = Result<T, CoreErlangError>;

/// Core Erlang code emitter.
pub struct CoreErlangEmitter {
    output: String,
    indent: usize,
    /// Counter for generating fresh variable names.
    var_counter: usize,
    /// Current module name (needed for local function calls)
    #[allow(dead_code)]
    module_name: String,
    /// Module context for resolving crate::/super::/self:: paths
    module_context: ModuleContext,
    /// Imported names: local_name → (module, original_name)
    imports: HashMap<String, (String, String)>,
    /// Impl block methods: (actual_type, func_prefix, method_name) for resolving method calls
    /// - actual_type: the struct name for pattern matching (e.g., "User")
    /// - func_prefix: the prefix for the function name (e.g., "User" or "Serialize_User")
    /// - method_name: the method name (e.g., "serialize")
    impl_methods: HashSet<(String, String, String)>,
    /// Trait definitions: trait_name → TraitDef
    traits: HashMap<String, TraitDef>,
    /// Trait implementations: (trait_name, method_name) → Vec<type_name>
    /// Maps each trait method to the types that implement it
    trait_impls: HashMap<(String, String), Vec<String>>,
    /// Whether the current function has a `self` parameter
    has_self_param: bool,
    /// Variables currently in scope (to distinguish from local function calls)
    variables: HashSet<String>,
    /// Local functions defined in the current module (name, arity)
    local_functions: HashSet<(String, usize)>,
    /// Local type names (structs, enums) defined in this module
    /// Used to distinguish `impl Type { }` from `impl Trait { }`
    local_types: HashSet<String>,

    // Monomorphization support
    /// Generic functions: func_name → Function (for functions with type params)
    generic_functions: HashMap<String, Function>,
    /// Pending monomorphizations: (func_name, Vec<type_name>)
    /// Each entry represents a call like foo::<Counter, String>()
    pending_monomorphizations: HashSet<(String, Vec<String>)>,
    /// Current type parameter substitutions during monomorphized function emission
    /// Maps type param name (e.g., "T") to concrete type name (e.g., "Counter")
    type_param_subst: HashMap<String, String>,
    /// Trait bounds for current type params: type_param_name → Vec<trait_name>
    type_param_bounds: HashMap<String, Vec<String>>,
    /// Type parameter names of the current function being emitted (for non-monomorphized generics)
    /// When set, calls using these type params should use dynamic dispatch, not monomorphization
    current_func_type_params: HashSet<String>,

    // Cross-module generic function support
    /// Registry of generic functions from other modules (shared across compilations)
    external_generics: Option<SharedGenericRegistry>,
    /// Cross-module monomorphizations: (source_module, func_name, Vec<type_name>)
    /// Functions from other modules that need to be monomorphized in this module
    cross_module_monomorphizations: HashSet<(String, String, Vec<String>)>,
    /// When emitting a cross-module inlined function, this tracks the source module.
    /// Used to rewrite same-module calls from the source to cross-module calls.
    cross_module_inlining_source: Option<String>,
    /// Compile options for conditional compilation (test mode, features).
    compile_options: CompileOptions,
    /// Maps Dream extern module name -> BEAM module name
    /// Used for #[name = "Elixir.Enum"] attribute support
    extern_module_names: HashMap<String, String>,
    /// Maps (module, dream_name, arity) -> BEAM function name
    /// Used for #[name = "encode!"] attribute support on functions
    extern_function_names: HashMap<(String, String, usize), String>,
    /// Struct metadata including record names (struct_name -> StructInfo)
    struct_info: HashMap<String, StructInfo>,
    /// Variable type tracking for record field access (var_name -> struct_type_name)
    variable_types: HashMap<String, String>,
}

impl CoreErlangEmitter {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent: 0,
            var_counter: 0,
            module_name: String::new(),
            module_context: ModuleContext::new(),
            imports: HashMap::new(),
            impl_methods: HashSet::new(),
            traits: HashMap::new(),
            trait_impls: HashMap::new(),
            has_self_param: false,
            variables: HashSet::new(),
            local_functions: HashSet::new(),
            local_types: HashSet::new(),
            generic_functions: HashMap::new(),
            pending_monomorphizations: HashSet::new(),
            type_param_subst: HashMap::new(),
            type_param_bounds: HashMap::new(),
            current_func_type_params: HashSet::new(),
            external_generics: None,
            cross_module_monomorphizations: HashSet::new(),
            cross_module_inlining_source: None,
            compile_options: CompileOptions::new(),
            extern_module_names: HashMap::new(),
            extern_function_names: HashMap::new(),
            struct_info: HashMap::new(),
            variable_types: HashMap::new(),
        }
    }

    /// Create an emitter with compile options.
    pub fn with_options(options: CompileOptions) -> Self {
        Self {
            compile_options: options,
            ..Self::new()
        }
    }

    /// Create an emitter with a shared generic function registry.
    pub fn with_registry(registry: SharedGenericRegistry) -> Self {
        Self {
            external_generics: Some(registry),
            ..Self::new()
        }
    }

    /// Create an emitter with a module context for path resolution.
    pub fn with_context(context: ModuleContext) -> Self {
        Self {
            module_context: context,
            ..Self::new()
        }
    }

    /// Create an emitter with both a registry and module context.
    pub fn with_registry_and_context(
        registry: SharedGenericRegistry,
        context: ModuleContext,
    ) -> Self {
        Self {
            external_generics: Some(registry),
            module_context: context,
            ..Self::new()
        }
    }

    /// Create a fully configured emitter with all options.
    pub fn with_all(
        registry: SharedGenericRegistry,
        context: ModuleContext,
        options: CompileOptions,
    ) -> Self {
        Self {
            external_generics: Some(registry),
            module_context: context,
            compile_options: options,
            ..Self::new()
        }
    }

    /// Set the extern module name mappings (Dream name -> BEAM name).
    /// Used for #[name = "Elixir.Enum"] attribute support.
    pub fn set_extern_module_names(&mut self, mappings: HashMap<String, String>) {
        self.extern_module_names = mappings;
    }

    /// Set the extern function name mappings ((module, dream_name, arity) -> BEAM name).
    /// Used for #[name = "encode!"] attribute support on functions.
    pub fn set_extern_function_names(
        &mut self,
        mappings: HashMap<(String, String, usize), String>,
    ) {
        self.extern_function_names = mappings;
    }

    /// Set the struct info mappings (struct_name -> StructInfo).
    /// Used for Erlang record compilation support.
    pub fn set_struct_info(&mut self, info: HashMap<String, StructInfo>) {
        self.struct_info = info;
    }

    /// Register this module's generic functions in the shared registry.
    /// Call this after emit_module to make generics available to other modules.
    pub fn register_generics(&self, registry: &mut GenericFunctionRegistry) {
        for (_name, func) in &self.generic_functions {
            registry.register(&self.module_name, func);
            // Also register without the dream:: prefix for easier lookup (stdlib modules)
            let short_name = self.module_name
                .strip_prefix(Self::STDLIB_PREFIX)
                .unwrap_or(&self.module_name);
            registry.register(short_name, func);
        }
    }

    /// Get the module name and generic functions for deferred batch registration.
    /// This allows collecting generics from multiple modules before acquiring a write lock.
    pub fn get_generics_for_registration(&self) -> (String, Vec<Function>) {
        let funcs: Vec<Function> = self.generic_functions.values().cloned().collect();
        (self.module_name.clone(), funcs)
    }

    /// Known stdlib modules that live under the dream:: namespace.
    const STDLIB_MODULES: &'static [&'static str] = &[
        "io", "list", "enumerable", "iterator", "option", "result",
        "string", "map", "file", "timer", "display", "convert",
        "process", "genserver", "supervisor", "application", "logger",
    ];

    /// Resolve a simple module name string, adding dream:: prefix for Dream modules.
    /// All Dream modules (both stdlib and user modules) get the dream:: prefix.
    fn resolve_module_name(module: &str) -> String {
        // If already has dream:: prefix, use as-is
        if module.starts_with(Self::STDLIB_PREFIX) {
            module.to_string()
        } else {
            // Add dream:: prefix for all Dream modules
            format!("{}{}", Self::STDLIB_PREFIX, module)
        }
    }

    /// Resolve a module path, handling stdlib prefixing and relative paths.
    /// Uses the module context to resolve crate::/super::/self:: prefixes.
    fn resolve_module_path(&self, module: &ModulePath) -> String {
        match &module.prefix {
            PathPrefix::None => {
                let path_str = module.segments_string();
                // If already has multiple segments, use as-is
                if module.segments.len() > 1 {
                    path_str
                } else if Self::STDLIB_MODULES.contains(&path_str.as_str()) {
                    // Stdlib module - add dream:: prefix
                    format!("dream::{}", path_str)
                } else {
                    // User module - use as-is
                    path_str
                }
            }
            // Use module context to resolve relative paths
            PathPrefix::Crate | PathPrefix::Super | PathPrefix::SelfMod => {
                // Try to resolve using the module context
                if let Some(resolved) = self.module_context.resolve(module) {
                    resolved
                } else {
                    // Fall back to unresolved string if context is not available
                    // (e.g., standalone files without dream.toml)
                    module.to_unresolved_string()
                }
            }
        }
    }

    /// Resolve a user module path (like "utils::math") to its full form.
    /// For user modules in a package, prepends the package name.
    /// For standalone files or stdlib, uses beam_module_name.
    fn resolve_user_module_path(&self, path: &str) -> String {
        // First check if it's a stdlib module by looking at the first segment
        let first_segment = path.split("::").next().unwrap_or(path);
        if Self::STDLIB_MODULES.contains(&first_segment) {
            // Stdlib module - add dream:: prefix to first segment
            format!("dream::{}", path)
        } else if let Some(pkg) = &self.module_context.package_name {
            // User module in a package - prepend package name
            format!("{}::{}", pkg, path)
        } else {
            // Standalone file - use as-is
            path.to_string()
        }
    }

    /// Collect imports from a UseDecl into the imports map.
    fn collect_imports(&mut self, use_decl: &UseDecl) {
        match &use_decl.tree {
            UseTree::Path { module, name, rename } => {
                let local_name = rename.as_ref().unwrap_or(name).clone();
                let resolved_module = self.resolve_module_path(module);
                self.imports.insert(local_name, (resolved_module, name.clone()));
            }
            UseTree::Glob { module: _ } => {
                // Glob imports require knowing what the module exports.
                // TODO: Implement glob imports when module metadata is available.
            }
            UseTree::Group { module, items } => {
                let resolved_module = self.resolve_module_path(module);
                for item in items {
                    let local_name = item.rename.as_ref().unwrap_or(&item.name).clone();
                    self.imports.insert(local_name, (resolved_module.clone(), item.name.clone()));
                }
            }
        }
    }

    /// Generate a fresh variable name.
    #[allow(dead_code)]
    fn fresh_var(&mut self) -> String {
        let name = format!("_@c{}", self.var_counter);
        self.var_counter += 1;
        name
    }

    /// Check if a return type is Result<T, E>.
    fn is_result_type(ty: &Option<SpannedType>) -> bool {
        matches!(ty.as_ref().map(|t| &t.ty), Some(Type::Named { name, .. }) if name == "Result")
    }

    /// Check if a return type is Option<T>.
    fn is_option_type(ty: &Option<SpannedType>) -> bool {
        matches!(ty.as_ref().map(|t| &t.ty), Some(Type::Named { name, .. }) if name == "Option")
    }

    /// Find all types that have a method with the given name in their impl block.
    /// Returns (actual_type, func_prefix) pairs.
    fn find_impl_types_for_method(&self, method_name: &str) -> Vec<(String, String)> {
        self.impl_methods
            .iter()
            .filter(|(_, _, m)| m == method_name)
            .map(|(actual_type, func_prefix, _)| (actual_type.clone(), func_prefix.clone()))
            .collect()
    }

    /// Check if a type has a method with the given name in its impl block.
    /// Used for Type::method() calls.
    fn has_impl_method(&self, type_name: &str, method_name: &str) -> bool {
        self.impl_methods
            .iter()
            .any(|(actual_type, _, m)| actual_type == type_name && m == method_name)
    }

    /// Check if a trait impl method exists with the given func_prefix and method name.
    /// Used for checking From trait impls.
    fn has_trait_impl_method(&self, func_prefix: &str, method_name: &str) -> bool {
        self.impl_methods
            .iter()
            .any(|(_, fp, m)| fp == func_prefix && m == method_name)
    }

    /// Collect variable names from a pattern and add them to the variables set.
    fn collect_pattern_vars(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Ident(name) if name != "_" => {
                self.variables.insert(name.clone());
            }
            Pattern::Tuple(patterns) | Pattern::List(patterns) => {
                for p in patterns {
                    self.collect_pattern_vars(p);
                }
            }
            Pattern::ListCons { head, tail } => {
                self.collect_pattern_vars(head);
                self.collect_pattern_vars(tail);
            }
            Pattern::Struct { fields, .. } => {
                for (_, field_pat) in fields {
                    self.collect_pattern_vars(field_pat);
                }
            }
            Pattern::Enum { fields, .. } => {
                match fields {
                    EnumPatternFields::Unit => {}
                    EnumPatternFields::Tuple(patterns) => {
                        for p in patterns {
                            self.collect_pattern_vars(p);
                        }
                    }
                    EnumPatternFields::Struct(field_patterns) => {
                        for (_, p) in field_patterns {
                            self.collect_pattern_vars(p);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    /// Check if an expression contains a return statement.
    fn contains_return(expr: &Expr) -> bool {
        match expr {
            Expr::Return(_) => true,
            Expr::If { then_block, else_block, .. } => {
                Self::block_contains_return(then_block)
                    || else_block.as_ref().is_some_and(|b| Self::block_contains_return(b))
            }
            Expr::Match { arms, .. } => {
                arms.iter().any(|arm| Self::contains_return(arm.body.inner()))
            }
            Expr::Block(block) => Self::block_contains_return(block),
            _ => false,
        }
    }

    /// Check if a block contains a return statement.
    fn block_contains_return(block: &Block) -> bool {
        block.stmts.iter().any(|stmt| match stmt {
            Stmt::Expr { expr: e, .. } => Self::contains_return(e.inner()),
            Stmt::Let { value, .. } => Self::contains_return(value.inner()),
        }) || block.expr.as_ref().is_some_and(|e| Self::contains_return(e.inner()))
    }


    /// Convert an identifier to Core Erlang variable format (capitalize first letter).
    fn var_name(name: &str) -> String {
        let mut chars: Vec<char> = name.chars().collect();
        if !chars.is_empty() {
            chars[0] = chars[0].to_ascii_uppercase();
        }
        // Prefix with _ if it starts with a number or is a reserved word
        let result: String = chars.into_iter().collect();
        if result.chars().next().map(|c| c.is_numeric()).unwrap_or(false) {
            format!("_{}", result)
        } else {
            result
        }
    }

    /// Extract the simple trait name from a potentially qualified name.
    /// e.g., "inspect::Inspect" -> "Inspect", "Inspect" -> "Inspect"
    fn simple_trait_name(name: &str) -> &str {
        name.rsplit("::").next().unwrap_or(name)
    }

    /// Find a trait impl method for a type.
    /// Searches impl_methods for entries matching pattern `*_TypeName` with the given method.
    /// Returns the full mangled name if found.
    fn find_trait_impl_method(
        &self,
        type_name: &str,
        method_name: &str,
        trait_type_args: &[Type],
    ) -> Option<String> {
        // Build the suffix we're looking for: "_TypeName"
        let type_suffix = format!("_{}", type_name);

        // If we have explicit trait type args (turbofish), look for an exact match
        if !trait_type_args.is_empty() {
            let trait_type_suffix = self.format_trait_type_args(trait_type_args);
            // Look for patterns like "From_int_Wrapper" when calling Wrapper::from::<int>()
            for (_actual_type, func_prefix, method) in &self.impl_methods {
                if method == method_name && func_prefix.ends_with(&type_suffix) {
                    // Check if this matches our trait type args
                    // E.g., "From_int_Wrapper" should match trait_type_suffix "_int"
                    let without_type = func_prefix.strip_suffix(&type_suffix).unwrap_or(func_prefix);
                    if without_type.ends_with(&trait_type_suffix) {
                        return Some(format!("{}_{}", func_prefix, method_name));
                    }
                }
            }
        }

        // Without explicit type args, look for any trait impl for this type
        // If multiple impls have the method, we can't disambiguate (return None)
        let mut matches = Vec::new();
        for (_actual_type, func_prefix, method) in &self.impl_methods {
            if method == method_name && func_prefix.ends_with(&type_suffix) {
                matches.push(format!("{}_{}", func_prefix, method_name));
            }
        }

        if matches.len() == 1 {
            Some(matches.into_iter().next().unwrap())
        } else {
            // Zero or multiple matches - can't disambiguate
            None
        }
    }

    /// Format trait type arguments for name mangling.
    /// e.g., [int, string] -> "_int_string", [] -> ""
    fn format_trait_type_args(&self, type_args: &[Type]) -> String {
        if type_args.is_empty() {
            String::new()
        } else {
            let names: Vec<String> = type_args.iter().map(|t| self.type_to_name(t)).collect();
            format!("_{}", names.join("_"))
        }
    }

    /// Check if a function name is a built-in function (BIF).
    fn is_bif(name: &str) -> bool {
        matches!(
            name,
            "self"
                | "is_atom"
                | "is_binary"
                | "is_integer"
                | "is_float"
                | "is_list"
                | "is_tuple"
                | "is_pid"
                | "is_reference"
                | "is_function"
                | "is_map"
                | "is_boolean"
                | "is_number"
                | "hd"
                | "tl"
                | "length"
                | "tuple_size"
                | "map_size"
                | "element"
                | "setelement"
                | "make_ref"
                | "throw"
                | "error"
                | "exit"
                | "abs"
                | "trunc"
                | "round"
                | "float"
                | "atom_to_list"
                | "list_to_atom"
                | "integer_to_list"
                | "list_to_integer"
                | "float_to_list"
                | "list_to_float"
                | "tuple_to_list"
                | "list_to_tuple"
                | "binary_to_list"
                | "list_to_binary"
                | "term_to_binary"
                | "binary_to_term"
                | "size"
                | "byte_size"
                | "bit_size"
                | "node"
                | "nodes"
                | "time"
                | "date"
                | "register"
                | "unregister"
                | "whereis"
                | "registered"
                | "link"
                | "unlink"
                | "spawn_link"
                | "monitor"
                | "demonitor"
                | "process_info"
                | "processes"
                | "put"
                | "get"
                | "erase"
                | "get_keys"
        )
    }

    /// Convert an AST Type to a simple type name string for monomorphization.
    fn type_to_name(&self, ty: &Type) -> String {
        match ty {
            Type::Int => "int".to_string(),
            Type::Float => "float".to_string(),
            Type::String => "string".to_string(),
            Type::Atom => "atom".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Unit => "unit".to_string(),
            Type::Binary => "binary".to_string(),
            Type::Pid => "pid".to_string(),
            Type::Ref => "ref".to_string(),
            Type::Any => "any".to_string(),
            Type::Map => "map".to_string(),
            Type::Named { name, .. } => {
                // Check if this is a type parameter that should be substituted
                if let Some(concrete_type) = self.type_param_subst.get(name) {
                    concrete_type.clone()
                } else {
                    name.clone()
                }
            }
            Type::TypeVar(name) => {
                // Check if this is a type parameter that should be substituted
                if let Some(concrete_type) = self.type_param_subst.get(name) {
                    concrete_type.clone()
                } else {
                    name.clone()
                }
            }
            Type::Tuple(types) => {
                let names: Vec<String> = types.iter().map(|t| self.type_to_name(t)).collect();
                format!("tuple_{}", names.join("_"))
            }
            Type::List(inner) => format!("list_{}", self.type_to_name(inner)),
            Type::Fn { params, ret } => {
                let param_names: Vec<String> =
                    params.iter().map(|t| self.type_to_name(t)).collect();
                format!("fn_{}_{}", param_names.join("_"), self.type_to_name(ret))
            }
            Type::AssociatedType { base, name } => format!("{}_{}", base, name),
            Type::AtomLiteral(name) => format!("atom_{}", name),
            Type::Union(types) => {
                let names: Vec<String> = types.iter().map(|t| self.type_to_name(t)).collect();
                format!("union_{}", names.join("_or_"))
            }
        }
    }

    /// Infer the type name of an expression (for method dispatch).
    /// This is a simple inference for primitive types and identifiers.
    fn infer_expr_type(&self, expr: &Expr) -> String {
        match expr {
            Expr::Int(_) => "int".to_string(),
            Expr::String(_) | Expr::StringInterpolation(_) => "string".to_string(),
            Expr::Atom(_) => "atom".to_string(),
            Expr::Bool(_) => "bool".to_string(),
            Expr::BitString(_) => "binary".to_string(),
            Expr::Unit => "unit".to_string(),
            Expr::List(_) | Expr::ListCons { .. } => "list".to_string(),
            Expr::MapLiteral(_) => "map".to_string(),
            Expr::Tuple(_) => "tuple".to_string(),
            Expr::StructInit { name, .. } => name.clone(),
            // For other expressions, default to "any" (runtime dispatch)
            _ => "any".to_string(),
        }
    }

    /// Emit a string to the output.
    fn emit(&mut self, s: &str) {
        self.output.push_str(s);
    }

    /// Emit a newline and indentation.
    fn newline(&mut self) {
        self.emit("\n");
        for _ in 0..self.indent {
            self.emit("    ");
        }
    }

    /// Emit trait method dispatch.
    /// Generates a case expression that matches on __struct__ and calls the appropriate impl.
    /// In monomorphized contexts, can emit direct calls instead of runtime dispatch.
    fn emit_trait_dispatch(
        &mut self,
        trait_name: &str,
        method_name: &str,
        args: &[SpannedExpr],
    ) -> CoreErlangResult<()> {
        if args.is_empty() {
            return Err(CoreErlangError::new(format!(
                "{}::{} requires at least one argument (the receiver)",
                trait_name, method_name
            )));
        }

        // Check if we're in a monomorphized context and can do static dispatch
        // Look for a type parameter that has this trait as a bound
        let static_type = self.find_static_dispatch_type(trait_name);

        if let Some(concrete_type) = static_type {
            // Static dispatch - call the concrete implementation directly
            let mangled_name = format!("{}_{}_{}", trait_name, concrete_type, method_name);
            self.emit(&format!("apply '{}'/{}", mangled_name, args.len()));
            self.emit("(");
            self.emit_args(args)?;
            self.emit(")");
            return Ok(());
        }

        // Dynamic dispatch - runtime case on __struct__
        // Look up the struct's module and call the impl method there
        let key = (trait_name.to_string(), method_name.to_string());
        let impl_types = self.trait_impls.get(&key).cloned().unwrap_or_default();

        // Get current module name without dream:: prefix (for stdlib)
        let current_module = self
            .module_name
            .strip_prefix(Self::STDLIB_PREFIX)
            .unwrap_or(&self.module_name)
            .to_string();

        // Bind the first argument (receiver) to a variable for dispatch
        let receiver_var = self.fresh_var();
        self.emit(&format!("let <{}> = ", receiver_var));
        self.emit_expr(&args[0])?;
        self.newline();
        self.emit("in ");

        // Get the struct tag
        let tag_var = self.fresh_var();
        self.emit(&format!(
            "let <{}> = call 'maps':'get'('__struct__', {}) in",
            tag_var, receiver_var
        ));
        self.newline();

        if impl_types.is_empty() {
            // No local implementations - emit universal dispatch via struct tag
            // Parse the struct tag to get module and type, then call module:Trait_Type_method
            self.emit_universal_trait_dispatch(
                trait_name,
                method_name,
                &tag_var,
                &receiver_var,
                args,
            )?;
        } else {
            // Have local implementations - use case dispatch
            self.emit(&format!("case {} of", tag_var));
            self.newline();
            self.indent += 1;

            // Generate a clause for each implementing type
            for (i, type_name) in impl_types.iter().enumerate() {
                let mangled_name = format!("{}_{}_{}", trait_name, type_name, method_name);

                // Match on fully qualified atom 'module::Type'
                self.emit(&format!("<'{}::{}'>", current_module, type_name));
                self.emit(" when 'true' ->");
                self.newline();
                self.indent += 1;

                // Call the implementation with the receiver and remaining args
                self.emit(&format!("apply '{}'/{}", mangled_name, args.len()));
                self.emit("(");
                self.emit(&receiver_var);
                for arg in args.iter().skip(1) {
                    self.emit(", ");
                    self.emit_expr(arg)?;
                }
                self.emit(")");

                self.indent -= 1;
                if i < impl_types.len() - 1 {
                    self.newline();
                }
            }

            // Add a catch-all clause for types from other modules
            self.newline();
            self.emit("<_OtherTag> when 'true' ->");
            self.newline();
            self.indent += 1;
            self.emit_universal_trait_dispatch(
                trait_name,
                method_name,
                "_OtherTag",
                &receiver_var,
                args,
            )?;
            self.indent -= 1;

            self.newline();
            self.indent -= 1;
            self.emit("end");
        }

        Ok(())
    }

    /// Find a concrete type for static trait dispatch in monomorphized context.
    /// Returns Some(type_name) if we're in a monomorphized function with a type
    /// parameter that has the given trait as a bound.
    fn find_static_dispatch_type(&self, trait_name: &str) -> Option<String> {
        // Look through our type parameter bounds to find one with this trait
        for (type_param, bounds) in &self.type_param_bounds {
            if bounds.contains(&trait_name.to_string()) {
                // Found a type param with this bound - get its concrete type
                if let Some(concrete_type) = self.type_param_subst.get(type_param) {
                    // Skip 'any' type - it's not a real type with trait implementations,
                    // so we need to fall through to dynamic dispatch
                    if concrete_type == "any" {
                        return None;
                    }
                    // Skip if the "concrete" type is actually a type parameter of the current function
                    // This happens when we're emitting a generic function or when a bogus monomorphization
                    // was created with a type param name like "T"
                    if self.current_func_type_params.contains(concrete_type) {
                        return None;
                    }
                    return Some(concrete_type.clone());
                }
            }
        }
        None
    }

    /// Emit universal trait dispatch that works for any struct from any module.
    /// Parses the struct tag (e.g., 'module::Type') and calls dream::module:Trait_Type_method
    /// using erlang:apply/3 for dynamic dispatch.
    fn emit_universal_trait_dispatch(
        &mut self,
        trait_name: &str,
        method_name: &str,
        tag_var: &str,
        receiver_var: &str,
        args: &[SpannedExpr],
    ) -> CoreErlangResult<()> {
        // The struct tag is an atom like 'dream::module::Type'
        // We need to parse it at runtime to get the module and type
        // Then call dream::module:Trait_Type_method(receiver, args...)

        // Convert atom to list for parsing
        let tag_list_var = self.fresh_var();
        self.emit(&format!(
            "let <{}> = call 'erlang':'atom_to_list'({}) in",
            tag_list_var, tag_var
        ));
        self.newline();

        // Split on "::" to get parts like ["dream", "http_api", "models", "user", "User"]
        let parts_var = self.fresh_var();
        self.emit(&format!(
            "let <{}> = call 'string':'split'({}, \"::\",'all') in",
            parts_var, tag_list_var
        ));
        self.newline();

        // Extract type name (last element)
        let type_var = self.fresh_var();
        self.emit(&format!(
            "let <{}> = call 'lists':'last'({}) in",
            type_var, parts_var
        ));
        self.newline();

        // Extract module parts (all but last), then join with "::"
        let module_parts_var = self.fresh_var();
        self.emit(&format!(
            "let <{}> = call 'lists':'droplast'({}) in",
            module_parts_var, parts_var
        ));
        self.newline();

        // Join module parts with "::" to get full module name (e.g., "dream::http_api::models::user")
        let full_module_var = self.fresh_var();
        self.emit(&format!(
            "let <{}> = call 'erlang':'list_to_atom'(call 'lists':'flatten'(call 'lists':'join'(\"::\", {}))) in",
            full_module_var, module_parts_var
        ));
        self.newline();

        // Build the mangled method name: Trait_Type_method
        let method_name_var = self.fresh_var();
        self.emit(&format!(
            "let <{}> = call 'erlang':'list_to_atom'(call 'lists':'flatten'([[\"{}\" | \"_\"], {}, \"_{}\"])) in",
            method_name_var, trait_name, type_var, method_name
        ));
        self.newline();

        // Build the args list
        self.emit("call 'erlang':'apply'(");
        self.emit(&full_module_var);
        self.emit(", ");
        self.emit(&method_name_var);
        self.emit(", [");
        self.emit(receiver_var);
        for arg in args.iter().skip(1) {
            self.emit(", ");
            self.emit_expr(arg)?;
        }
        self.emit("])");

        Ok(())
    }

    /// Emit UFCS method dispatch for impl methods.
    /// Generates a case expression that matches on __struct__ and calls the appropriate impl.
    /// impl_types is a list of (actual_type, func_prefix) tuples.
    fn emit_method_dispatch(
        &mut self,
        method_name: &str,
        receiver: &SpannedExpr,
        args: &[SpannedExpr],
        impl_types: &[(String, String)],
    ) -> CoreErlangResult<()> {
        // Total arity includes receiver
        let arity = args.len() + 1;

        // Bind the receiver to a variable for dispatch
        let receiver_var = self.fresh_var();
        self.emit(&format!("let <{}> = ", receiver_var));
        self.emit_expr(receiver)?;
        self.newline();
        self.emit("in ");

        // Generate case on maps:get('__struct__', Receiver, undefined)
        // Using 3-arg version with default to handle non-struct values gracefully
        self.emit(&format!(
            "case call 'maps':'get'('__struct__', {}, 'undefined') of",
            receiver_var
        ));
        self.newline();
        self.indent += 1;

        // Generate a clause for each implementing type (local types)
        for (i, (actual_type, func_prefix)) in impl_types.iter().enumerate() {
            let mangled_name = format!("{}_{}", func_prefix, method_name);

            // Match on fully qualified atom 'module::Type' (includes dream:: prefix)
            // Use actual_type for the pattern (not func_prefix which may include trait name)
            self.emit(&format!("<'{}::{}'>", self.module_name, actual_type));
            self.emit(" when 'true' ->");
            self.newline();
            self.indent += 1;

            // Call the implementation with the receiver and args
            self.emit(&format!("apply '{}'/{}", mangled_name, arity));
            self.emit("(");
            self.emit(&receiver_var);
            for arg in args {
                self.emit(", ");
                self.emit_expr(arg)?;
            }
            self.emit(")");

            self.indent -= 1;
            if i < impl_types.len() - 1 {
                self.newline();
            }
        }

        // Add a catch-all clause that raises an error for unknown types
        self.newline();
        self.emit("<_Other> when 'true' ->");
        self.newline();
        self.indent += 1;
        self.emit(&format!(
            "call 'erlang':'error'({{'undefined_method', '{}', _Other}})",
            method_name
        ));
        self.indent -= 1;

        self.newline();
        self.indent -= 1;
        self.emit("end");

        Ok(())
    }

    /// Emit dynamic method dispatch for cross-module struct methods.
    /// Uses the __struct__ tag 'module::Type' atom to dispatch to the correct module.
    ///
    /// The __struct__ tag format is 'beam_module::TypeName', e.g.:
    ///   'dream::http_api::models::user::User'
    /// We need to:
    ///   - Module: all parts except last joined by "::" -> 'dream::http_api::models::user'
    ///   - Type: last part -> 'User'
    ///   - Function: Type_method -> 'User_to_json'
    fn emit_dynamic_method_dispatch(
        &mut self,
        method_name: &str,
        receiver: &SpannedExpr,
        args: &[SpannedExpr],
    ) -> CoreErlangResult<()> {
        // Bind the receiver to a variable for dispatch
        let receiver_var = self.fresh_var();
        self.emit(&format!("let <{}> = ", receiver_var));
        self.emit_expr(receiver)?;
        self.newline();
        self.emit("in ");

        // Get the struct tag atom 'module::Type'
        let tag_var = self.fresh_var();
        self.emit(&format!(
            "let <{}> = call 'maps':'get'('__struct__', {}, 'undefined')",
            tag_var, receiver_var
        ));
        self.newline();
        self.emit("in ");

        // Convert tag atom to list and split on "::"
        // TagList = atom_to_list('beam_mod::Type') -> "beam_mod::Type"
        let tag_list_var = self.fresh_var();
        self.emit(&format!(
            "let <{}> = call 'erlang':'atom_to_list'({})",
            tag_list_var, tag_var
        ));
        self.newline();
        self.emit("in ");

        // Split on "::" using string:split/3 -> ["dream", "http_api", "models", "user", "User"]
        // "::" is [58, 58] in character codes
        let parts_var = self.fresh_var();
        self.emit(&format!(
            "let <{}> = call 'string':'split'({}, [58, 58], 'all')",
            parts_var, tag_list_var
        ));
        self.newline();
        self.emit("in ");

        // Extract type string: lists:last(Parts) -> "User"
        let type_str_var = self.fresh_var();
        self.emit(&format!(
            "let <{}> = call 'lists':'last'({})",
            type_str_var, parts_var
        ));
        self.newline();
        self.emit("in ");

        // Extract module parts: lists:droplast(Parts) -> ["dream", "http_api", "models", "user"]
        let mod_parts_var = self.fresh_var();
        self.emit(&format!(
            "let <{}> = call 'lists':'droplast'({})",
            mod_parts_var, parts_var
        ));
        self.newline();
        self.emit("in ");

        // Join module parts with "::" -> "dream::http_api::models::user"
        // lists:join("::", ModParts) returns a nested list, flatten it
        let mod_str_var = self.fresh_var();
        self.emit(&format!(
            "let <{}> = call 'lists':'flatten'(call 'lists':'join'([58, 58], {}))",
            mod_str_var, mod_parts_var
        ));
        self.newline();
        self.emit("in ");

        // Convert module string to atom
        let beam_mod_var = self.fresh_var();
        self.emit(&format!(
            "let <{}> = call 'erlang':'list_to_atom'({})",
            beam_mod_var, mod_str_var
        ));
        self.newline();
        self.emit("in ");

        // Build function name: TypeStr ++ "_" ++ method -> list_to_atom
        // [95] = "_"
        let method_chars = Self::string_to_core_list(method_name);
        let func_str_var = self.fresh_var();
        self.emit(&format!(
            "let <{}> = call 'erlang':'++'({}, call 'erlang':'++'([95], {}))",
            func_str_var, type_str_var, method_chars
        ));
        self.newline();
        self.emit("in ");

        let func_var = self.fresh_var();
        self.emit(&format!(
            "let <{}> = call 'erlang':'list_to_atom'({})",
            func_var, func_str_var
        ));
        self.newline();
        self.emit("in ");

        // Call erlang:apply(BeamMod, Func, [Receiver | Args])
        self.emit(&format!(
            "call 'erlang':'apply'({}, {}, [{}",
            beam_mod_var, func_var, receiver_var
        ));
        for arg in args {
            self.emit(", ");
            self.emit_expr(arg)?;
        }
        self.emit("])");

        Ok(())
    }

    /// Emit all methods for a trait implementation, including default methods.
    /// Also emits simple-name wrapper functions (Type_method) for dynamic dispatch.
    fn emit_trait_impl_methods(&mut self, trait_impl: &TraitImpl) -> CoreErlangResult<()> {
        // Collect method names explicitly implemented (only those passing cfg)
        let impl_method_names: HashSet<String> =
            trait_impl.methods.iter()
                .filter(|m| cfg::should_include(&m.attrs, &self.compile_options))
                .map(|m| m.name.clone())
                .collect();

        let simple_trait = Self::simple_trait_name(&trait_impl.trait_name);
        // Format trait type args for mangling: From<int> -> "From_int"
        let trait_type_args: Vec<Type> = trait_impl.trait_type_args.iter().map(|st| st.ty.clone()).collect();
        let trait_type_args_suffix = self.format_trait_type_args(&trait_type_args);

        // Emit explicitly implemented methods (filtered by cfg)
        for method in &trait_impl.methods {
            // Skip methods excluded by cfg
            if !cfg::should_include(&method.attrs, &self.compile_options) {
                continue;
            }
            // Mangled name: Trait_TypeArg_ImplType_method
            // e.g., From_int_MyType_from
            let mangled_name = format!(
                "{}{}_{}_{}",
                simple_trait, trait_type_args_suffix, trait_impl.type_name, method.name
            );
            let mangled_method = Function {
                attrs: vec![],
                name: mangled_name.clone(),
                type_params: method.type_params.clone(),
                params: method.params.clone(),
                guard: method.guard.clone(),
                return_type: method.return_type.clone(),
                body: method.body.clone(),
                is_pub: method.is_pub,
                span: method.span.clone(),
            };
            self.newline();
            self.emit_function(&mangled_method)?;

            // Also emit simple-name wrapper for dynamic dispatch: Type_method -> Trait_Type_method
            // This allows cross-module method calls like user.to_json() to work
            let simple_name = format!("{}_{}", trait_impl.type_name, method.name);
            self.emit_simple_method_wrapper(
                &simple_name,
                &mangled_name,
                method.params.len(),
            )?;
        }

        // Emit default methods not overridden in this impl
        if let Some(trait_def) = self.traits.get(&trait_impl.trait_name).cloned() {
            for trait_method in &trait_def.methods {
                if let Some(ref body) = trait_method.body {
                    if !impl_method_names.contains(&trait_method.name) {
                        let mangled_name = format!(
                            "{}{}_{}_{}",
                            simple_trait, trait_type_args_suffix, trait_impl.type_name, trait_method.name
                        );
                        let default_method = Function {
                            attrs: vec![],
                            name: mangled_name.clone(),
                            type_params: trait_method.type_params.clone(),
                            params: trait_method.params.clone(),
                            guard: None,
                            return_type: trait_method.return_type.clone(),
                            body: body.clone(),
                            is_pub: true, // Default methods are public
                            span: 0..0,   // Synthetic span
                        };
                        self.newline();
                        self.emit_function(&default_method)?;

                        // Also emit simple-name wrapper for default methods
                        let simple_name = format!("{}_{}", trait_impl.type_name, trait_method.name);
                        self.emit_simple_method_wrapper(
                            &simple_name,
                            &mangled_name,
                            trait_method.params.len(),
                        )?;
                    }
                }
            }
        }

        // Blanket impl: impl From<T> for U automatically provides impl Into<U> for T
        // When we have `impl From<int> for Wrapper`, generate `impl Into<Wrapper> for int`
        if simple_trait == "From" && trait_impl.trait_type_args.len() == 1 {
            self.emit_blanket_into_impl(trait_impl)?;
        }

        Ok(())
    }

    /// Emit a simple wrapper function that delegates to a mangled trait method.
    /// This enables dynamic dispatch for cross-module method calls.
    /// e.g., User_to_json(Self) -> ToJson_User_to_json(Self)
    fn emit_simple_method_wrapper(
        &mut self,
        simple_name: &str,
        mangled_name: &str,
        arity: usize,
    ) -> CoreErlangResult<()> {
        self.newline();
        // Generate parameter names
        let params: Vec<String> = (0..arity).map(|i| format!("_@p{}", i)).collect();
        let params_str = params.join(", ");

        // Core Erlang function format: 'Name'/Arity = fun (Params) -> Body
        self.emit(&format!("'{}'/{} =", simple_name, arity));
        self.newline();
        self.indent += 1;
        self.emit(&format!("fun ({}) ->", params_str));
        self.newline();
        self.indent += 1;
        self.emit(&format!("apply '{}'/{}", mangled_name, arity));
        self.emit(&format!("({})", params_str));
        self.indent -= 1;
        self.indent -= 1;

        Ok(())
    }

    /// Emit the blanket Into implementation for a From impl.
    /// `impl From<T> for U` provides `impl Into<U> for T`
    fn emit_blanket_into_impl(&mut self, from_impl: &TraitImpl) -> CoreErlangResult<()> {
        // from_impl is: impl From<SourceType> for TargetType
        // We generate: impl Into<TargetType> for SourceType
        // The into method: fn into(self) -> TargetType { TargetType::from::<SourceType>(self) }

        let source_type = self.type_to_name(&from_impl.trait_type_args[0]);
        let target_type = &from_impl.type_name;

        // Mangled name: Into_TargetType_SourceType_into
        let mangled_name = format!("Into_{}_{}_into", target_type, source_type);

        // Emit the function directly in Core Erlang
        // 'Into_Wrapper_int_into'/1 = fun (Value) -> apply 'From_int_Wrapper_from'/1(Value)
        self.newline();
        self.emit(&format!("'{}'", mangled_name));
        self.emit("/1 = fun (Value) ->");
        self.newline();
        self.indent += 1;

        // Call the From implementation: From_SourceType_TargetType_from(Value)
        let from_mangled = format!("From_{}_{}_from", source_type, target_type);
        self.emit(&format!("apply '{}'", from_mangled));
        self.emit("/1(Value)");

        self.indent -= 1;

        Ok(())
    }

    /// Prefix for Dream stdlib modules in the BEAM.
    pub const STDLIB_PREFIX: &'static str = "dream::";

    /// Get the BEAM module name for a Dream module.
    /// Resolves stdlib module names to their full dream:: prefixed names.
    pub fn beam_module_name(name: &str) -> String {
        Self::resolve_module_name(name)
    }

    /// Convert a Rust string to Core Erlang list format (e.g., "foo" -> "[102, 111, 111]")
    fn string_to_core_list(s: &str) -> String {
        let chars: Vec<String> = s.chars().map(|c| (c as u32).to_string()).collect();
        format!("[{}]", chars.join(", "))
    }

    /// Emit a complete Core Erlang module.
    pub fn emit_module(&mut self, module: &Module) -> CoreErlangResult<String> {
        // All Dream modules are prefixed with dream:: (like Elixir uses Elixir.)
        // This ensures Dream modules are properly namespaced on the BEAM
        // Unless skip_stdlib_prefix is set (for REPL modules)
        self.module_name = if self.module_context.skip_stdlib_prefix {
            module.name.clone()
        } else if module.name.starts_with(Self::STDLIB_PREFIX) {
            module.name.clone()
        } else {
            format!("{}{}", Self::STDLIB_PREFIX, module.name)
        };

        // First pass: collect imports, traits, local functions, and register impl methods
        for item in &module.items {
            match item {
                Item::Struct(s) => {
                    // Register struct as a local type
                    self.local_types.insert(s.name.clone());
                }
                Item::Enum(e) => {
                    // Register enum as a local type
                    self.local_types.insert(e.name.clone());
                }
                Item::Use(use_decl) => {
                    self.collect_imports(use_decl);
                }
                Item::Function(func) => {
                    // Skip functions excluded by cfg
                    if !cfg::should_include(&func.attrs, &self.compile_options) {
                        continue;
                    }

                    // Register local function for BIF shadowing
                    self.local_functions
                        .insert((func.name.clone(), func.params.len()));

                    // Collect generic functions for monomorphization
                    if !func.type_params.is_empty() {
                        self.generic_functions
                            .insert(func.name.clone(), func.clone());
                    }
                }
                Item::Impl(impl_block) => {
                    // Register impl methods for Type::method() resolution
                    // Filter individual methods by cfg
                    // For regular impls, actual_type == func_prefix (both are the type name)
                    for method in &impl_block.methods {
                        if cfg::should_include(&method.attrs, &self.compile_options) {
                            self.impl_methods.insert((
                                impl_block.type_name.clone(), // actual_type
                                impl_block.type_name.clone(), // func_prefix
                                method.name.clone(),          // method_name
                            ));
                        }
                    }
                }
                Item::Trait(trait_def) => {
                    // Register trait definition
                    self.traits
                        .insert(trait_def.name.clone(), trait_def.clone());
                }
                Item::TraitImpl(trait_impl) => {
                    // Collect method names implemented in this impl (only those passing cfg)
                    let impl_method_names: HashSet<String> =
                        trait_impl.methods.iter()
                            .filter(|m| cfg::should_include(&m.attrs, &self.compile_options))
                            .map(|m| m.name.clone())
                            .collect();
                    let simple_trait =
                        Self::simple_trait_name(&trait_impl.trait_name).to_string();
                    // Format trait type args for registration: From<int> -> "_int"
                    let trait_type_args: Vec<Type> = trait_impl.trait_type_args.iter().map(|st| st.ty.clone()).collect();
                    let trait_type_args_suffix =
                        self.format_trait_type_args(&trait_type_args);

                    // Register trait impl methods for dispatch (filtered by cfg)
                    for method in &trait_impl.methods {
                        // Skip methods excluded by cfg
                        if !cfg::should_include(&method.attrs, &self.compile_options) {
                            continue;
                        }

                        // Use simple trait name with type args for func_prefix: Serialize_User
                        // actual_type is just the type name: User
                        self.impl_methods.insert((
                            trait_impl.type_name.clone(), // actual_type
                            format!(
                                "{}{}_{}",
                                simple_trait, trait_type_args_suffix, trait_impl.type_name
                            ), // func_prefix
                            method.name.clone(), // method_name
                        ));

                        // Track which types implement each trait method
                        // Include type args in key for parameterized traits
                        let key = (
                            format!("{}{}", simple_trait, trait_type_args_suffix),
                            method.name.clone(),
                        );
                        self.trait_impls
                            .entry(key)
                            .or_insert_with(Vec::new)
                            .push(trait_impl.type_name.clone());
                    }

                    // Also register default methods not overridden in this impl
                    if let Some(trait_def) = self.traits.get(&trait_impl.trait_name).cloned() {
                        for trait_method in &trait_def.methods {
                            if trait_method.body.is_some()
                                && !impl_method_names.contains(&trait_method.name)
                            {
                                // Register default method for dispatch
                                self.impl_methods.insert((
                                    trait_impl.type_name.clone(), // actual_type
                                    format!(
                                        "{}{}_{}",
                                        simple_trait, trait_type_args_suffix, trait_impl.type_name
                                    ), // func_prefix
                                    trait_method.name.clone(), // method_name
                                ));

                                let key = (
                                    format!("{}{}", simple_trait, trait_type_args_suffix),
                                    trait_method.name.clone(),
                                );
                                self.trait_impls
                                    .entry(key)
                                    .or_insert_with(Vec::new)
                                    .push(trait_impl.type_name.clone());
                            }
                        }
                    }

                    // Blanket impl: impl From<T> for U automatically provides impl Into<U> for T
                    if simple_trait == "From" && trait_impl.trait_type_args.len() == 1 {
                        let source_type = self.type_to_name(&trait_impl.trait_type_args[0]);
                        let target_type = &trait_impl.type_name;

                        // Register Into_TargetType_SourceType with method "into"
                        self.impl_methods.insert((
                            source_type.clone(), // actual_type
                            format!("Into_{}_{}", target_type, source_type), // func_prefix
                            "into".to_string(), // method_name
                        ));

                        // Track for trait dispatch
                        let key = (format!("Into_{}", target_type), "into".to_string());
                        self.trait_impls
                            .entry(key)
                            .or_insert_with(Vec::new)
                            .push(source_type.clone());
                    }
                }
                _ => {}
            }
        }

        // Module header (with Dream. prefix)
        self.emit(&format!("module '{}'", self.module_name));

        // Group functions by (name, arity) for multi-clause support (filtered by cfg)
        let mut func_groups: std::collections::HashMap<(String, usize), Vec<&Function>> =
            std::collections::HashMap::new();
        for item in &module.items {
            if let Item::Function(f) = item {
                // Skip functions excluded by cfg
                if !cfg::should_include(&f.attrs, &self.compile_options) {
                    continue;
                }
                func_groups
                    .entry((f.name.clone(), f.params.len()))
                    .or_default()
                    .push(f);
            }
        }

        // Collect exported functions (including impl block methods)
        // Note: Only one export per (name, arity) even with multiple clauses
        let mut exports: Vec<String> = Vec::new();
        let mut exported: std::collections::HashSet<(String, usize)> =
            std::collections::HashSet::new();

        for item in &module.items {
            match item {
                Item::Function(f) if f.is_pub => {
                    // Skip functions excluded by cfg
                    if !cfg::should_include(&f.attrs, &self.compile_options) {
                        continue;
                    }
                    // Note: Generic functions ARE exported - Erlang is dynamically typed
                    // so they work at runtime via type erasure
                    let key = (f.name.clone(), f.params.len());
                    if !exported.contains(&key) {
                        exports.push(format!("'{}'/{}", f.name, f.params.len()));
                        exported.insert(key);
                    }
                }
                Item::Impl(impl_block) => {
                    // Check if this impl is for a local type (struct/enum) or a trait
                    // impl Point { } -> local type, mangle names
                    // impl Application { } -> trait, inject methods directly
                    let is_local_type = self.local_types.contains(&impl_block.type_name);

                    for method in &impl_block.methods {
                        // Skip methods excluded by cfg
                        if !cfg::should_include(&method.attrs, &self.compile_options) {
                            continue;
                        }
                        if method.is_pub {
                            let export_name = if is_local_type {
                                // Local type impl: mangle name (e.g., Point_new)
                                format!("{}_{}", impl_block.type_name, method.name)
                            } else {
                                // Trait impl: use original name (e.g., start, stop)
                                method.name.clone()
                            };
                            exports.push(format!("'{}'/{}", export_name, method.params.len()));
                        }
                    }
                }
                Item::TraitImpl(trait_impl) => {
                    // Collect method names explicitly implemented (filtered by cfg)
                    let impl_method_names: HashSet<String> =
                        trait_impl.methods.iter()
                            .filter(|m| cfg::should_include(&m.attrs, &self.compile_options))
                            .map(|m| m.name.clone())
                            .collect();
                    let simple_trait =
                        Self::simple_trait_name(&trait_impl.trait_name);
                    // Format trait type args for mangling: From<int> -> "_int"
                    let trait_type_args: Vec<Type> = trait_impl.trait_type_args.iter().map(|st| st.ty.clone()).collect();
                    let trait_type_args_suffix =
                        self.format_trait_type_args(&trait_type_args);

                    // Export explicitly implemented methods (filtered by cfg)
                    // Trait impl methods are always exported (visibility comes from trait/type)
                    for method in &trait_impl.methods {
                        // Skip methods excluded by cfg
                        if !cfg::should_include(&method.attrs, &self.compile_options) {
                            continue;
                        }
                        let mangled_name = format!(
                            "{}{}_{}_{}",
                            simple_trait, trait_type_args_suffix, trait_impl.type_name, method.name
                        );
                        exports.push(format!("'{}'/{}", mangled_name, method.params.len()));
                        // Also export simple-name wrapper for dynamic dispatch
                        let simple_name = format!("{}_{}", trait_impl.type_name, method.name);
                        exports.push(format!("'{}'/{}", simple_name, method.params.len()));
                    }

                    // Export default methods not overridden
                    if let Some(trait_def) = self.traits.get(&trait_impl.trait_name).cloned() {
                        for trait_method in &trait_def.methods {
                            if trait_method.body.is_some()
                                && !impl_method_names.contains(&trait_method.name)
                            {
                                let mangled_name = format!(
                                    "{}{}_{}_{}",
                                    simple_trait,
                                    trait_type_args_suffix,
                                    trait_impl.type_name,
                                    trait_method.name
                                );
                                exports.push(format!(
                                    "'{}'/{}", mangled_name, trait_method.params.len()
                                ));
                                // Also export simple-name wrapper for dynamic dispatch
                                let simple_name = format!("{}_{}", trait_impl.type_name, trait_method.name);
                                exports.push(format!(
                                    "'{}'/{}", simple_name, trait_method.params.len()
                                ));
                            }
                        }
                    }

                    // Export blanket Into impl for From traits
                    if simple_trait == "From" && trait_impl.trait_type_args.len() == 1 {
                        let source_type = self.type_to_name(&trait_impl.trait_type_args[0]);
                        let target_type = &trait_impl.type_name;
                        let into_mangled = format!("Into_{}_{}_into", target_type, source_type);
                        exports.push(format!("'{}'/{}", into_mangled, 1));
                    }
                }
                _ => {}
            }
        }

        self.emit(" [");
        self.emit(&exports.join(", "));
        self.emit("]");

        self.newline();
        self.emit("    attributes []");
        self.newline();

        // Emit grouped functions (supports multi-clause functions)
        let mut emitted_funcs: std::collections::HashSet<(String, usize)> =
            std::collections::HashSet::new();
        for item in &module.items {
            if let Item::Function(f) = item {
                // Note: Generic functions ARE emitted - Erlang is dynamically typed
                // so they work at runtime via type erasure
                let key = (f.name.clone(), f.params.len());
                if !emitted_funcs.contains(&key) {
                    emitted_funcs.insert(key.clone());
                    if let Some(clauses) = func_groups.get(&key) {
                        self.newline();
                        self.emit_function_clauses(&f.name, f.params.len(), clauses)?;
                    }
                }
            }
        }

        // Emit impl block methods (filtered by cfg)
        for item in &module.items {
            if let Item::Impl(impl_block) = item {
                // Check if this impl is for a local type (struct/enum) or a trait
                let is_local_type = self.local_types.contains(&impl_block.type_name);

                for method in &impl_block.methods {
                    // Skip methods excluded by cfg
                    if !cfg::should_include(&method.attrs, &self.compile_options) {
                        continue;
                    }
                    let func_name = if is_local_type {
                        // Local type impl: mangle name (e.g., Point_new)
                        format!("{}_{}", impl_block.type_name, method.name)
                    } else {
                        // Trait impl: use original name (e.g., start, stop)
                        method.name.clone()
                    };
                    let emitted_method = Function {
                        attrs: vec![],
                        name: func_name,
                        type_params: method.type_params.clone(),
                        params: method.params.clone(),
                        guard: method.guard.clone(),
                        return_type: method.return_type.clone(),
                        body: method.body.clone(),
                        is_pub: method.is_pub,
                        span: method.span.clone(),
                    };
                    self.newline();
                    self.emit_function(&emitted_method)?;
                }
            }
        }

        // Emit trait impl methods (including default methods)
        for item in &module.items {
            if let Item::TraitImpl(trait_impl) = item {
                self.emit_trait_impl_methods(trait_impl)?;
            }
        }

        // Emit monomorphized functions (local)
        self.emit_monomorphized_functions()?;

        // Emit cross-module monomorphized functions
        self.emit_cross_module_monomorphized_functions()?;

        self.newline();
        self.emit("end");
        self.newline();

        Ok(self.output.clone())
    }

    /// Emit monomorphized versions of generic functions.
    /// For each (func_name, [Type1, Type2, ...]) in pending_monomorphizations,
    /// generate a specialized function func_name_Type1_Type2.
    fn emit_monomorphized_functions(&mut self) -> CoreErlangResult<()> {
        // Take the pending monomorphizations to avoid borrow issues
        let monos: Vec<(String, Vec<String>)> =
            self.pending_monomorphizations.drain().collect();

        for (func_name, type_names) in monos {
            if let Some(generic_func) = self.generic_functions.get(&func_name).cloned() {
                // Set up type parameter substitution
                self.type_param_subst.clear();
                self.type_param_bounds.clear();

                for (type_param, type_name) in
                    generic_func.type_params.iter().zip(type_names.iter())
                {
                    self.type_param_subst
                        .insert(type_param.name.clone(), type_name.clone());
                    self.type_param_bounds
                        .insert(type_param.name.clone(), type_param.bounds.clone());
                }

                // Create the monomorphized function name
                let mono_name = format!("{}_{}", func_name, type_names.join("_"));

                // Create a modified function with the monomorphized name
                let mono_func = Function {
                    attrs: vec![],
                    name: mono_name,
                    type_params: vec![], // No type params in monomorphized version
                    params: generic_func.params.clone(),
                    guard: generic_func.guard.clone(),
                    return_type: generic_func.return_type.clone(),
                    body: generic_func.body.clone(),
                    is_pub: generic_func.is_pub,
                    span: generic_func.span.clone(),
                };

                self.newline();
                self.emit_function(&mono_func)?;

                // Clear substitutions after emitting
                self.type_param_subst.clear();
                self.type_param_bounds.clear();
            }
        }

        Ok(())
    }

    /// Emit monomorphized versions of cross-module generic functions.
    /// These are generic functions from other modules called with concrete types.
    /// Runs in a loop because inlined functions may call other generics from the same source module.
    fn emit_cross_module_monomorphized_functions(&mut self) -> CoreErlangResult<()> {
        // Get the external registry
        let registry = match &self.external_generics {
            Some(reg) => reg.clone(),
            None => return Ok(()), // No registry, nothing to do
        };

        // Track which monomorphizations we've already emitted to avoid duplicates
        let mut emitted: HashSet<(String, String, Vec<String>)> = HashSet::new();

        // Keep running until no new monomorphizations are added
        loop {
            // Take the cross-module monomorphizations to avoid borrow issues
            let monos: Vec<(String, String, Vec<String>)> =
                self.cross_module_monomorphizations.drain().collect();

            if monos.is_empty() {
                break;
            }

            let guard = registry.read().unwrap();

            for (source_module, func_name, type_names) in monos {
                // Skip if already emitted
                let key = (source_module.clone(), func_name.clone(), type_names.clone());
                if emitted.contains(&key) {
                    continue;
                }
                emitted.insert(key);

                if let Some(generic_func) = guard.get(&source_module, &func_name) {
                    // Set up type parameter substitution
                    self.type_param_subst.clear();
                    self.type_param_bounds.clear();

                    for (type_param, type_name) in
                        generic_func.type_params.iter().zip(type_names.iter())
                    {
                        self.type_param_subst
                            .insert(type_param.name.clone(), type_name.clone());
                        self.type_param_bounds
                            .insert(type_param.name.clone(), type_param.bounds.clone());
                    }

                    // Create the monomorphized function name: module_func_Type
                    let mono_name = format!(
                        "{}_{}_{}",
                        source_module,
                        func_name,
                        type_names.join("_")
                    );

                    // Create a modified function with the monomorphized name
                    let mono_func = Function {
                        attrs: vec![],
                        name: mono_name,
                        type_params: vec![], // No type params in monomorphized version
                        params: generic_func.params.clone(),
                        guard: generic_func.guard.clone(),
                        return_type: generic_func.return_type.clone(),
                        body: generic_func.body.clone(),
                        is_pub: false, // Cross-module mono is local only
                        span: generic_func.span.clone(),
                    };

                    // Set the source module so calls get rewritten correctly
                    self.cross_module_inlining_source = Some(source_module.clone());

                    self.newline();
                    self.emit_function(&mono_func)?;

                    // Clear source module and substitutions after emitting
                    self.cross_module_inlining_source = None;
                    self.type_param_subst.clear();
                    self.type_param_bounds.clear();
                }
            }
            // Drop guard before next iteration
            drop(guard);
        }

        Ok(())
    }

    /// Emit a function definition.
    fn emit_function(&mut self, func: &Function) -> CoreErlangResult<()> {
        let arity = func.params.len();
        self.emit(&format!("'{}'/{} =", func.name, arity));
        self.newline();
        self.indent += 1;

        // Check if this function has a `self` parameter
        self.has_self_param = func.params.iter().any(|p| {
            matches!(&p.pattern, Pattern::Ident(name) if name == "self")
        });

        // Track type parameters for this function (for generic functions without monomorphization)
        // This enables us to detect when a call like func::<T>() is using the current function's
        // type parameter, so we use dynamic dispatch instead of creating bogus monomorphizations
        self.current_func_type_params.clear();
        for tp in &func.type_params {
            self.current_func_type_params.insert(tp.name.clone());
        }

        // Clear variables from previous function and add this function's parameters
        self.variables.clear();
        for p in &func.params {
            if let Pattern::Ident(name) = &p.pattern {
                self.variables.insert(name.clone());
            }
        }

        self.emit("fun (");

        // Emit parameters
        let param_names: Vec<String> = func
            .params
            .iter()
            .enumerate()
            .map(|(i, p)| {
                // Use pattern if it's an identifier, otherwise generate a name
                match &p.pattern {
                    Pattern::Ident(name) => Self::var_name(name),
                    _ => format!("_@p{}", i),
                }
            })
            .collect();
        self.emit(&param_names.join(", "));
        self.emit(") ->");
        self.newline();

        self.indent += 1;

        // Check if we need try/catch wrapping for ? operator error propagation
        let needs_result_catch = Self::is_result_type(&func.return_type);
        let needs_option_catch = Self::is_option_type(&func.return_type);

        if needs_result_catch || needs_option_catch {
            self.emit("try");
            self.newline();
            self.indent += 1;
        }

        self.emit_block(&func.body)?;

        if needs_result_catch {
            // After emit_block, indent is at try body level (indent 3)
            // We need to emit 'of' at same visual level as 'try' (4 spaces = indent 1)
            // But 'try' was emitted when indent was 1, then we incremented to 2, then to 3 for body
            // So we need to go back 2 levels to get 'of' at the right visual position
            self.indent -= 2;
            self.newline();
            let result_var = self.fresh_var();
            self.emit(&format!("of <{}> ->", result_var));
            self.indent += 1;
            self.newline();
            self.emit(&result_var);
            self.indent -= 1;
            self.newline();
            let class_var = self.fresh_var();
            let reason_var = self.fresh_var();
            let stack_var = self.fresh_var();
            let err_var = self.fresh_var();
            // Core Erlang catch pattern must be on same line as 'catch'
            self.emit(&format!(
                "catch <{}, {}, {}> ->",
                class_var, reason_var, stack_var
            ));
            self.indent += 1;
            self.newline();
            // Match the reason to extract error or re-raise
            self.emit(&format!("case {} of", reason_var));
            self.indent += 1;
            self.newline();
            self.emit(&format!("<{{'error', {}}}> when 'true' ->", err_var));
            self.indent += 1;
            self.newline();
            self.emit(&format!("{{'error', {}}}", err_var));
            self.indent -= 1;
            self.newline();
            // Re-raise other exceptions
            let other_var = self.fresh_var();
            self.emit(&format!("<{}> when 'true' ->", other_var));
            self.indent += 1;
            self.newline();
            self.emit(&format!(
                "call 'erlang':'raise'({}, {}, {})",
                class_var, other_var, stack_var
            ));
            self.indent -= 2; // Back from case body to case keyword level
            self.newline();
            self.emit("end"); // Close case
            // Note: Core Erlang try expressions do NOT have a closing 'end' keyword
            // The try ends implicitly after the catch body
        } else if needs_option_catch {
            // After emit_block, indent is at try body level (indent 3)
            // We need to emit 'of' at same visual level as 'try' (4 spaces = indent 1)
            self.indent -= 2;
            self.newline();
            let result_var = self.fresh_var();
            self.emit(&format!("of <{}> ->", result_var));
            self.indent += 1;
            self.newline();
            self.emit(&result_var);
            self.indent -= 1;
            self.newline();
            let class_var = self.fresh_var();
            let reason_var = self.fresh_var();
            let stack_var = self.fresh_var();
            // Core Erlang catch pattern must be on same line as 'catch'
            self.emit(&format!(
                "catch <{}, {}, {}> ->",
                class_var, reason_var, stack_var
            ));
            self.indent += 1;
            self.newline();
            // Match the reason to check for 'none' or re-raise
            self.emit(&format!("case {} of", reason_var));
            self.indent += 1;
            self.newline();
            self.emit("<'none'> when 'true' ->");
            self.indent += 1;
            self.newline();
            self.emit("'none'");
            self.indent -= 1;
            self.newline();
            // Re-raise other exceptions
            let other_var = self.fresh_var();
            self.emit(&format!("<{}> when 'true' ->", other_var));
            self.indent += 1;
            self.newline();
            self.emit(&format!(
                "call 'erlang':'raise'({}, {}, {})",
                class_var, other_var, stack_var
            ));
            self.indent -= 2; // Back from case body to case keyword level
            self.newline();
            self.emit("end"); // Close case
            // Note: Core Erlang try expressions do NOT have a closing 'end' keyword
        }

        self.indent -= 1;

        self.indent -= 1;
        // Reset after function
        self.has_self_param = false;
        Ok(())
    }

    /// Emit a function with multiple clauses (for pattern matching on parameters).
    /// Core Erlang doesn't support multi-clause fun directly, so we generate:
    /// ```text
    /// 'factorial'/1 =
    ///     fun (_@p0) ->
    ///         case _@p0 of
    ///             <0> when 'true' -> 1
    ///             <N> when 'true' -> call 'erlang':'*'(N, ...)
    ///         end
    /// ```
    fn emit_function_clauses(
        &mut self,
        name: &str,
        arity: usize,
        clauses: &[&Function],
    ) -> CoreErlangResult<()> {
        // For single-clause functions without guards, use simpler direct emission
        if clauses.len() == 1 && clauses[0].guard.is_none() {
            return self.emit_function(clauses[0]);
        }

        self.emit(&format!("'{}'/{} =", name, arity));
        self.newline();
        self.indent += 1;

        // Generate parameter names
        let param_names: Vec<String> = (0..arity).map(|i| format!("_@p{}", i)).collect();

        self.emit("fun (");
        self.emit(&param_names.join(", "));
        self.emit(") ->");
        self.newline();

        self.indent += 1;

        // Check if we need try/catch wrapping for ? operator error propagation
        // Use the first clause's return type (all clauses have the same return type)
        let needs_result_catch = Self::is_result_type(&clauses[0].return_type);
        let needs_option_catch = Self::is_option_type(&clauses[0].return_type);

        if needs_result_catch || needs_option_catch {
            self.emit("try");
            self.newline();
            self.indent += 1;
        }

        // For single param, case on that param directly
        // For multiple params, case on a tuple of params
        if arity == 1 {
            self.emit(&format!("case {} of", param_names[0]));
        } else {
            self.emit("case {");
            self.emit(&param_names.join(", "));
            self.emit("} of");
        }

        self.newline();
        self.indent += 1;

        // Emit each clause as a case arm
        for clause in clauses.iter() {
            // Collect pattern variables for this clause
            self.variables.clear();
            for p in &clause.params {
                self.collect_pattern_vars(&p.pattern);
            }

            // Check for self parameter
            self.has_self_param = clause.params.iter().any(|p| {
                matches!(&p.pattern, Pattern::Ident(n) if n == "self")
            });

            // Emit pattern
            self.emit("<");
            if arity == 1 {
                self.emit_pattern(&clause.params[0].pattern)?;
            } else {
                self.emit("{");
                for (j, param) in clause.params.iter().enumerate() {
                    if j > 0 {
                        self.emit(", ");
                    }
                    self.emit_pattern(&param.pattern)?;
                }
                self.emit("}");
            }
            // Emit guard clause
            self.emit("> when ");
            if let Some(guard) = &clause.guard {
                self.emit_expr(guard)?;
            } else {
                self.emit("'true'");
            }
            self.emit(" ->");
            self.newline();

            self.indent += 1;
            self.emit_block(&clause.body)?;
            self.indent -= 1;
            self.newline();
        }

        self.indent -= 1;
        self.emit("end");

        // Close try/catch if needed
        if needs_result_catch {
            // After case end, indent is at try body level (indent 3)
            // We need to emit 'of' at same visual level as 'try' (4 spaces = indent 1)
            self.indent -= 2;
            self.newline();
            let result_var = self.fresh_var();
            self.emit(&format!("of <{}> ->", result_var));
            self.indent += 1;
            self.newline();
            self.emit(&result_var);
            self.indent -= 1;
            self.newline();
            let class_var = self.fresh_var();
            let reason_var = self.fresh_var();
            let stack_var = self.fresh_var();
            let err_var = self.fresh_var();
            // Core Erlang catch pattern must be on same line as 'catch'
            self.emit(&format!(
                "catch <{}, {}, {}> ->",
                class_var, reason_var, stack_var
            ));
            self.indent += 1;
            self.newline();
            // Match the reason to extract error or re-raise
            self.emit(&format!("case {} of", reason_var));
            self.indent += 1;
            self.newline();
            self.emit(&format!("<{{'error', {}}}> when 'true' ->", err_var));
            self.indent += 1;
            self.newline();
            self.emit(&format!("{{'error', {}}}", err_var));
            self.indent -= 1;
            self.newline();
            // Re-raise other exceptions
            let other_var = self.fresh_var();
            self.emit(&format!("<{}> when 'true' ->", other_var));
            self.indent += 1;
            self.newline();
            self.emit(&format!(
                "call 'erlang':'raise'({}, {}, {})",
                class_var, other_var, stack_var
            ));
            self.indent -= 2; // Back from case body to case keyword level
            self.newline();
            self.emit("end"); // Close case
            // Note: Core Erlang try expressions do NOT have a closing 'end' keyword
        } else if needs_option_catch {
            // After case end, indent is at try body level (indent 3)
            // We need to emit 'of' at same visual level as 'try' (4 spaces = indent 1)
            self.indent -= 2;
            self.newline();
            let result_var = self.fresh_var();
            self.emit(&format!("of <{}> ->", result_var));
            self.indent += 1;
            self.newline();
            self.emit(&result_var);
            self.indent -= 1;
            self.newline();
            let class_var = self.fresh_var();
            let reason_var = self.fresh_var();
            let stack_var = self.fresh_var();
            // Core Erlang catch pattern must be on same line as 'catch'
            self.emit(&format!(
                "catch <{}, {}, {}> ->",
                class_var, reason_var, stack_var
            ));
            self.indent += 1;
            self.newline();
            // Match the reason to check for 'none' or re-raise
            self.emit(&format!("case {} of", reason_var));
            self.indent += 1;
            self.newline();
            self.emit("<'none'> when 'true' ->");
            self.indent += 1;
            self.newline();
            self.emit("'none'");
            self.indent -= 1;
            self.newline();
            // Re-raise other exceptions
            let other_var = self.fresh_var();
            self.emit(&format!("<{}> when 'true' ->", other_var));
            self.indent += 1;
            self.newline();
            self.emit(&format!(
                "call 'erlang':'raise'({}, {}, {})",
                class_var, other_var, stack_var
            ));
            self.indent -= 2; // Back from case body to case keyword level
            self.newline();
            self.emit("end"); // Close case
            // Note: Core Erlang try expressions do NOT have a closing 'end' keyword
        }

        self.indent -= 1;

        self.indent -= 1;
        self.has_self_param = false;
        Ok(())
    }

    /// Emit a block (statements + optional expression).
    fn emit_block(&mut self, block: &Block) -> CoreErlangResult<()> {
        // In Core Erlang, we need to convert statements to nested let expressions
        self.emit_block_inner(&block.stmts, &block.expr)
    }

    /// Recursively emit statements as nested let expressions.
    /// Handles early returns by transforming them to nested conditionals.
    fn emit_block_inner(
        &mut self,
        stmts: &[Stmt],
        final_expr: &Option<Box<SpannedExpr>>,
    ) -> CoreErlangResult<()> {
        if stmts.is_empty() {
            // No more statements, emit the final expression
            if let Some(expr) = final_expr {
                self.emit_expr(expr)?;
            } else {
                // Empty block returns 'ok'
                self.emit("'ok'");
            }
            return Ok(());
        }

        let (first, rest) = stmts.split_first().unwrap();

        match first {
            Stmt::Let { pattern, value, else_block, .. } => {
                // Add bound variables to scope
                self.collect_pattern_vars(pattern);

                // Track variable type for record field access
                if let (Pattern::Ident(var_name), Expr::StructInit { name: struct_name, .. }) = (pattern, value.inner()) {
                    self.variable_types.insert(var_name.clone(), struct_name.clone());
                }

                // If there's an else block, always use case with two arms
                if let Some(else_blk) = else_block {
                    self.emit("case ");
                    self.emit_expr(value)?;
                    self.emit(" of");
                    self.newline();
                    self.indent += 1;
                    // Pattern arm - continues with rest of block
                    self.emit("<");
                    self.emit_pattern(pattern)?;
                    self.emit("> when 'true' ->");
                    self.newline();
                    self.indent += 1;
                    self.emit_block_inner(rest, final_expr)?;
                    self.indent -= 1;
                    self.newline();
                    // Wildcard arm - executes else block (must diverge)
                    self.emit("<_> when 'true' ->");
                    self.newline();
                    self.indent += 1;
                    self.emit_block(else_blk)?;
                    self.indent -= 1;
                    self.newline();
                    self.indent -= 1;
                    self.emit("end");
                } else if matches!(pattern, Pattern::Ident(_) | Pattern::Wildcard) {
                    // Simple identifier patterns use let
                    self.emit("let <");
                    self.emit_pattern(pattern)?;
                    self.emit("> =");
                    self.newline();
                    self.indent += 1;
                    self.emit_expr(value)?;
                    self.indent -= 1;
                    self.newline();
                    self.emit("in ");
                    self.emit_block_inner(rest, final_expr)?;
                } else {
                    // Use case for complex patterns (tuple, list, struct, enum)
                    self.emit("case ");
                    self.emit_expr(value)?;
                    self.emit(" of");
                    self.newline();
                    self.indent += 1;
                    self.emit("<");
                    self.emit_pattern(pattern)?;
                    self.emit("> when 'true' ->");
                    self.newline();
                    self.indent += 1;
                    self.emit_block_inner(rest, final_expr)?;
                    self.indent -= 1;
                    self.newline();
                    self.indent -= 1;
                    self.emit("end");
                }
            }
            Stmt::Expr { expr, .. } => {
                // Check for early return patterns
                if let Expr::Return(ret_val) = expr.inner() {
                    // Direct return - emit value and stop
                    if let Some(val) = ret_val {
                        self.emit_expr(val)?;
                    } else {
                        self.emit("'ok'");
                    }
                    // Ignore rest of block after return
                    return Ok(());
                }

                // Check for if with early return
                if let Expr::If { cond, then_block, else_block } = expr.inner() {
                    let then_returns = Self::block_contains_return(then_block);
                    let else_returns = else_block.as_ref().is_some_and(|b| Self::block_contains_return(b));

                    if then_returns || else_returns {
                        // Transform: if with return becomes case with continuation
                        self.emit_if_with_early_return(
                            cond,
                            then_block,
                            else_block.as_ref(),
                            rest,
                            final_expr,
                        )?;
                        return Ok(());
                    }
                }

                if rest.is_empty() && final_expr.is_none() {
                    // Last expression statement is the result
                    self.emit_expr(expr)?;
                } else {
                    // Expression statement - bind to _ and continue
                    self.emit("let <_> =");
                    self.newline();
                    self.indent += 1;
                    self.emit_expr(expr)?;
                    self.indent -= 1;
                    self.newline();
                    self.emit("in ");
                    self.emit_block_inner(rest, final_expr)?;
                }
            }
        }

        Ok(())
    }

    /// Emit an if expression that contains early returns.
    /// Transforms `if cond { return x; } rest` into `case cond of true -> x; false -> rest end`
    fn emit_if_with_early_return(
        &mut self,
        cond: &SpannedExpr,
        then_block: &Block,
        else_block: Option<&Block>,
        rest_stmts: &[Stmt],
        final_expr: &Option<Box<SpannedExpr>>,
    ) -> CoreErlangResult<()> {
        self.emit("case ");
        self.emit_expr(cond)?;
        self.emit(" of");
        self.indent += 1;
        self.newline();

        // True branch
        self.emit("<'true'> when 'true' ->");
        self.indent += 1;
        self.newline();
        self.emit_block_with_continuation(then_block, rest_stmts, final_expr)?;
        self.indent -= 1;
        self.newline();

        // False branch
        self.emit("<'false'> when 'true' ->");
        self.indent += 1;
        self.newline();
        if let Some(else_blk) = else_block {
            self.emit_block_with_continuation(else_blk, rest_stmts, final_expr)?;
        } else {
            // No else block - continue with rest of the code
            self.emit_block_inner(rest_stmts, final_expr)?;
        }
        self.indent -= 1;

        self.indent -= 1;
        self.newline();
        self.emit("end");

        Ok(())
    }

    /// Emit a block, but if it doesn't contain a return, append the continuation.
    fn emit_block_with_continuation(
        &mut self,
        block: &Block,
        rest_stmts: &[Stmt],
        final_expr: &Option<Box<SpannedExpr>>,
    ) -> CoreErlangResult<()> {
        // Combine block statements with continuation.
        // emit_block_inner will handle early returns properly - paths that return
        // will stop, paths that don't will continue with the rest.
        let mut all_stmts = block.stmts.clone();

        // If block.expr contains a return, treat it as a statement so emit_block_inner
        // can handle it with the continuation. Otherwise it's the block's final value.
        let combined_final = if let Some(expr) = &block.expr {
            if Self::contains_return(expr.inner()) {
                // Convert the expr to a statement so it gets processed with early return logic
                all_stmts.push(Stmt::Expr { expr: (**expr).clone(), span: None });
                final_expr
            } else {
                // No return in expr - it's the block's normal result
                &block.expr
            }
        } else {
            final_expr
        };

        all_stmts.extend(rest_stmts.iter().cloned());

        self.emit_block_inner(&all_stmts, combined_final)?;
        Ok(())
    }

    /// Emit a default enum pattern (non-Result types).
    fn emit_enum_pattern_default(&mut self, variant: &str, fields: &EnumPatternFields) -> CoreErlangResult<()> {
        match fields {
            EnumPatternFields::Unit => {
                // Unit variant: just an atom
                self.emit(&format!("'{}'", variant.to_lowercase()));
            }
            EnumPatternFields::Tuple(patterns) => {
                // Tuple variant: {'variant', pat1, pat2, ...}
                self.emit("{");
                self.emit(&format!("'{}'", variant.to_lowercase()));
                for pat in patterns {
                    self.emit(", ");
                    self.emit_pattern(pat)?;
                }
                self.emit("}");
            }
            EnumPatternFields::Struct(field_patterns) => {
                // Struct variant: {'variant', ~{field1 => pat1, field2 => pat2, ...}~}
                self.emit("{");
                self.emit(&format!("'{}'", variant.to_lowercase()));
                self.emit(", ~{");
                let mut first = true;
                for (field_name, field_pat) in field_patterns {
                    if !first {
                        self.emit(", ");
                    }
                    first = false;
                    self.emit(&format!("'{}' := ", field_name));
                    self.emit_pattern(field_pat)?;
                }
                self.emit("}~}");
            }
        }
        Ok(())
    }

    /// Emit a default enum variant (non-Result types).
    fn emit_enum_variant_default(&mut self, variant: &str, args: &EnumVariantArgs) -> CoreErlangResult<()> {
        match args {
            EnumVariantArgs::Unit => {
                // Unit variant: just an atom
                self.emit(&format!("'{}'", variant.to_lowercase()));
            }
            EnumVariantArgs::Tuple(exprs) => {
                // Tuple variant: {'variant', arg1, arg2, ...}
                self.emit("{");
                self.emit(&format!("'{}'", variant.to_lowercase()));
                for arg in exprs {
                    self.emit(", ");
                    self.emit_expr(arg)?;
                }
                self.emit("}");
            }
            EnumVariantArgs::Struct(fields) => {
                // Struct variant: {'variant', #{field1 => val1, field2 => val2, ...}}
                self.emit("{");
                self.emit(&format!("'{}'", variant.to_lowercase()));
                self.emit(", ~{");
                let mut first = true;
                for (field_name, field_expr) in fields {
                    if !first {
                        self.emit(", ");
                    }
                    first = false;
                    self.emit(&format!("'{}' => ", field_name));
                    self.emit_expr(field_expr)?;
                }
                self.emit("}~}");
            }
        }
        Ok(())
    }

    /// Emit a map key expression.
    /// For atoms, emit directly without module resolution.
    fn emit_map_key(&mut self, expr: &SpannedExpr) -> CoreErlangResult<()> {
        self.emit_data_expr(expr)
    }

    /// Emit an expression as data (inside tuples, lists, maps).
    /// For atoms, emit directly without module resolution since atoms in data
    /// contexts are literal values, not module references.
    fn emit_data_expr(&mut self, expr: &SpannedExpr) -> CoreErlangResult<()> {
        match expr.inner() {
            Expr::Atom(a) => {
                // Emit atom directly - don't resolve as module reference
                self.emit(&format!("'{}'", a));
                Ok(())
            }
            // For tuples and lists, recursively emit elements as data
            Expr::Tuple(elements) => {
                self.emit("{");
                for (i, e) in elements.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit_data_expr(e)?;
                }
                self.emit("}");
                Ok(())
            }
            Expr::List(elements) => {
                if elements.is_empty() {
                    self.emit("[]");
                } else {
                    self.emit("[");
                    for (i, e) in elements.iter().enumerate() {
                        if i > 0 {
                            self.emit(", ");
                        }
                        self.emit_data_expr(e)?;
                    }
                    self.emit("]");
                }
                Ok(())
            }
            // For all other expressions, use normal emission
            _ => self.emit_expr(expr),
        }
    }

    /// Emit an expression.
    fn emit_expr(&mut self, expr: &SpannedExpr) -> CoreErlangResult<()> {
        match expr.inner() {
            Expr::Int(n) => {
                self.emit(&n.to_string());
            }

            Expr::String(s) => {
                // Binary string (double quotes) - emit as Erlang binary
                // Use list_to_binary to convert char codes to binary
                self.emit("call 'erlang':'list_to_binary'([");
                let chars: Vec<String> = s.chars().map(|c| (c as u32).to_string()).collect();
                self.emit(&chars.join(", "));
                self.emit("])");
            }

            Expr::Charlist(s) => {
                // Charlist (single quotes) - emit as list of integers
                self.emit("[");
                let chars: Vec<String> = s.chars().map(|c| (c as u32).to_string()).collect();
                self.emit(&chars.join(", "));
                self.emit("]");
            }

            Expr::StringInterpolation(parts) => {
                // Convert interpolated string to iolist and then binary
                // Result: call 'erlang':'iolist_to_binary'([part1, part2, ...])
                self.emit("call 'erlang':'iolist_to_binary'([");
                for (i, part) in parts.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    match part {
                        StringPart::Literal(s) => {
                            // Emit literal as char code list
                            self.emit("[");
                            let chars: Vec<String> =
                                s.chars().map(|c| (c as u32).to_string()).collect();
                            self.emit(&chars.join(", "));
                            self.emit("]");
                        }
                        StringPart::Expr(e) => {
                            // Convert expression to string using display::to_string
                            // (doesn't add quotes around strings)
                            self.emit("call 'dream::display':'to_string'(");
                            self.emit_expr(e)?;
                            self.emit(")");
                        }
                    }
                }
                self.emit("])");
            }

            Expr::Atom(a) => {
                // Atoms are literal values, not module references.
                // Always emit them directly without module resolution.
                self.emit(&format!("'{}'", a));
            }

            Expr::Bool(b) => {
                self.emit(if *b { "'true'" } else { "'false'" });
            }

            Expr::Ident(name) => {
                // Handle `self` - if it's a function parameter, use as variable;
                // otherwise it's the erlang:self() BIF
                if name == "self" && !self.has_self_param {
                    self.emit("call 'erlang':'self'()");
                } else {
                    self.emit(&Self::var_name(name));
                }
            }

            Expr::Unit => {
                // Unit is represented as empty tuple or 'ok'
                self.emit("'ok'");
            }

            Expr::Binary { op, left, right } => {
                self.emit_binary_op(*op, left, right)?;
            }

            Expr::Unary { op, expr } => {
                match op {
                    UnaryOp::Neg => {
                        self.emit("call 'erlang':'-'(0, ");
                        self.emit_expr(expr)?;
                        self.emit(")");
                    }
                    UnaryOp::Not => {
                        self.emit("call 'erlang':'not'(");
                        self.emit_expr(expr)?;
                        self.emit(")");
                    }
                }
            }

            Expr::Call {
                func,
                type_args,
                inferred_type_args,
                args,
            } => {
                // Use explicit type_args if provided, otherwise use inferred
                let effective_type_args = if !type_args.is_empty() {
                    type_args
                } else {
                    inferred_type_args
                };

                // Check if it's a local function call or external
                match func.inner() {
                    Expr::Ident(name) => {
                        // Check if this is a call to a generic function with type args
                        if !effective_type_args.is_empty()
                            && self.generic_functions.contains_key(name)
                        {
                            // Check if any type args are the current function's type parameters
                            // If so, we're in a generic function calling another generic with our type param
                            // We should call the generic function directly, not create a bogus monomorphization
                            let uses_current_type_params = effective_type_args.iter().any(|t| {
                                match t {
                                    Type::Named { name: n, .. } | Type::TypeVar(n) => {
                                        self.current_func_type_params.contains(n)
                                    }
                                    _ => false,
                                }
                            });

                            if uses_current_type_params {
                                // Call the generic function directly (not monomorphized)
                                // This handles recursive calls like server_loop_typed::<T>(state)
                                // within the generic function server_loop_typed<T>
                                self.emit(&format!("apply '{}'/{}", name, args.len()));
                                self.emit("(");
                                self.emit_args(args)?;
                                self.emit(")");
                            } else {
                                // Record the monomorphization and call the specialized version
                                let type_names: Vec<String> = effective_type_args
                                    .iter()
                                    .map(|t| self.type_to_name(t))
                                    .collect();
                                self.pending_monomorphizations
                                    .insert((name.clone(), type_names.clone()));

                                // Call the monomorphized function: name_Type1_Type2
                                let mono_name = format!("{}_{}", name, type_names.join("_"));
                                self.emit(&format!("apply '{}'/{}", mono_name, args.len()));
                                self.emit("(");
                                self.emit_args(args)?;
                                self.emit(")");
                            }
                        } else if !effective_type_args.is_empty() {
                            // Check if we're inlining from another module and this is
                            // a call to another generic from that same source module
                            let type_names: Vec<String> = effective_type_args
                                .iter()
                                .map(|t| self.type_to_name(t))
                                .collect();

                            let is_source_module_generic = self.cross_module_inlining_source
                                .as_ref()
                                .and_then(|source| {
                                    self.external_generics.as_ref().map(|reg| {
                                        let guard = reg.read().unwrap();
                                        if guard.contains(source, name) {
                                            Some(source.clone())
                                        } else {
                                            None
                                        }
                                    })
                                })
                                .flatten();

                            if let Some(source_module) = is_source_module_generic {
                                // This is a call to another generic from the source module
                                // Record for cross-module monomorphization
                                self.cross_module_monomorphizations.insert((
                                    source_module.clone(),
                                    name.clone(),
                                    type_names.clone(),
                                ));

                                // Call the locally monomorphized version
                                let mono_name = format!(
                                    "{}_{}_{}",
                                    source_module,
                                    name,
                                    type_names.join("_")
                                );
                                self.emit(&format!("apply '{}'/{}", mono_name, args.len()));
                                self.emit("(");
                                self.emit_args(args)?;
                                self.emit(")");
                            } else {
                                // Unknown generic call - emit as-is (will likely fail at runtime)
                                let mono_name = format!("{}_{}", name, type_names.join("_"));
                                self.emit(&format!("apply '{}'/{}", mono_name, args.len()));
                                self.emit("(");
                                self.emit_args(args)?;
                                self.emit(")");
                            }
                        } else if self.local_functions.contains(&(name.clone(), args.len())) {
                            // Local function call
                            self.emit(&format!("apply '{}'/{}", name, args.len()));
                            self.emit("(");
                            self.emit_args(args)?;
                            self.emit(")");
                        } else if let Some((module, original_name)) = self.imports.get(name) {
                            // Imported function call - add dream:: prefix for Dream stdlib modules
                            self.emit(&format!(
                                "call '{}':'{}'(",
                                Self::beam_module_name(&module.to_lowercase()),
                                original_name
                            ));
                            self.emit_args(args)?;
                            self.emit(")");
                        } else if self.variables.contains(name) {
                            // Variable in scope - apply the variable as a function
                            self.emit(&format!("apply {}(", Self::var_name(name)));
                            self.emit_args(args)?;
                            self.emit(")");
                        } else if Self::is_bif(name) {
                            // Check if it's a BIF (built-in function)
                            self.emit(&format!("call 'erlang':'{}'(", name));
                            self.emit_args(args)?;
                            self.emit(")");
                        } else if let Some(source_module) = &self.cross_module_inlining_source {
                            // We're inlining from another module - emit as cross-module call
                            self.emit(&format!(
                                "call '{}':'{}'(",
                                Self::beam_module_name(&source_module.to_lowercase()),
                                name
                            ));
                            self.emit_args(args)?;
                            self.emit(")");
                        } else {
                            // Unknown function - assume local
                            self.emit(&format!("apply '{}'/{}", name, args.len()));
                            self.emit("(");
                            self.emit_args(args)?;
                            self.emit(")");
                        }
                    }
                    Expr::Path { segments } if segments.len() == 2 => {
                        let first = &segments[0];
                        let second = &segments[1];

                        // Check if this is a trait dispatch with explicit type args: Trait::method::<T>(...)
                        // In this case, use the type arg to call the concrete implementation directly
                        // But only if the type args are concrete types, not type parameters
                        let has_concrete_type_args = !effective_type_args.is_empty()
                            && effective_type_args.iter().all(|t| {
                                match t {
                                    // Type parameters of the current function should use runtime dispatch
                                    Type::Named { name, .. } | Type::TypeVar(name) => {
                                        !self.current_func_type_params.contains(name)
                                    }
                                    // Concrete types like int, string, etc. can use static dispatch
                                    _ => true,
                                }
                            });

                        if self.traits.contains_key(first) && has_concrete_type_args {
                            let type_names: Vec<String> = effective_type_args
                                .iter()
                                .map(|t| self.type_to_name(t))
                                .collect();
                            // Trait_Type_method format: GenServer_Counter_init
                            let mangled_name = format!(
                                "{}_{}_{}",
                                first,
                                type_names.first().unwrap_or(&"".to_string()),
                                second
                            );
                            self.emit(&format!("apply '{}'/{}", mangled_name, args.len()));
                            self.emit("(");
                            self.emit_args(args)?;
                            self.emit(")");
                        } else if self.traits.contains_key(first) {
                            // Trait dispatch without explicit type args - use runtime dispatch
                            self.emit_trait_dispatch(first, second, args)?;
                        } else if self.cross_module_inlining_source.is_some()
                            && !type_args.is_empty()
                            && first.chars().next().map_or(false, |c| c.is_uppercase())
                        {
                            // When inlining from another module, trait dispatch calls like
                            // GenServer::init::<T>(args) need to be emitted as local calls
                            // to the concrete implementation in the current module
                            let type_names: Vec<String> = type_args
                                .iter()
                                .map(|t| self.type_to_name(t))
                                .collect();
                            // Trait_Type_method format: GenServer_Counter_init
                            let mangled_name = format!(
                                "{}_{}_{}",
                                first,
                                type_names.first().unwrap_or(&"".to_string()),
                                second
                            );
                            self.emit(&format!("apply '{}'/{}", mangled_name, args.len()));
                            self.emit("(");
                            self.emit_args(args)?;
                            self.emit(")");
                        } else if self.has_impl_method(first, second) {
                            // Call the mangled impl method: Type::method()
                            let mangled_name = format!("{}_{}", first, second);
                            self.emit(&format!("apply '{}'/{}", mangled_name, args.len()));
                            self.emit("(");
                            self.emit_args(args)?;
                            self.emit(")");
                        } else if let Some(mangled_name) =
                            self.find_trait_impl_method(first, second, effective_type_args)
                        {
                            // Call a trait impl method: Type::trait_method() or Type::method::<TraitArg>()
                            // E.g., Wrapper::from::<int>() -> From_int_Wrapper_from
                            self.emit(&format!("apply '{}'/{}", mangled_name, args.len()));
                            self.emit("(");
                            self.emit_args(args)?;
                            self.emit(")");
                        } else if second == "from" && args.len() == 1 && effective_type_args.is_empty() {
                            // Special case: Type::from(value) - infer trait type arg from argument
                            // E.g., Wrapper::from(42) -> infer From<int>, call From_int_Wrapper_from
                            let arg_type = self.infer_expr_type(&args[0]);
                            let mangled_name = format!("From_{}_{}_from", arg_type, first);

                            // Check if this impl exists
                            if self.has_trait_impl_method(&format!("From_{}_{}", arg_type, first), "from") {
                                self.emit(&format!("apply '{}'/{}", mangled_name, args.len()));
                                self.emit("(");
                                self.emit_args(args)?;
                                self.emit(")");
                            } else {
                                // Fall through to module call
                                self.emit(&format!(
                                    "call '{}':'{}'",
                                    Self::beam_module_name(&first.to_lowercase()),
                                    second
                                ));
                                self.emit("(");
                                self.emit_args(args)?;
                                self.emit(")");
                            }
                        } else {
                            // Module:Function call - add dream:: prefix for Dream modules
                            // Check if this is a call with type args (explicit or inferred)
                            if !effective_type_args.is_empty() {
                                // Cross-module generic call: genserver::start_typed::<Counter>()
                                // Check if we have the generic function in the registry
                                let type_names: Vec<String> = effective_type_args
                                    .iter()
                                    .map(|t| self.type_to_name(t))
                                    .collect();
                                let source_module = segments[0].clone();
                                let func_name = segments[1].clone();

                                // Check if this generic exists in external registry
                                let has_external_generic = self.external_generics
                                    .as_ref()
                                    .map(|reg| {
                                        let guard = reg.read().unwrap();
                                        guard.contains(&source_module, &func_name)
                                    })
                                    .unwrap_or(false);

                                if has_external_generic {
                                    // Record for cross-module monomorphization
                                    self.cross_module_monomorphizations.insert((
                                        source_module.clone(),
                                        func_name.clone(),
                                        type_names.clone(),
                                    ));

                                    // Call the locally monomorphized version
                                    let mono_name = format!(
                                        "{}_{}_{}",
                                        source_module,
                                        func_name,
                                        type_names.join("_")
                                    );
                                    self.emit(&format!("apply '{}'/{}", mono_name, args.len()));
                                    self.emit("(");
                                    self.emit_args(args)?;
                                    self.emit(")");
                                } else {
                                    // No registry or function not found - emit as cross-module call
                                    // (this will fail at runtime if the function doesn't exist)
                                    let mono_name = format!("{}_{}", func_name, type_names.join("_"));
                                    self.emit(&format!(
                                        "call '{}':'{}'",
                                        Self::beam_module_name(&source_module.to_lowercase()),
                                        mono_name
                                    ));
                                    self.emit("(");
                                    self.emit_args(args)?;
                                    self.emit(")");
                                }
                            } else {
                                // Non-generic cross-module call
                                // Check if first segment is an imported type
                                if let Some((module, _original_name)) = self.imports.get(first) {
                                    // Imported type impl method call: User::new() where User is imported
                                    // Becomes: call 'dream::module':'Type_method'(args)
                                    let mangled_name = format!("{}_{}", first, second);
                                    self.emit(&format!(
                                        "call '{}':'{}'",
                                        Self::beam_module_name(&module.to_lowercase()),
                                        mangled_name
                                    ));
                                    self.emit("(");
                                    self.emit_args(args)?;
                                    self.emit(")");
                                } else if Self::STDLIB_MODULES.contains(&first.as_str()) {
                                    // Stdlib module call: io::println -> dream::io::println
                                    // These take precedence over extern modules with the same name
                                    self.emit(&format!(
                                        "call '{}':'{}'",
                                        Self::beam_module_name(first),
                                        second
                                    ));
                                    self.emit("(");
                                    self.emit_args(args)?;
                                    self.emit(")");
                                } else if let Some(beam_module) = self.extern_module_names.get(first) {
                                    // Extern module call: jason::encode() where jason is extern mod
                                    // Uses the BEAM module name from #[name = "..."] attribute
                                    self.emit(&format!(
                                        "call '{}':'{}'",
                                        beam_module,
                                        second
                                    ));
                                    self.emit("(");
                                    self.emit_args(args)?;
                                    self.emit(")");
                                } else {
                                    // Check if this is an external crate call (e.g., serde_json::to_string_value)
                                    // If so, resolve to the crate's lib module: dream::crate::crate
                                    let module_name = if self.module_context.dependencies.contains(&segments[0]) {
                                        // External crate - lib module is at crate::crate
                                        format!("dream::{}::{}", segments[0], segments[0])
                                    } else {
                                        Self::beam_module_name(&segments[0].to_lowercase())
                                    };
                                    self.emit(&format!(
                                        "call '{}':'{}'",
                                        module_name,
                                        segments[1]
                                    ));
                                    self.emit("(");
                                    self.emit_args(args)?;
                                    self.emit(")");
                                }
                            }
                        }
                    }
                    Expr::Path { segments } if segments.len() >= 3 => {
                        // Distinguish between:
                        // 1. module::Type::method() - impl method (Type starts uppercase)
                        // 2. module::submodule::function() - nested module (submodule starts lowercase)
                        let second = &segments[1];
                        let is_type = second.chars().next().map_or(false, |c| c.is_uppercase());

                        if is_type && segments.len() == 3 {
                            // Cross-module impl method call: module::Type::method()
                            // Becomes: call 'dream::module':'Type_method'(args)
                            let module = &segments[0];
                            let type_name = &segments[1];
                            let method = &segments[2];
                            let mangled_name = format!("{}_{}", type_name, method);
                            self.emit(&format!(
                                "call '{}':'{}'",
                                Self::beam_module_name(&module.to_lowercase()),
                                mangled_name
                            ));
                            self.emit("(");
                            self.emit_args(args)?;
                            self.emit(")");
                        } else {
                            // Nested module function call: module::submodule::function()
                            // Module path is segments[0..n-1], function is segments[n-1]
                            let func_name = &segments[segments.len() - 1];
                            let module_segments = &segments[..segments.len() - 1];
                            let module_path = module_segments.join("::");

                            // Resolve the module path (handles user modules within package)
                            let resolved = self.resolve_user_module_path(&module_path);
                            self.emit(&format!(
                                "call '{}':'{}'",
                                resolved,
                                func_name
                            ));
                            self.emit("(");
                            self.emit_args(args)?;
                            self.emit(")");
                        }
                    }
                    Expr::Atom(name) => {
                        // Bare atom call: :hd(args) -> erlang:hd(args)
                        // This handles calls like :hd(list), :tl(list), :length(list), etc.
                        self.emit(&format!("call 'erlang':'{}'(", name));
                        self.emit_args(args)?;
                        self.emit(")");
                    }
                    _ => {
                        // Higher-order function application
                        self.emit("apply ");
                        self.emit_expr(func)?;
                        self.emit("(");
                        self.emit_args(args)?;
                        self.emit(")");
                    }
                }
            }

            Expr::If {
                cond,
                then_block,
                else_block,
            } => {
                // Convert if to case on boolean
                self.emit("case ");
                self.emit_expr(cond)?;
                self.emit(" of");
                self.newline();
                self.indent += 1;

                self.emit("<'true'> when 'true' ->");
                self.newline();
                self.indent += 1;
                self.emit_block(then_block)?;
                self.indent -= 1;
                self.newline();

                self.emit("<'false'> when 'true' ->");
                self.newline();
                self.indent += 1;
                if let Some(else_blk) = else_block {
                    self.emit_block(else_blk)?;
                } else {
                    self.emit("'ok'");
                }
                self.indent -= 1;

                self.indent -= 1;
                self.newline();
                self.emit("end");
            }

            Expr::Match { expr, arms } => {
                self.emit("case ");
                self.emit_expr(expr)?;
                self.emit(" of");
                self.newline();
                self.indent += 1;

                for (i, arm) in arms.iter().enumerate() {
                    if i > 0 {
                        self.newline();
                    }
                    self.emit_match_arm(arm)?;
                }

                self.indent -= 1;
                self.newline();
                self.emit("end");
            }

            Expr::Block(block) => {
                self.emit_block(block)?;
            }

            Expr::Tuple(elements) => {
                // Use emit_data_expr for tuple elements to avoid module resolution
                self.emit("{");
                for (i, e) in elements.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit_data_expr(e)?;
                }
                self.emit("}");
            }

            Expr::List(elements) => {
                // Use emit_data_expr for list elements to avoid module resolution
                if elements.is_empty() {
                    self.emit("[]");
                } else {
                    self.emit("[");
                    for (i, e) in elements.iter().enumerate() {
                        if i > 0 {
                            self.emit(", ");
                        }
                        self.emit_data_expr(e)?;
                    }
                    self.emit("]");
                }
            }

            Expr::ListCons { head, tail } => {
                // Use emit_data_expr for list cons elements
                self.emit("[");
                self.emit_data_expr(head)?;
                self.emit("|");
                self.emit_data_expr(tail)?;
                self.emit("]");
            }

            Expr::MapLiteral(pairs) => {
                if pairs.is_empty() {
                    self.emit("~{}~");
                } else {
                    self.emit("~{");
                    for (i, (key, value)) in pairs.iter().enumerate() {
                        if i > 0 {
                            self.emit(",");
                        }
                        // Emit map keys directly (atoms shouldn't be resolved as module names)
                        self.emit_map_key(key)?;
                        self.emit("=>");
                        self.emit_expr(value)?;
                    }
                    self.emit("}~");
                }
            }

            Expr::Spawn(expr) => {
                // Core Erlang requires binding the fun to a variable first
                let tmp_var = self.fresh_var();
                self.emit(&format!("let <{}> =", tmp_var));
                self.newline();
                self.indent += 1;
                self.emit("fun () ->");
                self.newline();
                self.indent += 1;
                self.emit_expr(expr)?;
                self.indent -= 1;
                self.indent -= 1;
                self.newline();
                self.emit(&format!("in call 'erlang':'spawn'({})", tmp_var));
            }

            Expr::SpawnClosure(block) => {
                // Core Erlang requires binding the fun to a variable first
                let tmp_var = self.fresh_var();
                self.emit(&format!("let <{}> =", tmp_var));
                self.newline();
                self.indent += 1;
                self.emit("fun () ->");
                self.newline();
                self.indent += 1;
                self.emit_block(block)?;
                self.indent -= 1;
                self.indent -= 1;
                self.newline();
                self.emit(&format!("in call 'erlang':'spawn'({})", tmp_var));
            }

            Expr::Closure { params, body } => {
                // Generate Core Erlang anonymous function
                // Add closure parameters to scope (closures capture outer variables)
                for param in params {
                    self.variables.insert(param.clone());
                }

                self.emit("fun (");
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    // Convert param name to Core Erlang variable (capitalize first letter)
                    self.emit(&Self::var_name(param));
                }
                self.emit(") ->");
                self.newline();
                self.indent += 1;
                self.emit_block(body)?;
                self.indent -= 1;
            }

            Expr::Send { to, msg } => {
                self.emit("call 'erlang':'!'(");
                self.emit_expr(to)?;
                self.emit(", ");
                self.emit_expr(msg)?;
                self.emit(")");
            }

            Expr::Receive { arms, timeout } => {
                // Core Erlang uses primops for receive, not a simple receive...end
                // Structure:
                // ( letrec
                //     'recv$^N'/0 = fun () -> ...
                //     in apply 'recv$^N'/0() )
                self.emit_receive_primops(arms, timeout.as_ref())?;
            }

            Expr::Return(opt_expr) => {
                // Core Erlang doesn't have explicit return, just emit the expression
                if let Some(e) = opt_expr {
                    self.emit_expr(e)?;
                } else {
                    self.emit("'ok'");
                }
            }

            Expr::BitString(segments) => {
                self.emit("#{");
                for (i, seg) in segments.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit_bitstring_segment_expr(seg)?;
                }
                self.emit("}#");
            }

            Expr::MethodCall { receiver, method, type_args, args, resolved_module, inferred_type_args: _ } => {
                // Check for trait method calls with type args: 42.into::<Wrapper>()
                // For Into<T>, the type arg is the target type
                if method == "into" && type_args.len() == 1 {
                    // into::<TargetType>() on a value
                    // We need to determine the source type and call Into_TargetType_SourceType_into
                    let target_type = self.type_to_name(&type_args[0]);
                    let source_type = self.infer_expr_type(receiver);

                    let mangled_name = format!("Into_{}_{}_into", target_type, source_type);
                    self.emit(&format!("apply '{}'", mangled_name));
                    self.emit("/1(");
                    self.emit_expr(receiver)?;
                    self.emit(")");
                    return Ok(());
                }

                // UFCS: Transform expr.method(args) into method(expr, args)
                let mut all_args = vec![receiver.as_ref().clone()];
                all_args.extend(args.iter().cloned());

                // Priority 1: Type-directed resolution (from resolve_stdlib_methods pass)
                if let Some(module) = resolved_module {
                    self.emit(&format!(
                        "call '{}':'{}'(",
                        Self::beam_module_name(module),
                        method
                    ));
                    self.emit_args(&all_args)?;
                    self.emit(")");
                }
                // Priority 2: Check if it's an imported function
                else if let Some((module, original_name)) = self.imports.get(method) {
                    // Imported function call - add dream:: prefix for Dream stdlib modules
                    self.emit(&format!(
                        "call '{}':'{}'(",
                        Self::beam_module_name(&module.to_lowercase()),
                        original_name
                    ));
                    self.emit_args(&all_args)?;
                    self.emit(")");
                } else {
                    // Check if there are impl methods with this name
                    let impl_types = self.find_impl_types_for_method(method);

                    if impl_types.is_empty() {
                        // No local impl methods - use dynamic cross-module dispatch
                        // The __struct__ tag format is 'module_TypeName', we parse it to find the module
                        self.emit_dynamic_method_dispatch(method, receiver, args)?;
                    } else {
                        // Runtime dispatch based on __struct__ tag for known local types
                        self.emit_method_dispatch(method, receiver, args, &impl_types)?;
                    }
                }
            }

            Expr::StructInit { name, fields, base } => {
                // Check if this struct is marked as an Erlang record
                let record_name = self.struct_info.get(name).and_then(|info| info.record_name.clone());

                if let Some(record_tag) = record_name {
                    // Erlang record: emit as tuple {'record_name', field1, field2, ...}
                    // Struct update syntax not supported for records
                    if base.is_some() {
                        return Err(CoreErlangError::new(format!(
                            "Struct update syntax not supported for record types ({})",
                            name
                        )));
                    }

                    // Get field order from struct_info (clone to avoid borrow issues)
                    let struct_fields: Vec<_> = self.struct_info.get(name)
                        .map(|info| info.fields.iter().map(|(n, _)| n.clone()).collect())
                        .unwrap_or_default();
                    self.emit(&format!("{{'{}'", record_tag));
                    for field_name in &struct_fields {
                        self.emit(", ");
                        // Find the value for this field in the initialization
                        if let Some((_, value)) = fields.iter().find(|(n, _)| n == field_name) {
                            self.emit_expr(value)?;
                        } else {
                            // Field not provided - use 'undefined' as default
                            self.emit("'undefined'");
                        }
                    }
                    self.emit("}");
                } else {
                    // Regular struct: map with __struct__ tag
                    let (beam_module, type_name) = if let Some((module, original_name)) = self.imports.get(name) {
                        // Imported struct - use the imported module's BEAM name
                        (Self::beam_module_name(&module.to_lowercase()), original_name.clone())
                    } else {
                        // Local struct - use current module's BEAM name (already has correct prefix)
                        (self.module_name.clone(), name.clone())
                    };

                    if let Some(base_expr) = base {
                        // Struct update syntax: maps:merge(base, #{updated_fields})
                        self.emit("call 'maps':'merge'(");
                        self.emit_expr(base_expr)?;
                        self.emit(", ~{");
                        for (i, (field_name, value)) in fields.iter().enumerate() {
                            if i > 0 {
                                self.emit(", ");
                            }
                            self.emit(&format!("'{}' => ", field_name));
                            self.emit_expr(value)?;
                        }
                        self.emit("}~)");
                    } else {
                        // Full struct init
                        self.emit("~{");
                        // Add __struct__ tag with BEAM module name for method dispatch
                        self.emit(&format!("'__struct__' => '{}::{}'", beam_module, type_name));
                        for (field_name, value) in fields.iter() {
                            self.emit(", ");
                            self.emit(&format!("'{}' => ", field_name));
                            self.emit_expr(value)?;
                        }
                        self.emit("}~");
                    }
                }
            }

            Expr::EnumVariant { type_name, variant, args } => {
                // Special handling for Result type to match Erlang conventions:
                // - Ok(()) → 'ok' (just the atom, for Result<(), E>)
                // - Ok(value) → {'ok', value}
                // - Err(value) → {'error', value} (note: 'error' not 'err')
                // Note: type_name may be None when Result is inferred, so we also check variant names
                let is_result = type_name.as_deref() == Some("Result")
                    || (type_name.is_none() && (variant == "Ok" || variant == "Err"));
                if is_result {
                    match (variant.as_str(), args) {
                        ("Ok", EnumVariantArgs::Tuple(exprs)) if exprs.len() == 1 => {
                            if let Expr::Unit = exprs[0].inner() {
                                // Ok(()) becomes just 'ok'
                                self.emit("'ok'");
                            } else {
                                // Ok(value) becomes {'ok', value}
                                self.emit("{'ok', ");
                                self.emit_expr(&exprs[0])?;
                                self.emit("}");
                            }
                        }
                        ("Ok", EnumVariantArgs::Tuple(exprs)) => {
                            // Multiple args (shouldn't happen for Result, but handle it)
                            self.emit("{'ok'");
                            for arg in exprs {
                                self.emit(", ");
                                self.emit_expr(arg)?;
                            }
                            self.emit("}");
                        }
                        ("Err", EnumVariantArgs::Tuple(exprs)) => {
                            // Err(value) becomes {'error', value}
                            self.emit("{'error'");
                            for arg in exprs {
                                self.emit(", ");
                                self.emit_expr(arg)?;
                            }
                            self.emit("}");
                        }
                        _ => {
                            // Unknown Result variant or struct variant, use default behavior
                            self.emit_enum_variant_default(variant, args)?;
                        }
                    }
                } else {
                    // Default enum variant handling
                    self.emit_enum_variant_default(variant, args)?;
                }
            }

            Expr::FieldAccess { expr, field } => {
                // Check if this is accessing a record type
                let record_info = if let Expr::Ident(var_name) = expr.inner() {
                    // Look up variable type
                    if let Some(struct_name) = self.variable_types.get(var_name) {
                        // Check if the struct is a record
                        if let Some(info) = self.struct_info.get(struct_name) {
                            if let Some(record_name) = &info.record_name {
                                // Find field index
                                let field_index = info.fields.iter()
                                    .position(|(f, _)| f == field)
                                    .map(|i| (record_name.clone(), i + 2)); // +2 because element 1 is record name
                                field_index
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                };

                if let Some((_record_name, field_position)) = record_info {
                    // Record field access: element(N, Record)
                    self.emit(&format!("call 'erlang':'element'({}, ", field_position));
                    self.emit_expr(expr)?;
                    self.emit(")");
                } else {
                    // Map field access
                    self.emit("call 'maps':'get'('");
                    self.emit(field);
                    self.emit("', ");
                    self.emit_expr(expr)?;
                    self.emit(")");
                }
            }

            Expr::Try { expr } => {
                // The ? operator desugars to a case expression:
                // - Ok(value) or 'ok' → return value or 'ok'
                // - Err(e) → throw the error for early return
                // A surrounding try/catch at function level can handle this.
                let tmp = self.fresh_var();
                let err_var = self.fresh_var();
                self.emit("case ");
                self.emit_expr(expr)?;
                self.emit(" of");
                self.indent += 1;
                self.newline();

                // Match Ok(value) → value
                self.emit(&format!("<{{'ok', {}}}> when 'true' ->", tmp));
                self.indent += 1;
                self.newline();
                self.emit(&tmp);
                self.indent -= 1;
                self.newline();

                // Match 'ok' (for Result<(), E>) → 'ok'
                self.emit("<'ok'> when 'true' ->");
                self.indent += 1;
                self.newline();
                self.emit("'ok'");
                self.indent -= 1;
                self.newline();

                // Match Some(value) for Option → value
                self.emit(&format!("<{{'some', {}}}> when 'true' ->", tmp));
                self.indent += 1;
                self.newline();
                self.emit(&tmp);
                self.indent -= 1;
                self.newline();

                // Match Err(e) → throw for early return
                self.emit(&format!("<{{'error', {}}}> when 'true' ->", err_var));
                self.indent += 1;
                self.newline();
                self.emit(&format!("call 'erlang':'throw'({{'error', {}}})", err_var));
                self.indent -= 1;
                self.newline();

                // Match None for Option → throw for early return
                self.emit("<'none'> when 'true' ->");
                self.indent += 1;
                self.newline();
                self.emit("call 'erlang':'throw'('none')");
                self.indent -= 1;
                self.newline();

                self.indent -= 1;
                self.emit("end");
            }

            Expr::Path { segments } => {
                // Module path - emit as atom or function reference
                if segments.len() == 1 {
                    self.emit(&format!("'{}'", segments[0]));
                } else {
                    // Module:function reference
                    self.emit(&format!(
                        "fun '{}':'{}'",
                        segments[0].to_lowercase(),
                        segments[1]
                    ));
                }
            }

            Expr::ExternCall { module, function, args } => {
                // External function call: :erlang::abs(x) → call 'erlang':'abs'(X)
                // Use mapped BEAM module name if #[name = "..."] attribute was used
                let beam_module = self
                    .extern_module_names
                    .get(module)
                    .map(|s| s.as_str())
                    .unwrap_or(module);
                // Use mapped BEAM function name if #[name = "..."] attribute was used
                let arity = args.len();
                let beam_function = self
                    .extern_function_names
                    .get(&(module.clone(), function.clone(), arity))
                    .map(|s| s.as_str())
                    .unwrap_or(function);
                self.emit(&format!("call '{}':'{}'(", beam_module, beam_function));
                self.emit_args(args)?;
                self.emit(")");
            }

            Expr::Pipe { left, right } => {
                // Transform `left |> right` where right is typically a call expression.
                // `a |> f(b, c)` becomes `f(a, b, c)`
                // `a |> f` becomes `f(a)`
                match right.inner() {
                    Expr::Call {
                        func,
                        type_args,
                        inferred_type_args,
                        args,
                    } => {
                        // Prepend left as first argument
                        let mut new_args = vec![(**left).clone()];
                        new_args.extend(args.iter().cloned());
                        let new_call = SpannedExpr::unspanned(Expr::Call {
                            func: func.clone(),
                            type_args: type_args.clone(),
                            inferred_type_args: inferred_type_args.clone(),
                            args: new_args,
                        });
                        self.emit_expr(&new_call)?;
                    }
                    Expr::Ident(name) => {
                        // Bare function: `a |> f` becomes `f(a)`
                        let new_call = SpannedExpr::unspanned(Expr::Call {
                            func: SpannedExpr::boxed(Expr::Ident(name.clone())),
                            type_args: vec![],
                            inferred_type_args: vec![],
                            args: vec![(**left).clone()],
                        });
                        self.emit_expr(&new_call)?;
                    }
                    Expr::Path { segments } => {
                        // Path without call: `a |> Module::func` becomes `Module::func(a)`
                        let new_call = SpannedExpr::unspanned(Expr::Call {
                            func: SpannedExpr::boxed(Expr::Path {
                                segments: segments.clone(),
                            }),
                            type_args: vec![],
                            inferred_type_args: vec![],
                            args: vec![(**left).clone()],
                        });
                        self.emit_expr(&new_call)?;
                    }
                    _ => {
                        return Err(CoreErlangError::new(
                            "pipe right-hand side must be a function call or identifier",
                        ));
                    }
                }
            }

            // Quote: generate Erlang tuple construction code representing the AST
            Expr::Quote(inner) => {
                self.emit_quoted_expr(inner)?;
            }

            // Quote item: generate Erlang tuple for an item (impl, fn, struct, etc.)
            Expr::QuoteItem(item) => {
                self.emit_quoted_item(item)?;
            }

            // For loop expression
            Expr::For {
                clauses,
                body,
                is_comprehension,
            } => {
                self.emit_for_expr(clauses, body, *is_comprehension)?;
            }

            // Unquote outside of quote is an error
            Expr::Unquote(_) | Expr::UnquoteSplice(_) | Expr::UnquoteAtom(_) | Expr::UnquoteFieldAccess { .. } | Expr::QuoteRepetition { .. } => {
                return Err(CoreErlangError::new(
                    "unquote/unquote-splice/quote-repetition can only be used inside quote blocks",
                ));
            }
        }

        Ok(())
    }

    /// Emit a quoted expression as Erlang tuple construction.
    ///
    /// This generates Core Erlang code that builds tuples representing the AST.
    /// For example, `quote { 1 + 2 }` becomes `{'binary_op', '+', {'int', 1}, {'int', 2}}`.
    ///
    /// Unquote (`#var`) inside a quote inserts the variable's value directly,
    /// allowing runtime values to be spliced into the generated AST.
    fn emit_quoted_expr(&mut self, expr: &SpannedExpr) -> CoreErlangResult<()> {
        match expr.inner() {
            // Unquote: insert the variable's value directly
            Expr::Unquote(inner) => {
                // The inner expression is evaluated at runtime and its value
                // (which should be an AST tuple) is inserted directly
                self.emit_expr(inner)?;
            }

            // UnquoteAtom: create an atom tuple from the unquoted value
            Expr::UnquoteAtom(inner) => {
                // Generate: {'atom', <value>}
                // The inner expression should evaluate to an atom at runtime
                self.emit("{'atom', ");
                self.emit_expr(inner)?;
                self.emit("}");
            }

            // UnquoteSplice is only valid in list context
            Expr::UnquoteSplice(_) => {
                return Err(CoreErlangError::new(
                    "unquote-splice (#..) can only be used inside lists",
                ));
            }

            // Nested quote
            Expr::Quote(inner) => {
                self.emit("{'quote', ");
                self.emit_quoted_expr(inner)?;
                self.emit("}");
            }

            // Nested quote item
            Expr::QuoteItem(item) => {
                self.emit("{'quote_item', ");
                self.emit_quoted_item(item)?;
                self.emit("}");
            }

            // Literals
            Expr::Int(n) => {
                self.emit(&format!("{{'int', {}}}", n));
            }

            Expr::String(s) => {
                // Use list_to_binary to construct the binary at runtime (Core Erlang compatible)
                let chars: Vec<String> = s.chars().map(|c| (c as u32).to_string()).collect();
                self.emit(&format!("{{'string', call 'erlang':'list_to_binary'([{}])}}", chars.join(", ")));
            }

            Expr::Charlist(s) => {
                self.emit(&format!("{{'charlist', \"{}\"}}", self.escape_binary_string(s)));
            }

            Expr::Atom(a) => {
                self.emit(&format!("{{'atom', '{}'}}", self.escape_erlang_atom(a)));
            }

            Expr::Bool(b) => {
                self.emit(&format!("{{'bool', {}}}", b));
            }

            Expr::Unit => {
                self.emit("{'unit'}");
            }

            Expr::Ident(name) => {
                self.emit(&format!("{{'ident', '{}'}}", self.escape_erlang_atom(name)));
            }

            Expr::Path { segments } => {
                self.emit("{'path', [");
                for (i, seg) in segments.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit(&format!("'{}'", self.escape_erlang_atom(seg)));
                }
                self.emit("]}");
            }

            Expr::Binary { op, left, right } => {
                self.emit(&format!("{{'binary_op', '{}', ", self.binop_to_atom(*op)));
                self.emit_quoted_expr(left)?;
                self.emit(", ");
                self.emit_quoted_expr(right)?;
                self.emit("}");
            }

            Expr::Unary { op, expr } => {
                self.emit(&format!("{{'unary_op', '{}', ", self.unaryop_to_atom(*op)));
                self.emit_quoted_expr(expr)?;
                self.emit("}");
            }

            Expr::Call { func, args, .. } => {
                self.emit("{'call', ");
                self.emit_quoted_expr(func)?;
                self.emit(", ");
                self.emit_quoted_list(args)?;
                self.emit("}");
            }

            Expr::MethodCall { receiver, method, args, .. } => {
                self.emit("{'method_call', ");
                self.emit_quoted_expr(receiver)?;
                self.emit(&format!(", '{}', ", self.escape_erlang_atom(method)));
                self.emit_quoted_list(args)?;
                self.emit("}");
            }

            Expr::FieldAccess { expr, field } => {
                self.emit("{'field_access', ");
                self.emit_quoted_expr(expr)?;
                self.emit(&format!(", '{}'}}", self.escape_erlang_atom(field)));
            }

            // Dynamic field access: expr.#field_var - the field name comes from a variable
            Expr::UnquoteFieldAccess { expr, field_expr } => {
                self.emit("{'field_access', ");
                self.emit_quoted_expr(expr)?;
                self.emit(", ");
                // The field expression is evaluated at runtime to get the field name
                self.emit_expr(field_expr)?;
                self.emit("}");
            }

            Expr::Tuple(elems) => {
                self.emit("{'tuple', ");
                self.emit_quoted_list(elems)?;
                self.emit("}");
            }

            Expr::List(elems) => {
                self.emit("{'list', ");
                self.emit_quoted_list_with_splice(elems)?;
                self.emit("}");
            }

            Expr::ListCons { head, tail } => {
                self.emit("{'list_cons', ");
                self.emit_quoted_expr(head)?;
                self.emit(", ");
                self.emit_quoted_expr(tail)?;
                self.emit("}");
            }

            Expr::MapLiteral(pairs) => {
                self.emit("{'map_literal', [");
                for (i, (key, value)) in pairs.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit("{");
                    self.emit_quoted_expr(key)?;
                    self.emit(", ");
                    self.emit_quoted_expr(value)?;
                    self.emit("}");
                }
                self.emit("]}");
            }

            Expr::StructInit { name, fields, base } => {
                self.emit(&format!("{{'struct_init', '{}', [", self.escape_erlang_atom(name)));
                for (i, (field_name, field_expr)) in fields.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit(&format!("{{'{}', ", self.escape_erlang_atom(field_name)));
                    self.emit_quoted_expr(field_expr)?;
                    self.emit("}");
                }
                self.emit("]");
                if let Some(base_expr) = base {
                    self.emit(", {'base', ");
                    self.emit_quoted_expr(base_expr)?;
                    self.emit("}");
                }
                self.emit("}");
            }

            Expr::If { cond, then_block, else_block } => {
                self.emit("{'if', ");
                self.emit_quoted_expr(cond)?;
                self.emit(", ");
                self.emit_quoted_block(then_block)?;
                if let Some(else_blk) = else_block {
                    self.emit(", ");
                    self.emit_quoted_block(else_blk)?;
                } else {
                    self.emit(", 'none'");
                }
                self.emit("}");
            }

            Expr::Block(block) => {
                self.emit("{'block', ");
                self.emit_quoted_block(block)?;
                self.emit("}");
            }

            Expr::Return(e) => {
                self.emit("{'return', ");
                if let Some(inner) = e {
                    self.emit_quoted_expr(inner)?;
                } else {
                    self.emit("'none'");
                }
                self.emit("}");
            }

            Expr::Closure { params, body } => {
                self.emit("{'closure', [");
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit(&format!("'{}'", self.escape_erlang_atom(param)));
                }
                self.emit("], ");
                self.emit_quoted_block(body)?;
                self.emit("}");
            }

            Expr::ExternCall { module, function, args } => {
                self.emit(&format!("{{'extern_call', '{}', '{}', ",
                    self.escape_erlang_atom(module),
                    self.escape_erlang_atom(function)));
                self.emit_quoted_list(args)?;
                self.emit("}");
            }

            // For complex expressions, emit a simplified representation
            _ => {
                // Fallback: emit as a generic tagged tuple
                self.emit("{'expr', 'unsupported'}");
            }
        }
        Ok(())
    }

    /// Emit a list of quoted expressions.
    fn emit_quoted_list(&mut self, exprs: &[SpannedExpr]) -> CoreErlangResult<()> {
        self.emit("[");
        for (i, expr) in exprs.iter().enumerate() {
            if i > 0 {
                self.emit(", ");
            }
            self.emit_quoted_expr(expr)?;
        }
        self.emit("]");
        Ok(())
    }

    /// Emit a list of quoted expressions, handling unquote-splice.
    fn emit_quoted_list_with_splice(&mut self, exprs: &[SpannedExpr]) -> CoreErlangResult<()> {
        // Check if any element is an unquote-splice
        let has_splice = exprs.iter().any(|e| matches!(e.inner(), Expr::UnquoteSplice(_)));

        if !has_splice {
            // Simple case: no splicing
            return self.emit_quoted_list(exprs);
        }

        // Complex case: need to concatenate lists
        // Build: lists:append([[elem1], [elem2], SplicedList, [elem3], ...])
        self.emit("call 'lists':'append'([");
        let mut first = true;
        for expr in exprs {
            if !first {
                self.emit(", ");
            }
            first = false;

            match expr.inner() {
                Expr::UnquoteSplice(inner) => {
                    // The spliced value should be a list, insert directly
                    self.emit_expr(inner)?;
                }
                _ => {
                    // Wrap single element in a list
                    self.emit("[");
                    self.emit_quoted_expr(expr)?;
                    self.emit("]");
                }
            }
        }
        self.emit("])");
        Ok(())
    }

    /// Emit a quoted block.
    fn emit_quoted_block(&mut self, block: &Block) -> CoreErlangResult<()> {
        self.emit("{[");
        for (i, stmt) in block.stmts.iter().enumerate() {
            if i > 0 {
                self.emit(", ");
            }
            self.emit_quoted_stmt(stmt)?;
        }
        self.emit("], ");
        if let Some(expr) = &block.expr {
            self.emit_quoted_expr(expr)?;
        } else {
            self.emit("'none'");
        }
        self.emit("}");
        Ok(())
    }

    /// Emit a quoted statement.
    fn emit_quoted_stmt(&mut self, stmt: &Stmt) -> CoreErlangResult<()> {
        match stmt {
            Stmt::Let { pattern, ty: _, value, else_block, .. } => {
                self.emit("{'let', ");
                self.emit_quoted_pattern(pattern)?;
                self.emit(", ");
                self.emit_quoted_expr(value)?;
                if let Some(else_blk) = else_block {
                    self.emit(", ");
                    self.emit_quoted_block(else_blk)?;
                }
                self.emit("}");
            }
            Stmt::Expr { expr, .. } => {
                self.emit("{'expr_stmt', ");
                self.emit_quoted_expr(expr)?;
                self.emit("}");
            }
        }
        Ok(())
    }

    /// Emit a quoted pattern.
    fn emit_quoted_pattern(&mut self, pattern: &Pattern) -> CoreErlangResult<()> {
        match pattern {
            Pattern::Wildcard => {
                self.emit("{'wildcard'}");
            }
            Pattern::Ident(name) => {
                self.emit(&format!("{{'ident', '{}'}}", self.escape_erlang_atom(name)));
            }
            Pattern::Int(n) => {
                self.emit(&format!("{{'int', {}}}", n));
            }
            Pattern::String(s) => {
                // Use list_to_binary to construct the binary at runtime (Core Erlang compatible)
                let chars: Vec<String> = s.chars().map(|c| (c as u32).to_string()).collect();
                self.emit(&format!("{{'string', call 'erlang':'list_to_binary'([{}])}}", chars.join(", ")));
            }
            Pattern::Charlist(s) => {
                self.emit(&format!("{{'charlist', \"{}\"}}", self.escape_binary_string(s)));
            }
            Pattern::Atom(a) => {
                self.emit(&format!("{{'atom', '{}'}}", self.escape_erlang_atom(a)));
            }
            Pattern::Bool(b) => {
                self.emit(&format!("{{'bool', {}}}", b));
            }
            Pattern::Tuple(pats) => {
                self.emit("{'tuple', [");
                for (i, pat) in pats.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit_quoted_pattern(pat)?;
                }
                self.emit("]}");
            }
            Pattern::List(pats) => {
                self.emit("{'list', [");
                for (i, pat) in pats.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit_quoted_pattern(pat)?;
                }
                self.emit("]}");
            }
            Pattern::ListCons { head, tail } => {
                self.emit("{'list_cons', ");
                self.emit_quoted_pattern(head)?;
                self.emit(", ");
                self.emit_quoted_pattern(tail)?;
                self.emit("}");
            }
            Pattern::Struct { name, fields } => {
                self.emit(&format!("{{'struct', '{}', [", self.escape_erlang_atom(name)));
                for (i, (field_name, field_pat)) in fields.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit(&format!("{{'{}', ", self.escape_erlang_atom(field_name)));
                    self.emit_quoted_pattern(field_pat)?;
                    self.emit("}");
                }
                self.emit("]}");
            }
            Pattern::Enum { name, variant, fields } => {
                self.emit(&format!("{{'enum', '{}', '{}', ",
                    self.escape_erlang_atom(name),
                    self.escape_erlang_atom(variant)));
                match fields {
                    EnumPatternFields::Unit => self.emit("'unit'"),
                    EnumPatternFields::Tuple(pats) => {
                        self.emit("[");
                        for (i, pat) in pats.iter().enumerate() {
                            if i > 0 {
                                self.emit(", ");
                            }
                            self.emit_quoted_pattern(pat)?;
                        }
                        self.emit("]");
                    }
                    EnumPatternFields::Struct(flds) => {
                        self.emit("[");
                        for (i, (field_name, pat)) in flds.iter().enumerate() {
                            if i > 0 {
                                self.emit(", ");
                            }
                            self.emit(&format!("{{'{}', ", self.escape_erlang_atom(field_name)));
                            self.emit_quoted_pattern(pat)?;
                            self.emit("}");
                        }
                        self.emit("]");
                    }
                }
                self.emit("}");
            }
            Pattern::BitString(_) => {
                // BitString patterns are complex, emit simplified representation
                self.emit("{'bitstring', 'unsupported'}");
            }
        }
        Ok(())
    }

    /// Emit a quoted type.
    fn emit_quoted_type(&mut self, ty: &Type) -> CoreErlangResult<()> {
        match ty {
            Type::Int => self.emit("{'type', 'int'}"),
            Type::Float => self.emit("{'type', 'float'}"),
            Type::String => self.emit("{'type', 'string'}"),
            Type::Bool => self.emit("{'type', 'bool'}"),
            Type::Atom => self.emit("{'type', 'atom'}"),
            Type::Pid => self.emit("{'type', 'pid'}"),
            Type::Any => self.emit("{'type', 'any'}"),
            Type::Unit => self.emit("{'type', 'unit'}"),
            Type::Binary => self.emit("{'type', 'binary'}"),
            Type::Map => self.emit("{'type', 'map'}"),
            Type::Ref => self.emit("{'type', 'ref'}"),
            Type::Named { name, type_args } => {
                // Check if type name is an unquote marker: $UNQUOTE:varname
                if let Some(unquote_var) = name.strip_prefix("$UNQUOTE:") {
                    // Emit the variable reference directly (the variable holds the type AST)
                    self.emit(&Self::var_name(unquote_var));
                } else {
                    self.emit(&format!("{{'named', '{}', [", self.escape_erlang_atom(name)));
                    for (i, arg) in type_args.iter().enumerate() {
                        if i > 0 {
                            self.emit(", ");
                        }
                        self.emit_quoted_type(arg)?;
                    }
                    self.emit("]}");
                }
            }
            Type::List(inner) => {
                self.emit("{'list', ");
                self.emit_quoted_type(inner)?;
                self.emit("}");
            }
            Type::Tuple(elems) => {
                self.emit("{'tuple', [");
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit_quoted_type(elem)?;
                }
                self.emit("]}");
            }
            Type::Fn { params, ret } => {
                self.emit("{'fn', [");
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit_quoted_type(param)?;
                }
                self.emit("], ");
                self.emit_quoted_type(ret)?;
                self.emit("}");
            }
            Type::TypeVar(name) => {
                self.emit(&format!("{{'type_var', '{}'}}", self.escape_erlang_atom(name)));
            }
            Type::AtomLiteral(name) => {
                self.emit(&format!("{{'atom_literal', '{}'}}", self.escape_erlang_atom(name)));
            }
            Type::Union(types) => {
                self.emit("{'union', [");
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit_quoted_type(ty)?;
                }
                self.emit("]}");
            }
            Type::AssociatedType { base, name } => {
                self.emit(&format!(
                    "{{'assoc_type', '{}', '{}'}}",
                    self.escape_erlang_atom(base),
                    self.escape_erlang_atom(name)
                ));
            }
        }
        Ok(())
    }

    /// Emit a quoted item (impl, function, struct, enum, etc.) as Erlang tuple.
    fn emit_quoted_item(&mut self, item: &Item) -> CoreErlangResult<()> {
        match item {
            Item::Impl(impl_block) => {
                // Check if type name is an unquote marker: $UNQUOTE:varname
                if let Some(unquote_var) = impl_block.type_name.strip_prefix("$UNQUOTE:") {
                    // Emit as: {'impl', VarName, [methods]}
                    // where VarName is evaluated at runtime
                    self.emit("{'impl', ");
                    self.emit(&Self::var_name(unquote_var));
                    self.emit(", [");
                } else {
                    self.emit(&format!("{{'impl', '{}', [", self.escape_erlang_atom(&impl_block.type_name)));
                }
                for (i, method) in impl_block.methods.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit_quoted_function(method)?;
                }
                self.emit("]}");
            }
            Item::Function(func) => {
                self.emit_quoted_function(func)?;
            }
            Item::Struct(s) => {
                // Check if struct name is an unquote marker: $UNQUOTE:varname
                if let Some(unquote_var) = s.name.strip_prefix("$UNQUOTE:") {
                    self.emit("{'struct', ");
                    self.emit(&Self::var_name(unquote_var));
                    self.emit(", [");
                } else {
                    self.emit(&format!("{{'struct', '{}', [", self.escape_erlang_atom(&s.name)));
                }
                for (i, (field_name, field_type)) in s.fields.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    // Check if field name is an unquote marker: $UNQUOTE:varname
                    if let Some(unquote_var) = field_name.strip_prefix("$UNQUOTE:") {
                        self.emit("{");
                        self.emit(&Self::var_name(unquote_var));
                        self.emit(", ");
                    } else {
                        self.emit(&format!("{{'{}', ", self.escape_erlang_atom(field_name)));
                    }
                    self.emit_quoted_type(field_type)?;
                    self.emit("}");
                }
                self.emit("], [");
                for (i, tp) in s.type_params.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit(&format!("'{}'", self.escape_erlang_atom(&tp.name)));
                }
                self.emit("]}");
            }
            Item::Enum(e) => {
                self.emit(&format!("{{'enum', '{}', [", self.escape_erlang_atom(&e.name)));
                for (i, variant) in e.variants.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit_quoted_variant(variant)?;
                }
                self.emit("]}");
            }
            _ => {
                // For other items, emit a placeholder
                self.emit("{'item', 'unsupported'}");
            }
        }
        Ok(())
    }

    /// Emit a quoted function as Erlang tuple.
    fn emit_quoted_function(&mut self, func: &Function) -> CoreErlangResult<()> {
        // Format: {function, Name, TypeParams, Params, ReturnType, Body}
        // Check if function name is an unquote marker: $UNQUOTE:varname
        if let Some(unquote_var) = func.name.strip_prefix("$UNQUOTE:") {
            self.emit("{'function', ");
            self.emit(&Self::var_name(unquote_var));
            self.emit(", [");
        } else {
            self.emit(&format!("{{'function', '{}', [", self.escape_erlang_atom(&func.name)));
        }

        // Type params
        for (i, tp) in func.type_params.iter().enumerate() {
            if i > 0 {
                self.emit(", ");
            }
            self.emit(&format!("'{}'", self.escape_erlang_atom(&tp.name)));
        }
        self.emit("], [");

        // Params: [{pattern, type}, ...]
        for (i, param) in func.params.iter().enumerate() {
            if i > 0 {
                self.emit(", ");
            }
            self.emit("{");
            self.emit_quoted_pattern(&param.pattern)?;
            self.emit(", ");
            self.emit_quoted_type(&param.ty)?;
            self.emit("}");
        }
        self.emit("], ");

        // Return type
        if let Some(ref ret_ty) = func.return_type {
            self.emit_quoted_type(ret_ty)?;
        } else {
            self.emit("{'type', 'unit'}");
        }
        self.emit(", ");

        // Body: {stmts, expr}
        self.emit_quoted_block(&func.body)?;

        self.emit("}");
        Ok(())
    }

    /// Emit a quoted enum variant.
    fn emit_quoted_variant(&mut self, variant: &EnumVariant) -> CoreErlangResult<()> {
        match &variant.kind {
            VariantKind::Unit => {
                self.emit(&format!("{{'{}', 'unit'}}", self.escape_erlang_atom(&variant.name)));
            }
            VariantKind::Tuple(types) => {
                self.emit(&format!("{{'{}', 'tuple', [", self.escape_erlang_atom(&variant.name)));
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit_quoted_type(ty)?;
                }
                self.emit("]}");
            }
            VariantKind::Struct(fields) => {
                self.emit(&format!("{{'{}', 'struct', [", self.escape_erlang_atom(&variant.name)));
                for (i, (field_name, field_type)) in fields.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit(&format!("{{'{}', ", self.escape_erlang_atom(field_name)));
                    self.emit_quoted_type(field_type)?;
                    self.emit("}");
                }
                self.emit("]}");
            }
        }
        Ok(())
    }

    /// Convert a binary operator to an Erlang atom name.
    fn binop_to_atom(&self, op: BinOp) -> &'static str {
        match op {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "rem",
            BinOp::Eq => "==",
            BinOp::Ne => "!=",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            BinOp::And => "and",
            BinOp::Or => "or",
        }
    }

    /// Convert a unary operator to an Erlang atom name.
    fn unaryop_to_atom(&self, op: UnaryOp) -> &'static str {
        match op {
            UnaryOp::Neg => "-",
            UnaryOp::Not => "not",
        }
    }

    /// Escape a string for binary literals.
    fn escape_binary_string(&self, s: &str) -> String {
        let mut result = String::new();
        for c in s.chars() {
            match c {
                '"' => result.push_str("\\\""),
                '\\' => result.push_str("\\\\"),
                '\n' => result.push_str("\\n"),
                '\r' => result.push_str("\\r"),
                '\t' => result.push_str("\\t"),
                _ => result.push(c),
            }
        }
        result
    }

    /// Escape a string for use as an Erlang atom.
    /// Simple atoms (lowercase letter followed by alphanumerics/underscores) don't need quoting.
    /// All other atoms need single quotes with escaped special characters.
    fn escape_erlang_atom(&self, s: &str) -> String {
        // Check if it's a simple atom that doesn't need quoting
        let is_simple = !s.is_empty()
            && s.chars().next().map_or(false, |c| c.is_ascii_lowercase())
            && s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_');

        if is_simple {
            s.to_string()
        } else {
            // Need to escape single quotes and backslashes
            let mut result = String::new();
            for c in s.chars() {
                match c {
                    '\'' => result.push_str("\\'"),
                    '\\' => result.push_str("\\\\"),
                    _ => result.push(c),
                }
            }
            result
        }
    }

    /// Emit a binary operation.
    fn emit_binary_op(&mut self, op: BinOp, left: &SpannedExpr, right: &SpannedExpr) -> CoreErlangResult<()> {
        let erlang_op = match op {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "div", // Integer division in Erlang
            BinOp::Mod => "rem",
            BinOp::Eq => "=:=",  // Exact equality
            BinOp::Ne => "=/=",  // Exact inequality
            BinOp::Lt => "<",
            BinOp::Le => "=<",   // Note: Erlang uses =< not <=
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            BinOp::And => "and",
            BinOp::Or => "or",
        };

        self.emit(&format!("call 'erlang':'{}'(", erlang_op));
        self.emit_expr(left)?;
        self.emit(", ");
        self.emit_expr(right)?;
        self.emit(")");

        Ok(())
    }

    /// Emit function arguments.
    fn emit_args(&mut self, args: &[SpannedExpr]) -> CoreErlangResult<()> {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                self.emit(", ");
            }
            self.emit_expr(arg)?;
        }
        Ok(())
    }

    /// Emit a for loop expression.
    ///
    /// Side-effect loop (`for x in list { body }`):
    ///   Generates a recursive letrec that iterates through the list.
    ///
    /// List comprehension (`for x <- list { expr }`):
    ///   Uses :lists:map, :lists:flatmap, or :lists:filtermap depending on the clauses.
    fn emit_for_expr(
        &mut self,
        clauses: &[ForClause],
        body: &SpannedExpr,
        is_comprehension: bool,
    ) -> CoreErlangResult<()> {
        // Extract generators and filters
        let mut generators = Vec::new();
        let mut filters = Vec::new();

        for clause in clauses {
            match clause {
                ForClause::Generator {
                    pattern,
                    source,
                    style: _,
                } => {
                    generators.push((pattern, source));
                }
                ForClause::When(expr) => {
                    filters.push(expr);
                }
            }
        }

        if is_comprehension {
            self.emit_for_comprehension(&generators, &filters, body)?;
        } else {
            self.emit_for_side_effect(&generators, body)?;
        }

        Ok(())
    }

    /// Emit a list comprehension using :lists functions.
    fn emit_for_comprehension(
        &mut self,
        generators: &[(&Pattern, &SpannedExpr)],
        filters: &[&SpannedExpr],
        body: &SpannedExpr,
    ) -> CoreErlangResult<()> {
        if generators.is_empty() {
            return Err(CoreErlangError::new(
                "for comprehension requires at least one generator",
            ));
        }

        // Single generator case
        if generators.len() == 1 {
            let (pattern, source) = generators[0];

            if filters.is_empty() {
                // Simple map: for x <- list { expr }
                // In Core Erlang, fun must be let-bound before use in call
                // let <F> = fun (X) -> case X of <Pattern> -> Body end in call 'lists':'map'(F, List)
                let fun_var = self.fresh_var();

                self.emit(&format!("let <{fun_var}> ="));
                self.indent += 1;
                self.newline();

                // For simple identifier patterns, emit directly
                // For complex patterns (tuple, etc.), use case destructuring
                if let Pattern::Ident(name) = pattern {
                    self.emit("fun (");
                    self.emit(&Self::var_name(name));
                    self.emit(") -> ");
                    self.emit_expr(body)?;
                } else {
                    // Complex pattern - use case to destructure
                    self.emit("fun (__for_arg_0) ->");
                    self.indent += 1;
                    self.newline();
                    self.emit("case __for_arg_0 of");
                    self.indent += 1;
                    self.newline();
                    self.emit("<");
                    self.emit_pattern(pattern)?;
                    self.emit("> when 'true' ->");
                    self.indent += 1;
                    self.newline();
                    self.emit_expr(body)?;
                    self.indent -= 1;
                    self.indent -= 1;
                    self.newline();
                    self.emit("end");
                    self.indent -= 1;
                }

                self.indent -= 1;
                self.newline();
                self.emit(&format!("in call 'lists':'map'({fun_var}, "));
                self.emit_expr(source)?;
                self.emit(")");
            } else {
                // Filter + map: for x <- list, when cond { expr }
                // let <F> = fun (X) -> case Cond of ... end in call 'lists':'filtermap'(F, List)
                let fun_var = self.fresh_var();

                self.emit(&format!("let <{fun_var}> ="));
                self.indent += 1;
                self.newline();

                // For simple identifier patterns, emit directly
                // For complex patterns (tuple, etc.), use case destructuring
                if let Pattern::Ident(name) = pattern {
                    self.emit("fun (");
                    self.emit(&Self::var_name(name));
                    self.emit(") ->");
                    self.indent += 1;
                    self.newline();
                    self.emit("case ");
                } else {
                    // Complex pattern - use case to destructure first
                    self.emit("fun (__for_arg_0) ->");
                    self.indent += 1;
                    self.newline();
                    self.emit("case __for_arg_0 of");
                    self.indent += 1;
                    self.newline();
                    self.emit("<");
                    self.emit_pattern(pattern)?;
                    self.emit("> when 'true' ->");
                    self.indent += 1;
                    self.newline();
                    self.emit("case ");
                }

                // Emit combined filter condition (all filters must be true)
                if filters.len() == 1 {
                    self.emit_expr(filters[0])?;
                } else {
                    // Multiple filters: cond1 andalso cond2 andalso ...
                    for (i, filter) in filters.iter().enumerate() {
                        if i > 0 {
                            self.emit(" andalso ");
                        }
                        self.emit_expr(filter)?;
                    }
                }

                self.emit(" of");
                self.indent += 1;
                self.newline();
                self.emit("<'true'> when 'true' -> {'true', ");
                self.emit_expr(body)?;
                self.emit("}");
                self.newline();
                self.emit("<'false'> when 'true' -> 'false'");
                self.indent -= 1;
                self.newline();
                self.emit("end");

                // Close additional scopes for complex patterns
                if !matches!(pattern, Pattern::Ident(_)) {
                    self.indent -= 1;
                    self.indent -= 1;
                    self.newline();
                    self.emit("end");
                }

                self.indent -= 1;
                self.indent -= 1;
                self.newline();
                self.emit(&format!("in call 'lists':'filtermap'({fun_var}, "));
                self.emit_expr(source)?;
                self.emit(")");
            }
        } else {
            // Multiple generators: nested flatmap
            // for x <- xs, y <- ys { expr }
            // call 'lists':'flatmap'(fun (X) ->
            //     call 'lists':'map'(fun (Y) -> Body end, Ys)
            // end, Xs)
            self.emit_nested_generators(generators, filters, body, 0)?;
        }

        Ok(())
    }

    /// Emit nested generators using flatmap/map.
    fn emit_nested_generators(
        &mut self,
        generators: &[(&Pattern, &SpannedExpr)],
        filters: &[&SpannedExpr],
        body: &SpannedExpr,
        depth: usize,
    ) -> CoreErlangResult<()> {
        if depth >= generators.len() {
            // Base case: emit body with filter if present
            if filters.is_empty() {
                self.emit("[");
                self.emit_expr(body)?;
                self.emit("]");
            } else {
                // case filter of true -> [body] | false -> [] end
                self.emit("case ");
                if filters.len() == 1 {
                    self.emit_expr(filters[0])?;
                } else {
                    for (i, filter) in filters.iter().enumerate() {
                        if i > 0 {
                            self.emit(" andalso ");
                        }
                        self.emit_expr(filter)?;
                    }
                }
                self.emit(" of <'true'> when 'true' -> [");
                self.emit_expr(body)?;
                self.emit("] <'false'> when 'true' -> [] end");
            }
            return Ok(());
        }

        let (pattern, source) = generators[depth];
        let is_last = depth == generators.len() - 1;
        let fun_var = self.fresh_var();

        // Emit: let <F> = fun (...) -> ... in call 'lists':'map'(F, Source)
        self.emit(&format!("let <{fun_var}> ="));
        self.indent += 1;
        self.newline();

        // Handle simple vs complex patterns
        if let Pattern::Ident(name) = pattern {
            self.emit("fun (");
            self.emit(&Self::var_name(name));
            self.emit(") ->");
            self.indent += 1;
            self.newline();
            self.emit_nested_generators(generators, filters, body, depth + 1)?;
            self.indent -= 1;
        } else {
            // Complex pattern - use case to destructure
            let arg_name = format!("__for_arg_{}", depth);
            self.emit(&format!("fun ({arg_name}) ->"));
            self.indent += 1;
            self.newline();
            self.emit(&format!("case {arg_name} of"));
            self.indent += 1;
            self.newline();
            self.emit("<");
            self.emit_pattern(pattern)?;
            self.emit("> when 'true' ->");
            self.indent += 1;
            self.newline();
            self.emit_nested_generators(generators, filters, body, depth + 1)?;
            self.indent -= 1;
            self.indent -= 1;
            self.newline();
            self.emit("end");
            self.indent -= 1;
        }

        self.indent -= 1;
        self.newline();

        if is_last {
            // Last generator: use map
            self.emit(&format!("in call 'lists':'map'({fun_var}, "));
        } else {
            // Not last: use flatmap
            self.emit(&format!("in call 'lists':'flatmap'({fun_var}, "));
        }
        self.emit_expr(source)?;
        self.emit(")");

        Ok(())
    }

    /// Emit a side-effect for loop using letrec.
    fn emit_for_side_effect(
        &mut self,
        generators: &[(&Pattern, &SpannedExpr)],
        body: &SpannedExpr,
    ) -> CoreErlangResult<()> {
        if generators.is_empty() {
            return Err(CoreErlangError::new(
                "for loop requires at least one generator",
            ));
        }

        // For simplicity, only handle single generator for now
        if generators.len() != 1 {
            return Err(CoreErlangError::new(
                "for loops with multiple generators are not yet supported (use comprehension syntax)",
            ));
        }

        let (pattern, source) = generators[0];
        let loop_var = self.fresh_var();
        let list_var = self.fresh_var();
        let head_var = self.fresh_var();
        let tail_var = self.fresh_var();

        // ( letrec
        //     'loop'/1 = fun (List) ->
        //         case List of
        //             <[]> when 'true' -> 'ok'
        //             <[H|T]> when 'true' ->
        //                 do <body with pattern bound to H>
        //                 apply 'loop'/1(T)
        //         end
        // in apply 'loop'/1(Source)
        // -| ['letrec_goto'] )

        self.emit("( letrec");
        self.indent += 1;
        self.newline();

        // 'loop'/1 = fun (List) ->
        self.emit(&format!("'{loop_var}'/1 = fun ({list_var}) ->"));
        self.indent += 1;
        self.newline();

        // case List of
        self.emit(&format!("case {list_var} of"));
        self.indent += 1;
        self.newline();

        // <[]> when 'true' -> 'ok'
        self.emit("<[]> when 'true' -> 'ok'");
        self.newline();

        // <[H|T]> when 'true' ->
        self.emit(&format!("<[{head_var}|{tail_var}]> when 'true' ->"));
        self.indent += 1;
        self.newline();

        // Bind pattern to head_var using let or case
        // For simple variable pattern, use let
        // For complex patterns, use case
        match pattern {
            Pattern::Ident(name) => {
                // let <Name> = Head in do Body apply 'loop'/1(Tail)
                let erlang_name = Self::var_name(name);
                self.emit(&format!("let <{erlang_name}> = {head_var}"));
                self.newline();
                self.emit("in ( do ");
                self.emit_expr(body)?;
                self.newline();
                self.emit(&format!("apply '{loop_var}'/1({tail_var}) )"));
            }
            _ => {
                // case Head of <Pattern> when 'true' -> do Body apply 'loop'/1(Tail) end
                self.emit(&format!("case {head_var} of"));
                self.indent += 1;
                self.newline();
                self.emit("<");
                self.emit_pattern(pattern)?;
                self.emit("> when 'true' ->");
                self.indent += 1;
                self.newline();
                self.emit("( do ");
                self.emit_expr(body)?;
                self.newline();
                self.emit(&format!("apply '{loop_var}'/1({tail_var}) )"));
                self.indent -= 1;
                self.indent -= 1;
                self.newline();
                self.emit("end");
            }
        }

        self.indent -= 1;
        self.indent -= 1;
        self.newline();
        self.emit("end");
        self.indent -= 1;
        self.newline();

        // in apply 'loop'/1(Source)
        self.emit(&format!("in apply '{loop_var}'/1("));
        self.emit_expr(source)?;
        self.emit(")");
        self.indent -= 1;
        self.newline();
        self.emit("-| ['letrec_goto'] )");

        Ok(())
    }

    /// Emit receive using Core Erlang primops.
    ///
    /// Core Erlang doesn't have a simple `receive...end` construct.
    /// Instead, it uses a letrec with primops:
    /// - recv_peek_message() - returns {HasMessage, Message}
    /// - remove_message() - removes current message from mailbox
    /// - recv_next() - skips to next message (for selective receive)
    /// - recv_wait_timeout(Timeout) - blocks until message or timeout
    fn emit_receive_primops(
        &mut self,
        arms: &[MatchArm],
        timeout: Option<&(Box<SpannedExpr>, Block)>,
    ) -> CoreErlangResult<()> {
        // Generate unique names for this receive
        let recv_loop = self.fresh_var();
        let has_msg_var = self.fresh_var();
        let msg_var = self.fresh_var();
        let timeout_var = self.fresh_var();
        let other_var = self.fresh_var();

        // ( letrec
        self.emit("( letrec");
        self.indent += 1;
        self.newline();

        // 'recv$^N'/0 =
        self.emit(&format!("'{recv_loop}'/0 ="));
        self.indent += 1;
        self.newline();

        // fun () ->
        self.emit("fun () ->");
        self.indent += 1;
        self.newline();

        // let <HasMsg, Msg> = primop 'recv_peek_message'()
        self.emit(&format!(
            "let <{has_msg_var},{msg_var}> = primop 'recv_peek_message'()"
        ));
        self.newline();

        // in case HasMsg of
        self.emit(&format!("in case {has_msg_var} of"));
        self.indent += 1;
        self.newline();

        // <'true'> when 'true' -> (message available)
        self.emit("<'true'> when 'true' ->");
        self.indent += 1;
        self.newline();

        // case Msg of (pattern matching on the message)
        self.emit(&format!("case {msg_var} of"));
        self.indent += 1;
        self.newline();

        // Emit each arm with remove_message before the body
        for arm in arms {
            self.emit("<");
            self.emit_pattern(&arm.pattern)?;
            self.emit("> when ");

            if let Some(guard) = &arm.guard {
                self.emit_expr(guard)?;
            } else {
                self.emit("'true'");
            }

            self.emit(" ->");
            self.indent += 1;
            self.newline();

            // do primop 'remove_message'() <body>
            self.emit("do primop 'remove_message'()");
            self.newline();
            self.emit_expr(&arm.body)?;

            self.indent -= 1;
            self.newline();
        }

        // Check if any arm has a catch-all pattern (wildcard or unguarded variable)
        // If so, we don't need the recv_next fallback as it would be unreachable
        let has_catchall = arms.iter().any(|arm| {
            arm.guard.is_none()
                && matches!(arm.pattern, Pattern::Wildcard | Pattern::Ident(_))
        });

        if !has_catchall {
            // Catch-all: message doesn't match any pattern (selective receive)
            self.emit(&format!("<{other_var}> when 'true' ->"));
            self.indent += 1;
            self.newline();
            self.emit("do primop 'recv_next'()");
            self.newline();
            self.emit(&format!("apply '{recv_loop}'/0()"));
            self.indent -= 1;
            self.newline();
        }

        // end (case Msg of)
        self.indent -= 1; // back to inner case level (same as case keyword)
        self.newline();
        self.emit("end");

        // Back out of the true branch to outer case level
        self.indent -= 1; // back to outer case level (same as <'true'> pattern)
        self.newline();

        // <'false'> when 'true' -> (no message available)
        self.emit("<'false'> when 'true' ->");
        self.indent += 1;
        self.newline();

        // Determine timeout expression
        let timeout_expr = if let Some((time_expr, _)) = timeout {
            // Clone the expression to format it
            let mut timeout_emitter = CoreErlangEmitter::new();
            timeout_emitter.emit_expr(time_expr)?;
            timeout_emitter.output
        } else {
            "'infinity'".to_string()
        };

        // let <TimedOut> = primop 'recv_wait_timeout'(Timeout)
        self.emit(&format!(
            "let <{timeout_var}> = primop 'recv_wait_timeout'({timeout_expr})"
        ));
        self.newline();

        // in case TimedOut of
        self.emit(&format!("in case {timeout_var} of"));
        self.indent += 1;
        self.newline();

        // <'true'> when 'true' -> (timed out)
        self.emit("<'true'> when 'true' ->");
        self.indent += 1;
        self.newline();

        if let Some((_, after_block)) = timeout {
            // Emit the timeout body
            self.emit_block(after_block)?;
        } else {
            // No timeout specified, this case shouldn't be reached with 'infinity'
            self.emit("'true'");
        }

        self.indent -= 1;
        self.newline();

        // <'false'> when 'true' -> loop again
        self.emit("<'false'> when 'true' ->");
        self.indent += 1;
        self.newline();
        self.emit(&format!("apply '{recv_loop}'/0()"));
        self.indent -= 1;

        // end (case TimedOut of)
        self.newline();
        self.indent -= 1; // back to timeout case level
        self.emit("end");

        // Back out of the false branch to outer case level
        self.indent -= 1; // back from false branch body to outer case level
        self.newline();

        // end (case HasMsg of)
        self.emit("end");

        // Close fun () -> and letrec function definition
        // We're at outer case level (after <'false'> branch end)
        // Need to go back to function definition level for "in apply"
        self.indent -= 1; // back from outer case to in case level
        self.indent -= 1; // back from in case to fun body level
        self.indent -= 1; // back from fun body to function def level
        self.newline();

        // in apply 'recv$^N'/0()
        self.emit(&format!("in apply '{recv_loop}'/0()"));
        self.newline();
        self.emit("-| ['letrec_goto'] )");

        // Close letrec - ) already emitted above with annotation
        self.indent -= 1;

        Ok(())
    }

    /// Emit a match arm.
    fn emit_match_arm(&mut self, arm: &MatchArm) -> CoreErlangResult<()> {
        // Add pattern-bound variables to scope
        self.collect_pattern_vars(&arm.pattern);

        self.emit("<");
        self.emit_pattern(&arm.pattern)?;
        self.emit("> when ");

        if let Some(guard) = &arm.guard {
            self.emit_expr(guard)?;
        } else {
            self.emit("'true'");
        }

        self.emit(" ->");
        self.newline();
        self.indent += 1;
        self.emit_expr(&arm.body)?;
        self.indent -= 1;

        Ok(())
    }

    /// Emit a pattern.
    fn emit_pattern(&mut self, pattern: &Pattern) -> CoreErlangResult<()> {
        match pattern {
            Pattern::Wildcard => {
                self.emit("_");
            }

            Pattern::Ident(name) => {
                self.emit(&Self::var_name(name));
            }

            Pattern::Int(n) => {
                self.emit(&n.to_string());
            }

            Pattern::String(s) => {
                // Binary string pattern - emit as binary literal syntax
                // Core Erlang binary literals use: #{Seg1, Seg2, ...}#
                // Each segment is: #<Value>(Size, Unit, Type, Flags)
                self.emit("#{");
                let chars: Vec<String> = s
                    .chars()
                    .map(|c| format!("#<{}>(8, 1, 'integer', ['unsigned', 'big'])", c as u32))
                    .collect();
                self.emit(&chars.join(", "));
                self.emit("}#");
            }

            Pattern::Charlist(s) => {
                // Charlist pattern - match as list of chars
                self.emit("[");
                let chars: Vec<String> = s.chars().map(|c| (c as u32).to_string()).collect();
                self.emit(&chars.join(", "));
                self.emit("]");
            }

            Pattern::Atom(a) => {
                self.emit(&format!("'{}'", a));
            }

            Pattern::Bool(b) => {
                self.emit(if *b { "'true'" } else { "'false'" });
            }

            Pattern::Tuple(elements) => {
                // Core Erlang uses {} for tuple patterns
                self.emit("{");
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit_pattern(elem)?;
                }
                self.emit("}");
            }

            Pattern::List(elements) => {
                if elements.is_empty() {
                    self.emit("[]");
                } else {
                    self.emit("[");
                    for (i, elem) in elements.iter().enumerate() {
                        if i > 0 {
                            self.emit(", ");
                        }
                        self.emit_pattern(elem)?;
                    }
                    self.emit("]");
                }
            }

            Pattern::ListCons { head, tail } => {
                self.emit("[");
                self.emit_pattern(head)?;
                self.emit("|");
                self.emit_pattern(tail)?;
                self.emit("]");
            }

            Pattern::Struct { name, fields } => {
                // Check if this struct is marked as an Erlang record
                let record_info = self.struct_info.get(name).and_then(|info| {
                    info.record_name.as_ref().map(|r| (r.clone(), info.fields.clone()))
                });

                if let Some((record_tag, struct_fields)) = record_info {
                    // Record pattern: tuple pattern {'record_name', Pat1, Pat2, ...}
                    self.emit(&format!("{{'{}'", record_tag));
                    for (field_name, _field_ty) in &struct_fields {
                        self.emit(", ");
                        // Find the pattern for this field
                        if let Some((_, pat)) = fields.iter().find(|(n, _)| n == field_name) {
                            self.emit_pattern(pat)?;
                        } else {
                            // Field not specified in pattern - use wildcard
                            self.emit("_");
                        }
                    }
                    self.emit("}");
                } else {
                    // Regular struct pattern: map pattern with __struct__ tag
                    let (module_name, type_name) = if let Some((module, original_name)) = self.imports.get(name) {
                        (module.to_lowercase(), original_name.clone())
                    } else {
                        // Local struct - use current module name (strip dream:: prefix for stdlib)
                        let module_prefix = self.module_name.strip_prefix(Self::STDLIB_PREFIX).unwrap_or(&self.module_name);
                        (module_prefix.to_string(), name.clone())
                    };

                    self.emit("~{");
                    // Match __struct__ tag as fully qualified atom 'module::Type'
                    self.emit(&format!("'__struct__' := '{}::{}'", module_name, type_name));
                    for (field_name, pat) in fields.iter() {
                        self.emit(", ");
                        self.emit(&format!("'{}' := ", field_name));
                        self.emit_pattern(pat)?;
                    }
                    self.emit("}~");
                }
            }

            Pattern::Enum { name, variant, fields } => {
                // Special handling for Result type patterns to match Erlang conventions:
                // - Ok() or Ok(()) → 'ok'
                // - Ok(x) → {'ok', x}
                // - Err(e) → {'error', e}
                let is_result = name == "Result" || name.is_empty() && (variant == "Ok" || variant == "Err");

                if is_result {
                    match (variant.as_str(), fields) {
                        ("Ok", EnumPatternFields::Unit) => {
                            self.emit("'ok'");
                        }
                        ("Ok", EnumPatternFields::Tuple(pats)) => {
                            // Check if this is Ok(()) - should match 'ok'
                            let is_unit_ok = pats.len() == 1 && matches!(&pats[0], Pattern::Tuple(t) if t.is_empty());
                            if is_unit_ok {
                                self.emit("'ok'");
                            } else {
                                // Ok(value) matches {'ok', value}
                                self.emit("{'ok'");
                                for field in pats {
                                    self.emit(", ");
                                    self.emit_pattern(field)?;
                                }
                                self.emit("}");
                            }
                        }
                        ("Err", EnumPatternFields::Tuple(pats)) => {
                            // Err(e) matches {'error', e}
                            self.emit("{'error'");
                            for field in pats {
                                self.emit(", ");
                                self.emit_pattern(field)?;
                            }
                            self.emit("}");
                        }
                        _ => {
                            // Unknown Result variant or struct variant, use default
                            self.emit_enum_pattern_default(variant, fields)?;
                        }
                    }
                } else {
                    // Default enum pattern handling
                    self.emit_enum_pattern_default(variant, fields)?;
                }
            }

            Pattern::BitString(segments) => {
                self.emit("#{");
                for (i, seg) in segments.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit_bitstring_segment_pattern(seg)?;
                }
                self.emit("}#");
            }
        }

        Ok(())
    }

    /// Emit a bitstring segment in an expression context.
    fn emit_bitstring_segment_expr(
        &mut self,
        seg: &BitStringSegment<Box<SpannedExpr>>,
    ) -> CoreErlangResult<()> {
        // Special handling for string literals in bitstrings
        // Each character becomes a separate integer segment
        // e.g., <<"hi">> becomes #<104>(8,1,'integer',['unsigned'|['big']]), #<105>(8,1,'integer',['unsigned'|['big']])
        match seg.value.inner() {
            Expr::String(s) => {
                let chars: Vec<u32> = s.chars().map(|c| c as u32).collect();
                for (i, code) in chars.iter().enumerate() {
                    if i > 0 {
                        self.emit(", ");
                    }
                    self.emit(&format!(
                        "#<{}>(8,1,'integer',['unsigned'|['big']])",
                        code
                    ));
                }
                return Ok(());
            }
            _ => {}
        }

        self.emit("#<");
        self.emit_expr(&seg.value)?;
        self.emit(">(");

        // Size
        if let Some(size) = &seg.size {
            // size is Option<Box<Expr>>, wrap in SpannedExpr for emit_expr
            let size_spanned = SpannedExpr::unspanned((**size).clone());
            self.emit_expr(&size_spanned)?;
        } else {
            self.emit("8"); // default size
        }

        // Type specifiers
        self.emit(", ");
        self.emit("1, "); // unit = 1 bit

        let type_spec = match seg.segment_type {
            BitSegmentType::Integer => "'integer'",
            BitSegmentType::Float => "'float'",
            BitSegmentType::Binary => "'binary'",
            BitSegmentType::Utf8 => "'utf8'",
        };
        self.emit(type_spec);

        // Emit specifiers in linked list format: ['unsigned'|['big']]
        let sign_spec = match seg.signedness {
            BitSignedness::Unsigned => "'unsigned'",
            BitSignedness::Signed => "'signed'",
        };
        let endian_spec = match seg.endianness {
            BitEndianness::Big => "'big'",
            BitEndianness::Little => "'little'",
        };
        self.emit(&format!(", [{}|[{}]])", sign_spec, endian_spec));

        Ok(())
    }

    /// Emit a bitstring segment in a pattern context.
    fn emit_bitstring_segment_pattern(
        &mut self,
        seg: &BitStringSegment<Box<Pattern>>,
    ) -> CoreErlangResult<()> {
        self.emit("#<");
        self.emit_pattern(&seg.value)?;
        self.emit(">(");

        // Size
        if let Some(size) = &seg.size {
            // size is Option<Box<Expr>>, wrap in SpannedExpr for emit_expr
            let size_spanned = SpannedExpr::unspanned((**size).clone());
            self.emit_expr(&size_spanned)?;
        } else {
            match seg.segment_type {
                BitSegmentType::Binary => self.emit("'all'"),
                _ => self.emit("8"),
            }
        }

        // Type specifiers
        self.emit(", ");
        self.emit("1, "); // unit = 1 bit

        let type_spec = match seg.segment_type {
            BitSegmentType::Integer => "'integer'",
            BitSegmentType::Float => "'float'",
            BitSegmentType::Binary => "'binary'",
            BitSegmentType::Utf8 => "'utf8'",
        };
        self.emit(type_spec);

        self.emit(", [");
        let mut specs = Vec::new();
        match seg.endianness {
            BitEndianness::Big => specs.push("'big'"),
            BitEndianness::Little => specs.push("'little'"),
        }
        match seg.signedness {
            BitSignedness::Unsigned => specs.push("'unsigned'"),
            BitSignedness::Signed => specs.push("'signed'"),
        }
        self.emit(&specs.join(", "));
        self.emit("])");

        Ok(())
    }
}

impl Default for CoreErlangEmitter {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function to compile source code to Core Erlang.
pub fn emit_core_erlang(source: &str) -> CoreErlangResult<String> {
    emit_core_erlang_with_typecheck(source, false)
}

/// Compile source code to Core Erlang, optionally with type checking and annotation.
/// When typecheck is true, the module goes through type checking which performs
/// transformations like wrapping FFI Result<T, E> returns.
pub fn emit_core_erlang_with_typecheck(source: &str, typecheck: bool) -> CoreErlangResult<String> {
    use crate::compiler::parser::Parser;

    let mut parser = Parser::new(source);
    let module = parser
        .parse_module()
        .map_err(|e| CoreErlangError::new(format!("Parse error: {}", e.message)))?;

    let (module, extern_module_names, extern_function_names) = if typecheck {
        use crate::compiler::typeck::check_modules_with_metadata;
        // Type check and annotate the module, extract extern module/function name mappings
        let result = check_modules_with_metadata(&[module]);
        let (_, type_result) = result.modules.into_iter().next().unwrap();
        let module = type_result.map_err(|e| CoreErlangError::new(format!("Type error: {}", e.message)))?;
        (module, result.extern_module_names, result.extern_function_names)
    } else {
        (module, HashMap::new(), HashMap::new())
    };

    let mut emitter = CoreErlangEmitter::new();
    emitter.set_extern_module_names(extern_module_names);
    emitter.set_extern_function_names(extern_function_names);
    emitter.emit_module(&module)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_function() {
        let source = r#"
            mod test {
                pub fn add(a: int, b: int) -> int {
                    a + b
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();
        // All Dream modules are prefixed with dream:: (like Elixir uses Elixir.)
        assert!(result.contains("module 'dream::test'"));
        assert!(result.contains("'add'/2"));
        assert!(result.contains("call 'erlang':'+'"));
    }

    #[test]
    fn test_let_binding() {
        let source = r#"
            mod test {
                pub fn example() -> int {
                    let x = 1;
                    let y = 2;
                    x + y
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();
        assert!(result.contains("let <X>"));
        assert!(result.contains("let <Y>"));
        assert!(result.contains("in"));
    }

    #[test]
    fn test_match_expression() {
        let source = r#"
            mod test {
                pub fn check(n: int) -> atom {
                    match n {
                        0 => :zero,
                        _ => :other,
                    }
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();
        assert!(result.contains("case"));
        assert!(result.contains("'zero'"));
        assert!(result.contains("'other'"));
    }

    #[test]
    fn test_spawn_and_send() {
        let source = r#"
            mod test {
                pub fn start() -> pid {
                    spawn(loop())
                }

                fn loop() -> int {
                    0
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();
        assert!(result.contains("call 'erlang':'spawn'"));
    }

    #[test]
    fn test_extern_call() {
        let source = r#"
            mod test {
                pub fn abs_value(x: int) -> int {
                    :erlang::abs(x)
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();
        assert!(result.contains("call 'erlang':'abs'(X)"));
    }

    #[test]
    fn test_extern_call_lists() {
        let source = r#"
            mod test {
                pub fn rev(items: [int]) -> [int] {
                    :lists::reverse(items)
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();
        assert!(result.contains("call 'lists':'reverse'(Items)"));
    }

    #[test]
    fn test_extern_call_multiple_args() {
        let source = r#"
            mod test {
                pub fn max_val(a: int, b: int) -> int {
                    :erlang::max(a, b)
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();
        assert!(result.contains("call 'erlang':'max'(A, B)"));
    }

    #[test]
    fn test_quoted_atom_extern_call() {
        let source = r#"
            mod test {
                pub fn map_items(items: [int]) -> [int] {
                    :'Elixir.Enum'::to_list(items)
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();
        assert!(result.contains("call 'Elixir.Enum':'to_list'(Items)"));
    }

    #[test]
    fn test_result_ok_unit_compiles_to_ok_atom() {
        let source = r#"
            mod test {
                pub fn success() -> Result<(), string> {
                    Ok(())
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();
        // Ok(()) should compile to just 'ok', not {'ok', 'ok'}
        assert!(result.contains("'ok'"));
        assert!(!result.contains("{'ok'"));
    }

    #[test]
    fn test_result_ok_value_compiles_to_tuple() {
        let source = r#"
            mod test {
                pub fn get_value() -> Result<int, string> {
                    Ok(42)
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();
        // Ok(42) should compile to {'ok', 42}
        assert!(result.contains("{'ok', 42}"));
    }

    #[test]
    fn test_result_err_compiles_to_error_tuple() {
        let source = r#"
            mod test {
                pub fn fail() -> Result<int, string> {
                    Err("oops")
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();
        // Err("oops") should compile to {'error', "oops"}, not {'err', ...}
        assert!(result.contains("{'error'"));
        assert!(!result.contains("{'err'"));
    }

    #[test]
    fn test_extern_call_result_transformation() {
        // When extern function declares Result<T, E> return type,
        // the call should pass through directly since Erlang's {:ok, T} | {:error, E}
        // is already compatible with Dream's Result<T, E> representation.
        let source = r#"
            mod test {
                extern mod file {
                    fn read_file(path: String) -> Result<Binary, Atom>;
                }

                pub fn read(path: String) -> Result<Binary, Atom> {
                    :file::read_file(path)
                }
            }
        "#;

        // Use typecheck=true to trigger the FFI Result handling
        let result = emit_core_erlang_with_typecheck(source, true).unwrap();
        // Should contain the extern call directly
        assert!(result.contains("call 'file':'read_file'"), "missing file:read_file call");
        // Should NOT contain function calls to Ok/Err (which would be apply 'Ok'/1)
        assert!(!result.contains("apply 'Ok'"), "should not have apply 'Ok' call, got:\n{}", result);
        assert!(!result.contains("apply 'Err'"), "should not have apply 'Err' call, got:\n{}", result);
        // Should NOT have redundant case wrapper that matches {ok, V} -> {ok, V}
        // The extern call result passes through directly
        assert!(!result.contains("case call 'file':'read_file'"),
            "should not have redundant case wrapper around extern call, got:\n{}", result);
    }

    #[test]
    fn test_ffi_result_with_unwrap_or() {
        // Test that unwrap_or works with FFI Result that passes through directly
        let source = r#"
            mod test {
                extern mod file {
                    fn read_file(path: Any) -> Result<Binary, Atom>;
                }

                pub fn read_with_default(path: Any) -> Binary {
                    :file::read_file(path).unwrap_or(<<"default">>)
                }
            }
        "#;

        let result = emit_core_erlang_with_typecheck(source, true).unwrap();

        // The FFI call should pass through directly (Erlang {ok, V} | {error, E} is compatible)
        assert!(result.contains("call 'file':'read_file'"), "missing file:read_file call in:\n{}", result);
        // unwrap_or should be called on the result
        // The UFCS method call should resolve to result::unwrap_or
    }

    #[test]
    fn test_extern_mod_name_attribute() {
        // Test that #[name = "Elixir.Jason"] attribute maps module names in extern calls
        let source = r#"
            mod test {
                #[name = "Elixir.Jason"]
                extern mod jason {
                    fn encode(input: any) -> Result<string, any>;
                    fn decode(input: string) -> Result<any, any>;
                }

                pub fn encode_data(data: any) -> Result<string, any> {
                    :jason::encode(data)
                }
            }
        "#;

        let result = emit_core_erlang_with_typecheck(source, true).unwrap();

        // The extern call should use 'Elixir.Jason' instead of 'jason'
        assert!(
            result.contains("call 'Elixir.Jason':'encode'"),
            "Expected call 'Elixir.Jason':'encode', but got:\n{}",
            result
        );
        // Should NOT contain 'jason' as the module name
        assert!(
            !result.contains("call 'jason':'encode'"),
            "Should not have call 'jason':'encode', but got:\n{}",
            result
        );
    }

    #[test]
    fn test_extern_mod_without_name_attribute() {
        // Test that extern mods without #[name] use the Dream module name
        let source = r#"
            mod test {
                extern mod erlang {
                    fn abs(n: int) -> int;
                }

                pub fn absolute(n: int) -> int {
                    :erlang::abs(n)
                }
            }
        "#;

        let result = emit_core_erlang_with_typecheck(source, true).unwrap();

        // Without #[name] attribute, should use 'erlang' as-is
        assert!(
            result.contains("call 'erlang':'abs'"),
            "Expected call 'erlang':'abs', but got:\n{}",
            result
        );
    }

    #[test]
    fn test_qualified_call_syntax_for_extern_module() {
        // Test that module::func() syntax (without colon prefix) works for extern modules
        let source = r#"
            mod test {
                #[name = "Elixir.Jason"]
                extern mod jason {
                    fn encode(input: any) -> Result<string, any>;
                }

                pub fn encode_data(data: any) -> Result<string, any> {
                    // Using qualified call syntax without colon prefix
                    jason::encode(data)
                }
            }
        "#;

        let result = emit_core_erlang_with_typecheck(source, true).unwrap();

        // The extern call should use 'Elixir.Jason' (from #[name] attribute)
        assert!(
            result.contains("call 'Elixir.Jason':'encode'"),
            "Expected call 'Elixir.Jason':'encode', but got:\n{}",
            result
        );
        // Should NOT contain 'jason' as the module name
        assert!(
            !result.contains("call 'jason':'encode'"),
            "Should not have call 'jason':'encode', but got:\n{}",
            result
        );
    }

    #[test]
    fn test_qualified_call_syntax_without_name_attribute() {
        // Test that module::func() syntax works for extern modules without #[name]
        let source = r#"
            mod test {
                extern mod erlang {
                    fn abs(n: int) -> int;
                }

                pub fn absolute(n: int) -> int {
                    // Using qualified call syntax without colon prefix
                    erlang::abs(n)
                }
            }
        "#;

        let result = emit_core_erlang_with_typecheck(source, true).unwrap();

        // Should use 'erlang' as the module name
        assert!(
            result.contains("call 'erlang':'abs'"),
            "Expected call 'erlang':'abs', but got:\n{}",
            result
        );
    }

    #[test]
    fn test_imported_extern_function() {
        // Test that `use jason::encode; encode(data)` works
        let source = r#"
            mod test {
                #[name = "Elixir.Jason"]
                extern mod jason {
                    fn encode(input: any) -> Result<string, any>;
                }

                use jason::encode;

                pub fn encode_data(data: any) -> Result<string, any> {
                    // Using imported function name without module prefix
                    encode(data)
                }
            }
        "#;

        let result = emit_core_erlang_with_typecheck(source, true).unwrap();

        // The extern call should use 'Elixir.Jason' (from #[name] attribute)
        assert!(
            result.contains("call 'Elixir.Jason':'encode'"),
            "Expected call 'Elixir.Jason':'encode', but got:\n{}",
            result
        );
    }

    #[test]
    fn test_imported_extern_function_with_rename() {
        // Test that `use jason::encode as json_encode; json_encode(data)` works
        let source = r#"
            mod test {
                #[name = "Elixir.Jason"]
                extern mod jason {
                    fn encode(input: any) -> any;
                }

                use jason::encode as json_encode;

                pub fn encode_data(data: any) -> any {
                    // Using renamed import
                    json_encode(data)
                }
            }
        "#;

        let result = emit_core_erlang_with_typecheck(source, true).unwrap();

        // The extern call should use 'Elixir.Jason' with original function name 'encode'
        assert!(
            result.contains("call 'Elixir.Jason':'encode'"),
            "Expected call 'Elixir.Jason':'encode', but got:\n{}",
            result
        );
    }

    #[test]
    fn test_quote_unquote_function_name() {
        // Test that #func_name in quoted function emits variable reference
        let source = r#"
            mod test {
                pub fn make_getter(func_name: atom) -> any {
                    quote {
                        fn #func_name(self) -> int { 42 }
                    }
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();

        // The quoted function should emit the func_name variable (Func_name)
        // in the function name position, not a literal atom
        assert!(
            result.contains("'function', Func_name"),
            "Expected function name to be variable Func_name, got:\n{}",
            result
        );
    }

    #[test]
    fn test_quote_unquote_struct_name() {
        // Test that #name in quoted struct emits variable reference
        let source = r#"
            mod test {
                pub fn make_struct(name: atom) -> any {
                    quote {
                        struct #name {
                            value: int,
                        }
                    }
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();

        // The quoted struct should emit the name variable (Name)
        // in the struct name position, not a literal atom
        assert!(
            result.contains("'struct', Name"),
            "Expected struct name to be variable Name, got:\n{}",
            result
        );
    }

    #[test]
    fn test_quote_unquote_struct_field_name() {
        // Test that #field_name in quoted struct emits variable reference
        let source = r#"
            mod test {
                pub fn make_struct(field_name: atom) -> any {
                    quote {
                        struct Foo {
                            #field_name: int,
                        }
                    }
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();

        // The quoted struct field should emit the field_name variable (Field_name)
        // in the field name position, not a literal atom
        assert!(
            result.contains("{Field_name, {'type', 'int'}}"),
            "Expected field name to be variable Field_name, got:\n{}",
            result
        );
    }

    #[test]
    fn test_quote_unquote_return_type() {
        // Test that #ret_type in quoted function return type emits variable reference
        let source = r#"
            mod test {
                pub fn make_fn(ret_type: any) -> any {
                    quote {
                        fn foo() -> #ret_type { }
                    }
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();

        // The quoted function should emit the ret_type variable (Ret_type)
        // in the return type position, not a literal type
        // The return type should use the variable directly (since it's unquoted)
        assert!(
            result.contains("Ret_type"),
            "Expected return type to be variable Ret_type, got:\n{}",
            result
        );
        // Should NOT contain the literal marker
        assert!(
            !result.contains("'$UNQUOTE:ret_type'"),
            "Should not have literal marker, got:\n{}",
            result
        );
    }

    #[test]
    fn test_quote_unquote_splice_in_list() {
        // Test that #..items splices a list into a quoted list
        let source = r#"
            mod test {
                pub fn make_list(items: [any]) -> any {
                    quote {
                        [1, #..items, 2]
                    }
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();

        // The quoted list with splice should use lists:append
        assert!(
            result.contains("call 'lists':'append'"),
            "Expected lists:append call for splicing, got:\n{}",
            result
        );
        // Should include the Items variable for the spliced content
        assert!(
            result.contains("Items"),
            "Expected Items variable for spliced content, got:\n{}",
            result
        );
    }

    #[test]
    fn test_quote_unquote_splice_only_element() {
        // Test that #..items works as the only element in a list
        let source = r#"
            mod test {
                pub fn make_list(items: [any]) -> any {
                    quote {
                        [#..items]
                    }
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();

        // The quoted list with splice should use lists:append even with single splice
        assert!(
            result.contains("call 'lists':'append'"),
            "Expected lists:append call for splicing, got:\n{}",
            result
        );
    }

    #[test]
    fn test_quote_unquote_splice_multiple() {
        // Test that multiple #..items works in a list
        let source = r#"
            mod test {
                pub fn make_list(a: [any], b: [any]) -> any {
                    quote {
                        [#..a, 0, #..b]
                    }
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();

        // The quoted list should include both A and B variables
        assert!(
            result.contains("call 'lists':'append'"),
            "Expected lists:append call for splicing, got:\n{}",
            result
        );
        assert!(
            result.contains("A") && result.contains("B"),
            "Expected both A and B variables, got:\n{}",
            result
        );
    }

    #[test]
    fn test_stdlib_module_call_gets_dream_prefix() {
        // Stdlib modules like io, logger should be prefixed with dream::
        // NOT treated as extern calls to Erlang's io/logger modules.
        // This tests that io::println compiles to 'dream::io':'println'
        // rather than 'io':'println' (which would fail at runtime).
        let source = r#"
            mod test {
                pub fn log_message(msg: String) -> Atom {
                    io::println(msg)
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();

        // Should call dream::io, NOT bare io
        assert!(
            result.contains("call 'dream::io':'println'"),
            "Expected 'dream::io':'println' but got:\n{}",
            result
        );
        // Make sure it's NOT calling the bare Erlang io module
        assert!(
            !result.contains("call 'io':'println'"),
            "Should NOT call bare 'io':'println', got:\n{}",
            result
        );
    }

    #[test]
    fn test_stdlib_logger_call_gets_dream_prefix() {
        // Logger is also a stdlib module that should get the dream:: prefix
        let source = r#"
            mod test {
                pub fn log_info(msg: String) -> Atom {
                    logger::info(msg)
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();

        // Should call dream::logger, NOT bare logger
        assert!(
            result.contains("call 'dream::logger':'info'"),
            "Expected 'dream::logger':'info' but got:\n{}",
            result
        );
    }

    #[test]
    fn test_extern_call_with_colon_prefix_bypasses_stdlib() {
        // Direct extern calls with : prefix should call Erlang directly
        // :logger::info should call 'logger':'info', not 'dream::logger':'info'
        let source = r#"
            mod test {
                pub fn direct_log(msg: String) -> Atom {
                    :logger::info(msg)
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();

        // Should call logger directly (Erlang's logger module)
        assert!(
            result.contains("call 'logger':'info'"),
            "Expected direct 'logger':'info' call but got:\n{}",
            result
        );
        // Should NOT have dream:: prefix for direct extern calls
        assert!(
            !result.contains("call 'dream::logger':'info'"),
            "Direct extern call should NOT have dream:: prefix, got:\n{}",
            result
        );
    }

    #[test]
    fn test_stdlib_import_gets_dream_prefix() {
        // `use logger::info; info(msg)` should compile to 'dream::logger':'info'
        // just like `logger::info(msg)` does.
        let source = r#"
            mod test {
                use logger::info;

                pub fn log_message(msg: String) -> Atom {
                    info(msg)
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();

        // Should call dream::logger, NOT bare logger
        assert!(
            result.contains("call 'dream::logger':'info'"),
            "Expected 'dream::logger':'info' for stdlib import but got:\n{}",
            result
        );
        // Make sure it's NOT calling the bare Erlang logger module
        assert!(
            !result.contains("call 'logger':'info'("),
            "Should NOT call bare 'logger':'info', got:\n{}",
            result
        );
    }

    #[test]
    fn test_stdlib_group_import_gets_dream_prefix() {
        // `use io::{println, print}; println(msg)` should compile to 'dream::io':'println'
        let source = r#"
            mod test {
                use io::{println};

                pub fn log_message(msg: String) -> Atom {
                    println(msg)
                }
            }
        "#;

        let result = emit_core_erlang(source).unwrap();

        // Should call dream::io
        assert!(
            result.contains("call 'dream::io':'println'"),
            "Expected 'dream::io':'println' for stdlib group import but got:\n{}",
            result
        );
    }
}
