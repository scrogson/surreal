//! Derive macro expansion for Dream.
//!
//! Supports `#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]` on structs and enums.
//! Generates impl blocks with the corresponding methods at compile time.
//!
//! Also supports user-defined macros that execute on BEAM via the MacroRegistry.

use std::collections::HashMap;
use std::path::PathBuf;

use crate::compiler::ast::*;
use crate::compiler::ast_serde;
use crate::compiler::lexer::Span;
use crate::compiler::macro_expander::MacroExpander;

/// Errors that can occur during derive expansion.
#[derive(Debug)]
pub struct DeriveError {
    pub message: String,
    pub span: Span,
}

impl DeriveError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

impl std::fmt::Display for DeriveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

/// Built-in derive kinds.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DeriveKind {
    Debug,
    Clone,
    Default,
    Eq,
    Hash,
}

impl DeriveKind {
    /// Try to parse a derive name into a DeriveKind.
    fn from_name(name: &str) -> Option<Self> {
        match name {
            "Debug" => Some(DeriveKind::Debug),
            "Clone" => Some(DeriveKind::Clone),
            "Default" => Some(DeriveKind::Default),
            "PartialEq" | "Eq" => Some(DeriveKind::Eq),
            "Hash" => Some(DeriveKind::Hash),
            _ => None,
        }
    }
}

// =============================================================================
// Macro Registry
// =============================================================================

/// A registry of both built-in and user-defined macros.
///
/// User-defined macros are Dream functions marked with `#[macro]` that take
/// AST data as input and return transformed AST.
pub struct MacroRegistry {
    /// User-defined macros in current module scope: maps derive name to (module, function).
    /// These are populated via `use` statements like `use serde::Serialize;`
    user_defined: HashMap<String, (String, String)>,
    /// Package macros: maps package name to its macros (derive_name -> (module, function)).
    /// Used for qualified paths like `#[derive(serde::Serialize)]`
    package_macros: HashMap<String, HashMap<String, (String, String)>>,
    /// BEAM paths for loading macro modules.
    beam_paths: Vec<PathBuf>,
    /// The macro expander (lazily initialized on first use).
    expander: Option<MacroExpander>,
}

impl MacroRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        MacroRegistry {
            user_defined: HashMap::new(),
            package_macros: HashMap::new(),
            beam_paths: Vec::new(),
            expander: None,
        }
    }

    /// Create a new registry with BEAM paths for loading macro modules.
    pub fn with_paths(beam_paths: Vec<PathBuf>) -> Self {
        MacroRegistry {
            user_defined: HashMap::new(),
            package_macros: HashMap::new(),
            beam_paths,
            expander: None,
        }
    }

    /// Register a user-defined macro in the current module scope.
    /// This is used for macros imported via `use` statements.
    ///
    /// # Arguments
    ///
    /// * `name` - The derive name (e.g., "Serialize")
    /// * `module` - The Dream module containing the macro (e.g., "surreal::serde::serde")
    /// * `function` - The macro function name (e.g., "serialize_derive")
    pub fn register(&mut self, name: &str, module: &str, function: &str) {
        self.user_defined
            .insert(name.to_string(), (module.to_string(), function.to_string()));
    }

    /// Register a macro from a specific package.
    /// Used for qualified path lookups like `#[derive(serde::Serialize)]`.
    pub fn register_package_macro(
        &mut self,
        package: &str,
        derive_name: &str,
        module: &str,
        function: &str,
    ) {
        self.package_macros
            .entry(package.to_string())
            .or_default()
            .insert(derive_name.to_string(), (module.to_string(), function.to_string()));
    }

    /// Check if a derive name is a built-in derive.
    pub fn is_builtin(name: &str) -> bool {
        DeriveKind::from_name(name).is_some()
    }

    /// Check if a derive name is registered (built-in or user-defined in scope).
    pub fn is_registered(&self, name: &str) -> bool {
        Self::is_builtin(name) || self.user_defined.contains_key(name)
    }

    /// Get the user-defined macro info for a derive name (unqualified, in scope).
    pub fn get_user_defined(&self, name: &str) -> Option<(&str, &str)> {
        self.user_defined
            .get(name)
            .map(|(m, f)| (m.as_str(), f.as_str()))
    }

    /// Get macro info by qualified path (e.g., `serde::Serialize`).
    /// The package is the first segment(s), name is the last segment.
    pub fn get_qualified(&self, package: &[String], name: &str) -> Option<(&str, &str)> {
        // For now, support single-segment package names (e.g., "serde")
        // TODO: support nested packages
        if package.len() == 1 {
            self.package_macros
                .get(&package[0])
                .and_then(|macros| macros.get(name))
                .map(|(m, f)| (m.as_str(), f.as_str()))
        } else {
            // Multi-segment package path - join with "::"
            let pkg_name = package.join("::");
            self.package_macros
                .get(&pkg_name)
                .and_then(|macros| macros.get(name))
                .map(|(m, f)| (m.as_str(), f.as_str()))
        }
    }

    /// Get macro info by DeriveRef (handles both qualified and unqualified).
    pub fn get_by_ref(&self, derive_ref: &DeriveRef) -> Option<(&str, &str)> {
        match derive_ref {
            DeriveRef::Name(name) => self.get_user_defined(name),
            DeriveRef::Path { package, name } => self.get_qualified(package, name),
        }
    }

    /// Expand a user-defined derive macro on a struct.
    ///
    /// Returns the generated impl blocks as Items.
    pub fn expand_user_defined_struct(
        &mut self,
        name: &str,
        struct_def: &StructDef,
    ) -> Result<Vec<Item>, DeriveError> {
        let (module, function) = self.user_defined.get(name).cloned().ok_or_else(|| {
            DeriveError::new(
                format!("unknown user-defined macro `{}`", name),
                Span::default(),
            )
        })?;

        // Serialize the struct to TokenStream format
        let ast_term = ast_serde::struct_to_token_stream(struct_def);

        // Get or create the expander
        let expander = self.get_expander()?;

        // Call the macro on BEAM
        let result = expander.expand_macro(&module, &function, &ast_term).map_err(|e| {
            DeriveError::new(format!("macro expansion failed: {}", e.message), Span::default())
        })?;

        // Parse the result as Erlang term
        let term = ast_serde::parse_term(&result).map_err(|e| {
            DeriveError::new(format!("failed to parse macro result: {}", e), Span::default())
        })?;

        // Convert to Item(s)
        // Macros can return a single item or a list of items
        match &term {
            ast_serde::Term::List(items) => {
                items.iter()
                    .map(|t| ast_serde::term_to_item(t).map_err(|e| {
                        DeriveError::new(format!("failed to convert macro result: {}", e), Span::default())
                    }))
                    .collect()
            }
            _ => {
                // Single item
                let item = ast_serde::term_to_item(&term).map_err(|e| {
                    DeriveError::new(format!("failed to convert macro result: {}", e), Span::default())
                })?;
                Ok(vec![item])
            }
        }
    }

    /// Expand a user-defined derive macro on an enum.
    pub fn expand_user_defined_enum(
        &mut self,
        name: &str,
        enum_def: &EnumDef,
    ) -> Result<Vec<Item>, DeriveError> {
        let (module, function) = self.user_defined.get(name).cloned().ok_or_else(|| {
            DeriveError::new(
                format!("unknown user-defined macro `{}`", name),
                Span::default(),
            )
        })?;

        // Serialize the enum to TokenStream format
        let ast_term = ast_serde::enum_to_token_stream(enum_def);

        // Get or create the expander
        let expander = self.get_expander()?;

        // Call the macro on BEAM
        let result = expander.expand_macro(&module, &function, &ast_term).map_err(|e| {
            DeriveError::new(format!("macro expansion failed: {}", e.message), Span::default())
        })?;

        // Parse the result as Erlang term
        let term = ast_serde::parse_term(&result).map_err(|e| {
            DeriveError::new(format!("failed to parse macro result: {}", e), Span::default())
        })?;

        // Convert to Item(s)
        match &term {
            ast_serde::Term::List(items) => {
                items.iter()
                    .map(|t| ast_serde::term_to_item(t).map_err(|e| {
                        DeriveError::new(format!("failed to convert macro result: {}", e), Span::default())
                    }))
                    .collect()
            }
            _ => {
                let item = ast_serde::term_to_item(&term).map_err(|e| {
                    DeriveError::new(format!("failed to convert macro result: {}", e), Span::default())
                })?;
                Ok(vec![item])
            }
        }
    }

    /// Expand a derive macro on a struct using DeriveRef (supports both qualified and unqualified).
    pub fn expand_struct_by_ref(
        &mut self,
        derive_ref: &DeriveRef,
        struct_def: &StructDef,
    ) -> Result<Vec<Item>, DeriveError> {
        let (module, function) = self.get_by_ref(derive_ref).ok_or_else(|| {
            DeriveError::new(
                format!("unknown derive macro `{}`", derive_ref),
                Span::default(),
            )
        })?;
        let module = module.to_string();
        let function = function.to_string();

        // Serialize the struct to TokenStream format
        let ast_term = ast_serde::struct_to_token_stream(struct_def);

        // Get or create the expander
        let expander = self.get_expander()?;

        // Call the macro on BEAM
        let result = expander.expand_macro(&module, &function, &ast_term).map_err(|e| {
            DeriveError::new(format!("macro expansion failed: {}", e.message), Span::default())
        })?;

        // Parse the result as Erlang term
        let term = ast_serde::parse_term(&result).map_err(|e| {
            DeriveError::new(format!("failed to parse macro result: {}", e), Span::default())
        })?;

        // Convert to Item(s)
        match &term {
            ast_serde::Term::List(items) => {
                items.iter()
                    .map(|t| ast_serde::term_to_item(t).map_err(|e| {
                        DeriveError::new(format!("failed to convert macro result: {}", e), Span::default())
                    }))
                    .collect()
            }
            _ => {
                let item = ast_serde::term_to_item(&term).map_err(|e| {
                    DeriveError::new(format!("failed to convert macro result: {}", e), Span::default())
                })?;
                Ok(vec![item])
            }
        }
    }

    /// Expand a derive macro on an enum using DeriveRef (supports both qualified and unqualified).
    pub fn expand_enum_by_ref(
        &mut self,
        derive_ref: &DeriveRef,
        enum_def: &EnumDef,
    ) -> Result<Vec<Item>, DeriveError> {
        let (module, function) = self.get_by_ref(derive_ref).ok_or_else(|| {
            DeriveError::new(
                format!("unknown derive macro `{}`", derive_ref),
                Span::default(),
            )
        })?;
        let module = module.to_string();
        let function = function.to_string();

        // Serialize the enum to TokenStream format
        let ast_term = ast_serde::enum_to_token_stream(enum_def);

        // Get or create the expander
        let expander = self.get_expander()?;

        // Call the macro on BEAM
        let result = expander.expand_macro(&module, &function, &ast_term).map_err(|e| {
            DeriveError::new(format!("macro expansion failed: {}", e.message), Span::default())
        })?;

        // Parse the result as Erlang term
        let term = ast_serde::parse_term(&result).map_err(|e| {
            DeriveError::new(format!("failed to parse macro result: {}", e), Span::default())
        })?;

        // Convert to Item(s)
        match &term {
            ast_serde::Term::List(items) => {
                items.iter()
                    .map(|t| ast_serde::term_to_item(t).map_err(|e| {
                        DeriveError::new(format!("failed to convert macro result: {}", e), Span::default())
                    }))
                    .collect()
            }
            _ => {
                let item = ast_serde::term_to_item(&term).map_err(|e| {
                    DeriveError::new(format!("failed to convert macro result: {}", e), Span::default())
                })?;
                Ok(vec![item])
            }
        }
    }

    /// Get or create the macro expander.
    fn get_expander(&mut self) -> Result<&mut MacroExpander, DeriveError> {
        if self.expander.is_none() {
            self.expander = Some(MacroExpander::new(self.beam_paths.clone()));
        }
        Ok(self.expander.as_mut().unwrap())
    }

    /// Shutdown the macro expander if running.
    pub fn shutdown(&mut self) {
        if let Some(ref mut expander) = self.expander {
            expander.shutdown();
        }
        self.expander = None;
    }
}

impl Default for MacroRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for MacroRegistry {
    fn drop(&mut self) {
        self.shutdown();
    }
}

/// Scan module for `use` statements that import macros from dependencies
/// and register them in the macro registry's user_defined map.
///
/// For example, `use serde::Serialize;` would check if `package_macros["serde"]["Serialize"]`
/// exists and if so, register it as `user_defined["Serialize"]`.
fn register_imported_macros(module: &Module, registry: &mut MacroRegistry) {
    for item in &module.items {
        if let Item::Use(use_decl) = item {
            process_use_tree_for_macros(&use_decl.tree, registry);
        }
    }
}

/// Process a use tree to find and register any imported macros.
fn process_use_tree_for_macros(tree: &UseTree, registry: &mut MacroRegistry) {
    match tree {
        UseTree::Path { module: mod_path, name, rename } => {
            // Check if this import refers to a macro from a known package
            // e.g., `use serde::Serialize;` -> package="serde", name="Serialize"
            if mod_path.prefix == PathPrefix::None && mod_path.segments.len() == 1 {
                let package = &mod_path.segments[0];
                // Look up in package_macros - clone to avoid borrow issues
                if let Some((module, function)) = registry.get_qualified(&[package.clone()], name) {
                    let module = module.to_string();
                    let function = function.to_string();
                    let local_name = rename.as_ref().unwrap_or(name);
                    registry.register(local_name, &module, &function);
                }
            }
        }
        UseTree::Group { module: mod_path, items } => {
            // e.g., `use serde::{Serialize, Deserialize};`
            if mod_path.prefix == PathPrefix::None && mod_path.segments.len() == 1 {
                let package = &mod_path.segments[0];
                // Collect all found macros first to avoid borrow issues
                let found_macros: Vec<_> = items.iter()
                    .filter_map(|item| {
                        registry.get_qualified(&[package.clone()], &item.name)
                            .map(|(m, f)| {
                                let local_name = item.rename.as_ref().unwrap_or(&item.name).clone();
                                (local_name, m.to_string(), f.to_string())
                            })
                    })
                    .collect();
                // Now register them
                for (local_name, module, function) in found_macros {
                    registry.register(&local_name, &module, &function);
                }
            }
        }
        UseTree::Glob { .. } => {
            // Glob imports don't work well with macros - skip
        }
    }
}

/// Expand all `#[derive(...)]` attributes in a module.
///
/// This adds generated impl blocks for each derive macro found on structs and enums.
/// Only supports built-in derives. For user-defined macros, use `expand_derives_with_registry`.
pub fn expand_derives(module: &mut Module) -> Result<(), Vec<DeriveError>> {
    let mut new_items = Vec::new();
    let mut errors = Vec::new();

    for item in &module.items {
        match item {
            Item::Struct(struct_def) => {
                match generate_struct_derives(struct_def, None) {
                    Ok(impls) => new_items.extend(impls),
                    Err(errs) => errors.extend(errs),
                }
            }
            Item::Enum(enum_def) => {
                match generate_enum_derives(enum_def, None) {
                    Ok(impls) => new_items.extend(impls),
                    Err(errs) => errors.extend(errs),
                }
            }
            _ => {}
        }
    }

    // Add generated impl blocks to the module
    module.items.extend(new_items);

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Expand all `#[derive(...)]` attributes in a module with registry support.
///
/// This version supports both built-in derives and user-defined macros.
/// User-defined macros are executed on BEAM via the MacroRegistry.
pub fn expand_derives_with_registry(
    module: &mut Module,
    registry: &mut MacroRegistry,
) -> Result<(), Vec<DeriveError>> {
    // First, process use statements to register imported macros
    register_imported_macros(module, registry);

    let mut new_items = Vec::new();
    let mut errors = Vec::new();

    for item in &module.items {
        match item {
            Item::Struct(struct_def) => {
                match generate_struct_derives(struct_def, Some(registry)) {
                    Ok(impls) => new_items.extend(impls),
                    Err(errs) => errors.extend(errs),
                }
            }
            Item::Enum(enum_def) => {
                match generate_enum_derives(enum_def, Some(registry)) {
                    Ok(impls) => new_items.extend(impls),
                    Err(errs) => errors.extend(errs),
                }
            }
            _ => {}
        }
    }

    // Add generated impl blocks to the module
    module.items.extend(new_items);

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// A derive macro reference - either a simple name or a qualified path.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DeriveRef {
    /// Simple unqualified name: `Serialize`
    Name(String),
    /// Qualified path: `serde::Serialize` -> (["serde"], "Serialize")
    Path { package: Vec<String>, name: String },
}

impl DeriveRef {
    /// Get the derive macro name (last component of path).
    pub fn name(&self) -> &str {
        match self {
            DeriveRef::Name(n) => n,
            DeriveRef::Path { name, .. } => name,
        }
    }

    /// Get the package path if this is a qualified path.
    pub fn package(&self) -> Option<&[String]> {
        match self {
            DeriveRef::Name(_) => None,
            DeriveRef::Path { package, .. } => Some(package),
        }
    }
}

impl std::fmt::Display for DeriveRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeriveRef::Name(n) => write!(f, "{}", n),
            DeriveRef::Path { package, name } => {
                write!(f, "{}::{}", package.join("::"), name)
            }
        }
    }
}

/// Extract derive references from a struct's attributes.
fn get_derive_refs(attrs: &[Attribute]) -> Vec<(DeriveRef, Span)> {
    let mut derives = Vec::new();

    for attr in attrs {
        if attr.name == "derive" {
            if let AttributeArgs::Parenthesized(args) = &attr.args {
                for arg in args {
                    match arg {
                        AttributeArg::Ident(name) => {
                            derives.push((DeriveRef::Name(name.clone()), attr.span.clone()));
                        }
                        AttributeArg::Path(segments) => {
                            if let Some((name, package)) = segments.split_last() {
                                derives.push((
                                    DeriveRef::Path {
                                        package: package.to_vec(),
                                        name: name.clone(),
                                    },
                                    attr.span.clone(),
                                ));
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    derives
}

/// Generate impl blocks for all derives on a struct.
fn generate_struct_derives(
    struct_def: &StructDef,
    mut registry: Option<&mut MacroRegistry>,
) -> Result<Vec<Item>, Vec<DeriveError>> {
    let derives = get_derive_refs(&struct_def.attrs);
    let mut impls = Vec::new();
    let mut errors = Vec::new();

    // Track which derives we've already processed to avoid duplicates
    let mut processed = std::collections::HashSet::new();

    for (derive_ref, span) in derives {
        if processed.contains(&derive_ref) {
            continue;
        }
        processed.insert(derive_ref.clone());

        // Check if it's a built-in derive (only for unqualified names)
        if let DeriveRef::Name(ref name) = derive_ref {
            if let Some(kind) = DeriveKind::from_name(name) {
                if let Some(impl_block) = generate_struct_derive(struct_def, kind) {
                    impls.push(Item::Impl(impl_block));
                }
                continue;
            }
        }

        // Try user-defined macro if registry is available
        if let Some(ref mut reg) = registry {
            if reg.get_by_ref(&derive_ref).is_some() {
                match reg.expand_struct_by_ref(&derive_ref, struct_def) {
                    Ok(items) => impls.extend(items),
                    Err(err) => errors.push(err),
                }
                continue;
            }
        }

        // Not found
        errors.push(DeriveError::new(
            format!("unknown derive macro `{}`", derive_ref),
            span,
        ));
    }

    if errors.is_empty() {
        Ok(impls)
    } else {
        Err(errors)
    }
}

/// Generate impl blocks for all derives on an enum.
fn generate_enum_derives(
    enum_def: &EnumDef,
    mut registry: Option<&mut MacroRegistry>,
) -> Result<Vec<Item>, Vec<DeriveError>> {
    let derives = get_derive_refs(&enum_def.attrs);
    let mut impls = Vec::new();
    let mut errors = Vec::new();

    let mut processed = std::collections::HashSet::new();

    for (derive_ref, span) in derives {
        if processed.contains(&derive_ref) {
            continue;
        }
        processed.insert(derive_ref.clone());

        // Check if it's a built-in derive (only for unqualified names)
        if let DeriveRef::Name(ref name) = derive_ref {
            if let Some(kind) = DeriveKind::from_name(name) {
                if let Some(impl_block) = generate_enum_derive(enum_def, kind) {
                    impls.push(Item::Impl(impl_block));
                }
                continue;
            }
        }

        // Try user-defined macro if registry is available
        if let Some(ref mut reg) = registry {
            if reg.get_by_ref(&derive_ref).is_some() {
                match reg.expand_enum_by_ref(&derive_ref, enum_def) {
                    Ok(items) => impls.extend(items),
                    Err(err) => errors.push(err),
                }
                continue;
            }
        }

        // Not found
        errors.push(DeriveError::new(
            format!("unknown derive macro `{}`", derive_ref),
            span,
        ));
    }

    if errors.is_empty() {
        Ok(impls)
    } else {
        Err(errors)
    }
}

/// Generate a single derive impl for a struct.
fn generate_struct_derive(struct_def: &StructDef, kind: DeriveKind) -> Option<ImplBlock> {
    let method = match kind {
        DeriveKind::Debug => generate_struct_debug(struct_def),
        DeriveKind::Clone => generate_struct_clone(struct_def),
        DeriveKind::Default => generate_struct_default(struct_def),
        DeriveKind::Eq => generate_struct_eq(struct_def),
        DeriveKind::Hash => generate_hash(),
    };

    Some(ImplBlock {
        type_name: struct_def.name.clone(),
        methods: vec![method],
        span: 0..0,
    })
}

/// Generate a single derive impl for an enum.
fn generate_enum_derive(enum_def: &EnumDef, kind: DeriveKind) -> Option<ImplBlock> {
    let method = match kind {
        DeriveKind::Debug => generate_enum_debug(enum_def),
        DeriveKind::Clone => generate_enum_clone(enum_def),
        DeriveKind::Default => generate_enum_default(enum_def)?,
        DeriveKind::Eq => generate_enum_eq(enum_def),
        DeriveKind::Hash => generate_hash(),
    };

    Some(ImplBlock {
        type_name: enum_def.name.clone(),
        methods: vec![method],
        span: 0..0,
    })
}

// =============================================================================
// Debug derive
// =============================================================================

/// Generate `fn debug(self) -> String` for a struct.
///
/// Produces code like:
/// ```text
/// fn debug(self) -> String {
///     :erlang::iolist_to_binary(:io_lib::format("Point { x: ~p, y: ~p }", [self.x, self.y]))
/// }
/// ```
fn generate_struct_debug(struct_def: &StructDef) -> Function {
    let name = &struct_def.name;
    let fields = &struct_def.fields;

    // Build format string: "Point { x: ~p, y: ~p }"
    let format_parts: Vec<String> = fields
        .iter()
        .map(|(field_name, _)| format!("{}: ~p", field_name))
        .collect();
    let format_string = format!("{} {{ {} }}", name, format_parts.join(", "));

    // Build args list: [self.x, self.y]
    let args: Vec<SpannedExpr> = fields
        .iter()
        .map(|(field_name, _)| SpannedExpr::unspanned(Expr::FieldAccess {
            expr: SpannedExpr::boxed(Expr::Ident("self".to_string())),
            field: field_name.clone(),
        }))
        .collect();

    // :io_lib::format("Point { x: ~p, y: ~p }", [self.x, self.y])
    let io_lib_format = Expr::ExternCall {
        module: "io_lib".to_string(),
        function: "format".to_string(),
        args: vec![
            SpannedExpr::unspanned(Expr::Charlist(format_string)),
            SpannedExpr::unspanned(Expr::List(args)),
        ],
    };

    // :erlang::iolist_to_binary(...)
    let body_expr = Expr::ExternCall {
        module: "erlang".to_string(),
        function: "iolist_to_binary".to_string(),
        args: vec![SpannedExpr::unspanned(io_lib_format)],
    };

    make_method(
        "debug",
        vec![make_self_param()],
        Some(Type::String),
        body_expr,
    )
}

/// Generate `fn debug(self) -> String` for an enum.
fn generate_enum_debug(enum_def: &EnumDef) -> Function {
    let name = &enum_def.name;

    // For enums, we use a match expression to handle each variant
    // For now, use a simple approach that just shows the term
    // More sophisticated: generate match arms for each variant

    // Simple approach: :erlang::iolist_to_binary(:io_lib::format("~p", [self]))
    let io_lib_format = Expr::ExternCall {
        module: "io_lib".to_string(),
        function: "format".to_string(),
        args: vec![
            SpannedExpr::unspanned(Expr::Charlist(format!("{}::~p", name))),
            SpannedExpr::unspanned(Expr::List(vec![SpannedExpr::unspanned(Expr::Ident("self".to_string()))])),
        ],
    };

    let body_expr = Expr::ExternCall {
        module: "erlang".to_string(),
        function: "iolist_to_binary".to_string(),
        args: vec![SpannedExpr::unspanned(io_lib_format)],
    };

    make_method(
        "debug",
        vec![make_self_param()],
        Some(Type::String),
        body_expr,
    )
}

// =============================================================================
// Clone derive
// =============================================================================

/// Generate `fn clone(self) -> Self` for a struct.
///
/// Produces code like:
/// ```text
/// fn clone(self) -> Point {
///     Point { x: self.x, y: self.y }
/// }
/// ```
fn generate_struct_clone(struct_def: &StructDef) -> Function {
    let name = &struct_def.name;
    let fields = &struct_def.fields;

    // Build struct init with field accesses
    let field_inits: Vec<(String, SpannedExpr)> = fields
        .iter()
        .map(|(field_name, _)| {
            (
                field_name.clone(),
                SpannedExpr::unspanned(Expr::FieldAccess {
                    expr: SpannedExpr::boxed(Expr::Ident("self".to_string())),
                    field: field_name.clone(),
                }),
            )
        })
        .collect();

    let body_expr = Expr::StructInit {
        name: name.clone(),
        fields: field_inits,
        base: None,
    };

    let return_type = Type::Named {
        name: name.clone(),
        type_args: vec![],
    };

    make_method(
        "clone",
        vec![make_self_param()],
        Some(return_type),
        body_expr,
    )
}

/// Generate `fn clone(self) -> Self` for an enum.
///
/// In BEAM, data is immutable, so clone is identity.
fn generate_enum_clone(enum_def: &EnumDef) -> Function {
    let name = &enum_def.name;

    // Just return self - BEAM data is immutable
    let body_expr = Expr::Ident("self".to_string());

    let return_type = Type::Named {
        name: name.clone(),
        type_args: vec![],
    };

    make_method(
        "clone",
        vec![make_self_param()],
        Some(return_type),
        body_expr,
    )
}

// =============================================================================
// Default derive
// =============================================================================

/// Generate `fn default() -> Self` for a struct.
///
/// Produces code like:
/// ```text
/// fn default() -> Point {
///     Point { x: 0, y: 0 }
/// }
/// ```
fn generate_struct_default(struct_def: &StructDef) -> Function {
    let name = &struct_def.name;
    let fields = &struct_def.fields;

    // Build struct init with default values for each field
    let field_inits: Vec<(String, SpannedExpr)> = fields
        .iter()
        .map(|(field_name, field_type)| {
            (field_name.clone(), SpannedExpr::unspanned(default_value_for_type(field_type)))
        })
        .collect();

    let body_expr = Expr::StructInit {
        name: name.clone(),
        fields: field_inits,
        base: None,
    };

    let return_type = Type::Named {
        name: name.clone(),
        type_args: vec![],
    };

    // Note: default() is an associated function, no self parameter
    make_method("default", vec![], Some(return_type), body_expr)
}

/// Generate `fn default() -> Self` for an enum.
///
/// Returns None if there's no suitable default variant (first unit variant).
fn generate_enum_default(enum_def: &EnumDef) -> Option<Function> {
    let name = &enum_def.name;

    // Find the first unit variant to use as default
    let default_variant = enum_def
        .variants
        .iter()
        .find(|v| matches!(v.kind, VariantKind::Unit))?;

    let body_expr = Expr::EnumVariant {
        type_name: Some(name.clone()),
        variant: default_variant.name.clone(),
        args: EnumVariantArgs::Unit,
    };

    let return_type = Type::Named {
        name: name.clone(),
        type_args: vec![],
    };

    Some(make_method("default", vec![], Some(return_type), body_expr))
}

/// Get the default value for a type.
fn default_value_for_type(ty: &Type) -> Expr {
    match ty {
        Type::Int => Expr::Int(0),
        // Dream doesn't have float literals, use int 0 (BEAM handles conversion)
        Type::Float => Expr::Int(0),
        Type::String => Expr::String(String::new()),
        Type::Bool => Expr::Bool(false),
        Type::List(_) => Expr::List(vec![]),
        Type::Named { name, .. } if name == "Option" => {
            Expr::EnumVariant {
                type_name: Some("Option".to_string()),
                variant: "None".to_string(),
                args: EnumVariantArgs::Unit,
            }
        }
        // For other named types, try to call their default()
        Type::Named { name, .. } => Expr::Call {
            func: SpannedExpr::boxed(Expr::Path {
                segments: vec![name.clone(), "default".to_string()],
            }),
            type_args: vec![],
            inferred_type_args: vec![],
            args: vec![],
        },
        // Fallback to unit
        _ => Expr::Tuple(vec![]),
    }
}

// =============================================================================
// PartialEq / Eq derive
// =============================================================================

/// Generate `fn eq(self, other: Self) -> bool` for a struct.
///
/// Produces code like:
/// ```text
/// fn eq(self, other: Point) -> bool {
///     self.x == other.x && self.y == other.y
/// }
/// ```
fn generate_struct_eq(struct_def: &StructDef) -> Function {
    let name = &struct_def.name;
    let fields = &struct_def.fields;

    let body_expr = if fields.is_empty() {
        // Empty struct - always equal
        Expr::Bool(true)
    } else {
        // Build chain of field comparisons: self.x == other.x && self.y == other.y
        let comparisons: Vec<Expr> = fields
            .iter()
            .map(|(field_name, _)| {
                Expr::Binary {
                    op: BinOp::Eq,
                    left: SpannedExpr::boxed(Expr::FieldAccess {
                        expr: SpannedExpr::boxed(Expr::Ident("self".to_string())),
                        field: field_name.clone(),
                    }),
                    right: SpannedExpr::boxed(Expr::FieldAccess {
                        expr: SpannedExpr::boxed(Expr::Ident("other".to_string())),
                        field: field_name.clone(),
                    }),
                }
            })
            .collect();

        // Chain with &&
        comparisons
            .into_iter()
            .reduce(|acc, cmp| Expr::Binary {
                op: BinOp::And,
                left: SpannedExpr::boxed(acc),
                right: SpannedExpr::boxed(cmp),
            })
            .unwrap_or(Expr::Bool(true))
    };

    let other_type = Type::Named {
        name: name.clone(),
        type_args: vec![],
    };

    make_method(
        "eq",
        vec![make_self_param(), make_param("other", other_type)],
        Some(Type::Bool),
        body_expr,
    )
}

/// Generate `fn eq(self, other: Self) -> bool` for an enum.
///
/// Uses Erlang's structural equality.
fn generate_enum_eq(enum_def: &EnumDef) -> Function {
    let name = &enum_def.name;

    // Use Erlang's == operator for structural equality
    let body_expr = Expr::Binary {
        op: BinOp::Eq,
        left: SpannedExpr::boxed(Expr::Ident("self".to_string())),
        right: SpannedExpr::boxed(Expr::Ident("other".to_string())),
    };

    let other_type = Type::Named {
        name: name.clone(),
        type_args: vec![],
    };

    make_method(
        "eq",
        vec![make_self_param(), make_param("other", other_type)],
        Some(Type::Bool),
        body_expr,
    )
}

// =============================================================================
// Hash derive
// =============================================================================

/// Generate `fn hash(self) -> int` using Erlang's phash2.
///
/// Produces code like:
/// ```text
/// fn hash(self) -> int {
///     :erlang::phash2(self)
/// }
/// ```
fn generate_hash() -> Function {
    let body_expr = Expr::ExternCall {
        module: "erlang".to_string(),
        function: "phash2".to_string(),
        args: vec![SpannedExpr::unspanned(Expr::Ident("self".to_string()))],
    };

    make_method(
        "hash",
        vec![make_self_param()],
        Some(Type::Int),
        body_expr,
    )
}

// =============================================================================
// AST construction helpers
// =============================================================================

/// Create a method function.
fn make_method(
    name: &str,
    params: Vec<Param>,
    return_type: Option<Type>,
    body_expr: Expr,
) -> Function {
    Function {
        attrs: vec![],
        name: name.to_string(),
        type_params: vec![],
        params,
        guard: None,
        return_type: return_type.map(SpannedType::unspanned),
        body: Block {
            stmts: vec![],
            expr: Some(SpannedExpr::boxed(body_expr)),
            span: 0..0,
        },
        is_pub: true,
        span: Span::default(),
    }
}

/// Create a `self` parameter.
fn make_self_param() -> Param {
    Param {
        pattern: Pattern::Ident("self".to_string()),
        ty: SpannedType::unspanned(Type::Any), // Type will be inferred
    }
}

/// Create a named parameter with a type.
fn make_param(name: &str, ty: Type) -> Param {
    Param {
        pattern: Pattern::Ident(name.to_string()),
        ty: SpannedType::unspanned(ty),
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn make_test_struct(name: &str, fields: Vec<(&str, Type)>) -> StructDef {
        StructDef {
            attrs: vec![Attribute {
                name: "derive".to_string(),
                args: AttributeArgs::Parenthesized(vec![AttributeArg::Ident("Debug".to_string())]),
                span: Span::default(),
            }],
            name: name.to_string(),
            type_params: vec![],
            fields: fields
                .into_iter()
                .map(|(n, t)| (n.to_string(), SpannedType::unspanned(t)))
                .collect(),
            is_pub: true,
            span: 0..0,
        }
    }

    #[test]
    fn test_get_derive_refs() {
        let attrs = vec![Attribute {
            name: "derive".to_string(),
            args: AttributeArgs::Parenthesized(vec![
                AttributeArg::Ident("Debug".to_string()),
                AttributeArg::Ident("Clone".to_string()),
            ]),
            span: Span::default(),
        }];

        let derives = get_derive_refs(&attrs);
        assert_eq!(derives.len(), 2);
        assert_eq!(derives[0].0, DeriveRef::Name("Debug".to_string()));
        assert_eq!(derives[1].0, DeriveRef::Name("Clone".to_string()));
    }

    #[test]
    fn test_get_derive_refs_with_path() {
        let attrs = vec![Attribute {
            name: "derive".to_string(),
            args: AttributeArgs::Parenthesized(vec![
                AttributeArg::Path(vec!["serde".to_string(), "Serialize".to_string()]),
                AttributeArg::Ident("Debug".to_string()),
            ]),
            span: Span::default(),
        }];

        let derives = get_derive_refs(&attrs);
        assert_eq!(derives.len(), 2);
        assert_eq!(
            derives[0].0,
            DeriveRef::Path {
                package: vec!["serde".to_string()],
                name: "Serialize".to_string()
            }
        );
        assert_eq!(derives[1].0, DeriveRef::Name("Debug".to_string()));
    }

    #[test]
    fn test_derive_kind_from_name() {
        assert_eq!(DeriveKind::from_name("Debug"), Some(DeriveKind::Debug));
        assert_eq!(DeriveKind::from_name("Clone"), Some(DeriveKind::Clone));
        assert_eq!(DeriveKind::from_name("Default"), Some(DeriveKind::Default));
        assert_eq!(DeriveKind::from_name("PartialEq"), Some(DeriveKind::Eq));
        assert_eq!(DeriveKind::from_name("Eq"), Some(DeriveKind::Eq));
        assert_eq!(DeriveKind::from_name("Hash"), Some(DeriveKind::Hash));
        assert_eq!(DeriveKind::from_name("Unknown"), None);
    }

    #[test]
    fn test_generate_struct_debug() {
        let struct_def = make_test_struct("Point", vec![("x", Type::Int), ("y", Type::Int)]);
        let method = generate_struct_debug(&struct_def);

        assert_eq!(method.name, "debug");
        assert_eq!(method.params.len(), 1);
        assert!(matches!(method.return_type, Some(ref rt) if matches!(rt.ty, Type::String)));
    }

    #[test]
    fn test_generate_struct_clone() {
        let struct_def = make_test_struct("Point", vec![("x", Type::Int), ("y", Type::Int)]);
        let method = generate_struct_clone(&struct_def);

        assert_eq!(method.name, "clone");
        assert_eq!(method.params.len(), 1);
    }

    #[test]
    fn test_generate_struct_default() {
        let struct_def = make_test_struct("Point", vec![("x", Type::Int), ("y", Type::Int)]);
        let method = generate_struct_default(&struct_def);

        assert_eq!(method.name, "default");
        assert_eq!(method.params.len(), 0); // No self parameter
    }

    #[test]
    fn test_generate_struct_eq() {
        let struct_def = make_test_struct("Point", vec![("x", Type::Int), ("y", Type::Int)]);
        let method = generate_struct_eq(&struct_def);

        assert_eq!(method.name, "eq");
        assert_eq!(method.params.len(), 2); // self and other
        assert!(matches!(method.return_type, Some(ref rt) if matches!(rt.ty, Type::Bool)));
    }

    #[test]
    fn test_generate_hash() {
        let method = generate_hash();

        assert_eq!(method.name, "hash");
        assert_eq!(method.params.len(), 1);
        assert!(matches!(method.return_type, Some(ref rt) if matches!(rt.ty, Type::Int)));
    }

    #[test]
    fn test_unknown_derive_error() {
        let struct_def = StructDef {
            attrs: vec![Attribute {
                name: "derive".to_string(),
                args: AttributeArgs::Parenthesized(vec![AttributeArg::Ident(
                    "Unknown".to_string(),
                )]),
                span: Span::default(),
            }],
            name: "Test".to_string(),
            type_params: vec![],
            fields: vec![],
            is_pub: true,
            span: 0..0,
        };

        let result = generate_struct_derives(&struct_def, None);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("unknown derive macro"));
    }
}
