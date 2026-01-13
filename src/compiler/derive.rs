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
    /// User-defined macros: maps derive name to (module, function).
    user_defined: HashMap<String, (String, String)>,
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
            beam_paths: Vec::new(),
            expander: None,
        }
    }

    /// Create a new registry with BEAM paths for loading macro modules.
    pub fn with_paths(beam_paths: Vec<PathBuf>) -> Self {
        MacroRegistry {
            user_defined: HashMap::new(),
            beam_paths,
            expander: None,
        }
    }

    /// Register a user-defined macro.
    ///
    /// # Arguments
    ///
    /// * `name` - The derive name (e.g., "my_debug")
    /// * `module` - The Dream module containing the macro (e.g., "dream::macros")
    /// * `function` - The macro function name (e.g., "my_debug")
    pub fn register(&mut self, name: &str, module: &str, function: &str) {
        self.user_defined
            .insert(name.to_string(), (module.to_string(), function.to_string()));
    }

    /// Check if a derive name is a built-in derive.
    pub fn is_builtin(name: &str) -> bool {
        DeriveKind::from_name(name).is_some()
    }

    /// Check if a derive name is registered (built-in or user-defined).
    pub fn is_registered(&self, name: &str) -> bool {
        Self::is_builtin(name) || self.user_defined.contains_key(name)
    }

    /// Get the user-defined macro info for a derive name.
    pub fn get_user_defined(&self, name: &str) -> Option<(&str, &str)> {
        self.user_defined
            .get(name)
            .map(|(m, f)| (m.as_str(), f.as_str()))
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

        // Serialize the struct to Erlang term format
        let ast_term = ast_serde::struct_def_to_erlang_term(struct_def);

        // Get or create the expander
        let expander = self.get_expander()?;

        // Call the macro on BEAM
        let result = expander.expand_macro(&module, &function, &ast_term).map_err(|e| {
            DeriveError::new(format!("macro expansion failed: {}", e.message), Span::default())
        })?;

        // For now, we parse the result as an impl block
        // TODO: Implement proper deserialization from Erlang term to AST
        // For now, return empty - the full implementation will come when
        // we have complete AST deserialization
        let _ = result;
        Ok(vec![])
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

        // Serialize the enum to Erlang term format
        let ast_term = ast_serde::enum_def_to_erlang_term(enum_def);

        // Get or create the expander
        let expander = self.get_expander()?;

        // Call the macro on BEAM
        let result = expander.expand_macro(&module, &function, &ast_term).map_err(|e| {
            DeriveError::new(format!("macro expansion failed: {}", e.message), Span::default())
        })?;

        // For now, return empty - see above
        let _ = result;
        Ok(vec![])
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

/// Extract derive names from a struct's attributes.
fn get_derive_names(attrs: &[Attribute]) -> Vec<(String, Span)> {
    let mut derives = Vec::new();

    for attr in attrs {
        if attr.name == "derive" {
            if let AttributeArgs::Parenthesized(args) = &attr.args {
                for arg in args {
                    if let AttributeArg::Ident(name) = arg {
                        derives.push((name.clone(), attr.span.clone()));
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
    let derives = get_derive_names(&struct_def.attrs);
    let mut impls = Vec::new();
    let mut errors = Vec::new();

    // Track which derives we've already processed to avoid duplicates
    let mut processed = std::collections::HashSet::new();

    for (derive_name, span) in derives {
        if processed.contains(&derive_name) {
            continue;
        }
        processed.insert(derive_name.clone());

        match DeriveKind::from_name(&derive_name) {
            Some(kind) => {
                if let Some(impl_block) = generate_struct_derive(struct_def, kind) {
                    impls.push(Item::Impl(impl_block));
                }
            }
            None => {
                // Try user-defined macro if registry is available
                let mut found_user_macro = false;
                if let Some(ref mut reg) = registry {
                    if reg.get_user_defined(&derive_name).is_some() {
                        match reg.expand_user_defined_struct(&derive_name, struct_def) {
                            Ok(items) => impls.extend(items),
                            Err(err) => errors.push(err),
                        }
                        found_user_macro = true;
                    }
                }
                if !found_user_macro {
                    errors.push(DeriveError::new(
                        format!("unknown derive macro `{}`", derive_name),
                        span,
                    ));
                }
            }
        }
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
    let derives = get_derive_names(&enum_def.attrs);
    let mut impls = Vec::new();
    let mut errors = Vec::new();

    let mut processed = std::collections::HashSet::new();

    for (derive_name, span) in derives {
        if processed.contains(&derive_name) {
            continue;
        }
        processed.insert(derive_name.clone());

        match DeriveKind::from_name(&derive_name) {
            Some(kind) => {
                if let Some(impl_block) = generate_enum_derive(enum_def, kind) {
                    impls.push(Item::Impl(impl_block));
                }
            }
            None => {
                // Try user-defined macro if registry is available
                let mut found_user_macro = false;
                if let Some(ref mut reg) = registry {
                    if reg.get_user_defined(&derive_name).is_some() {
                        match reg.expand_user_defined_enum(&derive_name, enum_def) {
                            Ok(items) => impls.extend(items),
                            Err(err) => errors.push(err),
                        }
                        found_user_macro = true;
                    }
                }
                if !found_user_macro {
                    errors.push(DeriveError::new(
                        format!("unknown derive macro `{}`", derive_name),
                        span,
                    ));
                }
            }
        }
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
    let args: Vec<Expr> = fields
        .iter()
        .map(|(field_name, _)| Expr::FieldAccess {
            expr: Box::new(Expr::Ident("self".to_string())),
            field: field_name.clone(),
        })
        .collect();

    // :io_lib::format("Point { x: ~p, y: ~p }", [self.x, self.y])
    let io_lib_format = Expr::ExternCall {
        module: "io_lib".to_string(),
        function: "format".to_string(),
        args: vec![
            Expr::Charlist(format_string),
            Expr::List(args),
        ],
    };

    // :erlang::iolist_to_binary(...)
    let body_expr = Expr::ExternCall {
        module: "erlang".to_string(),
        function: "iolist_to_binary".to_string(),
        args: vec![io_lib_format],
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
            Expr::Charlist(format!("{}::~p", name)),
            Expr::List(vec![Expr::Ident("self".to_string())]),
        ],
    };

    let body_expr = Expr::ExternCall {
        module: "erlang".to_string(),
        function: "iolist_to_binary".to_string(),
        args: vec![io_lib_format],
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
    let field_inits: Vec<(String, Expr)> = fields
        .iter()
        .map(|(field_name, _)| {
            (
                field_name.clone(),
                Expr::FieldAccess {
                    expr: Box::new(Expr::Ident("self".to_string())),
                    field: field_name.clone(),
                },
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
    let field_inits: Vec<(String, Expr)> = fields
        .iter()
        .map(|(field_name, field_type)| {
            (field_name.clone(), default_value_for_type(field_type))
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
            func: Box::new(Expr::Path {
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
                    left: Box::new(Expr::FieldAccess {
                        expr: Box::new(Expr::Ident("self".to_string())),
                        field: field_name.clone(),
                    }),
                    right: Box::new(Expr::FieldAccess {
                        expr: Box::new(Expr::Ident("other".to_string())),
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
                left: Box::new(acc),
                right: Box::new(cmp),
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
        left: Box::new(Expr::Ident("self".to_string())),
        right: Box::new(Expr::Ident("other".to_string())),
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
        args: vec![Expr::Ident("self".to_string())],
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
        return_type,
        body: Block {
            stmts: vec![],
            expr: Some(Box::new(body_expr)),
        },
        is_pub: true,
        span: Span::default(),
    }
}

/// Create a `self` parameter.
fn make_self_param() -> Param {
    Param {
        pattern: Pattern::Ident("self".to_string()),
        ty: Type::Any, // Type will be inferred
    }
}

/// Create a named parameter with a type.
fn make_param(name: &str, ty: Type) -> Param {
    Param {
        pattern: Pattern::Ident(name.to_string()),
        ty,
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
                .map(|(n, t)| (n.to_string(), t))
                .collect(),
            is_pub: true,
        }
    }

    #[test]
    fn test_get_derive_names() {
        let attrs = vec![Attribute {
            name: "derive".to_string(),
            args: AttributeArgs::Parenthesized(vec![
                AttributeArg::Ident("Debug".to_string()),
                AttributeArg::Ident("Clone".to_string()),
            ]),
            span: Span::default(),
        }];

        let derives = get_derive_names(&attrs);
        assert_eq!(derives.len(), 2);
        assert_eq!(derives[0].0, "Debug");
        assert_eq!(derives[1].0, "Clone");
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
        assert!(matches!(method.return_type, Some(Type::String)));
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
        assert!(matches!(method.return_type, Some(Type::Bool)));
    }

    #[test]
    fn test_generate_hash() {
        let method = generate_hash();

        assert_eq!(method.name, "hash");
        assert_eq!(method.params.len(), 1);
        assert!(matches!(method.return_type, Some(Type::Int)));
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
        };

        let result = generate_struct_derives(&struct_def, None);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("unknown derive macro"));
    }
}
