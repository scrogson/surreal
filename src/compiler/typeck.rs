//! Type checker for Dream.
//!
//! Performs semantic analysis to validate types across the program.
//! The type checker runs after parsing and before code generation.

use std::collections::HashMap;

use crate::compiler::ast::{
    self, BinOp, Block, Expr, Function, ImplBlock, Item, MatchArm, Module, Pattern, Stmt,
    TypeParam, UnaryOp,
};
use crate::compiler::error::{TypeError, TypeResult};

/// Internal type representation for type checking.
/// This is separate from ast::Type to allow for inference variables.
#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    /// Primitive types
    Int,
    Float,
    String,
    Atom,
    Bool,
    Unit,
    Binary,
    Pid,

    /// Raw Erlang map (untyped)
    RawMap,

    /// Tuple type
    Tuple(Vec<Ty>),

    /// List type
    List(Box<Ty>),

    /// Named type (struct, enum) with type arguments
    Named {
        name: String,
        module: Option<String>,
        args: Vec<Ty>,
    },

    /// Function type
    Fn {
        params: Vec<Ty>,
        ret: Box<Ty>,
    },

    /// Type variable (for generics)
    Var(String),

    /// Inference variable (to be resolved during type checking)
    Infer(u32),

    /// Error type (for error recovery)
    Error,

    /// Any type (for dynamic/untyped contexts)
    Any,

    /// Associated type reference (e.g., Self::State)
    /// Will be resolved to concrete type during trait impl checking
    AssociatedType {
        base: String,
        name: String,
    },
}

impl Ty {
    /// Check if this type contains any inference variables.
    pub fn has_infer(&self) -> bool {
        match self {
            Ty::Infer(_) => true,
            Ty::Tuple(tys) => tys.iter().any(|t| t.has_infer()),
            Ty::List(t) => t.has_infer(),
            Ty::Named { args, .. } => args.iter().any(|t| t.has_infer()),
            Ty::Fn { params, ret } => {
                params.iter().any(|t| t.has_infer()) || ret.has_infer()
            }
            _ => false,
        }
    }

    /// Check if this type contains a specific inference variable (occurs check).
    pub fn has_infer_id(&self, target_id: u32) -> bool {
        match self {
            Ty::Infer(id) => *id == target_id,
            Ty::Tuple(tys) => tys.iter().any(|t| t.has_infer_id(target_id)),
            Ty::List(t) => t.has_infer_id(target_id),
            Ty::Named { args, .. } => args.iter().any(|t| t.has_infer_id(target_id)),
            Ty::Fn { params, ret } => {
                params.iter().any(|t| t.has_infer_id(target_id)) || ret.has_infer_id(target_id)
            }
            _ => false,
        }
    }

    /// Substitute type variables according to a mapping.
    pub fn substitute(&self, subst: &HashMap<String, Ty>) -> Ty {
        match self {
            Ty::Var(name) => subst.get(name).cloned().unwrap_or_else(|| self.clone()),
            Ty::Tuple(tys) => Ty::Tuple(tys.iter().map(|t| t.substitute(subst)).collect()),
            Ty::List(t) => Ty::List(Box::new(t.substitute(subst))),
            Ty::Named { name, module, args } => {
                // Check if this is a type parameter (Named with no args, matching a subst key)
                if args.is_empty() && module.is_none() && subst.contains_key(name) {
                    subst.get(name).cloned().unwrap()
                } else {
                    Ty::Named {
                        name: name.clone(),
                        module: module.clone(),
                        args: args.iter().map(|t| t.substitute(subst)).collect(),
                    }
                }
            }
            Ty::Fn { params, ret } => Ty::Fn {
                params: params.iter().map(|t| t.substitute(subst)).collect(),
                ret: Box::new(ret.substitute(subst)),
            },
            _ => self.clone(),
        }
    }
}

impl std::fmt::Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Int => write!(f, "int"),
            Ty::Float => write!(f, "float"),
            Ty::String => write!(f, "string"),
            Ty::Atom => write!(f, "atom"),
            Ty::Bool => write!(f, "bool"),
            Ty::Unit => write!(f, "()"),
            Ty::Binary => write!(f, "binary"),
            Ty::Pid => write!(f, "pid"),
            Ty::RawMap => write!(f, "map"),
            Ty::Tuple(tys) => {
                write!(f, "(")?;
                for (i, t) in tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
            Ty::List(t) => write!(f, "[{}]", t),
            Ty::Named { name, args, .. } => {
                write!(f, "{}", name)?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", arg)?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            Ty::Fn { params, ret } => {
                write!(f, "fn(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", p)?;
                }
                write!(f, ") -> {}", ret)
            }
            Ty::Var(name) => write!(f, "{}", name),
            Ty::Infer(id) => write!(f, "?{}", id),
            Ty::Error => write!(f, "<error>"),
            Ty::Any => write!(f, "any"),
            Ty::AssociatedType { base, name } => write!(f, "{}::{}", base, name),
        }
    }
}

/// Information about a struct type.
#[derive(Debug, Clone)]
pub struct StructInfo {
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub fields: Vec<(String, Ty)>,
}

/// Information about an enum type.
#[derive(Debug, Clone)]
pub struct EnumInfo {
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub variants: Vec<(String, Vec<Ty>)>,
}

/// Information about a function.
#[derive(Debug, Clone)]
pub struct FnInfo {
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub params: Vec<(String, Ty)>,
    pub ret: Ty,
}

/// Information about a trait definition.
#[derive(Debug, Clone)]
pub struct TraitInfo {
    pub name: String,
    /// Associated types declared in the trait (e.g., `type State;`)
    pub associated_types: Vec<String>,
    /// Method signatures (name, params, return type)
    pub methods: Vec<TraitMethodInfo>,
}

/// Information about a trait method signature.
#[derive(Debug, Clone)]
pub struct TraitMethodInfo {
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub params: Vec<(String, Ty)>,
    pub ret: Ty,
}

/// Information about a trait implementation.
#[derive(Debug, Clone)]
pub struct TraitImplInfo {
    /// The trait being implemented
    pub trait_name: String,
    /// The type implementing the trait
    pub type_name: String,
    /// Associated type bindings: type_name -> concrete type
    pub type_bindings: HashMap<String, Ty>,
    /// Implemented methods
    pub methods: Vec<String>,
}

/// Type environment for a scope.
#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    /// Variable bindings: name -> type
    vars: HashMap<String, Ty>,
    /// Struct definitions: name -> info
    structs: HashMap<String, StructInfo>,
    /// Enum definitions: name -> info
    enums: HashMap<String, EnumInfo>,
    /// Function signatures: name -> info
    functions: HashMap<String, FnInfo>,
    /// Impl methods: (type_name, method_name) -> FnInfo
    methods: HashMap<(String, String), FnInfo>,
    /// Type aliases (for future use)
    type_aliases: HashMap<String, Ty>,
    /// Trait definitions: trait_name -> TraitInfo
    traits: HashMap<String, TraitInfo>,
    /// Trait implementations: (trait_name, type_name) -> TraitImplInfo
    trait_impls: HashMap<(String, String), TraitImplInfo>,
    /// Module-level trait declarations: trait names this module implements
    module_traits: Vec<String>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a child scope that inherits from this environment.
    pub fn child(&self) -> Self {
        Self {
            vars: self.vars.clone(),
            structs: self.structs.clone(),
            enums: self.enums.clone(),
            functions: self.functions.clone(),
            methods: self.methods.clone(),
            type_aliases: self.type_aliases.clone(),
            traits: self.traits.clone(),
            trait_impls: self.trait_impls.clone(),
            module_traits: self.module_traits.clone(),
        }
    }

    /// Look up a variable in this scope and parent scopes.
    pub fn get_var(&self, name: &str) -> Option<&Ty> {
        self.vars.get(name)
    }

    /// Bind a variable in this scope.
    pub fn bind_var(&mut self, name: String, ty: Ty) {
        self.vars.insert(name, ty);
    }

    /// Get struct info.
    pub fn get_struct(&self, name: &str) -> Option<&StructInfo> {
        self.structs.get(name)
    }

    /// Get enum info.
    /// Handles both simple names (e.g., "Call") and module-qualified names (e.g., "counter::Call").
    pub fn get_enum(&self, name: &str) -> Option<&EnumInfo> {
        // Try exact match first
        if let Some(info) = self.enums.get(name) {
            return Some(info);
        }
        // Try looking up just the last segment for module-qualified names
        if let Some(simple_name) = name.rsplit("::").next() {
            if let Some(info) = self.enums.get(simple_name) {
                return Some(info);
            }
        }
        None
    }

    /// Get function info.
    pub fn get_function(&self, name: &str) -> Option<&FnInfo> {
        self.functions.get(name)
    }

    /// Get method info.
    pub fn get_method(&self, type_name: &str, method_name: &str) -> Option<&FnInfo> {
        self.methods.get(&(type_name.to_string(), method_name.to_string()))
    }

    /// Get trait info.
    pub fn get_trait(&self, name: &str) -> Option<&TraitInfo> {
        self.traits.get(name)
    }

    /// Get trait implementation info.
    pub fn get_trait_impl(&self, trait_name: &str, type_name: &str) -> Option<&TraitImplInfo> {
        self.trait_impls
            .get(&(trait_name.to_string(), type_name.to_string()))
    }

    /// Check if a type implements a trait.
    pub fn type_implements_trait(&self, type_name: &str, trait_name: &str) -> bool {
        self.trait_impls
            .contains_key(&(trait_name.to_string(), type_name.to_string()))
    }
}

/// The type checker.
pub struct TypeChecker {
    /// Current type environment
    env: TypeEnv,
    /// Counter for generating fresh inference variables
    infer_counter: u32,
    /// Collected errors (for error recovery)
    errors: Vec<TypeError>,
    /// Current function's return type (for checking return statements)
    current_return_type: Option<Ty>,
    /// Current function's span for error reporting
    current_function_span: Option<crate::compiler::lexer::Span>,
    /// Type variable substitutions from unification (reserved for future use)
    #[allow(dead_code)]
    substitutions: HashMap<u32, Ty>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            env: TypeEnv::new(),
            infer_counter: 0,
            errors: Vec::new(),
            current_return_type: None,
            current_function_span: None,
            substitutions: HashMap::new(),
        }
    }

    /// Generate a fresh inference variable.
    fn fresh_infer(&mut self) -> Ty {
        let id = self.infer_counter;
        self.infer_counter += 1;
        Ty::Infer(id)
    }

    /// Record an error and continue (for error recovery).
    fn error(&mut self, err: TypeError) {
        self.errors.push(err);
    }

    /// Record an error with a source span.
    fn error_with_span(
        &mut self,
        message: impl Into<String>,
        help: impl Into<String>,
        span: crate::compiler::lexer::Span,
    ) {
        let mut err = TypeError::with_span(message, span);
        err.help = Some(help.into());
        self.errors.push(err);
    }

    /// Unify two types, recording substitutions for inference variables.
    /// Returns Ok(()) if types can be unified, Err otherwise.
    fn unify(&mut self, ty1: &Ty, ty2: &Ty) -> TypeResult<()> {
        // Apply existing substitutions first
        let ty1 = self.apply_substitutions(ty1);
        let ty2 = self.apply_substitutions(ty2);

        match (&ty1, &ty2) {
            // Same types unify trivially
            _ if ty1 == ty2 => Ok(()),

            // Any/Error unify with anything
            (Ty::Any, _) | (_, Ty::Any) => Ok(()),
            (Ty::Error, _) | (_, Ty::Error) => Ok(()),

            // Inference variable unifies with anything, record substitution
            (Ty::Infer(id), other) | (other, Ty::Infer(id)) => {
                // Occurs check: don't allow Infer(id) = ... Infer(id) ...
                if !other.has_infer_id(*id) {
                    self.substitutions.insert(*id, other.clone());
                }
                Ok(())
            }

            // Type variables unify with anything (for now)
            (Ty::Var(_), _) | (_, Ty::Var(_)) => Ok(()),

            // Named types must have same name and unify args
            (
                Ty::Named { name: n1, args: a1, .. },
                Ty::Named { name: n2, args: a2, .. },
            ) => {
                if n1 != n2 {
                    return Err(TypeError::new(format!(
                        "type mismatch: {} vs {}",
                        ty1, ty2
                    )));
                }
                // Unify type arguments pairwise
                for (t1, t2) in a1.iter().zip(a2.iter()) {
                    self.unify(t1, t2)?;
                }
                Ok(())
            }

            // Tuples must have same length and unify elements
            (Ty::Tuple(tys1), Ty::Tuple(tys2)) => {
                if tys1.len() != tys2.len() {
                    return Err(TypeError::new(format!(
                        "tuple length mismatch: {} vs {}",
                        tys1.len(),
                        tys2.len()
                    )));
                }
                for (t1, t2) in tys1.iter().zip(tys2.iter()) {
                    self.unify(t1, t2)?;
                }
                Ok(())
            }

            // Lists unify element types
            (Ty::List(t1), Ty::List(t2)) => self.unify(t1, t2),

            // Function types
            (Ty::Fn { params: p1, ret: r1 }, Ty::Fn { params: p2, ret: r2 }) => {
                if p1.len() != p2.len() {
                    return Err(TypeError::new("function arity mismatch"));
                }
                for (t1, t2) in p1.iter().zip(p2.iter()) {
                    self.unify(t1, t2)?;
                }
                self.unify(r1, r2)
            }

            // Primitives must match exactly (handled by ty1 == ty2 above)
            _ => Err(TypeError::new(format!(
                "cannot unify {} with {}",
                ty1, ty2
            ))),
        }
    }

    /// Apply all recorded substitutions to a type.
    fn apply_substitutions(&self, ty: &Ty) -> Ty {
        match ty {
            Ty::Infer(id) => {
                if let Some(resolved) = self.substitutions.get(id) {
                    // Recursively apply substitutions
                    self.apply_substitutions(resolved)
                } else {
                    ty.clone()
                }
            }
            Ty::Tuple(tys) => {
                Ty::Tuple(tys.iter().map(|t| self.apply_substitutions(t)).collect())
            }
            Ty::List(t) => Ty::List(Box::new(self.apply_substitutions(t))),
            Ty::Named { name, module, args } => Ty::Named {
                name: name.clone(),
                module: module.clone(),
                args: args.iter().map(|t| self.apply_substitutions(t)).collect(),
            },
            Ty::Fn { params, ret } => Ty::Fn {
                params: params.iter().map(|t| self.apply_substitutions(t)).collect(),
                ret: Box::new(self.apply_substitutions(ret)),
            },
            // Primitives and type variables don't need substitution
            _ => ty.clone(),
        }
    }

    /// Get the type name from a Ty for trait checking.
    fn type_name_for_trait_check(&self, ty: &Ty) -> Option<String> {
        match ty {
            Ty::Named { name, .. } => Some(name.clone()),
            Ty::Int => Some("int".to_string()),
            Ty::Float => Some("float".to_string()),
            Ty::String => Some("string".to_string()),
            Ty::Bool => Some("bool".to_string()),
            Ty::Atom => Some("atom".to_string()),
            Ty::Pid => Some("pid".to_string()),
            Ty::Binary => Some("binary".to_string()),
            Ty::Unit => Some("unit".to_string()),
            Ty::Infer(id) => {
                // Look up in substitutions if available
                // For now, return None (unresolved)
                let _ = id;
                None
            }
            _ => None,
        }
    }

    /// Check if a type satisfies all trait bounds.
    /// Returns Ok if satisfied, Err with missing trait name if not.
    fn check_type_satisfies_bounds(&self, ty: &Ty, bounds: &[String]) -> Result<(), String> {
        if bounds.is_empty() {
            return Ok(());
        }

        let type_name = match self.type_name_for_trait_check(ty) {
            Some(name) => name,
            None => {
                // Type not resolved yet (inference var) - defer check
                // In a full implementation, we'd need to collect constraints
                // and check them after unification
                return Ok(());
            }
        };

        for bound in bounds {
            if !self.env.type_implements_trait(&type_name, bound) {
                return Err(bound.clone());
            }
        }
        Ok(())
    }

    /// Instantiate a generic function with fresh inference variables.
    /// Stores the original type params for later bound checking.
    fn instantiate_function(&mut self, info: &FnInfo) -> FnInfo {
        if info.type_params.is_empty() {
            return info.clone();
        }

        // Create a substitution mapping each type param name to a fresh inference var
        let mut subst = HashMap::new();
        for param in &info.type_params {
            subst.insert(param.name.clone(), self.fresh_infer());
        }

        // Note: In a full implementation, we'd need to track the mapping
        // from inference vars to their bounds for later validation.
        // For now, bounds checking happens at call sites with concrete types.

        FnInfo {
            name: info.name.clone(),
            type_params: vec![], // Instantiated function has no type params
            params: info
                .params
                .iter()
                .map(|(name, ty)| (name.clone(), ty.substitute(&subst)))
                .collect(),
            ret: info.ret.substitute(&subst),
        }
    }

    /// Instantiate a generic enum with fresh inference variables.
    fn instantiate_enum(&mut self, info: &EnumInfo) -> (EnumInfo, HashMap<String, Ty>) {
        if info.type_params.is_empty() {
            return (info.clone(), HashMap::new());
        }

        // Create a substitution mapping each type param name to a fresh inference var
        let mut subst = HashMap::new();
        for param in &info.type_params {
            subst.insert(param.name.clone(), self.fresh_infer());
        }

        let instantiated = EnumInfo {
            name: info.name.clone(),
            type_params: vec![],
            variants: info
                .variants
                .iter()
                .map(|(name, fields)| {
                    (
                        name.clone(),
                        fields.iter().map(|t| t.substitute(&subst)).collect(),
                    )
                })
                .collect(),
        };

        (instantiated, subst)
    }

    /// Convert AST type to internal type representation.
    fn ast_type_to_ty(&self, ast_ty: &ast::Type) -> Ty {
        match ast_ty {
            ast::Type::Int => Ty::Int,
            ast::Type::String => Ty::String,
            ast::Type::Atom => Ty::Atom,
            ast::Type::Bool => Ty::Bool,
            ast::Type::Unit => Ty::Unit,
            ast::Type::Binary => Ty::Binary,
            ast::Type::Pid => Ty::Pid,
            ast::Type::Map => Ty::RawMap,
            ast::Type::Tuple(tys) => {
                Ty::Tuple(tys.iter().map(|t| self.ast_type_to_ty(t)).collect())
            }
            ast::Type::List(t) => Ty::List(Box::new(self.ast_type_to_ty(t))),
            ast::Type::TypeVar(name) => Ty::Var(name.clone()),
            ast::Type::Named { name, type_args } => {
                // Check if it's a primitive type name
                match name.as_str() {
                    "int" => Ty::Int,
                    "string" => Ty::String,
                    "atom" => Ty::Atom,
                    "bool" => Ty::Bool,
                    "pid" => Ty::Pid,
                    "binary" => Ty::Binary,
                    _ => Ty::Named {
                        name: name.clone(),
                        module: None,
                        args: type_args.iter().map(|t| self.ast_type_to_ty(t)).collect(),
                    },
                }
            }
            ast::Type::Fn { params, ret } => Ty::Fn {
                params: params.iter().map(|t| self.ast_type_to_ty(t)).collect(),
                ret: Box::new(self.ast_type_to_ty(ret)),
            },
            ast::Type::AssociatedType { base, name } => Ty::AssociatedType {
                base: base.clone(),
                name: name.clone(),
            },
            ast::Type::Any => Ty::Any,
        }
    }

    /// Type check a module.
    pub fn check_module(&mut self, module: &Module) -> TypeResult<()> {
        // First pass: collect all type definitions
        self.collect_types(module)?;

        // Second pass: collect all function signatures
        self.collect_functions(module)?;

        // Third pass: type check function bodies
        for item in &module.items {
            if let Item::Function(func) = item {
                self.check_function(func)?;
            }
            if let Item::Impl(impl_block) = item {
                self.check_impl_block(impl_block)?;
            }
        }

        // Return first error if any
        if let Some(err) = self.errors.first() {
            return Err(err.clone());
        }

        Ok(())
    }

    /// First pass: collect struct and enum definitions.
    fn collect_types(&mut self, module: &Module) -> TypeResult<()> {
        for item in &module.items {
            match item {
                Item::Struct(s) => {
                    let fields = s
                        .fields
                        .iter()
                        .map(|(name, ty)| (name.clone(), self.ast_type_to_ty(ty)))
                        .collect();
                    self.env.structs.insert(
                        s.name.clone(),
                        StructInfo {
                            name: s.name.clone(),
                            type_params: s.type_params.clone(),
                            fields,
                        },
                    );
                }
                Item::Enum(e) => {
                    let variants = e
                        .variants
                        .iter()
                        .map(|v| {
                            let field_tys = v.fields.iter().map(|t| self.ast_type_to_ty(t)).collect();
                            (v.name.clone(), field_tys)
                        })
                        .collect();
                    self.env.enums.insert(
                        e.name.clone(),
                        EnumInfo {
                            name: e.name.clone(),
                            type_params: e.type_params.clone(),
                            variants,
                        },
                    );
                }
                Item::Trait(trait_def) => {
                    let methods = trait_def
                        .methods
                        .iter()
                        .map(|m| {
                            let params = m
                                .params
                                .iter()
                                .map(|p| {
                                    let name = match &p.pattern {
                                        Pattern::Ident(n) => n.clone(),
                                        _ => "_".to_string(),
                                    };
                                    (name, self.ast_type_to_ty(&p.ty))
                                })
                                .collect();
                            let ret = m
                                .return_type
                                .as_ref()
                                .map(|t| self.ast_type_to_ty(t))
                                .unwrap_or(Ty::Unit);
                            TraitMethodInfo {
                                name: m.name.clone(),
                                type_params: m.type_params.clone(),
                                params,
                                ret,
                            }
                        })
                        .collect();
                    self.env.traits.insert(
                        trait_def.name.clone(),
                        TraitInfo {
                            name: trait_def.name.clone(),
                            associated_types: trait_def.associated_types.clone(),
                            methods,
                        },
                    );
                }
                Item::TraitImpl(impl_def) => {
                    let type_bindings = impl_def
                        .type_bindings
                        .iter()
                        .map(|(name, ty)| (name.clone(), self.ast_type_to_ty(ty)))
                        .collect();
                    let methods = impl_def.methods.iter().map(|m| m.name.clone()).collect();
                    self.env.trait_impls.insert(
                        (impl_def.trait_name.clone(), impl_def.type_name.clone()),
                        TraitImplInfo {
                            trait_name: impl_def.trait_name.clone(),
                            type_name: impl_def.type_name.clone(),
                            type_bindings,
                            methods,
                        },
                    );
                }
                Item::TraitDecl(decl) => {
                    // Record that this module implements the trait
                    self.env.module_traits.push(decl.trait_name.clone());
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Second pass: collect function signatures.
    fn collect_functions(&mut self, module: &Module) -> TypeResult<()> {
        for item in &module.items {
            match item {
                Item::Function(func) => {
                    let info = self.function_to_info(func);
                    self.env.functions.insert(func.name.clone(), info);
                }
                Item::Impl(impl_block) => {
                    for method in &impl_block.methods {
                        let info = self.function_to_info(method);
                        self.env.methods.insert(
                            (impl_block.type_name.clone(), method.name.clone()),
                            info,
                        );
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Convert a function AST to FnInfo.
    fn function_to_info(&self, func: &Function) -> FnInfo {
        let params = func
            .params
            .iter()
            .map(|p| {
                let name = match &p.pattern {
                    Pattern::Ident(n) => n.clone(),
                    _ => "_".to_string(),
                };
                (name, self.ast_type_to_ty(&p.ty))
            })
            .collect();

        let ret = func
            .return_type
            .as_ref()
            .map(|t| self.ast_type_to_ty(t))
            .unwrap_or(Ty::Unit);

        FnInfo {
            name: func.name.clone(),
            type_params: func.type_params.clone(),
            params,
            ret,
        }
    }

    /// Type check a function body.
    fn check_function(&mut self, func: &Function) -> TypeResult<()> {
        // Create new scope for function
        let mut scope = self.env.child();

        // Set current function span for error reporting
        self.current_function_span = Some(func.span.clone());

        // Bind parameters
        for param in &func.params {
            if let Pattern::Ident(name) = &param.pattern {
                let ty = self.ast_type_to_ty(&param.ty);
                scope.bind_var(name.clone(), ty);
            }
        }

        // Set expected return type
        let ret_ty = func
            .return_type
            .as_ref()
            .map(|t| self.ast_type_to_ty(t))
            .unwrap_or(Ty::Unit);
        self.current_return_type = Some(ret_ty.clone());

        // Type check the body
        let old_env = std::mem::replace(&mut self.env, scope);

        // Check guard expression (if any) has type bool
        if let Some(ref guard) = func.guard {
            let guard_ty = self.infer_expr(guard)?;
            if !self.types_compatible(&guard_ty, &Ty::Bool) {
                self.error_with_span(
                    format!(
                        "guard clause in function '{}' must be boolean, found {}",
                        func.name, guard_ty
                    ),
                    format!("expected bool, found {}", guard_ty),
                    func.span.clone(),
                );
            }
        }

        let body_ty = self.check_block(&func.body)?;
        self.env = old_env;

        // Check return type matches
        if !self.types_compatible(&body_ty, &ret_ty) {
            self.error_with_span(
                format!(
                    "function '{}' returns {} but body has type {}",
                    func.name, ret_ty, body_ty
                ),
                format!("expected {}, found {}", ret_ty, body_ty),
                func.span.clone(),
            );
        }

        self.current_return_type = None;
        self.current_function_span = None;
        Ok(())
    }

    /// Type check an impl block.
    fn check_impl_block(&mut self, impl_block: &ImplBlock) -> TypeResult<()> {
        for method in &impl_block.methods {
            self.check_function(method)?;
        }
        Ok(())
    }

    /// Type check a block, returning the type of the final expression.
    fn check_block(&mut self, block: &Block) -> TypeResult<Ty> {
        // Check all statements
        for stmt in &block.stmts {
            self.check_stmt(stmt)?;
        }

        // Return type of trailing expression, or unit
        if let Some(expr) = &block.expr {
            self.infer_expr(expr)
        } else {
            Ok(Ty::Unit)
        }
    }

    /// Type check a statement.
    fn check_stmt(&mut self, stmt: &Stmt) -> TypeResult<()> {
        match stmt {
            Stmt::Let { pattern, ty, value } => {
                let value_ty = self.infer_expr(value)?;

                // If there's a type annotation, check it matches
                if let Some(ann_ty) = ty {
                    let expected = self.ast_type_to_ty(ann_ty);
                    if !self.types_compatible(&value_ty, &expected) {
                        self.error(TypeError::with_help(
                            format!("type mismatch in let binding"),
                            format!("expected {}, found {}", expected, value_ty),
                        ));
                    }
                }

                // Bind pattern variables
                self.bind_pattern(pattern, &value_ty)?;
            }
            Stmt::Expr(expr) => {
                self.infer_expr(expr)?;
            }
        }
        Ok(())
    }

    /// Bind variables from a pattern with a given type.
    fn bind_pattern(&mut self, pattern: &Pattern, ty: &Ty) -> TypeResult<()> {
        match pattern {
            Pattern::Ident(name) => {
                self.env.bind_var(name.clone(), ty.clone());
            }
            Pattern::Wildcard => {}
            Pattern::Tuple(pats) => {
                if let Ty::Tuple(tys) = ty {
                    if pats.len() == tys.len() {
                        for (p, t) in pats.iter().zip(tys.iter()) {
                            self.bind_pattern(p, t)?;
                        }
                    }
                } else {
                    // When type is Any or unknown, bind all sub-patterns as Any
                    for p in pats {
                        self.bind_pattern(p, &Ty::Any)?;
                    }
                }
            }
            Pattern::List(pats) => {
                if let Ty::List(elem_ty) = ty {
                    for p in pats {
                        self.bind_pattern(p, elem_ty)?;
                    }
                } else {
                    // When type is Any or unknown, bind all sub-patterns as Any
                    for p in pats {
                        self.bind_pattern(p, &Ty::Any)?;
                    }
                }
            }
            Pattern::ListCons { head, tail } => {
                if let Ty::List(elem_ty) = ty {
                    self.bind_pattern(head, elem_ty)?;
                    self.bind_pattern(tail, ty)?;
                } else {
                    // When type is Any or unknown, bind as Any
                    self.bind_pattern(head, &Ty::Any)?;
                    self.bind_pattern(tail, &Ty::Any)?;
                }
            }
            Pattern::Struct { name: _, fields } => {
                // For struct patterns, we'd need to look up field types
                // For now, bind all fields to Any
                for (_, p) in fields {
                    self.bind_pattern(p, &Ty::Any)?;
                }
            }
            Pattern::Enum { name: _, variant: _, fields } => {
                // For enum patterns, bind fields to Any for now
                for p in fields {
                    self.bind_pattern(p, &Ty::Any)?;
                }
            }
            Pattern::BitString(segments) => {
                // Bind variables in binary pattern segments
                for seg in segments {
                    // Each segment value is a pattern (could be Ident, Wildcard, etc.)
                    self.bind_pattern(&seg.value, &Ty::Int)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// Infer the type of an expression.
    fn infer_expr(&mut self, expr: &Expr) -> TypeResult<Ty> {
        match expr {
            // Literals
            Expr::Int(_) => Ok(Ty::Int),
            Expr::String(_) => Ok(Ty::String),
            Expr::Atom(_) => Ok(Ty::Atom),
            Expr::Bool(_) => Ok(Ty::Bool),
            Expr::Unit => Ok(Ty::Unit),

            // Variables
            Expr::Ident(name) => {
                if let Some(ty) = self.env.get_var(name) {
                    Ok(ty.clone())
                } else if self.env.get_function(name).is_some() {
                    // It's a function reference
                    Ok(Ty::Any) // TODO: proper function type
                } else {
                    self.error(TypeError::new(format!("undefined variable: {}", name)));
                    Ok(Ty::Error)
                }
            }

            // Binary operations
            Expr::Binary { op, left, right } => {
                let left_ty = self.infer_expr(left)?;
                let right_ty = self.infer_expr(right)?;
                self.check_binary_op(*op, &left_ty, &right_ty)
            }

            // Unary operations
            Expr::Unary { op, expr } => {
                let ty = self.infer_expr(expr)?;
                self.check_unary_op(*op, &ty)
            }

            // Function calls
            Expr::Call { func, args, .. } => {
                self.infer_call(func, args)
            }

            // Method calls
            Expr::MethodCall { receiver, method, args, .. } => {
                let recv_ty = self.infer_expr(receiver)?;
                self.infer_method_call(&recv_ty, method, args)
            }

            // If expression
            Expr::If { cond, then_block, else_block } => {
                let cond_ty = self.infer_expr(cond)?;
                if !self.types_compatible(&cond_ty, &Ty::Bool) {
                    self.error(TypeError::with_help(
                        "if condition must be bool",
                        format!("found {}", cond_ty),
                    ));
                }

                let then_ty = self.check_block(then_block)?;

                if let Some(else_blk) = else_block {
                    let else_ty = self.check_block(else_blk)?;
                    if !self.types_compatible(&then_ty, &else_ty) {
                        self.error(TypeError::with_help(
                            "if branches have different types",
                            format!("then: {}, else: {}", then_ty, else_ty),
                        ));
                    }
                    Ok(then_ty)
                } else {
                    Ok(Ty::Unit)
                }
            }

            // Match expression
            Expr::Match { expr, arms } => {
                let scrutinee_ty = self.infer_expr(expr)?;
                self.infer_match(&scrutinee_ty, arms)
            }

            // Block
            Expr::Block(block) => self.check_block(block),

            // Tuple
            Expr::Tuple(exprs) => {
                let tys: Vec<Ty> = exprs
                    .iter()
                    .map(|e| self.infer_expr(e))
                    .collect::<TypeResult<_>>()?;
                Ok(Ty::Tuple(tys))
            }

            // List
            Expr::List(exprs) => {
                if exprs.is_empty() {
                    Ok(Ty::List(Box::new(self.fresh_infer())))
                } else {
                    let elem_ty = self.infer_expr(&exprs[0])?;
                    for expr in &exprs[1..] {
                        let ty = self.infer_expr(expr)?;
                        if !self.types_compatible(&ty, &elem_ty) {
                            self.error(TypeError::with_help(
                                "list elements must have the same type",
                                format!("expected {}, found {}", elem_ty, ty),
                            ));
                        }
                    }
                    Ok(Ty::List(Box::new(elem_ty)))
                }
            }

            // Struct initialization
            Expr::StructInit { name, fields } => {
                if let Some(info) = self.env.get_struct(name).cloned() {
                    // Check fields
                    for (field_name, field_expr) in fields {
                        let field_ty = self.infer_expr(field_expr)?;
                        if let Some((_, expected_ty)) = info.fields.iter().find(|(n, _)| n == field_name) {
                            if !self.types_compatible(&field_ty, expected_ty) {
                                self.error(TypeError::with_help(
                                    format!("type mismatch for field '{}'", field_name),
                                    format!("expected {}, found {}", expected_ty, field_ty),
                                ));
                            }
                        } else {
                            self.error(TypeError::new(format!(
                                "struct '{}' has no field '{}'",
                                name, field_name
                            )));
                        }
                    }
                    Ok(Ty::Named {
                        name: name.clone(),
                        module: None,
                        args: vec![],
                    })
                } else {
                    self.error(TypeError::new(format!("undefined struct: {}", name)));
                    Ok(Ty::Error)
                }
            }

            // Enum variant
            Expr::EnumVariant { type_name, variant, args } => {
                let variant_as_enum = variant.clone();
                let enum_name = type_name.as_ref().unwrap_or(&variant_as_enum);
                if let Some(info) = self.env.get_enum(enum_name).cloned() {
                    // Instantiate the generic enum with fresh inference vars
                    let (instantiated, subst) = self.instantiate_enum(&info);

                    if let Some((_, expected_tys)) = instantiated.variants.iter().find(|(v, _)| v == variant) {
                        if args.len() != expected_tys.len() {
                            self.error(TypeError::new(format!(
                                "variant '{}' expects {} arguments, got {}",
                                variant,
                                expected_tys.len(),
                                args.len()
                            )));
                        }
                        // Check and unify argument types
                        for (arg, expected_ty) in args.iter().zip(expected_tys.iter()) {
                            let arg_ty = self.infer_expr(arg)?;
                            if self.unify(&arg_ty, expected_ty).is_err()
                                && !self.types_compatible(&arg_ty, expected_ty)
                            {
                                self.error(TypeError::with_help(
                                    format!("type mismatch in variant '{}'", variant),
                                    format!("expected {}, found {}", expected_ty, arg_ty),
                                ));
                            }
                        }
                    } else {
                        self.error(TypeError::new(format!(
                            "enum '{}' has no variant '{}'",
                            enum_name, variant
                        )));
                    }

                    // Build the type arguments from substitutions
                    let type_args: Vec<Ty> = info
                        .type_params
                        .iter()
                        .map(|p| {
                            subst
                                .get(&p.name)
                                .map(|t| self.apply_substitutions(t))
                                .unwrap_or(Ty::Any)
                        })
                        .collect();

                    Ok(Ty::Named {
                        name: enum_name.clone(),
                        module: None,
                        args: type_args,
                    })
                } else {
                    // Could be a variant without a type name (e.g., Some(x))
                    // Search all enums for this variant
                    for (name, info) in &self.env.enums.clone() {
                        if let Some((_, _expected_tys)) =
                            info.variants.iter().find(|(v, _)| v == variant)
                        {
                            // Found the enum - instantiate it
                            let (instantiated, subst) = self.instantiate_enum(&info);
                            let inst_expected: Vec<Ty> = instantiated
                                .variants
                                .iter()
                                .find(|(v, _)| v == variant)
                                .map(|(_, tys)| tys.clone())
                                .unwrap_or_default();

                            // Unify arguments
                            for (arg, expected_ty) in args.iter().zip(inst_expected.iter()) {
                                let arg_ty = self.infer_expr(arg)?;
                                let _ = self.unify(&arg_ty, expected_ty);
                            }

                            // Build type arguments
                            let type_args: Vec<Ty> = info
                                .type_params
                                .iter()
                                .map(|p| {
                                    subst
                                        .get(&p.name)
                                        .map(|t| self.apply_substitutions(t))
                                        .unwrap_or(Ty::Any)
                                })
                                .collect();

                            return Ok(Ty::Named {
                                name: name.clone(),
                                module: None,
                                args: type_args,
                            });
                        }
                    }
                    self.error(TypeError::new(format!("undefined variant: {}", variant)));
                    Ok(Ty::Error)
                }
            }

            // Field access
            Expr::FieldAccess { expr, field } => {
                let ty = self.infer_expr(expr)?;
                if let Ty::Named { name, .. } = &ty {
                    if let Some(info) = self.env.get_struct(name).cloned() {
                        if let Some((_, field_ty)) = info.fields.iter().find(|(n, _)| n == field) {
                            return Ok(field_ty.clone());
                        } else {
                            self.error(TypeError::new(format!(
                                "struct '{}' has no field '{}'",
                                name, field
                            )));
                        }
                    }
                }
                Ok(Ty::Any)
            }

            // Path (module::item)
            Expr::Path { segments: _ } => {
                // For now, treat paths as Any
                // TODO: proper module resolution
                Ok(Ty::Any)
            }

            // Return
            Expr::Return(opt_expr) => {
                let ret_ty = if let Some(e) = opt_expr {
                    self.infer_expr(e)?
                } else {
                    Ty::Unit
                };

                if let Some(expected) = &self.current_return_type {
                    if !self.types_compatible(&ret_ty, expected) {
                        self.error(TypeError::with_help(
                            "return type mismatch",
                            format!("expected {}, found {}", expected, ret_ty),
                        ));
                    }
                }

                Ok(Ty::Unit) // return expression itself is unit
            }

            // Spawn
            Expr::Spawn(_) | Expr::SpawnClosure(_) => Ok(Ty::Pid),

            // Closure - return function type
            Expr::Closure { params, body } => {
                // Create a child scope and bind closure parameters
                let mut scope = self.env.child();
                for param in params {
                    scope.bind_var(param.clone(), Ty::Any);
                }

                // Check body with params in scope
                let old_env = std::mem::replace(&mut self.env, scope);
                self.check_block(body)?;
                self.env = old_env;

                Ok(Ty::Fn {
                    params: params.iter().map(|_| Ty::Any).collect(),
                    ret: Box::new(Ty::Any),
                })
            }

            // Send
            Expr::Send { to, msg: _ } => {
                let to_ty = self.infer_expr(to)?;
                if !self.types_compatible(&to_ty, &Ty::Pid) {
                    self.error(TypeError::with_help(
                        "send target must be a pid",
                        format!("found {}", to_ty),
                    ));
                }
                Ok(Ty::Any) // send returns the message
            }

            // Receive
            Expr::Receive { arms, timeout } => {
                // For receive, check each arm with pattern variables bound
                for arm in arms {
                    // Create new scope for arm
                    let mut scope = self.env.child();
                    std::mem::swap(&mut self.env, &mut scope);

                    // Bind pattern variables (message type is unknown, use Any)
                    self.bind_pattern(&arm.pattern, &Ty::Any)?;

                    // Check guard if present
                    if let Some(guard) = &arm.guard {
                        let guard_ty = self.infer_expr(guard)?;
                        if !self.types_compatible(&guard_ty, &Ty::Bool) {
                            self.error(TypeError::with_help(
                                "receive guard must be bool",
                                format!("found {}", guard_ty),
                            ));
                        }
                    }

                    // Infer body type
                    self.infer_expr(&arm.body)?;

                    std::mem::swap(&mut self.env, &mut scope);
                }

                // Check timeout block if present
                if let Some((timeout_expr, timeout_block)) = timeout {
                    self.infer_expr(timeout_expr)?;
                    self.check_block(timeout_block)?;
                }

                Ok(Ty::Any)
            }

            // Pipe
            Expr::Pipe { left, right } => {
                // left |> right(args) becomes right(left, args)
                let _left_ty = self.infer_expr(left)?;
                self.infer_expr(right)
            }

            // External call
            Expr::ExternCall { module: _, function: _, args: _ } => {
                // External calls are untyped
                Ok(Ty::Any)
            }

            // Bit strings
            Expr::BitString(_) => Ok(Ty::Binary),
        }
    }

    /// Check a binary operation and return the result type.
    fn check_binary_op(&mut self, op: BinOp, left: &Ty, right: &Ty) -> TypeResult<Ty> {
        match op {
            // Arithmetic: int -> int -> int
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                if !self.types_compatible(left, &Ty::Int) {
                    self.error(TypeError::with_help(
                        format!("operator {} requires int operands", op),
                        format!("left operand is {}", left),
                    ));
                }
                if !self.types_compatible(right, &Ty::Int) {
                    self.error(TypeError::with_help(
                        format!("operator {} requires int operands", op),
                        format!("right operand is {}", right),
                    ));
                }
                Ok(Ty::Int)
            }

            // Comparison: T -> T -> bool
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                if !self.types_compatible(left, right) {
                    self.error(TypeError::with_help(
                        "comparison operands must have the same type",
                        format!("left: {}, right: {}", left, right),
                    ));
                }
                Ok(Ty::Bool)
            }

            // Logical: bool -> bool -> bool
            BinOp::And | BinOp::Or => {
                if !self.types_compatible(left, &Ty::Bool) {
                    self.error(TypeError::with_help(
                        format!("operator {} requires bool operands", op),
                        format!("left operand is {}", left),
                    ));
                }
                if !self.types_compatible(right, &Ty::Bool) {
                    self.error(TypeError::with_help(
                        format!("operator {} requires bool operands", op),
                        format!("right operand is {}", right),
                    ));
                }
                Ok(Ty::Bool)
            }
        }
    }

    /// Check a unary operation and return the result type.
    fn check_unary_op(&mut self, op: UnaryOp, ty: &Ty) -> TypeResult<Ty> {
        match op {
            UnaryOp::Neg => {
                if !self.types_compatible(ty, &Ty::Int) {
                    self.error(TypeError::with_help(
                        "negation requires int operand",
                        format!("found {}", ty),
                    ));
                }
                Ok(Ty::Int)
            }
            UnaryOp::Not => {
                if !self.types_compatible(ty, &Ty::Bool) {
                    self.error(TypeError::with_help(
                        "logical not requires bool operand",
                        format!("found {}", ty),
                    ));
                }
                Ok(Ty::Bool)
            }
        }
    }

    /// Infer type of a function call.
    fn infer_call(&mut self, func: &Expr, args: &[Expr]) -> TypeResult<Ty> {
        match func {
            Expr::Ident(name) => {
                if let Some(info) = self.env.get_function(name).cloned() {
                    // Instantiate generic function with fresh inference variables
                    let instantiated = self.instantiate_function(&info);

                    // Check argument count
                    if args.len() != instantiated.params.len() {
                        self.error(TypeError::new(format!(
                            "function '{}' expects {} arguments, got {}",
                            name,
                            instantiated.params.len(),
                            args.len()
                        )));
                    }

                    // Check argument types and unify
                    for (arg, (_, param_ty)) in args.iter().zip(instantiated.params.iter()) {
                        let arg_ty = self.infer_expr(arg)?;
                        // Try to unify, fall back to compatibility check
                        if self.unify(&arg_ty, param_ty).is_err()
                            && !self.types_compatible(&arg_ty, param_ty)
                        {
                            self.error(TypeError::with_help(
                                format!("type mismatch in call to '{}'", name),
                                format!("expected {}, found {}", param_ty, arg_ty),
                            ));
                        }
                    }

                    // Apply substitutions to return type
                    Ok(self.apply_substitutions(&instantiated.ret))
                } else {
                    // Unknown function - could be external
                    for arg in args {
                        self.infer_expr(arg)?;
                    }
                    Ok(Ty::Any)
                }
            }
            Expr::Path { segments: _ } => {
                // Module::function call
                // For now, treat as untyped
                for arg in args {
                    self.infer_expr(arg)?;
                }
                Ok(Ty::Any)
            }
            _ => {
                // Dynamic call
                self.infer_expr(func)?;
                for arg in args {
                    self.infer_expr(arg)?;
                }
                Ok(Ty::Any)
            }
        }
    }

    /// Infer type of a method call.
    fn infer_method_call(&mut self, recv_ty: &Ty, method: &str, args: &[Expr]) -> TypeResult<Ty> {
        if let Ty::Named { name, .. } = recv_ty {
            if let Some(info) = self.env.get_method(name, method).cloned() {
                // Instantiate generic method with fresh inference variables
                let instantiated = self.instantiate_function(&info);

                // Check argument count (excluding self)
                let expected_args = instantiated.params.len().saturating_sub(1);
                if args.len() != expected_args {
                    self.error(TypeError::new(format!(
                        "method '{}' expects {} arguments, got {}",
                        method, expected_args, args.len()
                    )));
                }

                // Check argument types and unify
                for (arg, (_, param_ty)) in args.iter().zip(instantiated.params.iter().skip(1)) {
                    let arg_ty = self.infer_expr(arg)?;
                    if self.unify(&arg_ty, param_ty).is_err()
                        && !self.types_compatible(&arg_ty, param_ty)
                    {
                        self.error(TypeError::with_help(
                            format!("type mismatch in method call '{}'", method),
                            format!("expected {}, found {}", param_ty, arg_ty),
                        ));
                    }
                }

                // Apply substitutions to return type
                return Ok(self.apply_substitutions(&instantiated.ret));
            }
        }

        // Unknown method - check args and return Any
        for arg in args {
            self.infer_expr(arg)?;
        }
        Ok(Ty::Any)
    }

    /// Infer type of a match expression.
    fn infer_match(&mut self, scrutinee_ty: &Ty, arms: &[MatchArm]) -> TypeResult<Ty> {
        if arms.is_empty() {
            return Ok(Ty::Unit);
        }

        let mut result_ty: Option<Ty> = None;

        for arm in arms {
            // Create new scope for arm
            let mut scope = self.env.child();
            std::mem::swap(&mut self.env, &mut scope);

            // Bind pattern variables
            self.bind_pattern(&arm.pattern, scrutinee_ty)?;

            // Check guard if present
            if let Some(guard) = &arm.guard {
                let guard_ty = self.infer_expr(guard)?;
                if !self.types_compatible(&guard_ty, &Ty::Bool) {
                    self.error(TypeError::with_help(
                        "match guard must be bool",
                        format!("found {}", guard_ty),
                    ));
                }
            }

            // Infer body type
            let body_ty = self.infer_expr(&arm.body)?;

            std::mem::swap(&mut self.env, &mut scope);

            // Check all arms have compatible types
            if let Some(ref expected) = result_ty {
                if !self.types_compatible(&body_ty, expected) {
                    self.error(TypeError::with_help(
                        "match arms have different types",
                        format!("expected {}, found {}", expected, body_ty),
                    ));
                }
            } else {
                result_ty = Some(body_ty);
            }
        }

        Ok(result_ty.unwrap_or(Ty::Unit))
    }

    /// Check if two types are compatible (for assignment, comparison, etc.).
    fn types_compatible(&self, ty1: &Ty, ty2: &Ty) -> bool {
        // Any is compatible with everything
        if matches!(ty1, Ty::Any) || matches!(ty2, Ty::Any) {
            return true;
        }

        // Error type is compatible with everything (for error recovery)
        if matches!(ty1, Ty::Error) || matches!(ty2, Ty::Error) {
            return true;
        }

        // Type variables are compatible with anything (for now)
        if matches!(ty1, Ty::Var(_)) || matches!(ty2, Ty::Var(_)) {
            return true;
        }

        // Inference variables are compatible with anything (will be resolved later)
        if matches!(ty1, Ty::Infer(_)) || matches!(ty2, Ty::Infer(_)) {
            return true;
        }

        match (ty1, ty2) {
            (Ty::Int, Ty::Int) => true,
            (Ty::Float, Ty::Float) => true,
            (Ty::String, Ty::String) => true,
            (Ty::Atom, Ty::Atom) => true,
            (Ty::Bool, Ty::Bool) => true,
            (Ty::Unit, Ty::Unit) => true,
            (Ty::Binary, Ty::Binary) => true,
            (Ty::Pid, Ty::Pid) => true,
            (Ty::RawMap, Ty::RawMap) => true,

            (Ty::Tuple(tys1), Ty::Tuple(tys2)) => {
                tys1.len() == tys2.len()
                    && tys1.iter().zip(tys2.iter()).all(|(t1, t2)| self.types_compatible(t1, t2))
            }

            (Ty::List(t1), Ty::List(t2)) => self.types_compatible(t1, t2),

            (
                Ty::Named { name: n1, args: a1, .. },
                Ty::Named { name: n2, args: a2, .. },
            ) => {
                n1 == n2
                    && a1.len() == a2.len()
                    && a1.iter().zip(a2.iter()).all(|(t1, t2)| self.types_compatible(t1, t2))
            }

            (
                Ty::Fn { params: p1, ret: r1 },
                Ty::Fn { params: p2, ret: r2 },
            ) => {
                p1.len() == p2.len()
                    && p1.iter().zip(p2.iter()).all(|(t1, t2)| self.types_compatible(t1, t2))
                    && self.types_compatible(r1, r2)
            }

            _ => false,
        }
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

/// Type check a module and return any errors.
pub fn check_module(module: &Module) -> TypeResult<()> {
    let mut checker = TypeChecker::new();
    checker.check_module(module)
}

/// Type check multiple modules with shared type information.
/// This allows cross-module type references (e.g., using enums from another module).
pub fn check_modules(modules: &[Module]) -> Vec<(String, TypeResult<()>)> {
    let mut checker = TypeChecker::new();

    // First pass: collect all type definitions from ALL modules
    for module in modules {
        let _ = checker.collect_types(module);
    }

    // Second pass: collect all function signatures from ALL modules
    for module in modules {
        let _ = checker.collect_functions(module);
    }

    // Third pass: type check each module's function bodies
    let mut results = Vec::new();
    for module in modules {
        // Clear errors before checking each module
        checker.errors.clear();

        for item in &module.items {
            if let Item::Function(func) = item {
                let _ = checker.check_function(func);
            }
            if let Item::Impl(impl_block) = item {
                let _ = checker.check_impl_block(impl_block);
            }
        }

        // Collect result for this module
        let result = if let Some(err) = checker.errors.first() {
            Err(err.clone())
        } else {
            Ok(())
        };
        results.push((module.name.clone(), result));
    }

    results
}

// =============================================================================
// UFCS Method Resolution
// =============================================================================

/// Map primitive types to their stdlib module.
/// Returns None for types without a stdlib module (user-defined types, etc.).
fn primitive_module(ty: &Ty) -> Option<&'static str> {
    match ty {
        Ty::String => Some("string"),
        Ty::List(_) => Some("enumerable"),
        Ty::RawMap => Some("map"),
        // Future: extend for user-defined types
        _ => None,
    }
}

/// Infer the return type of a stdlib method.
/// Used for type-directed resolution of chained method calls.
fn stdlib_method_return_type(module: &str, method: &str, recv_ty: &Ty) -> Ty {
    match module {
        "string" => match method {
            // String -> String methods
            "trim" | "to_upper" | "to_lower" | "reverse" | "replace" | "slice" => Ty::String,
            // String -> Int methods
            "len" => Ty::Int,
            // String -> Bool methods
            "contains" | "starts_with" | "ends_with" | "is_empty" => Ty::Bool,
            // String -> List methods
            "split" | "chars" => Ty::List(Box::new(Ty::String)),
            _ => Ty::Any,
        },
        "enumerable" => match method {
            // List -> List methods (preserve element type)
            "map" | "filter" | "reject" | "take" | "drop" | "reverse" | "sort" | "uniq" => {
                recv_ty.clone()
            }
            // List -> Int methods
            "sum" | "count" | "count_all" => Ty::Int,
            // List -> Bool methods
            "any" | "all" | "none" | "is_empty" | "member" => Ty::Bool,
            // List -> element methods (return Any since we don't track element type precisely)
            "first" | "last" | "find" | "max" | "min" | "reduce" => Ty::Any,
            _ => Ty::Any,
        },
        "map" => match method {
            // Map operations
            "get" | "fetch" => Ty::Any,
            "put" | "delete" | "merge" => Ty::RawMap,
            "keys" | "values" => Ty::List(Box::new(Ty::Any)),
            "size" => Ty::Int,
            "has_key" | "is_empty" => Ty::Bool,
            _ => Ty::Any,
        },
        _ => Ty::Any,
    }
}

/// Resolve stdlib method calls in a module.
/// For method calls on primitive types (string, list, map), fills in `resolved_module`
/// so codegen can emit UFCS calls to the appropriate stdlib module.
pub fn resolve_stdlib_methods(module: &mut Module) {
    let mut resolver = MethodResolver::new();
    resolver.resolve_module(module);
}

/// Walks the AST and resolves method calls to stdlib modules.
struct MethodResolver {
    /// Variable types in current scope
    vars: HashMap<String, Ty>,
}

impl MethodResolver {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    fn resolve_module(&mut self, module: &mut Module) {
        for item in &mut module.items {
            match item {
                Item::Function(func) => self.resolve_function(func),
                Item::Impl(impl_block) => {
                    for method in &mut impl_block.methods {
                        self.resolve_function(method);
                    }
                }
                Item::TraitImpl(trait_impl) => {
                    for method in &mut trait_impl.methods {
                        self.resolve_function(method);
                    }
                }
                _ => {}
            }
        }
    }

    fn resolve_function(&mut self, func: &mut Function) {
        // Bind parameters
        let old_vars = std::mem::take(&mut self.vars);
        for param in &func.params {
            if let Pattern::Ident(name) = &param.pattern {
                let ty = self.ast_type_to_ty(&param.ty);
                self.vars.insert(name.clone(), ty);
            }
        }

        self.resolve_block(&mut func.body);

        self.vars = old_vars;
    }

    fn resolve_block(&mut self, block: &mut Block) {
        for stmt in &mut block.stmts {
            self.resolve_stmt(stmt);
        }
        if let Some(expr) = &mut block.expr {
            self.resolve_expr(expr);
        }
    }

    fn resolve_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Let { pattern, ty, value } => {
                self.resolve_expr(value);

                // Bind variable with inferred or annotated type
                let value_ty = if let Some(ann) = ty {
                    self.ast_type_to_ty(ann)
                } else {
                    self.infer_expr_type(value)
                };

                if let Pattern::Ident(name) = pattern {
                    self.vars.insert(name.clone(), value_ty);
                }
            }
            Stmt::Expr(expr) => {
                self.resolve_expr(expr);
            }
        }
    }

    fn resolve_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::MethodCall {
                receiver,
                method: _,
                args,
                resolved_module,
            } => {
                // Resolve receiver first
                self.resolve_expr(receiver);

                // Infer receiver type and look up stdlib module
                let recv_ty = self.infer_expr_type(receiver);
                if let Some(module) = primitive_module(&recv_ty) {
                    *resolved_module = Some(module.to_string());
                }

                // Resolve args
                for arg in args {
                    self.resolve_expr(arg);
                }
            }

            // Recursively resolve all other expression types
            Expr::Binary { left, right, .. } => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Expr::Unary { expr, .. } => {
                self.resolve_expr(expr);
            }
            Expr::Call { func, args, .. } => {
                self.resolve_expr(func);
                for arg in args {
                    self.resolve_expr(arg);
                }
            }
            Expr::If {
                cond,
                then_block,
                else_block,
            } => {
                self.resolve_expr(cond);
                self.resolve_block(then_block);
                if let Some(blk) = else_block {
                    self.resolve_block(blk);
                }
            }
            Expr::Match { expr, arms } => {
                self.resolve_expr(expr);
                for arm in arms {
                    self.resolve_expr(&mut arm.body);
                    if let Some(guard) = &mut arm.guard {
                        self.resolve_expr(guard);
                    }
                }
            }
            Expr::Block(block) => {
                self.resolve_block(block);
            }
            Expr::Tuple(exprs) | Expr::List(exprs) => {
                for e in exprs {
                    self.resolve_expr(e);
                }
            }
            Expr::StructInit { fields, .. } => {
                for (_, e) in fields {
                    self.resolve_expr(e);
                }
            }
            Expr::EnumVariant { args, .. } => {
                for arg in args {
                    self.resolve_expr(arg);
                }
            }
            Expr::FieldAccess { expr, .. } => {
                self.resolve_expr(expr);
            }
            Expr::Spawn(e) => {
                self.resolve_expr(e);
            }
            Expr::SpawnClosure(block) => {
                self.resolve_block(block);
            }
            Expr::Closure { body, .. } => {
                self.resolve_block(body);
            }
            Expr::Send { to, msg } => {
                self.resolve_expr(to);
                self.resolve_expr(msg);
            }
            Expr::Pipe { left, right } => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Expr::Receive { arms, timeout } => {
                for arm in arms {
                    self.resolve_expr(&mut arm.body);
                }
                if let Some((timeout_expr, block)) = timeout {
                    self.resolve_expr(timeout_expr);
                    self.resolve_block(block);
                }
            }
            Expr::Return(Some(e)) => {
                self.resolve_expr(e);
            }
            Expr::ExternCall { args, .. } => {
                for arg in args {
                    self.resolve_expr(arg);
                }
            }
            Expr::BitString(segments) => {
                for seg in segments {
                    self.resolve_expr(&mut seg.value);
                }
            }
            // Leaf expressions - no children to resolve
            Expr::Int(_)
            | Expr::String(_)
            | Expr::Atom(_)
            | Expr::Bool(_)
            | Expr::Ident(_)
            | Expr::Path { .. }
            | Expr::Unit
            | Expr::Return(None) => {}
        }
    }

    /// Simple type inference for the resolver.
    /// Only needs to determine primitive types for UFCS resolution.
    fn infer_expr_type(&self, expr: &Expr) -> Ty {
        match expr {
            Expr::String(_) => Ty::String,
            Expr::Int(_) => Ty::Int,
            Expr::Bool(_) => Ty::Bool,
            Expr::Atom(_) => Ty::Atom,
            Expr::Unit => Ty::Unit,
            Expr::List(_) => Ty::List(Box::new(Ty::Any)),
            Expr::Tuple(exprs) => {
                Ty::Tuple(exprs.iter().map(|e| self.infer_expr_type(e)).collect())
            }
            Expr::Ident(name) => self.vars.get(name).cloned().unwrap_or(Ty::Any),
            Expr::FieldAccess { expr, .. } => {
                // Could look up struct field types, but Any is safe
                let _ = self.infer_expr_type(expr);
                Ty::Any
            }
            Expr::MethodCall { receiver, method, resolved_module, .. } => {
                let recv_ty = self.infer_expr_type(receiver);
                // If resolved to a stdlib module, infer return type
                if let Some(module) = resolved_module {
                    stdlib_method_return_type(module, method, &recv_ty)
                } else {
                    Ty::Any
                }
            }
            Expr::Call { .. } | Expr::ExternCall { .. } | Expr::Path { .. } => Ty::Any,
            Expr::Binary { op, .. } => match op {
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => Ty::Int,
                BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => Ty::Bool,
                BinOp::And | BinOp::Or => Ty::Bool,
            },
            Expr::Unary { op, .. } => match op {
                UnaryOp::Neg => Ty::Int,
                UnaryOp::Not => Ty::Bool,
            },
            Expr::If { then_block, .. } => {
                if let Some(expr) = &then_block.expr {
                    self.infer_expr_type(expr)
                } else {
                    Ty::Unit
                }
            }
            Expr::Block(block) => {
                if let Some(expr) = &block.expr {
                    self.infer_expr_type(expr)
                } else {
                    Ty::Unit
                }
            }
            _ => Ty::Any,
        }
    }

    /// Convert AST type to internal Ty (simplified version for resolver).
    fn ast_type_to_ty(&self, ast_ty: &ast::Type) -> Ty {
        match ast_ty {
            ast::Type::Int => Ty::Int,
            ast::Type::String => Ty::String,
            ast::Type::Atom => Ty::Atom,
            ast::Type::Bool => Ty::Bool,
            ast::Type::Unit => Ty::Unit,
            ast::Type::Binary => Ty::Binary,
            ast::Type::Pid => Ty::Pid,
            ast::Type::Map => Ty::RawMap,
            ast::Type::Tuple(tys) => {
                Ty::Tuple(tys.iter().map(|t| self.ast_type_to_ty(t)).collect())
            }
            ast::Type::List(t) => Ty::List(Box::new(self.ast_type_to_ty(t))),
            ast::Type::Named { name, type_args } => {
                match name.as_str() {
                    "int" => Ty::Int,
                    "string" => Ty::String,
                    "atom" => Ty::Atom,
                    "bool" => Ty::Bool,
                    "pid" => Ty::Pid,
                    "binary" => Ty::Binary,
                    _ => Ty::Named {
                        name: name.clone(),
                        module: None,
                        args: type_args.iter().map(|t| self.ast_type_to_ty(t)).collect(),
                    },
                }
            }
            ast::Type::TypeVar(name) => Ty::Var(name.clone()),
            ast::Type::Fn { params, ret } => Ty::Fn {
                params: params.iter().map(|t| self.ast_type_to_ty(t)).collect(),
                ret: Box::new(self.ast_type_to_ty(ret)),
            },
            ast::Type::AssociatedType { base, name } => Ty::AssociatedType {
                base: base.clone(),
                name: name.clone(),
            },
            ast::Type::Any => Ty::Any,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::parser::Parser;

    fn parse_and_check(source: &str) -> TypeResult<()> {
        let mut parser = Parser::new(source);
        let module = parser.parse_module().expect("parse error");
        check_module(&module)
    }

    #[test]
    fn test_simple_function() {
        let result = parse_and_check(r#"
            mod test {
                fn add(x: int, y: int) -> int {
                    x + y
                }
            }
        "#);
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_mismatch_return() {
        let result = parse_and_check(r#"
            mod test {
                fn bad() -> int {
                    "hello"
                }
            }
        "#);
        assert!(result.is_err());
    }

    #[test]
    fn test_if_branch_types() {
        let result = parse_and_check(r#"
            mod test {
                fn test(x: bool) -> int {
                    if x {
                        1
                    } else {
                        2
                    }
                }
            }
        "#);
        assert!(result.is_ok());
    }

    #[test]
    fn test_if_condition_not_bool() {
        let result = parse_and_check(r#"
            mod test {
                fn test(x: int) -> int {
                    if x {
                        1
                    } else {
                        2
                    }
                }
            }
        "#);
        assert!(result.is_err());
    }

    #[test]
    fn test_undefined_variable() {
        let result = parse_and_check(r#"
            mod test {
                fn test() -> int {
                    x
                }
            }
        "#);
        assert!(result.is_err());
    }

    #[test]
    fn test_struct_field_access() {
        let result = parse_and_check(r#"
            mod test {
                struct Point {
                    x: int,
                    y: int,
                }

                fn get_x(p: Point) -> int {
                    p.x
                }
            }
        "#);
        assert!(result.is_ok());
    }

    #[test]
    fn test_wrong_argument_count() {
        let result = parse_and_check(r#"
            mod test {
                fn add(x: int, y: int) -> int {
                    x + y
                }

                fn test() -> int {
                    add(1)
                }
            }
        "#);
        assert!(result.is_err());
    }
}
