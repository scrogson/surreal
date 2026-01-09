//! Type checker for Dream.
//!
//! Performs semantic analysis to validate types across the program.
//! The type checker runs after parsing and before code generation.

use std::collections::HashMap;

use crate::compiler::ast::{
    self, BinOp, Block, Expr, ExternItem, ExternMod, Function, ImplBlock, Item, MatchArm, Module,
    Pattern, Stmt, StringPart, TypeParam, UnaryOp,
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
    Ref,

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

    /// Literal atom type (e.g., :ok, :error as types)
    AtomLiteral(String),

    /// Union type (e.g., :ok | :error, int | string)
    Union(Vec<Ty>),
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
            Ty::Union(tys) => tys.iter().any(|t| t.has_infer()),
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
            Ty::Union(tys) => tys.iter().any(|t| t.has_infer_id(target_id)),
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
            Ty::Union(tys) => Ty::Union(tys.iter().map(|t| t.substitute(subst)).collect()),
            _ => self.clone(),
        }
    }

    /// Convert internal Ty back to AST Type for storing inferred types.
    pub fn to_ast_type(&self) -> ast::Type {
        match self {
            Ty::Int => ast::Type::Int,
            Ty::Float => ast::Type::Float,
            Ty::String => ast::Type::String,
            Ty::Atom => ast::Type::Atom,
            Ty::Bool => ast::Type::Bool,
            Ty::Unit => ast::Type::Unit,
            Ty::Binary => ast::Type::Binary,
            Ty::Pid => ast::Type::Pid,
            Ty::Ref => ast::Type::Ref,
            Ty::RawMap => ast::Type::Map,
            Ty::Tuple(tys) => ast::Type::Tuple(tys.iter().map(|t| t.to_ast_type()).collect()),
            Ty::List(t) => ast::Type::List(Box::new(t.to_ast_type())),
            Ty::Named { name, args, .. } => ast::Type::Named {
                name: name.clone(),
                type_args: args.iter().map(|t| t.to_ast_type()).collect(),
            },
            Ty::Fn { params, ret } => ast::Type::Fn {
                params: params.iter().map(|t| t.to_ast_type()).collect(),
                ret: Box::new(ret.to_ast_type()),
            },
            Ty::Var(name) => ast::Type::TypeVar(name.clone()),
            Ty::Infer(_) => ast::Type::Any, // Unresolved inference var becomes any
            Ty::Error => ast::Type::Any,
            Ty::Any => ast::Type::Any,
            Ty::AtomLiteral(name) => ast::Type::AtomLiteral(name.clone()),
            Ty::Union(tys) => ast::Type::Union(tys.iter().map(|t| t.to_ast_type()).collect()),
            Ty::AssociatedType { base, name } => ast::Type::AssociatedType {
                base: base.clone(),
                name: name.clone(),
            },
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
            Ty::Ref => write!(f, "ref"),
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
            Ty::AtomLiteral(name) => write!(f, ":{}", name),
            Ty::Union(tys) => {
                for (i, t) in tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", t)?;
                }
                Ok(())
            }
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
    /// Whether this method has a default implementation
    pub has_default: bool,
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

// ============================================================================
// Match Exhaustiveness Checking Types
// ============================================================================

/// Represents a "constructor" - the shape a value can have.
/// Used for exhaustiveness checking in match expressions.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constructor {
    /// Enum variant: (enum_name, variant_name, arity)
    Variant(String, String, usize),
    /// Boolean literal
    Bool(bool),
    /// Tuple with given arity
    Tuple(usize),
    /// List cons cell (head :: tail)
    ListCons,
    /// Empty list
    ListNil,
    /// Struct type
    Struct(String),
    /// Wildcard (matches anything)
    Wildcard,
    /// Non-exhaustive marker for open types (int, string, etc.)
    NonExhaustive,
}

impl Constructor {
    /// Check if this is a wildcard constructor.
    pub fn is_wildcard(&self) -> bool {
        matches!(self, Constructor::Wildcard)
    }

    /// Get the arity (number of sub-patterns) for this constructor.
    pub fn arity(&self) -> usize {
        match self {
            Constructor::Variant(_, _, arity) => *arity,
            Constructor::Bool(_) => 0,
            Constructor::Tuple(arity) => *arity,
            Constructor::ListCons => 2, // head and tail
            Constructor::ListNil => 0,
            Constructor::Struct(_) => 0, // struct fields handled separately
            Constructor::Wildcard => 0,
            Constructor::NonExhaustive => 0,
        }
    }
}

impl std::fmt::Display for Constructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constructor::Variant(enum_name, variant, _) => write!(f, "{}::{}", enum_name, variant),
            Constructor::Bool(b) => write!(f, "{}", b),
            Constructor::Tuple(n) => write!(f, "({}-tuple)", n),
            Constructor::ListCons => write!(f, "[_ | _]"),
            Constructor::ListNil => write!(f, "[]"),
            Constructor::Struct(name) => write!(f, "{}", name),
            Constructor::Wildcard => write!(f, "_"),
            Constructor::NonExhaustive => write!(f, "_"),
        }
    }
}

/// A deconstructed pattern for exhaustiveness analysis.
/// Normalized representation that's easier to analyze than AST patterns.
#[derive(Debug, Clone)]
pub struct DeconstructedPat {
    /// The constructor this pattern matches
    pub ctor: Constructor,
    /// Sub-patterns for constructor fields
    pub fields: Vec<DeconstructedPat>,
    /// The type of the pattern
    pub ty: Ty,
}

impl DeconstructedPat {
    /// Create a wildcard pattern for a given type.
    pub fn wildcard(ty: Ty) -> Self {
        DeconstructedPat {
            ctor: Constructor::Wildcard,
            fields: vec![],
            ty,
        }
    }

    /// Check if this is a wildcard pattern.
    pub fn is_wildcard(&self) -> bool {
        self.ctor.is_wildcard()
    }
}

/// A matrix of patterns for usefulness checking.
/// Each row represents the patterns from one match arm.
#[derive(Debug, Clone)]
pub struct PatternMatrix {
    /// Each row is a list of patterns (one per column being matched)
    pub rows: Vec<Vec<DeconstructedPat>>,
}

impl PatternMatrix {
    /// Create an empty pattern matrix.
    pub fn new() -> Self {
        PatternMatrix { rows: vec![] }
    }

    /// Add a row to the matrix.
    pub fn push_row(&mut self, row: Vec<DeconstructedPat>) {
        self.rows.push(row);
    }

    /// Check if the matrix is empty (no rows).
    pub fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }

    /// Get the number of columns (0 if empty).
    pub fn num_columns(&self) -> usize {
        self.rows.first().map(|r| r.len()).unwrap_or(0)
    }
}

impl Default for PatternMatrix {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Type Environment
// ============================================================================

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
    /// Associated type bindings from module-level trait declarations: "State" -> Ty::Int
    associated_types: HashMap<String, Ty>,
    /// External function signatures: (module, function, arity) -> FnInfo
    /// Used for type-checking FFI calls to Erlang/Elixir/etc
    extern_functions: HashMap<(String, String, usize), FnInfo>,
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
            associated_types: self.associated_types.clone(),
            extern_functions: self.extern_functions.clone(),
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

    /// Get cross-module function info (e.g., "io::dbg").
    pub fn get_cross_module_function(&self, module: &str, func: &str) -> Option<&FnInfo> {
        let key = format!("{}::{}", module, func);
        self.functions.get(&key)
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

    /// Get external function info (from .dreamt stubs).
    pub fn get_extern_function(&self, module: &str, function: &str, arity: usize) -> Option<&FnInfo> {
        self.extern_functions
            .get(&(module.to_string(), function.to_string(), arity))
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
    /// Current function's type parameters and their bounds (for checking calls within generic functions)
    /// Maps type param name (e.g., "T") to Vec<trait_name> (e.g., ["GenServer"])
    current_type_param_bounds: HashMap<String, Vec<String>>,
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
            current_type_param_bounds: HashMap::new(),
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

            // AtomLiteral is a subtype of Atom
            (Ty::AtomLiteral(_), Ty::Atom) | (Ty::Atom, Ty::AtomLiteral(_)) => Ok(()),

            // AtomLiterals with the same value unify
            (Ty::AtomLiteral(a), Ty::AtomLiteral(b)) if a == b => Ok(()),

            // Union type unification:
            // A type T unifies with a union if T matches any variant in the union
            (ty, Ty::Union(variants)) | (Ty::Union(variants), ty) => {
                // Check if the type matches any variant in the union
                for variant in variants {
                    if self.types_compatible(ty, variant) {
                        return Ok(());
                    }
                }
                // Also allow if ty is a union and all its variants are in the target union
                if let Ty::Union(ty_variants) = ty {
                    let all_match = ty_variants.iter().all(|v| {
                        variants.iter().any(|target| self.types_compatible(v, target))
                    });
                    if all_match {
                        return Ok(());
                    }
                }
                Err(TypeError::new(format!(
                    "type {} is not compatible with union {}",
                    ty, Ty::Union(variants.clone())
                )))
            }

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
            Ty::Union(tys) => {
                Ty::Union(tys.iter().map(|t| self.apply_substitutions(t)).collect())
            }
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
            Ty::Ref => Some("ref".to_string()),
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

        // Check if this is a type parameter with declared bounds
        if let Some(param_bounds) = self.current_type_param_bounds.get(&type_name) {
            // This is a type parameter - check if its declared bounds satisfy the required bounds
            for required_bound in bounds {
                if !param_bounds.contains(required_bound) {
                    return Err(required_bound.clone());
                }
            }
            return Ok(());
        }

        // Not a type parameter - check if the concrete type implements the traits
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
            ast::Type::Float => Ty::Float,
            ast::Type::String => Ty::String,
            ast::Type::Atom => Ty::Atom,
            ast::Type::Bool => Ty::Bool,
            ast::Type::Unit => Ty::Unit,
            ast::Type::Binary => Ty::Binary,
            ast::Type::Pid => Ty::Pid,
            ast::Type::Ref => Ty::Ref,
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
                    "float" => Ty::Float,
                    "string" => Ty::String,
                    "atom" => Ty::Atom,
                    "bool" => Ty::Bool,
                    "pid" => Ty::Pid,
                    "ref" => Ty::Ref,
                    "binary" => Ty::Binary,
                    _ => {
                        // Check if it's a type alias
                        if type_args.is_empty() {
                            if let Some(aliased_ty) = self.env.type_aliases.get(name) {
                                return aliased_ty.clone();
                            }
                        }
                        // Otherwise, it's a named type (struct, enum, etc.)
                        Ty::Named {
                            name: name.clone(),
                            module: None,
                            args: type_args.iter().map(|t| self.ast_type_to_ty(t)).collect(),
                        }
                    }
                }
            }
            ast::Type::Fn { params, ret } => Ty::Fn {
                params: params.iter().map(|t| self.ast_type_to_ty(t)).collect(),
                ret: Box::new(self.ast_type_to_ty(ret)),
            },
            ast::Type::AssociatedType { base, name } => {
                // Try to resolve Self::X using associated type bindings
                if base == "Self" {
                    if let Some(resolved) = self.env.associated_types.get(name) {
                        return resolved.clone();
                    }
                }
                // If not resolvable, keep as unresolved associated type
                Ty::AssociatedType {
                    base: base.clone(),
                    name: name.clone(),
                }
            }
            ast::Type::Any => Ty::Any,
            ast::Type::AtomLiteral(name) => Ty::AtomLiteral(name.clone()),
            ast::Type::Union(tys) => {
                Ty::Union(tys.iter().map(|t| self.ast_type_to_ty(t)).collect())
            }
        }
    }

    /// Type check a module.
    pub fn check_module(&mut self, module: &Module) -> TypeResult<()> {
        // First pass: collect all type definitions
        self.collect_types(module)?;

        // Validate trait implementations (after types are collected)
        for item in &module.items {
            if let Item::TraitImpl(impl_def) = item {
                self.validate_trait_impl(impl_def);
            }
        }

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
                                has_default: m.body.is_some(),
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
                    // Store associated type bindings for Self::X resolution
                    for (name, ty) in &decl.type_bindings {
                        let resolved_ty = self.ast_type_to_ty(ty);
                        self.env.associated_types.insert(name.clone(), resolved_ty);
                    }
                }
                Item::ExternMod(extern_mod) => {
                    // Collect external function signatures from .dreamt stubs
                    self.collect_extern_mod(extern_mod, &extern_mod.name);
                }
                Item::TypeAlias(alias) => {
                    // Type aliases are collected but resolved lazily in ast_type_to_ty
                    // For now, store the raw AST type; we'll resolve it when used
                    let ty = self.ast_type_to_ty(&alias.ty);
                    self.env.type_aliases.insert(alias.name.clone(), ty);
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Recursively collect external function signatures from an extern mod.
    fn collect_extern_mod(&mut self, extern_mod: &ExternMod, module_path: &str) {
        for item in &extern_mod.items {
            match item {
                ExternItem::Mod(nested) => {
                    // Build nested module path: erlang.maps, etc.
                    let nested_path = format!("{}.{}", module_path, nested.name);
                    self.collect_extern_mod(nested, &nested_path);
                }
                ExternItem::Function(func) => {
                    let params: Vec<(String, Ty)> = func
                        .params
                        .iter()
                        .map(|(name, ty)| (name.clone(), self.ast_type_to_ty(ty)))
                        .collect();
                    let ret = self.ast_type_to_ty(&func.return_type);
                    let info = FnInfo {
                        name: func.name.clone(),
                        type_params: func.type_params.clone(),
                        params,
                        ret,
                    };
                    let arity = info.params.len();
                    self.env.extern_functions.insert(
                        (module_path.to_string(), func.name.clone(), arity),
                        info,
                    );
                }
                ExternItem::Type(_) => {
                    // TODO: Handle opaque type declarations
                    // For now, we skip them - they're just markers
                }
            }
        }
    }

    /// Second pass: collect function signatures.
    fn collect_functions(&mut self, module: &Module) -> TypeResult<()> {
        for item in &module.items {
            match item {
                Item::Function(func) => {
                    let info = self.function_to_info(func);
                    // Store with both simple name and module-qualified name
                    self.env.functions.insert(func.name.clone(), info.clone());
                    let qualified_name = format!("{}::{}", module.name, func.name);
                    self.env.functions.insert(qualified_name, info);
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

        // Set type parameter bounds for this function (for checking calls within generic functions)
        let old_type_param_bounds =
            std::mem::replace(&mut self.current_type_param_bounds, HashMap::new());
        for type_param in &func.type_params {
            self.current_type_param_bounds
                .insert(type_param.name.clone(), type_param.bounds.clone());
        }

        // Bind parameters - use bind_pattern to handle all pattern types including enum variants
        let old_env = std::mem::replace(&mut self.env, scope);
        for param in &func.params {
            let ty = self.ast_type_to_ty(&param.ty);
            self.bind_pattern(&param.pattern, &ty)?;
        }
        scope = std::mem::replace(&mut self.env, old_env);

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
        self.current_type_param_bounds = old_type_param_bounds;
        Ok(())
    }

    /// Validate that a trait implementation provides all required methods.
    fn validate_trait_impl(&mut self, impl_def: &ast::TraitImpl) {
        // Get trait definition
        let trait_info = match self.env.traits.get(&impl_def.trait_name) {
            Some(info) => info.clone(),
            None => {
                self.error(TypeError::new(format!(
                    "trait '{}' not found",
                    impl_def.trait_name
                )));
                return;
            }
        };

        // Get implemented method names
        let impl_methods: std::collections::HashSet<&str> = impl_def
            .methods
            .iter()
            .map(|m| m.name.as_str())
            .collect();

        // Check each required method is implemented
        let mut missing_methods = Vec::new();
        for method in &trait_info.methods {
            if !method.has_default && !impl_methods.contains(method.name.as_str()) {
                missing_methods.push(method.name.clone());
            }
        }

        if !missing_methods.is_empty() {
            self.error(TypeError::with_help(
                format!(
                    "incomplete implementation of trait '{}' for '{}'",
                    impl_def.trait_name, impl_def.type_name
                ),
                format!("missing required methods: {}", missing_methods.join(", ")),
            ));
        }

        // Check associated types are bound
        let bound_types: std::collections::HashSet<&str> = impl_def
            .type_bindings
            .iter()
            .map(|(name, _)| name.as_str())
            .collect();

        let mut missing_types = Vec::new();
        for assoc_type in &trait_info.associated_types {
            if !bound_types.contains(assoc_type.as_str()) {
                missing_types.push(assoc_type.clone());
            }
        }

        if !missing_types.is_empty() {
            self.error(TypeError::with_help(
                format!(
                    "incomplete implementation of trait '{}' for '{}'",
                    impl_def.trait_name, impl_def.type_name
                ),
                format!("missing associated types: {}", missing_types.join(", ")),
            ));
        }
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
                    } else {
                        // Length mismatch - still bind variables as Any
                        // This happens when matching different tuple sizes
                        for p in pats {
                            self.bind_pattern(p, &Ty::Any)?;
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
            Expr::StringInterpolation(parts) => {
                // Type check each expression part (any type is allowed)
                for part in parts {
                    if let StringPart::Expr(e) = part {
                        self.infer_expr(e)?;
                    }
                }
                // Result is always String
                Ok(Ty::String)
            }
            Expr::Atom(name) => Ok(Ty::AtomLiteral(name.clone())),
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
            Expr::Call { func, type_args, args, .. } => {
                self.infer_call(func, type_args, args)
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
                    if self.types_compatible(&then_ty, &else_ty) {
                        // Types are compatible, use the more general one
                        // (e.g., :ok with atom → atom, :ok with :ok → :ok)
                        if matches!(then_ty, Ty::AtomLiteral(_)) && matches!(else_ty, Ty::Atom) {
                            Ok(else_ty)
                        } else if matches!(else_ty, Ty::AtomLiteral(_)) && matches!(then_ty, Ty::Atom) {
                            Ok(then_ty)
                        } else {
                            Ok(then_ty)
                        }
                    } else {
                        // Types are different - create a union type
                        // Flatten nested unions and deduplicate
                        let mut variants = Vec::new();
                        match &then_ty {
                            Ty::Union(tys) => variants.extend(tys.clone()),
                            ty => variants.push(ty.clone()),
                        }
                        match &else_ty {
                            Ty::Union(tys) => {
                                for ty in tys {
                                    if !variants.iter().any(|v| self.types_compatible(v, ty)) {
                                        variants.push(ty.clone());
                                    }
                                }
                            }
                            ty => {
                                if !variants.iter().any(|v| self.types_compatible(v, ty)) {
                                    variants.push(ty.clone());
                                }
                            }
                        }
                        Ok(Ty::Union(variants))
                    }
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
            Expr::StructInit { name, fields, base } => {
                if let Some(info) = self.env.get_struct(name).cloned() {
                    let struct_ty = Ty::Named {
                        name: name.clone(),
                        module: None,
                        args: vec![],
                    };

                    // Check base expression if present (struct update syntax)
                    if let Some(base_expr) = base {
                        let base_ty = self.infer_expr(base_expr)?;
                        if !self.types_compatible(&base_ty, &struct_ty) {
                            self.error(TypeError::with_help(
                                "struct update base type mismatch".to_string(),
                                format!("expected {}, found {}", struct_ty, base_ty),
                            ));
                        }
                    }

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
                    Ok(struct_ty)
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

            // Try operator: expr? - early return on Err/None
            Expr::Try { expr } => {
                let ty = self.infer_expr(expr)?;
                // Extract the success type from Result<T, E> or Option<T>
                if let Ty::Named { name, args, .. } = &ty {
                    match name.as_str() {
                        "Result" if args.len() >= 1 => {
                            // Result<T, E> - the ? operator returns T
                            // TODO: Check that function return type is compatible with Result<_, E>
                            Ok(args[0].clone())
                        }
                        "Option" if args.len() >= 1 => {
                            // Option<T> - the ? operator returns T
                            // TODO: Check that function return type is compatible with Option<_>
                            Ok(args[0].clone())
                        }
                        _ => {
                            self.error(TypeError::new(format!(
                                "the `?` operator can only be applied to Result or Option, found {}",
                                ty
                            )));
                            Ok(Ty::Error)
                        }
                    }
                } else {
                    self.error(TypeError::new(format!(
                        "the `?` operator can only be applied to Result or Option, found {}",
                        ty
                    )));
                    Ok(Ty::Error)
                }
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
            Expr::ExternCall { module, function, args } => {
                // Look up extern function signature from .dreamt stubs (by arity)
                let arity = args.len();
                if let Some(info) = self.env.get_extern_function(module, function, arity).cloned() {
                    // Instantiate generic function
                    let instantiated = self.instantiate_function(&info);

                    // Check argument types
                    for (arg, (_, param_ty)) in args.iter().zip(instantiated.params.iter()) {
                        let arg_ty = self.infer_expr(arg)?;
                        if self.unify(&arg_ty, param_ty).is_err()
                            && !self.types_compatible(&arg_ty, param_ty)
                        {
                            self.error(TypeError::with_help(
                                format!("type mismatch in call to '{}::{}'", module, function),
                                format!("expected {}, found {}", param_ty, arg_ty),
                            ));
                        }
                    }

                    // Return the function's declared return type
                    Ok(self.apply_substitutions(&instantiated.ret))
                } else {
                    // No stub found - still type check args but return Any
                    for arg in args {
                        self.infer_expr(arg)?;
                    }
                    Ok(Ty::Any)
                }
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
    fn infer_call(
        &mut self,
        func: &Expr,
        type_args: &[ast::Type],
        args: &[Expr],
    ) -> TypeResult<Ty> {
        match func {
            Expr::Ident(name) => {
                if let Some(info) = self.env.get_function(name).cloned() {
                    // Instantiate generic function
                    let instantiated = if !type_args.is_empty() {
                        // Explicit type arguments (turbofish syntax)
                        self.instantiate_function_with_args(&info, type_args, name)?
                    } else {
                        // Type inference - use fresh variables
                        self.instantiate_function(&info)
                    };

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
            Expr::Path { segments } => {
                // Module::function call (e.g., io::dbg)
                if segments.len() == 2 {
                    let module = &segments[0];
                    let func_name = &segments[1];
                    let qualified_name = format!("{}::{}", module, func_name);

                    if let Some(info) = self.env.get_function(&qualified_name).cloned() {
                        // Instantiate generic function
                        let instantiated = if !type_args.is_empty() {
                            // Explicit type arguments (turbofish syntax)
                            self.instantiate_function_with_args(&info, type_args, &qualified_name)?
                        } else {
                            // Type inference - use fresh variables
                            self.instantiate_function(&info)
                        };

                        // Check argument count
                        if args.len() != instantiated.params.len() {
                            self.error(TypeError::new(format!(
                                "function '{}' expects {} arguments, got {}",
                                qualified_name,
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
                                    format!("type mismatch in call to '{}'", qualified_name),
                                    format!("expected {}, found {}", param_ty, arg_ty),
                                ));
                            }
                        }

                        // Apply substitutions to return type
                        return Ok(self.apply_substitutions(&instantiated.ret));
                    }
                }

                // Unknown cross-module function - could be external
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

    /// Instantiate a generic function with explicit type arguments.
    /// Validates that the type arguments satisfy the bounds.
    fn instantiate_function_with_args(
        &mut self,
        info: &FnInfo,
        type_args: &[ast::Type],
        func_name: &str,
    ) -> TypeResult<FnInfo> {
        // Check type argument count
        if type_args.len() != info.type_params.len() {
            self.error(TypeError::new(format!(
                "function '{}' expects {} type arguments, got {}",
                func_name,
                info.type_params.len(),
                type_args.len()
            )));
            return Ok(info.clone());
        }

        // Build substitution map and validate bounds
        let mut subst = HashMap::new();
        for (type_arg, type_param) in type_args.iter().zip(info.type_params.iter()) {
            let ty = self.ast_type_to_ty(type_arg);

            // Check that the type satisfies all bounds
            if let Err(missing_trait) = self.check_type_satisfies_bounds(&ty, &type_param.bounds) {
                self.error(TypeError::with_help(
                    format!(
                        "type argument does not satisfy bound in call to '{}'",
                        func_name
                    ),
                    format!(
                        "type {} does not implement trait {}",
                        ty, missing_trait
                    ),
                ));
            }

            subst.insert(type_param.name.clone(), ty);
        }

        // Apply substitution to create instantiated function
        Ok(FnInfo {
            name: info.name.clone(),
            type_params: vec![], // Instantiated function has no type params
            params: info
                .params
                .iter()
                .map(|(name, ty)| (name.clone(), ty.substitute(&subst)))
                .collect(),
            ret: info.ret.substitute(&subst),
        })
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

        // Check exhaustiveness
        let missing = self.check_exhaustiveness(scrutinee_ty, arms);
        if !missing.is_empty() {
            self.error(TypeError::with_help(
                "non-exhaustive match",
                format!("missing patterns: {}", missing.join(", ")),
            ));
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

        // Self type is compatible with any named type (resolved at impl level)
        if matches!(ty1, Ty::Named { name, .. } if name == "Self")
            || matches!(ty2, Ty::Named { name, .. } if name == "Self")
        {
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
            (Ty::Ref, Ty::Ref) => true,
            (Ty::RawMap, Ty::RawMap) => true,

            // AtomLiteral is compatible with Atom
            (Ty::AtomLiteral(_), Ty::Atom) | (Ty::Atom, Ty::AtomLiteral(_)) => true,

            // Same atom literals are compatible
            (Ty::AtomLiteral(a), Ty::AtomLiteral(b)) => a == b,

            // Union type compatibility:
            // A type T is compatible with a union if T matches any variant in the union
            (ty, Ty::Union(variants)) | (Ty::Union(variants), ty) => {
                // Check if the type matches any variant in the union
                variants.iter().any(|v| self.types_compatible(ty, v))
            }

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

    // =========================================================================
    // Match Exhaustiveness Checking
    // =========================================================================

    /// Convert an AST Pattern to a DeconstructedPat for exhaustiveness analysis.
    fn deconstruct_pattern(&self, pattern: &Pattern, ty: &Ty) -> DeconstructedPat {
        match pattern {
            // Wildcards and identifiers match anything
            Pattern::Wildcard => DeconstructedPat::wildcard(ty.clone()),
            Pattern::Ident(_) => DeconstructedPat::wildcard(ty.clone()),

            // Boolean literals
            Pattern::Bool(b) => DeconstructedPat {
                ctor: Constructor::Bool(*b),
                fields: vec![],
                ty: ty.clone(),
            },

            // Enum variants
            Pattern::Enum { name, variant, fields } => {
                // Get enum info to determine field types
                let field_types = self.get_enum_variant_types(name, variant);
                let decon_fields: Vec<DeconstructedPat> = fields
                    .iter()
                    .zip(field_types.iter())
                    .map(|(p, ft)| self.deconstruct_pattern(p, ft))
                    .collect();
                DeconstructedPat {
                    ctor: Constructor::Variant(name.clone(), variant.clone(), fields.len()),
                    fields: decon_fields,
                    ty: ty.clone(),
                }
            }

            // Tuple patterns
            Pattern::Tuple(pats) => {
                // Get element types from the scrutinee type
                let elem_types: Vec<Ty> = match ty {
                    Ty::Tuple(tys) => tys.clone(),
                    _ => pats.iter().map(|_| Ty::Any).collect(),
                };
                let decon_fields: Vec<DeconstructedPat> = pats
                    .iter()
                    .zip(elem_types.iter())
                    .map(|(p, et)| self.deconstruct_pattern(p, et))
                    .collect();
                DeconstructedPat {
                    ctor: Constructor::Tuple(pats.len()),
                    fields: decon_fields,
                    ty: ty.clone(),
                }
            }

            // List patterns
            Pattern::List(pats) => {
                if pats.is_empty() {
                    DeconstructedPat {
                        ctor: Constructor::ListNil,
                        fields: vec![],
                        ty: ty.clone(),
                    }
                } else {
                    // Non-empty list treated as cons cell for first element
                    // This is simplified - full implementation would recursively build cons cells
                    let elem_ty = match ty {
                        Ty::List(t) => (**t).clone(),
                        _ => Ty::Any,
                    };
                    let head = self.deconstruct_pattern(&pats[0], &elem_ty);
                    let tail = if pats.len() == 1 {
                        DeconstructedPat {
                            ctor: Constructor::ListNil,
                            fields: vec![],
                            ty: ty.clone(),
                        }
                    } else {
                        // Remaining elements form the tail
                        DeconstructedPat::wildcard(ty.clone())
                    };
                    DeconstructedPat {
                        ctor: Constructor::ListCons,
                        fields: vec![head, tail],
                        ty: ty.clone(),
                    }
                }
            }

            // List cons pattern [h | t]
            Pattern::ListCons { head, tail } => {
                let elem_ty = match ty {
                    Ty::List(t) => (**t).clone(),
                    _ => Ty::Any,
                };
                let head_pat = self.deconstruct_pattern(head, &elem_ty);
                let tail_pat = self.deconstruct_pattern(tail, ty);
                DeconstructedPat {
                    ctor: Constructor::ListCons,
                    fields: vec![head_pat, tail_pat],
                    ty: ty.clone(),
                }
            }

            // Struct patterns - treat as matching the struct constructor
            Pattern::Struct { name, fields: _ } => DeconstructedPat {
                ctor: Constructor::Struct(name.clone()),
                fields: vec![],
                ty: ty.clone(),
            },

            // Literals that don't have finite alternatives - treat as wildcard for exhaustiveness
            // (we'll use NonExhaustive for the type itself)
            Pattern::Int(_) | Pattern::String(_) | Pattern::Atom(_) => {
                DeconstructedPat::wildcard(ty.clone())
            }

            // Catch-all for other patterns
            _ => DeconstructedPat::wildcard(ty.clone()),
        }
    }

    /// Get the types of fields for an enum variant.
    fn get_enum_variant_types(&self, enum_name: &str, variant_name: &str) -> Vec<Ty> {
        if let Some(info) = self.env.get_enum(enum_name) {
            for (vname, vtypes) in &info.variants {
                if vname == variant_name {
                    return vtypes.clone();
                }
            }
        }
        vec![]
    }

    /// Get all constructors for a type.
    fn all_constructors(&self, ty: &Ty) -> Vec<Constructor> {
        match ty {
            Ty::Bool => vec![Constructor::Bool(true), Constructor::Bool(false)],

            Ty::Named { name, .. } => {
                // Check if it's an enum
                if let Some(info) = self.env.get_enum(name) {
                    info.variants
                        .iter()
                        .map(|(vname, fields)| {
                            Constructor::Variant(name.clone(), vname.clone(), fields.len())
                        })
                        .collect()
                } else if self.env.get_struct(name).is_some() {
                    // It's a struct - single constructor
                    vec![Constructor::Struct(name.clone())]
                } else {
                    // Unknown named type - treat as non-exhaustive
                    vec![Constructor::NonExhaustive]
                }
            }

            Ty::Tuple(tys) => vec![Constructor::Tuple(tys.len())],

            Ty::List(_) => vec![Constructor::ListNil, Constructor::ListCons],

            // Open types (int, string, etc.) require wildcard for exhaustiveness
            Ty::Int | Ty::Float | Ty::String | Ty::Atom | Ty::Binary | Ty::Pid | Ty::Ref => {
                vec![Constructor::NonExhaustive]
            }

            // For unit type, there's only one constructor (the unit value)
            Ty::Unit => vec![Constructor::Tuple(0)],

            // For inference variables and other types, be conservative
            _ => vec![Constructor::NonExhaustive],
        }
    }

    /// Check if a new pattern is useful given existing patterns.
    /// Returns true if the pattern matches something not covered by existing patterns.
    fn is_useful(&self, matrix: &PatternMatrix, pattern: &[DeconstructedPat]) -> bool {
        // Base case: empty pattern vector
        if pattern.is_empty() {
            // Useful if matrix is empty (nothing covered yet)
            return matrix.is_empty();
        }

        let first = &pattern[0];
        let rest = &pattern[1..];

        if first.is_wildcard() {
            // Wildcard pattern: check all constructors for this type
            let all_ctors = self.all_constructors(&first.ty);

            // Check if type is non-exhaustive (open types like int)
            if all_ctors.iter().any(|c| matches!(c, Constructor::NonExhaustive)) {
                // For non-exhaustive types, wildcard is always useful if matrix doesn't
                // have a wildcard covering all cases
                let has_full_coverage = matrix.rows.iter().any(|row| {
                    row.first().map(|p| p.is_wildcard()).unwrap_or(false)
                });
                return !has_full_coverage;
            }

            // For exhaustive types, check if any constructor is useful
            for ctor in &all_ctors {
                let specialized_matrix = self.specialize_matrix(matrix, ctor, &first.ty);
                let specialized_pattern = self.specialize_pattern(first, rest, ctor);
                if self.is_useful(&specialized_matrix, &specialized_pattern) {
                    return true;
                }
            }
            false
        } else {
            // Specific constructor: specialize for this constructor
            let specialized_matrix = self.specialize_matrix(matrix, &first.ctor, &first.ty);
            let specialized_pattern = self.specialize_pattern(first, rest, &first.ctor);
            self.is_useful(&specialized_matrix, &specialized_pattern)
        }
    }

    /// Specialize a pattern matrix for a specific constructor.
    /// Only keeps rows that match the constructor, expanding fields.
    fn specialize_matrix(
        &self,
        matrix: &PatternMatrix,
        ctor: &Constructor,
        ty: &Ty,
    ) -> PatternMatrix {
        let mut result = PatternMatrix::new();

        for row in &matrix.rows {
            if row.is_empty() {
                continue;
            }

            let first = &row[0];
            let rest = &row[1..];

            if first.is_wildcard() {
                // Wildcard matches any constructor - expand to wildcards for fields
                let field_types = self.get_constructor_field_types(ctor, ty);
                let wildcard_fields: Vec<DeconstructedPat> = field_types
                    .iter()
                    .map(|ft| DeconstructedPat::wildcard(ft.clone()))
                    .collect();
                let mut new_row = wildcard_fields;
                new_row.extend(rest.iter().cloned());
                result.push_row(new_row);
            } else if self.constructors_match(&first.ctor, ctor) {
                // Same constructor - expand fields
                let mut new_row = first.fields.clone();
                new_row.extend(rest.iter().cloned());
                result.push_row(new_row);
            }
            // Different constructor - skip row
        }

        result
    }

    /// Specialize a single pattern for a constructor.
    fn specialize_pattern(
        &self,
        first: &DeconstructedPat,
        rest: &[DeconstructedPat],
        ctor: &Constructor,
    ) -> Vec<DeconstructedPat> {
        let mut result = if first.is_wildcard() {
            // Expand wildcard to wildcard fields
            let field_types = self.get_constructor_field_types(ctor, &first.ty);
            field_types
                .iter()
                .map(|ft| DeconstructedPat::wildcard(ft.clone()))
                .collect()
        } else {
            first.fields.clone()
        };
        result.extend(rest.iter().cloned());
        result
    }

    /// Get field types for a constructor.
    fn get_constructor_field_types(&self, ctor: &Constructor, ty: &Ty) -> Vec<Ty> {
        match ctor {
            Constructor::Variant(enum_name, variant, _) => {
                self.get_enum_variant_types(enum_name, variant)
            }
            Constructor::Tuple(arity) => match ty {
                Ty::Tuple(tys) => tys.clone(),
                _ => (0..*arity).map(|_| Ty::Any).collect(),
            },
            Constructor::ListCons => {
                let elem_ty = match ty {
                    Ty::List(t) => (**t).clone(),
                    _ => Ty::Any,
                };
                vec![elem_ty, ty.clone()]
            }
            _ => vec![],
        }
    }

    /// Check if two constructors match.
    fn constructors_match(&self, c1: &Constructor, c2: &Constructor) -> bool {
        match (c1, c2) {
            (Constructor::Variant(e1, v1, _), Constructor::Variant(e2, v2, _)) => {
                e1 == e2 && v1 == v2
            }
            (Constructor::Bool(b1), Constructor::Bool(b2)) => b1 == b2,
            (Constructor::Tuple(n1), Constructor::Tuple(n2)) => n1 == n2,
            (Constructor::ListCons, Constructor::ListCons) => true,
            (Constructor::ListNil, Constructor::ListNil) => true,
            (Constructor::Struct(s1), Constructor::Struct(s2)) => s1 == s2,
            _ => false,
        }
    }

    /// Compute missing patterns for error messages.
    fn compute_missing_patterns(&self, matrix: &PatternMatrix, ty: &Ty) -> Vec<String> {
        let all_ctors = self.all_constructors(ty);

        // Get constructors used in the first column
        let used_ctors: std::collections::HashSet<Constructor> = matrix
            .rows
            .iter()
            .filter_map(|row| row.first())
            .filter(|p| !p.is_wildcard())
            .map(|p| p.ctor.clone())
            .collect();

        // Check if there's a wildcard in the first column
        let has_wildcard = matrix
            .rows
            .iter()
            .filter_map(|row| row.first())
            .any(|p| p.is_wildcard());

        if has_wildcard {
            return vec![];
        }

        // Find missing constructors
        let mut missing = Vec::new();

        for ctor in all_ctors {
            if matches!(ctor, Constructor::NonExhaustive) {
                // Non-exhaustive type needs wildcard pattern
                missing.push("_".to_string());
                break;
            }
            if !used_ctors.contains(&ctor) {
                missing.push(ctor.to_string());
            }
        }

        missing
    }

    /// Check exhaustiveness of match arms and return missing patterns if any.
    pub fn check_exhaustiveness(&self, scrutinee_ty: &Ty, arms: &[MatchArm]) -> Vec<String> {
        // Build pattern matrix from arms (skip guarded patterns)
        let mut matrix = PatternMatrix::new();
        for arm in arms {
            if arm.guard.is_none() {
                let decon = self.deconstruct_pattern(&arm.pattern, scrutinee_ty);
                matrix.push_row(vec![decon]);
            }
        }

        // Check if wildcard pattern is useful (meaning match is non-exhaustive)
        let wildcard = vec![DeconstructedPat::wildcard(scrutinee_ty.clone())];
        if self.is_useful(&matrix, &wildcard) {
            self.compute_missing_patterns(&matrix, scrutinee_ty)
        } else {
            vec![]
        }
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

// =============================================================================
// AST Annotation (fill in inferred_type_args)
// =============================================================================

impl TypeChecker {
    /// Annotate a module by filling in inferred_type_args on Call expressions.
    pub fn annotate_module(&mut self, module: &Module) -> Module {
        Module {
            name: module.name.clone(),
            items: module.items.iter().map(|item| self.annotate_item(item)).collect(),
            source: module.source.clone(),
        }
    }

    fn annotate_item(&mut self, item: &Item) -> Item {
        match item {
            Item::Function(func) => Item::Function(self.annotate_function(func)),
            Item::Impl(impl_block) => Item::Impl(self.annotate_impl_block(impl_block)),
            // Other items don't contain expressions
            _ => item.clone(),
        }
    }

    fn annotate_function(&mut self, func: &Function) -> Function {
        Function {
            name: func.name.clone(),
            type_params: func.type_params.clone(),
            params: func.params.clone(),
            guard: func.guard.as_ref().map(|g| Box::new(self.annotate_expr(g))),
            return_type: func.return_type.clone(),
            body: self.annotate_block(&func.body),
            is_pub: func.is_pub,
            span: func.span.clone(),
        }
    }

    fn annotate_impl_block(&mut self, impl_block: &ImplBlock) -> ImplBlock {
        ImplBlock {
            type_name: impl_block.type_name.clone(),
            methods: impl_block.methods.iter().map(|m| self.annotate_function(m)).collect(),
        }
    }

    fn annotate_block(&mut self, block: &Block) -> Block {
        Block {
            stmts: block.stmts.iter().map(|s| self.annotate_stmt(s)).collect(),
            expr: block.expr.as_ref().map(|e| Box::new(self.annotate_expr(e))),
        }
    }

    fn annotate_stmt(&mut self, stmt: &Stmt) -> Stmt {
        match stmt {
            Stmt::Let { pattern, ty, value } => Stmt::Let {
                pattern: pattern.clone(),
                ty: ty.clone(),
                value: self.annotate_expr(value),
            },
            Stmt::Expr(e) => Stmt::Expr(self.annotate_expr(e)),
        }
    }

    fn annotate_expr(&mut self, expr: &Expr) -> Expr {
        match expr {
            Expr::Call { func, type_args, args, .. } => {
                let annotated_func = Box::new(self.annotate_expr(func));
                let annotated_args: Vec<Expr> = args.iter().map(|a| self.annotate_expr(a)).collect();

                // If explicit type_args provided, no need to infer
                if !type_args.is_empty() {
                    return Expr::Call {
                        func: annotated_func,
                        type_args: type_args.clone(),
                        inferred_type_args: vec![],
                        args: annotated_args,
                    };
                }

                // Try to infer type args for generic functions
                let inferred = self.infer_type_args_for_call(func, args);

                Expr::Call {
                    func: annotated_func,
                    type_args: type_args.clone(),
                    inferred_type_args: inferred,
                    args: annotated_args,
                }
            }

            Expr::MethodCall { receiver, method, args, resolved_module, .. } => {
                Expr::MethodCall {
                    receiver: Box::new(self.annotate_expr(receiver)),
                    method: method.clone(),
                    args: args.iter().map(|a| self.annotate_expr(a)).collect(),
                    resolved_module: resolved_module.clone(),
                    inferred_type_args: vec![], // TODO: implement method type inference
                }
            }

            // Recursively annotate compound expressions
            Expr::Binary { op, left, right } => Expr::Binary {
                op: *op,
                left: Box::new(self.annotate_expr(left)),
                right: Box::new(self.annotate_expr(right)),
            },

            Expr::Unary { op, expr } => Expr::Unary {
                op: *op,
                expr: Box::new(self.annotate_expr(expr)),
            },

            Expr::If { cond, then_block, else_block } => Expr::If {
                cond: Box::new(self.annotate_expr(cond)),
                then_block: self.annotate_block(then_block),
                else_block: else_block.as_ref().map(|b| self.annotate_block(b)),
            },

            Expr::Match { expr, arms } => Expr::Match {
                expr: Box::new(self.annotate_expr(expr)),
                arms: arms.iter().map(|arm| MatchArm {
                    pattern: arm.pattern.clone(),
                    guard: arm.guard.as_ref().map(|g| Box::new(self.annotate_expr(g))),
                    body: self.annotate_expr(&arm.body),
                }).collect(),
            },

            Expr::Block(block) => Expr::Block(self.annotate_block(block)),

            Expr::Tuple(exprs) => Expr::Tuple(exprs.iter().map(|e| self.annotate_expr(e)).collect()),

            Expr::List(exprs) => Expr::List(exprs.iter().map(|e| self.annotate_expr(e)).collect()),

            Expr::FieldAccess { expr, field } => Expr::FieldAccess {
                expr: Box::new(self.annotate_expr(expr)),
                field: field.clone(),
            },

            Expr::StructInit { name, fields, base } => Expr::StructInit {
                name: name.clone(),
                fields: fields.iter().map(|(n, e)| (n.clone(), self.annotate_expr(e))).collect(),
                base: base.as_ref().map(|b| Box::new(self.annotate_expr(b))),
            },

            Expr::EnumVariant { type_name, variant, args } => Expr::EnumVariant {
                type_name: type_name.clone(),
                variant: variant.clone(),
                args: args.iter().map(|a| self.annotate_expr(a)).collect(),
            },

            Expr::Closure { params, body } => Expr::Closure {
                params: params.clone(),
                body: self.annotate_block(body),
            },

            Expr::Spawn(e) => Expr::Spawn(Box::new(self.annotate_expr(e))),
            Expr::SpawnClosure(block) => Expr::SpawnClosure(self.annotate_block(block)),

            Expr::Send { to, msg } => Expr::Send {
                to: Box::new(self.annotate_expr(to)),
                msg: Box::new(self.annotate_expr(msg)),
            },

            Expr::Receive { arms, timeout } => Expr::Receive {
                arms: arms.iter().map(|arm| MatchArm {
                    pattern: arm.pattern.clone(),
                    guard: arm.guard.as_ref().map(|g| Box::new(self.annotate_expr(g))),
                    body: self.annotate_expr(&arm.body),
                }).collect(),
                timeout: timeout.as_ref().map(|(t, b)| (Box::new(self.annotate_expr(t)), self.annotate_block(b))),
            },

            Expr::Pipe { left, right } => Expr::Pipe {
                left: Box::new(self.annotate_expr(left)),
                right: Box::new(self.annotate_expr(right)),
            },

            Expr::Try { expr } => Expr::Try {
                expr: Box::new(self.annotate_expr(expr)),
            },

            Expr::ExternCall { module, function, args } => Expr::ExternCall {
                module: module.clone(),
                function: function.clone(),
                args: args.iter().map(|a| self.annotate_expr(a)).collect(),
            },

            Expr::BitString(segments) => Expr::BitString(
                segments.iter().map(|seg| ast::BitStringSegment {
                    value: Box::new(self.annotate_expr(&seg.value)),
                    size: seg.size.as_ref().map(|s| Box::new(self.annotate_expr(s))),
                    segment_type: seg.segment_type.clone(),
                    endianness: seg.endianness.clone(),
                    signedness: seg.signedness.clone(),
                }).collect()
            ),

            Expr::Return(e) => Expr::Return(e.as_ref().map(|e| Box::new(self.annotate_expr(e)))),

            // Simple expressions that don't need annotation
            Expr::Int(_) | Expr::String(_) | Expr::Atom(_)
            | Expr::Bool(_) | Expr::Unit | Expr::Ident(_) | Expr::Path { .. } => expr.clone(),

            Expr::StringInterpolation(parts) => {
                let annotated_parts = parts.iter().map(|part| {
                    match part {
                        StringPart::Literal(s) => StringPart::Literal(s.clone()),
                        StringPart::Expr(e) => StringPart::Expr(Box::new(self.annotate_expr(e))),
                    }
                }).collect();
                Expr::StringInterpolation(annotated_parts)
            }
        }
    }

    /// Infer type arguments for a generic function call.
    fn infer_type_args_for_call(&mut self, func: &Expr, args: &[Expr]) -> Vec<ast::Type> {
        // Get function info
        let fn_info = match func {
            Expr::Ident(name) => self.env.get_function(name).cloned(),
            Expr::Path { segments } if segments.len() == 2 => {
                let qualified_name = format!("{}::{}", segments[0], segments[1]);
                self.env.get_function(&qualified_name).cloned()
            }
            _ => None,
        };

        let fn_info = match fn_info {
            Some(info) if !info.type_params.is_empty() => info,
            _ => return vec![], // Not a generic function
        };

        // Clear substitutions for a fresh inference
        self.substitutions.clear();

        // Create fresh inference variables for type params
        let mut type_var_map: HashMap<String, u32> = HashMap::new();
        for type_param in &fn_info.type_params {
            let var_id = self.infer_counter;
            self.infer_counter += 1;
            type_var_map.insert(type_param.name.clone(), var_id);
        }

        // Substitute type params in function signature with inference vars
        let subst: HashMap<String, Ty> = type_var_map.iter()
            .map(|(name, id)| (name.clone(), Ty::Infer(*id)))
            .collect();

        let instantiated_params: Vec<Ty> = fn_info.params.iter()
            .map(|(_, ty)| ty.substitute(&subst))
            .collect();

        // Unify each argument type with parameter type
        for (arg, param_ty) in args.iter().zip(instantiated_params.iter()) {
            if let Ok(arg_ty) = self.infer_expr(arg) {
                let _ = self.unify(&arg_ty, param_ty);
            }
        }

        // Extract resolved types for each type param
        fn_info.type_params.iter()
            .map(|tp| {
                if let Some(&var_id) = type_var_map.get(&tp.name) {
                    let resolved = self.apply_substitutions(&Ty::Infer(var_id));
                    resolved.to_ast_type()
                } else {
                    ast::Type::Any
                }
            })
            .collect()
    }
}

/// Type check a module and return any errors.
pub fn check_module(module: &Module) -> TypeResult<()> {
    let mut checker = TypeChecker::new();
    checker.check_module(module)
}

/// Type check multiple modules with shared type information.
/// This allows cross-module type references (e.g., using enums from another module).
/// Returns annotated modules with inferred type arguments filled in.
pub fn check_modules(modules: &[Module]) -> Vec<(String, TypeResult<Module>)> {
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

        // Validate trait implementations for this module
        for item in &module.items {
            if let Item::TraitImpl(impl_def) = item {
                checker.validate_trait_impl(impl_def);
            }
        }

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
            // Fourth pass: annotate the AST with inferred type args
            let annotated = checker.annotate_module(module);
            Ok(annotated)
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
                ..
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
            Expr::Try { expr } => {
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
            Expr::StringInterpolation(parts) => {
                for part in parts {
                    if let StringPart::Expr(e) = part {
                        self.resolve_expr(e);
                    }
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
            Expr::StringInterpolation(_) => Ty::String,
            Expr::Int(_) => Ty::Int,
            Expr::Bool(_) => Ty::Bool,
            Expr::Atom(name) => Ty::AtomLiteral(name.clone()),
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
            Expr::Try { expr } => {
                // Return the inner type of Result<T, _> or Option<T>
                let inner_ty = self.infer_expr_type(expr);
                if let Ty::Named { name, args, .. } = &inner_ty {
                    if (name == "Result" || name == "Option") && !args.is_empty() {
                        return args[0].clone();
                    }
                }
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
            ast::Type::Float => Ty::Float,
            ast::Type::String => Ty::String,
            ast::Type::Atom => Ty::Atom,
            ast::Type::Bool => Ty::Bool,
            ast::Type::Unit => Ty::Unit,
            ast::Type::Binary => Ty::Binary,
            ast::Type::Pid => Ty::Pid,
            ast::Type::Ref => Ty::Ref,
            ast::Type::Map => Ty::RawMap,
            ast::Type::Tuple(tys) => {
                Ty::Tuple(tys.iter().map(|t| self.ast_type_to_ty(t)).collect())
            }
            ast::Type::List(t) => Ty::List(Box::new(self.ast_type_to_ty(t))),
            ast::Type::Named { name, type_args } => {
                match name.as_str() {
                    "int" => Ty::Int,
                    "float" => Ty::Float,
                    "string" => Ty::String,
                    "atom" => Ty::Atom,
                    "bool" => Ty::Bool,
                    "pid" => Ty::Pid,
                    "ref" => Ty::Ref,
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
            ast::Type::AssociatedType { base, name } => {
                // MethodResolver doesn't resolve Self::X - just keep as-is
                Ty::AssociatedType {
                    base: base.clone(),
                    name: name.clone(),
                }
            }
            ast::Type::Any => Ty::Any,
            ast::Type::AtomLiteral(name) => Ty::AtomLiteral(name.clone()),
            ast::Type::Union(tys) => {
                Ty::Union(tys.iter().map(|t| self.ast_type_to_ty(t)).collect())
            }
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

    #[test]
    fn test_extern_function_type_checking() {
        // Test that extern function stubs enable type checking of FFI calls
        let result = parse_and_check(r#"
            mod test {
                extern mod erlang {
                    fn abs(x: int) -> int;
                    fn self() -> pid;
                }

                fn test() -> int {
                    :erlang::abs(-5)
                }
            }
        "#);
        assert!(result.is_ok());
    }

    #[test]
    fn test_extern_function_arity_lookup() {
        // With arity-based lookup, calling with wrong arity finds no stub
        // and returns Any (no type checking). This test verifies that
        // the correct arity IS found and type-checked.
        let result = parse_and_check(r#"
            mod test {
                extern mod erlang {
                    fn abs(x: int) -> int;
                }

                fn test() -> int {
                    // Correct arity - should type check and return int
                    :erlang::abs(1)
                }
            }
        "#);
        assert!(result.is_ok());
    }

    #[test]
    fn test_extern_function_wrong_arg_type() {
        // Extern function with wrong argument type should error
        let result = parse_and_check(r#"
            mod test {
                extern mod erlang {
                    fn abs(x: int) -> int;
                }

                fn test() -> int {
                    :erlang::abs("hello")
                }
            }
        "#);
        assert!(result.is_err());
    }

    #[test]
    fn test_extern_function_return_type() {
        // Extern function return type should be used in type checking
        let result = parse_and_check(r#"
            mod test {
                extern mod erlang {
                    fn abs(x: int) -> int;
                }

                fn test() -> string {
                    :erlang::abs(-5)
                }
            }
        "#);
        assert!(result.is_err());
    }

    #[test]
    fn test_extern_maps_module() {
        // Test extern module for Erlang's maps module
        let result = parse_and_check(r#"
            mod test {
                extern mod maps {
                    fn new() -> any;
                    fn get(key: any, map: any) -> any;
                }

                fn test() -> any {
                    let m = :maps::new();
                    :maps::get(:foo, m)
                }
            }
        "#);
        assert!(result.is_ok());
    }

    // ========== Union Type Tests ==========

    #[test]
    fn test_atom_literal_return_type() {
        let result = parse_and_check(r#"
            mod test {
                fn return_ok() -> :ok {
                    :ok
                }
            }
        "#);
        assert!(result.is_ok());
    }

    #[test]
    fn test_atom_literal_wrong_return() {
        let result = parse_and_check(r#"
            mod test {
                fn return_ok() -> :ok {
                    :error
                }
            }
        "#);
        assert!(result.is_err());
    }

    #[test]
    fn test_atom_literal_compatible_with_atom() {
        let result = parse_and_check(r#"
            mod test {
                fn return_atom() -> atom {
                    :hello
                }
            }
        "#);
        assert!(result.is_ok());
    }

    #[test]
    fn test_union_type_ok_or_error() {
        let result = parse_and_check(r#"
            mod test {
                fn maybe_fail(succeed: bool) -> :ok | :error {
                    if succeed {
                        :ok
                    } else {
                        :error
                    }
                }
            }
        "#);
        assert!(result.is_ok());
    }

    #[test]
    fn test_union_type_int_or_string() {
        let result = parse_and_check(r#"
            mod test {
                fn get_value(use_int: bool) -> int | string {
                    if use_int {
                        42
                    } else {
                        "hello"
                    }
                }
            }
        "#);
        assert!(result.is_ok());
    }

    #[test]
    fn test_union_type_value_matches_variant() {
        // :ok should be assignable to :ok | :error
        let result = parse_and_check(r#"
            mod test {
                fn returns_union() -> :ok | :error {
                    :ok
                }
            }
        "#);
        assert!(result.is_ok());
    }

    #[test]
    fn test_union_type_wrong_variant() {
        // :wrong should NOT be assignable to :ok | :error
        let result = parse_and_check(r#"
            mod test {
                fn returns_union() -> :ok | :error {
                    :wrong
                }
            }
        "#);
        assert!(result.is_err());
    }

    #[test]
    fn test_if_creates_union_from_branches() {
        // If branches with different types should create a union
        let result = parse_and_check(r#"
            mod test {
                fn infer_union(b: bool) -> int | string {
                    if b { 1 } else { "two" }
                }
            }
        "#);
        assert!(result.is_ok());
    }

    #[test]
    fn test_union_with_three_variants() {
        let result = parse_and_check(r#"
            mod test {
                fn three_way(x: int) -> :a | :b | :c {
                    if x == 1 {
                        :a
                    } else {
                        if x == 2 {
                            :b
                        } else {
                            :c
                        }
                    }
                }
            }
        "#);
        assert!(result.is_ok());
    }

    // ========== Type Alias Tests ==========

    #[test]
    fn test_simple_type_alias() {
        let result = parse_and_check(r#"
            mod test {
                type UserId = int;

                fn get_user(id: UserId) -> UserId {
                    id
                }
            }
        "#);
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_alias_with_union() {
        let result = parse_and_check(r#"
            mod test {
                type Status = :ok | :error;

                fn get_status(success: bool) -> Status {
                    if success { :ok } else { :error }
                }
            }
        "#);
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_alias_resolves_correctly() {
        // Using alias should be same as using the underlying type
        let result = parse_and_check(r#"
            mod test {
                type Count = int;

                fn add(a: Count, b: Count) -> Count {
                    a + b
                }

                fn use_it() -> int {
                    add(1, 2)
                }
            }
        "#);
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_alias_wrong_type() {
        // Type alias should enforce the underlying type
        let result = parse_and_check(r#"
            mod test {
                type UserId = int;

                fn bad() -> UserId {
                    "not an int"
                }
            }
        "#);
        assert!(result.is_err());
    }

    #[test]
    fn test_pub_type_alias() {
        let result = parse_and_check(r#"
            mod test {
                pub type Result = :ok | :error;

                pub fn get_result() -> Result {
                    :ok
                }
            }
        "#);
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_alias_tuple() {
        let result = parse_and_check(r#"
            mod test {
                type Point = (int, int);

                fn origin() -> Point {
                    (0, 0)
                }
            }
        "#);
        assert!(result.is_ok());
    }
}
