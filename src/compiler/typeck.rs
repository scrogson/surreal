//! Type checker for Dream.
//!
//! Performs semantic analysis to validate types across the program.
//! The type checker runs after parsing and before code generation.

use std::collections::HashMap;

use crate::compiler::ast::{
    self, BinOp, Block, Expr, Function, ImplBlock, Item, MatchArm, Module, Pattern, Stmt, UnaryOp,
};

/// Type checking errors.
#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub hint: Option<String>,
}

impl TypeError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            hint: None,
        }
    }

    pub fn with_hint(message: impl Into<String>, hint: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            hint: Some(hint.into()),
        }
    }
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)?;
        if let Some(hint) = &self.hint {
            write!(f, "\n  hint: {}", hint)?;
        }
        Ok(())
    }
}

impl std::error::Error for TypeError {}

pub type TypeResult<T> = Result<T, TypeError>;

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

    /// Substitute type variables according to a mapping.
    pub fn substitute(&self, subst: &HashMap<String, Ty>) -> Ty {
        match self {
            Ty::Var(name) => subst.get(name).cloned().unwrap_or_else(|| self.clone()),
            Ty::Tuple(tys) => Ty::Tuple(tys.iter().map(|t| t.substitute(subst)).collect()),
            Ty::List(t) => Ty::List(Box::new(t.substitute(subst))),
            Ty::Named { name, module, args } => Ty::Named {
                name: name.clone(),
                module: module.clone(),
                args: args.iter().map(|t| t.substitute(subst)).collect(),
            },
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
        }
    }
}

/// Information about a struct type.
#[derive(Debug, Clone)]
pub struct StructInfo {
    pub name: String,
    pub type_params: Vec<String>,
    pub fields: Vec<(String, Ty)>,
}

/// Information about an enum type.
#[derive(Debug, Clone)]
pub struct EnumInfo {
    pub name: String,
    pub type_params: Vec<String>,
    pub variants: Vec<(String, Vec<Ty>)>,
}

/// Information about a function.
#[derive(Debug, Clone)]
pub struct FnInfo {
    pub name: String,
    pub type_params: Vec<String>,
    pub params: Vec<(String, Ty)>,
    pub ret: Ty,
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
}

impl TypeEnv {
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a child scope that inherits from this environment.
    pub fn child(&self) -> Self {
        Self {
            vars: HashMap::new(),
            structs: self.structs.clone(),
            enums: self.enums.clone(),
            functions: self.functions.clone(),
            methods: self.methods.clone(),
            type_aliases: self.type_aliases.clone(),
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
    pub fn get_enum(&self, name: &str) -> Option<&EnumInfo> {
        self.enums.get(name)
    }

    /// Get function info.
    pub fn get_function(&self, name: &str) -> Option<&FnInfo> {
        self.functions.get(name)
    }

    /// Get method info.
    pub fn get_method(&self, type_name: &str, method_name: &str) -> Option<&FnInfo> {
        self.methods.get(&(type_name.to_string(), method_name.to_string()))
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
    /// Type variable substitutions from unification
    substitutions: HashMap<u32, Ty>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            env: TypeEnv::new(),
            infer_counter: 0,
            errors: Vec::new(),
            current_return_type: None,
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
        let body_ty = self.check_block(&func.body)?;
        self.env = old_env;

        // Check return type matches
        if !self.types_compatible(&body_ty, &ret_ty) {
            self.error(TypeError::with_hint(
                format!(
                    "function '{}' returns {} but body has type {}",
                    func.name, ret_ty, body_ty
                ),
                format!("expected {}, found {}", ret_ty, body_ty),
            ));
        }

        self.current_return_type = None;
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
                        self.error(TypeError::with_hint(
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
                }
            }
            Pattern::List(pats) => {
                if let Ty::List(elem_ty) = ty {
                    for p in pats {
                        self.bind_pattern(p, elem_ty)?;
                    }
                }
            }
            Pattern::ListCons { head, tail } => {
                if let Ty::List(elem_ty) = ty {
                    self.bind_pattern(head, elem_ty)?;
                    self.bind_pattern(tail, ty)?;
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
            Expr::Call { func, args } => {
                self.infer_call(func, args)
            }

            // Method calls
            Expr::MethodCall { receiver, method, args } => {
                let recv_ty = self.infer_expr(receiver)?;
                self.infer_method_call(&recv_ty, method, args)
            }

            // If expression
            Expr::If { cond, then_block, else_block } => {
                let cond_ty = self.infer_expr(cond)?;
                if !self.types_compatible(&cond_ty, &Ty::Bool) {
                    self.error(TypeError::with_hint(
                        "if condition must be bool",
                        format!("found {}", cond_ty),
                    ));
                }

                let then_ty = self.check_block(then_block)?;

                if let Some(else_blk) = else_block {
                    let else_ty = self.check_block(else_blk)?;
                    if !self.types_compatible(&then_ty, &else_ty) {
                        self.error(TypeError::with_hint(
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
                            self.error(TypeError::with_hint(
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
                                self.error(TypeError::with_hint(
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
                    if let Some((_, expected_tys)) = info.variants.iter().find(|(v, _)| v == variant) {
                        if args.len() != expected_tys.len() {
                            self.error(TypeError::new(format!(
                                "variant '{}' expects {} arguments, got {}",
                                variant,
                                expected_tys.len(),
                                args.len()
                            )));
                        }
                        for (arg, expected_ty) in args.iter().zip(expected_tys.iter()) {
                            let arg_ty = self.infer_expr(arg)?;
                            if !self.types_compatible(&arg_ty, expected_ty) {
                                self.error(TypeError::with_hint(
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
                    Ok(Ty::Named {
                        name: enum_name.clone(),
                        module: None,
                        args: vec![],
                    })
                } else {
                    // Could be a variant without a type name (e.g., Some(x))
                    // Search all enums for this variant
                    for (name, info) in &self.env.enums.clone() {
                        if info.variants.iter().any(|(v, _)| v == variant) {
                            return Ok(Ty::Named {
                                name: name.clone(),
                                module: None,
                                args: vec![],
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
                        self.error(TypeError::with_hint(
                            "return type mismatch",
                            format!("expected {}, found {}", expected, ret_ty),
                        ));
                    }
                }

                Ok(Ty::Unit) // return expression itself is unit
            }

            // Spawn
            Expr::Spawn(_) | Expr::SpawnClosure(_) => Ok(Ty::Pid),

            // Send
            Expr::Send { to, msg: _ } => {
                let to_ty = self.infer_expr(to)?;
                if !self.types_compatible(&to_ty, &Ty::Pid) {
                    self.error(TypeError::with_hint(
                        "send target must be a pid",
                        format!("found {}", to_ty),
                    ));
                }
                Ok(Ty::Any) // send returns the message
            }

            // Receive
            Expr::Receive { arms, timeout: _ } => {
                // For receive, just check the arms and return Any
                // since we don't know message types statically
                for arm in arms {
                    self.check_block(&Block {
                        stmts: vec![],
                        expr: Some(Box::new(arm.body.clone())),
                    })?;
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
                    self.error(TypeError::with_hint(
                        format!("operator {} requires int operands", op),
                        format!("left operand is {}", left),
                    ));
                }
                if !self.types_compatible(right, &Ty::Int) {
                    self.error(TypeError::with_hint(
                        format!("operator {} requires int operands", op),
                        format!("right operand is {}", right),
                    ));
                }
                Ok(Ty::Int)
            }

            // Comparison: T -> T -> bool
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                if !self.types_compatible(left, right) {
                    self.error(TypeError::with_hint(
                        "comparison operands must have the same type",
                        format!("left: {}, right: {}", left, right),
                    ));
                }
                Ok(Ty::Bool)
            }

            // Logical: bool -> bool -> bool
            BinOp::And | BinOp::Or => {
                if !self.types_compatible(left, &Ty::Bool) {
                    self.error(TypeError::with_hint(
                        format!("operator {} requires bool operands", op),
                        format!("left operand is {}", left),
                    ));
                }
                if !self.types_compatible(right, &Ty::Bool) {
                    self.error(TypeError::with_hint(
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
                    self.error(TypeError::with_hint(
                        "negation requires int operand",
                        format!("found {}", ty),
                    ));
                }
                Ok(Ty::Int)
            }
            UnaryOp::Not => {
                if !self.types_compatible(ty, &Ty::Bool) {
                    self.error(TypeError::with_hint(
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
                    // Check argument count
                    if args.len() != info.params.len() {
                        self.error(TypeError::new(format!(
                            "function '{}' expects {} arguments, got {}",
                            name,
                            info.params.len(),
                            args.len()
                        )));
                    }

                    // Check argument types
                    for (arg, (_, param_ty)) in args.iter().zip(info.params.iter()) {
                        let arg_ty = self.infer_expr(arg)?;
                        if !self.types_compatible(&arg_ty, param_ty) {
                            self.error(TypeError::with_hint(
                                format!("type mismatch in call to '{}'", name),
                                format!("expected {}, found {}", param_ty, arg_ty),
                            ));
                        }
                    }

                    Ok(info.ret.clone())
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
                // Check argument count (excluding self)
                let expected_args = info.params.len().saturating_sub(1);
                if args.len() != expected_args {
                    self.error(TypeError::new(format!(
                        "method '{}' expects {} arguments, got {}",
                        method, expected_args, args.len()
                    )));
                }

                // Check argument types
                for (arg, (_, param_ty)) in args.iter().zip(info.params.iter().skip(1)) {
                    let arg_ty = self.infer_expr(arg)?;
                    if !self.types_compatible(&arg_ty, param_ty) {
                        self.error(TypeError::with_hint(
                            format!("type mismatch in method call '{}'", method),
                            format!("expected {}, found {}", param_ty, arg_ty),
                        ));
                    }
                }

                return Ok(info.ret.clone());
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
                    self.error(TypeError::with_hint(
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
                    self.error(TypeError::with_hint(
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
