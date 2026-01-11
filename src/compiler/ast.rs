//! Abstract Syntax Tree types.

use crate::compiler::lexer::Span;

// =============================================================================
// Module Path Types (for Rust-style module resolution)
// =============================================================================

/// Represents a path prefix for relative module resolution.
#[derive(Debug, Clone, PartialEq)]
pub enum PathPrefix {
    /// `crate::` - resolve from package root
    Crate,
    /// `super::` - resolve from parent module
    Super,
    /// `self::` - resolve from current module
    SelfMod,
    /// No prefix - direct module name
    None,
}

/// A module path that can include relative prefixes.
/// Examples:
/// - `crate::db` → Crate prefix + ["db"]
/// - `super::sibling` → Super prefix + ["sibling"]
/// - `io` → None prefix + ["io"]
#[derive(Debug, Clone, PartialEq)]
pub struct ModulePath {
    /// Optional prefix (crate, super, self)
    pub prefix: PathPrefix,
    /// Path segments after the prefix
    pub segments: Vec<String>,
}

impl ModulePath {
    /// Create a simple path with no prefix.
    pub fn simple(name: &str) -> Self {
        ModulePath {
            prefix: PathPrefix::None,
            segments: vec![name.to_string()],
        }
    }

    /// Create a path from segments with no prefix.
    pub fn from_segments(segments: Vec<String>) -> Self {
        ModulePath {
            prefix: PathPrefix::None,
            segments,
        }
    }

    /// Create a crate-prefixed path.
    pub fn crate_path(segments: Vec<String>) -> Self {
        ModulePath {
            prefix: PathPrefix::Crate,
            segments,
        }
    }

    /// Create a super-prefixed path.
    pub fn super_path(segments: Vec<String>) -> Self {
        ModulePath {
            prefix: PathPrefix::Super,
            segments,
        }
    }

    /// Check if this is a simple (non-prefixed) single-segment path.
    pub fn is_simple(&self) -> bool {
        self.prefix == PathPrefix::None && self.segments.len() == 1
    }

    /// Get the module name as a string (for simple paths).
    pub fn as_simple(&self) -> Option<&str> {
        if self.is_simple() {
            self.segments.first().map(|s| s.as_str())
        } else {
            None
        }
    }

    /// Convert to a string representation without resolving relative paths.
    /// Used for error messages and when context isn't available.
    pub fn to_unresolved_string(&self) -> String {
        let prefix_str = match &self.prefix {
            PathPrefix::Crate => "crate::",
            PathPrefix::Super => "super::",
            PathPrefix::SelfMod => "self::",
            PathPrefix::None => "",
        };
        format!("{}{}", prefix_str, self.segments.join("::"))
    }

    /// Get the segments as a joined string (without prefix).
    pub fn segments_string(&self) -> String {
        self.segments.join("::")
    }
}

/// Context for module resolution during compilation.
#[derive(Debug, Clone, Default)]
pub struct ModuleContext {
    /// Package name from dream.toml (e.g., "my_app")
    pub package_name: Option<String>,
    /// Current module's path relative to src/ (e.g., ["users", "auth"] for src/users/auth.dream)
    pub current_path: Vec<String>,
}

impl ModuleContext {
    pub fn new() -> Self {
        ModuleContext::default()
    }

    pub fn with_package(package_name: String) -> Self {
        ModuleContext {
            package_name: Some(package_name),
            current_path: vec![],
        }
    }

    /// Create a context for a specific module within a package.
    /// The module_fqn is the full module name (e.g., "my_app::users::auth").
    /// This extracts the path after the package name (e.g., ["users", "auth"]).
    pub fn for_module(package_name: &str, module_fqn: &str) -> Self {
        let current_path = if module_fqn == package_name {
            // Root module - no path
            vec![]
        } else if let Some(suffix) = module_fqn.strip_prefix(&format!("{}::", package_name)) {
            // Extract path segments after package name
            suffix.split("::").map(|s| s.to_string()).collect()
        } else {
            // Module doesn't match package - use empty path
            vec![]
        };

        ModuleContext {
            package_name: Some(package_name.to_string()),
            current_path,
        }
    }

    /// Resolve a module path to its fully qualified name.
    /// Returns None if resolution fails (e.g., super:: at root level).
    pub fn resolve(&self, path: &ModulePath) -> Option<String> {
        let resolved_segments = match &path.prefix {
            PathPrefix::Crate => {
                // crate:: resolves to package root
                let pkg = self.package_name.as_ref()?;
                let mut segments = vec![pkg.clone()];
                segments.extend(path.segments.clone());
                segments
            }
            PathPrefix::Super => {
                // super:: resolves to parent module
                let pkg = self.package_name.as_ref()?;
                if self.current_path.is_empty() {
                    return None; // Can't go above root
                }
                let mut segments = vec![pkg.clone()];
                segments.extend(self.current_path[..self.current_path.len() - 1].to_vec());
                segments.extend(path.segments.clone());
                segments
            }
            PathPrefix::SelfMod => {
                // self:: resolves to current module
                let pkg = self.package_name.as_ref()?;
                let mut segments = vec![pkg.clone()];
                segments.extend(self.current_path.clone());
                segments.extend(path.segments.clone());
                segments
            }
            PathPrefix::None => {
                // No prefix - just use segments as-is
                path.segments.clone()
            }
        };

        Some(resolved_segments.join("::"))
    }

    /// Get the fully qualified name of the current module.
    pub fn current_module_fqn(&self) -> Option<String> {
        let pkg = self.package_name.as_ref()?;
        if self.current_path.is_empty() {
            Some(pkg.clone())
        } else {
            let mut parts = vec![pkg.clone()];
            parts.extend(self.current_path.clone());
            Some(parts.join("::"))
        }
    }
}

/// A node with its span in the source.
#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }
}

/// A module declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub items: Vec<Item>,
    /// Source code for error reporting (optional)
    pub source: Option<String>,
}

/// Top-level items in a module.
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Function(Function),
    Struct(StructDef),
    Enum(EnumDef),
    /// Type alias: `type Result = :ok | :error;`
    TypeAlias(TypeAlias),
    /// External module declaration: `mod foo;`
    ModDecl(ModDecl),
    /// Use declaration: `use foo::bar;`
    Use(UseDecl),
    /// Impl block: `impl Point { ... }`
    Impl(ImplBlock),
    /// Trait definition: `trait Display { ... }`
    Trait(TraitDef),
    /// Trait implementation: `impl Display for Point { ... }`
    TraitImpl(TraitImpl),
    /// Module-level trait declaration: `impl GenServer;`
    /// Declares that this module implements a trait (functions are in the module itself)
    TraitDecl(TraitDecl),
    /// External module type declarations for FFI: `extern mod erlang { ... }`
    ExternMod(ExternMod),
}

/// Type alias definition.
/// `type Result = :ok | :error;`
/// `type Result<T> = (:ok, T) | :error;`
#[derive(Debug, Clone, PartialEq)]
pub struct TypeAlias {
    pub name: String,
    /// Generic type parameters (e.g., `<T, E>`)
    pub type_params: Vec<TypeParam>,
    /// The aliased type
    pub ty: Type,
    pub is_pub: bool,
}

// =============================================================================
// External Module Declarations (for .dreamt type stub files)
// =============================================================================

/// External module declaration for FFI type stubs.
/// Declares types for functions in external Erlang/Elixir/Gleam modules.
#[derive(Debug, Clone, PartialEq)]
pub struct ExternMod {
    pub name: String,
    pub items: Vec<ExternItem>,
}

/// Items that can appear in an extern mod block.
#[derive(Debug, Clone, PartialEq)]
pub enum ExternItem {
    /// Nested module: `mod socket { ... }`
    Mod(ExternMod),
    /// Function declaration: `fn get<K, V>(key: K, map: Map<K, V>) -> V;`
    Function(ExternFn),
    /// Opaque type declaration: `type Socket;`
    Type(ExternType),
}

/// External function declaration (signature only, no body).
#[derive(Debug, Clone, PartialEq)]
pub struct ExternFn {
    pub name: String,
    pub type_params: Vec<TypeParam>,
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
}

/// External opaque type declaration.
/// Represents a type from an external module that we can't inspect.
#[derive(Debug, Clone, PartialEq)]
pub struct ExternType {
    pub name: String,
    pub type_params: Vec<TypeParam>,
}

/// Impl block for associated functions and methods.
#[derive(Debug, Clone, PartialEq)]
pub struct ImplBlock {
    pub type_name: String,
    pub methods: Vec<Function>,
}

/// Trait definition.
#[derive(Debug, Clone, PartialEq)]
pub struct TraitDef {
    pub name: String,
    /// Type parameters for the trait (e.g., `<T>` in `trait From<T>`)
    pub type_params: Vec<TypeParam>,
    /// Associated types declared in the trait (e.g., `type State;`)
    pub associated_types: Vec<String>,
    pub methods: Vec<TraitMethod>,
}

/// Trait method signature with optional default implementation.
#[derive(Debug, Clone, PartialEq)]
pub struct TraitMethod {
    pub name: String,
    /// Generic type parameters with optional bounds
    pub type_params: Vec<TypeParam>,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    /// Optional default implementation body
    pub body: Option<Block>,
}

/// A type parameter with optional trait bounds.
/// E.g., `T` or `T: Display` or `T: Display + Debug`
#[derive(Debug, Clone, PartialEq)]
pub struct TypeParam {
    pub name: String,
    /// Trait bounds (e.g., ["Display", "Debug"] for `T: Display + Debug`)
    pub bounds: Vec<String>,
}

/// Trait implementation for a type.
#[derive(Debug, Clone, PartialEq)]
pub struct TraitImpl {
    pub trait_name: String,
    /// Type arguments for parameterized traits (e.g., `int` in `impl From<int> for MyType`)
    pub trait_type_args: Vec<Type>,
    pub type_name: String,
    /// Associated type bindings (e.g., `type State = int;`)
    pub type_bindings: Vec<(String, Type)>,
    pub methods: Vec<Function>,
}

/// Module-level trait declaration.
/// Declares that this module implements a trait.
/// The module's functions serve as the trait method implementations.
/// E.g., `impl genserver::GenServer;` or `impl genserver::GenServer { type State = int; }`
#[derive(Debug, Clone, PartialEq)]
pub struct TraitDecl {
    /// The trait being implemented (possibly module-qualified)
    pub trait_name: String,
    /// Associated type bindings (e.g., `type State = int;`)
    pub type_bindings: Vec<(String, Type)>,
}

/// External module declaration.
/// Used to load a module from a separate file.
#[derive(Debug, Clone, PartialEq)]
pub struct ModDecl {
    pub name: String,
    pub is_pub: bool,
}

/// Use declaration for importing names.
#[derive(Debug, Clone, PartialEq)]
pub struct UseDecl {
    pub tree: UseTree,
}

/// A use tree representing what to import.
#[derive(Debug, Clone, PartialEq)]
pub enum UseTree {
    /// Simple path: `use foo::bar;` or `use foo::bar as baz;`
    /// Also: `use crate::db::query;` or `use super::helpers::format;`
    Path {
        module: ModulePath,
        name: String,
        rename: Option<String>,
    },
    /// Glob import: `use foo::*;` or `use crate::*;`
    Glob { module: ModulePath },
    /// Grouped imports: `use foo::{a, b as c};` or `use crate::db::{query, insert};`
    Group {
        module: ModulePath,
        items: Vec<UseTreeItem>,
    },
}

/// An item in a use group.
#[derive(Debug, Clone, PartialEq)]
pub struct UseTreeItem {
    pub name: String,
    pub rename: Option<String>,
}

/// A function definition.
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    /// Generic type parameters with optional bounds (e.g., `<T, U: Display>`)
    pub type_params: Vec<TypeParam>,
    pub params: Vec<Param>,
    /// Guard clause: `when <expr>`
    pub guard: Option<Box<Expr>>,
    pub return_type: Option<Type>,
    pub body: Block,
    pub is_pub: bool,
    /// Source span for error reporting
    pub span: Span,
}

/// A function parameter.
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub pattern: Pattern,
    pub ty: Type,
}

/// A block of statements with optional trailing expression.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
}

/// Statements.
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// Let binding: `let pattern: type = value;`
    Let {
        pattern: Pattern,
        ty: Option<Type>,
        value: Expr,
    },
    /// Expression statement (with semicolon).
    Expr(Expr),
}

/// Part of an interpolated string.
#[derive(Debug, Clone, PartialEq)]
pub enum StringPart {
    /// Literal string segment.
    Literal(String),
    /// Interpolated expression.
    Expr(Box<Expr>),
}

/// Expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Integer literal.
    Int(i64),
    /// String literal.
    String(String),
    /// Interpolated string: `"Hello {name}!"`.
    StringInterpolation(Vec<StringPart>),
    /// Atom literal (e.g., `:ok`).
    Atom(String),
    /// Boolean literal.
    Bool(bool),
    /// Identifier.
    Ident(String),
    /// Binary operation.
    Binary {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// Unary operation.
    Unary { op: UnaryOp, expr: Box<Expr> },
    /// Function call with optional turbofish type arguments: `func::<T>(args)`.
    Call {
        func: Box<Expr>,
        /// Explicit type arguments from turbofish syntax (e.g., `::<Counter>`)
        type_args: Vec<Type>,
        /// Inferred type arguments (filled in by type checker for generic functions)
        inferred_type_args: Vec<Type>,
        args: Vec<Expr>,
    },
    /// Method call: `expr.method(args)` or `expr.method::<T>(args)`.
    MethodCall {
        receiver: Box<Expr>,
        method: String,
        /// Explicit type arguments from turbofish syntax (e.g., `.into::<Wrapper>()`)
        type_args: Vec<Type>,
        args: Vec<Expr>,
        /// Module for UFCS dispatch, filled by resolution pass.
        /// When set, `x.foo(args)` becomes `module::foo(x, args)`.
        /// Currently used for stdlib primitives; extensible to UDTs.
        resolved_module: Option<String>,
        /// Inferred type arguments for generic methods
        inferred_type_args: Vec<Type>,
    },
    /// If expression.
    If {
        cond: Box<Expr>,
        then_block: Block,
        else_block: Option<Block>,
    },
    /// Match expression.
    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    /// Block expression.
    Block(Block),
    /// Tuple expression.
    Tuple(Vec<Expr>),
    /// List expression.
    List(Vec<Expr>),
    /// Struct initialization with optional base for update syntax.
    /// `Point { x: 1, y: 2 }` or `Point { x: 1, ..base }`
    StructInit {
        name: String,
        fields: Vec<(String, Expr)>,
        /// Optional base expression for struct update syntax `..base`
        base: Option<Box<Expr>>,
    },
    /// Enum variant construction: `Some(42)` or `Option::Some(42)` or `Move { x: 1, y: 2 }`.
    EnumVariant {
        /// Optional type name (e.g., `Option` in `Option::Some`)
        type_name: Option<String>,
        /// Variant name (e.g., `Some`)
        variant: String,
        /// Arguments for tuple or struct variants
        args: EnumVariantArgs,
    },
    /// Field access: `expr.field`.
    FieldAccess { expr: Box<Expr>, field: String },
    /// Try operator: `expr?` - early return on Err/None.
    Try { expr: Box<Expr> },
    /// Module path access: `Module::item`.
    Path { segments: Vec<String> },
    /// Spawn expression.
    Spawn(Box<Expr>),
    /// Closure for spawn: `spawn || { ... }`.
    SpawnClosure(Block),
    /// Anonymous function / closure: `fn(x, y) { x + y }`.
    Closure {
        params: Vec<String>,
        body: Block,
    },
    /// Message send: `pid ! message`.
    Send { to: Box<Expr>, msg: Box<Expr> },
    /// Pipe expression: `expr |> func(args)` transforms to `func(expr, args)`.
    Pipe { left: Box<Expr>, right: Box<Expr> },
    /// Receive expression.
    Receive {
        arms: Vec<MatchArm>,
        timeout: Option<(Box<Expr>, Block)>,
    },
    /// Return expression.
    Return(Option<Box<Expr>>),
    /// Unit expression: `()`.
    Unit,
    /// Bit string / binary expression: `<<1, 2, X:16/little>>`.
    BitString(Vec<BitStringSegment<Box<Expr>>>),
    /// External function call: `:erlang::abs(-5)` → erlang:abs(-5) in Core Erlang.
    ExternCall {
        /// The module atom (e.g., "erlang", "lists", "Elixir.Enum")
        module: String,
        /// The function name
        function: String,
        /// Arguments to the function
        args: Vec<Expr>,
    },
}

/// A match arm.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Box<Expr>>,
    pub body: Expr,
}

/// Binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

impl BinOp {
    /// Get the precedence of this operator (higher = binds tighter).
    pub fn precedence(self) -> u8 {
        match self {
            BinOp::Or => 1,
            BinOp::And => 2,
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => 3,
            BinOp::Add | BinOp::Sub => 4,
            BinOp::Mul | BinOp::Div | BinOp::Mod => 5,
        }
    }
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
            BinOp::Eq => write!(f, "=="),
            BinOp::Ne => write!(f, "!="),
            BinOp::Lt => write!(f, "<"),
            BinOp::Le => write!(f, "<="),
            BinOp::Gt => write!(f, ">"),
            BinOp::Ge => write!(f, ">="),
            BinOp::And => write!(f, "&&"),
            BinOp::Or => write!(f, "||"),
        }
    }
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
}

/// Patterns for matching.
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// Wildcard pattern: `_`.
    Wildcard,
    /// Identifier pattern (binds a value).
    Ident(String),
    /// Integer literal pattern.
    Int(i64),
    /// String literal pattern.
    String(String),
    /// Atom literal pattern.
    Atom(String),
    /// Boolean literal pattern.
    Bool(bool),
    /// Tuple pattern.
    Tuple(Vec<Pattern>),
    /// List pattern.
    List(Vec<Pattern>),
    /// List cons pattern: `[head | tail]`.
    ListCons {
        head: Box<Pattern>,
        tail: Box<Pattern>,
    },
    /// Struct pattern.
    Struct {
        name: String,
        fields: Vec<(String, Pattern)>,
    },
    /// Enum variant pattern: `Some(x)`, `None`, or `Move { x, y }`.
    Enum {
        name: String,
        variant: String,
        fields: EnumPatternFields,
    },
    /// Bit string / binary pattern: `<<A:8, B:16/little, Rest/binary>>`.
    BitString(Vec<BitStringSegment<Box<Pattern>>>),
}

// ========== Bit String / Binary Syntax ==========

/// Type specifier for a binary segment
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BitSegmentType {
    /// Integer (default)
    #[default]
    Integer,
    /// IEEE 754 float (32 or 64 bits)
    Float,
    /// Raw binary/bytes
    Binary,
    /// UTF-8 encoded codepoint
    Utf8,
}

/// Endianness for multi-byte values
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BitEndianness {
    /// Big endian (network byte order, default)
    #[default]
    Big,
    /// Little endian
    Little,
}

/// Signedness for integer values
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BitSignedness {
    /// Unsigned (default)
    #[default]
    Unsigned,
    /// Signed (two's complement)
    Signed,
}

/// A segment in a bit string expression or pattern.
/// Syntax: `value:size/type-specifiers`
/// Examples: `X:16`, `Y:8/little`, `Z:32/signed-little`
#[derive(Debug, Clone, PartialEq)]
pub struct BitStringSegment<T> {
    /// The value (expression for construction, pattern for matching)
    pub value: T,
    /// Size in bits (None = default for type, or "rest" for binary)
    pub size: Option<Box<Expr>>,
    /// Type specifier
    pub segment_type: BitSegmentType,
    /// Endianness
    pub endianness: BitEndianness,
    /// Signedness
    pub signedness: BitSignedness,
}

impl<T> BitStringSegment<T> {
    pub fn new(value: T) -> Self {
        Self {
            value,
            size: None,
            segment_type: BitSegmentType::Integer,
            endianness: BitEndianness::Big,
            signedness: BitSignedness::Unsigned,
        }
    }
}

/// Type annotations.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Named type with optional type arguments (e.g., `Foo` or `Option<T>`).
    Named {
        name: String,
        type_args: Vec<Type>,
    },
    /// Type variable (e.g., `T` in a generic context).
    TypeVar(String),
    /// Tuple type.
    Tuple(Vec<Type>),
    /// List type.
    List(Box<Type>),
    /// Pid type.
    Pid,
    /// Reference type.
    Ref,
    /// Integer type.
    Int,
    /// String type.
    String,
    /// Atom type.
    Atom,
    /// Literal atom type (e.g., `:ok` as a type).
    AtomLiteral(std::string::String),
    /// Boolean type.
    Bool,
    /// Float type.
    Float,
    /// Unit type.
    Unit,
    /// Binary type.
    Binary,
    /// Map type (raw Erlang map).
    Map,
    /// Any type (dynamic/untyped).
    Any,
    /// Union type (e.g., `int | string` or `:ok | :error`).
    Union(Vec<Type>),
    /// Function type (e.g., `fn(T, U) -> R`).
    Fn {
        params: Vec<Type>,
        ret: Box<Type>,
    },
    /// Associated type reference (e.g., `Self::State`).
    AssociatedType {
        /// The base type (usually "Self")
        base: String,
        /// The associated type name (e.g., "State")
        name: String,
    },
}

/// Struct definition.
#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub name: String,
    /// Generic type parameters with optional bounds (e.g., `<T: Display>`)
    pub type_params: Vec<TypeParam>,
    pub fields: Vec<(String, Type)>,
    pub is_pub: bool,
}

/// Enum definition.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumDef {
    pub name: String,
    /// Generic type parameters with optional bounds (e.g., `<T: Display, E>`)
    pub type_params: Vec<TypeParam>,
    pub variants: Vec<EnumVariant>,
    pub is_pub: bool,
}

/// The kind of an enum variant.
#[derive(Debug, Clone, PartialEq)]
pub enum VariantKind {
    /// Unit variant: `None`
    Unit,
    /// Tuple variant: `Some(T)`
    Tuple(Vec<Type>),
    /// Struct variant: `Move { x: Int, y: Int }`
    Struct(Vec<(String, Type)>),
}

/// Arguments for enum variant construction expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum EnumVariantArgs {
    /// Unit variant: `None`
    Unit,
    /// Tuple variant: `Some(42)`
    Tuple(Vec<Expr>),
    /// Struct variant: `Move { x: 10, y: 20 }`
    Struct(Vec<(String, Expr)>),
}

/// Fields for enum variant patterns.
#[derive(Debug, Clone, PartialEq)]
pub enum EnumPatternFields {
    /// Unit variant: `None`
    Unit,
    /// Tuple variant: `Some(x)`
    Tuple(Vec<Pattern>),
    /// Struct variant: `Move { x, y }` or `Move { x: px, y: py }`
    Struct(Vec<(String, Pattern)>),
}

/// Enum variant.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub kind: VariantKind,
}
