//! Abstract Syntax Tree types.

use crate::compiler::lexer::Span;

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
}

/// Top-level items in a module.
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Function(Function),
    Struct(StructDef),
    Enum(EnumDef),
}

/// A function definition.
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: Block,
    pub is_pub: bool,
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

/// Expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Integer literal.
    Int(i64),
    /// String literal.
    String(String),
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
    /// Function call.
    Call { func: Box<Expr>, args: Vec<Expr> },
    /// Method call: `expr.method(args)`.
    MethodCall {
        receiver: Box<Expr>,
        method: String,
        args: Vec<Expr>,
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
    /// Struct initialization.
    StructInit {
        name: String,
        fields: Vec<(String, Expr)>,
    },
    /// Field access: `expr.field`.
    FieldAccess { expr: Box<Expr>, field: String },
    /// Module path access: `Module::item`.
    Path { segments: Vec<String> },
    /// Spawn expression.
    Spawn(Box<Expr>),
    /// Closure for spawn: `spawn || { ... }`.
    SpawnClosure(Block),
    /// Message send: `pid ! message`.
    Send { to: Box<Expr>, msg: Box<Expr> },
    /// Receive expression.
    Receive {
        arms: Vec<MatchArm>,
        timeout: Option<(Box<Expr>, Block)>,
    },
    /// Return expression.
    Return(Option<Box<Expr>>),
    /// Unit expression: `()`.
    Unit,
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
    /// Enum variant pattern.
    Enum {
        name: String,
        variant: String,
        fields: Vec<Pattern>,
    },
}

/// Type annotations.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Named type (e.g., `Foo`).
    Named(String),
    /// Tuple type.
    Tuple(Vec<Type>),
    /// List type.
    List(Box<Type>),
    /// Pid type.
    Pid,
    /// Integer type.
    Int,
    /// String type.
    String,
    /// Atom type.
    Atom,
    /// Boolean type.
    Bool,
    /// Unit type.
    Unit,
}

/// Struct definition.
#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<(String, Type)>,
    pub is_pub: bool,
}

/// Enum definition.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumDef {
    pub name: String,
    pub variants: Vec<EnumVariant>,
    pub is_pub: bool,
}

/// Enum variant.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub fields: Vec<Type>,
}
