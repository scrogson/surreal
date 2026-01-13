//! Compiler for the Dream language.
//!
//! This module contains the lexer, parser, AST types, and code generator
//! for a Rust-like language that compiles to Dream bytecode.

mod ast;
pub mod cfg;
mod codegen;
pub mod core_erlang;
pub mod derive;
mod error;
mod lexer;
mod loader;
mod parser;
mod prelude;
mod token;
pub mod typeck;

pub use ast::*;
pub use cfg::{is_test, should_include};
pub use codegen::{compile, compile_file, Codegen, CodegenError, CodegenResult};
pub use core_erlang::{
    emit_core_erlang, CoreErlangEmitter, CoreErlangError, GenericFunctionRegistry,
    SharedGenericRegistry,
};
pub use error::{CompilerError, ParseError, ParseResult, TypeError, TypeResult};
pub use lexer::Lexer;
pub use loader::{LoadError, LoadResult, ModuleLoader};
pub use parser::Parser;
pub use token::Token;
pub use derive::expand_derives;
pub use typeck::{check_module, check_modules, check_modules_with_metadata, resolve_stdlib_methods, TypeCheckResult};
