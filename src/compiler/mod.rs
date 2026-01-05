//! Compiler for the Dream language.
//!
//! This module contains the lexer, parser, AST types, and code generator
//! for a Rust-like language that compiles to Dream bytecode.

mod ast;
mod codegen;
pub mod core_erlang;
mod error;
mod lexer;
mod loader;
mod parser;
mod prelude;
mod token;
pub mod typeck;

pub use ast::*;
pub use codegen::{compile, compile_file, Codegen, CodegenError, CodegenResult};
pub use core_erlang::{emit_core_erlang, CoreErlangEmitter, CoreErlangError};
pub use error::{CompilerError, ParseError, ParseResult, TypeError, TypeResult};
pub use lexer::Lexer;
pub use loader::{LoadError, LoadResult, ModuleLoader};
pub use parser::Parser;
pub use token::Token;
pub use typeck::{check_module, check_modules, resolve_stdlib_methods};
