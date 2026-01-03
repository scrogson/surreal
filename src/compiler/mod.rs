//! Compiler for the ToyBEAM language.
//!
//! This module contains the lexer, parser, AST types, and code generator
//! for a Rust-like language that compiles to ToyBEAM bytecode.

mod ast;
mod codegen;
pub mod core_erlang;
mod error;
mod lexer;
mod loader;
mod parser;
mod token;

pub use ast::*;
pub use codegen::{compile, Codegen, CodegenError, CodegenResult};
pub use core_erlang::{emit_core_erlang, CoreErlangEmitter, CoreErlangError};
pub use error::{ParseError, ParseResult};
pub use lexer::Lexer;
pub use loader::{LoadError, LoadResult, ModuleLoader};
pub use parser::Parser;
pub use token::Token;
