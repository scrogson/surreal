//! Compiler for the ToyBEAM language.
//!
//! This module contains the lexer, parser, and AST types for a Rust-like
//! language that compiles to ToyBEAM bytecode.

mod ast;
mod error;
mod lexer;
mod parser;
mod token;

pub use ast::*;
pub use error::{ParseError, ParseResult};
pub use lexer::Lexer;
pub use parser::Parser;
pub use token::Token;
