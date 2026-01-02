//! Parse errors.

use crate::compiler::lexer::Span;
use crate::compiler::token::Token;

/// A parse error.
#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl ParseError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }

    pub fn unexpected_token(found: &Token, expected: &str, span: Span) -> Self {
        Self::new(format!("expected {}, found `{}`", expected, found), span)
    }

    pub fn unexpected_eof(expected: &str) -> Self {
        Self::new(format!("unexpected end of input, expected {}", expected), 0..0)
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "error at {:?}: {}", self.span, self.message)
    }
}

impl std::error::Error for ParseError {}

/// Result type for parsing.
pub type ParseResult<T> = Result<T, ParseError>;
