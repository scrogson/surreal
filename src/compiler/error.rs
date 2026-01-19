//! Compiler errors with rich diagnostics.

use crate::compiler::lexer::Span;
use crate::compiler::token::Token;
use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

/// A parse error with source location.
#[derive(Error, Debug, Diagnostic, Clone)]
#[error("{message}")]
#[diagnostic()]
pub struct ParseError {
    pub message: String,

    #[label("here")]
    pub span: SourceSpan,

    #[help]
    pub help: Option<String>,
}

impl ParseError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span: span.into(),
            help: None,
        }
    }

    pub fn with_help(message: impl Into<String>, span: Span, help: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            span: span.into(),
            help: Some(help.into()),
        }
    }

    pub fn unexpected_token(found: &Token, expected: &str, span: Span) -> Self {
        Self::new(format!("expected {}, found `{}`", expected, found), span)
    }

    pub fn unexpected_eof(expected: &str) -> Self {
        Self::new(
            format!("unexpected end of input, expected {}", expected),
            0..0,
        )
    }
}

impl PartialEq for ParseError {
    fn eq(&self, other: &Self) -> bool {
        self.message == other.message
            && self.span.offset() == other.span.offset()
            && self.span.len() == other.span.len()
    }
}

/// Result type for parsing.
pub type ParseResult<T> = Result<T, ParseError>;

/// A type error with source location.
#[derive(Error, Debug, Diagnostic, Clone)]
#[error("{message}")]
#[diagnostic(code(surreal::type_error))]
pub struct TypeError {
    pub message: String,

    #[label("type mismatch here")]
    pub span: Option<SourceSpan>,

    #[help]
    pub help: Option<String>,
}

impl TypeError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            span: None,
            help: None,
        }
    }

    pub fn with_span(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span: Some(span.into()),
            help: None,
        }
    }

    pub fn with_help(message: impl Into<String>, help: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            span: None,
            help: Some(help.into()),
        }
    }
}

/// Result type for type checking.
pub type TypeResult<T> = Result<T, TypeError>;

/// A compiler warning (internal representation during type checking).
#[derive(Debug, Clone)]
pub struct Warning {
    pub message: String,
    pub help: Option<String>,
    /// Module where the warning occurred
    pub module: Option<String>,
    /// Source span for the warning
    pub span: Option<Span>,
}

impl Warning {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            help: None,
            module: None,
            span: None,
        }
    }

    pub fn with_help(message: impl Into<String>, help: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            help: Some(help.into()),
            module: None,
            span: None,
        }
    }

    pub fn with_span(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            help: None,
            module: None,
            span: Some(span),
        }
    }

    pub fn with_help_and_span(message: impl Into<String>, help: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            help: Some(help.into()),
            module: None,
            span: Some(span),
        }
    }

    pub fn in_module(mut self, module: impl Into<String>) -> Self {
        self.module = Some(module.into());
        self
    }
}

/// A compiler warning with source context for rich diagnostics.
#[derive(Error, Debug, Diagnostic)]
#[error("{message}")]
#[diagnostic(severity(warning), code(surreal::warning))]
pub struct CompilerWarning {
    pub message: String,

    #[source_code]
    pub src: NamedSource<String>,

    #[label("this value is unused")]
    pub span: Option<SourceSpan>,

    #[help]
    pub help: Option<String>,
}

impl CompilerWarning {
    /// Create a compiler warning from a Warning with source context.
    pub fn from_warning(
        filename: impl Into<String>,
        source: impl Into<String>,
        warning: Warning,
    ) -> Self {
        let filename: String = filename.into();
        Self {
            message: warning.message,
            src: NamedSource::new(filename, source.into()),
            span: warning.span.map(|s| s.into()),
            help: warning.help,
        }
    }
}

/// A compiler error that can include source code context.
#[derive(Error, Debug, Diagnostic)]
#[error("{message}")]
#[diagnostic(code(surreal::compiler_error))]
pub struct CompilerError {
    pub message: String,

    #[source_code]
    pub src: NamedSource<String>,

    #[label("{label}")]
    pub span: Option<SourceSpan>,

    pub label: String,

    #[help]
    pub help: Option<String>,
}

impl CompilerError {
    /// Create a compiler error from a parse error with source context.
    pub fn parse(filename: impl Into<String>, source: impl Into<String>, err: ParseError) -> Self {
        let filename: String = filename.into();
        Self {
            message: err.message.clone(),
            src: NamedSource::new(filename, source.into()),
            span: Some(err.span),
            label: "here".to_string(),
            help: err.help,
        }
    }

    /// Create a compiler error from a type error with source context.
    pub fn type_error(
        filename: impl Into<String>,
        source: impl Into<String>,
        err: TypeError,
    ) -> Self {
        let filename: String = filename.into();
        Self {
            message: err.message.clone(),
            src: NamedSource::new(filename, source.into()),
            span: err.span,
            label: "type mismatch here".to_string(),
            help: err.help,
        }
    }
}
