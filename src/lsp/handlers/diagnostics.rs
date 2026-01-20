//! Diagnostics handler - converts compiler errors to LSP diagnostics.

use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString};

use crate::compiler::{ParseError, TypeError, Warning};
use crate::lsp::position::LineIndex;

/// Convert compiler errors and warnings to LSP diagnostics.
pub fn publish_diagnostics(
    line_index: &LineIndex,
    parse_errors: &[ParseError],
    type_errors: &[TypeError],
    warnings: &[Warning],
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Convert parse errors
    for error in parse_errors {
        let span = error.span.offset()..(error.span.offset() + error.span.len());
        if let Some(range) = line_index.span_to_range(span) {
            diagnostics.push(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String("parse-error".to_string())),
                code_description: None,
                source: Some("surreal".to_string()),
                message: error.message.clone(),
                related_information: None,
                tags: None,
                data: None,
            });
        }
    }

    // Convert type errors
    for error in type_errors {
        let range = if let Some(ref span) = error.span {
            let s = span.offset()..(span.offset() + span.len());
            line_index.span_to_range(s)
        } else {
            // If no span, put at start of file
            Some(tower_lsp::lsp_types::Range {
                start: tower_lsp::lsp_types::Position { line: 0, character: 0 },
                end: tower_lsp::lsp_types::Position { line: 0, character: 1 },
            })
        };

        if let Some(range) = range {
            let mut message = error.message.clone();
            if let Some(ref help) = error.help {
                message.push_str("\nHelp: ");
                message.push_str(help);
            }

            diagnostics.push(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String("type-error".to_string())),
                code_description: None,
                source: Some("surreal".to_string()),
                message,
                related_information: None,
                tags: None,
                data: None,
            });
        }
    }

    // Convert warnings
    for warning in warnings {
        let range = if let Some(ref span) = warning.span {
            line_index.span_to_range(span.clone())
        } else {
            // If no span, put at start of file
            Some(tower_lsp::lsp_types::Range {
                start: tower_lsp::lsp_types::Position { line: 0, character: 0 },
                end: tower_lsp::lsp_types::Position { line: 0, character: 1 },
            })
        };

        if let Some(range) = range {
            let mut message = warning.message.clone();
            if let Some(ref help) = warning.help {
                message.push_str("\nHelp: ");
                message.push_str(help);
            }

            diagnostics.push(Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::WARNING),
                code: Some(NumberOrString::String("warning".to_string())),
                code_description: None,
                source: Some("surreal".to_string()),
                message,
                related_information: None,
                tags: None,
                data: None,
            });
        }
    }

    diagnostics
}
