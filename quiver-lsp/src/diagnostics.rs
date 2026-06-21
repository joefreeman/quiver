//! Conversion of Quiver compiler/parser errors into LSP diagnostics.

use crate::convert::span_to_range;
use crate::documents::LineIndex;
use quiver_compiler::compiler::LocatedError;
use quiver_compiler::parser::Error as ParseError;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

/// The whole-document start position, used when an error has no span.
fn fallback_range() -> Range {
    Range {
        start: Position::new(0, 0),
        end: Position::new(0, 0),
    }
}

fn error(range: Range, message: String) -> Diagnostic {
    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("quiver".to_string()),
        message,
        ..Default::default()
    }
}

/// Convert a (typecheck) compiler error into a diagnostic, located at its span when known.
pub fn located_error_to_diagnostic(
    err: &LocatedError,
    text: &str,
    index: &LineIndex,
) -> Diagnostic {
    let range = match err.span {
        Some(span) => span_to_range(text, index, span),
        None => fallback_range(),
    };
    error(range, err.to_string())
}

pub fn parse_error_to_diagnostic(err: &ParseError, text: &str, index: &LineIndex) -> Diagnostic {
    let range = match err.span {
        Some(span) => span_to_range(text, index, span),
        None => Range {
            start: Position::new(0, 0),
            end: Position::new(0, 0),
        },
    };

    let mut message = err.kind.to_string();
    if let Some(help) = err.kind.help() {
        message.push_str("\n\n");
        message.push_str(&help);
    }

    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("quiver".to_string()),
        message,
        ..Default::default()
    }
}
