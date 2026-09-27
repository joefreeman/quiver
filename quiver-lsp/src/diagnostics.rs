//! Conversion of Quiver compiler/parser errors into LSP diagnostics.

use crate::convert::span_to_range;
use crate::documents::LineIndex;
use quiver_compiler::compiler::{LocatedError, LocatedWarning};
use quiver_compiler::parser::Error as ParseError;
use quiver_compiler::resolver::ModuleOrigin;
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, Position, Range, Url,
};

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
/// An error inside an imported module sits on the import that reached it, with related
/// locations for the positions in module files.
pub fn located_error_to_diagnostic(
    err: &LocatedError,
    text: &str,
    index: &LineIndex,
) -> Diagnostic {
    let range = match err.span {
        Some(span) => span_to_range(text, index, span),
        None => fallback_range(),
    };
    let related: Vec<DiagnosticRelatedInformation> = err
        .modules
        .iter()
        .enumerate()
        .filter_map(|(depth, site)| {
            let ModuleOrigin::Path(path) = &site.origin else {
                return None;
            };
            let span = site.span?;
            let uri = Url::from_file_path(path).ok()?;
            let message = match err.modules.get(depth + 1) {
                Some(next) => format!("{} is imported here", next.name),
                None => "the error is here".to_string(),
            };
            Some(DiagnosticRelatedInformation {
                location: Location {
                    uri,
                    range: span_to_range(&site.source, &LineIndex::new(&site.source), span),
                },
                message,
            })
        })
        .collect();
    Diagnostic {
        related_information: (!related.is_empty()).then_some(related),
        ..error(range, err.to_string())
    }
}

/// Convert a warning in the document into a diagnostic. `None` for a warning with no
/// position in the document.
pub fn warning_to_diagnostic(
    warning: &LocatedWarning,
    text: &str,
    index: &LineIndex,
) -> Option<Diagnostic> {
    Some(Diagnostic {
        range: span_to_range(text, index, warning.span?),
        severity: Some(DiagnosticSeverity::WARNING),
        source: Some("quiver".to_string()),
        message: warning.warning.to_string(),
        ..Default::default()
    })
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
