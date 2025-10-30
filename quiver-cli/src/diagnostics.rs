use ariadne::{Color, Label, Report, ReportKind, Source};
use quiver_compiler::parser::{Error, ErrorKind};

/// Generate a visual diagnostic report using ariadne
pub fn to_report<'a>(
    error: &Error,
    source_id: &'a str,
    _source: &'a str,
) -> Report<'a, (&'a str, std::ops::Range<usize>)> {
    let offset = error.span.map(|s| s.offset).unwrap_or(0);

    let mut report =
        Report::build(ReportKind::Error, source_id, offset).with_message(format!("{}", error.kind));

    // Add label with span if available
    if let Some(span) = error.span {
        let range = span.offset..span.offset + span.length.max(1);
        let label = Label::new((source_id, range))
            .with_message(hint(&error.kind))
            .with_color(Color::Red);
        report = report.with_label(label);
    }

    // Add help text if available
    if let Some(help) = help_text(&error.kind) {
        report = report.with_help(help);
    }

    report.finish()
}

/// Print the error using ariadne formatting
pub fn eprint(error: &Error, source_id: &str, source: &str) {
    to_report(error, source_id, source)
        .eprint((source_id, Source::from(source)))
        .unwrap();
}

/// Get a contextual hint message for the error label
fn hint(kind: &ErrorKind) -> String {
    match kind {
        ErrorKind::UnterminatedTuple => "tuple is not closed".to_string(),
        ErrorKind::UnterminatedString => "string is not closed".to_string(),
        ErrorKind::UnterminatedBlock => "block is not closed".to_string(),
        ErrorKind::MissingClosingBrace => "expected '}' here".to_string(),
        ErrorKind::MissingClosingBracket => "expected ']' here".to_string(),
        ErrorKind::MissingClosingParen => "expected ')' here".to_string(),

        ErrorKind::ExpectedPipe => "expected '~>' here".to_string(),
        ErrorKind::InvalidFunctionBody => "function body is incomplete or invalid".to_string(),

        ErrorKind::IntegerMalformed(lit) => format!("'{}' is not a valid integer", lit),
        ErrorKind::HexMalformed(lit) => format!("'{}' is not a valid hex literal", lit),
        ErrorKind::StringEscapeInvalid(esc) => {
            format!("'{}' is not a valid escape sequence", esc)
        }

        ErrorKind::UnexpectedToken { expected, found } => {
            format!("expected {}, but found '{}'", expected, found)
        }
        ErrorKind::UnexpectedEndOfInput { context } => {
            format!("unexpected end while parsing {}", context)
        }

        ErrorKind::ParseError(_) => "parse error occurred here".to_string(),
    }
}

/// Get optional help text with suggestions for fixing the error
fn help_text(kind: &ErrorKind) -> Option<String> {
    match kind {
        ErrorKind::UnterminatedTuple => Some("Add a closing ']' to complete the tuple".to_string()),
        ErrorKind::UnterminatedString => {
            Some("Add a closing '\"' to complete the string".to_string())
        }
        ErrorKind::UnterminatedBlock => Some("Add a closing '}' to complete the block".to_string()),
        ErrorKind::MissingClosingBrace => Some("Add '}' to close the block".to_string()),
        ErrorKind::MissingClosingBracket => Some("Add ']' to close the tuple".to_string()),
        ErrorKind::MissingClosingParen => {
            Some("Add ')' to close the parenthesized expression".to_string())
        }

        ErrorKind::ExpectedPipe => {
            Some("Chains use '~>' to pipe values, e.g., '[1, 2] ~> __add__'".to_string())
        }
        ErrorKind::InvalidFunctionBody => {
            Some("Function body should be a valid expression".to_string())
        }

        ErrorKind::StringEscapeInvalid(esc) => {
            if esc.starts_with('\\') && esc.len() == 2 {
                Some("Valid escape sequences are: \\n \\r \\t \\\\ \\\"".to_string())
            } else {
                Some("Use valid escape sequences: \\n (newline), \\r (return), \\t (tab), \\\\ (backslash), \\\" (quote)".to_string())
            }
        }

        _ => None,
    }
}
