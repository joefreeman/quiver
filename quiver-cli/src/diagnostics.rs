use ariadne::{Color, Label, Report, ReportKind, Source};
use quiver_compiler::parser::{Error, ErrorKind};

/// Generate a visual diagnostic report using ariadne
pub fn to_report<'a>(
    error: &Error,
    source_id: &'a str,
    _source: &'a str,
) -> Report<'a, (&'a str, std::ops::Range<usize>)> {
    let offset = error.span.map(|s| s.offset).unwrap_or(0);

    // Use yellow for parser errors (non-fatal, no side effects)
    let mut report = Report::build(
        ReportKind::Custom("Error", Color::Yellow),
        source_id,
        offset,
    )
    .with_message(format!("{}", error.kind));

    // Add label with span if available
    if let Some(span) = error.span {
        let range = span.offset..span.offset + span.length.max(1);
        let label = Label::new((source_id, range))
            .with_message(hint(&error.kind))
            .with_color(Color::Yellow);
        report = report.with_label(label);
    }

    // Add help text if available
    if let Some(help) = error.kind.help() {
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
