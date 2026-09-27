use ariadne::{Color, Label, Report, ReportKind, Source};
use quiver_compiler::compiler::{Error as CompileError, LocatedError, ModuleSite};
use quiver_compiler::parser::{Error, ErrorKind, SourceSpan};
use quiver_compiler::resolver::ModuleOrigin;
use std::ops::Range;

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

/// A position in one of the sources a compile error spans: the error itself, or an import on
/// the way to it.
struct Frame<'a> {
    source_id: String,
    source: &'a str,
    span: Option<SourceSpan>,
    /// The module this position is in, when it isn't the compiled source.
    module: Option<&'a str>,
}

/// The positions of a compile error, innermost first: where the error is, then each import
/// that reached it, ending in the compiled source.
fn frames<'a>(error: &'a LocatedError, source_id: &str, source: &'a str) -> Vec<Frame<'a>> {
    let mut frames: Vec<Frame> = error
        .modules
        .iter()
        .rev()
        .map(|site| Frame {
            source_id: module_source_id(site),
            source: &site.source,
            span: site.span,
            module: Some(&site.name),
        })
        .collect();
    frames.push(Frame {
        source_id: source_id.to_string(),
        source,
        span: error.span,
        module: None,
    });
    frames
}

/// How a module's source is named in a report: its file, relative to the working directory
/// when under it, or its import name when it has no file (the embedded standard library).
fn module_source_id(site: &ModuleSite) -> String {
    match &site.origin {
        ModuleOrigin::Path(path) => std::env::current_dir()
            .ok()
            .and_then(|cwd| path.strip_prefix(cwd).ok())
            .unwrap_or(path)
            .display()
            .to_string(),
        ModuleOrigin::Virtual => site.name.clone(),
    }
}

/// A span as the character range ariadne expects (spans count bytes).
fn span_range(source: &str, span: SourceSpan) -> Range<usize> {
    let chars = |offset: usize| source[..offset.min(source.len())].chars().count();
    chars(span.offset)..chars(span.offset + span.length.max(1))
}

/// The report's message, the primary label's hint, and help: a module's parse error reads
/// like the compiled source's own.
fn compile_error_text(error: &CompileError) -> (String, Option<String>, Option<String>) {
    match error {
        CompileError::ModuleParse { error, .. } => (
            error.kind.to_string(),
            Some(hint(&error.kind)),
            error.kind.help(),
        ),
        error => (error.to_string(), None, None),
    }
}

/// Print a compile error using ariadne formatting, with a snippet of each source it spans.
pub fn eprint_compile(error: &LocatedError, source_id: &str, source: &str) {
    let frames = frames(error, source_id, source);
    let (message, hint, help) = compile_error_text(&error.error);
    // The error's own position, or — for one with none in its module, such as a failure
    // while evaluating it — the import of that module.
    let primary = frames.iter().position(|frame| frame.span.is_some());
    let (report_id, offset) = match primary {
        Some(index) => (
            frames[index].source_id.clone(),
            frames[index]
                .span
                .map_or(0, |span| span_range(frames[index].source, span).start),
        ),
        None => (source_id.to_string(), 0),
    };

    let mut report = Report::build(ReportKind::Error, report_id, offset).with_message(message);
    for (index, frame) in frames.iter().enumerate() {
        let Some(span) = frame.span else { continue };
        let label = Label::new((frame.source_id.clone(), span_range(frame.source, span)))
            .with_color(if Some(index) == primary {
                Color::Red
            } else {
                Color::Blue
            });
        let label = if index == 0 {
            // ariadne draws no underline for a label without a message.
            label.with_message(hint.as_deref().unwrap_or("here"))
        } else {
            // An import on the way to the error: of the module one frame further in.
            let imported = frames[index - 1].module.unwrap_or_default();
            label.with_message(format!("{imported} is imported here"))
        };
        report = report.with_label(label);
    }
    // A failed evaluation names its module itself.
    if frames[0].span.is_none()
        && !matches!(error.error, CompileError::ModuleEvaluationFailed { .. })
        && let Some(module) = frames[0].module
    {
        report = report.with_note(format!("the error is in {module}"));
    }
    if let Some(help) = help {
        report = report.with_help(help);
    }

    let sources: Vec<(String, &str)> = frames
        .iter()
        .map(|frame| (frame.source_id.clone(), frame.source))
        .collect();
    report.finish().eprint(ariadne::sources(sources)).unwrap();
}

/// A compile error as plain text: `file:line:column: message`, then each import that reached
/// it.
pub fn plain_compile_error(error: &LocatedError, source_id: &str, source: &str) -> String {
    let frames = frames(error, source_id, source);
    let location = |frame: &Frame| match frame.span {
        Some(span) => format!("{}:{}:{}", frame.source_id, span.line, span.column),
        None => frame.source_id.clone(),
    };
    let mut text = format!(
        "{}: {}",
        location(&frames[0]),
        compile_error_text(&error.error).0
    );
    for frame in &frames[1..] {
        text.push_str(&format!("\n  imported at {}", location(frame)));
    }
    text
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

        ErrorKind::SpawnBlock => "'@{ ... }' does not state a parameter type".to_string(),

        ErrorKind::StepComma => "',' is not a step separator".to_string(),
        ErrorKind::MissingChainArrow => "expected '~>' or ';' here".to_string(),
        ErrorKind::AssertionOnAlias => "a type alias produces no value to assert on".to_string(),
        ErrorKind::AssertionNotLineFinal => "code may not follow an assertion".to_string(),
        ErrorKind::NestingTooDeep => "nesting starts here".to_string(),

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
