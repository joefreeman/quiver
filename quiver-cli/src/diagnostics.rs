use ariadne::{Color, Label, Report, ReportKind, Source};
use quiver_compiler::compiler::{Error as CompileError, LocatedError, LocatedWarning, ModuleSite};
use quiver_compiler::parser::{Error, ErrorKind, SourceSpan};
use quiver_compiler::resolver::ModuleOrigin;
use std::io::IsTerminal;
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

/// A position in one of the sources a compile diagnostic spans: the problem itself, or an
/// import on the way to it.
struct Frame<'a> {
    source_id: String,
    source: &'a str,
    span: Option<SourceSpan>,
    /// The module this position is in, when it isn't the compiled source.
    module: Option<&'a str>,
}

impl<'a> Frame<'a> {
    fn in_module(site: &'a ModuleSite) -> Self {
        Frame {
            source_id: module_source_id(site),
            source: &site.source,
            span: site.span,
            module: Some(&site.name),
        }
    }

    fn location(&self) -> String {
        match self.span {
            Some(span) => format!("{}:{}:{}", self.source_id, span.line, span.column),
            None => self.source_id.clone(),
        }
    }
}

/// A compile error or warning, ready to print.
struct Diagnostic<'a> {
    warning: bool,
    message: String,
    /// What the primary label says.
    hint: Option<String>,
    help: Option<String>,
    note: Option<String>,
    /// Innermost first: where the problem is, then each import that reached it.
    frames: Vec<Frame<'a>>,
    /// Further positions in the innermost frame's source, each with what to say about it.
    related: Vec<(SourceSpan, &'static str)>,
}

impl<'a> Diagnostic<'a> {
    fn error(error: &'a LocatedError, source_id: &str, source: &'a str) -> Self {
        let mut frames: Vec<Frame> = error.modules.iter().rev().map(Frame::in_module).collect();
        frames.push(Frame {
            source_id: source_id.to_string(),
            source,
            span: error.span,
            module: None,
        });
        // A module's parse error reads like the compiled source's own.
        let (message, hint, help) = match &error.error {
            CompileError::ModuleParse { error, .. } => (
                error.kind.to_string(),
                Some(hint(&error.kind)),
                error.kind.help(),
            ),
            error => (error.to_string(), None, None),
        };
        // With no position in its module, the report points at the import; say where the
        // problem is, unless the message does (a failed evaluation names its module).
        let note = match frames[0].module {
            Some(module)
                if frames[0].span.is_none()
                    && !matches!(error.error, CompileError::ModuleEvaluationFailed { .. }) =>
            {
                Some(format!("the error is in {module}"))
            }
            _ => None,
        };
        Diagnostic {
            warning: false,
            message,
            hint,
            help,
            note,
            frames,
            related: Vec::new(),
        }
    }

    fn warning(warning: &'a LocatedWarning, source_id: &str, source: &'a str) -> Self {
        let frame = match &warning.module {
            Some(site) => Frame::in_module(site),
            None => Frame {
                source_id: source_id.to_string(),
                source,
                span: warning.span,
                module: None,
            },
        };
        Diagnostic {
            warning: true,
            message: warning.warning.to_string(),
            hint: Some(warning.warning.label().to_string()),
            help: warning.warning.help().map(str::to_string),
            note: None,
            frames: vec![frame],
            related: warning.warning.related(),
        }
    }

    /// Print with ariadne formatting, with a snippet of each source involved.
    fn eprint(self) {
        let (kind, color) = if self.warning {
            (ReportKind::Warning, Color::Yellow)
        } else {
            (ReportKind::Error, Color::Red)
        };
        let frames = &self.frames;
        // The problem's own position, or — for one with none in its module, such as a
        // failure while evaluating it — the import of that module.
        let primary = frames.iter().position(|frame| frame.span.is_some());
        let (report_id, offset) = match primary {
            Some(index) => (
                frames[index].source_id.clone(),
                frames[index]
                    .span
                    .map_or(0, |span| span_range(frames[index].source, span).start),
            ),
            None => (frames[frames.len() - 1].source_id.clone(), 0),
        };

        let mut report = Report::build(kind, report_id, offset).with_message(&self.message);
        for (index, frame) in frames.iter().enumerate() {
            let Some(span) = frame.span else { continue };
            let label = Label::new((frame.source_id.clone(), span_range(frame.source, span)))
                .with_color(if Some(index) == primary {
                    color
                } else {
                    Color::Blue
                });
            let label = if index == 0 {
                // ariadne draws no underline for a label without a message.
                label.with_message(self.hint.as_deref().unwrap_or("here"))
            } else {
                // An import on the way to the problem: of the module one frame further in.
                let imported = frames[index - 1].module.unwrap_or_default();
                label.with_message(format!("{imported} is imported here"))
            };
            report = report.with_label(label);
        }
        for (span, message) in &self.related {
            report = report.with_label(
                Label::new((
                    frames[0].source_id.clone(),
                    span_range(frames[0].source, *span),
                ))
                .with_message(message)
                .with_color(Color::Blue),
            );
        }
        if let Some(note) = &self.note {
            report = report.with_note(note);
        }
        if let Some(help) = &self.help {
            report = report.with_help(help);
        }

        let sources: Vec<(String, &str)> = frames
            .iter()
            .map(|frame| (frame.source_id.clone(), frame.source))
            .collect();
        report.finish().eprint(ariadne::sources(sources)).unwrap();
    }

    /// As plain text: `file:line:column: message`, then each import that reached it.
    fn plain(&self) -> String {
        let mut text = format!(
            "{}: {}{}",
            self.frames[0].location(),
            if self.warning { "warning: " } else { "" },
            self.message
        );
        for frame in &self.frames[1..] {
            text.push_str(&format!("\n  imported at {}", frame.location()));
        }
        text
    }
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

/// Whether to print with ariadne formatting: stderr is a terminal, and colour isn't
/// turned off.
pub fn use_color() -> bool {
    std::io::stderr().is_terminal() && std::env::var("NO_COLOR").is_err()
}

/// Print a compile error: with ariadne formatting (and a snippet of each source it spans) on
/// a terminal, otherwise as plain text.
pub fn eprint_compile_error(error: &LocatedError, source_id: &str, source: &str) {
    let diagnostic = Diagnostic::error(error, source_id, source);
    if use_color() {
        diagnostic.eprint();
    } else {
        eprintln!("{}", diagnostic.plain());
    }
}

/// Print compile warnings, as [`eprint_compile_error`] prints an error.
pub fn eprint_warnings(warnings: &[LocatedWarning], source_id: &str, source: &str) {
    for warning in warnings {
        let diagnostic = Diagnostic::warning(warning, source_id, source);
        if use_color() {
            diagnostic.eprint();
        } else {
            eprintln!("{}", diagnostic.plain());
        }
    }
}

/// A compile warning as plain text: `file:line:column: warning: message`.
pub fn plain_warning(warning: &LocatedWarning, source_id: &str, source: &str) -> String {
    Diagnostic::warning(warning, source_id, source).plain()
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
        ErrorKind::SpawnUnglued => "glue the function to the '@' to spawn it".to_string(),

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
