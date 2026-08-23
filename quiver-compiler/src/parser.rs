use crate::ast::*;
use nom::{
    IResult, Slice,
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{
        char, digit1, line_ending, multispace0, multispace1, satisfy, space0, space1,
    },
    combinator::{
        cut, map, map_res, not, opt, peek, recognize, success, value as nom_value, verify,
    },
    multi::{many0, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
};
use nom_locate::LocatedSpan;
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::Zero;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceSpan {
    pub offset: usize,
    pub line: usize,
    pub column: usize,
    pub length: usize,
}

impl SourceSpan {
    pub fn from_span(span: Span) -> Self {
        Self {
            offset: span.location_offset(),
            line: span.location_line() as usize,
            column: span.get_column(),
            length: span.fragment().len(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Option<SourceSpan>,
}

impl Error {
    fn new(kind: ErrorKind, span: Option<SourceSpan>) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    // Literal parsing errors
    IntegerMalformed(String),
    HexMalformed(String),
    StringEscapeInvalid(String),

    // Delimiter errors
    UnterminatedTuple,
    UnterminatedString,
    UnterminatedBlock,
    MissingClosingBrace,
    MissingClosingBracket,
    MissingClosingParen,

    // Function/chain errors
    ExpectedPipe,
    InvalidFunctionBody,

    // Spawn errors
    SpawnBlock,

    // Sequence errors
    StepComma,
    MissingChainArrow,
    AssertionOnAlias,
    AssertionNotLineFinal,

    // Generic parser errors
    ParseError(String),
    UnexpectedToken { expected: String, found: String },
    UnexpectedEndOfInput { context: String },
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::IntegerMalformed(lit) => write!(f, "Malformed integer: {}", lit),
            ErrorKind::HexMalformed(lit) => write!(f, "Malformed hex literal: {}", lit),
            ErrorKind::StringEscapeInvalid(esc) => write!(f, "Invalid string escape: {}", esc),

            ErrorKind::UnterminatedTuple => write!(f, "Unterminated tuple: expected ']'"),
            ErrorKind::UnterminatedString => write!(f, "Unterminated string: expected '\"'"),
            ErrorKind::UnterminatedBlock => write!(f, "Unterminated block: expected '}}'"),
            ErrorKind::MissingClosingBrace => write!(f, "Missing closing brace: expected '}}'"),
            ErrorKind::MissingClosingBracket => write!(f, "Missing closing bracket: expected ']'"),
            ErrorKind::MissingClosingParen => {
                write!(f, "Missing closing parenthesis: expected ')'")
            }

            ErrorKind::ExpectedPipe => write!(f, "Expected a term after '~>'"),
            ErrorKind::InvalidFunctionBody => write!(f, "Invalid function body"),

            ErrorKind::SpawnBlock => {
                write!(f, "A spawn needs the root function's parameter type")
            }

            ErrorKind::StepComma => write!(f, "Unexpected ','; use ';' between steps"),
            ErrorKind::MissingChainArrow => {
                write!(f, "Expected '~>' between chain terms, or ';' between steps")
            }
            ErrorKind::AssertionOnAlias => {
                write!(f, "A '//=' assertion cannot attach to a type alias")
            }
            ErrorKind::AssertionNotLineFinal => {
                write!(f, "A '//=' assertion must end its line")
            }

            ErrorKind::ParseError(msg) => write!(f, "Parse error: {}", msg),
            ErrorKind::UnexpectedToken { expected, found } => {
                write!(f, "Expected {}, found '{}'", expected, found)
            }
            ErrorKind::UnexpectedEndOfInput { context } => {
                write!(f, "Unexpected end of input while parsing {}", context)
            }
        }
    }
}

impl ErrorKind {
    /// An actionable suggestion for fixing this error, when one applies. Shared by every
    /// front-end that renders parse errors (the CLI's ariadne reports and the language
    /// server's LSP diagnostics) so the guidance stays consistent in one place.
    pub fn help(&self) -> Option<String> {
        let text = match self {
            ErrorKind::UnterminatedTuple => "Add a closing ']' to complete the tuple",
            ErrorKind::UnterminatedString => "Add a closing '\"' to complete the string",
            ErrorKind::UnterminatedBlock => "Add a closing '}' to complete the block",
            ErrorKind::MissingClosingBrace => "Add '}' to close the block",
            ErrorKind::MissingClosingBracket => "Add ']' to close the tuple",
            ErrorKind::MissingClosingParen => "Add ')' to close the parenthesized expression",
            ErrorKind::ExpectedPipe => {
                "'~>' continues a chain and must be followed by a term, e.g. '[1, 2] ~> __integer_add__'"
            }
            ErrorKind::InvalidFunctionBody => "A function body should be a valid expression",
            ErrorKind::SpawnBlock => {
                "A root function's parameter is the process's state, so it is written rather than inferred: '@'t { ... }', or '@[] { ... }' for a root that takes nil"
            }
            ErrorKind::StepComma => {
                "',' separates tuple fields and type arguments; sequence steps are separated by ';' or a newline"
            }
            ErrorKind::MissingChainArrow => {
                "Whitespace does not join chain terms: write 'a ~> b' to chain them, or 'a; b' for separate steps"
            }
            ErrorKind::AssertionOnAlias => {
                "An assertion observes a step's value, and an alias declares only a type; attach the '//=' to a value step"
            }
            ErrorKind::AssertionNotLineFinal => {
                "An assertion runs to the end of its line, like a comment; move code after it to the next line"
            }
            ErrorKind::HexMalformed(_) => {
                "Binary literals are an even number of hexadecimal digits between angle brackets: <0a1b>"
            }
            ErrorKind::StringEscapeInvalid(_) => {
                "Valid escape sequences are: \\n \\r \\t \\\\ \\\""
            }
            _ => return None,
        };
        Some(text.to_string())
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(span) = self.span {
            write!(f, "{}:{}: {}", span.line, span.column, self.kind)
        } else {
            write!(f, "{}", self.kind)
        }
    }
}

impl std::error::Error for Error {}

/// Detect the most specific error kind based on the source code
fn detect_error_kind(source: &str, _span: Option<&SourceSpan>) -> ErrorKind {
    // Analyze the entire source, not just up to the error position
    // This is because nom reports errors at the start of failed constructs
    let analyzed = source;

    // Count unclosed delimiters
    let open_brackets = analyzed.matches('[').count();
    let close_brackets = analyzed.matches(']').count();
    let open_braces = analyzed.matches('{').count();
    let close_braces = analyzed.matches('}').count();
    let open_parens = analyzed.matches('(').count();
    let close_parens = analyzed.matches(')').count();

    // Check for unterminated string (odd number of quotes, accounting for escapes)
    let mut in_string = false;
    let mut escaped = false;
    for ch in analyzed.chars() {
        if escaped {
            escaped = false;
            continue;
        }
        if ch == '\\' {
            escaped = true;
            continue;
        }
        if ch == '"' {
            in_string = !in_string;
        }
    }

    // Return the most specific error kind
    if in_string {
        return ErrorKind::UnterminatedString;
    }
    if open_brackets > close_brackets {
        return ErrorKind::UnterminatedTuple;
    }
    if open_braces > close_braces {
        // Try to determine if it's a function body or block
        if analyzed.contains("=>") {
            return ErrorKind::InvalidFunctionBody;
        }
        return ErrorKind::UnterminatedBlock;
    }
    if open_parens > close_parens {
        return ErrorKind::MissingClosingParen;
    }

    // Look for patterns to provide more context
    // Check if we're after => (function body expected)
    if analyzed.contains("=>") {
        let after_arrow = analyzed.split("=>").last().unwrap_or("");
        // If after the arrow there's just whitespace and/or a closing brace, we're in function body context
        let after_trimmed = after_arrow.trim();
        if after_trimmed.is_empty() || after_trimmed == "}" || after_trimmed.starts_with('}') {
            return ErrorKind::InvalidFunctionBody;
        }
    }

    let trimmed = analyzed.trim_end();
    if trimmed.ends_with("=>") {
        return ErrorKind::InvalidFunctionBody;
    }
    if trimmed.ends_with("~>") {
        return ErrorKind::ExpectedPipe;
    }

    // Default to context-based error for backward compatibility
    ErrorKind::UnexpectedEndOfInput {
        context: "expression".to_string(),
    }
}

pub fn parse(source: &str) -> Result<Sequence, Error> {
    let span = Span::new(source);
    match program(span) {
        Ok((remaining, prog)) => {
            // Check if there's unparsed input remaining
            let remaining_fragment = remaining.fragment().trim();
            if !remaining_fragment.is_empty() {
                let span = Some(SourceSpan::from_span(remaining));
                // A sequence stops in front of a comma (commas are bracket-only), so leftover
                // input starting with one is a step-level comma: point at the `;` fix.
                if remaining_fragment.starts_with(',') {
                    return Err(Error::new(ErrorKind::StepComma, span));
                }
                let found = remaining_fragment.chars().take(10).collect::<String>();
                return Err(Error::new(
                    ErrorKind::UnexpectedToken {
                        expected: "end of input".to_string(),
                        found,
                    },
                    span,
                ));
            }
            Ok(prog)
        }
        Err(e) => {
            let is_failure = matches!(&e, nom::Err::Failure(_));
            let (span, kind) = match &e {
                nom::Err::Error(e) | nom::Err::Failure(e) => {
                    let span = Some(SourceSpan::from_span(e.input));
                    let fragment = e.input.fragment();

                    // A failure sitting on a comma is a step-level comma (a bracket-internal
                    // comma is consumed by its bracket's own parser): point at the `;` fix.
                    if fragment.trim_start().starts_with(',') {
                        return Err(Error::new(ErrorKind::StepComma, span));
                    }
                    // `sequence_boundary_cut` smuggles a missing chain `~>` out as a hard
                    // failure with the (otherwise unused) `Space` code.
                    if is_failure && e.code == nom::error::ErrorKind::Space {
                        return Err(Error::new(ErrorKind::MissingChainArrow, span));
                    }
                    // `step` smuggles an assertion attached to a type alias out as a hard
                    // failure with the (otherwise unused) `Not` code.
                    if is_failure && e.code == nom::error::ErrorKind::Not {
                        return Err(Error::new(ErrorKind::AssertionOnAlias, span));
                    }
                    // `spawn_term` smuggles a `@{ … }` spawn out as a hard failure with
                    // the (otherwise unused) `Permutation` code.
                    if is_failure && e.code == nom::error::ErrorKind::Permutation {
                        return Err(Error::new(ErrorKind::SpawnBlock, span));
                    }
                    // `assertion` smuggles code following an assertion on its line out as a
                    // hard failure with the (otherwise unused) `CrLf` code.
                    if is_failure && e.code == nom::error::ErrorKind::CrLf {
                        return Err(Error::new(ErrorKind::AssertionNotLineFinal, span));
                    }

                    let kind = match e.code {
                        nom::error::ErrorKind::Eof => detect_error_kind(source, span.as_ref()),
                        nom::error::ErrorKind::Tag => {
                            let found = fragment.chars().take(10).collect::<String>();
                            ErrorKind::UnexpectedToken {
                                expected: "keyword or delimiter".to_string(),
                                found,
                            }
                        }
                        nom::error::ErrorKind::HexDigit => {
                            ErrorKind::HexMalformed(fragment.to_string())
                        }
                        _ => {
                            let found = fragment.chars().take(20).collect::<String>();
                            ErrorKind::ParseError(format!("unexpected input: {}", found))
                        }
                    };
                    (span, kind)
                }
                nom::Err::Incomplete(_) => {
                    (None, ErrorKind::ParseError("incomplete input".to_string()))
                }
            };
            Err(Error::new(kind, span))
        }
    }
}

/// Parse exactly one chain from `source`, requiring the whole input to be consumed.
/// Used to splice a dialect `Unquote` span; the result's spans are relative to `source`
/// (the splicer remaps them into the invocation file).
pub fn parse_chain_exact(source: &str) -> Result<Chain, Error> {
    let span = Span::new(source);
    match chain(span) {
        Ok((remaining, chain)) if remaining.fragment().is_empty() => Ok(chain),
        Ok((remaining, _)) => {
            let found = remaining.fragment().chars().take(10).collect::<String>();
            Err(Error::new(
                ErrorKind::UnexpectedToken {
                    expected: "end of unquoted expression".to_string(),
                    found,
                },
                Some(SourceSpan::from_span(remaining)),
            ))
        }
        Err(e) => {
            let (span, kind) = match &e {
                nom::Err::Error(e) | nom::Err::Failure(e) => {
                    let found = e.input.fragment().chars().take(10).collect::<String>();
                    (
                        Some(SourceSpan::from_span(e.input)),
                        ErrorKind::UnexpectedToken {
                            expected: "an expression".to_string(),
                            found,
                        },
                    )
                }
                nom::Err::Incomplete(_) => {
                    (None, ErrorKind::ParseError("incomplete input".to_string()))
                }
            };
            Err(Error::new(kind, span))
        }
    }
}

/// Byte offset just past one term starting at `offset` — which must sit exactly at the
/// term's first byte; no leading whitespace is skipped — or `None` when no term parses
/// there. Backs the `term` callback handed to dialect functions.
pub fn term_prefix_end(source: &str, offset: usize) -> Option<usize> {
    let rest = source.get(offset..)?;
    let (remaining, _) = primary(Span::new(rest)).ok()?;
    Some(offset + (rest.len() - remaining.fragment().len()))
}

/// Byte offset just past one chain starting at `offset` (same contract as
/// [`term_prefix_end`]). A chain ends where the host grammar says it does — before a
/// `,`, a newline, or any token that cannot start a term. Backs the `chain` callback.
pub fn chain_prefix_end(source: &str, offset: usize) -> Option<usize> {
    let rest = source.get(offset..)?;
    let (remaining, _) = chain(Span::new(rest)).ok()?;
    Some(offset + (rest.len() - remaining.fragment().len()))
}

// Utility parsers

/// The [`SourceSpan`] covering the input consumed between `start` and `end` (where `end` is
/// the remaining input after a parser ran).
fn span_between(start: Span, end: Span) -> SourceSpan {
    SourceSpan {
        offset: start.location_offset(),
        line: start.location_line() as usize,
        column: start.get_column(),
        length: start.fragment().len() - end.fragment().len(),
    }
}

/// A span of `length` bytes starting at `input` — for stamping an opening token (`#`, `@`, `!`,
/// a tuple's `[` or name) rather than the whole construct, so hover/go-to-definition land on the
/// token and not on everything inside it.
fn token_span(input: Span, length: usize) -> SourceSpan {
    SourceSpan {
        offset: input.location_offset(),
        line: input.location_line() as usize,
        column: input.get_column(),
        length,
    }
}

/// Wrap a parser so it also yields the [`SourceSpan`] of the input it consumed. Used to
/// stamp source positions onto the AST nodes the language server needs to address (variable
/// references, builtins, binding patterns). The captured span runs from the start of the
/// input to wherever the inner parser stopped.
fn spanned<'a, O, P>(mut parser: P) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, (SourceSpan, O)>
where
    P: FnMut(Span<'a>) -> IResult<Span<'a>, O>,
{
    move |input: Span<'a>| {
        let start = input;
        let (rest, out) = parser(input)?;
        Ok((rest, (span_between(start, rest), out)))
    }
}

fn ws0(input: Span) -> IResult<Span, ()> {
    nom_value((), multispace0)(input)
}

fn ws1(input: Span) -> IResult<Span, ()> {
    nom_value((), multispace1)(input)
}

/// Horizontal whitespace (spaces/tabs only, no newline) - used as the gap in a
/// function application like `f [1, 2]` or `f x`, so it doesn't swallow the
/// newline that separates steps.
fn hspace1(input: Span) -> IResult<Span, ()> {
    nom_value((), space1)(input)
}

/// Parse a bracketed argument/field list: `[ ... ]`.
/// Parse `[ ... ]` arguments, also yielding the span of the opening `[` (for hover on the
/// argument tuple).
fn bracket_args(input: Span) -> IResult<Span, (SourceSpan, Vec<TupleField>)> {
    let start = input;
    let (rest, fields) =
        delimited(pair(char('['), wsc), tuple_field_list, pair(wsc, char(']')))(input)?;
    Ok((rest, (token_span(start, 1), fields)))
}

/// An adjacent `[...]` immediately after an access head, allowed only when it begins with a
/// spread (`a[..., y]`, `~[..., y]`) — a tuple spread-update. Application is argument-first
/// (`[1, 2] ~> f`), so an adjacent non-spread bracket (`f[1]`) is rejected as a syntax error.
fn adjacent_spread_args(input: Span) -> IResult<Span, (SourceSpan, Vec<TupleField>)> {
    verify(
        bracket_args,
        |(_, fields): &(SourceSpan, Vec<TupleField>)| {
            matches!(
                fields.first(),
                Some(TupleField {
                    value: FieldValue::Spread(_),
                    ..
                })
            )
        },
    )(input)
}

fn comment(input: Span) -> IResult<Span, Span> {
    // `//=` opens a step assertion, not a comment — leave it for [`assertion`].
    let (rest, _) = tag("//")(input)?;
    if rest.fragment().starts_with("=") {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )));
    }
    take_while(|c| c != '\n' && c != '\r')(rest)
}

fn ws_with_comments(input: Span) -> IResult<Span, ()> {
    nom_value(
        (),
        many0(alt((nom_value((), multispace1), nom_value((), comment)))),
    )(input)
}

// Whitespace and/or comments (including inline comments after commas)
fn wsc(input: Span) -> IResult<Span, ()> {
    nom_value(
        (),
        many0(alt((
            nom_value((), multispace1),
            nom_value((), comment),
            nom_value((), preceded(multispace0, comment)),
        ))),
    )(input)
}

// Identifier parsers

fn identifier(input: Span) -> IResult<Span, String> {
    map(
        recognize(tuple((
            satisfy(|c: char| c.is_ascii_lowercase()),
            take_while(|c: char| c.is_ascii_alphanumeric() || c == '_'),
            opt(char('?')),
            opt(char('!')),
        ))),
        |s: Span| s.fragment().to_string(),
    )(input)
}

/// A type name: a leading `'` followed by an identifier (e.g. `'int`, `'point`).
/// The apostrophe distinguishes types from variables and field names; the returned
/// string is the bare name without the prefix.
fn type_name(input: Span) -> IResult<Span, String> {
    preceded(char('\''), identifier)(input)
}

fn tuple_name(input: Span) -> IResult<Span, String> {
    map(
        recognize(pair(
            satisfy(|c: char| c.is_ascii_uppercase()),
            take_while(|c: char| c.is_ascii_alphanumeric() || c == '_'),
        )),
        |s: Span| s.fragment().to_string(),
    )(input)
}

// Literal parsers

fn integer_literal(input: Span) -> IResult<Span, Literal> {
    map(
        pair(
            opt(char('-')),
            // Decimal only. Hexadecimal byte sequences are binary literals (`<...>`).
            map_res(digit1, |s: Span| {
                s.fragment().parse::<BigInt>().map_err(|_| {
                    Error::new(
                        ErrorKind::IntegerMalformed(s.fragment().to_string()),
                        Some(SourceSpan::from_span(s)),
                    )
                })
            }),
        ),
        |(sign, value)| Literal::Integer(if sign.is_some() { -value } else { value }),
    )(input)
}

/// One line of a binary literal: hex-digit groups separated by horizontal whitespace.
fn binary_row(input: Span) -> IResult<Span, Vec<Span>> {
    separated_list1(hspace1, take_while1(|c: char| c.is_ascii_hexdigit()))(input)
}

/// The break between two lines of a binary literal, absorbing the trailing whitespace of the
/// line it ends and the indentation of the one it starts. Blank lines collapse into it — an
/// empty row carries no bytes and no meaning.
fn binary_row_break(input: Span) -> IResult<Span, ()> {
    nom_value(
        (),
        tuple((
            space0,
            line_ending,
            many0(pair(space0, line_ending)),
            space0,
        )),
    )(input)
}

fn binary_literal(input: Span) -> IResult<Span, Literal> {
    // Hexadecimal bytes between angle brackets: `<0a1b>`, with `<>` the empty binary.
    //
    // Horizontal whitespace *separates* groups of digits (`<6a09e667 bb67ae85>`) — on a
    // single line it does not pad the brackets, so `< 0a>` is malformed rather than quietly
    // accepted. A newline instead starts a new row, which is how a table of constants is
    // written; there the brackets do sit apart from the digits, on their own lines. Each
    // group must be a whole number of bytes, so a miscounted run is caught at the group that
    // is wrong rather than only when the total comes out odd.
    //
    // The formatter re-emits both the grouping and the rows, so what it produces is exactly
    // what this accepts — it never reflows a table the author laid out to match a spec.
    //
    // A `<` only ever opens a binary here — the type-argument lists that share the bracket
    // are glued to a name and parsed as part of an access — and anything else fails the
    // closing `>`, so a `<` belonging to something else falls through to the other term
    // parsers untouched.
    let (input, _) = char('<')(input)?;
    let (input, _) = opt(binary_row_break)(input)?;
    let (input, rows) = separated_list0(binary_row_break, binary_row)(input)?;
    let (input, _) = opt(binary_row_break)(input)?;
    let (remaining, _) = char('>')(input)?;

    // Each group decodes on its own - this fails on a group with an odd number of digits.
    let rows = rows
        .into_iter()
        .map(|row| {
            row.into_iter()
                .map(|group| {
                    hex::decode(group.fragment()).map_err(|_| {
                        nom::Err::Failure(nom::error::Error::new(
                            group,
                            nom::error::ErrorKind::HexDigit,
                        ))
                    })
                })
                .collect::<Result<Vec<_>, _>>()
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok((remaining, Literal::Binary(BinaryLiteral::new(rows))))
}

/// Build the `Str[<…>]` tuple term that a string literal desugars to.
/// Parse a single-line string body: the characters between `"` quotes, with escapes processed.
/// The scan for the closing `"` is escape-aware (a backslash skips the next character), so an
/// escaped quote `\"` does not terminate the string.
fn single_line_string(input: Span) -> IResult<Span, String> {
    let (body, _) = char('"')(input)?;
    let frag = body.fragment();
    let mut iter = frag.char_indices();
    let mut close = None;
    while let Some((idx, ch)) = iter.next() {
        match ch {
            '\\' => {
                iter.next(); // skip the escaped character
            }
            '"' => {
                close = Some(idx);
                break;
            }
            _ => {}
        }
    }
    // Unterminated: surface as an EOF error so `detect_error_kind` reports it as an unterminated
    // string (matching the multi-line scanner).
    let idx = close.ok_or_else(|| {
        nom::Err::Failure(nom::error::Error::new(input, nom::error::ErrorKind::Eof))
    })?;
    let value = parse_string_content(body.slice(..idx)).map_err(|_| {
        nom::Err::Failure(nom::error::Error::new(input, nom::error::ErrorKind::MapRes))
    })?;
    Ok((body.slice(idx + 1..), value))
}

fn string_term(input: Span) -> IResult<Span, Term> {
    // Multi-line (`"""`) first, so its opening delimiter isn't read as an empty `""` string.
    alt((
        map(multiline_string_segments, |segments| {
            Term::String(StringStyle::Multi, segments)
        }),
        map(single_line_segments, |segments| {
            Term::String(StringStyle::Single, segments)
        }),
    ))(input)
}

/// Parse a single-line string at term position into its segments, where `{ … }` introduces an
/// interpolation hole.
fn single_line_segments(input: Span) -> IResult<Span, Vec<StrSegment>> {
    let (rest, _) = char('"')(input)?;
    string_segments(input, rest)
}

/// Split the body of a single-line string (after the opening `"`) into literal-text and hole
/// segments, stopping at the closing `"`. Escapes are decoded into the text; an unescaped `{` opens
/// a hole, which is parsed exactly like a block body (`{ … }`). `open` is the opening quote, used
/// only to locate errors.
fn string_segments<'a>(open: Span<'a>, input: Span<'a>) -> IResult<Span<'a>, Vec<StrSegment>> {
    let unterminated =
        || nom::Err::Failure(nom::error::Error::new(open, nom::error::ErrorKind::Eof));
    let frag = *input.fragment();
    let mut segments = Vec::new();
    let mut text: Vec<u8> = Vec::new();
    let mut buf = [0u8; 4];
    let mut pos = 0;
    loop {
        let Some(ch) = frag[pos..].chars().next() else {
            return Err(unterminated());
        };
        match ch {
            '"' => {
                if !text.is_empty() {
                    segments.push(StrSegment::Text(std::mem::take(&mut text)));
                }
                return Ok((input.slice(pos + 1..), segments));
            }
            '{' => {
                if !text.is_empty() {
                    segments.push(StrSegment::Text(std::mem::take(&mut text)));
                }
                let (after, hole) = block(input.slice(pos..))?;
                segments.push(StrSegment::Hole(hole));
                pos = after.location_offset() - input.location_offset();
            }
            '\\' => {
                let esc = frag[pos + 1..].chars().next().ok_or_else(unterminated)?;
                let decoded = match esc {
                    '"' => '"',
                    '\\' => '\\',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '{' => '{',
                    _ => {
                        return Err(nom::Err::Failure(nom::error::Error::new(
                            open,
                            nom::error::ErrorKind::MapRes,
                        )));
                    }
                };
                text.extend_from_slice(decoded.encode_utf8(&mut buf).as_bytes());
                pos += '\\'.len_utf8() + esc.len_utf8();
            }
            c => {
                text.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
                pos += c.len_utf8();
            }
        }
    }
}

/// Reduce a numerator/denominator pair to lowest terms. The denominator is assumed
/// positive (decimal and fraction literals are built that way).
fn reduce_rational(numer: BigInt, denom: BigInt) -> (BigInt, BigInt) {
    let g = numer.gcd(&denom);
    if g.is_zero() {
        (numer, denom)
    } else {
        (numer / &g, denom / &g)
    }
}

/// A single integer field of a desugared `Rational` tuple.
fn rational_field(value: BigInt) -> TupleField {
    TupleField {
        name: None,
        name_span: Spanned::default(),
        span: Spanned::default(),
        value: FieldValue::Chain(Chain {
            binding: None,
            binding_span: Spanned::default(),
            span: Spanned::default(),
            continuations: Vec::new(),
            terms: vec![Term::Literal(Literal::Integer(value))],
            assertions: Vec::new(),
        }),
    }
}

/// Build the `Rational[numer, denom]` tuple term that a numeric literal desugars to,
/// reduced to canonical form (mirrors how `"…"` desugars to `Str[…]`). The result is
/// always a `Rational` — an integer-valued literal like `2.0` or `4/2` becomes
/// `Rational[2, 1]`, distinct from the integer `2`.
fn rational_term(numer: BigInt, denom: BigInt) -> Term {
    let (n, d) = reduce_rational(numer, denom);
    Term::Tuple(Tuple {
        name: TupleName::Named("Rational".to_string()),
        fields: vec![rational_field(n), rational_field(d)],
        span: Spanned::default(),
        punned: false,
    })
}

/// The `Rational[numer, denom]` pattern that a numeric literal pattern desugars to,
/// reduced to canonical form (always a `Rational`, mirroring `rational_term`).
fn rational_match(numer: BigInt, denom: BigInt) -> Match {
    let (n, d) = reduce_rational(numer, denom);
    Match::Tuple(MatchTuple {
        name: Some("Rational".to_string()),
        fields: vec![
            MatchField {
                name: None,
                pattern: Match::Literal(Literal::Integer(n)),
            },
            MatchField {
                name: None,
                pattern: Match::Literal(Literal::Integer(d)),
            },
        ],
    })
}

/// Parse a decimal literal (`1.5`, `-0.25`) into a reduced numerator/denominator pair.
/// The fractional part fixes the denominator as a power of ten.
fn decimal_parts(input: Span) -> IResult<Span, (BigInt, BigInt)> {
    map_res(
        tuple((opt(char('-')), digit1, char('.'), digit1)),
        |(sign, int_part, _dot, frac_part): (Option<char>, Span, char, Span)| {
            let combined = format!("{}{}", int_part.fragment(), frac_part.fragment());
            let magnitude = combined.parse::<BigInt>().map_err(|_| {
                Error::new(
                    ErrorKind::IntegerMalformed(combined.clone()),
                    Some(SourceSpan::from_span(int_part)),
                )
            })?;
            let numer = if sign.is_some() {
                -magnitude
            } else {
                magnitude
            };
            let denom = BigInt::from(10).pow(frac_part.fragment().len() as u32);
            Ok::<(BigInt, BigInt), Error>((numer, denom))
        },
    )(input)
}

/// Parse a fraction literal (`1/3`, `-2/4`) into a reduced numerator/denominator pair.
/// A zero denominator is rejected.
fn fraction_parts(input: Span) -> IResult<Span, (BigInt, BigInt)> {
    map_res(
        tuple((opt(char('-')), digit1, char('/'), digit1)),
        |(sign, num_part, _slash, den_part): (Option<char>, Span, char, Span)| {
            let parse = |s: &Span| {
                s.fragment().parse::<BigInt>().map_err(|_| {
                    Error::new(
                        ErrorKind::IntegerMalformed(s.fragment().to_string()),
                        Some(SourceSpan::from_span(*s)),
                    )
                })
            };
            let magnitude = parse(&num_part)?;
            let numer = if sign.is_some() {
                -magnitude
            } else {
                magnitude
            };
            let denom = parse(&den_part)?;
            if denom.is_zero() {
                return Err(Error::new(
                    ErrorKind::IntegerMalformed(
                        "rational literal with zero denominator".to_string(),
                    ),
                    Some(SourceSpan::from_span(den_part)),
                ));
            }
            Ok::<(BigInt, BigInt), Error>((numer, denom))
        },
    )(input)
}

fn decimal_term(input: Span) -> IResult<Span, Term> {
    map(decimal_parts, |(n, d)| rational_term(n, d))(input)
}

fn fraction_term(input: Span) -> IResult<Span, Term> {
    map(fraction_parts, |(n, d)| rational_term(n, d))(input)
}

fn match_decimal(input: Span) -> IResult<Span, Match> {
    map(decimal_parts, |(n, d)| rational_match(n, d))(input)
}

fn match_fraction(input: Span) -> IResult<Span, Match> {
    map(fraction_parts, |(n, d)| rational_match(n, d))(input)
}

fn parse_string_content(span: Span) -> Result<String, Error> {
    let s = span.fragment();
    let mut result = String::new();
    let mut chars = s.chars();
    let mut offset = 0;

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            let escape_offset = offset;
            offset += ch.len_utf8();

            match chars.next() {
                Some('"') => {
                    result.push('"');
                    offset += 1;
                }
                Some('\\') => {
                    result.push('\\');
                    offset += 1;
                }
                Some('n') => {
                    result.push('\n');
                    offset += 1;
                }
                Some('r') => {
                    result.push('\r');
                    offset += 1;
                }
                Some('t') => {
                    result.push('\t');
                    offset += 1;
                }
                // A literal brace; `{` alone opens an interpolation hole in a string term.
                Some('{') => {
                    result.push('{');
                    offset += 1;
                }
                Some(c) => {
                    let error_span = SourceSpan {
                        offset: span.location_offset() + escape_offset,
                        line: span.location_line() as usize,
                        column: span.get_column() + escape_offset,
                        length: 2, // backslash + character
                    };
                    return Err(Error::new(
                        ErrorKind::StringEscapeInvalid(format!("\\{}", c)),
                        Some(error_span),
                    ));
                }
                None => {
                    let error_span = SourceSpan {
                        offset: span.location_offset() + escape_offset,
                        line: span.location_line() as usize,
                        column: span.get_column() + escape_offset,
                        length: 1, // just the backslash
                    };
                    return Err(Error::new(
                        ErrorKind::StringEscapeInvalid("\\".to_string()),
                        Some(error_span),
                    ));
                }
            }
        } else {
            result.push(ch);
            offset += ch.len_utf8();
        }
    }

    Ok(result)
}

/// Test whether `c` is horizontal whitespace (space or tab).
fn is_hspace(c: char) -> bool {
    c == ' ' || c == '\t'
}

/// Parse a triple-quoted (`"""`) multi-line string, returning the processed contents.
///
/// The scan for the closing `"""` is escape-aware, so `\"""` does not end the string. The
/// raw text between the delimiters is then de-indented and its escapes processed (see
/// [`process_multiline_string`]).
fn multiline_string(input: Span) -> IResult<Span, String> {
    let (rest, raw) = multiline_string_raw(input)?;
    match process_multiline_string(raw.fragment()) {
        Some(s) => Ok((rest, s)),
        // Structural problems (bad indentation, invalid escape) abort the parse. The detail is
        // reconstructed from source by `detect_error_kind`, matching single-line escape errors.
        None => Err(nom::Err::Failure(nom::error::Error::new(
            input,
            nom::error::ErrorKind::MapRes,
        ))),
    }
}

/// Parse a multi-line string at term position into its segments, so `{ … }` interpolation holes are
/// recognised (unlike [`multiline_string`], the pattern-position form).
fn multiline_string_segments(input: Span) -> IResult<Span, Vec<StrSegment>> {
    let (rest, raw) = multiline_string_raw(input)?;
    match process_multiline_segments(raw.fragment()) {
        Some(segments) => Ok((rest, segments)),
        None => Err(nom::Err::Failure(nom::error::Error::new(
            input,
            nom::error::ErrorKind::MapRes,
        ))),
    }
}

/// Consume `"""…"""` and return the raw span between the delimiters. The scan is escape-aware:
/// a backslash skips the following character, so an escaped quote can't close the string.
fn multiline_string_raw(input: Span) -> IResult<Span, Span> {
    let (body, _) = tag("\"\"\"")(input)?;
    let frag = body.fragment();
    let mut iter = frag.char_indices();
    let mut close = None;
    while let Some((idx, ch)) = iter.next() {
        match ch {
            '\\' => {
                iter.next(); // skip the escaped character
            }
            '"' if frag[idx..].starts_with("\"\"\"") => {
                close = Some(idx);
                break;
            }
            _ => {}
        }
    }
    match close {
        Some(idx) => Ok((body.slice(idx + 3..), body.slice(..idx))),
        // Unterminated: surface as an EOF error so `detect_error_kind` reports it as an
        // unterminated string (the unbalanced quotes give it away).
        None => Err(nom::Err::Failure(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Eof,
        ))),
    }
}

/// De-indent the raw text between `"""` delimiters, returning it with newlines normalised and the
/// closing delimiter's indentation (the *margin*) stripped from every line — or `None` if malformed
/// (the opening `"""` not followed by a newline, a non-whitespace margin, or a non-blank line
/// indented less than the margin). Escapes are *not* yet processed.
fn multiline_dedent(raw: &str) -> Option<String> {
    let normalized = raw.replace("\r\n", "\n").replace('\r', "\n");

    // The opening delimiter must be followed by a newline (only horizontal whitespace may
    // precede it on the opening line).
    let after_open = match normalized.split_once('\n') {
        Some((first, rest)) if first.chars().all(is_hspace) => rest,
        _ => return None,
    };

    // The text after the final newline is the closing delimiter's indentation — the margin —
    // and must be whitespace-only (the delimiter is on its own line). With no further newline
    // there are no content lines (an empty or whitespace-only string).
    let (body, margin) = after_open.rsplit_once('\n').unwrap_or(("", after_open));
    if !margin.chars().all(is_hspace) {
        return None;
    }

    // De-indent each line by the margin; blank lines contribute an empty line regardless of
    // their indentation. A non-blank line indented less than the margin is an error.
    let mut dedented = String::new();
    for (i, line) in body.split('\n').enumerate() {
        if i > 0 {
            dedented.push('\n');
        }
        if line.chars().all(is_hspace) {
            continue;
        }
        dedented.push_str(line.strip_prefix(margin)?);
    }
    Some(dedented)
}

/// Process the raw text between `"""` delimiters into the final string value, or `None` if the text
/// is malformed. After de-indentation, escapes are processed — the single-line set plus `\s` (a
/// strip-proof space) and `\<newline>` (line continuation, dropping the newline and the next line's
/// leading whitespace). Trailing whitespace is stripped from each line; blank lines are emitted
/// empty. This is the pattern-position form — `{` is literal (no interpolation).
fn process_multiline_string(raw: &str) -> Option<String> {
    let dedented = multiline_dedent(raw)?;

    // Process escapes, strip trailing whitespace and apply line continuations in one pass.
    // `pending` buffers horizontal whitespace, which is discarded if a newline or EOF follows
    // (trailing) and flushed otherwise.
    let mut result = String::new();
    let mut pending = String::new();
    let mut chars = dedented.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            ' ' | '\t' => pending.push(ch),
            '\n' => {
                pending.clear();
                result.push('\n');
            }
            '\\' => {
                result.push_str(&pending);
                pending.clear();
                match chars.next()? {
                    // Line continuation: drop the newline and the next line's leading whitespace.
                    '\n' => {
                        while chars.peek().is_some_and(|c| is_hspace(*c)) {
                            chars.next();
                        }
                    }
                    '"' => result.push('"'),
                    '\\' => result.push('\\'),
                    'n' => result.push('\n'),
                    'r' => result.push('\r'),
                    't' => result.push('\t'),
                    's' => result.push(' '),
                    '{' => result.push('{'),
                    _ => return None,
                }
            }
            _ => {
                result.push_str(&pending);
                pending.clear();
                result.push(ch);
            }
        }
    }
    Some(result)
}

/// Like [`process_multiline_string`], but for term position: an unescaped `{` opens an interpolation
/// hole (parsed like a block body), splitting the result into text and hole segments. The same
/// escape/strip/continuation rules apply to the text between holes; `\{` is a literal brace.
/// Returns `None` if malformed (bad de-indentation/escape, or an unparseable hole).
fn process_multiline_segments(raw: &str) -> Option<Vec<StrSegment>> {
    let dedented = multiline_dedent(raw)?;

    let mut segments: Vec<StrSegment> = Vec::new();
    let mut result = String::new(); // the current text run (decoded)
    let mut pending = String::new(); // buffered horizontal whitespace, dropped at a line end
    let mut pos = 0;
    while pos < dedented.len() {
        let rest = &dedented[pos..];
        let ch = rest.chars().next().unwrap();
        match ch {
            ' ' | '\t' => {
                pending.push(ch);
                pos += 1;
            }
            '\n' => {
                pending.clear();
                result.push('\n');
                pos += 1;
            }
            '{' => {
                // A hole boundary: whitespace before a hole is content, so keep it, then finalise
                // the text run and parse the hole from the (de-indented) source.
                result.push_str(&pending);
                pending.clear();
                if !result.is_empty() {
                    segments.push(StrSegment::Text(std::mem::take(&mut result).into_bytes()));
                }
                let (after, hole) = block(LocatedSpan::new(rest)).ok()?;
                segments.push(StrSegment::Hole(hole));
                pos += after.location_offset();
            }
            '\\' => {
                result.push_str(&pending);
                pending.clear();
                let esc = rest[1..].chars().next()?;
                pos += 1 + esc.len_utf8();
                match esc {
                    // Line continuation: drop the newline and the next line's leading whitespace.
                    '\n' => {
                        while dedented[pos..].chars().next().is_some_and(is_hspace) {
                            pos += 1;
                        }
                    }
                    '"' => result.push('"'),
                    '\\' => result.push('\\'),
                    'n' => result.push('\n'),
                    'r' => result.push('\r'),
                    't' => result.push('\t'),
                    's' => result.push(' '),
                    '{' => result.push('{'),
                    _ => return None,
                }
            }
            _ => {
                result.push_str(&pending);
                pending.clear();
                result.push(ch);
                pos += ch.len_utf8();
            }
        }
    }
    // Keep the final text run if non-empty, or if it is the only segment (an empty/text-only string).
    if !result.is_empty() || segments.is_empty() {
        segments.push(StrSegment::Text(result.into_bytes()));
    }
    Some(segments)
}

fn literal(input: Span) -> IResult<Span, Literal> {
    alt((binary_literal, integer_literal))(input)
}

// Pattern parsers for terms

/// Parse a pin pattern: `&` followed by an access path rooted at an existing variable
/// (`&x`, `&x.y.0`) or the function parameter (`&$`, `&$x`, `&$0.y` — a single accessor may
/// be glued to the `$`, as usual). Only field/index accessors: a pin compares by value, so
/// annotation retrieval has no place in its target.
fn pin_pattern(input: Span) -> IResult<Span, Match> {
    let (input, _) = char('&')(input)?;
    let start = input;
    let (after_root, root) = alt((
        map(take_while1(|c| c == '$'), |s: Span| PinRoot::Parameter {
            depth: s.fragment().len() - 1,
        }),
        map(identifier, PinRoot::Variable),
    ))(input)?;
    // The base span covers just the root (`p` / `$` / `$$`), before any accessors.
    let base_span = span_between(start, after_root);
    // `$foo` / `$0` sugar: with a parameter root a single accessor may follow with no dot.
    let (after_root, leading) = if matches!(root, PinRoot::Parameter { .. }) {
        opt(accessor)(after_root)?
    } else {
        (after_root, None)
    };
    let (rest, dotted) = many0(preceded(char('.'), accessor))(after_root)?;
    let (accessors, accessor_spans): (Vec<_>, Vec<_>) = leading.into_iter().chain(dotted).unzip();
    Ok((
        rest,
        Match::Pin(PinTarget {
            root,
            accessors,
            accessor_spans,
            base_span: Spanned(Some(base_span)),
            span: Spanned(Some(span_between(start, rest))),
        }),
    ))
}

/// Parse a star pattern, which binds all named fields: bare `*`, or `Name*` which
/// additionally requires the matched value to carry that tuple name.
fn star_pattern(input: Span) -> IResult<Span, Match> {
    alt((
        map(terminated(tuple_name, char('*')), |name| {
            Match::Star(Some(name))
        }),
        map(char('*'), |_| Match::Star(None)),
    ))(input)
}

/// Parse a single partial pattern field: either `name` or `name: pattern`
fn partial_pattern_field(input: Span) -> IResult<Span, PartialPatternField> {
    // Forward reference for nested patterns within partial pattern fields
    // We use a limited pattern parser here to avoid left recursion
    fn nested_pattern(input: Span) -> IResult<Span, Match> {
        alt((
            // Pin with & prefix (a variable- or `$`-rooted access path)
            pin_pattern,
            // String literal
            match_string,
            // Star (optionally named) and placeholder. Before `match_tuple` so `Name*` isn't
            // first consumed as a bare named tuple, leaving the `*` dangling.
            star_pattern,
            // As-pattern `(P)x` as a field value (`A(a: ('int)x)`) — narrow-and-capture a field.
            // Before `type_identifier` so `('int)x` isn't read as a bare type leaving `x` dangling.
            as_pattern,
            // Named/structural tuple pattern: `Dir`, `Circle[r]`, `[a, b]`. Before
            // `type_identifier` so a bare named tuple in a field is a *value* pattern (matching the
            // field's runtime value, like `=Dir`), not a type assertion — the latter compiles to a
            // root type check that cannot narrow a union-typed field.
            map(match_tuple, Match::Tuple),
            // Type reference: 'int, 'list<'t>
            map(type_identifier, Match::Type),
            // Literals
            map(literal, Match::Literal),
            map(char('_'), |_| Match::Placeholder),
            // Identifier
            map(spanned(identifier), |(span, name)| {
                Match::Identifier(name, Spanned(Some(span)))
            }),
        ))(input)
    }

    alt((
        // Field with nested pattern: name: pattern
        map(
            tuple((spanned(identifier), ws0, char(':'), ws0, nested_pattern)),
            |((span, name), _, _, _, pattern)| PartialPatternField {
                name,
                name_span: Spanned(Some(span)),
                pattern: Some(pattern),
            },
        ),
        // Simple field name binding
        map(spanned(identifier), |(span, name)| PartialPatternField {
            name,
            name_span: Spanned(Some(span)),
            pattern: None,
        }),
    ))(input)
}

fn partial_pattern_inner(input: Span) -> IResult<Span, PartialPattern> {
    alt((
        // Named partial pattern: TupleName(field1, field2, ...)
        map(
            tuple((
                tuple_name,
                delimited(
                    pair(char('('), ws0),
                    terminated(
                        separated_list1(tuple((ws0, char(','), ws1)), partial_pattern_field),
                        opt(pair(ws0, char(','))),
                    ),
                    pair(ws0, char(')')),
                ),
            )),
            |(name, fields)| PartialPattern {
                name: Some(name),
                fields,
            },
        ),
        // Unnamed partial pattern: (field1, field2, ...)
        map(
            delimited(
                pair(char('('), ws0),
                terminated(
                    separated_list1(tuple((ws0, char(','), ws1)), partial_pattern_field),
                    opt(pair(ws0, char(','))),
                ),
                pair(ws0, char(')')),
            ),
            |fields| PartialPattern { name: None, fields },
        ),
    ))(input)
}

// Type parsers

fn resource_type_name(input: Span) -> IResult<Span, String> {
    map(
        recognize(pair(
            satisfy(|c: char| c.is_ascii_uppercase()),
            take_while(|c: char| c.is_ascii_alphanumeric() || c == '_'),
        )),
        |s: Span| s.fragment().to_string(),
    )(input)
}

fn resource_type(input: Span) -> IResult<Span, Type> {
    map(preceded(char('\\'), resource_type_name), Type::Resource)(input)
}

/// A field's default value: ` = <chain>`, glued to nothing and spaced like a binding. A
/// chain ends at the `,` or `]` closing the field, so no delimiter guard is needed. Only
/// meaningful in a function literal's parameter spelling; anywhere else the compiler
/// rejects it, which gives a better message than making the grammar positional.
fn field_default(input: Span) -> IResult<Span, Chain> {
    preceded(tuple((ws0, char('='), ws1)), chain)(input)
}

fn field_type(input: Span) -> IResult<Span, FieldType> {
    let start = input;
    let (rest, base) = field_type_base(input)?;
    // Spreads name no field, so they take no default.
    if let FieldType::Spread {
        identifier,
        type_arguments,
        ..
    } = base
    {
        return Ok((
            rest,
            FieldType::Spread {
                span: Spanned(Some(span_between(start, rest))),
                identifier,
                type_arguments,
            },
        ));
    }
    let (rest, default) = opt(map(field_default, Box::new))(rest)?;
    let FieldType::Field {
        name,
        omittable,
        type_def,
        ..
    } = base
    else {
        unreachable!("spread handled above")
    };
    Ok((
        rest,
        FieldType::Field {
            // The whole entry, so a comment written above it attaches here.
            span: Spanned(Some(span_between(start, rest))),
            name,
            omittable,
            type_def,
            default,
        },
    ))
}

fn field_type_base(input: Span) -> IResult<Span, FieldType> {
    alt((
        // Spread with optional type name and optional type arguments: ... or ...'alias or ...'alias<type, type>
        map(
            preceded(
                tag("..."),
                opt(pair(
                    type_name,
                    opt(delimited(
                        char('<'),
                        separated_list1(tuple((ws0, char(','), ws0)), type_definition),
                        char('>'),
                    )),
                )),
            ),
            |id_and_args| {
                if let Some((id, type_args)) = id_and_args {
                    FieldType::Spread {
                        span: Spanned::default(),
                        identifier: Some(id),
                        type_arguments: type_args.unwrap_or_default(),
                    }
                } else {
                    FieldType::Spread {
                        span: Spanned::default(),
                        identifier: None,
                        type_arguments: vec![],
                    }
                }
            },
        ),
        // Optional-name field: (name): type — a caller may omit the label. The `):` is
        // glued, like other glued forms.
        map(
            separated_pair(
                delimited(char('('), identifier, char(')')),
                tuple((char(':'), ws1)),
                type_definition,
            ),
            |(name, type_def)| FieldType::Field {
                span: Spanned::default(),
                name: Some(name),
                omittable: true,
                type_def: Some(type_def),
                default: None,
            },
        ),
        // Named field: name: type
        map(
            separated_pair(identifier, tuple((char(':'), ws1)), type_definition),
            |(name, type_def)| FieldType::Field {
                span: Spanned::default(),
                name: Some(name),
                omittable: false,
                type_def: Some(type_def),
                default: None,
            },
        ),
        // Decorators — an entry naming a field a spread already brought in, giving no type
        // and so adjusting only its label and default. `(name)` makes the label omittable;
        // a bare `name` changes nothing by itself and is either a checked restatement (it
        // must name a field the spread has) or the carrier of a ` = value` default, which
        // `field_type` picks up after this. Both must come after the `name:` forms above,
        // so a typed entry is never mistaken for one, and before the unnamed-field form.
        map(
            terminated(
                delimited(char('('), identifier, char(')')),
                peek(field_entry_end),
            ),
            |name| FieldType::Field {
                span: Spanned::default(),
                name: Some(name),
                omittable: true,
                type_def: None,
                default: None,
            },
        ),
        map(terminated(identifier, peek(field_entry_end)), |name| {
            FieldType::Field {
                span: Spanned::default(),
                name: Some(name),
                omittable: false,
                type_def: None,
                default: None,
            }
        }),
        // Unnamed field: type
        map(type_definition, |type_def| FieldType::Field {
            span: Spanned::default(),
            name: None,
            omittable: false,
            type_def: Some(type_def),
            default: None,
        }),
    ))(input)
}

/// What may legally follow a typeless decorator entry: the `,` or closing bracket that
/// ends it, or the `=` introducing its default. Peeking at this keeps a bare identifier
/// from swallowing the head of something longer — a lowercase tuple-type name (`foo[…]`)
/// or a type application (`foo<…>`) — which would otherwise be mis-read as a decorator.
fn field_entry_end(input: Span) -> IResult<Span, ()> {
    nom_value(
        (),
        pair(wsc, alt((char(','), char(']'), char(')'), char('=')))),
    )(input)
}

fn field_type_list(input: Span) -> IResult<Span, Vec<FieldType>> {
    verify(
        terminated(
            separated_list0(tuple((wsc, char(','), wsc)), field_type),
            opt(pair(wsc, char(','))),
        ),
        |fields: &Vec<FieldType>| {
            // A bare `name` decorator — no parens, no default — is the one field form that
            // is just an identifier, so allowing it anywhere would make `A[x]` a well-formed
            // tuple *type* and a pattern alternation like `=(A[x] | B[x])` would be read as
            // a type ascription instead. Requiring a spread alongside it restores the
            // distinction, and costs nothing: a decorator with no spread has nothing to
            // decorate anyway. The `(name)` and `name = value` forms carry their own marker,
            // so they stay unambiguous and reach resolution, which reports them properly.
            let bare = |field: &FieldType| {
                matches!(
                    field,
                    FieldType::Field {
                        name: Some(_),
                        omittable: false,
                        type_def: None,
                        default: None,
                        ..
                    }
                )
            };
            let spreads = |field: &FieldType| matches!(field, FieldType::Spread { .. });
            !fields.iter().any(bare) || fields.iter().any(spreads)
        },
    )(input)
}

fn partial_type(input: Span) -> IResult<Span, Type> {
    map(
        alt((
            // Named partial: Name(field: type, ...)
            map(
                tuple((
                    tuple_name,
                    delimited(pair(char('('), wsc), field_type_list, pair(wsc, char(')'))),
                )),
                |(name, fields)| TupleType {
                    name: Some(name),
                    fields,
                    is_partial: true,
                },
            ),
            // Unnamed partial: (field: type, ...)
            // Need to verify it's empty OR contains at least one named field to distinguish from grouping
            verify(
                map(
                    delimited(pair(char('('), wsc), field_type_list, pair(wsc, char(')'))),
                    |fields| TupleType {
                        name: None,
                        fields,
                        is_partial: true,
                    },
                ),
                |tuple_type: &TupleType| {
                    // Empty partial types are allowed, or at least one field must be named
                    tuple_type.fields.is_empty()
                        || tuple_type
                            .fields
                            .iter()
                            .any(|f| matches!(f, FieldType::Field { name: Some(_), .. }))
                },
            ),
        )),
        Type::Tuple,
    )(input)
}

fn tuple_type(input: Span) -> IResult<Span, Type> {
    map(
        alt((
            map(
                tuple((
                    tuple_name,
                    delimited(pair(char('['), wsc), field_type_list, pair(wsc, char(']'))),
                )),
                |(name, fields)| TupleType {
                    name: Some(name),
                    fields,
                    is_partial: false,
                },
            ),
            // 'alias[...] - inherit name from type alias and auto-spread
            verify(
                map(
                    tuple((
                        type_name,
                        delimited(pair(char('['), wsc), field_type_list, pair(wsc, char(']'))),
                    )),
                    |(id, mut fields)| {
                        // Transform unspecified spread (...) into identifier spread (...identifier)
                        // This allows 'event[..., timestamp: 'int] to mean "spread event and add timestamp"
                        for field in &mut fields {
                            if let FieldType::Spread {
                                identifier: spread_id,
                                ..
                            } = field
                                && spread_id.is_none()
                            {
                                *spread_id = Some(id.clone());
                            }
                        }
                        TupleType {
                            name: Some(id),
                            fields,
                            is_partial: false,
                        }
                    },
                ),
                |tup: &TupleType| {
                    tup.fields
                        .iter()
                        .any(|f| matches!(f, FieldType::Spread { .. }))
                },
            ),
            map(
                delimited(pair(char('['), wsc), field_type_list, pair(wsc, char(']'))),
                |fields| TupleType {
                    name: None,
                    fields,
                    is_partial: false,
                },
            ),
            // Only parse bare tuple name if not followed by '(' (which would indicate a partial type)
            map(
                tuple((
                    tuple_name,
                    peek(not(pair(ws0, char('(')))), // Ensure not followed by '('
                )),
                |(name, _)| TupleType {
                    name: Some(name),
                    fields: vec![],
                    is_partial: false,
                },
            ),
        )),
        Type::Tuple,
    )(input)
}

/// A *named* tuple type — `Done`, `Reply['ref, 'bin]` — for positions where the unnamed
/// form would collide with other syntax: after `!`, a glued `[` opens the general
/// source-list select, so only named tuple types get the bare receive shorthand.
fn named_tuple_type(input: Span) -> IResult<Span, Type> {
    map(
        alt((
            map(
                tuple((
                    tuple_name,
                    delimited(pair(char('['), wsc), field_type_list, pair(wsc, char(']'))),
                )),
                |(name, fields)| TupleType {
                    name: Some(name),
                    fields,
                    is_partial: false,
                },
            ),
            // Bare name, not followed by `(` (a named partial type), mirroring `tuple_type`.
            map(
                tuple((tuple_name, peek(not(pair(ws0, char('(')))))),
                |(name, _)| TupleType {
                    name: Some(name),
                    fields: vec![],
                    is_partial: false,
                },
            ),
        )),
        Type::Tuple,
    )(input)
}

fn type_parameter(input: Span) -> IResult<Span, Type> {
    map(delimited(char('<'), type_name, char('>')), |name| {
        Type::Identifier {
            name,
            arguments: vec![],
        }
    })(input)
}

/// The enclosing module's own default type: a bare `'`, optionally applied to type
/// arguments (`'<'int>`). Tried after `type_identifier`/`module_type`, so `'int` and `'%mod`
/// take precedence; only a `'` not followed by an identifier or `%` reaches here.
fn self_default_type(input: Span) -> IResult<Span, Type> {
    map(
        preceded(
            char('\''),
            opt(delimited(
                char('<'),
                separated_list1(tuple((ws0, char(','), ws0)), type_definition),
                char('>'),
            )),
        ),
        |arguments| Type::SelfDefault {
            arguments: arguments.unwrap_or_default(),
        },
    )(input)
}

/// A type reached through a module's type namespace: `'%mod` (default type) or
/// `'%mod.name` (named type), with optional type arguments (`'%list<'int>`).
fn module_type(input: Span) -> IResult<Span, Type> {
    map(
        preceded(
            char('\''),
            tuple((
                import,
                opt(preceded(char('.'), identifier)),
                opt(delimited(
                    char('<'),
                    separated_list1(tuple((ws0, char(','), ws0)), type_definition),
                    char('>'),
                )),
            )),
        ),
        |(module, member, arguments)| Type::ModuleType {
            module,
            member,
            arguments: arguments.unwrap_or_default(),
        },
    )(input)
}

fn type_identifier(input: Span) -> IResult<Span, Type> {
    map(
        pair(
            type_name,
            opt(delimited(
                char('<'),
                separated_list1(tuple((ws0, char(','), ws0)), type_definition),
                char('>'),
            )),
        ),
        |(name, arguments)| match arguments {
            // No arguments: `'int`/`'bin`/`'ref` resolve to primitives, anything else to an alias.
            None => identifier_to_type(name),
            Some(arguments) => Type::Identifier { name, arguments },
        },
    )(input)
}

fn type_cycle(input: Span) -> IResult<Span, Type> {
    map(
        preceded(
            char('^'),
            opt(map_res(digit1, |s: Span| s.fragment().parse::<usize>())),
        ),
        Type::Cycle,
    )(input)
}

// A process type spells each grant as the operation that exercises it: a glued head
// type (send — what applying the pid takes), a `!'r` clause (await — what selecting on
// the pid yields), and a `?'s` clause (sample — what `?` reads), in that order:
// `@['msg] [!'r] [?'s]`. A clause sigil glues directly to `@` when it is the first
// thing after it (`@!'r`, `@?'s`); otherwise it needs preceding *horizontal*
// whitespace — type names may end in `!`/`?`, and a newline must stay a step boundary.
// Clauses bind to the nearest sigil-head on their left, so a clause-bearing process
// type nested as another's head is parenthesized — as is one in a function type's
// output position, where trailing clauses are the function's (`process_type_head`).
fn process_type(input: Span) -> IResult<Span, Type> {
    let (input, receive_type) = preceded(char('@'), opt(base_type))(input)?;
    let glued = receive_type.is_none();
    let (input, return_type) = opt(preceded(clause_sigil('!', glued), base_type))(input)?;
    let glued = glued && return_type.is_none();
    let (input, state_type) = opt(preceded(clause_sigil('?', glued), base_type))(input)?;
    Ok((
        input,
        Type::Process(ProcessType {
            receive_type: receive_type.map(Box::new),
            return_type: return_type.map(Box::new),
            state_type: state_type.map(Box::new),
        }),
    ))
}

// The sigil introducing a process-type clause: glued when nothing has been parsed
// since the `@`, preceded by horizontal whitespace otherwise.
fn clause_sigil(sigil: char, glued: bool) -> impl FnMut(Span) -> IResult<Span, char> {
    move |input| {
        if glued {
            char(sigil)(input)
        } else {
            preceded(hspace1, char(sigil))(input)
        }
    }
}

// A process type in a function's output position: bare `@`/`@'msg` only — trailing
// `!`/`?` clauses there are the function's own. A clause-bearing process output is
// parenthesized: `#'a -> (@'m !'r) ?'d`.
fn process_type_head(input: Span) -> IResult<Span, Type> {
    map(preceded(char('@'), opt(base_type)), |receive_type| {
        Type::Process(ProcessType {
            receive_type: receive_type.map(Box::new),
            return_type: None,
            state_type: None,
        })
    })(input)
}

// A callable type: `#'a -> 'b`, with optional clauses ` !'c` (receive) and ` ?'d`
// (states beyond the parameter), in that order. The clause sigil must be preceded by
// *horizontal* whitespace (type names may themselves end in `?`/`!`, so `'b!'c` would
// tokenize as the name `'b!`, and a newline must stay a step boundary) and glued to
// its clause type, mirroring the select sugar `!'int`.
fn function_type(input: Span) -> IResult<Span, Type> {
    map(
        preceded(
            char('#'),
            tuple((
                function_input_type,
                preceded(tuple((ws1, tag("->"), ws1)), function_output_type),
                opt(preceded(pair(hspace1, char('!')), base_type)),
                opt(preceded(pair(hspace1, char('?')), base_type)),
            )),
        ),
        |(input, output, receive, states)| {
            Type::Function(FunctionType {
                input: Box::new(input),
                output: Box::new(output),
                receive: receive.map(Box::new),
                states: states.map(Box::new),
            })
        },
    )(input)
}

fn function_input_type(input: Span) -> IResult<Span, Type> {
    alt((
        partial_type, // Must come before grouping parentheses
        delimited(pair(char('('), ws0), type_definition, pair(ws0, char(')'))),
        tuple_type,
        resource_type,
        type_cycle,
        process_type,
        module_type, // Must come before type_identifier to match '% before trying identifier
        type_identifier,
        self_default_type, // Bare `'`; after type_identifier/module_type so `'int`/`'%mod` win
    ))(input)
}

fn function_output_type(input: Span) -> IResult<Span, Type> {
    alt((
        partial_type, // Must come before grouping parentheses
        delimited(pair(char('('), ws0), type_definition, pair(ws0, char(')'))),
        tuple_type,
        resource_type,
        type_cycle,
        // Head only: a trailing `!`/`?` clause after the output is the function's.
        process_type_head,
        module_type, // Must come before type_identifier to match '% before trying identifier
        type_identifier,
        self_default_type, // Bare `'`; after type_identifier/module_type so `'int`/`'%mod` win
    ))(input)
}

fn base_type(input: Span) -> IResult<Span, Type> {
    alt((
        tuple_type,
        partial_type,  // Must come before grouping parentheses to have priority
        resource_type, // Must come before type_identifier to match \Resource
        type_cycle,
        process_type,
        type_parameter, // Must come before type_identifier to match <'t> before trying identifier
        module_type,    // Must come before type_identifier to match '% before trying identifier
        delimited(pair(char('('), ws0), type_definition, pair(ws0, char(')'))),
        type_identifier,
        self_default_type, // Bare `'`; after type_identifier/module_type so `'int`/`'%mod` win
    ))(input)
}

/// A type intersection `'t & 'u & …` — one or more `base_type`s joined by `&`. Binds tighter
/// than union (`|`), so `'t & 'u | 'v` is `('t & 'u) | 'v`. A single member is just that type.
fn intersection_type(input: Span) -> IResult<Span, Type> {
    map(
        tuple((
            base_type,
            many0(preceded(tuple((wsc, char('&'), wsc)), base_type)),
        )),
        |(first, rest)| {
            if rest.is_empty() {
                first
            } else {
                let mut types = vec![first];
                types.extend(rest);
                Type::Intersection(types)
            }
        },
    )(input)
}

fn type_definition(input: Span) -> IResult<Span, Type> {
    alt((
        function_type,
        map(
            tuple((
                // Optional leading | for multi-line union types
                opt(tuple((wsc, char('|'), wsc))),
                // First member (an intersection, which binds tighter than `|`)
                intersection_type,
                // Remaining members separated by |
                many0(preceded(tuple((wsc, char('|'), wsc)), intersection_type)),
            )),
            |(_, first, rest)| {
                let mut types = vec![first];
                types.extend(rest);
                if types.len() == 1 {
                    types.into_iter().next().unwrap()
                } else {
                    Type::Union(UnionType { types })
                }
            },
        ),
    ))(input)
}

// Term parsers

// Parse a single accessor — a positional `0` index or a `foo` field — together with its span.
// Used both for `.`-prefixed accessors and for the dot-less `$foo` / `$0` parameter shorthand.
fn accessor(input: Span) -> IResult<Span, (AccessPath, Spanned)> {
    let start = input;
    let (rest, path) = alt((
        map(digit1, |s: Span| AccessPath::Index(s.parse().unwrap())),
        map(identifier, AccessPath::Field),
    ))(input)?;
    Ok((rest, (path, Spanned(Some(span_between(start, rest))))))
}

/// An annotation-retrieval accessor, glued to what precedes it: `:key`, or the checked
/// form `:('t)key` — a parenthesised expected shape between the `:` and the key, exactly
/// the `=('t)v` ascription syntax transplanted to retrieval. Everything is glued; a field
/// label (`x: v`) is distinguished by the space after its `:`.
fn annotation_accessor(input: Span) -> IResult<Span, (AccessPath, Spanned)> {
    let start = input;
    let (after_colon, _) = char(':')(input)?;
    // Like `as_pattern`, the shape must be parenthesised — `peek('(')` keeps a bare type
    // name from gluing onto the key (`:'intkey`) and admits `('t)`-style and `(x: 't)`
    // partial gates only.
    let (rest, expected) = opt(preceded(peek(char('(')), inline_type_expression))(after_colon)?;
    let (rest, name) = identifier(rest)?;
    Ok((
        rest,
        (
            AccessPath::Annotation(name, expected),
            Spanned(Some(span_between(start, rest))),
        ),
    ))
}

// Parse access patterns: identifier, $, ~, %import, with optional .field accessors and [args]
fn access(input: Span) -> IResult<Span, Access> {
    let start = input;
    let (after_source, source) = opt(alt((
        // A glued sigil run: `$` is the enclosing function's parameter, each extra `$`
        // reaches one function further out (`$$`, `$$$`, …).
        map(take_while1(|c| c == '$'), |s: Span| {
            AccessSource::Parameter {
                depth: s.fragment().len() - 1,
            }
        }),
        map(char('~'), |_| AccessSource::Ripple),
        // Import: %module or %module/submodule
        map(import, AccessSource::Import),
        // Builtin: __name__ (before identifier; the `__` lexical form is unambiguous)
        map(builtin_name, AccessSource::Builtin),
        map(identifier, AccessSource::Identifier),
    )))(input)?;
    // The base span covers just the `%util` / `$` / variable, before any accessors.
    let base_span = span_between(start, after_source);

    // `$foo` / `$0` are sugar for `$.foo` / `$.0`: when the parameter is the source, a single
    // accessor may follow immediately, with no dot (after the last sigil of a `$$` run).
    // Restricted to the parameter source — `~`, imports and identifiers keep requiring the
    // dot (e.g. `foo` is a variable, not `f.oo`).
    let (after_source, leading) = if matches!(source, Some(AccessSource::Parameter { .. })) {
        opt(accessor)(after_source)?
    } else {
        (after_source, None)
    };

    // Each accessor carries its own span (`.triple` → the `triple`), so the language server can
    // hover/navigate components separately. `:key` (glued) retrieves an annotation; the glue
    // distinguishes it from a field label, which requires a space after its `:`.
    let (after_ref, dotted) =
        many0(alt((preceded(char('.'), accessor), annotation_accessor)))(after_source)?;
    let (accessors, accessor_spans): (Vec<_>, Vec<_>) = leading.into_iter().chain(dotted).unzip();

    // The span covers just the reference (`%num.add`, `foo`, `$.x`), not a trailing call
    // argument, so hover and go-to-definition land precisely on the referenced symbol.
    let ref_span = span_between(start, after_ref);

    // Explicit type arguments (`f<'int>`, glued `<` like every access suffix): instantiate
    // the accessed callable's declared type parameters. Last — nothing follows them in an
    // access. There is no term-level `<` operator, so the glued list is unambiguous.
    let (after_ref, type_arguments) = opt(delimited(
        char('<'),
        separated_list1(tuple((ws0, char(','), ws0)), type_definition),
        char('>'),
    ))(after_ref)?;

    // An access must have a source or at least one accessor.
    if source.is_none() && accessors.is_empty() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Verify,
        )));
    }

    Ok((
        after_ref,
        Access {
            source,
            accessors,
            accessor_spans,
            type_arguments: type_arguments.unwrap_or_default(),
            base_span: Spanned(Some(base_span)),
            span: Spanned(Some(ref_span)),
        },
    ))
}

/// Parse a name-preserving tuple spread-update: `~[..., y]` (spread the chained value) or
/// `a[..., y]` (spread the variable `a`), each inheriting the source's tuple name. Produces a
/// `Term::Tuple` — the spread-update that `Access` used to carry as an argument now lives here.
/// A spread's source: an access restricted to variable (`a`, `a.b.0`), parameter (`$x`,
/// `$$conn` — sigil runs, with the glued first-accessor sugar), or ripple (`~`, `~.f`)
/// roots, with field/index steps only — no annotation retrieval, no type arguments.
fn spread_source_access(input: Span) -> IResult<Span, Access> {
    let start = input;
    let (after_root, source) = alt((
        map(take_while1(|c| c == '$'), |s: Span| {
            AccessSource::Parameter {
                depth: s.fragment().len() - 1,
            }
        }),
        map(char('~'), |_| AccessSource::Ripple),
        map(identifier, AccessSource::Identifier),
    ))(input)?;
    let base_span = span_between(start, after_root);
    let (after_root, leading) = if matches!(source, AccessSource::Parameter { .. }) {
        opt(accessor)(after_root)?
    } else {
        (after_root, None)
    };
    let (rest, dotted) = many0(preceded(char('.'), accessor))(after_root)?;
    let (accessors, accessor_spans): (Vec<_>, Vec<_>) = leading.into_iter().chain(dotted).unzip();
    Ok((
        rest,
        Access {
            source: Some(source),
            accessors,
            accessor_spans,
            type_arguments: vec![],
            base_span: Spanned(Some(base_span)),
            span: Spanned(Some(span_between(start, rest))),
        },
    ))
}

fn spread_update(input: Span) -> IResult<Span, Term> {
    let (input, source) = spread_source_access(input)?;
    let (input, (bracket_span, mut fields)) = adjacent_spread_args(input)?;
    // For `a[...]` / `$conn[...]` / `~.f[...]`, the bare spread `...` spreads the source;
    // rewrite it so the compiler loads through it. A bare-`~` source is the chained value —
    // exactly what bare `...` already means — so it stays `None` (and a `~` appearing in a
    // *field value* is likewise a chained-value ripple, left untouched).
    let bare_ripple =
        matches!(source.source, Some(AccessSource::Ripple)) && source.accessors.is_empty();
    if !bare_ripple {
        for field in &mut fields {
            if matches!(field.value, FieldValue::Spread(None)) {
                field.value = FieldValue::Spread(Some(source.clone()));
            }
        }
    }
    Ok((
        input,
        Term::Tuple(Tuple {
            name: TupleName::Inherit,
            fields,
            span: Spanned(Some(bracket_span)),
            punned: false,
        }),
    ))
}

// Helper to wrap a term in a single-element source chain
fn make_source_chain(term: Term) -> Chain {
    Chain {
        binding: None,
        binding_span: Spanned::default(),
        span: Spanned::default(),
        continuations: Vec::new(),
        terms: vec![term],
        assertions: Vec::new(),
    }
}

// Helper to convert a type name to a type (primitives or an alias reference)
fn identifier_to_type(ident: String) -> Type {
    match ident.as_str() {
        "int" => Type::Primitive(PrimitiveType::Int),
        "bin" => Type::Primitive(PrimitiveType::Bin),
        "ref" => Type::Primitive(PrimitiveType::Ref),
        _ => Type::Identifier {
            name: ident,
            arguments: vec![],
        },
    }
}

// Wrap a term as a single-source select: ![term]
fn single_source(term: Term) -> Term {
    Term::Select(Some(vec![make_source_chain(term)]), Spanned::default())
}

// Create a receive function from a type and optional body
fn make_receive(param_type: Type, body: Option<Block>) -> Term {
    let func = Function {
        type_parameters: vec![],
        parameter_type: Some(param_type),
        return_type: None,
        body,
        span: Spanned::default(),
    };
    single_source(Term::Function(func))
}

// Parse select operator - all forms desugar to Vec<Chain>
//
// The general form takes a *tuple* of sources and requires a space after `!` (`![...]`),
// mirroring function application (`f [...]`). The tight forms (no space) are shorthand for
// selecting on a single source.
//
// Syntax forms. A type shorthand with a same-line `{ … }` block is a receive function *with a
// body* — a **filter** (`!'int { =42 => Ok }` — the message stays in the mailbox on a nil
// verdict); a handler is an arrowed block chain-step (`!'int ~> { … }`) that processes the
// received message. The body-presence rule:
// - ![...]           - Tuple of source chains (general form, glued). A filter is
//                      equally writable here: `![#'int { =42 => Ok }]` (and this is the only
//                      form can filter — it leaves a non-matching message in the mailbox, whereas
//                      a handler block consumes the message and discards it if it doesn't match).
// - !                - Bare select (postfix form, empty sources)
// - !(type)          - Identity receive with explicit type
// - !'type           - Identity receive for a named type
// - !Name[...]       - Identity receive for a named tuple type (`!Done`, `!Reply['ref, 'bin]`;
//                      the unnamed `![…]` form is the general source list)
// - !#type           - Identity receive for a `#`-typed message
// - !var             - Single source (variable/process)
// - !@p              - Single source (spawn)
// - !1000            - Single source (timeout literal)
fn select_term(input: Span) -> IResult<Span, Term> {
    let start = input;
    let (rest, term) = preceded(
        char('!'),
        alt((
            // `[...]` - Tuple of source chains (general form), glued like every other select
            // form: `![p1, p2, 5000]`, `![]`.
            map(
                delimited(
                    pair(char('['), wsc),
                    separated_list0(tuple((wsc, char(','), wsc)), chain),
                    pair(wsc, char(']')),
                ),
                |sources| Term::Select(Some(sources), Spanned::default()),
            ),
            // (type) - parenthesized receive type: a partial type (`!(x: 'int)` — the parens
            // are part of the type) or a grouped union (`!('int | 'bin)`). An optional
            // same-line block is the receive function's body — a filter (`!(...) { … }`).
            map(
                pair(
                    alt((
                        partial_type,
                        delimited(pair(char('('), wsc), type_definition, pair(wsc, char(')'))),
                    )),
                    opt(preceded(opt(hspace1), block)),
                ),
                |(param_type, body)| make_receive(param_type, body),
            ),
            // 'type - named receive type: `!'int`, `!'%proc.changed`, or a filter with a
            // body `!'int { … }`. Module types first, so `'%` isn't rejected as an identifier.
            map(
                pair(
                    alt((module_type, type_identifier)),
                    opt(preceded(opt(hspace1), block)),
                ),
                |(param_type, body)| make_receive(param_type, body),
            ),
            // Named tuple type - `!Done`, `!Reply['ref, 'bin]`, or a filter with a body
            // (`!Reply[...] { … }`). Only the *named* forms: an unnamed `[…]` glued to `!`
            // is the general source-list select.
            map(
                pair(named_tuple_type, opt(preceded(opt(hspace1), block))),
                |(param_type, body)| make_receive(param_type, body),
            ),
            // access (variable / module member) → a single source. Select sources are a
            // tuple of values, and a name is a value, so `!f` is exactly `![f]`.
            map(access, |acc| single_source(Term::Access(acc))),
            // #type - `#`-typed receive: `!#'int`, `!#Reply[...]`, or a filter with a body
            // (`!#'int { … }`).
            map(
                pair(
                    preceded(char('#'), function_input_type),
                    opt(preceded(opt(hspace1), block)),
                ),
                |(param_type, body)| make_receive(param_type, body),
            ),
            // @N process reference (must come before spawn_term to match @1 before @f)
            map(process_ref_term, single_source),
            // @spawn
            map(spawn_term, single_source),
            // literal (timeout)
            map(literal, |l| single_source(Term::Literal(l))),
            // nothing - bare ! for postfix form (uses chained value)
            success(Term::Select(None, Spanned::default())),
        )),
    )(input)?;
    // Attach the `!` span to whatever select form was produced, for hover.
    let term = match term {
        // Stamp just the `!`, not the raced sources / awaited expression.
        Term::Select(sources, _) => Term::Select(sources, Spanned(Some(token_span(start, 1)))),
        other => other,
    };
    Ok((rest, term))
}

fn import(input: Span) -> IResult<Span, Vec<String>> {
    preceded(char('%'), separated_list1(char('/'), identifier))(input)
}

/// A dialect invocation `%mod{ raw }`: an import path glued (no whitespace) to a braced
/// raw-text region. The content is *not* parsed as Quiver — this scanner only finds the
/// matching close brace: braces must balance, except inside `"…"` string literals (where a
/// `\` escapes the next byte, so `\"` doesn't close the string) or when escaped as
/// `\{`/`\}`. Outside strings `\"` is an escaped literal quote (so an unpaired `"` is
/// writable without opening string mode). The text is kept verbatim (escapes included) so
/// the formatter round-trips it; the compiler unescapes the braces and quotes when
/// expanding.
fn dialect_term(input: Span) -> IResult<Span, Term> {
    let start = input;
    let (input, path) = import(input)?;
    let (input, _) = char('{')(input)?;
    let content_start = input;
    let bytes = input.fragment().as_bytes();
    let mut pos = 0;
    let mut depth = 1usize;
    let mut in_string = false;
    while pos < bytes.len() && depth > 0 {
        match (in_string, bytes[pos]) {
            (true, b'\\') => pos += 1,
            (true, b'"') => in_string = false,
            (false, b'\\') if matches!(bytes.get(pos + 1).copied(), Some(b'{' | b'}' | b'"')) => {
                pos += 1
            }
            (false, b'"') => in_string = true,
            (false, b'{') => depth += 1,
            (false, b'}') => depth -= 1,
            _ => {}
        }
        pos += 1;
    }
    if depth > 0 {
        return Err(nom::Err::Failure(nom::error::Error::new(
            input,
            nom::error::ErrorKind::TakeUntil,
        )));
    }
    let content_length = pos - 1; // exclude the closing `}`
    let raw = input.fragment()[..content_length].to_string();
    let rest = input.slice(pos..);
    Ok((
        rest,
        Term::Dialect(Dialect {
            path,
            raw,
            span: Spanned(Some(span_between(start, rest))),
            content_span: Spanned(Some(token_span(content_start, content_length))),
        }),
    ))
}

/// The label a punned entry lends its field: the final *named* segment of its access path — the
/// last field accessor (`p.x`, `$conn.buf`, `%num.add` → `x`, `buf`, `add`), or, with no
/// accessors, the path's own name (a variable `f`, or an import's last segment: `%num` → `num`).
///
/// The root must be one a reference can be taken of and that a label can be recovered from: a
/// variable, a parameter (`$x`, `$$x`), or an import. A ripple root is excluded because `&~.f`
/// is not supported; a builtin because `__integer_add__` is no field label (write the labeled
/// form, `[add: &__integer_add__]`); self and tail calls because they name no field. A final
/// index (`p.0`) or annotation (`f:doc`) likewise has no label to lend.
fn pun_label(path: &Access) -> Option<String> {
    match path.source {
        Some(
            AccessSource::Identifier(_) | AccessSource::Parameter { .. } | AccessSource::Import(_),
        ) => {}
        _ => return None,
    }
    match path.accessors.last() {
        Some(AccessPath::Field(name)) => Some(name.clone()),
        Some(AccessPath::Index(_) | AccessPath::Annotation(..)) => None,
        None => match &path.source {
            Some(AccessSource::Identifier(name)) => Some(name.clone()),
            Some(AccessSource::Import(segments)) => segments.last().cloned(),
            // A bare `$`/`$$` names the whole parameter, which has no label of its own.
            _ => None,
        },
    }
}

/// A punned tuple entry: a bare access path standing for both a field label and its value, so
/// `(a, p.x)` abbreviates `[a: a, x: p.x]`. An entry is a *name*, not an expression, so the
/// flowing value has nothing to flow into — which, now that every call is written, is all that
/// separates a pun from the spelled-out field. A punned tuple is pure repackaging.
fn pun_field(input: Span) -> IResult<Span, TupleField> {
    let start = input;
    let (rest, path) = access(input)?;
    let Some(name) = pun_label(&path) else {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Verify,
        )));
    };
    // The label and the value are the same text, so the label's span is the path's final
    // segment — where go-to-definition on the field should land.
    let name_span = path
        .accessor_spans
        .last()
        .copied()
        .unwrap_or(path.base_span);
    Ok((
        rest,
        TupleField {
            name: Some(name),
            name_span,
            span: Spanned(Some(span_between(start, rest))),
            value: FieldValue::Chain(make_source_chain(Term::Access(path))),
        },
    ))
}

/// The parenthesised entry list of a punning tuple: one or more puns, and *only* puns. An
/// explicit `k: v` entry is not accepted, so `(…)` never becomes a second spelling for a general
/// tuple literal — every entry in it is a name, exactly as in the partial *type* `(x: 'int)` and
/// the partial *pattern* `(x, y)`. The empty list is rejected too: `()` has no name to pun, and
/// nil is spelled `[]`.
fn punned_field_list(input: Span) -> IResult<Span, Vec<TupleField>> {
    delimited(
        pair(char('('), wsc),
        terminated(
            separated_list1(tuple((wsc, char(','), wsc)), pun_field),
            opt(pair(wsc, char(','))),
        ),
        pair(wsc, char(')')),
    )(input)
}

fn tuple_field(input: Span) -> IResult<Span, TupleField> {
    let start = input;
    let (rest, mut field) = alt((
        // Named field with chain: name: chain
        map(
            separated_pair(spanned(identifier), tuple((char(':'), ws1)), chain),
            |((span, name), chain_value)| TupleField {
                name: Some(name),
                name_span: Spanned(Some(span)),
                span: Spanned::default(),
                value: FieldValue::Chain(chain_value),
            },
        ),
        // Spread: bare `...` (the chained value), or a sourced `...a.b` / `...$conn` /
        // `...$$x` / `...~.f` — an access glued to the dots.
        map(preceded(tag("..."), opt(spread_source_access)), |source| {
            TupleField {
                name: None,
                name_span: Spanned::default(),
                span: Spanned::default(),
                value: FieldValue::Spread(source),
            }
        }),
        // Unnamed chain: chain
        map(chain, |chain_value| TupleField {
            name: None,
            name_span: Spanned::default(),
            span: Spanned::default(),
            value: FieldValue::Chain(chain_value),
        }),
    ))(input)?;
    // Record the field's start offset, for attaching leading trivia during formatting.
    field.span = Spanned(Some(span_between(start, rest)));
    Ok((rest, field))
}

fn tuple_field_list(input: Span) -> IResult<Span, Vec<TupleField>> {
    terminated(
        separated_list0(tuple((wsc, char(','), wsc)), tuple_field),
        opt(pair(wsc, char(','))),
    )(input)
}

fn tuple_term(input: Span) -> IResult<Span, Tuple> {
    let start = input;
    let (rest, mut tuple_value) = alt((
        // TupleName[...] - uppercase named tuple with fields
        map(
            tuple((
                tuple_name,
                delimited(pair(char('['), wsc), tuple_field_list, pair(wsc, char(']'))),
            )),
            |(name, fields)| Tuple {
                name: TupleName::Named(name),
                fields,
                span: Spanned::default(),
                punned: false,
            },
        ),
        // [...] - unnamed tuple with fields
        map(
            delimited(pair(char('['), wsc), tuple_field_list, pair(wsc, char(']'))),
            |fields| Tuple {
                name: TupleName::Anonymous,
                fields,
                span: Spanned::default(),
                punned: false,
            },
        ),
        // TupleName(...) - named punning tuple, the `(` glued like every tuple bracket.
        map(tuple((tuple_name, punned_field_list)), |(name, fields)| {
            Tuple {
                name: TupleName::Named(name),
                fields,
                span: Spanned::default(),
                punned: true,
            }
        }),
        // (...) - unnamed punning tuple. Only reached in expression position: a leading
        // pattern (`(a, b) = p`) is taken by `chain`'s binding alternative first, so the same
        // text still destructures there.
        map(punned_field_list, |fields| Tuple {
            name: TupleName::Anonymous,
            fields,
            span: Spanned::default(),
            punned: true,
        }),
        // TupleName - bare tuple name without fields
        // Only parse if not followed by '(' (which would indicate a punning tuple)
        map(
            tuple((
                tuple_name,
                peek(not(pair(ws0, char('(')))), // Ensure not followed by '('
            )),
            |(name, _)| Tuple {
                name: TupleName::Named(name),
                fields: vec![],
                span: Spanned::default(),
                punned: false,
            },
        ),
    ))(input)?;
    // Stamp the tuple's head — its name, or the opening `[` — not the whole literal, so hover
    // inside the tuple shows the fields, and only the head shows the composite type.
    let head_len = match &tuple_value.name {
        TupleName::Named(name) => name.len(),
        TupleName::Anonymous | TupleName::Inherit => 1,
    };
    tuple_value.span = Spanned(Some(token_span(start, head_len)));
    Ok((rest, tuple_value))
}

fn branch(input: Span) -> IResult<Span, Branch> {
    map(
        pair(
            sequence,
            opt(preceded(tuple((wsc, tag("=>"), wsc)), sequence)),
        ),
        |(condition, consequence)| Branch {
            condition,
            consequence,
        },
    )(input)
}

/// The contents of a block: `|`-separated branches, with an optional leading `|`. Every braced
/// form shares it — a block term, a function body, and a string interpolation hole.
fn block_body(input: Span) -> IResult<Span, Block> {
    map(
        preceded(
            opt(pair(char('|'), wsc)),
            separated_list1(tuple((wsc, char('|'), wsc)), branch),
        ),
        |branches| Block {
            annotations: vec![],
            branches,
        },
    )(input)
}

/// One annotation in a block prefix: `:key value`, where `value` is a chain. The `:` must be
/// glued to the key name; the value is terminated like any step (semicolon/newline).
fn annotation(input: Span) -> IResult<Span, Annotation> {
    let start = input;
    let (rest, ((name_span, name), value)) =
        separated_pair(spanned(preceded(char(':'), identifier)), hspace1, chain)(input)?;
    Ok((
        rest,
        Annotation {
            name,
            name_span: Spanned(Some(name_span)),
            span: Spanned(Some(span_between(start, rest))),
            value,
        },
    ))
}

/// A block `{ … }`: a braced expression that introduces a new scope. May begin with an
/// annotation prefix (`:key value` entries); the annotations attach to the value the braces
/// denote. A block with annotations and no body is identity-plus-attach.
fn block(input: Span) -> IResult<Span, Block> {
    delimited(
        pair(char('{'), wsc),
        alt((
            // Annotation prefix (one or more), then an optional body.
            map(
                pair(
                    terminated(
                        separated_list1(seq_sep, annotation),
                        // A step separator after the annotations means a body may follow; only
                        // when there is none must the block be ending (or the boundary is a
                        // step-level mistake the cut reports).
                        alt((nom_value((), seq_sep), sequence_boundary_cut)),
                    ),
                    opt(block_body),
                ),
                |(annotations, body)| Block {
                    annotations,
                    branches: body.map(|e| e.branches).unwrap_or_default(),
                },
            ),
            block_body,
        )),
        pair(wsc, char('}')),
    )(input)
}

fn function(input: Span) -> IResult<Span, Function> {
    let start = input;
    let (rest, mut func) = map(
        preceded(
            char('#'),
            tuple((
                opt(delimited(
                    char('<'),
                    separated_list1(tuple((ws0, char(','), ws0)), type_name),
                    char('>'),
                )),
                opt(preceded(not(peek(char('{'))), function_input_type)),
                opt(preceded(tuple((ws1, tag("->"), ws1)), function_output_type)),
                opt(alt((preceded(ws1, block), block))),
            )),
        ),
        |(type_parameters, parameter_type, return_type, body)| Function {
            type_parameters: type_parameters.unwrap_or_default(),
            parameter_type,
            return_type,
            body,
            span: Spanned::default(),
        },
    )(input)?;
    // Stamp just the `#`, so hover/go-to-definition land on it, not the whole function body.
    func.span = Spanned(Some(token_span(start, 1)));
    Ok((rest, func))
}

fn tail_call(input: Span) -> IResult<Span, Term> {
    let start = input;
    let (input, _) = char('^')(input)?;
    // `^~` - tail-call the flowing value. Guard the `~` against the `~>` chain separator, so a
    // bare `^` followed by `~> …` still parses as a self tail call.
    let (after_tilde, tilde) = opt(terminated(char('~'), not(char('>'))))(input)?;
    if tilde.is_some() {
        let span = span_between(start, after_tilde);
        return Ok((
            after_tilde,
            Term::Access(Access {
                source: Some(AccessSource::TailCallRipple),
                accessors: vec![],
                accessor_spans: vec![],
                type_arguments: vec![],
                base_span: Spanned(Some(span)),
                span: Spanned(Some(span)),
            }),
        ));
    }
    let (after_ident, ident) = opt(identifier)(input)?;
    // The base span covers the `^` / `^name`, before any accessors.
    let base_span = span_between(start, after_ident);
    let (after_ref, accessors_with_spans) = many0(preceded(char('.'), |i| {
        let acc_start = i;
        let (rest, accessor) = alt((
            map(digit1, |s: Span| AccessPath::Index(s.parse().unwrap())),
            map(identifier, AccessPath::Field),
        ))(i)?;
        Ok((
            rest,
            (accessor, Spanned(Some(span_between(acc_start, rest)))),
        ))
    }))(after_ident)?;
    let (accessors, accessor_spans): (Vec<_>, Vec<_>) = accessors_with_spans.into_iter().unzip();
    // A tail call is an access whose source is the tail target; a space-separated argument is a
    // juxtaposition application (`^g [~, 2]`), handled by `term`.
    Ok((
        after_ref,
        Term::Access(Access {
            source: Some(AccessSource::TailCall(ident)),
            accessors,
            accessor_spans,
            type_arguments: vec![],
            base_span: Spanned(Some(base_span)),
            span: Spanned(Some(span_between(start, after_ref))),
        }),
    ))
}

/// Parse a builtin name `__name__`, returning the inner name. A builtin is an [`AccessSource`],
/// so it flows through the same call/reference machinery as identifiers and imports — the access
/// parser captures its span (the `__name__`) and any `[...]` argument.
fn builtin_name(input: Span) -> IResult<Span, String> {
    // Parse opening __
    let (input, _) = tag("__")(input)?;

    // Parse identifier with potential trailing underscores
    let (_remaining, result) = recognize(tuple((
        satisfy(|c: char| c.is_ascii_lowercase()),
        take_while(|c: char| c.is_ascii_alphanumeric() || c == '_'),
    )))(input)?;

    // Trim trailing underscores from the result
    let trimmed = result.fragment().trim_end_matches('_');

    // Adjust the remaining input to include the trimmed underscores
    let input = input.slice(trimmed.len()..);

    // Parse closing __
    let (input, _) = tag("__")(input)?;

    Ok((input, trimmed.to_string()))
}

// Build a spawn of an inline function (the `#`-elided `@'type { … }` / `@[…] { … }` /
// `@(type) { … }` forms). The init argument, if any, comes from the chained value.
fn spawn_of_function(parameter_type: Type, body: Block) -> Term {
    Term::Spawn(
        Box::new(Term::Function(Function {
            type_parameters: vec![],
            parameter_type: Some(parameter_type),
            return_type: None,
            body: Some(body),
            span: Spanned::default(),
        })),
        None,
        Spanned::default(),
    )
}

fn spawn_term(input: Span) -> IResult<Span, Term> {
    let start = input;
    // `@{ … }` is the retired inferred-parameter sugar; reject it by name rather than
    // letting it fall through to a bare `@` (the current process) followed by a block,
    // which reads as a missing chain arrow.
    if peek(pair(char::<Span, nom::error::Error<Span>>('@'), char('{')))(input).is_ok() {
        return Err(nom::Err::Failure(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Permutation,
        )));
    }
    let (rest, term) = alt((
        // @(type) { ... } - Spawn with parenthesized type (sugar for @#(type) { ... })
        map(
            tuple((
                preceded(
                    char('@'),
                    delimited(pair(char('('), wsc), type_definition, pair(wsc, char(')'))),
                ),
                preceded(opt(ws1), block),
            )),
            |(param_type, body)| spawn_of_function(param_type, body),
        ),
        // @'type { ... } - Spawn with named type (sugar for @#'type { ... }); module types
        // first, so `@'%mod.event { ... }` isn't rejected as an identifier.
        map(
            tuple((
                preceded(char('@'), alt((module_type, type_identifier))),
                preceded(opt(ws1), block),
            )),
            |(param_type, body)| spawn_of_function(param_type, body),
        ),
        // @[...] { ... } - Spawn with tuple type (sugar for @#[...] { ... })
        map(
            tuple((preceded(char('@'), tuple_type), preceded(opt(ws1), block))),
            |(tuple_ty, body)| spawn_of_function(tuple_ty, body),
        ),
        // @<primary> - the spawned function, glued to the `@`: `@f`, `@$handler`, `@~`,
        // `@#'t { … }`. The glue is what leaves a bare `@` to mean the current process,
        // so the target is mandatory — `@~` is written out rather than implied. A block
        // is not a target: `@{ … }` would have to infer the root function's parameter,
        // which is the process's state and so is always written (`@'t { … }`, or
        // `@[] { … }` for a nilary root).
        map(
            preceded(char('@'), verify(primary, |t| !matches!(t, Term::Block(_)))),
            |target| Term::Spawn(Box::new(target), None, Spanned::default()),
        ),
    ))(input)?;
    // Attach the `@` span to the spawn, for hover (shows the process type).
    let term = match term {
        // Stamp just the `@`, not the spawned function body.
        Term::Spawn(inner, arg, _) => Term::Spawn(inner, arg, Spanned(Some(token_span(start, 1)))),
        other => other,
    };
    Ok((rest, term))
}

/// The current process, `@`. Tried after `spawn_term`, which claims every `@` with a
/// glued target, so a bare `@` is what is left: `@` is to a process what `~` is to the
/// flowing value and `$` to the parameter.
fn self_term(input: Span) -> IResult<Span, Term> {
    map(char('@'), |_| Term::Self_)(input)
}

fn bind_match(input: Span) -> IResult<Span, Term> {
    map(preceded(char('='), match_pattern), Term::Match)(input)
}

fn match_field(input: Span) -> IResult<Span, MatchField> {
    alt((
        // Named field: name: pattern
        map(
            separated_pair(identifier, tuple((char(':'), ws1)), match_pattern),
            |(name, pattern)| MatchField {
                name: Some(name),
                pattern,
            },
        ),
        // Unnamed pattern
        map(match_pattern, |pattern| MatchField {
            name: None,
            pattern,
        }),
    ))(input)
}

fn match_string(input: Span) -> IResult<Span, Match> {
    // Multi-line (`"""`) first, so its opening delimiter isn't read as an empty `""` string.
    // Patterns are text-only (no interpolation), so the bytes are matched directly.
    alt((
        map(multiline_string, |s| {
            Match::String(StringStyle::Multi, s.into_bytes())
        }),
        map(single_line_string, |s| {
            Match::String(StringStyle::Single, s.into_bytes())
        }),
    ))(input)
}

// Parse an inline type expression in a pattern: a parenthesized type expression,
// a type name (`'int`, `'list<'t>`), or a partial type (`(mode: W)`, `A(x: 'int)`).
fn inline_type_expression(input: Span) -> IResult<Span, Type> {
    alt((
        // Parenthesized type expression: ('int | 'bin), ('list<'int>), etc.
        delimited(pair(char('('), wsc), type_definition, pair(wsc, char(')'))),
        // Module type: '%mod, '%mod.name. Before type_identifier to match '% first.
        module_type,
        // Type name, optionally with arguments: 'int, 'list<'int>, 'tree<'int, 'bin>
        type_identifier,
        // The enclosing module's default type: a bare `'` (or `'<args>`). After the above so
        // `'int`/`'%mod` win; this lets a pattern reference the default type, e.g. `='`.
        self_default_type,
        // Partial type whose fields constrain by type: (mode: W), A(x: 'int)
        partial_type,
    ))(input)
}

/// An alternation pattern: `(p | q | …)`, two or more `|`-separated patterns in parentheses.
/// Tried after partial patterns and type expressions, so `(x: T)` stays a partial and
/// `('int | 'bin)` stays a type union; this captures groups with a genuinely structural
/// alternative (e.g. `([[], _] | [_, []])`).
fn or_pattern(input: Span) -> IResult<Span, Vec<Match>> {
    delimited(
        pair(char('('), wsc),
        verify(
            separated_list1(tuple((wsc, char('|'), wsc)), match_pattern),
            |alts: &Vec<Match>| alts.len() >= 2,
        ),
        pair(wsc, char(')')),
    )(input)
}

/// A type expression, or — where the contents are not a type — an alternation of patterns. Both
/// spell "one of these", and the type form wins wherever it parses, so `('int | 'bin)` is a single
/// union test and `(0 | 1)` an alternation (the type grammar has no literal members). This is the
/// sole place that order is decided: `match_pattern` and `as_pattern` both defer to it, so a `( … )`
/// head reads the same way whether or not a binder follows it.
fn type_or_alternation(input: Span) -> IResult<Span, Match> {
    alt((
        map(inline_type_expression, Match::Type),
        map(or_pattern, Match::Or),
    ))(input)
}

/// Parse an ascribed binding: a *parenthesised* pattern head immediately followed by a binding
/// identifier — `('int)x`, `('int | 'bin)x`, `(0 | 1)x`. Matches the head, then binds the whole
/// value, at the type the head narrowed it to, to the trailing identifier. The identifier must be
/// *adjacent* — no whitespace after `)` — so `('int) x` is not an as-pattern (the `x` is left for
/// the next term). The leading `(` is required, so a bare type (`'int`, `A['int]`) is never
/// silently turned into a binder.
fn as_pattern(input: Span) -> IResult<Span, Match> {
    // Require the parenthesised form. `peek('(')` keeps a bare type like `A['int]` from being read
    // as `('A['int]')` + binder; and lets a non-type `(x)` fall through to the partial-pattern rule.
    peek(char('('))(input)?;
    let (input, head) = type_or_alternation(input)?;
    let (input, (span, name)) = spanned(identifier)(input)?;
    Ok((input, Match::As(Box::new(head), name, Spanned(Some(span)))))
}

fn match_pattern(input: Span) -> IResult<Span, Match> {
    alt((
        // Pin with & prefix: a variable- or `$`-rooted access path to check the value against.
        pin_pattern,
        // Try string literals first (before tuples and literals)
        match_string,
        // Star (optionally named): `*` or `Name*`. Before match_tuple so `Name*` isn't
        // first consumed as a bare named tuple, leaving the `*` dangling.
        star_pattern,
        // As-pattern `(P)x` — before the bare paren-forms below, which would otherwise consume
        // `(P)` and leave the trailing binder dangling.
        as_pattern,
        // Try match tuple (handles both [..] and Name[..])
        map(match_tuple, Match::Tuple),
        // Try partial patterns before inline types (partial patterns use parentheses too)
        map(partial_pattern_inner, Match::Partial),
        // Type reference (`'int`, `'list<'t>`, `('int | 'bin)`), else an alternation of patterns
        // (`([[], _] | [_, []])`). Types need no & since they are never bound. Must come after
        // partial patterns to avoid ambiguity with (...).
        type_or_alternation,
        // Numeric literal patterns: decimal (`=1.5`) and fraction (`=1/3`), before bare
        // literals so the leading digits aren't consumed as a plain integer.
        match_decimal,
        match_fraction,
        // Then try literals
        map(literal, Match::Literal),
        map(char('_'), |_| Match::Placeholder),
        // Identifier must come last (since it's more general)
        map(spanned(identifier), |(span, name)| {
            Match::Identifier(name, Spanned(Some(span)))
        }),
    ))(input)
}

fn match_tuple(input: Span) -> IResult<Span, MatchTuple> {
    alt((
        // Named tuple: Name[...]
        map(
            tuple((
                tuple_name,
                delimited(
                    pair(char('['), wsc),
                    terminated(
                        separated_list0(tuple((wsc, char(','), wsc)), match_field),
                        opt(pair(wsc, char(','))),
                    ),
                    pair(wsc, char(']')),
                ),
            )),
            |(name, fields)| MatchTuple {
                name: Some(name),
                fields,
            },
        ),
        // Unnamed tuple: [...]
        map(
            delimited(
                pair(char('['), wsc),
                terminated(
                    separated_list0(tuple((wsc, char(','), wsc)), match_field),
                    opt(pair(wsc, char(','))),
                ),
                pair(wsc, char(']')),
            ),
            |fields| MatchTuple { name: None, fields },
        ),
        // Bare named tuple: Name (not followed by '(' which would be a partial pattern)
        map(
            tuple((tuple_name, peek(not(pair(ws0, char('(')))))),
            |(name, _)| MatchTuple {
                name: Some(name),
                fields: vec![],
            },
        ),
    ))(input)
}

fn process_ref_term(input: Span) -> IResult<Span, Term> {
    map(
        preceded(
            char('@'),
            map_res(digit1, |s: Span| {
                s.fragment().parse::<usize>().map_err(|_| {
                    Error::new(
                        ErrorKind::IntegerMalformed(format!("@{}", s.fragment())),
                        Some(SourceSpan::from_span(s)),
                    )
                })
            }),
        ),
        Term::Process,
    )(input)
}

fn primary(input: Span) -> IResult<Span, Term> {
    alt((
        // String terms (before literals to handle quotes)
        string_term,
        // Process operations (process_ref_term must come before spawn_term to match @N first)
        process_ref_term,
        spawn_term,
        self_term,
        // Bind match (must be before literals and identifiers)
        bind_match,
        // Numeric literals: decimal (`1.5`) and fraction (`1/3`) desugar to reduced
        // `Rational` tuples. Before bare integer/binary literals, which would otherwise
        // consume the leading digits and leave `.5` / `/3` dangling.
        decimal_term,
        fraction_term,
        // Literals
        map(literal, Term::Literal),
        // Complex terms
        map(tuple_term, Term::Tuple),
        map(function, Term::Function),
        map(block, Term::Block),
        // Name-preserving spread-update (`~[..., y]`, `a[..., y]`) — before access, which would
        // otherwise consume the `~`/identifier as a bare reference.
        spread_update,
        // Dialect invocation `%mod{ … }` (glued `{`) — before access, which would otherwise
        // consume the import and leave `{ … }` to parse as a separate block term.
        dialect_term,
        // Access (field/positional access, bare identifiers, and imports)
        map(access, Term::Access),
        // Operations
        select_term,
        state_term,
        tail_call,
    ))(input)
}

// Parse the state-sample operator (`?p`), glued like every
// select form. The target is an access: a variable or import member holding a pid.
// Ordering: `access` runs first in `primary`, so a trailing `?` on an identifier
// (`empty?`) is consumed there and never reaches this parser.
fn state_term(input: Span) -> IResult<Span, Term> {
    let start = input;
    let (rest, target) = preceded(char('?'), access)(input)?;
    Ok((
        rest,
        Term::State(target, Spanned(Some(token_span(start, 1)))),
    ))
}

fn chain(input: Span) -> IResult<Span, Chain> {
    let start = input;
    let (rest, mut chain) = alt((
        // Match pattern: pattern = chain_inner
        map(
            pair(
                terminated(spanned(match_pattern), tuple((ws1, char('='), ws1))),
                chain_inner,
            ),
            |((binding_span, binding), chain)| Chain {
                binding: Some(binding),
                binding_span: Spanned(Some(binding_span)),
                ..chain
            },
        ),
        // Plain chain
        chain_inner,
    ))(input)?;
    // Record the chain's start offset, for attaching leading trivia during formatting.
    chain.span = Spanned(Some(span_between(start, rest)));
    Ok((rest, chain))
}

/// A term is a primary optionally applied to a single argument by juxtaposition
/// (`f x`, `f [args]`, `f &g`, `@f x`, `^f [args]`, `~ [args]`, `^~ arg`). The gap is horizontal
/// whitespace, so it doesn't cross the newline that ends a chain. Application by juxtaposition
/// targets an *applicable* head: a looked-up callable `Access` (a variable, `$`, import member,
/// builtin, tail call, or a ripple — whose head consumes the flowing value and the argument
/// applies to its result), or a spawn (`@f x` supplies the spawned function's init argument,
/// mirroring `x ~> @f`). A *bare* field access (`.f`) is the one access that can't be applied —
/// write `~.f`. A literal/tuple/function-literal head isn't applicable either (`#{…} 5` is a
/// syntax error). Exactly one argument: `f x y` fails at the chain separator.
fn term(input: Span) -> IResult<Span, Term> {
    let (input, head) = primary(input)?;
    let applicable = match &head {
        Term::Access(access) => access.source.is_some(),
        Term::Spawn(..) => true,
        _ => false,
    };
    if !applicable {
        return Ok((input, head));
    }
    // The `~` of a `~>` chain separator begins a valid primary (a ripple), so guard against
    // consuming it as an application argument.
    let (input, arg) = opt(preceded(pair(hspace1, not(peek(tag("~>")))), primary))(input)?;
    let Some(arg) = arg else {
        return Ok((input, head));
    };
    let term = match head {
        Term::Access(access) => Term::Apply(access, Box::new(arg)),
        Term::Spawn(function, _, span) => Term::Spawn(function, Some(Box::new(arg)), span),
        _ => unreachable!("only applicable heads reach here"),
    };
    Ok((input, term))
}

/// A chain is a sequence of `term`s joined by a mandatory `~>`; the value flows left→right
/// through them, with nil passing through (no short-circuit — that is the sequence separator's
/// job). Whitespace does NOT join chain terms (it binds a juxtaposed application argument to its
/// head instead — see [`term`]). The separator's surrounding whitespace may include newlines, so
/// `~>` doubles as a **line continuation**: a chain ends at a bare newline, but a newline
/// followed by `~>` continues it, so a long chain can span lines:
///   foo          //= 1
///   ~> bar       // a comment sits in the gap, like the assertion above it
///   ~> baz
///
/// Yields the chain the terms make — with the separators between them, and the `//= P`
/// assertions written in those gaps, each observing the value at the end of the line it
/// terminates, so its position is the number of terms parsed before it. Any binding is the
/// caller's to attach; a chain parsed here has none.
fn chain_inner(input: Span) -> IResult<Span, Chain> {
    let (mut rest, first) = term(input)?;
    let mut terms = vec![first];
    let mut continuations = Vec::new();
    let mut assertions = Vec::new();
    loop {
        // A backtrackable failure ends the chain, leaving the input at the term before the
        // separator — so a trailing `//= P` with no continuation under it falls to [`step`],
        // which takes it as the chain's own. A hard failure is a real syntax error and
        // propagates (a malformed assertion pattern, say, which `assertion` cuts on).
        let (next, (continuation, mut found)) = match chain_continuation(rest) {
            Ok(ok) => ok,
            Err(nom::Err::Error(_)) => break,
            Err(err) => return Err(err),
        };
        let (next, next_term) = match term(next) {
            Ok(ok) => ok,
            Err(nom::Err::Error(_)) => break,
            Err(err) => return Err(err),
        };
        for assertion in &mut found {
            assertion.after = terms.len();
        }
        assertions.extend(found);
        continuations.push(continuation);
        terms.push(next_term);
        rest = next;
    }
    Ok((
        rest,
        Chain {
            binding: None,
            binding_span: Spanned::default(),
            span: Spanned::default(),
            terms,
            continuations,
            assertions,
        },
    ))
}

/// The `~>` separator between two chain terms, together with the trivia an author may write in
/// the gap before it: whitespace, line comments, and `//= P` assertions. Both of the latter run
/// to the end of their line, so whatever follows one is necessarily a continuation line — which
/// is what lets them sit here at all.
///
/// Fails (consuming nothing) when no `~>` follows, so an assertion ending the chain's last line
/// is left for [`step`]. The separator must still be set off from the term before it, as it was
/// when this was a bare `ws1 "~>" ws1`.
fn chain_continuation(input: Span) -> IResult<Span, (Continuation, Vec<Assertion>)> {
    let mut rest = input;
    let mut assertions = Vec::new();
    let mut own_line = false;
    loop {
        let (next, gap) = recognize(many0(alt((
            nom_value((), multispace1),
            nom_value((), comment),
        ))))(rest)?;
        own_line |= gap.fragment().contains(['\n', '\r']);
        rest = next;
        if !rest.fragment().starts_with("//=") {
            break;
        }
        let (next, mut found) = assertion(rest)?;
        found.own_line = own_line;
        assertions.push(found);
        own_line = false;
        rest = next;
    }
    if rest.location_offset() == input.location_offset() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Space,
        )));
    }
    let pipe = rest;
    let (rest, _) = tag("~>")(rest)?;
    let (rest, _) = ws1(rest)?;
    Ok((
        rest,
        (
            Continuation {
                end: Spanned(Some(token_span(input, 0))),
                pipe: Spanned(Some(token_span(pipe, 2))),
            },
            assertions,
        ),
    ))
}

/// Separator between the chains of a sequence: a semicolon or a newline (they are synonyms), with
/// surrounding horizontal whitespace and line comments, collapsing runs of them. Unlike the
/// chain-step separator (horizontal whitespace), this is where the value short-circuits on nil and
/// a new binding scope point begins. A newline therefore ends a chain and starts the next step.
/// A comma is NOT a step separator — commas are bracket-only (tuple fields, type args, select
/// sources); `parse` maps a step-level comma to the pointed `StepComma` error.
fn seq_sep(input: Span) -> IResult<Span, ()> {
    nom_value(
        (),
        tuple((
            // Leading horizontal whitespace / line comments (a newline here is the separator).
            many0(alt((nom_value((), space1), nom_value((), comment)))),
            // The separator itself: a semicolon or a newline.
            alt((nom_value((), char(';')), nom_value((), line_ending))),
            // Collapse any following whitespace, comments, and further separators.
            many0(alt((
                nom_value((), multispace1),
                nom_value((), comment),
                nom_value((), char(';')),
            ))),
        )),
    )(input)
}

/// Guard the boundary after a sequence: peek past horizontal whitespace and fail hard on the two
/// step-level mistakes, positioned on the offending token, so `parse` reports a pointed error
/// rather than a generic one from backtracking. Consumes nothing.
///
/// - A comma is a step-level comma (every legitimate comma — tuple fields, type args, select
///   sources — is consumed by its bracket's own parser before a step list sees it). Reported as
///   `StepComma` via the fragment check in `parse`.
/// - Any other token that isn't a legitimate sequence terminator (`}`/`]`/`)`/`|`/`;`, a `=>`
///   consequence marker, a comment, a newline, or end of input) is a chain term missing its `~>`
///   separator — whitespace no longer joins terms. Reported as `MissingChainArrow` via the
///   `Failure(Space)` code in `parse`. A `~` is let through so a dangling `~>` keeps its own
///   `ExpectedPipe` diagnosis.
fn sequence_boundary_cut(input: Span) -> IResult<Span, ()> {
    let (after_ws, _) = many0(nom_value((), space1))(input)?;
    let fragment = after_ws.fragment();
    if fragment.starts_with(',') {
        return Err(nom::Err::Failure(nom::error::Error::new(
            after_ws,
            nom::error::ErrorKind::Char,
        )));
    }
    // The missing-arrow diagnosis only applies when whitespace actually separates the sequence
    // from the offending token — two space-separated terms. A token glued to the sequence (e.g.
    // the `{` of a malformed function literal whose bare `#` parsed as a term) is some other
    // syntax error; defer to the ordinary error machinery for a better diagnosis.
    if after_ws.location_offset() == input.location_offset() {
        return Ok((input, ()));
    }
    let terminated = fragment.is_empty()
        || fragment.starts_with(['}', ']', ')', '|', ';', '\n', '\r', '~'])
        || fragment.starts_with("=>")
        || (fragment.starts_with("//") && !fragment.starts_with("//="));
    if !terminated {
        return Err(nom::Err::Failure(nom::error::Error::new(
            after_ws,
            nom::error::ErrorKind::Space,
        )));
    }
    Ok((input, ()))
}

/// A step-final assertion: `//= P`, with an optional prose note separated from the pattern by
/// three or more spaces (the note runs to the end of the line). The pattern is ordinary match
/// grammar; once the marker is seen, a malformed pattern is a hard error rather than a
/// backtrack, since the text can no longer be anything else. Like a comment, the assertion
/// terminates its line: code after the pattern is a hard error (smuggled out with the
/// otherwise-unused `CrLf` code, which `parse` maps to `AssertionNotLineFinal`).
fn assertion(input: Span) -> IResult<Span, Assertion> {
    let start = input;
    let (input, _) = tag("//=")(input)?;
    let (input, _) = space0(input)?;
    let (input, pattern) = cut(match_pattern)(input)?;
    let (input, note) = opt(map(
        pair(
            verify(space1, |gap: &Span| gap.fragment().len() >= 3),
            take_while1(|c| c != '\n' && c != '\r'),
        ),
        |(_, note): (_, Span)| note.fragment().trim_end().to_string(),
    ))(input)?;
    let (input, _) = space0(input)?;
    if !input.fragment().is_empty() && !input.fragment().starts_with(['\n', '\r']) {
        return Err(nom::Err::Failure(nom::error::Error::new(
            input,
            nom::error::ErrorKind::CrLf,
        )));
    }
    Ok((
        input,
        Assertion {
            pattern,
            after: 0,
            note,
            own_line: false,
            span: Spanned(Some(span_between(start, input))),
        },
    ))
}

/// The gap before a step-final assertion: horizontal space for a trailing `//= P`, or any run
/// of newlines, blank lines, comments and `;` separators for one on its own line — a leading
/// `//=` continues the step above, so the separators between belong to the step. Answers
/// whether the gap crossed a line break, i.e. whether the assertion sits on its own line.
fn assertion_gap(input: Span) -> IResult<Span, bool> {
    let (rest, gap) = recognize(many0(alt((
        nom_value((), multispace1),
        nom_value((), comment),
        nom_value((), char(';')),
    ))))(input)?;
    Ok((rest, gap.fragment().contains(['\n', '\r'])))
}

/// A step consisting solely of `//=` assertion lines — `//=` opening a block, a branch, or a
/// REPL entry. The chain is empty, so the step's value is the block's input, exactly as a bare
/// `~` step's would be; the assertions observe it. Consumes nothing: the shared assertion loop
/// in [`step`] takes the lines themselves.
fn assertion_only_step(input: Span) -> IResult<Span, Step> {
    let (rest, _) = peek(tag("//="))(input)?;
    Ok((
        rest,
        Step::Chain(Chain {
            binding: None,
            binding_span: Spanned(None),
            span: Spanned(Some(token_span(input, 4))),
            terms: Vec::new(),
            continuations: Vec::new(),
            assertions: Vec::new(),
        }),
    ))
}

/// One step of a sequence: a type-alias declaration, a chain, or nothing but assertion lines,
/// carrying any `//= P` assertions that end the step's last line (chains only — an alias
/// produces no value to assert on). One may trail on that line; further `//=` lines below
/// continue it, each observing the same value. Assertions written *above* the last line were
/// already taken by [`chain_inner`], the continuation under them being what marks them as
/// mid-chain.
///
/// The alias alternative is tried first because an alias whose right-hand side is a function type
/// also parses as a chain (`'q<'t> = #['int] -> ('t | [])` reads as a binding of an identity
/// literal with a declared return type). No chain term can begin with `'`, so a leading `'` at
/// step position is unambiguously an alias.
fn step(input: Span) -> IResult<Span, Step> {
    let (input, step) = alt((type_alias, map(chain, Step::Chain), assertion_only_step))(input)?;
    let before_assertions = input;
    let (input, assertions) = many0(map(
        pair(assertion_gap, assertion),
        |(own_line, mut assertion)| {
            assertion.own_line = own_line;
            assertion
        },
    ))(input)?;
    match (step, assertions) {
        (step, assertions) if assertions.is_empty() => Ok((input, step)),
        (Step::Chain(mut chain), assertions) => {
            // These end the chain, so they observe its result — every term having run.
            let after = chain.terms.len();
            chain.assertions.extend(
                assertions
                    .into_iter()
                    .map(|assertion| Assertion { after, ..assertion }),
            );
            Ok((input, Step::Chain(chain)))
        }
        // An alias produces no value to assert on. Positioned on the first `//=`, and
        // smuggled out as a hard failure with the (otherwise unused) `Not` code, which
        // `parse` maps to `AssertionOnAlias`.
        (Step::TypeAlias { .. }, assertions) => {
            let offset = assertions[0]
                .span
                .get()
                .expect("the assertion parser always records a span")
                .offset;
            Err(nom::Err::Failure(nom::error::Error::new(
                before_assertions.slice(offset - before_assertions.location_offset()..),
                nom::error::ErrorKind::Not,
            )))
        }
    }
}

fn sequence(input: Span) -> IResult<Span, Sequence> {
    let (rest, steps) = terminated(separated_list1(seq_sep, step), opt(seq_sep))(input)?;
    let (rest, _) = sequence_boundary_cut(rest)?;
    Ok((rest, Sequence { steps }))
}

fn type_alias(input: Span) -> IResult<Span, Step> {
    map(
        tuple((
            // `'name` for a named alias, or a bare `'` for the module's nameless
            // default-type marker (`' = ...` / `'<'t> = ...`).
            spanned(preceded(char('\''), opt(identifier))),
            opt(delimited(
                char('<'),
                separated_list1(tuple((ws0, char(','), ws0)), type_name),
                char('>'),
            )),
            preceded(tuple((ws0, char('='), ws0)), type_definition),
        )),
        |((name_span, name), type_parameters, type_definition)| Step::TypeAlias {
            name,
            name_span: Spanned(Some(name_span)),
            type_parameters: type_parameters.unwrap_or_default(),
            type_definition,
        },
    )(input)
}

/// A program is a single sequence: chain steps with type-alias declarations interspersed, all
/// separated by the sequence separator (semicolon or newline, which are synonyms). The chains
/// thread and short-circuit as one sequence; type aliases are transparent to that flow. (There is
/// no separator other than the step separator; `,` appears only inside brackets.)
///
/// This uses [`step`] directly rather than [`sequence`] so an empty program parses (a module may
/// declare only types), and so the boundary cut runs against end-of-input.
fn program(input: Span) -> IResult<Span, Sequence> {
    map(
        delimited(
            ws_with_comments,
            terminated(
                terminated(separated_list0(seq_sep, step), opt(seq_sep)),
                sequence_boundary_cut,
            ),
            pair(ws_with_comments, nom::combinator::eof),
        ),
        |steps| Sequence { steps },
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_context_detection_unclosed_tuple() {
        let source = "#{ [1, 2, 3 }";
        let result = parse(source);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err.kind, ErrorKind::UnterminatedTuple));
    }

    #[test]
    fn test_context_detection_function_body() {
        let source = "#{ x => }";
        let result = parse(source);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err.kind, ErrorKind::InvalidFunctionBody));
    }

    #[test]
    fn test_context_detection_unterminated_string() {
        let source = "#{ \"hello }";
        let result = parse(source);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err.kind, ErrorKind::UnterminatedString));
    }

    #[test]
    fn test_context_detection_unclosed_block() {
        let source = "#{ { x = 1 ";
        let result = parse(source);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err.kind, ErrorKind::UnterminatedBlock));
    }

    #[test]
    fn test_error_has_span() {
        let source = "#{ [1, 2, 3 }";
        let result = parse(source);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.span.is_some());
        let span = err.span.unwrap();
        assert_eq!(span.line, 1);
        assert!(span.column > 0);
    }

    #[test]
    fn test_valid_program_parses() {
        let source = "#{ [1, 2] ~> __integer_add__ }";
        let result = parse(source);
        assert!(result.is_ok());
    }

    /// The slice of `source` covered by a span.
    fn slice(source: &str, span: SourceSpan) -> &str {
        &source[span.offset..span.offset + span.length]
    }

    #[test]
    fn parse_populates_access_spans() {
        let source = "point ~> double";
        let program = parse(source).unwrap();
        let terms = &program.steps[0].as_chain().expect("chain step").terms;
        let Term::Access(a0) = &terms[0] else {
            panic!("expected access term");
        };
        assert_eq!(slice(source, a0.span.get().unwrap()), "point");
        let Term::Access(a1) = &terms[1] else {
            panic!("expected access term");
        };
        assert_eq!(slice(source, a1.span.get().unwrap()), "double");
    }

    #[test]
    fn parse_access_type_arguments() {
        // A glued `<…>` suffix on an access head carries explicit type arguments; the
        // juxtaposed argument stays a separate application argument.
        let source = "map<'int, Str['bin]> [xs, f]";
        let program = parse(source).unwrap();
        let Term::Apply(access, _) = &program.steps[0].as_chain().expect("chain step").terms[0]
        else {
            panic!("expected apply term");
        };
        assert_eq!(access.type_arguments.len(), 2);
        assert_eq!(slice(source, access.span.get().unwrap()), "map");

        // An import-member head takes the suffix too.
        let program = parse("%iter.fold<'int>").unwrap();
        let Term::Access(access) = &program.steps[0].as_chain().expect("chain step").terms[0]
        else {
            panic!("expected access term");
        };
        assert_eq!(access.type_arguments.len(), 1);
    }

    #[test]
    fn parse_populates_binding_span() {
        let source = "total = 5";
        let program = parse(source).unwrap();
        let span = program.steps[0]
            .as_chain()
            .expect("chain step")
            .binding_span
            .get()
            .unwrap();
        assert_eq!(slice(source, span), "total");
    }

    #[test]
    fn parse_populates_type_alias_name_span() {
        let source = "'point = Point[x: 'int, y: 'int]";
        let program = parse(source).unwrap();
        let Step::TypeAlias { name_span, .. } = &program.steps[0] else {
            panic!("expected type alias step");
        };
        assert_eq!(slice(source, name_span.get().unwrap()), "'point");
    }

    #[test]
    fn test_span_invalid_escape_sequence() {
        // Note: Currently invalid escapes produce UnexpectedEndOfInput errors
        // because nom converts our custom errors. This could be improved in future.
        let source = r#"#{ "hello\xworld" }"#;
        let result = parse(source);
        assert!(result.is_err());
        let err = result.unwrap_err();
        // Verify we still capture span information
        assert!(err.span.is_some());
        let span = err.span.unwrap();
        assert_eq!(span.line, 1);
    }

    #[test]
    fn test_step_comma_is_a_pointed_error() {
        // `,` is not a step separator; the error names the `;` fix and points at the comma —
        // at the top level, in a block body, in a consequence, and in an annotation prefix.
        for (source, column) in [
            ("1, 2", 2),
            ("x = { 1, 2 }; x", 8),
            ("5 ~> { =5 => 1, 2 | 0 }", 15),
            ("f = #'int { :doc \"d\", :pre #{ Ok } | $ }; 1 ~> f", 21),
        ] {
            let err = parse(source).expect_err(source);
            assert!(
                matches!(err.kind, ErrorKind::StepComma),
                "for {source}: {:?}",
                err.kind
            );
            let span = err.span.expect(source);
            assert_eq!((span.line, span.column), (1, column), "for {source}");
        }
        // Bracket-internal commas are untouched: tuple fields, type parameters, select sources.
        for source in [
            "[1, 2] ~> f",
            "f = #<'t, 'u>['t, 'u] { $0 }",
            "p = @#{ 42 }; ![p, 1000]",
        ] {
            assert!(parse(source).is_ok(), "expected {source} to parse");
        }
    }

    #[test]
    fn test_assertion_on_alias_is_a_pointed_error() {
        // An alias produces no value to assert on — trailing or on the following line; the
        // error points at the `//=`.
        for (source, line, column) in [("'t = 'int //= Ok", 1, 11), ("'t = 'int\n//= Ok\n5", 2, 1)]
        {
            let err = parse(source).expect_err(source);
            assert!(
                matches!(err.kind, ErrorKind::AssertionOnAlias),
                "for {source}: {:?}",
                err.kind
            );
            let span = err.span.expect(source);
            assert_eq!((span.line, span.column), (line, column), "for {source}");
        }
    }

    #[test]
    fn test_assertion_must_end_its_line() {
        // An assertion terminates its line, like the comment it resembles: code after the
        // pattern (or note) is a pointed error at the offending character.
        for (source, line, column) in [
            ("5 //= 5; Ok", 1, 8),
            ("{ 5 //= 6 }", 1, 11),
            ("5 //= 5 ~> f", 1, 9),
        ] {
            let err = parse(source).expect_err(source);
            assert!(
                matches!(err.kind, ErrorKind::AssertionNotLineFinal),
                "for {source}: {:?}",
                err.kind
            );
            let span = err.span.expect(source);
            assert_eq!((span.line, span.column), (line, column), "for {source}");
        }
    }

    #[test]
    fn test_own_line_assertions_attach_to_the_step_above() {
        // A leading `//=` continues the step: blank lines, comments and `;` between belong
        // to it, and several stack. At the start of a sequence the chain is empty.
        for (source, terms, assertions) in [
            ("5\n//= 5", 1, 1),
            ("5 //= 'int\n//= 5", 1, 2),
            ("5;\n\n// why\n//= 5", 1, 1),
            ("//= []\n5", 0, 1),
        ] {
            let program = parse(source).unwrap_or_else(|e| panic!("{source}: {e:?}"));
            let Step::Chain(chain) = &program.steps[0] else {
                panic!("{source}: expected a chain step");
            };
            assert_eq!(chain.terms.len(), terms, "for {source}");
            assert_eq!(chain.assertions.len(), assertions, "for {source}");
        }
    }

    #[test]
    fn test_missing_chain_arrow_is_a_pointed_error() {
        // Whitespace no longer joins chain terms; the error names the `~>`/`;` fix and points
        // at the term that is missing its separator.
        for (source, column) in [
            ("5 double", 3),
            ("x = { 1 f }; x", 9),
            ("5 ~> { =5 => 1 f | 0 }", 16),
        ] {
            let err = parse(source).expect_err(source);
            assert!(
                matches!(err.kind, ErrorKind::MissingChainArrow),
                "for {source}: {:?}",
                err.kind
            );
            let span = err.span.expect(source);
            assert_eq!((span.line, span.column), (1, column), "for {source}");
        }
        // Legitimate sequence terminators do not trip the cut.
        for source in [
            "5 ~> double",
            "5 ~> { =5 => 1 | 0 }",
            "x = 5; x",
            "f = #'int { $ }",
            "5 ~> double // trailing comment",
        ] {
            assert!(parse(source).is_ok(), "expected {source} to parse");
        }
    }

    #[test]
    fn test_large_integer_literal_parses() {
        // Integers are arbitrary-precision: a literal far beyond i64 range parses
        // successfully rather than overflowing.
        let source = "#{ 99999999999999999999 }";
        let result = parse(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_span_malformed_hex() {
        // Note: Currently malformed hex produces UnexpectedEndOfInput errors
        // because nom converts our custom errors. This could be improved in future.
        let source = "#{ <999999999999999999999> }";
        let result = parse(source);
        assert!(result.is_err());
        let err = result.unwrap_err();
        // Verify we still capture span information even if error kind is generic
        assert!(err.span.is_some());
        let span = err.span.unwrap();
        assert_eq!(span.line, 1);
    }

    #[test]
    fn test_binary_group_must_be_whole_bytes() {
        // Grouping is checked per group, not just on the total: `<0a1 b2c>` has six digits
        // but neither group is a whole number of bytes, and the error names the first.
        let err = parse("#{ <0a1 b2c> }").expect_err("an odd group must not parse");
        assert!(
            matches!(&err.kind, ErrorKind::HexMalformed(group) if group == "0a1"),
            "expected the offending group to be named, got {:?}",
            err.kind
        );
    }

    #[test]
    fn test_binary_grouping_does_not_change_the_value() {
        assert_eq!(
            binary_bytes("<6a09e667 bb67ae85>"),
            binary_bytes("<6a09e667bb67ae85>")
        );
        assert_eq!(
            binary_bytes("<\n  6a09e667\n  bb67ae85\n>"),
            binary_bytes("<6a09e667bb67ae85>")
        );
    }

    #[test]
    fn test_binary_rows_are_recorded_as_written() {
        let row_count = |source| match parse(source)
            .expect("source must parse")
            .chains()
            .flat_map(|chain| &chain.terms)
            .find_map(|term| match term {
                Term::Literal(Literal::Binary(binary)) => Some(binary.row_count()),
                _ => None,
            }) {
            Some(rows) => rows,
            None => panic!("program must contain a binary literal"),
        };
        assert_eq!(row_count("<0a1b 2c3d>"), 1);
        assert_eq!(row_count("<\n  0a1b\n  2c3d\n>"), 2);
        // A blank line carries no bytes, so it is not a row.
        assert_eq!(row_count("<\n  0a1b\n\n  2c3d\n>"), 2);
        assert_eq!(row_count("<>"), 0);
    }

    /// The bytes of the binary literal a single-step program evaluates to.
    fn binary_bytes(source: &str) -> Vec<u8> {
        parse(source)
            .expect("source must parse")
            .chains()
            .flat_map(|chain| &chain.terms)
            .find_map(|term| match term {
                Term::Literal(Literal::Binary(binary)) => Some(binary.bytes().to_vec()),
                _ => None,
            })
            .expect("program must contain a binary literal")
    }

    #[test]
    fn test_span_error_at_start() {
        let source = "[1, 2, 3";
        let result = parse(source);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.span.is_some());
        let span = err.span.unwrap();
        assert_eq!(span.line, 1);
        assert_eq!(span.column, 1); // Error at the very start
    }

    #[test]
    fn test_span_error_at_end() {
        let source = "#{ [1, 2, 3 ";
        let result = parse(source);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.span.is_some());
        let span = err.span.unwrap();
        assert_eq!(span.line, 1);
    }

    #[test]
    fn test_span_with_unicode() {
        // Test that column positions work correctly with Unicode characters
        // Note: Custom errors are converted by nom, so we just verify span is captured
        let source = "#{ \"hello 世界\\x\" }";
        let result = parse(source);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.span.is_some());
        // The span should correctly identify position even after Unicode chars
    }

    #[test]
    fn test_span_multiline_error() {
        // Note: Top-level error detection currently reports span from parse start,
        // not the specific error location. This could be improved.
        let source = "#{\n  [1, 2, 3\n}";
        let result = parse(source);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err.kind, ErrorKind::UnterminatedTuple));
        assert!(err.span.is_some());
        // Span is captured, even if not pinpointing exact error location
    }

    #[test]
    fn test_process_ref_large_integer_parses() {
        // A large integer spawn argument parses (arbitrary-precision integers no
        // longer overflow at parse time).
        let source = "#{ @99999999999999999999 }";
        let result = parse(source);
        assert!(result.is_ok());
    }

    #[test]
    fn multiline_dedents_by_closing_margin() {
        // The closing delimiter's indentation sets the margin; extra indent is preserved,
        // and the newline before the closing delimiter is not part of the value.
        let raw = "\n    hello\n      indented\n    ";
        assert_eq!(
            process_multiline_string(raw),
            Some("hello\n  indented".to_string())
        );
    }

    #[test]
    fn multiline_empty_and_blank_lines() {
        // No content lines yields the empty string.
        assert_eq!(process_multiline_string("\n    "), Some(String::new()));
        // A blank line is emitted empty regardless of its own indentation.
        let raw = "\n    a\n\n    b\n    ";
        assert_eq!(process_multiline_string(raw), Some("a\n\nb".to_string()));
    }

    #[test]
    fn multiline_processes_escapes() {
        let raw = "\n    a\\tb\\n\\\"c\n    ";
        assert_eq!(process_multiline_string(raw), Some("a\tb\n\"c".to_string()));
    }

    #[test]
    fn multiline_strips_trailing_whitespace_but_s_protects() {
        // Literal trailing whitespace is stripped; `\s` survives as a space.
        let raw = "\n    keep:\\s   \n    gone:   \n    ";
        assert_eq!(
            process_multiline_string(raw),
            Some("keep: \ngone:".to_string())
        );
    }

    #[test]
    fn multiline_line_continuation() {
        // `\` at end of line drops the newline and the next line's leading whitespace;
        // a space before the `\` is kept as the join separator.
        let raw = "\n    one \\\n    two\n    three\n    ";
        assert_eq!(
            process_multiline_string(raw),
            Some("one two\nthree".to_string())
        );
    }

    #[test]
    fn multiline_rejects_bad_structure() {
        // Text after the opening delimiter.
        assert_eq!(process_multiline_string("oops\n    "), None);
        // A line indented less than the closing margin.
        assert_eq!(process_multiline_string("\n  under\n    "), None);
        // The closing delimiter not alone on its line.
        assert_eq!(process_multiline_string("\n    x"), None);
        // An invalid escape.
        assert_eq!(process_multiline_string("\n    \\q\n    "), None);
    }

    #[test]
    fn multiline_normalizes_crlf() {
        let raw = "\r\n    a\r\n    b\r\n    ";
        assert_eq!(process_multiline_string(raw), Some("a\nb".to_string()));
    }

    #[test]
    fn multiline_escaped_quote_does_not_close() {
        // `\"""` is an escaped quote followed by the closing delimiter, so the value is `"`.
        let source = "#{ \"\"\"\n    \\\"\"\"\n    \"\"\" }";
        assert!(parse(source).is_ok());
    }

    #[test]
    fn multiline_unterminated_reports_string_error() {
        let source = "#{ \"\"\"\n    hello\n }";
        let err = parse(source).unwrap_err();
        assert!(matches!(err.kind, ErrorKind::UnterminatedString));
    }

    #[test]
    fn select_shorthand_accepts_module_type() {
        // `!'%mod.name` desugars exactly as `!#'%mod.name` (a body-less identity receive);
        // spans compare always-equal, so this pins the structural desugaring.
        assert_eq!(
            parse("#{ !'%proc.changed }").unwrap(),
            parse("#{ !#'%proc.changed }").unwrap()
        );
        // A module's default type, with and without type arguments.
        assert_eq!(
            parse("#{ !'%mod }").unwrap(),
            parse("#{ !#'%mod }").unwrap()
        );
        assert_eq!(
            parse("#{ !'%list<'int> }").unwrap(),
            parse("#{ !#'%list<'int> }").unwrap()
        );
        // A same-line block is the receive's filter body, as for `!'int { … }`.
        assert_eq!(
            parse("#{ !'%proc.changed { Ok } }").unwrap(),
            parse("#{ !#'%proc.changed { Ok } }").unwrap()
        );
    }

    #[test]
    fn spawn_shorthand_accepts_module_type() {
        // `@'%mod.name { … }` desugars exactly as `@#'%mod.name { … }`.
        assert_eq!(
            parse("#{ @'%proc.changed { $ } }").unwrap(),
            parse("#{ @#'%proc.changed { $ } }").unwrap()
        );
    }

    #[test]
    fn select_shorthand_accepts_named_tuple_type() {
        // A named tuple type desugars exactly as its `!#` form — with fields, bare, and
        // with a filter body.
        assert_eq!(
            parse("#{ !Reply['ref, 'bin] }").unwrap(),
            parse("#{ !#Reply['ref, 'bin] }").unwrap()
        );
        assert_eq!(parse("#{ !Done }").unwrap(), parse("#{ !#Done }").unwrap());
        assert_eq!(
            parse("#{ !Reply['ref, 'bin] { Ok } }").unwrap(),
            parse("#{ !#Reply['ref, 'bin] { Ok } }").unwrap()
        );
    }

    #[test]
    fn select_bracket_forms_stay_source_lists() {
        // Only *named* tuple types get the receive sugar: a glued `[…]` is the general
        // source-list form, so `![]` is the empty select, not a nil receive.
        assert_ne!(parse("#{ ![] }").unwrap(), parse("#{ !#[] }").unwrap());
        // And `![Done]` stays a source list (a chain whose term is the tuple `Done`),
        // not a `[Done]` receive type.
        assert_ne!(
            parse("#{ ![Done] }").unwrap(),
            parse("#{ !#[Done] }").unwrap()
        );
    }
}
