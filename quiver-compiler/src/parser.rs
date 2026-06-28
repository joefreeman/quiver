use crate::ast::*;
use nom::{
    IResult, Slice,
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{char, digit1, line_ending, multispace0, multispace1, satisfy, space1},
    combinator::{map, map_res, not, opt, peek, recognize, success, value as nom_value, verify},
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
            ErrorKind::HexMalformed(_) => {
                "Binary literals must contain only hexadecimal digits: 0-9, a-f, A-F"
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

pub fn parse(source: &str) -> Result<Program, Error> {
    let span = Span::new(source);
    match program(span) {
        Ok((remaining, prog)) => {
            // Check if there's unparsed input remaining
            let remaining_fragment = remaining.fragment().trim();
            if !remaining_fragment.is_empty() {
                let span = Some(SourceSpan::from_span(remaining));
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
            let (span, kind) = match &e {
                nom::Err::Error(e) | nom::Err::Failure(e) => {
                    let span = Some(SourceSpan::from_span(e.input));
                    let fragment = e.input.fragment();

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
/// newline that separates statements.
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
    preceded(tag("//"), take_while(|c| c != '\n' && c != '\r'))(input)
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
            // Decimal only. Hexadecimal byte sequences are binary literals (`0x...`).
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

fn binary_literal(input: Span) -> IResult<Span, Literal> {
    // Hexadecimal bytes: `0x` followed by an even number of hex digits. `0x` alone is
    // the empty binary.
    let (remaining, content) =
        preceded(tag("0x"), take_while(|c: char| c.is_ascii_hexdigit()))(input)?;

    // Decode the hex string - this fails on an odd number of digits.
    match hex::decode(content.fragment()) {
        Ok(bytes) => Ok((remaining, Literal::Binary(bytes))),
        Err(_) => Err(nom::Err::Failure(nom::error::Error::new(
            content,
            nom::error::ErrorKind::HexDigit,
        ))),
    }
}

/// Build the `Str[0x…]` tuple term that a string literal desugars to.
fn str_term(bytes: Vec<u8>) -> Term {
    Term::Tuple(Tuple {
        name: TupleName::Named("Str".to_string()),
        fields: vec![TupleField {
            name: None,
            name_span: Spanned::default(),
            span: Spanned::default(),
            value: FieldValue::Chain(Chain {
                match_pattern: None,
                bind_span: Spanned::default(),
                span: Spanned::default(),
                terms: vec![Term::Literal(Literal::Binary(bytes))],
            }),
        }],
        span: Spanned::default(),
    })
}

/// Build the `Str[0x…]` tuple pattern that a string literal pattern desugars to.
fn str_match(bytes: Vec<u8>) -> Match {
    Match::Tuple(MatchTuple {
        name: Some("Str".to_string()),
        fields: vec![MatchField {
            name: None,
            pattern: Match::Literal(Literal::Binary(bytes)),
        }],
    })
}

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
        map(multiline_string, |s| str_term(s.into_bytes())),
        map(single_line_string, |s| str_term(s.into_bytes())),
    ))(input)
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
            match_pattern: None,
            bind_span: Spanned::default(),
            span: Spanned::default(),
            terms: vec![Term::Literal(Literal::Integer(value))],
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

/// Process the raw text between `"""` delimiters into the final string value, or `None` if the
/// text is malformed. The opening delimiter must be followed by a newline; the closing delimiter
/// sits on its own line, and its indentation (the *margin*) is stripped from every line. After
/// de-indentation, escapes are processed — the single-line set plus `\s` (a strip-proof space)
/// and `\<newline>` (line continuation, dropping the newline and the next line's leading
/// whitespace). Trailing whitespace is stripped from each line; blank lines are emitted empty.
fn process_multiline_string(raw: &str) -> Option<String> {
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

fn literal(input: Span) -> IResult<Span, Literal> {
    // Binary first: `0x...` would otherwise be read as the integer `0`.
    alt((binary_literal, integer_literal))(input)
}

// Pattern parsers for terms

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
            // Variable pin with & prefix
            map(preceded(char('&'), spanned(identifier)), |(span, name)| {
                Match::Reference(name, Spanned(Some(span)))
            }),
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

fn field_type(input: Span) -> IResult<Span, FieldType> {
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
                        identifier: Some(id),
                        type_arguments: type_args.unwrap_or_default(),
                    }
                } else {
                    FieldType::Spread {
                        identifier: None,
                        type_arguments: vec![],
                    }
                }
            },
        ),
        // Named field: name: type
        map(
            separated_pair(identifier, tuple((char(':'), ws1)), type_definition),
            |(name, type_def)| FieldType::Field {
                name: Some(name),
                type_def,
            },
        ),
        // Unnamed field: type
        map(type_definition, |type_def| FieldType::Field {
            name: None,
            type_def,
        }),
    ))(input)
}

fn field_type_list(input: Span) -> IResult<Span, Vec<FieldType>> {
    terminated(
        separated_list0(tuple((wsc, char(','), wsc)), field_type),
        opt(pair(wsc, char(','))),
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

fn process_type(input: Span) -> IResult<Span, Type> {
    map(
        alt((
            // (@...) - parenthesized arrow forms (@ is inside parens)
            delimited(
                char('('),
                alt((
                    // (@-> type) - return only
                    map(
                        preceded(tuple((char('@'), ws0, tag("->"), ws1)), base_type),
                        |return_type| (None, Some(return_type)),
                    ),
                    // (@type -> type) - both receive and return
                    map(
                        preceded(
                            char('@'),
                            separated_pair(base_type, tuple((ws1, tag("->"), ws1)), base_type),
                        ),
                        |(receive_type, return_type)| (Some(receive_type), Some(return_type)),
                    ),
                )),
                char(')'),
            ),
            // @ with optional type - no arrow (@ is outside)
            map(preceded(char('@'), opt(base_type)), |receive_type| {
                (receive_type, None)
            }),
        )),
        |(receive_type, return_type)| {
            Type::Process(ProcessType {
                receive_type: receive_type.map(Box::new),
                return_type: return_type.map(Box::new),
            })
        },
    )(input)
}

fn function_type(input: Span) -> IResult<Span, Type> {
    map(
        preceded(
            char('#'),
            separated_pair(
                function_input_type,
                tuple((ws1, tag("->"), ws1)),
                function_output_type,
            ),
        ),
        |(input, output)| {
            Type::Function(FunctionType {
                input: Box::new(input),
                output: Box::new(output),
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
        process_type,
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

// Parse access patterns: identifier, $, ~, %import, with optional .field accessors and [args]
fn access(input: Span) -> IResult<Span, Access> {
    let start = input;
    let (after_source, source) = opt(alt((
        map(char('$'), |_| AccessSource::Parameter),
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
    // accessor may follow immediately, with no dot. Restricted to the parameter source — `~`,
    // imports and identifiers keep requiring the dot (e.g. `foo` is a variable, not `f.oo`).
    let (after_source, leading) = if matches!(source, Some(AccessSource::Parameter)) {
        opt(accessor)(after_source)?
    } else {
        (after_source, None)
    };

    // Each accessor carries its own span (`.triple` → the `triple`), so the language server can
    // hover/navigate components separately.
    let (after_ref, dotted) = many0(preceded(char('.'), accessor))(after_source)?;
    let (accessors, accessor_spans): (Vec<_>, Vec<_>) = leading.into_iter().chain(dotted).unzip();

    // The span covers just the reference (`%num.add`, `foo`, `$.x`), not a trailing call
    // argument, so hover and go-to-definition land precisely on the referenced symbol.
    let ref_span = span_between(start, after_ref);

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
            base_span: Spanned(Some(base_span)),
            span: Spanned(Some(ref_span)),
        },
    ))
}

/// Parse a name-preserving tuple spread-update: `~[..., y]` (spread the chained value) or
/// `a[..., y]` (spread the variable `a`), each inheriting the source's tuple name. Produces a
/// `Term::Tuple` — the spread-update that `Access` used to carry as an argument now lives here.
fn spread_update(input: Span) -> IResult<Span, Term> {
    let (input, source) = alt((map(char('~'), |_| None::<String>), map(identifier, Some)))(input)?;
    let (input, (bracket_span, mut fields)) = adjacent_spread_args(input)?;
    // For `a[...]`, the bare spread `...` spreads `a`; rewrite it so the compiler loads from `a`.
    // A `~` appearing in a *field value* is a chained-value ripple and is left untouched.
    if let Some(name) = &source {
        for field in &mut fields {
            if matches!(field.value, FieldValue::Spread(None)) {
                field.value = FieldValue::Spread(Some(name.clone()));
            }
        }
    }
    Ok((
        input,
        Term::Tuple(Tuple {
            name: TupleName::Inherit,
            fields,
            span: Spanned(Some(bracket_span)),
        }),
    ))
}

// Helper to wrap a term in a single-element source chain
fn make_source_chain(term: Term) -> Chain {
    Chain {
        match_pattern: None,
        bind_span: Spanned::default(),
        span: Spanned::default(),
        terms: vec![term],
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
fn make_receive(param_type: Type, body: Option<Expression>) -> Term {
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
// The general form takes a *tuple* of sources and requires a space after `!` (`! [...]`),
// mirroring function application (`f [...]`). The tight forms (no space) are shorthand for
// selecting on a single source.
//
// Syntax forms (the type shorthands are *body-less* identity receives — a `{ … }` after a select
// is a handler chain-step that processes the received message, not a filter):
// - ! [...]          - Tuple of source chains (general form; space required). A filter is a
//                      function *with a body* here: `! [#'int { =42 => Ok }]` (only the general
//                      form can filter — it leaves a non-matching message in the mailbox, whereas
//                      a handler block consumes the message and discards it if it doesn't match).
// - !                - Bare select (postfix form, empty sources)
// - !(type)          - Identity receive with explicit type
// - !'type           - Identity receive for a named type
// - !#type           - Identity receive for a `#`-typed message
// - !var             - Single source (variable/process)
// - !@p              - Single source (spawn)
// - !1000            - Single source (timeout literal)
fn select_term(input: Span) -> IResult<Span, Term> {
    let start = input;
    let (rest, term) = preceded(
        char('!'),
        alt((
            // ` [...]` - Tuple of source chains (general form). The leading space distinguishes it
            // from the tight single-source shorthands, so `! f` is not a single-source form.
            map(
                delimited(
                    tuple((hspace1, char('['), wsc)),
                    separated_list0(tuple((wsc, char(','), wsc)), chain),
                    pair(wsc, char(']')),
                ),
                |sources| Term::Select(Some(sources), Spanned::default()),
            ),
            // (type) - parenthesized receive type (body-less identity receive).
            map(
                delimited(pair(char('('), wsc), type_definition, pair(wsc, char(')'))),
                |param_type| make_receive(param_type, None),
            ),
            // 'type - named receive type (body-less identity receive): `!'int`.
            map(type_identifier, |param_type| make_receive(param_type, None)),
            // access (variable / module member) → reference it as a single source. Select
            // sources are a tuple of values, so a callable source must be referenced rather
            // than called: the tight form `!f` desugars to `! [&f]` (the `&` is part of the
            // sugar). A process variable references harmlessly (`&p` is just `p`).
            map(access, |acc| single_source(Term::Reference(acc))),
            // #type - `#`-typed receive (body-less identity receive): `!#'int`, `!#Reply[...]`.
            map(preceded(char('#'), function_input_type), |param_type| {
                make_receive(param_type, None)
            }),
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
        // Spread with identifier: ...identifier
        map(preceded(tag("..."), identifier), |id| TupleField {
            name: None,
            name_span: Spanned::default(),
            span: Spanned::default(),
            value: FieldValue::Spread(Some(id)),
        }),
        // Spread chained value: ...
        map(tag("..."), |_| TupleField {
            name: None,
            name_span: Spanned::default(),
            span: Spanned::default(),
            value: FieldValue::Spread(None),
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
            },
        ),
        // [...] - unnamed tuple with fields
        map(
            delimited(pair(char('['), wsc), tuple_field_list, pair(wsc, char(']'))),
            |fields| Tuple {
                name: TupleName::Anonymous,
                fields,
                span: Spanned::default(),
            },
        ),
        // TupleName - bare tuple name without fields
        // Only parse if not followed by '(' (which would indicate a partial pattern)
        map(
            tuple((
                tuple_name,
                peek(not(pair(ws0, char('(')))), // Ensure not followed by '('
            )),
            |(name, _)| Tuple {
                name: TupleName::Named(name),
                fields: vec![],
                span: Spanned::default(),
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

/// An expression: `|`-separated branches (with an optional leading `|`). This is the shared
/// grammar for statement bodies, function bodies, and block contents.
fn expression(input: Span) -> IResult<Span, Expression> {
    map(
        preceded(
            opt(pair(char('|'), wsc)),
            separated_list1(tuple((wsc, char('|'), wsc)), branch),
        ),
        |branches| Expression { branches },
    )(input)
}

/// A block `{ … }`: a braced expression that introduces a new scope.
fn block(input: Span) -> IResult<Span, Expression> {
    delimited(pair(char('{'), wsc), expression, pair(wsc, char('}')))(input)
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

fn equality(input: Span) -> IResult<Span, Term> {
    nom_value(Term::Equality, tag("=="))(input)
}

fn not_term(input: Span) -> IResult<Span, Term> {
    nom_value(Term::Not, tag("<>"))(input)
}

// Build a spawn of an inline function (the `@{ … }` / `@'type { … }` sugar forms). The init
// argument, if any, comes from the chained value (`x ~> @{ … }`).
fn spawn_of_function(parameter_type: Option<Type>, body: Expression) -> Term {
    Term::Spawn(
        Box::new(Term::Function(Function {
            type_parameters: vec![],
            parameter_type,
            return_type: None,
            body: Some(body),
            span: Spanned::default(),
        })),
        Spanned::default(),
    )
}

fn spawn_term(input: Span) -> IResult<Span, Term> {
    let start = input;
    let (rest, term) = alt((
        // @{ ... } - Spawn parameterless function (sugar for @#{ ... })
        map(preceded(pair(char('@'), opt(ws1)), block), |body| {
            spawn_of_function(None, body)
        }),
        // @(type) { ... } - Spawn with parenthesized type (sugar for @#(type) { ... })
        map(
            tuple((
                preceded(
                    char('@'),
                    delimited(pair(char('('), wsc), type_definition, pair(wsc, char(')'))),
                ),
                preceded(opt(ws1), block),
            )),
            |(param_type, body)| spawn_of_function(Some(param_type), body),
        ),
        // @'type { ... } - Spawn with named type (sugar for @#'type { ... })
        map(
            tuple((
                preceded(char('@'), type_identifier),
                preceded(opt(ws1), block),
            )),
            |(param_type, body)| spawn_of_function(Some(param_type), body),
        ),
        // @[...] { ... } - Spawn with tuple type (sugar for @#[...] { ... })
        map(
            tuple((preceded(char('@'), tuple_type), preceded(opt(ws1), block))),
            |(tuple_ty, body)| spawn_of_function(Some(tuple_ty), body),
        ),
        // @<primary> - Match @ followed by optional primary (bare @ becomes @~)
        map(preceded(char('@'), opt(primary)), |opt_t| {
            Term::Spawn(
                Box::new(opt_t.unwrap_or(Term::Access(Access {
                    source: Some(AccessSource::Ripple),
                    accessors: vec![],
                    accessor_spans: vec![],
                    base_span: Spanned::default(),
                    span: Spanned::default(),
                }))),
                Spanned::default(),
            )
        }),
    ))(input)?;
    // Attach the `@` span to the spawn, for hover (shows the process type).
    let term = match term {
        // Stamp just the `@`, not the spawned function body.
        Term::Spawn(inner, _) => Term::Spawn(inner, Spanned(Some(token_span(start, 1)))),
        other => other,
    };
    Ok((rest, term))
}

fn self_term(input: Span) -> IResult<Span, Term> {
    // Match '.' NOT followed by a lowercase letter or digit
    // This avoids conflicting with member access (.field or .0)
    map(
        terminated(
            char('.'),
            peek(not(alt((
                recognize(satisfy(|c: char| c.is_ascii_lowercase())),
                recognize(satisfy(|c: char| c.is_ascii_digit())),
            )))),
        ),
        |_| Term::Self_,
    )(input)
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
    alt((
        map(multiline_string, |s| str_match(s.into_bytes())),
        map(single_line_string, |s| str_match(s.into_bytes())),
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

/// Parse a type-ascribed binding: a *parenthesised type* immediately followed by a binding
/// identifier, e.g. `('int)x` or `('int | 'bin)x`. Asserts the value's type and binds the whole
/// (narrowed) value to the trailing identifier. The identifier must be *adjacent* — no whitespace
/// after `)` — so `('int) x` is not an as-pattern (the `x` is left for the next term). The leading
/// `(` is required, so a bare type (`'int`, `A['int]`) is never silently turned into a binder.
fn as_pattern(input: Span) -> IResult<Span, Match> {
    // Require a parenthesised type. `peek('(')` keeps a bare type like `A['int]` from being read
    // as `('A['int]')` + binder; and lets a non-type `(x)` fall through to the partial-pattern rule.
    peek(char('('))(input)?;
    let (input, ty) = inline_type_expression(input)?;
    let (input, (span, name)) = spanned(identifier)(input)?;
    Ok((input, Match::As(ty, name, Spanned(Some(span)))))
}

fn match_pattern(input: Span) -> IResult<Span, Match> {
    alt((
        // Variable pin with & prefix: &name checks against an existing variable's value.
        map(preceded(char('&'), spanned(identifier)), |(span, name)| {
            Match::Reference(name, Spanned(Some(span)))
        }),
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
        // Type reference: 'int, 'list<'t>, ('int | 'bin). Types need no & since they
        // are never bound. Must come after partial patterns to avoid ambiguity with (...).
        map(inline_type_expression, Match::Type),
        // Alternation of (structural) patterns: `([[], _] | [_, []])`. After the type form so a
        // pure type union stays a single `Match::Type`.
        map(or_pattern, Match::Or),
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

/// Reference term: &identifier, &module.func, or &. — an explicit reference, without calling.
fn reference_term(input: Span) -> IResult<Span, Term> {
    preceded(
        char('&'),
        alt((
            // &. - reference to self
            map(char('.'), |_| {
                Term::Reference(Access {
                    source: Some(AccessSource::Self_),
                    accessors: vec![],
                    accessor_spans: vec![],
                    base_span: Spanned::default(),
                    span: Spanned::default(),
                })
            }),
            // &identifier, &module.func, or &__builtin__ (a builtin is an access source)
            map(access, Term::Reference),
        )),
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
        // Reference (must come before access to parse &f before f)
        reference_term,
        // Name-preserving spread-update (`~[..., y]`, `a[..., y]`) — before access, which would
        // otherwise consume the `~`/identifier as a bare reference.
        spread_update,
        // Access (field/positional access, bare identifiers, and imports)
        map(access, Term::Access),
        // Operations
        equality,
        not_term,
        select_term,
        tail_call,
    ))(input)
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
            |((bind_span, match_pattern), terms)| Chain {
                match_pattern: Some(match_pattern),
                bind_span: Spanned(Some(bind_span)),
                span: Spanned::default(),
                terms,
            },
        ),
        // Plain chain
        map(chain_inner, |terms| Chain {
            match_pattern: None,
            bind_span: Spanned::default(),
            span: Spanned::default(),
            terms,
        }),
    ))(input)?;
    // Record the chain's start offset, for attaching leading trivia during formatting.
    chain.span = Spanned(Some(span_between(start, rest)));
    Ok((rest, chain))
}

fn chain_inner(input: Span) -> IResult<Span, Vec<Term>> {
    // A chain is a sequence of `primary` terms; the value flows left→right through them, with nil
    // passing through (no short-circuit — that is the sequence separator's job). Terms are joined
    // by horizontal whitespace (`a b c`), or, equivalently, by an optional `~>` — which doubles as
    // an explicit **line continuation**: a chain ends at a bare newline, but a newline followed by
    // `~>` continues it, so a long chain can span lines:
    //   foo
    //   ~> bar
    //   ~> baz
    // (`~>` is just the separator written explicitly; `a ~> b` and `a b` are identical.)
    //
    // Application is argument-first (`[args] f`); there is no juxtaposition. The bare ripple terms
    // `~`, `^~`, `@~`, `~.f` are primaries in their own right and take no juxtaposed argument
    // (`^~`/`@~` hand the flowing function a nil argument, so it must be nilary; to pass an
    // argument, bind the function first and name it).
    separated_list1(
        alt((nom_value((), tuple((ws1, tag("~>"), ws1))), hspace1)),
        primary,
    )(input)
}

/// Separator between the chains of a sequence: a comma or a newline (they are synonyms), with
/// surrounding horizontal whitespace and line comments, collapsing runs of them. Unlike the
/// chain-step separator (horizontal whitespace), this is where the value short-circuits on nil and
/// a new binding scope point begins. A newline therefore ends a chain and starts the next step.
fn seq_sep(input: Span) -> IResult<Span, ()> {
    nom_value(
        (),
        tuple((
            // Leading horizontal whitespace / line comments (a newline here is the separator).
            many0(alt((nom_value((), space1), nom_value((), comment)))),
            // The separator itself: a comma or a newline.
            alt((nom_value((), char(',')), nom_value((), line_ending))),
            // Collapse any following whitespace, comments, and further separators.
            many0(alt((
                nom_value((), multispace1),
                nom_value((), comment),
                nom_value((), char(',')),
            ))),
        )),
    )(input)
}

fn sequence(input: Span) -> IResult<Span, Sequence> {
    map(
        terminated(separated_list1(seq_sep, chain), opt(seq_sep)),
        |chains| Sequence { chains },
    )(input)
}

// Statement parsers

fn type_alias(input: Span) -> IResult<Span, Statement> {
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
        |((name_span, name), type_parameters, type_definition)| Statement::TypeAlias {
            name,
            name_span: Spanned(Some(name_span)),
            type_parameters: type_parameters.unwrap_or_default(),
            type_definition,
        },
    )(input)
}

/// A top-level item: a type alias or a value-producing sequence. A statement-level expression is a
/// branchless sequence (branches `|`/`=>` require a block).
fn top_level_item(input: Span) -> IResult<Span, Statement> {
    alt((type_alias, map(sequence, Statement::Expression)))(input)
}

/// A program is a single threaded sequence of chains with type-alias declarations interspersed —
/// all separated by the sequence separator (comma or newline, which are synonyms). The chains
/// thread and short-circuit as one sequence; type aliases are transparent to that flow. (There is
/// no separate statement separator and no `;`.)
fn program(input: Span) -> IResult<Span, Program> {
    map(
        delimited(
            ws_with_comments,
            terminated(separated_list0(seq_sep, top_level_item), opt(seq_sep)),
            pair(ws_with_comments, nom::combinator::eof),
        ),
        |statements| Program { statements },
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
        let source = "#{ { let x = 1 ";
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
        let source = "#{ [1, 2] __integer_add__ }";
        let result = parse(source);
        assert!(result.is_ok());
    }

    /// The slice of `source` covered by a span.
    fn slice(source: &str, span: SourceSpan) -> &str {
        &source[span.offset..span.offset + span.length]
    }

    #[test]
    fn parse_populates_access_spans() {
        let source = "point double";
        let program = parse(source).unwrap();
        let Statement::Expression(expr) = &program.statements[0] else {
            panic!("expected expression statement");
        };
        let terms = &expr.chains[0].terms;
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
    fn parse_populates_binding_span() {
        let source = "total = 5";
        let program = parse(source).unwrap();
        let Statement::Expression(expr) = &program.statements[0] else {
            panic!("expected expression statement");
        };
        let span = expr.chains[0].bind_span.get().unwrap();
        assert_eq!(slice(source, span), "total");
    }

    #[test]
    fn parse_populates_type_alias_name_span() {
        let source = "'point = Point[x: 'int, y: 'int]";
        let program = parse(source).unwrap();
        let Statement::TypeAlias { name_span, .. } = &program.statements[0] else {
            panic!("expected type alias statement");
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
        let source = "#{ 0x999999999999999999999 }";
        let result = parse(source);
        assert!(result.is_err());
        let err = result.unwrap_err();
        // Verify we still capture span information even if error kind is generic
        assert!(err.span.is_some());
        let span = err.span.unwrap();
        assert_eq!(span.line, 1);
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
}
