use crate::ast::*;
use nom::{
    IResult, Slice,
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, digit1, multispace0, multispace1, satisfy},
    combinator::{map, map_res, not, opt, peek, recognize, value as nom_value, verify},
    multi::{many0, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
};
use nom_locate::LocatedSpan;

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

            ErrorKind::ExpectedPipe => write!(f, "Expected '~>' in chain"),
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

fn ws0(input: Span) -> IResult<Span, ()> {
    nom_value((), multispace0)(input)
}

fn ws1(input: Span) -> IResult<Span, ()> {
    nom_value((), multispace1)(input)
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
            take_while(|c: char| c == '\''),
        ))),
        |s: Span| s.fragment().to_string(),
    )(input)
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
            alt((
                // Hexadecimal: 0x...
                map_res(
                    preceded(tag("0x"), take_while1(|c: char| c.is_ascii_hexdigit())),
                    |s: Span| {
                        i64::from_str_radix(s.fragment(), 16).map_err(|_| {
                            Error::new(
                                ErrorKind::IntegerMalformed(format!("0x{}", s.fragment())),
                                Some(SourceSpan::from_span(s)),
                            )
                        })
                    },
                ),
                // Binary: 0b...
                map_res(
                    preceded(tag("0b"), take_while1(|c: char| c == '0' || c == '1')),
                    |s: Span| {
                        i64::from_str_radix(s.fragment(), 2).map_err(|_| {
                            Error::new(
                                ErrorKind::IntegerMalformed(format!("0b{}", s.fragment())),
                                Some(SourceSpan::from_span(s)),
                            )
                        })
                    },
                ),
                // Decimal
                map_res(digit1, |s: Span| {
                    s.fragment().parse::<i64>().map_err(|_| {
                        Error::new(
                            ErrorKind::IntegerMalformed(s.fragment().to_string()),
                            Some(SourceSpan::from_span(s)),
                        )
                    })
                }),
            )),
        ),
        |(sign, value)| Literal::Integer(if sign.is_some() { -value } else { value }),
    )(input)
}

fn binary_literal(input: Span) -> IResult<Span, Literal> {
    map_res(
        delimited(
            char('\''),
            take_while(|c: char| c.is_ascii_hexdigit()),
            char('\''),
        ),
        |s: Span| {
            hex::decode(s.fragment()).map(Literal::Binary).map_err(|_| {
                Error::new(
                    ErrorKind::HexMalformed(s.fragment().to_string()),
                    Some(SourceSpan::from_span(s)),
                )
            })
        },
    )(input)
}

fn string_term(input: Span) -> IResult<Span, Term> {
    map(
        delimited(
            char('"'),
            map_res(take_while(|c| c != '"'), |s: Span| parse_string_content(s)),
            char('"'),
        ),
        |s: String| {
            // Convert string to binary bytes
            let bytes = s.into_bytes();
            // Create a Str tuple with the binary as its single field
            Term::Tuple(Tuple {
                name: TupleName::Literal("Str".to_string()),
                fields: vec![TupleField {
                    name: None,
                    value: FieldValue::Chain(Chain {
                        match_pattern: None,
                        terms: vec![Term::Literal(Literal::Binary(bytes))],
                        continuation: false,
                    }),
                }],
            })
        },
    )(input)
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

fn literal(input: Span) -> IResult<Span, Literal> {
    alt((integer_literal, binary_literal))(input)
}

// Pattern parsers for terms

fn partial_pattern_inner(input: Span) -> IResult<Span, PartialPattern> {
    alt((
        // Named partial pattern: TupleName(field1, field2, ...)
        map(
            tuple((
                tuple_name,
                delimited(
                    pair(char('('), ws0),
                    terminated(
                        separated_list1(tuple((ws0, char(','), ws1)), identifier),
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
                    separated_list1(tuple((ws0, char(','), ws1)), identifier),
                    opt(pair(ws0, char(','))),
                ),
                pair(ws0, char(')')),
            ),
            |fields| PartialPattern { name: None, fields },
        ),
    ))(input)
}

// Type parsers

fn primitive_type(input: Span) -> IResult<Span, Type> {
    alt((
        nom_value(Type::Primitive(PrimitiveType::Int), tag("int")),
        nom_value(Type::Primitive(PrimitiveType::Bin), tag("bin")),
    ))(input)
}

fn field_type(input: Span) -> IResult<Span, FieldType> {
    alt((
        // Spread with optional identifier and optional type arguments: ... or ...identifier or ...identifier<type, type>
        map(
            preceded(
                tag("..."),
                opt(pair(
                    identifier,
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
                    name: TupleName::Literal(name),
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
                        name: TupleName::None,
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
                    name: TupleName::Literal(name),
                    fields,
                    is_partial: false,
                },
            ),
            // identifier[...] - inherit name from type alias and auto-spread
            verify(
                map(
                    tuple((
                        identifier,
                        delimited(pair(char('['), wsc), field_type_list, pair(wsc, char(']'))),
                    )),
                    |(id, mut fields)| {
                        // Transform unspecified spread (...) into identifier spread (...identifier)
                        // This allows event[..., timestamp: int] to mean "spread event and add timestamp"
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
                            name: TupleName::Identifier(id),
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
                    name: TupleName::None,
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
                    name: TupleName::Literal(name),
                    fields: vec![],
                    is_partial: false,
                },
            ),
        )),
        Type::Tuple,
    )(input)
}

fn type_parameter(input: Span) -> IResult<Span, Type> {
    map(delimited(char('<'), identifier, char('>')), |name| {
        Type::Identifier {
            name,
            arguments: vec![],
        }
    })(input)
}

fn type_identifier(input: Span) -> IResult<Span, Type> {
    map(
        pair(
            identifier,
            opt(delimited(
                char('<'),
                separated_list1(tuple((ws0, char(','), ws0)), type_definition),
                char('>'),
            )),
        ),
        |(name, arguments)| Type::Identifier {
            name,
            arguments: arguments.unwrap_or_default(),
        },
    )(input)
}

fn type_cycle(input: Span) -> IResult<Span, Type> {
    map(
        preceded(
            char('&'),
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
        primitive_type,
        process_type,
        type_identifier,
    ))(input)
}

fn function_output_type(input: Span) -> IResult<Span, Type> {
    alt((
        partial_type, // Must come before grouping parentheses
        delimited(pair(char('('), ws0), type_definition, pair(ws0, char(')'))),
        tuple_type,
        primitive_type,
        process_type,
        type_identifier,
    ))(input)
}

fn base_type(input: Span) -> IResult<Span, Type> {
    alt((
        tuple_type,
        partial_type, // Must come before grouping parentheses to have priority
        primitive_type,
        type_cycle,
        process_type,
        type_parameter, // Must come before type_identifier to match <t> before trying identifier
        delimited(pair(char('('), ws0), type_definition, pair(ws0, char(')'))),
        type_identifier,
    ))(input)
}

fn type_definition(input: Span) -> IResult<Span, Type> {
    alt((
        function_type,
        map(
            tuple((
                // Optional leading | for multi-line union types
                opt(tuple((wsc, char('|'), wsc))),
                // First type
                base_type,
                // Remaining types separated by |
                many0(preceded(tuple((wsc, char('|'), wsc)), base_type)),
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

// Parse access patterns
fn access(input: Span) -> IResult<Span, Access> {
    verify(
        map(
            tuple((
                opt(identifier),
                many0(preceded(
                    char('.'),
                    alt((
                        map(digit1, |s: Span| AccessPath::Index(s.parse().unwrap())),
                        map(identifier, AccessPath::Field),
                    )),
                )),
                opt(preceded(
                    peek(char('[')),
                    delimited(pair(char('['), wsc), tuple_field_list, pair(wsc, char(']'))),
                )),
            )),
            |(identifier, accessors, argument)| Access {
                identifier,
                accessors,
                argument: argument.map(|fields| Tuple {
                    name: TupleName::None,
                    fields,
                }),
            },
        ),
        |ma| ma.identifier.is_some() || !ma.accessors.is_empty(),
    )(input)
}

// Parse select operator with four forms:
// 1. !identifier - Identifier (needs semantic resolution)
// 2. !identifier { ... } - Function (with identifier type)
// 3. !(type) or !(type) { ... } - Type or Function (with explicit type)
// 4. !(source1, source2, ...) - Sources (comma-separated)
// 5. !<term> - Sources with single term (e.g., !#int, !@process)
fn select_term(input: Span) -> IResult<Span, Term> {
    alt((
        // !identifier { ... } - Function with identifier type
        map(
            tuple((preceded(char('!'), identifier), preceded(opt(ws1), block))),
            |(ident, body)| {
                // Check if identifier is a primitive type
                let param_type = match ident.as_str() {
                    "int" => Type::Primitive(PrimitiveType::Int),
                    "bin" => Type::Primitive(PrimitiveType::Bin),
                    _ => Type::Identifier {
                        name: ident,
                        arguments: vec![],
                    },
                };
                Term::Select(Select::Function(Function {
                    type_parameters: vec![],
                    parameter_type: Some(param_type),
                    body: Some(body),
                }))
            },
        ),
        // !(type) { ... } - Function with explicit type
        // Note: If type is just an identifier, still treat as Function (not Identifier with block)
        // because the block indicates it's definitely a receive function
        map(
            tuple((
                delimited(
                    pair(char('!'), pair(char('('), wsc)),
                    type_definition,
                    pair(wsc, char(')')),
                ),
                preceded(opt(ws1), block),
            )),
            |(param_type, body)| {
                Term::Select(Select::Function(Function {
                    type_parameters: vec![],
                    parameter_type: Some(param_type),
                    body: Some(body),
                }))
            },
        ),
        // ![...] or ![..., optional] - Tuple type without body
        map(preceded(char('!'), tuple_type), |tuple_ty| {
            Term::Select(Select::Type(tuple_ty))
        }),
        // !(type) - Type without body
        // Note: !(identifier) is treated as Identifier, not Type, to allow semantic resolution
        map(
            delimited(
                pair(char('!'), pair(char('('), wsc)),
                type_definition,
                pair(wsc, char(')')),
            ),
            |param_type| {
                // If the type is just a bare identifier, treat it as Identifier for semantic resolution
                if let Type::Identifier {
                    ref name,
                    ref arguments,
                } = param_type
                    && arguments.is_empty()
                {
                    return Term::Select(Select::Identifier(name.clone()));
                }
                Term::Select(Select::Type(param_type))
            },
        ),
        // !(source1, source2, ...) - Sources (comma-separated chains)
        map(
            delimited(
                pair(char('!'), pair(char('('), wsc)),
                separated_list1(tuple((wsc, char(','), wsc)), chain),
                pair(wsc, char(')')),
            ),
            |sources| Term::Select(Select::Sources(sources)),
        ),
        // !<access> - Sources with accessor OR identifier (e.g., !math.add, !p1, !receiver_func)
        // access parser matches both bare identifiers and identifiers with accessors
        map(preceded(char('!'), access), |acc| {
            // Check if it's a bare identifier without accessors - use Identifier variant
            if acc.accessors.is_empty()
                && acc.argument.is_none()
                && let Some(ref id) = acc.identifier
            {
                return Term::Select(Select::Identifier(id.clone()));
            }
            // Otherwise, it's a Sources with the access term
            Term::Select(Select::Sources(vec![Chain {
                match_pattern: None,
                terms: vec![Term::Access(acc)],
                continuation: false,
            }]))
        }),
        // !<non-select-term> - Sources with single term (e.g., !#int, !@process)
        map(
            preceded(
                char('!'),
                alt((
                    // Parse non-select terms to avoid infinite recursion
                    map(function, Term::Function),
                    map(spawn_term, |t| t), // spawn_term returns Term directly
                    map(literal, Term::Literal),
                )),
            ),
            |t| {
                Term::Select(Select::Sources(vec![Chain {
                    match_pattern: None,
                    terms: vec![t],
                    continuation: false,
                }]))
            },
        ),
        // Bare ! - becomes Sources with implicit ripple
        map(char('!'), |_| {
            Term::Select(Select::Sources(vec![Chain {
                match_pattern: None,
                terms: vec![Term::Ripple],
                continuation: false,
            }]))
        }),
    ))(input)
}

fn import(input: Span) -> IResult<Span, String> {
    preceded(
        char('%'),
        delimited(
            char('"'),
            map_res(take_while(|c| c != '"'), |s: Span| parse_string_content(s)),
            char('"'),
        ),
    )(input)
}

fn tuple_field(input: Span) -> IResult<Span, TupleField> {
    alt((
        // Named field with chain: name: chain
        map(
            separated_pair(identifier, tuple((char(':'), ws1)), chain),
            |(name, chain_value)| TupleField {
                name: Some(name),
                value: FieldValue::Chain(chain_value),
            },
        ),
        // Spread with identifier: ...identifier
        map(preceded(tag("..."), identifier), |id| TupleField {
            name: None,
            value: FieldValue::Spread(SpreadSource::Identifier(id)),
        }),
        // Spread chained value: ...
        map(tag("..."), |_| TupleField {
            name: None,
            value: FieldValue::Spread(SpreadSource::Chained),
        }),
        // Unnamed chain: chain
        map(chain, |chain_value| TupleField {
            name: None,
            value: FieldValue::Chain(chain_value),
        }),
    ))(input)
}

fn tuple_field_list(input: Span) -> IResult<Span, Vec<TupleField>> {
    terminated(
        separated_list0(tuple((wsc, char(','), wsc)), tuple_field),
        opt(pair(wsc, char(','))),
    )(input)
}

fn tuple_term(input: Span) -> IResult<Span, Tuple> {
    alt((
        // TupleName[...] - uppercase named tuple with fields
        map(
            tuple((
                tuple_name,
                delimited(pair(char('['), wsc), tuple_field_list, pair(wsc, char(']'))),
            )),
            |(name, fields)| Tuple {
                name: TupleName::Literal(name),
                fields,
            },
        ),
        // identifier[...] with spread - lowercase identifier (preserves name)
        // This must come before unnamed tuple to catch lowercase names with spreads
        map(
            verify(
                map(
                    tuple((
                        identifier,
                        delimited(pair(char('['), wsc), tuple_field_list, pair(wsc, char(']'))),
                    )),
                    |(name, mut fields)| {
                        // Transform chained spread (...) into identifier spread (...identifier)
                        // This allows a[..., y: 2] to mean "spread a and add y"
                        for field in &mut fields {
                            if matches!(field.value, FieldValue::Spread(SpreadSource::Chained)) {
                                field.value =
                                    FieldValue::Spread(SpreadSource::Identifier(name.clone()));
                            }
                        }
                        (name, fields)
                    },
                ),
                |(_, fields)| {
                    fields
                        .iter()
                        .any(|f| matches!(f.value, FieldValue::Spread(_)))
                },
            ),
            |(name, fields)| Tuple {
                name: TupleName::Identifier(name),
                fields,
            },
        ),
        // ~[...] with spread - ripple with spread (preserves rippled name)
        map(
            verify(
                map(
                    preceded(
                        char('~'),
                        delimited(pair(char('['), wsc), tuple_field_list, pair(wsc, char(']'))),
                    ),
                    |mut fields| {
                        // Transform chained spread (...) to reference the ripple
                        for field in &mut fields {
                            if matches!(field.value, FieldValue::Spread(SpreadSource::Chained)) {
                                field.value = FieldValue::Spread(SpreadSource::Ripple);
                            }
                        }
                        fields
                    },
                ),
                |fields: &Vec<TupleField>| {
                    fields
                        .iter()
                        .any(|f| matches!(f.value, FieldValue::Spread(_)))
                },
            ),
            |fields| Tuple {
                name: TupleName::Ripple,
                fields,
            },
        ),
        // [...] - unnamed tuple with fields
        map(
            delimited(pair(char('['), wsc), tuple_field_list, pair(wsc, char(']'))),
            |fields| Tuple {
                name: TupleName::None,
                fields,
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
                name: TupleName::Literal(name),
                fields: vec![],
            },
        ),
    ))(input)
}

fn branch(input: Span) -> IResult<Span, Branch> {
    map(
        pair(
            expression,
            opt(preceded(tuple((ws1, tag("=>"), ws1)), expression)),
        ),
        |(condition, consequence)| Branch {
            condition,
            consequence,
        },
    )(input)
}

fn block(input: Span) -> IResult<Span, Block> {
    map(
        delimited(
            pair(char('{'), wsc),
            preceded(
                opt(pair(char('|'), wsc)),
                separated_list1(tuple((wsc, char('|'), wsc)), branch),
            ),
            pair(wsc, char('}')),
        ),
        |branches| Block { branches },
    )(input)
}

fn function(input: Span) -> IResult<Span, Function> {
    map(
        preceded(
            char('#'),
            tuple((
                opt(delimited(
                    char('<'),
                    separated_list1(tuple((ws0, char(','), ws0)), identifier),
                    char('>'),
                )),
                opt(preceded(not(peek(char('{'))), function_input_type)),
                opt(alt((preceded(ws1, block), block))),
            )),
        ),
        |(type_parameters, parameter_type, body)| Function {
            type_parameters: type_parameters.unwrap_or_default(),
            parameter_type,
            body,
        },
    )(input)
}

fn tail_call(input: Span) -> IResult<Span, Term> {
    let (input, _) = char('&')(input)?;
    let (input, ident) = opt(identifier)(input)?;
    let (input, accessors) = many0(preceded(
        char('.'),
        alt((
            map(digit1, |s: Span| AccessPath::Index(s.parse().unwrap())),
            map(identifier, AccessPath::Field),
        )),
    ))(input)?;
    let (input, argument) = opt(preceded(
        peek(char('[')),
        delimited(pair(char('['), wsc), tuple_field_list, pair(wsc, char(']'))),
    ))(input)?;
    Ok((
        input,
        Term::TailCall(TailCall {
            identifier: ident,
            accessors,
            argument: argument.map(|fields| Tuple {
                name: TupleName::None,
                fields,
            }),
        }),
    ))
}

fn builtin(input: Span) -> IResult<Span, Builtin> {
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

    // Parse optional argument [...] (same as access)
    let (input, argument) = opt(preceded(
        peek(char('[')),
        delimited(pair(char('['), wsc), tuple_field_list, pair(wsc, char(']'))),
    ))(input)?;

    Ok((
        input,
        Builtin {
            name: trimmed.to_string(),
            argument: argument.map(|fields| Tuple {
                name: TupleName::None,
                fields,
            }),
        },
    ))
}

fn equality(input: Span) -> IResult<Span, Term> {
    nom_value(Term::Equality, tag("=="))(input)
}

fn not_term(input: Span) -> IResult<Span, Term> {
    nom_value(Term::Not, char('/'))(input)
}

fn spawn_term(input: Span) -> IResult<Span, Term> {
    alt((
        // @{ ... } - Spawn parameterless function (sugar for @#{ ... })
        map(preceded(pair(char('@'), opt(ws1)), block), |body| {
            Term::Spawn(Box::new(Term::Function(Function {
                type_parameters: vec![],
                parameter_type: None,
                body: Some(body),
            })))
        }),
        // @identifier { ... } - Spawn with identifier type (sugar for @#identifier { ... })
        map(
            tuple((preceded(char('@'), identifier), preceded(opt(ws1), block))),
            |(ident, body)| {
                // Check if identifier is a primitive type
                let param_type = match ident.as_str() {
                    "int" => Type::Primitive(PrimitiveType::Int),
                    "bin" => Type::Primitive(PrimitiveType::Bin),
                    _ => Type::Identifier {
                        name: ident,
                        arguments: vec![],
                    },
                };
                Term::Spawn(Box::new(Term::Function(Function {
                    type_parameters: vec![],
                    parameter_type: Some(param_type),
                    body: Some(body),
                })))
            },
        ),
        // @[...] - Spawn with tuple type (sugar for @#[...] { ... })
        map(
            tuple((preceded(char('@'), tuple_type), preceded(opt(ws1), block))),
            |(tuple_ty, body)| {
                Term::Spawn(Box::new(Term::Function(Function {
                    type_parameters: vec![],
                    parameter_type: Some(tuple_ty),
                    body: Some(body),
                })))
            },
        ),
        // @<term> - Match @ followed by optional term (bare @ becomes @~)
        map(preceded(char('@'), opt(term)), |opt_t| {
            Term::Spawn(Box::new(opt_t.unwrap_or(Term::Ripple)))
        }),
    ))(input)
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
    map(preceded(char('='), match_pattern), Term::BindMatch)(input)
}

fn pin_match(input: Span) -> IResult<Span, Term> {
    map(preceded(char('^'), match_pattern), Term::PinMatch)(input)
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
    map(
        delimited(
            char('"'),
            map_res(take_while(|c| c != '"'), |s: Span| parse_string_content(s)),
            char('"'),
        ),
        |s: String| {
            // Create Str[binary] tuple pattern
            Match::Tuple(MatchTuple {
                name: Some("Str".to_string()),
                fields: vec![MatchField {
                    name: None,
                    pattern: Match::Literal(Literal::Binary(s.into_bytes())),
                }],
            })
        },
    )(input)
}

// Parse an inline type expression: either (type-expr) or identifier<type-args>
// This is used for pin patterns with explicit types
fn inline_type_expression(input: Span) -> IResult<Span, Type> {
    alt((
        // Parenthesized type expression: (int | bin), (list<int>), etc.
        delimited(pair(char('('), wsc), type_definition, pair(wsc, char(')'))),
        // Type identifier with required type arguments: list<int>, tree<int, bin>
        map(
            pair(
                identifier,
                delimited(
                    char('<'),
                    separated_list1(tuple((ws0, char(','), ws0)), type_definition),
                    char('>'),
                ),
            ),
            |(name, arguments)| Type::Identifier { name, arguments },
        ),
        // Partial types: A(x: int), (x: int), ()
        partial_type,
    ))(input)
}

fn match_pattern(input: Span) -> IResult<Span, Match> {
    alt((
        // Inline type with ^ prefix: ^(type-expression) or ^list<int>
        // The ^ switches to pin mode, wrapping the type in Pin
        map(preceded(char('^'), inline_type_expression), |type_def| {
            Match::Pin(Box::new(Match::Type(type_def)))
        }),
        // Pin pattern (must come after inline type to allow recursive patterns)
        map(preceded(char('^'), match_pattern), |inner| {
            Match::Pin(Box::new(inner))
        }),
        // Bind pattern (explicitly switches to bind mode)
        map(preceded(char('='), match_pattern), |inner| {
            Match::Bind(Box::new(inner))
        }),
        // Try string literals first (before tuples and literals)
        match_string,
        // Try match tuple (handles both [..] and Name[..])
        map(match_tuple, Match::Tuple),
        // Try partial patterns before inline types (partial patterns use parentheses too)
        map(partial_pattern_inner, Match::Partial),
        // Inline type without ^ prefix: (type-expression) or list<int>
        // For pin_match contexts where ^ is already consumed: 42 ~> ^(int | bin) or 42 ~> ^list<int>
        // Must come after partial patterns to avoid ambiguity with (identifier)
        map(inline_type_expression, Match::Type),
        // Then try literals
        map(literal, Match::Literal),
        // Star and placeholder
        map(char('*'), |_| Match::Star),
        map(char('_'), |_| Match::Placeholder),
        // Identifier must come last (since it's more general)
        map(identifier, Match::Identifier),
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

fn term(input: Span) -> IResult<Span, Term> {
    alt((
        // String terms (before literals to handle quotes)
        string_term,
        // Ripple placeholder (but not ~[...] which is tuple with ripple spread)
        map(terminated(char('~'), peek(not(char('[')))), |_| {
            Term::Ripple
        }),
        // Process operations (process_ref_term must come before spawn_term to match @N first)
        process_ref_term,
        spawn_term,
        self_term,
        // Pin match and bind match (must be before literals and identifiers)
        pin_match,
        bind_match,
        // Literals
        map(literal, Term::Literal),
        // Complex terms
        map(tuple_term, Term::Tuple),
        map(function, Term::Function),
        map(block, Term::Block),
        // Import
        map(import, Term::Import),
        // Builtins
        map(builtin, Term::Builtin),
        // Access (includes field/positional access and bare identifiers with optional argument)
        map(access, Term::Access),
        // Operations
        equality,
        not_term,
        select_term,
        tail_call,
    ))(input)
}

fn chain(input: Span) -> IResult<Span, Chain> {
    alt((
        // Match pattern: pattern = chain_inner
        map(
            pair(
                terminated(match_pattern, tuple((ws1, char('='), ws1))),
                chain_inner,
            ),
            |(match_pattern, (terms, continuation))| Chain {
                match_pattern: Some(match_pattern),
                terms,
                continuation,
            },
        ),
        // Plain chain
        map(chain_inner, |(terms, continuation)| Chain {
            match_pattern: None,
            terms,
            continuation,
        }),
    ))(input)
}

fn chain_inner(input: Span) -> IResult<Span, (Vec<Term>, bool)> {
    alt((
        // Continuation chain: starts with ~>, terms are optional
        map(
            pair(
                tag("~>"),
                opt(preceded(
                    ws1,
                    separated_list1(tuple((ws1, tag("~>"), ws1)), term),
                )),
            ),
            |(_, terms)| (terms.unwrap_or_default(), true),
        ),
        // Normal chain: must have at least one term
        map(
            separated_list1(tuple((ws1, tag("~>"), ws1)), term),
            |terms| (terms, false),
        ),
    ))(input)
}

fn expression(input: Span) -> IResult<Span, Expression> {
    map(
        terminated(
            separated_list1(tuple((ws0, char(','), wsc)), chain),
            opt(pair(ws0, char(','))),
        ),
        |chains| Expression { chains },
    )(input)
}

// Statement parsers

fn type_alias(input: Span) -> IResult<Span, Statement> {
    map(
        tuple((
            identifier,
            opt(delimited(
                char('<'),
                separated_list1(tuple((ws0, char(','), ws0)), identifier),
                char('>'),
            )),
            preceded(tuple((ws0, tag(":"), ws0)), type_definition),
        )),
        |(name, type_parameters, type_definition)| Statement::TypeAlias {
            name,
            type_parameters: type_parameters.unwrap_or_default(),
            type_definition,
        },
    )(input)
}

fn type_import_pattern(input: Span) -> IResult<Span, TypeImportPattern> {
    alt((
        nom_value(TypeImportPattern::Star, char('*')),
        map(
            delimited(
                pair(char('('), ws0),
                terminated(
                    separated_list1(tuple((ws0, char(','), ws1)), identifier),
                    opt(pair(ws0, char(','))),
                ),
                pair(ws0, char(')')),
            ),
            TypeImportPattern::Partial,
        ),
    ))(input)
}

fn type_import(input: Span) -> IResult<Span, Statement> {
    map(
        tuple((
            type_import_pattern,
            preceded(tuple((ws0, tag(":"), ws0)), import),
        )),
        |(pattern, module_path)| Statement::TypeImport {
            pattern,
            module_path,
        },
    )(input)
}

fn statement_expression(input: Span) -> IResult<Span, Statement> {
    map(expression, Statement::Expression)(input)
}

fn statement(input: Span) -> IResult<Span, Statement> {
    preceded(
        ws_with_comments,
        alt((type_import, type_alias, statement_expression)),
    )(input)
}

fn statements(input: Span) -> IResult<Span, Vec<Statement>> {
    terminated(
        separated_list0(
            alt((
                nom_value((), tuple((ws0, alt((char('\n'), char(';'))), ws0))),
                nom_value((), ws1),
            )),
            statement,
        ),
        opt(pair(ws0, char(';'))),
    )(input)
}

fn program(input: Span) -> IResult<Span, Program> {
    map(
        delimited(
            ws_with_comments,
            statements,
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
        let source = "#{ [1, 2] ~> __add__ }";
        let result = parse(source);
        assert!(result.is_ok());
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
    fn test_span_malformed_integer() {
        // Note: Currently malformed integers produce UnexpectedEndOfInput errors
        // because nom converts our custom errors. This could be improved in future.
        let source = "#{ 99999999999999999999 }";
        let result = parse(source);
        assert!(result.is_err());
        let err = result.unwrap_err();
        // Verify we still capture span information even if error kind is generic
        assert!(err.span.is_some());
        let span = err.span.unwrap();
        assert_eq!(span.line, 1);
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
    fn test_span_process_ref_malformed() {
        // Note: Custom errors are converted by nom
        let source = "#{ @99999999999999999999 }";
        let result = parse(source);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.span.is_some());
        let span = err.span.unwrap();
        assert_eq!(span.line, 1);
    }
}
