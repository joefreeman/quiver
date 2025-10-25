use crate::ast::*;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, digit1, multispace0, multispace1, satisfy},
    combinator::{map, map_res, not, opt, peek, recognize, value as nom_value, verify},
    multi::{many0, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    // Literal parsing errors
    IntegerMalformed(String),
    HexMalformed(String),
    StringEscapeInvalid(String),
    IndexMalformed(String),

    // Language construct errors
    ParameterInvalid(String),
    OperatorUnknown(String),

    // Parser errors
    ParseError(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::IntegerMalformed(lit) => write!(f, "Malformed integer: {}", lit),
            Error::HexMalformed(lit) => write!(f, "Malformed hex literal: {}", lit),
            Error::StringEscapeInvalid(esc) => write!(f, "Invalid string escape: {}", esc),
            Error::IndexMalformed(idx) => write!(f, "Malformed index: {}", idx),
            Error::ParameterInvalid(param) => write!(f, "Invalid parameter: {}", param),
            Error::OperatorUnknown(op) => write!(f, "Unknown operator: {}", op),
            Error::ParseError(msg) => write!(f, "Parse error: {}", msg),
        }
    }
}

impl std::error::Error for Error {}

pub fn parse(source: &str) -> Result<Program, Error> {
    match program(source) {
        Ok((_, prog)) => Ok(prog),
        Err(e) => Err(Error::ParseError(format!("{:?}", e))),
    }
}

// Utility parsers

fn ws0(input: &str) -> IResult<&str, ()> {
    nom_value((), multispace0)(input)
}

fn ws1(input: &str) -> IResult<&str, ()> {
    nom_value((), multispace1)(input)
}

fn comment(input: &str) -> IResult<&str, &str> {
    preceded(tag("//"), take_while(|c| c != '\n' && c != '\r'))(input)
}

fn ws_with_comments(input: &str) -> IResult<&str, ()> {
    nom_value(
        (),
        many0(alt((nom_value((), multispace1), nom_value((), comment)))),
    )(input)
}

// Whitespace and/or comments (including inline comments after commas)
fn wsc(input: &str) -> IResult<&str, ()> {
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

fn identifier(input: &str) -> IResult<&str, String> {
    map(
        recognize(tuple((
            satisfy(|c: char| c.is_ascii_lowercase()),
            take_while(|c: char| c.is_ascii_alphanumeric() || c == '_'),
            opt(char('?')),
            opt(char('!')),
            take_while(|c: char| c == '\''),
        ))),
        String::from,
    )(input)
}

fn tuple_name(input: &str) -> IResult<&str, String> {
    map(
        recognize(pair(
            satisfy(|c: char| c.is_ascii_uppercase()),
            take_while(|c: char| c.is_ascii_alphanumeric() || c == '_'),
        )),
        String::from,
    )(input)
}

// Literal parsers

fn integer_literal(input: &str) -> IResult<&str, Literal> {
    map(
        pair(
            opt(char('-')),
            alt((
                // Hexadecimal: 0x...
                map_res(
                    preceded(tag("0x"), take_while1(|c: char| c.is_ascii_hexdigit())),
                    |s: &str| {
                        i64::from_str_radix(s, 16)
                            .map_err(|_| Error::IntegerMalformed(format!("0x{}", s)))
                    },
                ),
                // Binary: 0b...
                map_res(
                    preceded(tag("0b"), take_while1(|c: char| c == '0' || c == '1')),
                    |s: &str| {
                        i64::from_str_radix(s, 2)
                            .map_err(|_| Error::IntegerMalformed(format!("0b{}", s)))
                    },
                ),
                // Decimal
                map_res(digit1, |s: &str| {
                    s.parse::<i64>()
                        .map_err(|_| Error::IntegerMalformed(s.to_string()))
                }),
            )),
        ),
        |(sign, value)| Literal::Integer(if sign.is_some() { -value } else { value }),
    )(input)
}

fn binary_literal(input: &str) -> IResult<&str, Literal> {
    map_res(
        delimited(
            char('\''),
            take_while(|c: char| c.is_ascii_hexdigit()),
            char('\''),
        ),
        |s: &str| hex::decode(s).map(Literal::Binary),
    )(input)
}

fn string_term(input: &str) -> IResult<&str, Term> {
    map(
        delimited(
            char('"'),
            map_res(take_while(|c| c != '"'), |s: &str| parse_string_content(s)),
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

fn parse_string_content(s: &str) -> Result<String, Error> {
    let mut result = String::new();
    let mut chars = s.chars();

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.next() {
                Some('"') => result.push('"'),
                Some('\\') => result.push('\\'),
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some(c) => return Err(Error::StringEscapeInvalid(format!("\\{}", c))),
                None => return Err(Error::StringEscapeInvalid("\\".to_string())),
            }
        } else {
            result.push(ch);
        }
    }

    Ok(result)
}

fn literal(input: &str) -> IResult<&str, Literal> {
    alt((integer_literal, binary_literal))(input)
}

// Pattern parsers for terms

fn partial_pattern_inner(input: &str) -> IResult<&str, PartialPattern> {
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

fn primitive_type(input: &str) -> IResult<&str, Type> {
    alt((
        nom_value(Type::Primitive(PrimitiveType::Int), tag("int")),
        nom_value(Type::Primitive(PrimitiveType::Bin), tag("bin")),
    ))(input)
}

fn field_type(input: &str) -> IResult<&str, FieldType> {
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

fn field_type_list(input: &str) -> IResult<&str, Vec<FieldType>> {
    terminated(
        separated_list0(tuple((wsc, char(','), wsc)), field_type),
        opt(pair(wsc, char(','))),
    )(input)
}

fn partial_type(input: &str) -> IResult<&str, Type> {
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

fn tuple_type(input: &str) -> IResult<&str, Type> {
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

fn type_parameter(input: &str) -> IResult<&str, Type> {
    map(delimited(char('<'), identifier, char('>')), |name| {
        Type::Identifier {
            name,
            arguments: vec![],
        }
    })(input)
}

fn type_identifier(input: &str) -> IResult<&str, Type> {
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

fn type_cycle(input: &str) -> IResult<&str, Type> {
    map(
        preceded(
            char('&'),
            opt(map_res(digit1, |s: &str| s.parse::<usize>())),
        ),
        Type::Cycle,
    )(input)
}

fn process_type(input: &str) -> IResult<&str, Type> {
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

fn function_type(input: &str) -> IResult<&str, Type> {
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

fn function_input_type(input: &str) -> IResult<&str, Type> {
    alt((
        partial_type, // Must come before grouping parentheses
        delimited(pair(char('('), ws0), type_definition, pair(ws0, char(')'))),
        tuple_type,
        primitive_type,
        process_type,
        type_identifier,
    ))(input)
}

fn function_output_type(input: &str) -> IResult<&str, Type> {
    alt((
        partial_type, // Must come before grouping parentheses
        delimited(pair(char('('), ws0), type_definition, pair(ws0, char(')'))),
        tuple_type,
        primitive_type,
        process_type,
        type_identifier,
    ))(input)
}

fn base_type(input: &str) -> IResult<&str, Type> {
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

fn type_definition(input: &str) -> IResult<&str, Type> {
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
fn access(input: &str) -> IResult<&str, Access> {
    verify(
        map(
            tuple((
                opt(identifier),
                many0(preceded(
                    char('.'),
                    alt((
                        map(digit1, |s: &str| AccessPath::Index(s.parse().unwrap())),
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

// Parse select operator - can be prefix !(p1 | p2) or postfix p ~> ! or p ~> !timeout
fn select_term(input: &str) -> IResult<&str, Term> {
    alt((
        // Prefix form: !(alt1 | alt2 | ...)
        map(
            delimited(
                pair(char('!'), pair(char('('), wsc)),
                separated_list1(tuple((wsc, char('|'), wsc)), chain),
                pair(wsc, char(')')),
            ),
            |sources| Term::Select(Select { sources }),
        ),
        // Postfix form: ! or !term
        // Transform bare ! into !(~) - a select with a single ripple source
        map(
            pair(
                char('!'),
                opt(map(term, |t| Chain {
                    match_pattern: None,
                    terms: vec![t],
                    continuation: false,
                })),
            ),
            |(_, opt_chain)| {
                let sources = if let Some(c) = opt_chain {
                    vec![c]
                } else {
                    // Bare ! becomes !(~) - implicit ripple source
                    vec![Chain {
                        match_pattern: None,
                        terms: vec![Term::Ripple],
                        continuation: false,
                    }]
                };
                Term::Select(Select { sources })
            },
        ),
    ))(input)
}

fn import(input: &str) -> IResult<&str, String> {
    preceded(
        char('%'),
        delimited(
            char('"'),
            map_res(take_while(|c| c != '"'), |s: &str| parse_string_content(s)),
            char('"'),
        ),
    )(input)
}

fn tuple_field(input: &str) -> IResult<&str, TupleField> {
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

fn tuple_field_list(input: &str) -> IResult<&str, Vec<TupleField>> {
    terminated(
        separated_list0(tuple((wsc, char(','), wsc)), tuple_field),
        opt(pair(wsc, char(','))),
    )(input)
}

fn tuple_term(input: &str) -> IResult<&str, Tuple> {
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

fn branch(input: &str) -> IResult<&str, Branch> {
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

fn block(input: &str) -> IResult<&str, Block> {
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

fn function(input: &str) -> IResult<&str, Function> {
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

fn tail_call(input: &str) -> IResult<&str, Term> {
    let (input, _) = char('&')(input)?;
    let (input, ident) = opt(identifier)(input)?;
    let (input, accessors) = many0(preceded(
        char('.'),
        alt((
            map(digit1, |s: &str| AccessPath::Index(s.parse().unwrap())),
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

fn builtin(input: &str) -> IResult<&str, Builtin> {
    // Parse opening __
    let (input, _) = tag("__")(input)?;

    // Parse identifier with potential trailing underscores
    let (_remaining, result) = recognize(tuple((
        satisfy(|c: char| c.is_ascii_lowercase()),
        take_while(|c: char| c.is_ascii_alphanumeric() || c == '_'),
    )))(input)?;

    // Trim trailing underscores from the result
    let trimmed = result.trim_end_matches('_');

    // Adjust the remaining input to include the trimmed underscores
    let input = &input[trimmed.len()..];

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

fn equality(input: &str) -> IResult<&str, Term> {
    nom_value(Term::Equality, tag("=="))(input)
}

fn not_term(input: &str) -> IResult<&str, Term> {
    nom_value(Term::Not, char('/'))(input)
}

fn spawn_term(input: &str) -> IResult<&str, Term> {
    // Match @ followed by a term
    map(preceded(char('@'), term), |t| Term::Spawn(Box::new(t)))(input)
}

fn self_term(input: &str) -> IResult<&str, Term> {
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

fn bind_match(input: &str) -> IResult<&str, Term> {
    map(preceded(char('='), match_pattern), Term::BindMatch)(input)
}

fn pin_match(input: &str) -> IResult<&str, Term> {
    map(preceded(char('^'), match_pattern), Term::PinMatch)(input)
}

fn match_field(input: &str) -> IResult<&str, MatchField> {
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

fn match_string(input: &str) -> IResult<&str, Match> {
    map(
        delimited(
            char('"'),
            map_res(take_while(|c| c != '"'), |s: &str| parse_string_content(s)),
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

fn match_pattern(input: &str) -> IResult<&str, Match> {
    alt((
        // Pin pattern (must come first to consume ^ prefix)
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
        // Then try partial patterns (must come before identifier)
        map(partial_pattern_inner, Match::Partial),
        // Then try literals
        map(literal, Match::Literal),
        // Star and placeholder
        map(char('*'), |_| Match::Star),
        map(char('_'), |_| Match::Placeholder),
        // Identifier must come last (since it's more general)
        map(identifier, Match::Identifier),
    ))(input)
}

fn match_tuple(input: &str) -> IResult<&str, MatchTuple> {
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

fn term(input: &str) -> IResult<&str, Term> {
    alt((
        // String terms (before literals to handle quotes)
        string_term,
        // Ripple placeholder (but not ~[...] which is tuple with ripple spread)
        map(terminated(char('~'), peek(not(char('[')))), |_| {
            Term::Ripple
        }),
        // Process operations
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

fn chain(input: &str) -> IResult<&str, Chain> {
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

fn chain_inner(input: &str) -> IResult<&str, (Vec<Term>, bool)> {
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

fn expression(input: &str) -> IResult<&str, Expression> {
    map(
        terminated(
            separated_list1(tuple((ws0, char(','), wsc)), chain),
            opt(pair(ws0, char(','))),
        ),
        |chains| Expression { chains },
    )(input)
}

// Statement parsers

fn type_alias(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            identifier,
            opt(delimited(
                char('<'),
                separated_list1(tuple((ws0, char(','), ws0)), identifier),
                char('>'),
            )),
            preceded(tuple((ws0, tag("::"), ws0)), type_definition),
        )),
        |(name, type_parameters, type_definition)| Statement::TypeAlias {
            name,
            type_parameters: type_parameters.unwrap_or_default(),
            type_definition,
        },
    )(input)
}

fn type_import_pattern(input: &str) -> IResult<&str, TypeImportPattern> {
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

fn type_import(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((
            type_import_pattern,
            preceded(tuple((ws0, tag("::"), ws0)), import),
        )),
        |(pattern, module_path)| Statement::TypeImport {
            pattern,
            module_path,
        },
    )(input)
}

fn statement_expression(input: &str) -> IResult<&str, Statement> {
    map(expression, Statement::Expression)(input)
}

fn statement(input: &str) -> IResult<&str, Statement> {
    preceded(
        ws_with_comments,
        alt((type_import, type_alias, statement_expression)),
    )(input)
}

fn statements(input: &str) -> IResult<&str, Vec<Statement>> {
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

fn program(input: &str) -> IResult<&str, Program> {
    map(
        delimited(
            ws_with_comments,
            statements,
            pair(ws_with_comments, nom::combinator::eof),
        ),
        |statements| Program { statements },
    )(input)
}
