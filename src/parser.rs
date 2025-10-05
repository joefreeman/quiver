use crate::ast::*;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{char, digit1, multispace0, multispace1, satisfy},
    combinator::{map, map_res, not, opt, peek, recognize, value as nom_value},
    multi::{many0, many1, separated_list0, separated_list1},
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
    map_res(recognize(pair(opt(char('-')), digit1)), |s: &str| {
        s.parse::<i64>().map(Literal::Integer)
    })(input)
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
                name: Some("Str".to_string()),
                fields: vec![TupleField {
                    name: None,
                    value: FieldValue::Chain(Chain {
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

fn pattern_star(input: &str) -> IResult<&str, Term> {
    nom_value(Term::Star, char('*'))(input)
}

fn pattern_placeholder(input: &str) -> IResult<&str, Term> {
    nom_value(Term::Placeholder, char('_'))(input)
}

fn partial_pattern(input: &str) -> IResult<&str, Term> {
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
            |(name, fields)| {
                Term::Partial(PartialPattern {
                    name: Some(name),
                    fields,
                })
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
            |fields| Term::Partial(PartialPattern { name: None, fields }),
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
        map(
            separated_pair(identifier, tuple((char(':'), ws1)), type_definition),
            |(name, type_def)| FieldType {
                name: Some(name),
                type_def,
            },
        ),
        map(type_definition, |type_def| FieldType {
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

fn tuple_type(input: &str) -> IResult<&str, Type> {
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
                },
            ),
            map(
                delimited(pair(char('['), wsc), field_type_list, pair(wsc, char(']'))),
                |fields| TupleType { name: None, fields },
            ),
            map(tuple_name, |name| TupleType {
                name: Some(name),
                fields: vec![],
            }),
        )),
        Type::Tuple,
    )(input)
}

fn type_identifier(input: &str) -> IResult<&str, Type> {
    map(identifier, Type::Identifier)(input)
}

fn type_cycle(input: &str) -> IResult<&str, Type> {
    map(
        preceded(
            char('&'),
            opt(map_res(digit1, |s: &str| s.parse::<usize>())),
        ),
        |depth| Type::Cycle(depth),
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
        delimited(pair(char('('), ws0), type_definition, pair(ws0, char(')'))),
        tuple_type,
        primitive_type,
        process_type,
        type_identifier,
    ))(input)
}

fn function_output_type(input: &str) -> IResult<&str, Type> {
    alt((
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
        primitive_type,
        type_cycle,
        process_type,
        delimited(pair(char('('), ws0), type_definition, pair(ws0, char(')'))),
        type_identifier,
    ))(input)
}

fn type_definition(input: &str) -> IResult<&str, Type> {
    alt((
        function_type,
        map(
            separated_list1(tuple((ws1, char('|'), ws1)), base_type),
            |types| {
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

// Parse member access patterns - no '!' at the end
// Examples: f.x, .x, .0, etc.
fn member_access(input: &str) -> IResult<&str, MemberAccess> {
    alt((
        // Member access with identifier and accessors: foo.bar, foo.0
        map(
            pair(
                identifier,
                many1(preceded(
                    // Changed to many1 - requires at least one accessor
                    char('.'),
                    alt((
                        map(digit1, |s: &str| AccessPath::Index(s.parse().unwrap())),
                        map(identifier, AccessPath::Field),
                    )),
                )),
            ),
            |(identifier, accessors)| MemberAccess {
                identifier: Some(identifier),
                accessors,
            },
        ),
        // Field/positional access without identifier (.x, .0, .x.0)
        map(
            many1(preceded(
                char('.'),
                alt((
                    map(digit1, |s: &str| AccessPath::Index(s.parse().unwrap())),
                    map(identifier, AccessPath::Field),
                )),
            )),
            |accessors| MemberAccess {
                identifier: None,
                accessors,
            },
        ),
    ))(input)
}

// Parse function calls - must end with '!'
// Examples: f!, f.x!, <add>!, etc.
fn function_call(input: &str) -> IResult<&str, FunctionCall> {
    alt((
        // Builtin function call: <builtin>!
        map(terminated(builtin, char('!')), FunctionCall::Builtin),
        // Identifier function call with optional accessors: f!, f.x!, f.0!
        map(
            pair(
                identifier,
                many0(preceded(
                    char('.'),
                    alt((
                        map(digit1, |s: &str| AccessPath::Index(s.parse().unwrap())),
                        map(identifier, AccessPath::Field),
                    )),
                )),
            ),
            |(name, accessors)| {
                // Must consume the '!' at the end
                FunctionCall::Identifier { name, accessors }
            },
        ),
    ))(input)
    .and_then(|(remaining, target)| {
        // For identifier calls, we need to ensure there's a '!' at the end
        match &target {
            FunctionCall::Builtin(_) => Ok((remaining, target)), // Already consumed '!'
            FunctionCall::Identifier { .. } => {
                let (remaining, _) = char('!')(remaining)?;
                Ok((remaining, target))
            }
        }
    })
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
        // Named field with ripple: name: ~
        map(
            separated_pair(identifier, tuple((char(':'), ws1)), char('~')),
            |(name, _)| TupleField {
                name: Some(name),
                value: FieldValue::Ripple,
            },
        ),
        // Named field with chain: name: chain
        map(
            separated_pair(identifier, tuple((char(':'), ws1)), chain),
            |(name, chain_value)| TupleField {
                name: Some(name),
                value: FieldValue::Chain(chain_value),
            },
        ),
        // Unnamed ripple: ~
        map(char('~'), |_| TupleField {
            name: None,
            value: FieldValue::Ripple,
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
        map(
            tuple((
                tuple_name,
                delimited(pair(char('['), wsc), tuple_field_list, pair(wsc, char(']'))),
            )),
            |(name, fields)| Tuple {
                name: Some(name),
                fields,
            },
        ),
        map(
            delimited(pair(char('['), wsc), tuple_field_list, pair(wsc, char(']'))),
            |fields| Tuple { name: None, fields },
        ),
        // Only parse bare tuple name if not followed by '(' (which would indicate a partial pattern)
        map(
            tuple((
                tuple_name,
                peek(not(pair(ws0, char('(')))), // Ensure not followed by '('
            )),
            |(name, _)| Tuple {
                name: Some(name),
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

fn function_definition(input: &str) -> IResult<&str, FunctionDefinition> {
    map(
        preceded(
            char('#'),
            pair(opt(terminated(function_input_type, ws1)), block),
        ),
        |(parameter_type, body)| FunctionDefinition {
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
    Ok((
        input,
        Term::TailCall(TailCall {
            identifier: ident,
            accessors,
        }),
    ))
}

fn builtin(input: &str) -> IResult<&str, String> {
    delimited(char('<'), identifier, char('>'))(input)
}

fn equality(input: &str) -> IResult<&str, Term> {
    nom_value(Term::Equality, tag("=="))(input)
}

fn not_term(input: &str) -> IResult<&str, Term> {
    nom_value(Term::Not, char('!'))(input)
}

fn spawn_term(input: &str) -> IResult<&str, Term> {
    // Match @ followed by a term
    map(preceded(char('@'), term), |t| Term::Spawn(Box::new(t)))(input)
}

fn send_call(input: &str) -> IResult<&str, Term> {
    // Match identifier with optional accessors followed by '$'
    let (input, name) = identifier(input)?;
    let (input, accessors) = many0(preceded(
        char('.'),
        alt((
            map(digit1, |s: &str| AccessPath::Index(s.parse().unwrap())),
            map(identifier, AccessPath::Field),
        )),
    ))(input)?;
    let (input, _) = char('$')(input)?;
    Ok((input, Term::SendCall(SendCall { name, accessors })))
}

fn self_ref_term(input: &str) -> IResult<&str, Term> {
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
        |_| Term::SelfRef,
    )(input)
}

fn receive_term(input: &str) -> IResult<&str, Term> {
    map(
        pair(
            preceded(char('$'), type_definition),
            opt(preceded(ws0, block)),
        ),
        |(type_def, block)| Term::Receive(Receive { type_def, block }),
    )(input)
}

fn term(input: &str) -> IResult<&str, Term> {
    alt((
        // String terms (before literals to handle quotes)
        string_term,
        // Process operations
        receive_term,
        spawn_term,
        send_call,
        self_ref_term,
        // Literals
        map(literal, Term::Literal),
        // Complex terms
        map(tuple_term, Term::Tuple),
        map(function_definition, Term::FunctionDefinition),
        map(block, Term::Block),
        // Import
        map(import, Term::Import),
        // Function calls must be tried before builtin and identifier
        map(function_call, Term::FunctionCall),
        // Builtins (without !)
        map(builtin, Term::Builtin),
        // Member access (includes field/positional access)
        map(member_access, Term::MemberAccess),
        // Identifier (must come after member_access and function_call)
        map(identifier, Term::Identifier),
        // Patterns
        partial_pattern,
        pattern_star,
        pattern_placeholder,
        // Operations
        equality,
        not_term,
        tail_call,
    ))(input)
}

fn chain(input: &str) -> IResult<&str, Chain> {
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
            |(_, terms)| Chain {
                terms: terms.unwrap_or_default(),
                continuation: true,
            },
        ),
        // Normal chain: must have at least one term
        map(
            separated_list1(tuple((ws1, tag("~>"), ws1)), term),
            |terms| Chain {
                terms,
                continuation: false,
            },
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
            preceded(pair(tag("type"), ws1), identifier),
            preceded(tuple((ws1, char('='), ws1)), type_definition),
        )),
        |(name, type_definition)| Statement::TypeAlias {
            name,
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
            preceded(pair(tag("type"), ws1), type_import_pattern),
            preceded(tuple((ws1, char('='), ws1)), import),
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
