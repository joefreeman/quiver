use crate::ast::*;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{char, digit1, multispace0, multispace1, satisfy},
    combinator::{map, map_res, opt, recognize, value as nom_value},
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

fn string_literal(input: &str) -> IResult<&str, Literal> {
    map(
        delimited(
            char('"'),
            map_res(take_while(|c| c != '"'), |s: &str| parse_string_content(s)),
            char('"'),
        ),
        Literal::String,
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
    alt((integer_literal, binary_literal, string_literal))(input)
}

// Pattern parsers

fn pattern_literal(input: &str) -> IResult<&str, Pattern> {
    map(literal, Pattern::Literal)(input)
}

fn pattern_identifier(input: &str) -> IResult<&str, Pattern> {
    map(identifier, Pattern::Identifier)(input)
}

fn pattern_star(input: &str) -> IResult<&str, Pattern> {
    nom_value(Pattern::Star, char('*'))(input)
}

fn pattern_placeholder(input: &str) -> IResult<&str, Pattern> {
    nom_value(Pattern::Placeholder, char('_'))(input)
}

fn partial_pattern(input: &str) -> IResult<&str, Pattern> {
    map(
        delimited(
            pair(char('('), ws0),
            separated_list1(tuple((ws0, char(','), ws1)), identifier),
            pair(ws0, char(')')),
        ),
        Pattern::Partial,
    )(input)
}

fn pattern_field(input: &str) -> IResult<&str, PatternField> {
    alt((
        map(
            separated_pair(identifier, tuple((char(':'), ws1)), pattern),
            |(name, pattern)| PatternField {
                name: Some(name),
                pattern,
            },
        ),
        map(pattern, |p| PatternField {
            name: None,
            pattern: p,
        }),
    ))(input)
}

fn pattern_field_list(input: &str) -> IResult<&str, Vec<PatternField>> {
    terminated(
        separated_list0(tuple((wsc, char(','), wsc)), pattern_field),
        opt(pair(wsc, char(','))),
    )(input)
}

fn tuple_pattern(input: &str) -> IResult<&str, Pattern> {
    map(
        alt((
            map(
                tuple((
                    tuple_name,
                    delimited(
                        pair(char('['), wsc),
                        pattern_field_list,
                        pair(wsc, char(']')),
                    ),
                )),
                |(name, fields)| TuplePattern {
                    name: Some(name),
                    fields,
                },
            ),
            map(
                delimited(
                    pair(char('['), wsc),
                    pattern_field_list,
                    pair(wsc, char(']')),
                ),
                |fields| TuplePattern { name: None, fields },
            ),
            map(tuple_name, |name| TuplePattern {
                name: Some(name),
                fields: vec![],
            }),
        )),
        Pattern::Tuple,
    )(input)
}

fn pattern(input: &str) -> IResult<&str, Pattern> {
    alt((
        partial_pattern,
        tuple_pattern,
        pattern_literal,
        pattern_star,
        pattern_placeholder,
        pattern_identifier,
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
        type_identifier,
    ))(input)
}

fn function_output_type(input: &str) -> IResult<&str, Type> {
    alt((
        delimited(pair(char('('), ws0), type_definition, pair(ws0, char(')'))),
        tuple_type,
        primitive_type,
        type_identifier,
    ))(input)
}

fn base_type(input: &str) -> IResult<&str, Type> {
    alt((
        tuple_type,
        primitive_type,
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

// Value and Operation parsers

// Parse member access patterns (for assignment) - no '!' at the end
// Examples: f, f.x, $.0, etc.
fn member_access(input: &str) -> IResult<&str, MemberAccess> {
    map(
        pair(
            alt((
                nom_value(MemberTarget::Parameter, char('$')),
                map(identifier, MemberTarget::Identifier),
            )),
            many0(preceded(
                char('.'),
                alt((
                    map(digit1, |s: &str| AccessPath::Index(s.parse().unwrap())),
                    map(identifier, AccessPath::Field),
                )),
            )),
        ),
        |(target, accessors)| MemberAccess { target, accessors },
    )(input)
}

// Parse function calls - must end with '!'
// Examples: f!, f.x!, $.0!, etc.
fn function_call(input: &str) -> IResult<&str, MemberAccess> {
    let (input, target) = alt((
        nom_value(MemberTarget::Parameter, char('$')),
        map(identifier, MemberTarget::Identifier),
    ))(input)?;

    // Check if there's a ! immediately after the identifier
    if let Ok((remaining, _)) = char::<&str, nom::error::Error<&str>>('!')(input) {
        return Ok((
            remaining,
            MemberAccess {
                target,
                accessors: vec![],
            },
        ));
    }

    // Otherwise, parse field accesses
    let mut accessors = vec![];
    let mut current_input = input;

    while let Ok((input_after_dot, _)) = char::<&str, nom::error::Error<&str>>('.')(current_input) {
        // Try to parse an index or field
        if let Ok((remaining, index_str)) = digit1::<&str, nom::error::Error<&str>>(input_after_dot)
        {
            accessors.push(AccessPath::Index(index_str.parse().unwrap()));
            current_input = remaining;

            // Check for ! after this accessor
            if let Ok((remaining, _)) = char::<&str, nom::error::Error<&str>>('!')(current_input) {
                return Ok((remaining, MemberAccess { target, accessors }));
            }
        } else {
            // Try to parse a field, but it might end with !
            // First try regular identifier
            if let Ok((remaining, field)) = identifier(input_after_dot) {
                accessors.push(AccessPath::Field(field.clone()));

                // Check if there's a ! immediately after this field
                if let Ok((remaining_after_excl, _)) =
                    char::<&str, nom::error::Error<&str>>('!')(remaining)
                {
                    return Ok((remaining_after_excl, MemberAccess { target, accessors }));
                }

                current_input = remaining;
            } else {
                // No valid field found
                break;
            }
        }
    }

    // If we get here, we didn't find a !, so this isn't a function call
    Err(nom::Err::Error(nom::error::Error::new(
        input,
        nom::error::ErrorKind::Tag,
    )))
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

fn value_tuple_field(input: &str) -> IResult<&str, ValueTupleField> {
    alt((
        map(
            separated_pair(identifier, tuple((char(':'), ws1)), chain),
            |(name, value)| ValueTupleField {
                name: Some(name),
                value,
            },
        ),
        map(chain, |value| ValueTupleField { name: None, value }),
    ))(input)
}

fn value_tuple_field_list(input: &str) -> IResult<&str, Vec<ValueTupleField>> {
    terminated(
        separated_list0(tuple((wsc, char(','), wsc)), value_tuple_field),
        opt(pair(wsc, char(','))),
    )(input)
}

fn value_tuple(input: &str) -> IResult<&str, ValueTuple> {
    alt((
        map(
            tuple((
                tuple_name,
                delimited(
                    pair(char('['), wsc),
                    value_tuple_field_list,
                    pair(wsc, char(']')),
                ),
            )),
            |(name, fields)| ValueTuple {
                name: Some(name),
                fields,
            },
        ),
        map(
            delimited(
                pair(char('['), wsc),
                value_tuple_field_list,
                pair(wsc, char(']')),
            ),
            |fields| ValueTuple { name: None, fields },
        ),
        map(tuple_name, |name| ValueTuple {
            name: Some(name),
            fields: vec![],
        }),
    ))(input)
}

fn operation_tuple_field(input: &str) -> IResult<&str, OperationTupleField> {
    alt((
        map(
            separated_pair(
                identifier,
                tuple((char(':'), ws1)),
                alt((
                    nom_value(OperationTupleFieldValue::Ripple, char('~')),
                    map(chain, OperationTupleFieldValue::Chain),
                )),
            ),
            |(name, value)| OperationTupleField {
                name: Some(name),
                value,
            },
        ),
        map(
            alt((
                nom_value(OperationTupleFieldValue::Ripple, char('~')),
                map(chain, OperationTupleFieldValue::Chain),
            )),
            |value| OperationTupleField { name: None, value },
        ),
    ))(input)
}

fn operation_tuple_field_list(input: &str) -> IResult<&str, Vec<OperationTupleField>> {
    terminated(
        separated_list0(tuple((wsc, char(','), wsc)), operation_tuple_field),
        opt(pair(wsc, char(','))),
    )(input)
}

fn operation_tuple(input: &str) -> IResult<&str, OperationTuple> {
    alt((
        map(
            tuple((
                tuple_name,
                delimited(
                    pair(char('['), wsc),
                    operation_tuple_field_list,
                    pair(wsc, char(']')),
                ),
            )),
            |(name, fields)| OperationTuple {
                name: Some(name),
                fields,
            },
        ),
        map(
            delimited(
                pair(char('['), wsc),
                operation_tuple_field_list,
                pair(wsc, char(']')),
            ),
            |fields| OperationTuple { name: None, fields },
        ),
        map(tuple_name, |name| OperationTuple {
            name: Some(name),
            fields: vec![],
        }),
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

// Branch for operation blocks - condition uses operation_expression
fn operation_branch(input: &str) -> IResult<&str, Branch> {
    map(
        pair(
            operation_expression,
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
            pair(char('{'), ws1),
            preceded(
                opt(pair(char('|'), ws1)),
                separated_list1(tuple((ws1, char('|'), ws1)), branch),
            ),
            pair(ws1, char('}')),
        ),
        |branches| Block { branches },
    )(input)
}

// Block for operations (parametrized blocks)
fn operation_block(input: &str) -> IResult<&str, Block> {
    map(
        delimited(
            pair(char('{'), ws1),
            preceded(
                opt(pair(char('|'), ws1)),
                separated_list1(tuple((ws1, char('|'), ws1)), operation_branch),
            ),
            pair(ws1, char('}')),
        ),
        |branches| Block { branches },
    )(input)
}

fn function_definition(input: &str) -> IResult<&str, FunctionDefinition> {
    map(
        preceded(
            char('#'),
            pair(
                opt(terminated(function_input_type, ws1)),
                operation_block, // Functions are parametrized blocks
            ),
        ),
        |(parameter_type, body)| FunctionDefinition {
            parameter_type,
            body,
        },
    )(input)
}

fn field_access(input: &str) -> IResult<&str, String> {
    preceded(char('.'), identifier)(input)
}

fn positional_access(input: &str) -> IResult<&str, usize> {
    map_res(preceded(char('.'), digit1), |s: &str| s.parse())(input)
}

fn tail_call(input: &str) -> IResult<&str, String> {
    preceded(char('&'), map(opt(identifier), |i| i.unwrap_or_default()))(input)
}

fn builtin(input: &str) -> IResult<&str, String> {
    delimited(char('<'), identifier, char('>'))(input)
}

fn operator(input: &str) -> IResult<&str, Operation> {
    alt((
        nom_value(Operation::Equality, tag("==")),
        nom_value(Operation::Not, char('!')),
    ))(input)
}

fn value(input: &str) -> IResult<&str, Value> {
    alt((
        map(literal, Value::Literal),
        map(value_tuple, Value::Tuple),
        map(function_definition, Value::FunctionDefinition),
        map(block, Value::Block),
        map(member_access, Value::MemberAccess),
        map(import, Value::Import),
    ))(input)
}

fn operation(input: &str) -> IResult<&str, Operation> {
    alt((
        map(builtin, Operation::Builtin),
        operator,
        map(operation_block, Operation::Block), // Use operation_block for parametrized blocks
        // Function calls (must end with !)
        map(function_call, Operation::MemberAccess),
        // Parse tuples specially - check if they contain ripple (~)
        tuple_or_pattern,
        // Simple patterns (identifiers, literals, etc - non-tuples)
        map(
            alt((
                partial_pattern,
                pattern_literal,
                pattern_star,
                pattern_placeholder,
                pattern_identifier,
            )),
            Operation::Match,
        ),
        map(field_access, Operation::FieldAccess),
        map(positional_access, Operation::PositionalAccess),
        map(tail_call, Operation::TailCall),
    ))(input)
}

// Parse a tuple and determine if it's an operation (contains ~) or pattern (no ~)
fn tuple_or_pattern(input: &str) -> IResult<&str, Operation> {
    // First check if this looks like a tuple at all
    let start = input;

    // Try to parse as an operation tuple first (which can contain ~)
    if let Ok((remaining, op_tuple)) = operation_tuple(start) {
        // Check if any field is a ripple
        let has_ripple = op_tuple
            .fields
            .iter()
            .any(|f| matches!(f.value, OperationTupleFieldValue::Ripple));

        if has_ripple {
            // It has ripple, so it's definitely an operation tuple
            return Ok((remaining, Operation::Tuple(op_tuple)));
        }
        // No ripple - fall through to try as pattern
    }

    // Try to parse as a tuple pattern
    map(tuple_pattern, Operation::Match)(input)
}

fn chain(input: &str) -> IResult<&str, Chain> {
    map(
        pair(
            value,
            many0(preceded(tuple((ws1, tag("~>"), ws1)), operation)),
        ),
        |(value, operations)| Chain {
            value: Some(value),
            operations,
        },
    )(input)
}

// Chain that starts with operations (no initial value) - used in parametrized blocks
fn operation_chain(input: &str) -> IResult<&str, Chain> {
    map(
        separated_list1(tuple((ws1, tag("~>"), ws1)), operation),
        |operations| Chain {
            value: None,
            operations,
        },
    )(input)
}

fn expression(input: &str) -> IResult<&str, Expression> {
    map(
        separated_list1(tuple((ws0, char(','), wsc)), chain),
        |chains| Expression { chains },
    )(input)
}

// Expression for parametrized blocks - first chain has no initial value
fn operation_expression(input: &str) -> IResult<&str, Expression> {
    alt((
        map(
            separated_pair(
                operation_chain,
                tuple((ws0, char(','), wsc)),
                separated_list0(tuple((ws0, char(','), wsc)), chain),
            ),
            |(first_chain, mut rest_chains)| {
                let mut chains = vec![first_chain];
                chains.append(&mut rest_chains);
                Expression { chains }
            },
        ),
        map(operation_chain, |chain| Expression {
            chains: vec![chain],
        }),
    ))(input)
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
                separated_list1(tuple((ws0, char(','), ws1)), identifier),
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
