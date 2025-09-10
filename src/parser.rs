use crate::ast::*;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct Grammar;

#[derive(Debug, Clone)]
pub enum Error {
    // Grammar parsing errors
    SyntaxError(Box<pest::error::Error<Rule>>),

    // Literal parsing errors
    IntegerMalformed(String),
    HexMalformed(String),
    StringEscapeInvalid(String),
    IndexMalformed(String),

    // Language construct errors
    ParameterInvalid(String),
    OperatorUnknown(String),

    // Structure errors
    RuleUnexpected { found: Rule, context: String },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::SyntaxError(err) => write!(f, "Syntax error: {}", err),
            Error::IntegerMalformed(lit) => write!(f, "Malformed integer: {}", lit),
            Error::HexMalformed(lit) => write!(f, "Malformed hex literal: {}", lit),
            Error::StringEscapeInvalid(esc) => write!(f, "Invalid string escape: {}", esc),
            Error::IndexMalformed(idx) => write!(f, "Malformed index: {}", idx),
            Error::ParameterInvalid(param) => write!(f, "Invalid parameter: {}", param),
            Error::OperatorUnknown(op) => write!(f, "Unknown operator: {}", op),
            Error::RuleUnexpected { found, context } => {
                write!(f, "Unexpected {:?} in {}", found, context)
            }
        }
    }
}

impl std::error::Error for Error {}

pub fn parse(source: &str) -> Result<Program, Error> {
    let pairs =
        Grammar::parse(Rule::program, source).map_err(|e| Error::SyntaxError(Box::new(e)))?;

    let program_pair = pairs.into_iter().next().unwrap();
    parse_program(program_pair)
}

fn parse_program(pair: pest::iterators::Pair<Rule>) -> Result<Program, Error> {
    let mut statements = Vec::new();

    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::statements => {
                for stmt_pair in inner_pair.into_inner() {
                    statements.push(parse_statement(stmt_pair)?);
                }
            }
            Rule::EOI => break,
            _ => {}
        }
    }

    Ok(Program { statements })
}

fn parse_statement(pair: pest::iterators::Pair<Rule>) -> Result<Statement, Error> {
    let inner_pair = pair.into_inner().next().unwrap();

    match inner_pair.as_rule() {
        Rule::type_alias => parse_type_alias(inner_pair),
        Rule::type_import => parse_type_import(inner_pair),
        Rule::expression => Ok(Statement::Expression(parse_expression(inner_pair)?)),
        rule => Err(Error::RuleUnexpected {
            found: rule,
            context: "statement".to_string(),
        }),
    }
}

fn parse_type_alias(pair: pest::iterators::Pair<Rule>) -> Result<Statement, Error> {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    let type_definition = parse_type_definition(inner.next().unwrap())?;

    Ok(Statement::TypeAlias {
        name,
        type_definition,
    })
}

fn parse_type_import(pair: pest::iterators::Pair<Rule>) -> Result<Statement, Error> {
    let mut inner = pair.into_inner();
    let pattern_pair = inner.next().unwrap();
    let import_pair = inner.next().unwrap();

    let pattern = parse_type_import_pattern(pattern_pair)?;
    let module_path = parse_string_literal(import_pair.into_inner().next().unwrap().as_str())?;

    Ok(Statement::TypeImport {
        pattern,
        module_path,
    })
}

fn parse_type_import_pattern(
    pair: pest::iterators::Pair<Rule>,
) -> Result<TypeImportPattern, Error> {
    let inner_pair = pair.into_inner().next().unwrap();

    match inner_pair.as_rule() {
        Rule::type_star_pattern => Ok(TypeImportPattern::Star),
        Rule::type_partial_pattern => {
            let identifiers: Vec<String> = inner_pair
                .into_inner()
                .map(|p| p.as_str().to_string())
                .collect();
            Ok(TypeImportPattern::Partial(identifiers))
        }
        rule => Err(Error::RuleUnexpected {
            found: rule,
            context: "type import pattern".to_string(),
        }),
    }
}

fn parse_block(pair: pest::iterators::Pair<Rule>) -> Result<Block, Error> {
    let mut branches = Vec::new();

    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::branch => branches.push(parse_branch(inner_pair)?),
            _ => {}
        }
    }

    Ok(Block { branches })
}

fn parse_branch(pair: pest::iterators::Pair<Rule>) -> Result<Branch, Error> {
    let mut inner = pair.into_inner();
    let condition = parse_expression(inner.next().unwrap())?;
    let consequence = inner.next().map(parse_expression).transpose()?;

    Ok(Branch {
        condition,
        consequence,
    })
}

fn parse_expression(pair: pest::iterators::Pair<Rule>) -> Result<Expression, Error> {
    let mut terms = Vec::new();

    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::term => terms.push(parse_term(inner_pair)?),
            _ => {}
        }
    }

    Ok(Expression { terms })
}

fn parse_term(pair: pest::iterators::Pair<Rule>) -> Result<Term, Error> {
    let inner_pair = pair.into_inner().next().unwrap();

    match inner_pair.as_rule() {
        Rule::assignment => parse_assignment(inner_pair),
        Rule::chain => Ok(Term::Chain(parse_chain(inner_pair)?)),
        rule => Err(Error::RuleUnexpected {
            found: rule,
            context: "term".to_string(),
        }),
    }
}

fn parse_assignment(pair: pest::iterators::Pair<Rule>) -> Result<Term, Error> {
    let mut inner = pair.into_inner();
    let pattern = parse_pattern(inner.next().unwrap())?;
    let value = parse_chain(inner.next().unwrap())?;

    Ok(Term::Assignment { pattern, value })
}

fn parse_chain(pair: pest::iterators::Pair<Rule>) -> Result<Chain, Error> {
    let mut inner = pair.into_inner();
    let value = parse_value(inner.next().unwrap())?;
    let mut operations = Vec::new();

    for op_pair in inner {
        operations.push(parse_operation(op_pair)?);
    }

    Ok(Chain { value, operations })
}

fn parse_value(pair: pest::iterators::Pair<Rule>) -> Result<Value, Error> {
    let inner_pair = pair.into_inner().next().unwrap();

    match inner_pair.as_rule() {
        Rule::literal => Ok(Value::Literal(parse_literal(inner_pair)?)),
        Rule::value_tuple => Ok(Value::Tuple(parse_value_tuple(inner_pair)?)),
        Rule::function_definition => Ok(Value::FunctionDefinition(parse_function_definition(
            inner_pair,
        )?)),
        Rule::block => Ok(Value::Block(parse_block(inner_pair)?)),
        Rule::parameter => Ok(Value::Parameter(parse_parameter(inner_pair)?)),
        Rule::member_access => Ok(Value::MemberAccess(parse_member_access(inner_pair)?)),
        Rule::import => {
            let path = parse_string_literal(inner_pair.into_inner().next().unwrap().as_str())?;
            Ok(Value::Import(path))
        }
        rule => Err(Error::RuleUnexpected {
            found: rule,
            context: "value".to_string(),
        }),
    }
}

fn parse_operation(pair: pest::iterators::Pair<Rule>) -> Result<Operation, Error> {
    match pair.as_rule() {
        Rule::operation => {
            // Operation is a wrapper, get the inner rule
            let inner_pair = pair.into_inner().next().unwrap();
            parse_operation(inner_pair)
        }
        Rule::operator => Ok(Operation::Operator(parse_operator(pair)?)),
        Rule::operation_tuple => Ok(Operation::Tuple(parse_operation_tuple(pair)?)),
        Rule::block => Ok(Operation::Block(parse_block(pair)?)),
        Rule::parameter => Ok(Operation::Parameter(parse_parameter(pair)?)),
        Rule::member_access => Ok(Operation::MemberAccess(parse_member_access(pair)?)),
        // Rule::identifier => Ok(Operation::Identifier(pair.as_str().to_string())),
        Rule::field_access => {
            let field_name = pair.as_str()[1..].to_string(); // Remove leading '.'
            Ok(Operation::FieldAccess(field_name))
        }
        Rule::positional_access => {
            let index_str = &pair.as_str()[1..]; // Remove leading '.'
            let index = index_str
                .parse()
                .map_err(|_| Error::IndexMalformed(index_str.to_string()))?;
            Ok(Operation::PositionalAccess(index))
        }
        Rule::tail_call => {
            let name = pair
                .into_inner()
                .next()
                .map(|p| p.as_str().to_string())
                .unwrap_or_default();
            Ok(Operation::TailCall(name))
        }
        Rule::builtin => {
            let name = pair.into_inner().next().unwrap().as_str().to_string();
            Ok(Operation::Builtin(name))
        }
        rule => Err(Error::RuleUnexpected {
            found: rule,
            context: "operation".to_string(),
        }),
    }
}

fn parse_literal(pair: pest::iterators::Pair<Rule>) -> Result<Literal, Error> {
    let inner_pair = pair.into_inner().next().unwrap();

    match inner_pair.as_rule() {
        Rule::integer_literal => {
            let value = inner_pair
                .as_str()
                .parse()
                .map_err(|_| Error::IntegerMalformed(inner_pair.as_str().to_string()))?;
            Ok(Literal::Integer(value))
        }
        Rule::binary_literal => {
            let hex_str = &inner_pair.as_str()[1..inner_pair.as_str().len() - 1]; // Remove quotes
            let bytes = hex::decode(hex_str)
                .map_err(|_| Error::HexMalformed(inner_pair.as_str().to_string()))?;
            Ok(Literal::Binary(bytes))
        }
        Rule::string_literal => {
            let string_value = parse_string_literal(inner_pair.as_str())?;
            Ok(Literal::String(string_value))
        }
        rule => Err(Error::RuleUnexpected {
            found: rule,
            context: "literal".to_string(),
        }),
    }
}

fn parse_string_literal(s: &str) -> Result<String, Error> {
    let content = &s[1..s.len() - 1]; // Remove quotes
    let mut result = String::new();
    let mut chars = content.chars();

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

fn parse_value_tuple(pair: pest::iterators::Pair<Rule>) -> Result<ValueTuple, Error> {
    let mut name = None;
    let mut fields = Vec::new();

    for field_pair in pair.into_inner() {
        match field_pair.as_rule() {
            Rule::tuple_name => name = Some(field_pair.as_str().to_string()),
            Rule::value_tuple_field_list => {
                for tuple_field_pair in field_pair.into_inner() {
                    fields.push(parse_value_tuple_field(tuple_field_pair)?);
                }
            }
            _ => {}
        }
    }

    Ok(ValueTuple { name, fields })
}

fn parse_value_tuple_field(pair: pest::iterators::Pair<Rule>) -> Result<ValueTupleField, Error> {
    let mut name = None;
    let mut value = None;

    for field_part in pair.into_inner() {
        match field_part.as_rule() {
            Rule::identifier => name = Some(field_part.as_str().to_string()),
            Rule::chain => value = Some(parse_chain(field_part)?),
            _ => {}
        }
    }

    Ok(ValueTupleField {
        name,
        value: value.unwrap(),
    })
}

fn parse_operation_tuple(pair: pest::iterators::Pair<Rule>) -> Result<OperationTuple, Error> {
    let mut name = None;
    let mut fields = Vec::new();

    for field_pair in pair.into_inner() {
        match field_pair.as_rule() {
            Rule::tuple_name => name = Some(field_pair.as_str().to_string()),
            Rule::operation_tuple_field_list => {
                for tuple_field_pair in field_pair.into_inner() {
                    fields.push(parse_operation_tuple_field(tuple_field_pair)?);
                }
            }
            _ => {}
        }
    }

    Ok(OperationTuple { name, fields })
}

fn parse_operation_tuple_field(
    pair: pest::iterators::Pair<Rule>,
) -> Result<OperationTupleField, Error> {
    let mut name = None;
    let mut value = None;

    for field_part in pair.into_inner() {
        match field_part.as_rule() {
            Rule::identifier => name = Some(field_part.as_str().to_string()),
            Rule::ripple => value = Some(OperationTupleFieldValue::Ripple),
            Rule::chain => value = Some(OperationTupleFieldValue::Chain(parse_chain(field_part)?)),
            _ => {}
        }
    }

    Ok(OperationTupleField {
        name,
        value: value.unwrap(),
    })
}

fn parse_function_definition(
    pair: pest::iterators::Pair<Rule>,
) -> Result<FunctionDefinition, Error> {
    let mut parameter_type = None;
    let mut body = None;

    for part in pair.into_inner() {
        match part.as_rule() {
            Rule::function_input_type => parameter_type = Some(parse_function_input_type(part)?),
            Rule::block => body = Some(parse_block(part)?),
            _ => {}
        }
    }

    Ok(FunctionDefinition {
        parameter_type,
        body: body.unwrap(),
    })
}

fn parse_parameter(pair: pest::iterators::Pair<Rule>) -> Result<Parameter, Error> {
    Parameter::from_string(pair.as_str())
        .ok_or_else(|| Error::ParameterInvalid(pair.as_str().to_string()))
}

fn parse_member_access(pair: pest::iterators::Pair<Rule>) -> Result<MemberAccess, Error> {
    let access_str = pair.as_str();
    let parts: Vec<&str> = access_str.split('.').collect();
    let target = parts[0].to_string();
    let mut accessors = Vec::new();

    for part in &parts[1..] {
        if part.chars().all(|c| c.is_ascii_digit()) {
            let index = part
                .parse()
                .map_err(|_| Error::IndexMalformed(part.to_string()))?;
            accessors.push(AccessPath::Index(index));
        } else {
            accessors.push(AccessPath::Field(part.to_string()));
        }
    }

    Ok(MemberAccess { target, accessors })
}

fn parse_operator(pair: pest::iterators::Pair<Rule>) -> Result<Operator, Error> {
    match pair.as_str() {
        "+" => Ok(Operator::Add),
        "-" => Ok(Operator::Subtract),
        "*" => Ok(Operator::Multiply),
        "/" => Ok(Operator::Divide),
        "%" => Ok(Operator::Modulo),
        "==" => Ok(Operator::Equal),
        "!=" => Ok(Operator::NotEqual),
        "<" => Ok(Operator::LessThan),
        "<=" => Ok(Operator::LessThanOrEqual),
        ">" => Ok(Operator::GreaterThan),
        ">=" => Ok(Operator::GreaterThanOrEqual),
        op => Err(Error::OperatorUnknown(op.to_string())),
    }
}

fn parse_pattern(pair: pest::iterators::Pair<Rule>) -> Result<Pattern, Error> {
    let inner_pair = pair.into_inner().next().unwrap();

    match inner_pair.as_rule() {
        Rule::literal => Ok(Pattern::Literal(parse_literal(inner_pair)?)),
        Rule::identifier => Ok(Pattern::Identifier(inner_pair.as_str().to_string())),
        Rule::tuple_pattern => Ok(Pattern::Tuple(parse_tuple_pattern(inner_pair)?)),
        Rule::partial_pattern => Ok(Pattern::Partial(parse_partial_pattern(inner_pair)?)),
        Rule::star => Ok(Pattern::Star),
        Rule::placeholder => Ok(Pattern::Placeholder),
        rule => Err(Error::RuleUnexpected {
            found: rule,
            context: "pattern".to_string(),
        }),
    }
}

fn parse_tuple_pattern(pair: pest::iterators::Pair<Rule>) -> Result<TuplePattern, Error> {
    let mut name = None;
    let mut fields = Vec::new();

    for part in pair.into_inner() {
        match part.as_rule() {
            Rule::tuple_name => name = Some(part.as_str().to_string()),
            Rule::pattern_field_list => {
                for field_pair in part.into_inner() {
                    fields.push(parse_pattern_field(field_pair)?);
                }
            }
            _ => {}
        }
    }

    Ok(TuplePattern { name, fields })
}

fn parse_pattern_field(pair: pest::iterators::Pair<Rule>) -> Result<PatternField, Error> {
    let mut name = None;
    let mut pattern = None;

    for part in pair.into_inner() {
        match part.as_rule() {
            Rule::identifier => {
                if name.is_none() {
                    name = Some(part.as_str().to_string());
                } else {
                    pattern = Some(Pattern::Identifier(part.as_str().to_string()));
                }
            }
            Rule::pattern => pattern = Some(parse_pattern(part)?),
            _ => {}
        }
    }

    // If we only have one identifier, it's the pattern, not the name
    if pattern.is_none() && name.is_some() {
        pattern = Some(Pattern::Identifier(name.take().unwrap()));
    }

    Ok(PatternField {
        name,
        pattern: pattern.unwrap(),
    })
}

fn parse_partial_pattern(pair: pest::iterators::Pair<Rule>) -> Result<Vec<String>, Error> {
    Ok(pair
        .into_inner()
        .next()
        .unwrap()
        .into_inner()
        .map(|p| p.as_str().to_string())
        .collect())
}

fn parse_type_definition(pair: pest::iterators::Pair<Rule>) -> Result<Type, Error> {
    let mut inner = pair.into_inner();
    let first_type = parse_base_type(inner.next().unwrap())?;

    let mut types = vec![first_type];

    // Check if there are more types separated by "|"
    for pair in inner {
        types.push(parse_base_type(pair)?);
    }

    if types.len() == 1 {
        Ok(types.into_iter().next().unwrap())
    } else {
        Ok(Type::Union(UnionType { types }))
    }
}

fn parse_base_type(pair: pest::iterators::Pair<Rule>) -> Result<Type, Error> {
    let inner_pair = pair.into_inner().next().unwrap();

    match inner_pair.as_rule() {
        Rule::primitive_type => parse_primitive_type(inner_pair),
        Rule::tuple_type => Ok(Type::Tuple(parse_tuple_type(inner_pair)?)),
        Rule::function_type => Ok(Type::Function(parse_function_type(inner_pair)?)),
        Rule::identifier => Ok(Type::Identifier(inner_pair.as_str().to_string())),
        rule => Err(Error::RuleUnexpected {
            found: rule,
            context: "base type".to_string(),
        }),
    }
}

fn parse_primitive_type(pair: pest::iterators::Pair<Rule>) -> Result<Type, Error> {
    match pair.as_str() {
        "int" => Ok(Type::Primitive(PrimitiveType::Int)),
        "bin" => Ok(Type::Primitive(PrimitiveType::Bin)),
        _ => Err(Error::RuleUnexpected {
            found: Rule::primitive_type,
            context: "primitive type".to_string(),
        }),
    }
}

fn parse_tuple_type(pair: pest::iterators::Pair<Rule>) -> Result<TupleType, Error> {
    let mut name = None;
    let mut fields = Vec::new();

    for part in pair.into_inner() {
        match part.as_rule() {
            Rule::tuple_name => name = Some(part.as_str().to_string()),
            Rule::field_type_list => {
                for field_pair in part.into_inner() {
                    fields.push(parse_field_type(field_pair)?);
                }
            }
            _ => {}
        }
    }

    Ok(TupleType { name, fields })
}

fn parse_field_type(pair: pest::iterators::Pair<Rule>) -> Result<FieldType, Error> {
    let mut name = None;
    let mut type_def = None;

    for part in pair.into_inner() {
        match part.as_rule() {
            Rule::identifier => name = Some(part.as_str().to_string()),
            Rule::type_definition => type_def = Some(parse_type_definition(part)?),
            _ => {}
        }
    }

    Ok(FieldType {
        name,
        type_def: type_def.unwrap(),
    })
}

fn parse_function_type(pair: pest::iterators::Pair<Rule>) -> Result<FunctionType, Error> {
    let mut inner = pair.into_inner();
    let input = Box::new(parse_function_input_type(inner.next().unwrap())?);
    let output = Box::new(parse_type_definition(inner.next().unwrap())?);

    Ok(FunctionType { input, output })
}

fn parse_function_input_type(pair: pest::iterators::Pair<Rule>) -> Result<Type, Error> {
    let inner_pair = pair.into_inner().next().unwrap();

    match inner_pair.as_rule() {
        Rule::function_type => Ok(Type::Function(parse_function_type(inner_pair)?)),
        Rule::type_definition => parse_type_definition(inner_pair),
        Rule::tuple_type => Ok(Type::Tuple(parse_tuple_type(inner_pair)?)),
        Rule::primitive_type => parse_primitive_type(inner_pair),
        Rule::identifier => Ok(Type::Identifier(inner_pair.as_str().to_string())),
        rule => Err(Error::RuleUnexpected {
            found: rule,
            context: "function input type".to_string(),
        }),
    }
}
