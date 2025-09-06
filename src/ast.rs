#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    TypeAlias {
        name: String,
        type_definition: Type,
    },
    TypeImport {
        pattern: TypeImportPattern,
        module_path: String,
    },
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeImportPattern {
    Star,
    Partial(Vec<String>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub branches: Vec<Branch>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Branch {
    pub condition: Expression,
    pub consequence: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub terms: Vec<Term>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Assignment { pattern: Pattern, value: Chain },
    Chain(Chain),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Chain {
    pub value: Value,
    pub operations: Vec<Operation>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Literal(Literal),
    Tuple(ValueTuple),
    FunctionDefinition(FunctionDefinition),
    Block(Block),
    Parameter(Parameter),
    MemberAccess(MemberAccess),
    Import(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operation {
    Operator(Operator),
    Tuple(OperationTuple),
    Block(Block),
    MemberAccess(MemberAccess),
    FieldAccess(String),
    PositionalAccess(usize),
    TailCall(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Binary(Vec<u8>),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueTuple {
    pub name: Option<String>,
    pub fields: Vec<ValueTupleField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueTupleField {
    pub name: Option<String>,
    pub value: Chain,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OperationTuple {
    pub name: Option<String>,
    pub fields: Vec<OperationTupleField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OperationTupleField {
    pub name: Option<String>,
    pub value: OperationTupleFieldValue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OperationTupleFieldValue {
    Ripple,
    Chain(Chain),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub parameter_type: Option<Type>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Parameter {
    Self_,
    Indexed(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberAccess {
    pub target: String,
    pub accessors: Vec<AccessPath>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AccessPath {
    Field(String),
    Index(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Literal(Literal),
    Identifier(String),
    Tuple(TuplePattern),
    Partial(Vec<String>),
    Star,
    Placeholder,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TuplePattern {
    pub name: Option<String>,
    pub fields: Vec<PatternField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PatternField {
    pub name: Option<String>,
    pub pattern: Pattern,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Tuple(TupleType),
    Function(FunctionType),
    Union(UnionType),
    Identifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    Int,
    Bin,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleType {
    pub name: Option<String>,
    pub fields: Vec<FieldType>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldType {
    pub name: Option<String>,
    pub type_def: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub input: Box<Type>,
    pub output: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionType {
    pub types: Vec<Type>,
}

impl Parameter {
    pub fn from_string(s: &str) -> Option<Self> {
        if s == "$" {
            Some(Parameter::Self_)
        } else if s.starts_with('$') {
            s[1..].parse().ok().map(Parameter::Indexed)
        } else {
            None
        }
    }
}
