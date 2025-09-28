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
    pub chains: Vec<Chain>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ChainInput {
    Ripple,
    Parameter,
    Value(Value),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Chain {
    pub input: ChainInput,
    pub operations: Vec<Operation>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Literal(Literal),
    Tuple(Tuple),
    FunctionDefinition(FunctionDefinition),
    Block(Block),
    MemberAccess(MemberAccess),
    Import(String),
    Builtin(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operation {
    Tuple(Tuple),
    Block(Block),
    FunctionCall(MemberAccess),
    FieldAccess(String),
    PositionalAccess(usize),
    TailCall(String),
    Equality,
    Not,
    Match(Pattern),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Binary(Vec<u8>),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    pub name: Option<String>,
    pub fields: Vec<TupleField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleField {
    pub name: Option<String>,
    pub value: Chain,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub parameter_type: Option<Type>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberAccess {
    pub target: MemberTarget,
    pub accessors: Vec<AccessPath>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MemberTarget {
    Identifier(String),
    Parameter,
    Builtin(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AccessPath {
    Field(String),
    Index(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Literal(Literal),
    Identifier(String),
    Tuple(TuplePattern),
    Partial(PartialPattern),
    Star,
    Placeholder,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TuplePattern {
    pub name: Option<String>,
    pub fields: Vec<PatternField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PartialPattern {
    pub name: Option<String>,
    pub fields: Vec<String>,
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
