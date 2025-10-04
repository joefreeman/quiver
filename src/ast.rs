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
pub struct Chain {
    pub terms: Vec<Term>,
    pub continuation: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionCall {
    Builtin(String),
    Identifier {
        name: String,
        accessors: Vec<AccessPath>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Literal(Literal),
    Identifier(String),
    Tuple(Tuple),
    Block(Block),
    FunctionDefinition(FunctionDefinition),
    FunctionCall(FunctionCall),
    MemberAccess(MemberAccess),
    Import(String),
    Builtin(String),
    TailCall(TailCall),
    Equality,
    Not,
    Partial(PartialPattern),
    Star,
    Placeholder,
    Spawn(Box<Term>),
    SendCall(SendCall),
    SelfRef,
    Receive(Receive),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Binary(Vec<u8>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    pub name: Option<String>,
    pub fields: Vec<TupleField>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FieldValue {
    Chain(Chain),
    Ripple,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleField {
    pub name: Option<String>,
    pub value: FieldValue,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub parameter_type: Option<Type>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberAccess {
    pub identifier: Option<String>,
    pub accessors: Vec<AccessPath>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AccessPath {
    Field(String),
    Index(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PartialPattern {
    pub name: Option<String>,
    pub fields: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TailCall {
    pub identifier: Option<String>,
    pub accessors: Vec<AccessPath>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SendCall {
    pub name: String,
    pub accessors: Vec<AccessPath>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Receive {
    pub type_def: Type,
    pub block: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Tuple(TupleType),
    Function(FunctionType),
    Union(UnionType),
    Identifier(String),
    Cycle(Option<usize>),
    Process(ProcessType),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcessType {
    pub receive_type: Box<Type>,
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
