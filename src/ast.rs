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
pub enum FunctionCallTarget {
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
    FunctionCall(FunctionCallTarget),
    MemberAccess(MemberAccess),
    Import(String),
    Builtin(String),
    TailCall(String),
    Equality,
    Not,
    Partial(PartialPattern),
    Star,
    Placeholder,
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
    pub target: MemberTarget,
    pub accessors: Vec<AccessPath>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MemberTarget {
    None,
    Identifier(String),
    Parameter,
}

#[derive(Debug, Clone, PartialEq)]
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
