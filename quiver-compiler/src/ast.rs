#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    TypeAlias {
        name: String,
        type_parameters: Vec<String>,
        type_definition: Type,
    },
    TypeImport {
        pattern: TypeImportPattern,
        module: Vec<String>,
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
    pub match_pattern: Option<Match>,
    pub terms: Vec<Term>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Literal(Literal),
    Tuple(Tuple),
    Match(Match),
    Block(Block),
    Function(Function),
    Access(Access),
    Builtin(Builtin),
    TailCall(TailCall),
    Equality,
    Not,
    Spawn(Box<Term>),
    Self_,
    /// Select operation. None means bare `!` (postfix form using chained value).
    /// Some(sources) means explicit sources like `![a, b]` or `![]` (discards chained value).
    Select(Option<Vec<Chain>>),
    Process(usize),
    /// Reference operator (`&`). None creates a new unique ref, Some references a value.
    Reference(Option<Access>),
    /// Reference to a builtin without calling it (`&__add__`).
    BuiltinReference(Builtin),
}

impl Term {
    /// Returns true if this is a bare ripple placeholder (`~`)
    pub fn is_bare_ripple(&self) -> bool {
        matches!(
            self,
            Term::Access(Access {
                source: Some(AccessSource::Ripple),
                accessors,
                argument: None,
            }) if accessors.is_empty()
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Binary(Vec<u8>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    /// Tuple name: None for unnamed, Some for named (e.g., "Point")
    pub name: Option<String>,
    pub fields: Vec<TupleField>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FieldValue {
    Chain(Chain),
    /// Spread: None for bare `...`, Some(name) for `...name`
    Spread(Option<String>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleField {
    pub name: Option<String>,
    pub value: FieldValue,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub type_parameters: Vec<String>,
    pub parameter_type: Option<Type>,
    pub return_type: Option<Type>,
    pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AccessSource {
    /// Identifier like `foo`
    Identifier(String),
    /// Function parameter `$`
    Parameter,
    /// Ripple `~` - references the piped value
    Ripple,
    /// Import like `%math` or `%math/trig`
    Import(Vec<String>),
    /// Self reference `.` - the current process
    Self_,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Access {
    pub source: Option<AccessSource>,
    pub accessors: Vec<AccessPath>,
    pub argument: Option<Vec<TupleField>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AccessPath {
    Field(String),
    Index(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Builtin {
    pub name: String,
    pub argument: Option<Vec<TupleField>>,
}

/// Partial pattern field: (field_name, optional_nested_pattern)
/// None = bind field by name, Some = match field against nested pattern
pub type PartialPatternField = (String, Option<Match>);

#[derive(Debug, Clone, PartialEq)]
pub struct PartialPattern {
    pub name: Option<String>,
    pub fields: Vec<PartialPatternField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TailCall {
    pub identifier: Option<String>,
    pub accessors: Vec<AccessPath>,
    pub argument: Option<Vec<TupleField>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Match {
    Identifier(String),
    Literal(Literal),
    Tuple(MatchTuple),
    Partial(PartialPattern),
    Star,
    Placeholder,
    Reference(Type),
    Type(Type),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchTuple {
    pub name: Option<String>,
    pub fields: Vec<MatchField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchField {
    pub name: Option<String>,
    pub pattern: Match,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimitiveType),
    Tuple(TupleType),
    Function(FunctionType),
    Union(UnionType),
    Identifier { name: String, arguments: Vec<Type> },
    Cycle(Option<usize>),
    Process(ProcessType),
    Resource(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcessType {
    pub receive_type: Option<Box<Type>>,
    pub return_type: Option<Box<Type>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    Int,
    Bin,
    Ref,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleType {
    /// Tuple name: None for unnamed, Some for named or type alias reference
    pub name: Option<String>,
    pub fields: Vec<FieldType>,
    pub is_partial: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FieldType {
    Field {
        name: Option<String>,
        type_def: Type,
    },
    Spread {
        identifier: Option<String>,
        type_arguments: Vec<Type>,
    },
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
