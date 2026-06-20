use crate::parser::SourceSpan;

/// An optional source span attached to an AST node.
///
/// Its `PartialEq`/`Eq` are intentionally always-true: attaching spans must not change the
/// structural equality of ASTs, so the parser's `assert_eq!`-style tests keep passing
/// regardless of source position. Spans are populated by [`crate::parse`] and consumed by
/// the language server.
#[derive(Debug, Clone, Copy, Default)]
pub struct Spanned(pub Option<SourceSpan>);

impl Spanned {
    pub fn get(self) -> Option<SourceSpan> {
        self.0
    }
}

impl PartialEq for Spanned {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl Eq for Spanned {}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    TypeAlias {
        name: String,
        /// Span of the alias name (`'point` in `'point = ...`), for symbols/go-to-definition.
        name_span: Spanned,
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
    /// Span of the binding pattern (the `x` in `x = ...`), for go-to-definition and symbols.
    /// `None` when the chain has no binding.
    pub bind_span: Spanned,
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
    Equality,
    Not,
    Spawn(Box<Term>, Spanned),
    Self_,
    /// Select operation. None means bare `!` (postfix form using chained value).
    /// Some(sources) means explicit sources like `![a, b]` or `![]` (discards chained value).
    /// The `Spanned` is the `!`, for hover (shows the received/awaited result type).
    Select(Option<Vec<Chain>>, Spanned),
    Process(usize),
    /// Reference operator (`&`). None creates a new unique ref, Some references a value
    /// (a variable, import member, or builtin — `&x`, `&m.f`, `&__add__`).
    Reference(Option<Access>),
    /// Function application: a looked-up callable head (an [`Access`] whose source is a variable,
    /// `$`, import member, or builtin) applied to an argument. `f 5`, `f [1, 2]`, `__add__ [3,4]`.
    /// The flowing value goes into the argument (evaluated as a flow position); the head is then
    /// invoked with it. Bare-accessor (`.f`) and ripple (`~`) heads are not applicable this way.
    Apply(Access, Box<Term>),
}

impl Term {
    /// The source span of this term, when it is an addressable node (a reference,
    /// builtin, or tail call). Used by the compiler to locate errors and by the language
    /// server for hover/go-to-definition.
    pub fn span(&self) -> Option<SourceSpan> {
        match self {
            Term::Access(access) => access.span.get(),
            _ => None,
        }
    }

    /// Returns true if this is a bare ripple placeholder (`~`)
    pub fn is_bare_ripple(&self) -> bool {
        matches!(
            self,
            Term::Access(Access {
                source: Some(AccessSource::Ripple),
                accessors,
                ..
            }) if accessors.is_empty()
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Binary(Vec<u8>),
}

/// How a tuple literal's name is determined.
#[derive(Debug, Clone, PartialEq)]
pub enum TupleName {
    /// Unnamed: `[...]`
    Anonymous,
    /// Named: `Point[...]`
    Named(String),
    /// Inherited from the first spread's source (`~[..., y]`, `a[..., y]`). The compiler resolves
    /// it from that source's tuple type.
    Inherit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    pub name: TupleName,
    pub fields: Vec<TupleField>,
    /// Span of the tuple literal, for hover (shows the constructed composite type).
    pub span: Spanned,
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
    /// Span of the field label (the `triple` in `triple: ...`), for go-to-definition onto a
    /// module's exported members. Absent for unnamed fields and spreads.
    pub name_span: Spanned,
    pub value: FieldValue,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub type_parameters: Vec<String>,
    pub parameter_type: Option<Type>,
    pub return_type: Option<Type>,
    pub body: Option<Block>,
    /// Span of the `#`, for hover (shows the inferred function type).
    pub span: Spanned,
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
    /// Builtin like `__add__` — a globally-resolved callable, looked up in the builtin
    /// registry rather than the lexical scope.
    Builtin(String),
    /// Tail call (`^`, `^f`, `^f.field`): `None` recurses into the current function, `Some(name)`
    /// tail-calls `name`. Compiled with the tail-call instruction (TCO), not a normal call.
    TailCall(Option<String>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Access {
    pub source: Option<AccessSource>,
    pub accessors: Vec<AccessPath>,
    /// Source span of each accessor (the `triple` in `.triple`), parallel to `accessors`, so
    /// the language server can hover/navigate each component of a chain separately. Kept beside
    /// `accessors` rather than inside `AccessPath`, whose identity is the field, not its position.
    pub accessor_spans: Vec<Spanned>,
    /// Span of the base (the `%util` / `$` / variable part, before any accessors).
    pub base_span: Spanned,
    /// Span of the whole access reference (`%util.triple`, `$.x`), for the fallback hover and
    /// for locating the symbol as a whole.
    pub span: Spanned,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AccessPath {
    Field(String),
    Index(usize),
}

/// Partial pattern field. `pattern` is `None` to bind the field by name (`(x)`), or `Some` to
/// match it against a nested pattern (`(x: pattern)`) — in which case `name` selects the field
/// and the binding lives in `pattern`. `name_span` covers the field name, for go-to-definition
/// on a bare binding.
#[derive(Debug, Clone, PartialEq)]
pub struct PartialPatternField {
    pub name: String,
    pub name_span: Spanned,
    pub pattern: Option<Match>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PartialPattern {
    pub name: Option<String>,
    pub fields: Vec<PartialPatternField>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Match {
    /// A binding identifier (`x` in `[x, y] = ...` or `~> =x`). The span covers the
    /// identifier itself, for go-to-definition and hover on pattern bindings.
    Identifier(String, Spanned),
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
