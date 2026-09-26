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

/// One entry of a block's annotation prefix (`:doc "..."`): a declared key name and the chain
/// producing its value. Annotations attach to the value the enclosing braces denote — the
/// closure for a function literal's body, the block's result for a chain block.
#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
    pub name: String,
    /// Span of the key name (`:doc`), for hover and go-to-definition.
    pub name_span: Spanned,
    /// Span starting at the annotation's `:`, for attaching leading trivia when formatting.
    pub span: Spanned,
    pub value: Chain,
}

/// The contents of a braced `{ … }`: an optional annotation prefix, then one or more
/// `|`-separated [`Branch`]es (a branchless block is a single branch with no consequence).
/// Every braced form is one — a [`Term::Block`], a [`Function`] body, and a string
/// interpolation hole ([`StrSegment::Hole`]) — and each introduces a scope.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    /// The annotation prefix (`:key value` before the first branch), attaching to the value the
    /// braces denote. An annotation-only block has no branches, and is identity-plus-attach.
    pub annotations: Vec<Annotation>,
    pub branches: Vec<Branch>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Branch {
    pub condition: Sequence,
    pub consequence: Option<Sequence>,
}

/// A sequence of [`Step`]s separated by the step separator (semicolon or newline, which are
/// synonyms) — the body of a [`Branch`], and a whole program (which is one sequence, and so is
/// branchless: branches require a block). Chain steps run left to right and the sequence
/// short-circuits to nil if any evaluates to nil; type-alias steps are transparent to that flow,
/// and scoped to the sequence's enclosing scope.
#[derive(Debug, Clone, PartialEq)]
pub struct Sequence {
    pub steps: Vec<Step>,
}

impl Sequence {
    /// The value-producing steps, skipping the (flow-transparent) type aliases.
    pub fn chains(&self) -> impl Iterator<Item = &Chain> {
        self.steps.iter().filter_map(Step::as_chain)
    }

    /// The value-producing steps, mutably.
    pub fn chains_mut(&mut self) -> impl Iterator<Item = &mut Chain> {
        self.steps.iter_mut().filter_map(Step::as_chain_mut)
    }

    /// The sole chain of a sequence whose *only* step is that chain. `None` when the sequence
    /// has several steps or declares a type alias — callers use this to decide whether a
    /// one-step shape applies, and an alias step means it does not.
    pub fn single_chain(&self) -> Option<&Chain> {
        match self.steps.as_slice() {
            [Step::Chain(chain)] => Some(chain),
            _ => None,
        }
    }

    /// Whether the sequence's value is statically its own input: its last step is that input
    /// itself (`~`, or an assertion-only step), or a match against it with no ripple — a match
    /// yields its scrutinee. Every step starts from the input, so earlier steps don't matter.
    pub fn yields_input(&self) -> bool {
        let Some(last) = self.chains().last() else {
            return false;
        };
        last.binding.is_none()
            && match last.terms.as_slice() {
                [] => true,
                [term] if term.is_bare_ripple() => true,
                [Term::Match(pattern)] => !pattern.contains_ripple(),
                [head, Term::Match(pattern)] => head.is_bare_ripple() && !pattern.contains_ripple(),
                _ => false,
            }
    }

    /// A sequence of chain steps, with no type aliases.
    pub fn from_chains(chains: impl IntoIterator<Item = Chain>) -> Self {
        Sequence {
            steps: chains.into_iter().map(Step::Chain).collect(),
        }
    }
}

/// One step of a [`Sequence`].
#[derive(Debug, Clone, PartialEq)]
pub enum Step {
    /// A type-alias declaration. Transparent to the value flow, and scoped to the sequence's
    /// enclosing scope — so a top-level alias is module-wide (and part of the module's exported
    /// type surface), while one inside a block or function body is local to it.
    TypeAlias {
        /// The alias name (`point` in `'point = ...`), or `None` for the module's
        /// nameless default-type marker (`' = ...` / `'<'t> = ...`).
        name: Option<String>,
        /// Span of the alias name (`'point` in `'point = ...`), for symbols/go-to-definition.
        name_span: Spanned,
        type_parameters: Vec<String>,
        type_definition: Type,
    },
    /// A value-producing step.
    Chain(Chain),
}

impl Step {
    pub fn as_chain(&self) -> Option<&Chain> {
        match self {
            Step::Chain(chain) => Some(chain),
            Step::TypeAlias { .. } => None,
        }
    }

    pub fn as_chain_mut(&mut self) -> Option<&mut Chain> {
        match self {
            Step::Chain(chain) => Some(chain),
            Step::TypeAlias { .. } => None,
        }
    }

    /// The span leading trivia (comments, blank lines) attaches to.
    pub fn span(&self) -> Spanned {
        match self {
            Step::Chain(chain) => chain.span,
            Step::TypeAlias { name_span, .. } => *name_span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Chain {
    /// A leading binding (`x = ...`): a pattern matched against the chain's *result*, exactly
    /// where a chain-final `=x` term sits — the two compile identically, and the field records
    /// which spelling was written so the formatter can render it back.
    pub binding: Option<Match>,
    /// Span of the binding pattern (the `x` in `x = ...`), for go-to-definition and symbols.
    /// `None` when the chain has no binding.
    pub binding_span: Spanned,
    /// Span starting at the chain's first character, for attaching leading comments/blank lines
    /// (trivia) to the chain during formatting. `None` for synthetic chains built by the parser.
    pub span: Spanned,
    /// Empty exactly for an assertion-only step (`//=` opening a sequence): the chain then
    /// evaluates to the block's input, as a bare `~` step would.
    pub terms: Vec<Term>,
    /// The `~>` separators between adjacent terms, one per gap, in order — so a parsed chain
    /// has one fewer of these than it has terms. Purely positional: the compiler never reads
    /// them, but the formatter needs them to re-attach the comments an author wrote in a gap.
    /// A chain the compiler synthesizes has none, having no source text to attach to.
    pub continuations: Vec<Continuation>,
    /// `//= P` assertions, each observing the chain's value at the position it was written
    /// (see [`Assertion::after`]). In debug builds that value is matched against the pattern
    /// and a mismatch aborts; release builds skip the check but still type-check the patterns,
    /// so types are identical across build modes. The value flows on unchanged either way — an
    /// asserted nil still short-circuits its sequence — and a pattern may not bind. Several
    /// share a position when `//=` lines are stacked: a leading `//=` continues the line
    /// above rather than opening a new step.
    pub assertions: Vec<Assertion>,
}

/// A `~>` separator's source positions (see [`Chain::continuations`]). Two are needed because
/// trivia attaches directionally: a comment written at the end of the previous term's line
/// *trails* [`end`](Continuation::end), while one on its own line *leads* the
/// [`pipe`](Continuation::pipe).
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Continuation {
    /// Empty span at the end of the preceding term.
    pub end: Spanned,
    /// Span of the `~>` token.
    pub pipe: Spanned,
}

/// The payload of a `//= P` assertion (see [`Chain::assertions`]).
#[derive(Debug, Clone, PartialEq)]
pub struct Assertion {
    pub pattern: Match,
    /// How many of the chain's terms precede the value this observes — the assertion ends a
    /// line, and observes what flows at the end of it. `terms.len()` is the chain's own result
    /// (the common case: an assertion trailing the last line of a step), and `0` the value the
    /// chain starts from, which is what an assertion-only step observes.
    ///
    /// A binding is applied *after* the assertions at `terms.len()`, so `x = e //= P` observes
    /// `e`, not the binding's `Ok`/nil verdict. The verdict is what the match spelling
    /// (`e ~> =P //= []`) observes, that being the chain's value there.
    pub after: usize,
    /// Whether the assertion sits on its own line (a leading `//=` continuing the line above)
    /// rather than trailing at the end of it; preserved by the formatter.
    pub own_line: bool,
    /// Span of the `//=` and its pattern, for diagnostics. It stops at the pattern's end, so a
    /// prose note — an ordinary trailing comment, `//= P // why` — falls outside it and attaches
    /// to the assertion as trivia rather than being swallowed by it.
    pub span: Spanned,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Literal(Literal),
    Tuple(Tuple),
    /// A string literal — single- or multi-line — as a sequence of literal-text and (single-line
    /// only) interpolation-hole segments. Lowered during compilation to a `Str` wrapping the
    /// concatenation of each segment's bytes; every hole must evaluate to a `Str`. Kept as a
    /// distinct term (rather than desugared to `Str[<bin>]` at parse time) so the formatter can
    /// round-trip the source form — including which delimiter style was written.
    String(StringStyle, Vec<StrSegment>),
    Match(Match),
    /// A braced expression `{ … }`: a new scope whose branches each start from the flowing value.
    Block(Block),
    Function(Function),
    /// A name: a variable, `$`, an import member, a builtin, a field of the flowing value, or
    /// `~` itself. Always the value it names — calling is written, and only [`Term::Apply`]
    /// does it — so a value flowing into a bare access is dropped.
    Access(Access),
    /// A juxtaposition application `f x` / `f [args]`: the access head applied to a single
    /// argument written after it, separated by horizontal space. The head covers everything an
    /// `Access` can name — a variable, `$`, import member, builtin, tail call (`^f [args]`,
    /// `^ [args]`), or a ripple (`~ [args]`, `~.f [args]`, `^~ arg`). This is the only thing
    /// that calls. The flowing value flows into the argument (so `f [~, 1]` and the piped
    /// spelling `f ~` work); for ripple heads it is consumed by the head instead, and the
    /// argument is evaluated without it.
    Apply(Access, Box<Term>),
    /// Spawn a process from a function (`@f x`, `@~ x`, `@[] { … } x`). The init is written like
    /// a call's argument, nil included (`@f []`) — the flowing value reaches it only through
    /// `~` (`x ~> @f ~`). `@~` spawns the flowing value itself, so its init is the argument.
    Spawn(Box<Term>, Option<Box<Term>>, Spanned),
    /// The current process, `@` — the pid of whoever is running this code. A bare `@`,
    /// since every spawn form glues its target to the sigil.
    Self_,
    /// Select operation. None means bare `!` (postfix form using chained value).
    /// Some(sources) means explicit sources like `![a, b]` or `![]` (discards chained value).
    /// The `Spanned` is the `!`, for hover (shows the received/awaited result type).
    Select(Option<Vec<Chain>>, Spanned),
    /// Sample a process's current state (`?p`): yields the
    /// target's state type, which the process type carries (inferred from spawn sites,
    /// or stated with a `?'s` clause). The access names the target (a variable or import
    /// member holding a pid); the `Spanned` is the `?`, for hover.
    State(Access, Spanned),
    Process(usize),
    /// A dialect invocation `%mod{ … }` (glued `{`): the raw brace content is handed at
    /// compile time to the function the module's `:dialect` annotation carries, and the
    /// expression tree it returns is spliced in place of this term. The flowing value is
    /// the expansion's input (like a block), referenced from the tree via `EFlow`.
    Dialect(Dialect),
}

/// A dialect invocation (see [`Term::Dialect`]).
#[derive(Debug, Clone, PartialEq)]
pub struct Dialect {
    /// Module path segments (`%ns/mod` → `["ns", "mod"]`).
    pub path: Vec<String>,
    /// The source text between the braces, verbatim (escapes unprocessed), so the formatter
    /// round-trips it exactly. The compiler unescapes `\{`/`\}` when expanding.
    pub raw: String,
    /// Span of the whole term (`%mod{…}`), for locating errors at the call site.
    pub span: Spanned,
    /// Span of the first content byte (just after the `{`), for mapping a dialect error's
    /// content offset back to a source position.
    pub content_span: Spanned,
}

impl Term {
    /// The source span of this term, when it is an addressable node (a reference,
    /// builtin, or tail call). Used by the compiler to locate errors and by the language
    /// server for hover/go-to-definition.
    pub fn span(&self) -> Option<SourceSpan> {
        match self {
            Term::Access(access) => access.span.get(),
            Term::Dialect(dialect) => dialect.span.get(),
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
    Integer(num_bigint::BigInt),
    Binary(BinaryLiteral),
}

/// A binary literal's bytes together with the layout written around them, which the formatter
/// re-emits: `<0a1b 2c>` is one row of two groups, and a literal spread over lines is one row
/// per line. Layout is presentation only — it is invisible to the compiler, which reads
/// [`bytes`](BinaryLiteral::bytes) — and the fields are private so it cannot drift from the
/// bytes it describes.
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryLiteral {
    bytes: Vec<u8>,
    /// The byte length of each whitespace-separated group, by row. Flattened, it sums to
    /// `bytes.len()`; a single entry is a literal written on one line.
    layout: Vec<Vec<usize>>,
}

impl BinaryLiteral {
    /// From the rows of groups as written. Empty groups and rows are dropped, so separator
    /// runs and blank lines collapse rather than rendering back out.
    pub fn new(rows: Vec<Vec<Vec<u8>>>) -> Self {
        let rows: Vec<Vec<Vec<u8>>> = rows
            .into_iter()
            .map(|row| row.into_iter().filter(|group| !group.is_empty()).collect())
            .filter(|row: &Vec<Vec<u8>>| !row.is_empty())
            .collect();
        Self {
            layout: rows
                .iter()
                .map(|row| row.iter().map(Vec::len).collect())
                .collect(),
            bytes: rows.concat().concat(),
        }
    }

    /// One undivided run on one line, for literals built by the compiler rather than
    /// written by hand.
    pub fn ungrouped(bytes: Vec<u8>) -> Self {
        let layout = if bytes.is_empty() {
            Vec::new()
        } else {
            vec![vec![bytes.len()]]
        };
        Self { bytes, layout }
    }

    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    /// How many lines the literal was written across. Zero for `<>`.
    pub fn row_count(&self) -> usize {
        self.layout.len()
    }

    /// The bytes of each group, row by row, in written order.
    pub fn rows(&self) -> Vec<Vec<&[u8]>> {
        let mut start = 0;
        let mut rows = Vec::with_capacity(self.layout.len());
        for row in &self.layout {
            let mut groups = Vec::with_capacity(row.len());
            for &len in row {
                groups.push(&self.bytes[start..start + len]);
                start += len;
            }
            rows.push(groups);
        }
        rows
    }

    /// The bytes of each group, in written order, ignoring where the rows divide.
    pub fn groups(&self) -> impl Iterator<Item = &[u8]> {
        self.layout.iter().flatten().scan(0, |start, &len| {
            let group = &self.bytes[*start..*start + len];
            *start += len;
            Some(group)
        })
    }
}

/// One piece of a string literal ([`Term::String`]).
#[derive(Debug, Clone, PartialEq)]
pub enum StrSegment {
    /// Literal text, with escapes already decoded (UTF-8 bytes).
    Text(Vec<u8>),
    /// An interpolation hole `{ … }`, parsed like a block body. Must evaluate to a `Str`.
    Hole(Block),
}

/// Which delimiter style a string literal was written with. Preserved so the formatter renders it
/// back in the same form rather than choosing by content (e.g. a single-line string containing a
/// newline stays single-line, with the newline escaped, rather than becoming a `"""` block).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringStyle {
    /// A `"…"` string.
    Single,
    /// A `"""…"""` string.
    Multi,
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
    /// Written with the punning spelling `(a, b)` / `Foo(a, b)`, where each entry is a *name*
    /// standing for both its own label and the value bound to it. The parser desugars a pun to
    /// the labeled reference it abbreviates (`a` → `a: a`), so the rest of the compiler sees an
    /// ordinary tuple; the flag records the spelling so the formatter can render it back — like
    /// [`Chain::binding`] and [`Term::String`]'s style. A punned tuple's fields are therefore
    /// always labeled, always [`FieldValue::Chain`], and always a lone [`Term::Access`].
    pub punned: bool,
}

#[derive(Debug, Clone, PartialEq)]
// `Chain` is the common, load-bearing variant (every non-spread field is one); `Spread` is rare, so
// boxing `Chain` just to even out the variant sizes would cost an allocation on the hot path.
#[allow(clippy::large_enum_variant)]
pub enum FieldValue {
    Chain(Chain),
    /// Spread: `None` for bare `...` (the flowing value); `Some` for a sourced spread
    /// (`...a`, `...a.b`, `...$conn`, `...$$x`, `...~.f`) — an access restricted by the
    /// parser to variable/parameter/ripple roots with field/index steps.
    Spread(Option<Access>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleField {
    pub name: Option<String>,
    /// Span of the field label (the `triple` in `triple: ...`), for go-to-definition onto a
    /// module's exported members. Absent for unnamed fields and spreads.
    pub name_span: Spanned,
    /// Span starting at the field's first character, for attaching leading comments/blank lines
    /// (trivia) to the field during formatting. `None` for synthetic fields built by the parser.
    pub span: Spanned,
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

/// The written spelling of a parameter reference at `depth`: `0` → `$`, `1` → `$$`, ….
/// Every depth-driven rendering of the sigil run (formatting, labels, diagnostics) goes
/// through here, so the surface spelling lives in one place; only fixed depth-0 display
/// strings (`"$"` in error text and hover labels) are written literally.
pub fn parameter_sigils(depth: usize) -> String {
    "$".repeat(depth + 1)
}

#[derive(Debug, Clone, PartialEq)]
pub enum AccessSource {
    /// Identifier like `foo`
    Identifier(String),
    /// Function parameter: `$` is the enclosing function's own parameter (`depth` 0); each
    /// extra glued sigil reaches one function further out (`$$` is the next enclosing
    /// function's parameter, depth 1, and so on). Outer parameters are captured by value at
    /// closure creation — per accessed path, like any capture — so `$$x` sees the enclosing
    /// activation's argument as it was when the closure was built.
    Parameter { depth: usize },
    /// Ripple `~` - references the piped value
    Ripple,
    /// Import like `%num` or `%mathx/vec`
    Import(Vec<String>),
    /// Builtin like `__integer_add__` — a globally-resolved callable, looked up in the builtin
    /// registry rather than the lexical scope.
    Builtin(String),
    /// Tail call (`^`, `^f`, `^f.field`): `None` recurses into the current function, `Some(name)`
    /// tail-calls `name`. Compiled with the tail-call instruction (TCO), not a normal call.
    TailCall(Option<String>),
    /// Ripple tail call (`^~ x`): tail-calls the flowing value, which is the callee rather than
    /// the argument — the flowing-value analogue of `^`/`^f`. Its argument is written like any
    /// other, nil included (`^~ []`).
    TailCallRipple,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Access {
    pub source: Option<AccessSource>,
    pub accessors: Vec<AccessPath>,
    /// Explicit type arguments (`f<'int, 'bin>`, glued `<`): instantiate the accessed
    /// callable's declared type parameters positionally, pinning them before inference
    /// (a prefix is allowed — the rest stay inferred). Purely static: generics are
    /// erased, so instantiation only narrows the type the use site sees.
    pub type_arguments: Vec<Type>,
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

#[derive(Debug, Clone, PartialEq)]
pub enum AccessPath {
    Field(String),
    Index(usize),
    /// Annotation retrieval (`x:key`, glued): yields the annotation value or nil. An
    /// optional expected shape (`x:key<'t>`, the checked form) makes the retrieval
    /// total: legal on any carrier, guarded by a runtime structural test — an entry
    /// outside the shape answers nil, exactly as a failed `=('t & v)` match does.
    Annotation(String, Option<Type>),
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

/// The root of a pin pattern's target: an existing variable, or a function parameter —
/// `depth` counts enclosing functions exactly as in `AccessSource::Parameter` (`^$x` is
/// depth 0, `&$$x` depth 1, …).
#[derive(Debug, Clone, PartialEq)]
pub enum PinRoot {
    Variable(String),
    Parameter { depth: usize },
}

/// A pin pattern's target: an access path rooted at a variable or the enclosing function's
/// parameter (`^name.field`, `^$x.0`). `accessor_spans` parallels `accessors` and `base_span`
/// covers the root — as in `Access` — so the language server can hover/navigate each component
/// separately; `span` covers the whole target after the `^`.
#[derive(Debug, Clone, PartialEq)]
pub struct PinTarget {
    pub root: PinRoot,
    pub accessors: Vec<AccessPath>,
    pub accessor_spans: Vec<Spanned>,
    pub base_span: Spanned,
    pub span: Spanned,
}

/// The reserved binder name a ripple (`~`) is analysed under. Not a valid identifier, so it
/// never collides with a user binding.
pub const RIPPLE: &str = "~";

#[derive(Debug, Clone, PartialEq)]
pub enum Match {
    /// A binding identifier (`x` in `[x, y] = ...` or `~> =x`). The span covers the
    /// identifier itself, for go-to-definition and hover on pattern bindings.
    Identifier(String, Spanned),
    Literal(Literal),
    /// A string-literal pattern (`="admin"`, or a `"""…"""` block). Matches the desugared
    /// `Str[<bin>]` value; kept distinct (rather than desugared to a `Tuple` pattern at parse time)
    /// so the formatter renders it back as a string. No interpolation — patterns are text-only.
    String(StringStyle, Vec<u8>),
    Tuple(MatchTuple),
    Partial(PartialPattern),
    /// Bind all named fields (`*`). An optional tuple name (`Config*`) additionally
    /// requires the value to be named, mirroring a named partial pattern.
    Star(Option<String>),
    Placeholder,
    /// The ripple `~`: the match yields the value here in place of its scrutinee, so
    /// `[42] ~> =[~]` flows `42` on. Repeated, the values at each must be equal, as for a
    /// repeated binder — which is how analysis treats it, under [`RIPPLE`].
    Ripple,
    /// A pin against an existing value: `^name`, `^name.field`, `^$`, `^$x.0` — matches only
    /// if the value equals the referenced value. The target is an access path rooted at a
    /// variable or the enclosing function's parameter (with the parameter's usual glued first
    /// accessor, as in `$x`); annotation accessors are not part of a pin target.
    Pin(PinTarget),
    Type(Type),
    /// An alternation of patterns (`(p | q | …)`): matches if any alternative matches. Every
    /// alternative must bind the same set of variables (so the body sees them regardless of which
    /// matched).
    Or(Vec<Match>),
    /// A conjunction of patterns (`(p & q & …)`): matches when every conjunct does, and binds
    /// what each binds. A binder conjunct captures the whole value at the type the other
    /// conjuncts narrowed it to, so `('int & x)` binds `x: 'int` — the as-pattern is simply a
    /// conjunction with a binder. Binds tighter than an alternation (`(A & x | B & x)`), and
    /// nested conjunctions are flattened by the parser.
    And(Vec<Match>),
    /// A negated pattern `\P`: matches exactly when `P` does not. `P` may not bind (nothing
    /// matched, so there is nothing to bind), but may pin, test types, and nest anywhere a pattern
    /// can (`=A[b: \'int]`). `(\[] & x)` binds `x` while requiring it to be non-nil.
    Not(Box<Match>),
}

impl Match {
    /// Whether a ripple (`~`) appears anywhere in the pattern.
    pub fn contains_ripple(&self) -> bool {
        match self {
            Match::Ripple => true,
            Match::Tuple(tuple) => tuple
                .fields
                .iter()
                .any(|field| field.pattern.contains_ripple()),
            Match::Partial(partial) => partial
                .fields
                .iter()
                .any(|field| field.pattern.as_ref().is_some_and(Match::contains_ripple)),
            Match::Or(parts) | Match::And(parts) => parts.iter().any(Match::contains_ripple),
            Match::Not(inner) => inner.contains_ripple(),
            Match::Identifier(..)
            | Match::Literal(_)
            | Match::String(..)
            | Match::Star(_)
            | Match::Placeholder
            | Match::Pin(_)
            | Match::Type(_) => false,
        }
    }
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
    /// A type intersection `'t & 'u`: values satisfying *every* member. Binds tighter than a
    /// union. Resolves by intersecting the members (so `'int & 'bin` is `never`); when matched,
    /// each member is checked separately so partial-type constraints compose soundly.
    Intersection(Vec<Type>),
    Identifier {
        name: String,
        arguments: Vec<Type>,
    },
    Cycle(Option<usize>),
    Process(ProcessType),
    Resource(String),
    /// The top type `_`, which every value belongs to.
    Top,
    /// A type reached through a module's type namespace: `'%mod` (the module's default
    /// type, `member: None`) or `'%mod.name` (a named type). `arguments` are type
    /// arguments applied to the referenced (parameterised) type, e.g. `'%list<'int>`.
    ModuleType {
        module: Vec<String>,
        member: Option<String>,
        arguments: Vec<Type>,
    },
    /// The enclosing module's own default type, written as a bare `'` (or `'<args>` to apply
    /// type arguments). The type-level counterpart of how other modules reach it as `'%mod`.
    SelfDefault {
        arguments: Vec<Type>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcessType {
    pub receive_type: Option<Box<Type>>,
    pub return_type: Option<Box<Type>>,
    /// The `?'s` clause: what `?p` samples. Omitted = sampling not granted.
    pub state_type: Option<Box<Type>>,
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
        /// Span starting at the field's first character, for attaching leading comments/blank
        /// lines (trivia) to the field during formatting, exactly as [`TupleField::span`] does for
        /// a value tuple's fields. `None` for fields the parser synthesises.
        span: Spanned,
        name: Option<String>,
        /// Written `(name): type` — a caller may omit this label, and the argument's field
        /// adopts it positionally. A calling convention, so it is legal only in a function
        /// type's parameter tuple and is recorded on that `Callable`, never on the tuple
        /// type: tuple types intern structurally, so a mark stored there would be shared
        /// by every identically shaped tuple in the program.
        omittable: bool,
        /// `None` when the entry gave no type — `name`, `(name)`, `name = v`, `(name) = v`.
        /// Such an entry *decorates* a field a spread already brought in, adjusting only
        /// its label and default and leaving its type and position alone; an entry that
        /// states a type instead *defines* the field, replacing any inherited one. A
        /// decorator with nothing to decorate is an error.
        type_def: Option<Type>,
        /// Written `name: type = value` — the field may be omitted by a call argument,
        /// which fills it with this value. Legal only in a function literal's parameter
        /// spelling, where it lowers to a `:defaults` annotation on the closure; the
        /// default belongs to the *function*, never to the type, so two functions with
        /// the same parameter type may declare different ones.
        /// Boxed: a default is rare, and inlining a `Chain` here would bloat every field.
        default: Option<Box<Chain>>,
    },
    Spread {
        /// As on [`FieldType::Field`]: where the entry starts, for trivia attachment.
        span: Spanned,
        identifier: Option<String>,
        type_arguments: Vec<Type>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub input: Box<Type>,
    pub output: Box<Type>,
    /// The `!'c` clause: what the function receives while running (widens the
    /// caller's/spawner's receive type). Omitted = receives nothing.
    pub receive: Option<Box<Type>>,
    /// The `?'d` clause: the states a spawn of it moves through, *beyond* the
    /// parameter (which is included implicitly: states = input | d). Omitted =
    /// sampling not granted through this type.
    pub states: Option<Box<Type>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionType {
    pub types: Vec<Type>,
}
