//! Expansion: converting the `'%meta.expr` value a dialect function returns into an AST term,
//! spliced at the invocation site and type-checked there like ordinary code.
//!
//! The IR is recognized structurally (`std/meta.qv` is its canonical Quiver-side definition,
//! and documents each node). Plain data splices as itself — integers, binaries, `Str`s, and a
//! bare `Nil` become the corresponding literals — and named tuples cover the rest: tuple
//! construction (`Tuple`, with `Labeled` and `Spread` fields), calls (`Apply`, `Pipe`), names
//! (`Ripple`, `Param`, `Local`, `Member`, `Get`), `Match`, `Block`, and `Function` literals, with
//! patterns (`'%meta.pattern`) and types (`'%meta.type`) in their positions.
//!
//! One node reaches the caller. `Unquote[offset, length]` is a span of the content, parsed as one
//! host chain and spliced in the *caller's* scope: bind-once, every hole
//! evaluates exactly once, in content order, with the dialect term's flowing value as its chain
//! input, and provably pure holes splice in place instead. An `Apply` of a module's member to an
//! `Unquote` is folded into the span's own hole, so the span is spliced in **argument
//! position**, where the callee's parameter type reaches it — as any `Apply`'s argument is.
//!
//! Everything else is the expansion's own, which is what keeps it hygienic: its bindings carry
//! a prefix numbered per expansion, its functions' type parameters a suffix no written name
//! can, `Param` reaches only its own functions, and tuple names are uppercase as written ones
//! are.

use std::cell::{Cell, RefCell};

use crate::ast;
use crate::compiler::Error;
use crate::parser::SourceSpan;
use quiver_core::{
    builtins::{BuiltinContext, BuiltinRegistry, Completion, Purity, TypeSpec},
    bytecode::Constant,
    effects::Effect,
    error::Error as CoreError,
    executor::Executor,
    program::Program,
    types::TypeLookup,
    value::{Binary, Value},
};

/// The scope binding an expansion reads the flowing value through. Unspellable in source
/// (identifiers can't start with `~`), so it can never collide with or capture a user
/// binding.
pub const RIPPLE_BINDING: &str = "~dialect-ripple";

/// Prefix of the scope bindings holding evaluated `Unquote` holes (`~dialect-hole-0`,
/// …). Unspellable, like [`RIPPLE_BINDING`].
pub const HOLE_BINDING_PREFIX: &str = "~dialect-hole-";

/// The prefix of the scope bindings the `expansion`-th expansion's own `Local`/`Bind` names
/// become (`~meta-3-v`). Unspellable, like [`RIPPLE_BINDING`], so generated code can neither
/// capture a caller's binding nor be captured by one, and numbered, so no two expansions share
/// one.
pub fn local_prefix(expansion: usize) -> String {
    format!("~meta-{expansion}-")
}

/// The suffix a generated function's own type parameter carries (`'d#meta`), which no written
/// name can, so a caller's type spliced into it never resolves to one. Displayed types drop
/// it, as they drop the compiler's other `#` uniquifiers.
const TYPE_PARAMETER_SUFFIX: &str = "#meta";

/// Names of the prefix-parse callbacks passed to dialect functions in the context
/// record. Registered only in the expansion executor's registry (see
/// [`register_callbacks`]) — at runtime a dialect function receives whatever callables
/// the caller passes (e.g. stubs in tests).
pub const TERM_CALLBACK: &str = "__dialect_term__";
pub const CHAIN_CALLBACK: &str = "__dialect_chain__";

/// Unescape dialect content: `\{`, `\}`, and `\"` outside string literals become literal
/// braces/quotes (`\"` lets content carry an unpaired quote without opening string mode).
/// Returns the content plus the content-relative positions of the unescaped characters, so
/// an error offset into the content can be mapped back to a position in the raw source text.
pub fn unescape_content(raw: &str) -> (String, Vec<usize>) {
    let bytes = raw.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut escapes = Vec::new();
    let mut in_string = false;
    let mut i = 0;
    while i < bytes.len() {
        match (in_string, bytes[i]) {
            (true, b'\\') => {
                out.push(bytes[i]);
                if let Some(&next) = bytes.get(i + 1) {
                    out.push(next);
                    i += 1;
                }
            }
            (true, b'"') => {
                in_string = false;
                out.push(bytes[i]);
            }
            (false, b'\\') if matches!(bytes.get(i + 1).copied(), Some(b'{' | b'}' | b'"')) => {
                escapes.push(out.len());
                out.push(bytes[i + 1]);
                i += 1;
            }
            (false, b'"') => {
                in_string = true;
                out.push(bytes[i]);
            }
            (_, b) => out.push(b),
        }
        i += 1;
    }
    let content = String::from_utf8(out).expect("dialect content is a subslice of UTF-8 source");
    (content, escapes)
}

/// Map an offset into the (unescaped) content to a source (line, column), via the
/// invocation's content span and the recorded escape positions (each shifts subsequent
/// content bytes one byte left of their raw-source position).
pub fn content_position(
    dialect: &ast::Dialect,
    escapes: &[usize],
    offset: usize,
) -> Option<(usize, usize)> {
    let base = dialect.content_span.get()?;
    let raw_offset =
        (offset + escapes.iter().filter(|&&e| e < offset).count()).min(dialect.raw.len());
    let prefix = &dialect.raw.as_bytes()[..raw_offset];
    let newlines = prefix.iter().filter(|&&b| b == b'\n').count();
    if newlines == 0 {
        Some((base.line, base.column + raw_offset))
    } else {
        let last_newline = prefix.iter().rposition(|&b| b == b'\n').unwrap();
        Some((base.line + newlines, raw_offset - last_newline))
    }
}

/// The source span of `length` bytes at an offset into the (unescaped) content, as
/// [`content_position`] maps it.
pub fn content_span(
    dialect: &ast::Dialect,
    escapes: &[usize],
    offset: usize,
    length: usize,
) -> Option<SourceSpan> {
    let base = dialect.content_span.get()?;
    let (line, column) = content_position(dialect, escapes, offset)?;
    let to_raw = |offset: usize| {
        (offset + escapes.iter().filter(|&&e| e < offset).count()).min(dialect.raw.len())
    };
    Some(SourceSpan {
        offset: base.offset + to_raw(offset),
        line,
        column,
        length: to_raw(offset + length) - to_raw(offset),
    })
}

/// An `Unquote` hole collected during splicing: its dedupe key — the same span spliced
/// twice shares one binding, so it evaluates once — and the chain that evaluates it into
/// its `~dialect-hole-N` binding.
pub struct Hole {
    /// `(offset, length)` into the unescaped content.
    span: (usize, usize),
    /// The member the span is passed to, when it is folded into one: one span reaching two
    /// members is two holes.
    callee: Option<CalleeKey>,
    terms: Vec<ast::Term>,
}

/// What identifies a callee among an expansion's holes: its access, apart from where it is
/// written, so that two `Unquote`s of one span folded into one callee evaluate once.
#[derive(PartialEq)]
struct CalleeKey {
    source: Option<ast::AccessSource>,
    accessors: Vec<ast::AccessPath>,
    type_arguments: Vec<ast::Type>,
}

/// An `Apply`'s callee: the access to apply.
struct Callee {
    access: ast::Access,
}

impl Callee {
    fn new(access: ast::Access) -> Self {
        Callee { access }
    }

    fn key(&self) -> CalleeKey {
        CalleeKey {
            source: self.access.source.clone(),
            accessors: self.access.accessors.clone(),
            type_arguments: self.access.type_arguments.clone(),
        }
    }

    /// Whether the callee is a module's member, which is in scope wherever the expansion is.
    fn is_member(&self) -> bool {
        matches!(self.access.source, Some(ast::AccessSource::Import(..)))
    }

    /// The callee applied to the argument `chain` leaves: `f arg` when the argument is one
    /// term, in argument position (where the callee's parameter type reaches it — omittable
    /// labels, defaults, an inferred parameter — as at a written call), and `arg ~> f ~`
    /// otherwise.
    fn apply(self, mut argument: ast::Chain) -> ast::Chain {
        if argument.binding.is_none()
            && let [term] = argument.terms.as_slice()
            && !matches!(term, ast::Term::Match(_))
        {
            let term = argument.terms.pop().expect("one term");
            return term_chain(ast::Term::Apply(self.access, Box::new(term)));
        }
        argument.terms.push(self.applied());
        argument
    }

    /// `f ~`: the callee applied to the chain's flowing value — the piped spelling of
    /// `f arg`, and the only thing that calls.
    fn applied(self) -> ast::Term {
        let span = self.access.span;
        ast::Term::Apply(
            self.access,
            Box::new(ast::Term::Access(ast::Access {
                source: Some(ast::AccessSource::Ripple),
                accessors: vec![],
                accessor_spans: vec![],
                type_arguments: vec![],
                base_span: span,
                span,
            })),
        )
    }
}

/// Wrap an expansion chain as a single term: a block binding the flowing value to
/// [`RIPPLE_BINDING`], then evaluating each collected hole **once**, in content order,
/// into its `~dialect-hole-N` binding, then running the expansion —
/// `{ =~dialect-ripple, &~dialect-ripple {hole} =~dialect-hole-0, …, <expansion> }`.
/// The block receives the dialect term's flowing value as its parameter, so `Ripple`
/// references and hole inputs resolve to it. (The binders are bare bindings, which the
/// type checker knows are irrefutable, so a nil-valued hole binds without widening the
/// block's type or short-circuiting.)
pub fn wrap_expansion(mut holes: Vec<Hole>, expansion: ast::Chain) -> ast::Term {
    let binder = ast::Chain {
        binding: None,
        binding_span: ast::Spanned::default(),
        span: ast::Spanned::default(),
        continuations: Vec::new(),
        terms: vec![ast::Term::Match(ast::Match::Identifier(
            RIPPLE_BINDING.to_string(),
            ast::Spanned::default(),
        ))],
        assertions: Vec::new(),
    };
    let mut chains = vec![binder];
    // In content order; a stable sort, so holes over one span keep the order they were met in.
    holes.sort_by_key(|hole| hole.span);
    chains.extend(holes.into_iter().map(|hole| ast::Chain {
        binding: None,
        binding_span: ast::Spanned::default(),
        span: ast::Spanned::default(),
        continuations: Vec::new(),
        assertions: Vec::new(),
        terms: hole.terms,
    }));
    chains.push(expansion);
    ast::Term::Block(ast::Block {
        annotations: vec![],
        branches: vec![ast::Branch {
            condition: ast::Sequence::from_chains(chains),
            consequence: None,
        }],
    })
}

/// A dialect invocation's brace content, which `Unquote` spans index into.
pub struct Content<'a> {
    /// The invocation, for mapping `Unquote` span offsets to file positions.
    pub dialect: &'a ast::Dialect,
    /// The unescaped brace content, with the escape positions recorded by
    /// [`unescape_content`].
    pub text: &'a str,
    pub escapes: &'a [usize],
}

/// The dialect an expansion is from.
pub struct Expander {
    /// As written at the invocation, for messages: `%json`.
    pub name: String,
    /// The package of the dialect's module, which the modules its expansion names resolve in.
    pub package: crate::resolver::PackageId,
    /// The invocation's span, stamped onto synthesized references (so unresolved-name and
    /// ill-typed-call errors point at it).
    pub span: ast::Spanned,
}

impl Expander {
    pub fn error(&self, message: &str) -> Error {
        Error::DialectFailed {
            module: self.name.clone(),
            message: message.to_string(),
        }
    }
}

/// Converts `'%meta.expr` values into AST chains for one dialect invocation.
pub struct Splicer<'a> {
    program: &'a Program,
    expander: &'a Expander,
    /// The invocation's content, which `Unquote` reads.
    content: Content<'a>,
    /// The prefix of this expansion's own bindings (see [`local_prefix`]).
    locals: String,
    /// `Unquote` holes collected while walking the returned IR (bind-once).
    holes: RefCell<Vec<Hole>>,
    /// The type parameters of the expansion's function literals enclosing the node being
    /// spliced, innermost last: the parameters `Param[depth]` may reach, and the names
    /// `Var[name]` may.
    functions: RefCell<Vec<Vec<String>>>,
    /// How many of the expansion's own blocks and functions enclose the node being spliced:
    /// inside one, `$`, `~` and type parameters mean the expansion's, not the caller's.
    scopes: Cell<usize>,
}

/// One entry of [`Splicer::value_to_chain`]'s work stack. `Visit` expands a value; the other
/// two consume the chains their children left behind, which is what makes the walk a loop
/// rather than a recursion.
enum Step<'v> {
    /// Expand this value, which sits `depth` levels below the expansion's root.
    Visit(&'v Value, usize),
    /// Build a tuple from its slots, taking each `Chain` slot's value from the chains left
    /// behind, in order.
    Tuple {
        name: ast::TupleName,
        slots: Vec<Slot>,
    },
    /// Append a call to the chain the argument left.
    Apply(Callee),
}

/// One field of a `Tuple` being built: a labelled or positional value, whose chain its visit
/// leaves behind, or a spread of an access path.
enum Slot {
    Chain(Option<String>),
    Spread(ast::Access),
}

/// How deeply a dialect's expansion may nest.
///
/// This walk is a loop, so it is not the one at risk — the bound is on what the expansion
/// then costs everyone downstream. A dialect's list-shaped output is a `Cons` spine with one
/// level per element, and each level mints its own tuple *type*: measured, a `%list{ … }` of
/// n elements adds n type-table entries and costs quadratic compile time (n=100 is +7ms,
/// n=1000 is +1.4s, n=2000 is +6.7s). The compiler's own walk over that AST recurses, and
/// overflows its stack somewhere past 2000.
///
/// 256 sits an order of magnitude under the point where the stack goes, and at the point
/// where the compile cost stops being something a caller would accept without knowing why.
/// A flat literal is unaffected however wide it is: `[1, …, 1000]` is one level and one type.
const MAX_EMISSION_DEPTH: usize = 256;

impl<'a> Splicer<'a> {
    /// A splicer for one expansion of `content`, its own bindings prefixed `locals`.
    pub fn new(
        program: &'a Program,
        expander: &'a Expander,
        content: Content<'a>,
        locals: String,
    ) -> Self {
        Splicer {
            program,
            expander,
            content,
            locals,
            holes: RefCell::new(Vec::new()),
            functions: RefCell::new(Vec::new()),
            scopes: Cell::new(0),
        }
    }

    /// The `Unquote` holes collected while splicing (for [`wrap_expansion`]).
    pub fn finish(self) -> Vec<Hole> {
        self.holes.into_inner()
    }

    /// Convert the `'expr` value a dialect function returned into a chain to splice at the
    /// invocation site.
    pub fn value_to_chain(&self, value: &Value) -> Result<ast::Chain, Error> {
        self.chain_at(value, 0)
    }

    /// The chain for an expression `depth` levels below the expansion's root.
    fn chain_at(&self, value: &Value, depth: usize) -> Result<ast::Chain, Error> {
        // Driven by an explicit stack rather than by recursion. A dialect's expansion is as
        // deep as its output is long — `%list{ … }` returns a `Cons` spine with one level
        // per element — so recursing per node would put the native stack, not the language,
        // in charge of how long a literal may be. The nodes that hold sequences, patterns
        // and types (`Block`, `Function`, `Match`) recurse, as the nesting written into code
        // is bounded by the same depth.
        let mut steps = vec![Step::Visit(value, depth)];
        let mut done: Vec<ast::Chain> = Vec::new();
        while let Some(step) = steps.pop() {
            match step {
                Step::Visit(value, depth) => {
                    self.check_depth(depth)?;
                    self.visit(value, depth, &mut steps, &mut done)?
                }
                Step::Tuple { name, slots } => {
                    // The fields were pushed in order, so their chains are the last results,
                    // already in the right order.
                    let chains = slots
                        .iter()
                        .filter(|slot| matches!(slot, Slot::Chain(_)))
                        .count();
                    let mut chains = done.split_off(done.len() - chains).into_iter();
                    let fields = slots
                        .into_iter()
                        .map(|slot| {
                            let (name, value) = match slot {
                                Slot::Chain(name) => (
                                    name,
                                    ast::FieldValue::Chain(
                                        chains.next().expect("a chain per chain slot"),
                                    ),
                                ),
                                Slot::Spread(access) => {
                                    (None, ast::FieldValue::Spread(Some(access)))
                                }
                            };
                            ast::TupleField {
                                name,
                                name_span: ast::Spanned::default(),
                                span: ast::Spanned::default(),
                                value,
                            }
                        })
                        .collect();
                    done.push(term_chain(ast::Term::Tuple(ast::Tuple {
                        name,
                        fields,
                        span: self.expander.span,
                        // A dialect emits tuples structurally; punning is a source spelling only.
                        punned: false,
                    })));
                }
                Step::Apply(callee) => {
                    let argument = done.pop().expect("an Apply's argument leaves its chain");
                    done.push(callee.apply(argument));
                }
            }
        }
        Ok(done.pop().expect("the root value leaves its chain"))
    }

    /// One node of [`value_to_chain`]'s walk: either finish a chain onto `done`, or push the
    /// steps that will.
    fn visit<'v>(
        &self,
        value: &'v Value,
        depth: usize,
        steps: &mut Vec<Step<'v>>,
        done: &mut Vec<ast::Chain>,
    ) -> Result<(), Error> {
        // Plain data splices as itself: integers and binaries become literals.
        match value {
            Value::Int(int) => {
                done.push(term_chain(ast::Term::Literal(ast::Literal::Integer(
                    num_bigint::BigInt::from(*int),
                ))));
                return Ok(());
            }
            Value::BigInt(big) => {
                done.push(term_chain(ast::Term::Literal(ast::Literal::Integer(
                    (**big).clone(),
                ))));
                return Ok(());
            }
            Value::Binary(binary) => {
                let bytes = binary_bytes(self.program, binary)
                    .ok_or_else(|| self.error("returned an unreadable binary"))?;
                done.push(term_chain(ast::Term::Literal(ast::Literal::Binary(
                    ast::BinaryLiteral::ungrouped(bytes),
                ))));
                return Ok(());
            }
            _ => {}
        }
        let (name, fields) = self.expect_tuple(value)?;
        match name {
            // A `Str` value is a string literal, and a bare `Nil` the named empty tuple —
            // both splice as themselves (identical to spelling them out with `Tuple`).
            "Str" => {
                let bytes = self.str_bytes(value, "Str")?;
                done.push(term_chain(ast::Term::String(
                    ast::StringStyle::Single,
                    vec![ast::StrSegment::Text(bytes)],
                )));
                Ok(())
            }
            "Nil" if fields.is_empty() => {
                done.push(term_chain(ast::Term::Tuple(ast::Tuple {
                    name: ast::TupleName::Named("Nil".to_string()),
                    fields: vec![],
                    span: self.expander.span,
                    punned: false,
                })));
                Ok(())
            }
            "Ripple" if fields.is_empty() => {
                done.push(term_chain(ast::Term::Access(
                    self.access_from(ast::AccessSource::Ripple),
                )));
                Ok(())
            }
            "Param" | "Local" | "Member" => {
                done.push(term_chain(ast::Term::Access(self.access(value, depth)?)));
                Ok(())
            }
            "Get" => {
                let of = self.field(value, "Get", &["of"], 0)?;
                let accessor = self.accessor(self.field(value, "Get", &["field"], 1)?)?;
                // A field of a name is read through the name's own accessors, which is what
                // a spread or a callee needs; any other value is read as it flows on.
                if self.is_access(of, depth + 1)? {
                    let mut access = self.access(of, depth + 1)?;
                    access.accessors.push(accessor);
                    access.accessor_spans.push(ast::Spanned::default());
                    done.push(term_chain(ast::Term::Access(access)));
                } else {
                    let mut chain = self.chain_at(of, depth + 1)?;
                    let mut access = self.access_from(ast::AccessSource::Ripple);
                    access.accessors.push(accessor);
                    access.accessor_spans.push(ast::Spanned::default());
                    chain.terms.push(ast::Term::Access(access));
                    done.push(chain);
                }
                Ok(())
            }
            "Apply" => {
                let callee = Callee::new(
                    self.access(self.field(value, "Apply", &["callee"], 0)?, depth + 1)?,
                );
                let arg = self.field(value, "Apply", &["arg"], 1)?;
                // A module member's `Unquote` argument folds into the call: the span is spliced
                // as the member's argument rather than bound first and passed in, so it
                // compiles against the parameter type. (A member is in scope wherever the span
                // is evaluated; the expansion's own names are not yet.)
                if callee.is_member()
                    && let Some((offset, length)) = self.as_unquote(arg)?
                {
                    done.push(self.unquote(offset, length, Some(callee))?);
                    return Ok(());
                }
                // The apply is queued first so it runs after the argument it consumes.
                steps.push(Step::Apply(callee));
                steps.push(Step::Visit(arg, depth + 1));
                Ok(())
            }
            "Pipe" => {
                // `value ~> into`: the chain the value left, carried on by `into`'s terms.
                let mut chain =
                    self.chain_at(self.field(value, "Pipe", &["value"], 0)?, depth + 1)?;
                let into = self.chain_at(self.field(value, "Pipe", &["into"], 1)?, depth + 1)?;
                chain.terms.extend(into.terms);
                done.push(chain);
                Ok(())
            }
            "Match" => {
                let scrutinee = self.field(value, "Match", &["value"], 0)?;
                let pattern =
                    self.pattern(self.field(value, "Match", &["pattern"], 1)?, depth + 1)?;
                // A match of the flowing value is written alone, `=P`, as it is matched.
                let chain = if self.is_ripple(scrutinee)? {
                    term_chain(ast::Term::Match(pattern))
                } else {
                    let mut chain = self.chain_at(scrutinee, depth + 1)?;
                    chain.terms.push(ast::Term::Match(pattern));
                    chain
                };
                done.push(chain);
                Ok(())
            }
            "Block" => {
                done.push(term_chain(ast::Term::Block(self.block(value, depth)?)));
                Ok(())
            }
            "Function" => {
                done.push(term_chain(ast::Term::Function(
                    self.function(value, depth)?,
                )));
                Ok(())
            }
            "Unquote" => {
                let (offset, length) = self.unquote_span(value)?;
                done.push(self.unquote(offset, length, None)?);
                Ok(())
            }
            "Tuple" => {
                let name = self.tuple_name(self.field(value, "Tuple", &["name"], 0)?)?;
                let entries =
                    self.tuple_field_entries(self.field(value, "Tuple", &["fields"], 1)?)?;
                let mut slots = Vec::with_capacity(entries.len());
                let mut values = Vec::new();
                for (label, entry) in entries {
                    match self.spread_source(entry)? {
                        Some(source) => {
                            if label.is_some() {
                                return Err(self.error("returned a labelled Spread"));
                            }
                            slots.push(Slot::Spread(self.access(source, depth + 1)?));
                        }
                        None => {
                            slots.push(Slot::Chain(label));
                            values.push(entry);
                        }
                    }
                }
                steps.push(Step::Tuple { name, slots });
                // Reversed, so the stack hands them back in field order.
                steps.extend(
                    values
                        .into_iter()
                        .rev()
                        .map(|value| Step::Visit(value, depth + 1)),
                );
                Ok(())
            }
            "Labeled" => {
                Err(self
                    .error("returned Labeled outside a Tuple's fields (it wraps a labeled field)"))
            }
            "" if fields.is_empty() => {
                Err(self.error("returned nil where a '%meta.expr was expected"))
            }
            "Hole" => Err(self.hole_returned()),
            other => Err(self.error(&format!("returned {other}[…], which is not a '%meta.expr"))),
        }
    }

    /// Splice an `Unquote[offset, length]` — a span of the content parsed as one host
    /// chain, evaluated in the caller's scope with the dialect term's flowing value as
    /// its input. Semantics is bind-once: every hole evaluates exactly once, in content
    /// order, before the expansion's own structure. Provably pure single-term holes —
    /// see [`splices_in_place`] — splice there instead; anything else becomes a
    /// `~dialect-hole-N` binding, deduplicated by span.
    ///
    /// `callee` is the member of an `Apply` whose argument this span is: it is appended to
    /// the span's own chain, so the span sits in argument position (typed by the callee's
    /// parameter) and the call joins the span inside the binding — one evaluation of both.
    fn unquote(
        &self,
        offset: usize,
        length: usize,
        callee: Option<Callee>,
    ) -> Result<ast::Chain, Error> {
        let content = &self.content;
        let text = (length > 0)
            .then(|| content.text.get(offset..offset + length))
            .flatten()
            .ok_or_else(|| {
                self.error(&format!(
                    "returned an Unquote span [{offset}, {length}] outside its content"
                ))
            })?;
        let mut chain = crate::parser::parse_chain_exact(text).map_err(|parse_error| {
            let error = self.error(&format!(
                "returned an Unquote span that is not a single block: {}",
                parse_error.kind
            ));
            let (error_offset, error_length) = parse_error
                .span
                .map_or((0, length), |span| (span.offset, span.length));
            match content_span(
                content.dialect,
                content.escapes,
                offset + error_offset,
                error_length,
            ) {
                Some(span) => Error::Positioned {
                    span,
                    error: Box::new(error),
                },
                None => error,
            }
        })?;
        self.remap_chain_spans(content, &mut chain, offset);

        if !self.functions.borrow().is_empty() {
            return Err(self.error(
                "returned an Unquote inside a Function: a span is evaluated once, when the \
                 expansion is entered, so it cannot stand for code run on each call",
            ));
        }
        let scoped = self.scopes.get() > 0;
        let (ripple, pure) = match &chain.terms[..] {
            [term] if chain.binding.is_none() => {
                (term.is_bare_ripple(), splices_in_place(term, scoped))
            }
            _ => (false, false),
        };
        if ripple || pure {
            let mut spliced = if ripple {
                term_chain(self.reference(RIPPLE_BINDING.to_string()))
            } else {
                chain
            };
            if let Some(callee) = callee {
                spliced.terms.push(callee.applied());
            }
            return Ok(spliced);
        }

        let span = (offset, length);
        let callee_key = callee.as_ref().map(Callee::key);
        let mut holes = self.holes.borrow_mut();
        let index = match holes
            .iter()
            .position(|hole| hole.span == span && hole.callee == callee_key)
        {
            Some(index) => index,
            None => {
                // The hole runs as a block applied to the ripple binding, so its chain
                // input — and hence `~` — is the dialect term's flowing value, and any
                // bindings it makes stay local to it. An `Apply`'s member is applied inside
                // the block, leaving the span in argument position.
                if let Some(callee) = callee {
                    chain.terms.push(callee.applied());
                }
                let terms = vec![
                    self.reference(RIPPLE_BINDING.to_string()),
                    ast::Term::Block(ast::Block {
                        annotations: vec![],
                        branches: vec![ast::Branch {
                            condition: ast::Sequence::from_chains([chain]),
                            consequence: None,
                        }],
                    }),
                    ast::Term::Match(ast::Match::Identifier(
                        format!("{HOLE_BINDING_PREFIX}{}", holes.len()),
                        ast::Spanned::default(),
                    )),
                ];
                holes.push(Hole {
                    span,
                    callee: callee_key,
                    terms,
                });
                holes.len() - 1
            }
        };
        Ok(term_chain(
            self.reference(format!("{HOLE_BINDING_PREFIX}{index}")),
        ))
    }

    /// Shift the spans of a parsed hole (relative to its slice) into invocation-file
    /// positions, via the hole's content offset and the escape positions. Without a
    /// content span (synthetic input), spans are cleared rather than left pointing into
    /// the wrong file.
    fn remap_chain_spans(&self, content: &Content, chain: &mut ast::Chain, hole_offset: usize) {
        remap_spans(content, chain, hole_offset);
    }

    fn check_depth(&self, depth: usize) -> Result<(), Error> {
        if depth > MAX_EMISSION_DEPTH {
            return Err(self.error(&format!(
                "returned an expansion nesting more than {MAX_EMISSION_DEPTH} levels \
                 deep. A list-shaped expansion nests once per element, and each \
                 level costs a type of its own — build a long list at run time \
                 instead of writing it as one literal"
            )));
        }
        Ok(())
    }

    /// Whether an expression is `Ripple`, the flowing value.
    fn is_ripple(&self, value: &Value) -> Result<bool, Error> {
        Ok(matches!(value, Value::Tuple(..)) && self.expect_tuple(value)? == ("Ripple", &[][..]))
    }

    /// The scope binding a `Local`/`Bind` name stands for.
    fn local(&self, name: &str) -> String {
        format!("{}{name}", self.locals)
    }

    /// A `Local`'s or `Bind`'s name, which must be one a variable could have.
    fn variable(&self, value: &Value, node: &str) -> Result<String, Error> {
        let name = self.str_field(value, node, &["name"], 0)?;
        if !is_variable_name(&name) {
            return Err(self.error(&format!(
                "returned {node}[\"{name}\"], which is not a variable name"
            )));
        }
        Ok(name)
    }

    /// Run `f` inside one of the expansion's own blocks or functions.
    fn scoped<R>(&self, f: impl FnOnce() -> R) -> R {
        self.scopes.set(self.scopes.get() + 1);
        let result = f();
        self.scopes.set(self.scopes.get() - 1);
        result
    }

    /// Whether an expression is a name an access can stand for: `Ripple`, `Param`, `Local`,
    /// `Member`, or a `Get` of one.
    fn is_access(&self, value: &Value, depth: usize) -> Result<bool, Error> {
        self.check_depth(depth)?;
        let Value::Tuple(tuple_id, _) = value else {
            return Ok(false);
        };
        match self
            .program
            .lookup_tuple(*tuple_id)
            .and_then(|info| info.name.as_deref())
        {
            Some("Param" | "Local" | "Member") => Ok(true),
            Some("Ripple") => self.is_ripple(value),
            Some("Get") => self.is_access(self.field(value, "Get", &["of"], 0)?, depth + 1),
            _ => Ok(false),
        }
    }

    /// The access a name expression stands for (see [`Self::is_access`]).
    fn access(&self, value: &Value, depth: usize) -> Result<ast::Access, Error> {
        self.check_depth(depth)?;
        match self.expect_tuple(value)?.0 {
            "Ripple" if self.is_ripple(value)? => Ok(self.access_from(ast::AccessSource::Ripple)),
            "Param" => {
                let parameter = self.int_field(value, "Param", &["depth"], 0)?;
                if parameter >= self.functions.borrow().len() {
                    return Err(self.error(&format!(
                        "returned Param[{parameter}] outside a function of its own that deep: \
                         an expansion reaches only its own functions' parameters"
                    )));
                }
                Ok(self.access_from(ast::AccessSource::Parameter { depth: parameter }))
            }
            "Local" => {
                let name = self.variable(value, "Local")?;
                Ok(self.access_from(ast::AccessSource::Identifier(self.local(&name))))
            }
            "Member" => {
                let module = self.str_field(value, "Member", &["module"], 0)?;
                let mut access = self.access_from(self.import(&module));
                // `member: []` is the module's own value.
                let member = self.field(value, "Member", &["member"], 1)?;
                if !member.is_nil() {
                    let member = self.str_bytes_to_string(self.str_bytes(member, "Member")?)?;
                    access.accessors.push(ast::AccessPath::Field(member));
                    access.accessor_spans.push(ast::Spanned::default());
                }
                access.type_arguments = self
                    .list(
                        self.field(value, "Member", &["arguments"], 2)?,
                        "Member arguments",
                    )?
                    .into_iter()
                    .map(|argument| self.type_expr(argument, depth + 1))
                    .collect::<Result<_, Error>>()?;
                Ok(access)
            }
            "Get" => {
                let mut access = self.access(self.field(value, "Get", &["of"], 0)?, depth + 1)?;
                access
                    .accessors
                    .push(self.accessor(self.field(value, "Get", &["field"], 1)?)?);
                access.accessor_spans.push(ast::Spanned::default());
                Ok(access)
            }
            other => Err(self.error(&format!(
                "returned {other}[…] where a name (Ripple, Param, Local, Member, or a Get of \
                 one) was expected"
            ))),
        }
    }

    /// A pin's target: a `Local`, a `Param` or a `Member` (with no type arguments), or a `Get`
    /// of one.
    fn pin(&self, value: &Value, depth: usize) -> Result<ast::Access, Error> {
        let access = self.access(value, depth)?;
        match access.source {
            Some(
                ast::AccessSource::Identifier(_)
                | ast::AccessSource::Parameter { .. }
                | ast::AccessSource::Import(..),
            ) if access.type_arguments.is_empty() => Ok(access),
            _ => Err(self.unexpected(
                "a name to pin (Local, Param, Member, or a Get of one)",
                value,
            )),
        }
    }

    /// A `Get`'s field: a label, or a position.
    fn accessor(&self, value: &Value) -> Result<ast::AccessPath, Error> {
        match value {
            Value::Int(index) if *index >= 0 => Ok(ast::AccessPath::Index(*index as usize)),
            _ => Ok(ast::AccessPath::Field(
                self.str_bytes_to_string(self.str_bytes(value, "Get field")?)?,
            )),
        }
    }

    /// The source of a `Spread[source]` tuple entry, or `None` for any other entry.
    fn spread_source<'v>(&self, value: &'v Value) -> Result<Option<&'v Value>, Error> {
        match value {
            Value::Tuple(tuple_id, _)
                if self
                    .program
                    .lookup_tuple(*tuple_id)
                    .is_some_and(|info| info.name.as_deref() == Some("Spread")) =>
            {
                Ok(Some(self.field(value, "Spread", &["source"], 0)?))
            }
            _ => Ok(None),
        }
    }

    /// `Block[branches: list<Branch[condition: list<expr>, consequence: (list<expr> | [])]>]`,
    /// `[]` for a branch without a consequence.
    fn block(&self, value: &Value, depth: usize) -> Result<ast::Block, Error> {
        self.scoped(|| self.block_branches(value, depth))
    }

    fn block_branches(&self, value: &Value, depth: usize) -> Result<ast::Block, Error> {
        let branches = self
            .list(
                self.field(value, "Block", &["branches"], 0)?,
                "Block branches",
            )?
            .into_iter()
            .map(|branch| {
                if self.expect_tuple(branch)?.0 != "Branch" {
                    return Err(self.unexpected("a Branch", branch));
                }
                let condition =
                    self.sequence(self.field(branch, "Branch", &["condition"], 0)?, depth)?;
                let consequence = self.field(branch, "Branch", &["consequence"], 1)?;
                let consequence = match consequence.is_nil() {
                    true => None,
                    false => Some(self.sequence(consequence, depth)?),
                };
                Ok(ast::Branch {
                    condition,
                    consequence,
                })
            })
            .collect::<Result<Vec<_>, Error>>()?;
        if branches.is_empty() {
            return Err(self.error("returned a Block with no branches"));
        }
        Ok(ast::Block {
            annotations: vec![],
            branches,
        })
    }

    /// A list of expressions as a sequence, each its own step.
    fn sequence(&self, value: &Value, depth: usize) -> Result<ast::Sequence, Error> {
        let steps = self.list(value, "sequence")?;
        if steps.is_empty() {
            return Err(self.error("returned an empty sequence of steps"));
        }
        Ok(ast::Sequence::from_chains(
            steps
                .into_iter()
                .map(|step| self.chain_at(step, depth + 1))
                .collect::<Result<Vec<_>, Error>>()?,
        ))
    }

    /// `Function[parameters: list<Str>, parameter: type, result: type, body: expr]`, its result
    /// inferred when the node gives none. A `Block` body is the function's own braces; any other
    /// expression is its one step.
    fn function(&self, value: &Value, depth: usize) -> Result<ast::Function, Error> {
        let parameters = self
            .list(
                self.field(value, "Function", &["parameters"], 0)?,
                "Function parameters",
            )?
            .into_iter()
            .map(|name| self.str_bytes_to_string(self.str_bytes(name, "Function parameters")?))
            .collect::<Result<Vec<_>, Error>>()?;
        for (index, name) in parameters.iter().enumerate() {
            if !is_type_parameter_name(name) {
                return Err(self.error(&format!(
                    "returned a Function with the type parameter \"{name}\", which is not a name"
                )));
            }
            if parameters[..index].contains(name) {
                return Err(self.error(&format!(
                    "returned a Function declaring the type parameter \"{name}\" twice"
                )));
            }
        }
        // An annotation's value is evaluated where the closure is made, outside the function,
        // so it is lowered before the function's own scope is entered.
        let annotations = self
            .list(
                self.field(value, "Function", &["annotations"], 3)?,
                "Function annotations",
            )?
            .into_iter()
            .map(|annotation| {
                if self.expect_tuple(annotation)?.0 != "Annotation" {
                    return Err(self.unexpected("an Annotation", annotation));
                }
                Ok(ast::Annotation {
                    name: self.str_field(annotation, "Annotation", &["key"], 0)?,
                    name_span: ast::Spanned::default(),
                    span: ast::Spanned::default(),
                    value: self.chain_at(
                        self.field(annotation, "Annotation", &["value"], 1)?,
                        depth + 1,
                    )?,
                })
            })
            .collect::<Result<Vec<_>, Error>>()?;
        self.functions.borrow_mut().push(parameters.clone());
        let lowered = self.scoped(|| {
            let parameter_type =
                self.type_expr(self.field(value, "Function", &["parameter"], 1)?, depth + 1)?;
            let result = self.field(value, "Function", &["result"], 2)?;
            let result_type = match result.is_nil() {
                true => None,
                false => Some(self.type_expr(result, depth + 1)?),
            };
            let body = self.field(value, "Function", &["body"], 4)?;
            let is_block =
                matches!(body, Value::Tuple(..)) && self.expect_tuple(body)?.0 == "Block";
            let body = if is_block {
                self.block(body, depth + 1)?
            } else {
                ast::Block {
                    annotations: vec![],
                    branches: vec![ast::Branch {
                        condition: ast::Sequence::from_chains([self.chain_at(body, depth + 1)?]),
                        consequence: None,
                    }],
                }
            };
            Ok((parameter_type, result_type, body))
        });
        self.functions.borrow_mut().pop();
        let (parameter_type, return_type, mut body) = lowered?;
        body.annotations = annotations;
        Ok(ast::Function {
            type_parameters: parameters
                .iter()
                .map(|name| ast::TypeParameter {
                    name: format!("{name}{TYPE_PARAMETER_SUFFIX}"),
                    bound: None,
                })
                .collect(),
            parameter_type: Some(parameter_type),
            return_type,
            body: Some(body),
            span: self.expander.span,
        })
    }

    /// A pattern: a literal (integer, binary, `Str`, `Nil`), `Tuple[name, fields]` of patterns,
    /// `Bind[name]`, `Any`, `Ripple`, `Not[p]`, `And[ps]`, `Or[ps]` or `Is[type]`.
    fn pattern(&self, value: &Value, depth: usize) -> Result<ast::Match, Error> {
        self.check_depth(depth)?;
        match value {
            Value::Int(int) => {
                return Ok(ast::Match::Literal(ast::Literal::Integer(
                    num_bigint::BigInt::from(*int),
                )));
            }
            Value::BigInt(big) => {
                return Ok(ast::Match::Literal(ast::Literal::Integer((**big).clone())));
            }
            Value::Binary(binary) => {
                let bytes = binary_bytes(self.program, binary)
                    .ok_or_else(|| self.error("returned an unreadable binary"))?;
                return Ok(ast::Match::Literal(ast::Literal::Binary(
                    ast::BinaryLiteral::ungrouped(bytes),
                )));
            }
            _ => {}
        }
        let (name, fields) = self.expect_tuple(value)?;
        let parts = |label: &str| -> Result<Vec<ast::Match>, Error> {
            self.list(self.field(value, label, &[], 0)?, label)?
                .into_iter()
                .map(|part| self.pattern(part, depth + 1))
                .collect()
        };
        Ok(match name {
            "Str" => ast::Match::String(ast::StringStyle::Single, self.str_bytes(value, "Str")?),
            "Nil" if fields.is_empty() => ast::Match::Tuple(ast::MatchTuple {
                name: Some("Nil".to_string()),
                fields: vec![],
            }),
            "Tuple" => {
                let name = match self.tuple_name(self.field(value, "Tuple", &["name"], 0)?)? {
                    ast::TupleName::Named(name) => Some(name),
                    _ => None,
                };
                let fields = self
                    .tuple_field_entries(self.field(value, "Tuple", &["fields"], 1)?)?
                    .into_iter()
                    .map(|(label, field)| {
                        Ok(ast::MatchField {
                            name: label,
                            pattern: self.pattern(field, depth + 1)?,
                            span: ast::Spanned::default(),
                        })
                    })
                    .collect::<Result<_, Error>>()?;
                ast::Match::Tuple(ast::MatchTuple { name, fields })
            }
            "Bind" => ast::Match::Identifier(
                self.local(&self.variable(value, "Bind")?),
                ast::Spanned::default(),
            ),
            "Any" if fields.is_empty() => ast::Match::Placeholder,
            "Ripple" if fields.is_empty() => ast::Match::Ripple,
            "Not" => ast::Match::Not(Box::new(
                self.pattern(self.field(value, "Not", &[], 0)?, depth + 1)?,
            )),
            "And" => ast::Match::And(parts("And")?),
            "Or" => ast::Match::Or(parts("Or")?),
            "Is" => ast::Match::Type(self.type_expr(self.field(value, "Is", &[], 0)?, depth + 1)?),
            "Partial" => {
                let name = match self.tuple_name(self.field(value, "Partial", &["name"], 0)?)? {
                    ast::TupleName::Named(name) => Some(name),
                    _ => None,
                };
                let fields = self
                    .tuple_field_entries(self.field(value, "Partial", &["fields"], 1)?)?
                    .into_iter()
                    .map(|(label, field)| {
                        let name = label.ok_or_else(|| {
                            self.error("returned a Partial with a field that isn't Labeled")
                        })?;
                        Ok(ast::PartialPatternField {
                            name,
                            name_span: ast::Spanned::default(),
                            pattern: Some(self.pattern(field, depth + 1)?),
                            span: ast::Spanned::default(),
                        })
                    })
                    .collect::<Result<_, Error>>()?;
                ast::Match::Partial(ast::PartialPattern { name, fields })
            }
            "Pin" => ast::Match::Pin(self.pin(self.field(value, "Pin", &[], 0)?, depth + 1)?),
            "Hole" => return Err(self.hole_returned()),
            other => {
                return Err(
                    self.error(&format!("returned {other}[…] where a pattern was expected"))
                );
            }
        })
    }

    /// A `'%meta.type`.
    fn type_expr(&self, value: &Value, depth: usize) -> Result<ast::Type, Error> {
        self.check_depth(depth)?;
        let (name, _) = self.expect_tuple(value)?;
        let optional = |value: &Value| -> Result<Option<Box<ast::Type>>, Error> {
            match value.is_nil() {
                true => Ok(None),
                false => Ok(Some(Box::new(self.type_expr(value, depth + 1)?))),
            }
        };
        let types = |value: &Value, node: &str| -> Result<Vec<ast::Type>, Error> {
            self.list(value, node)?
                .into_iter()
                .map(|member| self.type_expr(member, depth + 1))
                .collect()
        };
        // A type's fields are types (positional) or `Labeled[label, value]`, as a `Tuple`
        // expression's are.
        let fields = |value: &Value| -> Result<Vec<ast::FieldType>, Error> {
            self.tuple_field_entries(value)?
                .into_iter()
                .map(|(label, field)| {
                    Ok(ast::FieldType::Field {
                        span: ast::Spanned::default(),
                        name: label,
                        omittable: false,
                        type_def: Some(self.type_expr(field, depth + 1)?),
                        default: None,
                    })
                })
                .collect()
        };
        Ok(match name {
            "Int" => ast::Type::Primitive(ast::PrimitiveType::Int),
            "Bin" => ast::Type::Primitive(ast::PrimitiveType::Bin),
            "Ref" => ast::Type::Primitive(ast::PrimitiveType::Ref),
            "Top" => ast::Type::Top,
            "Var" => {
                let variable = self.str_field(value, "Var", &[], 0)?;
                // Only a function of the expansion's own declares a name it may use, so a
                // variable never resolves to one of the caller's.
                if !self
                    .functions
                    .borrow()
                    .iter()
                    .any(|parameters| parameters.contains(&variable))
                {
                    return Err(self.error(&format!(
                        "returned Var[\"{variable}\"] outside a Function declaring it: an \
                         expansion names only its own type parameters"
                    )));
                }
                ast::Type::Identifier {
                    name: format!("{variable}{TYPE_PARAMETER_SUFFIX}"),
                    arguments: vec![],
                }
            }
            "Resource" => ast::Type::Resource(self.str_field(value, "Resource", &[], 0)?),
            // `^` is `^0`, and is written so.
            "Cycle" => ast::Type::Cycle(match self.int_field(value, "Cycle", &[], 0)? {
                0 => None,
                depth => Some(depth),
            }),
            "Tuple" | "Partial" => {
                let tuple_name = match self.tuple_name(self.field(value, name, &["name"], 0)?)? {
                    ast::TupleName::Named(tuple_name) => Some(tuple_name),
                    _ => None,
                };
                let rest = if name == "Partial" {
                    optional(self.field(value, name, &["rest"], 2)?)?
                } else {
                    None
                };
                let fields = fields(self.field(value, name, &["fields"], 1)?)?;
                if name == "Partial"
                    && fields
                        .iter()
                        .any(|field| matches!(field, ast::FieldType::Field { name: None, .. }))
                {
                    return Err(
                        self.error("returned a Partial type with a field that isn't Labeled")
                    );
                }
                ast::Type::Tuple(ast::TupleType {
                    name: tuple_name,
                    fields,
                    is_partial: name == "Partial",
                    rest,
                })
            }
            "Union" => {
                let mut members = types(self.field(value, "Union", &[], 0)?, "Union")?;
                match members.len() {
                    0 => {
                        return Err(self.error(
                            "returned an empty Union, which has no spelling: no value is of it",
                        ));
                    }
                    1 => members.pop().expect("one member"),
                    _ => ast::Type::Union(ast::UnionType { types: members }),
                }
            }
            "Intersection" => {
                ast::Type::Intersection(types(self.field(value, "Intersection", &[], 0)?, name)?)
            }
            "Function" => ast::Type::Function(ast::FunctionType {
                input: Box::new(
                    self.type_expr(self.field(value, name, &["parameter"], 0)?, depth + 1)?,
                ),
                output: Box::new(
                    self.type_expr(self.field(value, name, &["result"], 1)?, depth + 1)?,
                ),
                receive: None,
                states: None,
            }),
            "Process" => ast::Type::Process(ast::ProcessType {
                receive_type: optional(self.field(value, name, &["send"], 0)?)?,
                return_type: optional(self.field(value, name, &["await"], 1)?)?,
                state_type: optional(self.field(value, name, &["state"], 2)?)?,
            }),
            "Named" => {
                let module = self.str_field(value, name, &["module"], 0)?;
                let member = match self.field(value, name, &["member"], 1)? {
                    member if member.is_nil() => None,
                    member => Some(self.str_bytes_to_string(self.str_bytes(member, name)?)?),
                };
                ast::Type::ModuleType {
                    module: module_path(&module),
                    member,
                    arguments: types(self.field(value, name, &["arguments"], 2)?, name)?,
                    package: Some(self.expander.package.clone()),
                }
            }
            "Hole" => return Err(self.hole_returned()),
            other => {
                return Err(self.error(&format!(
                    "returned {other}[…] where a '%meta.type was expected"
                )));
            }
        })
    }

    /// The items of a `Nil | Cons[…]` list.
    fn list<'v>(&self, mut list: &'v Value, node: &str) -> Result<Vec<&'v Value>, Error> {
        let mut items = Vec::new();
        loop {
            match self.expect_tuple(list)? {
                ("Nil", []) => return Ok(items),
                ("Cons", [_, _]) => {
                    items.push(self.field(list, "Cons", &[], 0)?);
                    list = self.field(list, "Cons", &[], 1)?;
                }
                _ => {
                    return Err(self.unexpected(&format!("a list (Nil | Cons[…]) in {node}"), list));
                }
            }
        }
    }

    fn tuple_name(&self, value: &Value) -> Result<ast::TupleName, Error> {
        if value.is_nil() {
            return Ok(ast::TupleName::Anonymous);
        }
        match self.expect_tuple(value)? {
            ("Str", _) => {
                let name = self.str_bytes_to_string(self.str_bytes(value, "Tuple name")?)?;
                // A tuple's name starts with an uppercase letter; a lowercase one would be read
                // as an alias to inherit a name from, which is the caller's to define.
                if !name.chars().next().is_some_and(|c| c.is_ascii_uppercase()) {
                    return Err(self.error(&format!(
                        "returned the tuple name \"{name}\", which does not start with an \
                         uppercase letter"
                    )));
                }
                Ok(ast::TupleName::Named(name))
            }
            (other, _) => Err(self.error(&format!(
                "returned {other} where a tuple name (Str['bin] | []) was expected"
            ))),
        }
    }

    fn tuple_field_entries<'v>(
        &self,
        mut list: &'v Value,
    ) -> Result<Vec<(Option<String>, &'v Value)>, Error> {
        let mut fields = Vec::new();
        loop {
            match self.expect_tuple(list)? {
                ("Nil", []) => return Ok(fields),
                ("Nil", _) => {
                    return Err(
                        self.error("returned a Nil with fields where a field-list terminator (bare Nil) was expected")
                    );
                }
                ("Cons", _) => {
                    let entry = self.field(list, "Cons", &[], 0)?;
                    // A field is a bare expression (positional), or a `Labeled[label,
                    // value]` wrapper. Unambiguous: no expression node is named Labeled
                    // (a data tuple by that name is spliced via `Tuple["Labeled", …]`).
                    let (label, value) = match entry {
                        Value::Tuple(tuple_id, _)
                            if self
                                .program
                                .lookup_tuple(*tuple_id)
                                .is_some_and(|info| info.name.as_deref() == Some("Labeled")) =>
                        {
                            let label = self.str_field(entry, "Labeled", &["label"], 0)?;
                            (Some(label), self.field(entry, "Labeled", &["value"], 1)?)
                        }
                        _ => (None, entry),
                    };
                    fields.push((label, value));
                    list = self.field(list, "Cons", &[], 1)?;
                }
                (other, _) => {
                    return Err(self.error(&format!(
                        "returned {other} where a field list (Nil | Cons[…]) was expected"
                    )));
                }
            }
        }
    }

    /// The span of an `Unquote` value, or `None` for any other expression.
    fn as_unquote(&self, value: &Value) -> Result<Option<(usize, usize)>, Error> {
        if !matches!(value, Value::Tuple(..)) {
            return Ok(None);
        }
        match self.expect_tuple(value)? {
            ("Unquote", _) => self.unquote_span(value).map(Some),
            _ => Ok(None),
        }
    }

    fn unquote_span(&self, value: &Value) -> Result<(usize, usize), Error> {
        Ok((
            self.int_field(value, "Unquote", &["offset"], 0)?,
            self.int_field(value, "Unquote", &["length"], 1)?,
        ))
    }

    /// A `Hole` is a quote's, standing for code computed where the quote is written; spliced, it
    /// means nothing.
    fn hole_returned(&self) -> Error {
        self.error(
            "returned a Hole, which stands only in code a quote lifts: as code to splice, the \
             caller's code is an Unquote",
        )
    }

    fn reference(&self, name: String) -> ast::Term {
        ast::Term::Access(self.access_from(ast::AccessSource::Identifier(name)))
    }

    /// The import of a module an expansion names, by its path in the dialect's package: the
    /// modules generated code reaches are the ones its author meant, whatever the caller's own
    /// modules are called.
    fn import(&self, module: &str) -> ast::AccessSource {
        ast::AccessSource::Import(module_path(module), Some(self.expander.package.clone()))
    }

    /// An access of `source` alone, at the invocation's span.
    fn access_from(&self, source: ast::AccessSource) -> ast::Access {
        ast::Access {
            source: Some(source),
            accessors: vec![],
            accessor_spans: vec![],
            type_arguments: vec![],
            base_span: self.expander.span,
            span: self.expander.span,
        }
    }

    fn expect_tuple<'v>(&self, value: &'v Value) -> Result<(&str, &'v [Value]), Error> {
        match value {
            Value::Tuple(tuple_id, payload) => {
                let name = self
                    .program
                    .lookup_tuple(*tuple_id)
                    .and_then(|info| info.name.as_deref())
                    .unwrap_or("");
                Ok((name, payload))
            }
            other => Err(self.unexpected("a '%meta.expr tuple", other)),
        }
    }

    /// A field of an IR tuple, located by label when the tuple carries labels, else by position
    /// — so a dialect may construct IR tuples with labelled fields in any order (see
    /// [`node_field`]).
    fn field<'v>(
        &self,
        value: &'v Value,
        node: &str,
        labels: &[&str],
        index: usize,
    ) -> Result<&'v Value, Error> {
        if !matches!(value, Value::Tuple(..)) {
            return Err(self.unexpected(node, value));
        }
        node_field(self.program, value, labels, Some(index)).ok_or_else(|| match labels {
            [label, ..] => self.error(&format!(
                "returned {}, a {node} without its `{label}`",
                format_value(value, self.program)
            )),
            [] => self.error(&format!("returned a malformed {node}")),
        })
    }

    fn str_field(
        &self,
        value: &Value,
        node: &str,
        labels: &[&str],
        index: usize,
    ) -> Result<String, Error> {
        let field = self.field(value, node, labels, index)?;
        self.str_bytes_to_string(self.str_bytes(field, node)?)
    }

    fn int_field(
        &self,
        value: &Value,
        node: &str,
        labels: &[&str],
        index: usize,
    ) -> Result<usize, Error> {
        match self.field(value, node, labels, index)? {
            Value::Int(n) if *n >= 0 => Ok(*n as usize),
            other => Err(self.unexpected(&format!("a non-negative integer (in {node})"), other)),
        }
    }

    /// The bytes of a `Str['bin]` value.
    fn str_bytes(&self, value: &Value, node: &str) -> Result<Vec<u8>, Error> {
        str_bytes(self.program, value)
            .ok_or_else(|| self.unexpected(&format!("a Str (in {node})"), value))
    }

    fn str_bytes_to_string(&self, bytes: Vec<u8>) -> Result<String, Error> {
        String::from_utf8(bytes).map_err(|_| self.error("returned a non-UTF-8 string"))
    }

    fn unexpected(&self, expected: &str, found: &Value) -> Error {
        let found = format_value(found, self.program);
        self.error(&format!("returned {found} where {expected} was expected"))
    }

    fn error(&self, message: &str) -> Error {
        self.expander.error(message)
    }
}

/// A field of a node a dialect built: the one whose label is among `labels` — or, for a node
/// built positionally (with no labels at all), the one at `position`, when it may be read by
/// position. A labelled node without the label has no such field, rather than one read by
/// position, so a misspelt label is an error rather than another field's value. Nodes are read by
/// this one rule wherever the compiler reads what a dialect returns.
pub fn node_field<'v>(
    program: &Program,
    value: &'v Value,
    labels: &[&str],
    position: Option<usize>,
) -> Option<&'v Value> {
    let Value::Tuple(tuple_id, payload) = value else {
        return None;
    };
    let info = program.lookup_tuple(*tuple_id)?;
    if info.fields.iter().all(|(label, _)| label.is_none()) {
        return payload.get(position?);
    }
    let labelled = info.fields.iter().position(|(label, _)| {
        label
            .as_deref()
            .is_some_and(|label| labels.contains(&label))
    })?;
    payload.get(labelled)
}

/// The bytes of a `Str['bin]` value.
pub fn str_bytes(program: &Program, value: &Value) -> Option<Vec<u8>> {
    match value {
        Value::Tuple(tuple_id, payload)
            if program
                .lookup_tuple(*tuple_id)
                .is_some_and(|info| info.name.as_deref() == Some("Str")) =>
        {
            match payload.first() {
                Some(Value::Binary(binary)) => binary_bytes(program, binary),
                _ => None,
            }
        }
        _ => None,
    }
}

/// A binary's bytes, from the program's constants or held by the value. A compile-time
/// executor runs over the program's own tables (its context arrives as a value, adding no
/// constants), so these are all a value it returns can hold.
pub fn binary_bytes(program: &Program, binary: &Binary) -> Option<Vec<u8>> {
    match binary {
        Binary::Constant(index) => match program.get_constant(*index) {
            Some(Constant::Binary(bytes)) => Some(bytes.clone()),
            _ => None,
        },
        Binary::Data(data) => Some(data.to_vec()),
    }
}

/// A compile-time value as source text.
pub fn format_value(value: &Value, program: &Program) -> String {
    struct Lookup<'p>(&'p Program);
    impl quiver_core::format::BinaryLookup for Lookup<'_> {
        fn get_bytes(&self, binary: &Binary) -> Option<Vec<u8>> {
            binary_bytes(self.0, binary)
        }
    }
    quiver_core::format::format_value(value, program, &Lookup(program))
}

/// Map a chain parsed from the content at `offset` back to the source positions it was written
/// at; without a content span (synthetic input), its spans are cleared rather than left pointing
/// into the wrong file.
pub fn remap_spans(content: &Content, chain: &mut ast::Chain, offset: usize) {
    let base = content.dialect.content_span.get();
    walk_chain_spans(chain, &mut |spanned: &mut ast::Spanned| {
        spanned.0 = match (base, spanned.0) {
            (Some(base), Some(span)) => Some(remap_span(content, span, offset, base)),
            _ => None,
        };
    });
}

fn remap_span(
    content: &Content,
    span: SourceSpan,
    hole_offset: usize,
    base: SourceSpan,
) -> SourceSpan {
    content_span(
        content.dialect,
        content.escapes,
        hole_offset + span.offset,
        span.length,
    )
    .unwrap_or(base)
}

/// A module path as an IR node writes it, `"html/live"`, as its segments.
fn module_path(module: &str) -> Vec<String> {
    module.split('/').map(str::to_string).collect()
}

/// Whether `name` can be written as a variable: a lowercase identifier, which may end in `?`,
/// `!`, or both.
fn is_variable_name(name: &str) -> bool {
    let base = name.trim_end_matches('!').trim_end_matches('?');
    let suffix = &name[base.len()..];
    matches!(suffix, "" | "?" | "!" | "?!") && is_type_parameter_name(base)
}

/// Whether `name` can be written as a type parameter: a lowercase identifier.
fn is_type_parameter_name(name: &str) -> bool {
    let mut chars = name.chars();
    chars.next().is_some_and(|c| c.is_ascii_lowercase())
        && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

fn term_chain(term: ast::Term) -> ast::Chain {
    ast::Chain {
        binding: None,
        binding_span: ast::Spanned::default(),
        span: ast::Spanned::default(),
        continuations: Vec::new(),
        terms: vec![term],
        assertions: Vec::new(),
    }
}

/// Register the prefix-parse callbacks in an expansion executor's registry. The values
/// handed to the dialect function in its context record dispatch to these by name.
pub fn register_callbacks<E: Effect>(registry: &mut BuiltinRegistry<E>) {
    let param = TypeSpec::Tuple(
        None,
        vec![(None, TypeSpec::Binary), (None, TypeSpec::Integer)],
    );
    let result = TypeSpec::Union(vec![TypeSpec::Integer, TypeSpec::Tuple(None, vec![])]);
    registry.register(
        TERM_CALLBACK.to_string(),
        term_callback::<E>,
        Purity::Pure,
        param.clone(),
        result.clone(),
    );
    registry.register(
        CHAIN_CALLBACK.to_string(),
        chain_callback::<E>,
        Purity::Pure,
        param,
        result,
    );
}

fn term_callback<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, CoreError> {
    prefix_callback(arg, ctx.executor, crate::parser::term_prefix_end)
}

fn chain_callback<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, CoreError> {
    prefix_callback(arg, ctx.executor, crate::parser::chain_prefix_end)
}

/// Shared body of the two callbacks: takes `['bin, 'int]` (content bytes and a byte
/// offset positioned exactly at an expression's first byte), answers the exclusive end
/// offset of one term/chain, or nil when none parses there.
fn prefix_callback<E: Effect>(
    arg: &Value,
    executor: &mut Executor<E>,
    prefix_end: fn(&str, usize) -> Option<usize>,
) -> Result<Completion<E>, CoreError> {
    let Value::Tuple(_, fields) = arg else {
        return Err(CoreError::TypeMismatch {
            expected: "['bin, 'int]".to_string(),
            found: arg.type_name().to_string(),
        });
    };
    let (Some(Value::Binary(binary)), Some(offset_value)) = (fields.first(), fields.get(1)) else {
        return Err(CoreError::TypeMismatch {
            expected: "['bin, 'int]".to_string(),
            found: arg.type_name().to_string(),
        });
    };
    let bytes = match binary {
        Binary::Constant(index) => match executor.get_constant(*index) {
            Some(Constant::Binary(bytes)) => bytes.clone(),
            _ => {
                return Err(CoreError::InvalidArgument(format!(
                    "constant {index} is not a binary"
                )));
            }
        },
        Binary::Data(data) => data.to_vec(),
    };
    let offset = quiver_core::builtins::value_to_usize(offset_value)?;
    let end = std::str::from_utf8(&bytes)
        .ok()
        .and_then(|source| prefix_end(source, offset));
    Ok(Completion::Value(match end {
        Some(end) => Value::int(end as i64),
        None => Value::nil(),
    }))
}

/// Visit every [`ast::Spanned`] reachable from a chain — used to remap a parsed hole's
/// slice-relative spans into invocation-file positions. Exhaustive over the AST so new
/// span fields can't be missed silently ([`ast::Type`] carries no spans, so type
/// positions stop at the type boundary).
fn walk_chain_spans(chain: &mut ast::Chain, f: &mut impl FnMut(&mut ast::Spanned)) {
    f(&mut chain.binding_span);
    f(&mut chain.span);
    if let Some(pattern) = &mut chain.binding {
        walk_match_spans(pattern, f);
    }
    for term in &mut chain.terms {
        walk_term_spans(term, f);
    }
}

fn walk_term_spans(term: &mut ast::Term, f: &mut impl FnMut(&mut ast::Spanned)) {
    match term {
        ast::Term::Literal(_) | ast::Term::Process(_) => {}
        ast::Term::Hole(chain) => walk_chain_spans(chain, f),
        ast::Term::String(_, segments) => {
            for segment in segments {
                match segment {
                    ast::StrSegment::Text(_) => {}
                    ast::StrSegment::Hole(block) => walk_block_spans(block, f),
                }
            }
        }
        ast::Term::Tuple(tuple) => {
            f(&mut tuple.span);
            for field in &mut tuple.fields {
                f(&mut field.name_span);
                f(&mut field.span);
                match &mut field.value {
                    ast::FieldValue::Chain(chain) => walk_chain_spans(chain, f),
                    ast::FieldValue::Spread(Some(access)) => walk_access_spans(access, f),
                    ast::FieldValue::Spread(None) => {}
                }
            }
        }
        ast::Term::Match(pattern) => walk_match_spans(pattern, f),
        ast::Term::Block(block) => walk_block_spans(block, f),
        ast::Term::Function(function) => {
            f(&mut function.span);
            for type_def in function
                .parameter_type
                .iter_mut()
                .chain(function.return_type.iter_mut())
            {
                walk_type_spans(type_def, f);
            }
            if let Some(body) = &mut function.body {
                walk_block_spans(body, f);
            }
        }
        ast::Term::Access(access) => walk_access_spans(access, f),
        ast::Term::State(access, span) => {
            f(span);
            walk_access_spans(access, f);
        }
        ast::Term::Spawn(inner, arg, span) => {
            f(span);
            walk_term_spans(inner, f);
            if let Some(arg) = arg {
                walk_term_spans(arg, f);
            }
        }
        ast::Term::Apply(access, arg) => {
            walk_access_spans(access, f);
            walk_term_spans(arg, f);
        }
        ast::Term::Select(sources, span) => {
            f(span);
            if let Some(sources) = sources {
                for chain in sources {
                    walk_chain_spans(chain, f);
                }
            }
        }
        ast::Term::Dialect(dialect) => {
            f(&mut dialect.span);
            f(&mut dialect.content_span);
        }
    }
}

fn walk_block_spans(block: &mut ast::Block, f: &mut impl FnMut(&mut ast::Spanned)) {
    for annotation in &mut block.annotations {
        f(&mut annotation.name_span);
        f(&mut annotation.span);
        walk_chain_spans(&mut annotation.value, f);
    }
    for branch in &mut block.branches {
        for chain in branch.condition.chains_mut() {
            walk_chain_spans(chain, f);
        }
        if let Some(consequence) = &mut branch.consequence {
            for chain in consequence.chains_mut() {
                walk_chain_spans(chain, f);
            }
        }
    }
}

/// A type's spans are its holes' (a quote's), the only code a type holds.
fn walk_type_spans(type_def: &mut ast::Type, f: &mut impl FnMut(&mut ast::Spanned)) {
    match type_def {
        ast::Type::Hole(chain) => walk_chain_spans(chain, f),
        ast::Type::Tuple(tuple) => {
            for field in &mut tuple.fields {
                if let ast::FieldType::Field {
                    type_def: Some(type_def),
                    ..
                } = field
                {
                    walk_type_spans(type_def, f);
                }
            }
            if let Some(rest) = &mut tuple.rest {
                walk_type_spans(rest, f);
            }
        }
        ast::Type::Function(function) => {
            walk_type_spans(&mut function.input, f);
            walk_type_spans(&mut function.output, f);
        }
        ast::Type::Union(union) => union
            .types
            .iter_mut()
            .for_each(|member| walk_type_spans(member, f)),
        ast::Type::Intersection(members) => members
            .iter_mut()
            .for_each(|member| walk_type_spans(member, f)),
        ast::Type::Identifier { arguments, .. }
        | ast::Type::ModuleType { arguments, .. }
        | ast::Type::SelfDefault { arguments } => arguments
            .iter_mut()
            .for_each(|argument| walk_type_spans(argument, f)),
        ast::Type::Process(process) => {
            for part in [
                &mut process.receive_type,
                &mut process.return_type,
                &mut process.state_type,
            ]
            .into_iter()
            .flatten()
            {
                walk_type_spans(part, f);
            }
        }
        ast::Type::Primitive(_) | ast::Type::Cycle(_) | ast::Type::Resource(_) | ast::Type::Top => {
        }
    }
}

fn walk_access_spans(access: &mut ast::Access, f: &mut impl FnMut(&mut ast::Spanned)) {
    for argument in &mut access.type_arguments {
        walk_type_spans(argument, f);
    }
    for span in &mut access.accessor_spans {
        f(span);
    }
    f(&mut access.base_span);
    f(&mut access.span);
}

fn walk_match_spans(pattern: &mut ast::Match, f: &mut impl FnMut(&mut ast::Spanned)) {
    match pattern {
        ast::Match::Identifier(_, span) => f(span),
        ast::Match::Hole(chain) => walk_chain_spans(chain, f),
        ast::Match::Type(type_def) => walk_type_spans(type_def, f),
        ast::Match::Not(inner) => walk_match_spans(inner, f),
        ast::Match::Pin(target) => walk_access_spans(target, f),
        ast::Match::Literal(_)
        | ast::Match::String(_, _)
        | ast::Match::Star(_)
        | ast::Match::Placeholder
        | ast::Match::Ripple => {}
        ast::Match::Tuple(tuple) => {
            for field in &mut tuple.fields {
                walk_match_spans(&mut field.pattern, f);
            }
        }
        ast::Match::Partial(partial) => {
            for field in &mut partial.fields {
                f(&mut field.name_span);
                if let Some(pattern) = &mut field.pattern {
                    walk_match_spans(pattern, f);
                }
            }
        }
        ast::Match::Or(parts) | ast::Match::And(parts) => {
            for part in parts {
                walk_match_spans(part, f);
            }
        }
    }
}

/// Whether a single-term hole can be spliced where it is written rather than bound to a
/// `~dialect-hole-N` first: evaluating it must have no effect and no result of its own, so
/// that "once, in content order" is satisfied trivially, and it must mean there what it means
/// where it was written. A literal and a text-only string are values; a name is only ever a
/// load, since every call is written (a tail call is the one access that is a call). Inside one
/// of the expansion's own blocks (`scoped`), `$` and `~` would be the expansion's, so only a name
/// that reads neither — a variable, an import, a builtin — splices there. (A span never stands
/// inside one of the expansion's functions, so the type parameters in scope are the caller's.)
fn splices_in_place(term: &ast::Term, scoped: bool) -> bool {
    match term {
        ast::Term::Literal(_) => true,
        ast::Term::Access(access) => match access.source {
            Some(
                ast::AccessSource::Identifier(_)
                | ast::AccessSource::Import(..)
                | ast::AccessSource::Builtin(_),
            ) => true,
            Some(ast::AccessSource::TailCall(_) | ast::AccessSource::TailCallRipple) => false,
            _ => !scoped,
        },
        ast::Term::String(_, segments) => segments
            .iter()
            .all(|segment| matches!(segment, ast::StrSegment::Text(_))),
        _ => false,
    }
}
