//! Dialect expansion: converting the `'expr` value a dialect function returns into an AST
//! term, spliced at the invocation site and type-checked there like ordinary code.
//!
//! The IR is recognized structurally (`std/meta.qv` is its canonical Quiver-side
//! definition). Plain data splices as itself — integers, binaries, `Str`s, and a bare
//! `Nil` become the corresponding literals — and the named-tuple wrappers cover the rest:
//!
//! - `Unquote[offset: 'int, length: 'int]` — a span of the content, parsed as one host
//!   chain and spliced in the *caller's* scope (the only way an expansion can reach the
//!   caller — and it can point solely at user-written text, so hygiene is by
//!   construction). Bind-once: every hole evaluates exactly once, in content order,
//!   with the dialect term's flowing value as its chain input; provably pure
//!   single-term holes splice inline instead.
//! - `Call[member: Str['bin], arg: ^]` — a call to a top-level export of the *dialect
//!   module itself*
//! - `Tuple[name: (Nil | Str['bin]), fields: 'list<^ | Labeled[label: Str['bin], value: ^]>]`
//!   — tuple construction (fields are bare expressions, positionally, unless wrapped in
//!   `Labeled`, which exists only inside `fields`)

use std::cell::RefCell;

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

/// An `Unquote` hole collected during splicing: its content span (the dedupe key — the
/// same span spliced twice shares one binding, so it evaluates once) and the chain that
/// evaluates it into its `~dialect-hole-N` binding.
pub struct Hole {
    /// `(offset, length)` into the unescaped content.
    key: (usize, usize),
    terms: Vec<ast::Term>,
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
        terms: vec![ast::Term::Match(ast::Match::Identifier(
            RIPPLE_BINDING.to_string(),
            ast::Spanned::default(),
        ))],
    };
    let mut chains = vec![binder];
    holes.sort_by_key(|hole| hole.key);
    chains.extend(holes.into_iter().map(|hole| ast::Chain {
        binding: None,
        binding_span: ast::Spanned::default(),
        span: ast::Spanned::default(),
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

/// Converts `'expr` values into AST chains for one dialect invocation.
pub struct Splicer<'a, F: Fn(&Binary) -> Option<Vec<u8>>> {
    pub program: &'a Program,
    /// Reads a binary's bytes from the program's constants or the expansion executor's heap.
    pub read_binary: F,
    /// The dialect module, for the error messages and `ECall` member resolution.
    pub module: String,
    pub path: Vec<String>,
    /// The invocation, for the call-site span stamped onto synthesized references (so
    /// unresolved-`Var` and ill-typed-`Call` errors point at it) and for mapping
    /// `Unquote` span offsets to file positions.
    pub dialect: &'a ast::Dialect,
    /// The unescaped brace content `Unquote` spans index into, with the escape positions
    /// recorded by [`unescape_content`].
    pub content: &'a str,
    pub escapes: &'a [usize],
    /// The invoking module's name, for positions in `Unquote` error messages.
    pub current_module: String,
    /// `Unquote` holes collected while walking the returned IR (bind-once).
    pub holes: RefCell<Vec<Hole>>,
}

impl<F: Fn(&Binary) -> Option<Vec<u8>>> Splicer<'_, F> {
    /// Convert the `'expr` value a dialect function returned into a chain to splice at the
    /// invocation site.
    pub fn value_to_chain(&self, value: &Value) -> Result<ast::Chain, Error> {
        // Plain data splices as itself: integers and binaries become literals.
        match value {
            Value::Int(int) => {
                return Ok(term_chain(ast::Term::Literal(ast::Literal::Integer(
                    num_bigint::BigInt::from(*int),
                ))));
            }
            Value::BigInt(big) => {
                return Ok(term_chain(ast::Term::Literal(ast::Literal::Integer(
                    (**big).clone(),
                ))));
            }
            Value::Binary(binary) => {
                let bytes = (self.read_binary)(binary)
                    .ok_or_else(|| self.error("returned an unreadable binary"))?;
                return Ok(term_chain(ast::Term::Literal(ast::Literal::Binary(bytes))));
            }
            _ => {}
        }
        let (name, fields) = self.expect_tuple(value)?;
        match name {
            // A `Str` value is a string literal, and a bare `Nil` the named empty tuple —
            // both splice as themselves (identical to spelling them out with `Tuple`).
            "Str" => {
                let bytes = self.str_bytes(value, "Str")?;
                Ok(term_chain(ast::Term::String(
                    ast::StringStyle::Single,
                    vec![ast::StrSegment::Text(bytes)],
                )))
            }
            "Nil" if fields.is_empty() => Ok(term_chain(ast::Term::Tuple(ast::Tuple {
                name: ast::TupleName::Named("Nil".to_string()),
                fields: vec![],
                span: self.dialect.span,
            }))),
            "Unquote" => {
                let offset = self.int_field(value, "Unquote", &["offset"], 0)?;
                let length = self.int_field(value, "Unquote", &["length"], 1)?;
                self.unquote(offset, length)
            }
            "Call" => {
                let member = self.str_field(value, "Call", &["member"], 0)?;
                let arg = self.field(value, "Call", &["arg"], 1)?;
                let mut chain = self.value_to_chain(arg)?;
                chain.terms.push(ast::Term::Access(ast::Access {
                    source: Some(ast::AccessSource::Import(self.path.clone())),
                    accessors: vec![ast::AccessPath::Field(member)],
                    accessor_spans: vec![ast::Spanned::default()],
                    type_arguments: vec![],
                    base_span: self.dialect.span,
                    span: self.dialect.span,
                }));
                Ok(chain)
            }
            "Tuple" => {
                let name = self.tuple_name(self.field(value, "Tuple", &["name"], 0)?)?;
                let fields = self.tuple_fields(self.field(value, "Tuple", &["fields"], 1)?)?;
                Ok(term_chain(ast::Term::Tuple(ast::Tuple {
                    name,
                    fields,
                    span: self.dialect.span,
                })))
            }
            "Labeled" => {
                Err(self
                    .error("returned Labeled outside a Tuple's fields (it wraps a labeled field)"))
            }
            "" if fields.is_empty() => {
                Err(self.error("returned nil where a '%meta.expr was expected"))
            }
            other => Err(self.error(&format!("returned {other}[…], which is not a '%meta.expr"))),
        }
    }

    /// Splice an `Unquote[offset, length]` — a span of the content parsed as one host
    /// chain, evaluated in the caller's scope with the dialect term's flowing value as
    /// its input. Semantics is bind-once: every hole evaluates exactly once, in content
    /// order, before the expansion's own structure. Provably pure single-term holes —
    /// a literal, a text-only string, a `&`-reference, or a bare `~` — splice in place
    /// instead; anything else (including a bare identifier, whose callability is
    /// type-dependent) becomes a `~dialect-hole-N` binding, deduplicated by span.
    fn unquote(&self, offset: usize, length: usize) -> Result<ast::Chain, Error> {
        let text = (length > 0)
            .then(|| self.content.get(offset..offset + length))
            .flatten()
            .ok_or_else(|| {
                self.error(&format!(
                    "returned an Unquote span [{offset}, {length}] outside its content"
                ))
            })?;
        let mut chain = crate::parser::parse_chain_exact(text).map_err(|parse_error| {
            let content_offset = offset + parse_error.span.map_or(0, |s| s.offset);
            let position = content_position(self.dialect, self.escapes, content_offset)
                .map(|(line, column)| format!(" (at {}:{line}:{column})", self.current_module))
                .unwrap_or_default();
            self.error(&format!(
                "returned an Unquote span that is not a single block: {}{position}",
                parse_error.kind
            ))
        })?;
        self.remap_chain_spans(&mut chain, offset);

        if chain.binding.is_none() && chain.terms.len() == 1 {
            let term = &chain.terms[0];
            if term.is_bare_ripple() {
                return Ok(term_chain(self.reference(RIPPLE_BINDING.to_string())));
            }
            let pure = matches!(term, ast::Term::Literal(_) | ast::Term::Reference(_))
                || matches!(term, ast::Term::String(_, segments)
                    if segments.iter().all(|s| matches!(s, ast::StrSegment::Text(_))));
            if pure {
                return Ok(chain);
            }
        }

        let mut holes = self.holes.borrow_mut();
        let index = match holes.iter().position(|hole| hole.key == (offset, length)) {
            Some(index) => index,
            None => {
                // The hole runs as a block applied to the ripple binding, so its chain
                // input — and hence `~` — is the dialect term's flowing value, and any
                // bindings it makes stay local to it.
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
                    key: (offset, length),
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
    fn remap_chain_spans(&self, chain: &mut ast::Chain, hole_offset: usize) {
        let base = self.dialect.content_span.get();
        walk_chain_spans(chain, &mut |spanned: &mut ast::Spanned| {
            spanned.0 = match (base, spanned.0) {
                (Some(base), Some(span)) => Some(self.remap_span(span, hole_offset, base)),
                _ => None,
            };
        });
    }

    fn remap_span(&self, span: SourceSpan, hole_offset: usize, base: SourceSpan) -> SourceSpan {
        let to_raw = |content_offset: usize| {
            content_offset + self.escapes.iter().filter(|&&e| e < content_offset).count()
        };
        let start = hole_offset + span.offset;
        let raw_start = to_raw(start);
        let raw_end = to_raw(start + span.length);
        let (line, column) =
            content_position(self.dialect, self.escapes, start).unwrap_or((base.line, base.column));
        SourceSpan {
            offset: base.offset + raw_start,
            line,
            column,
            length: raw_end - raw_start,
        }
    }

    /// The holes collected while splicing, for [`wrap_expansion`].
    pub fn take_holes(&self) -> Vec<Hole> {
        self.holes.take()
    }

    fn tuple_name(&self, value: &Value) -> Result<ast::TupleName, Error> {
        match self.expect_tuple(value)? {
            ("Nil", []) => Ok(ast::TupleName::Anonymous),
            ("Str", _) => Ok(ast::TupleName::Named(
                self.str_bytes_to_string(self.str_bytes(value, "Tuple name")?)?,
            )),
            (other, _) => Err(self.error(&format!(
                "returned {other} where a tuple name (Nil | Str['bin]) was expected"
            ))),
        }
    }

    fn tuple_fields(&self, mut list: &Value) -> Result<Vec<ast::TupleField>, Error> {
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
                    fields.push(ast::TupleField {
                        name: label,
                        name_span: ast::Spanned::default(),
                        span: ast::Spanned::default(),
                        value: ast::FieldValue::Chain(self.value_to_chain(value)?),
                    });
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

    fn reference(&self, name: String) -> ast::Term {
        ast::Term::Reference(ast::Access {
            source: Some(ast::AccessSource::Identifier(name)),
            accessors: vec![],
            accessor_spans: vec![],
            type_arguments: vec![],
            base_span: self.dialect.span,
            span: self.dialect.span,
        })
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

    /// A field of an IR tuple, located by label when the tuple type carries one, else by
    /// position — so a dialect may construct IR tuples with labelled fields in any order.
    fn field<'v>(
        &self,
        value: &'v Value,
        node: &str,
        labels: &[&str],
        index: usize,
    ) -> Result<&'v Value, Error> {
        let Value::Tuple(tuple_id, payload) = value else {
            return Err(self.unexpected(node, value));
        };
        if let Some(info) = self.program.lookup_tuple(*tuple_id) {
            for (position, (label, _)) in info.fields.iter().enumerate() {
                if let Some(label) = label
                    && labels.contains(&label.as_str())
                {
                    return payload
                        .get(position)
                        .ok_or_else(|| self.error(&format!("returned a malformed {node}")));
                }
            }
        }
        payload
            .get(index)
            .ok_or_else(|| self.error(&format!("returned a malformed {node}")))
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
        let bytes = match value {
            Value::Tuple(tuple_id, payload)
                if self
                    .program
                    .lookup_tuple(*tuple_id)
                    .is_some_and(|info| info.name.as_deref() == Some("Str")) =>
            {
                match payload.first() {
                    Some(Value::Binary(binary)) => (self.read_binary)(binary),
                    _ => None,
                }
            }
            _ => None,
        };
        bytes.ok_or_else(|| self.unexpected(&format!("a Str (in {node})"), value))
    }

    fn str_bytes_to_string(&self, bytes: Vec<u8>) -> Result<String, Error> {
        String::from_utf8(bytes).map_err(|_| self.error("returned a non-UTF-8 string"))
    }

    fn unexpected(&self, expected: &str, found: &Value) -> Error {
        self.error(&format!(
            "returned an unexpected value where {expected} was expected: {found:?}"
        ))
    }

    fn error(&self, message: &str) -> Error {
        Error::DialectFailed {
            module: self.module.clone(),
            message: message.to_string(),
        }
    }
}

fn term_chain(term: ast::Term) -> ast::Chain {
    ast::Chain {
        binding: None,
        binding_span: ast::Spanned::default(),
        span: ast::Spanned::default(),
        terms: vec![term],
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
        Binary::Heap(_) => executor.get_binary_data(binary)?.to_vec(),
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
        ast::Term::Literal(_) | ast::Term::Self_ | ast::Term::Process(_) => {}
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
            if let Some(body) = &mut function.body {
                walk_block_spans(body, f);
            }
        }
        ast::Term::Access(access) | ast::Term::Reference(access) => walk_access_spans(access, f),
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

fn walk_access_spans(access: &mut ast::Access, f: &mut impl FnMut(&mut ast::Spanned)) {
    for span in &mut access.accessor_spans {
        f(span);
    }
    f(&mut access.base_span);
    f(&mut access.span);
}

fn walk_match_spans(pattern: &mut ast::Match, f: &mut impl FnMut(&mut ast::Spanned)) {
    match pattern {
        ast::Match::Identifier(_, span) | ast::Match::As(_, _, span) => f(span),
        ast::Match::Reference(target) => {
            for span in &mut target.accessor_spans {
                f(span);
            }
            f(&mut target.base_span);
            f(&mut target.span);
        }
        ast::Match::Literal(_)
        | ast::Match::String(_, _)
        | ast::Match::Star(_)
        | ast::Match::Placeholder
        | ast::Match::Type(_) => {}
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
        ast::Match::Or(alternatives) => {
            for alternative in alternatives {
                walk_match_spans(alternative, f);
            }
        }
    }
}
