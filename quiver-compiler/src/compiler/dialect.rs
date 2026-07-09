//! Dialect expansion: converting the `'expr` value a dialect function returns into an AST
//! term, spliced at the invocation site and type-checked there like ordinary code.
//!
//! The IR is recognized structurally (`std/meta.qv` is its canonical Quiver-side
//! definition). Plain data splices as itself — integers, binaries, `Str`s, and a bare
//! `Nil` become the corresponding literals — and the named-tuple wrappers cover the rest:
//!
//! - `Var[Str['bin]]` — a variable, resolved in the *caller's* scope (the only way an
//!   expansion can reach the caller — hygiene by construction)
//! - `Ripple` — the value flowing into the dialect term (the invocation site's `~`)
//! - `Call[member: Str['bin], arg: ^]` — a call to a top-level export of the *dialect
//!   module itself*
//! - `Tup[name: (Nil | Str['bin]), fields: 'list<^ | Labeled[label: Str['bin], value: ^]>]`
//!   — tuple construction (fields are bare expressions, positionally, unless wrapped in
//!   `Labeled`)
//!
//! `Var`/`Ripple` compile to references (`&x` semantics): an expansion never implicitly
//! calls a caller value; application is expressed with `Call`.

use crate::ast;
use crate::compiler::Error;
use quiver_core::{
    program::Program,
    types::TypeLookup,
    value::{Binary, Value},
};

/// The scope binding an expansion reads the flowing value through. Unspellable in source
/// (identifiers can't start with `~`), so it can never collide with or capture a user
/// binding.
pub const RIPPLE_BINDING: &str = "~dialect-ripple";

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

/// Wrap an expansion chain as a single term: a block binding the flowing value to
/// [`RIPPLE_BINDING`] — `{ =~dialect-ripple, <expansion> }`. The block receives the dialect
/// term's flowing value as its parameter, so `Ripple` references resolve to it. (The
/// binder is a bare binding, which the type checker knows is irrefutable, so it doesn't
/// widen the block's type even on a nil-typed input.)
pub fn wrap_expansion(expansion: ast::Chain) -> ast::Term {
    let binder = ast::Chain {
        match_pattern: None,
        bind_span: ast::Spanned::default(),
        span: ast::Spanned::default(),
        terms: vec![ast::Term::Match(ast::Match::Identifier(
            RIPPLE_BINDING.to_string(),
            ast::Spanned::default(),
        ))],
    };
    ast::Term::Block(ast::Expression {
        annotations: vec![],
        branches: vec![ast::Branch {
            condition: ast::Sequence {
                chains: vec![binder, expansion],
            },
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
    /// The invocation's span, stamped onto synthesized references so unresolved-`Var` and
    /// ill-typed-`ECall` errors point at the call site.
    pub span: ast::Spanned,
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
            // both splice as themselves (identical to spelling them out with `ETup`).
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
                span: self.span,
            }))),
            "Var" => {
                let variable = self.str_field(value, "Var", &[], 0)?;
                Ok(term_chain(self.reference(variable)))
            }
            "Ripple" => Ok(term_chain(self.reference(RIPPLE_BINDING.to_string()))),
            "Call" => {
                let member = self.str_field(value, "Call", &["member"], 0)?;
                let arg = self.field(value, "Call", &["arg"], 1)?;
                let mut chain = self.value_to_chain(arg)?;
                chain.terms.push(ast::Term::Access(ast::Access {
                    source: Some(ast::AccessSource::Import(self.path.clone())),
                    accessors: vec![ast::AccessPath::Field(member)],
                    accessor_spans: vec![ast::Spanned::default()],
                    base_span: self.span,
                    span: self.span,
                }));
                Ok(chain)
            }
            "Tup" => {
                let name = self.tuple_name(self.field(value, "Tup", &["name"], 0)?)?;
                let fields = self.tuple_fields(self.field(value, "Tup", &["fields"], 1)?)?;
                Ok(term_chain(ast::Term::Tuple(ast::Tuple {
                    name,
                    fields,
                    span: self.span,
                })))
            }
            "Labeled" => {
                Err(self
                    .error("returned Labeled outside a Tup's fields (it wraps a labeled field)"))
            }
            "" if fields.is_empty() => {
                Err(self.error("returned nil where a '%meta.expr was expected"))
            }
            other => Err(self.error(&format!("returned {other}[…], which is not a '%meta.expr"))),
        }
    }

    fn tuple_name(&self, value: &Value) -> Result<ast::TupleName, Error> {
        match self.expect_tuple(value)? {
            ("Nil", []) => Ok(ast::TupleName::Anonymous),
            ("Str", _) => Ok(ast::TupleName::Named(
                self.str_bytes_to_string(self.str_bytes(value, "Tup name")?)?,
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
                    // (a data tuple by that name is spliced via `Tup["Labeled", …]`).
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
            base_span: self.span,
            span: self.span,
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
        match_pattern: None,
        bind_span: ast::Spanned::default(),
        span: ast::Spanned::default(),
        terms: vec![term],
    }
}
