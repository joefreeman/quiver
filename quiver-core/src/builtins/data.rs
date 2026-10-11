//! Quiver data notation (`%data`): the canonical textual form of Quiver **values**.
//!
//! The notation is the language's own literal syntax restricted to data — integers,
//! binaries (`<…>`), and tuples (with names and field labels), plus the `"…"` string
//! sugar for `Str` tuples. Encoding is a plain value walk (tuple names come from the
//! runtime type tables, so names — never ids — cross the program boundary). Decoding is
//! **type-directed**: the expected type (the builtin's explicit type argument) drives
//! the parse, tuple names resolve only against the expected type's members, and values
//! are constructed with those members' tuple ids — hostile text can never mint a shape
//! the program doesn't already contain. Any mismatch, unknown name, or trailing input
//! answers nil, like a failed match.
//!
//! Strings never interpolate here (data, not code); `{` is escaped on encode so encoded
//! text also reads as literal *code* unchanged.

use num_bigint::BigInt;

use super::{BuiltinContext, Completion, DecodeErrorDecl, TypeSpec};
use crate::binders::BinderStack;
use crate::effects::Effect;
use crate::error::Error;
use crate::types::{TupleTypeInfo, Type, TypeLookup};
use crate::value::Value;

// ===== encode ===========================================================================

/// `__data_encode__`: any data value → its notation as UTF-8 bytes (wrap in `Str[…]`
/// for display — the runtime cannot mint a `Str` tuple id, so the std wrapper does).
/// Non-data values (functions, builtins, processes, refs, resources) are a runtime
/// error: they have no meaning outside this program. Annotations are data *about* the
/// value and do not survive encoding.
pub fn builtin_data_encode<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let mut out = String::new();
    encode_value(arg, ctx, &mut out)?;
    let binary = ctx.executor.allocate_binary(out.into_bytes())?;
    Ok(Completion::Value(Value::Binary(binary)))
}

pub(crate) fn encode_value<E: Effect>(
    value: &Value,
    ctx: &mut BuiltinContext<E>,
    out: &mut String,
) -> Result<(), Error> {
    match value {
        Value::Int(n) => {
            out.push_str(&n.to_string());
            Ok(())
        }
        Value::BigInt(n) => {
            out.push_str(&n.to_string());
            Ok(())
        }
        Value::Binary(binary) => {
            let bytes = ctx.executor.get_binary_data(binary)?.to_vec();
            push_hex(&bytes, out);
            Ok(())
        }
        Value::Tuple(tuple_id, payload) => {
            let info = TypeLookup::lookup_tuple(&*ctx.executor, *tuple_id)
                .cloned()
                .ok_or_else(|| {
                    Error::InvalidArgument(format!(
                        "cannot encode: tuple type {tuple_id} is not in the runtime tables"
                    ))
                })?;

            // `Str` sugar: quotable text encodes as a string literal. Bytes that no
            // string literal can carry (invalid UTF-8, unescapable control characters)
            // fall through to the ordinary tuple form, `Str[<…>]`.
            if info.name.as_deref() == Some("Str")
                && let [Value::Binary(binary)] = &payload[..]
            {
                let bytes = ctx.executor.get_binary_data(binary)?.to_vec();
                if let Some(quoted) = quote_string(&bytes) {
                    out.push_str(&quoted);
                    return Ok(());
                }
            }

            if let Some(name) = &info.name {
                out.push_str(name);
                if payload.is_empty() {
                    return Ok(());
                }
            } else if payload.is_empty() {
                out.push_str("[]");
                return Ok(());
            }
            out.push('[');
            for (i, field) in payload.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                if let Some((Some(label), _)) = info.fields.get(i) {
                    out.push_str(label);
                    out.push_str(": ");
                }
                encode_value(field, ctx, out)?;
            }
            out.push(']');
            Ok(())
        }
        other => Err(Error::InvalidArgument(format!(
            "cannot encode a {}: %data notation carries data only (integers, binaries, \
             and tuples)",
            other.type_name()
        ))),
    }
}

fn push_hex(bytes: &[u8], out: &mut String) {
    use std::fmt::Write;
    out.push('<');
    for b in bytes {
        let _ = write!(out, "{b:02x}");
    }
    out.push('>');
}

/// Quote as a `"…"` literal if every character is representable: valid UTF-8 whose
/// control characters are limited to the escapable `\n`, `\r`, `\t`. `{` is escaped so
/// the encoded text also reads as literal code (data notation itself never
/// interpolates).
fn quote_string(bytes: &[u8]) -> Option<String> {
    let s = std::str::from_utf8(bytes).ok()?;
    if s.chars()
        .any(|c| c.is_control() && !matches!(c, '\n' | '\r' | '\t'))
    {
        return None;
    }
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '{' => out.push_str("\\{"),
            c => out.push(c),
        }
    }
    out.push('"');
    Some(out)
}

// ===== decode ===========================================================================

/// `__data_decode__<'t>`: notation text → a value of the expected type, or nil stamped with
/// a `DecodeError` under `:error`. The expected type is the builtin's explicit type argument,
/// read from the call — a bare (un-instantiated) reference that ends up called is a runtime
/// error.
pub fn builtin_data_decode<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let expected = ctx.type_arguments().first().copied().ok_or_else(|| {
        Error::InvalidArgument(
            "__data_decode__ called without a type argument — a bare reference carries \
             no instantiation; name it with one (`__data_decode__<'t>`) where the type \
             is concrete"
                .to_string(),
        )
    })?;
    let bytes = match arg {
        Value::Tuple(_, fields) if fields.len() == 1 => match &fields[0] {
            Value::Binary(binary) => ctx.executor.get_binary_data(binary)?.to_vec(),
            other => {
                return Err(Error::TypeMismatch {
                    expected: "Str[binary]".to_string(),
                    found: other.type_name().to_string(),
                });
            }
        },
        other => {
            return Err(Error::TypeMismatch {
                expected: "Str[binary]".to_string(),
                found: other.type_name().to_string(),
            });
        }
    };
    Ok(Completion::Value(decode_text(&bytes, expected, ctx)?))
}

/// Notation text as a value of type `expected`, built with the instantiation's labels, or nil
/// stamped with a `DecodeError` under `:error`.
fn decode_text<E: Effect>(
    bytes: &[u8],
    expected: usize,
    ctx: &mut BuiltinContext<E>,
) -> Result<Value, Error> {
    let mut decoder = Decoder {
        ctx,
        text: Text { bytes, pos: 0 },
        stack: BinderStack::default(),
        path: Vec::new(),
        failure: None,
    };
    let value = decoder.decode_type(expected)?;
    // The whole input must be one value: trailing non-whitespace fails the decode.
    decoder.text.skip_ws();
    let result = match value {
        Some(value) if decoder.text.pos == bytes.len() => value,
        Some(_) => {
            decoder.failure = Some(Failure {
                path: Vec::new(),
                position: decoder.text.pos,
                kind: FailureKind::Trailing,
                expected,
            });
            decoder.stamp()?
        }
        None => decoder.stamp()?,
    };
    Ok(result)
}

/// The decode-failure vocabulary: `DecodeError[path, kind, message]`, mirroring
/// `'%data.error` in std/data.qv.
pub(crate) fn decode_error_decl() -> DecodeErrorDecl {
    let str_spec = TypeSpec::Tuple(Some("Str"), vec![(None, TypeSpec::Binary)]);
    let kinds: Vec<TypeSpec> = FailureKind::NAMES
        .iter()
        .map(|name| TypeSpec::Tuple(Some(name), vec![]))
        .collect();
    let path = TypeSpec::Union(vec![
        TypeSpec::Tuple(Some("Nil"), vec![]),
        TypeSpec::Tuple(
            Some("Cons"),
            vec![
                (
                    None,
                    TypeSpec::Union(vec![str_spec.clone(), TypeSpec::Integer]),
                ),
                (None, TypeSpec::Cycle(1)),
            ],
        ),
    ]);
    DecodeErrorDecl {
        decode_error: TypeSpec::Tuple(
            Some("DecodeError"),
            vec![
                (Some("path"), path),
                (Some("kind"), TypeSpec::Union(kinds.clone())),
                (Some("message"), str_spec),
            ],
        ),
        kinds,
        demand_builtins: vec!["data_decode".to_string(), "codec_convert".to_string()],
        error_key: "error".to_string(),
    }
}

/// Why a decode failed, in the order of the `'%data.kind` tags.
#[derive(Clone, Copy, Debug)]
pub(crate) enum FailureKind {
    /// The text has the wrong shape here.
    Mismatch = 0,
    /// Text is left over after the value.
    Trailing = 1,
}

impl FailureKind {
    const NAMES: [&'static str; 2] = ["Mismatch", "Trailing"];
}

/// A failed attempt. Its message is rendered only once the whole decode has failed, so a
/// decode that succeeds after backtracking pays for none of them.
struct Failure {
    /// The fields descended to reach it: `(tuple, index)`.
    path: Vec<(usize, usize)>,
    /// Where in the text it failed.
    position: usize,
    kind: FailureKind,
    /// The type that was expected there.
    expected: usize,
}

impl Failure {
    /// The failure that got furthest wins: by text position, then by path depth.
    fn rank(&self) -> (usize, usize) {
        (self.position, self.path.len())
    }
}

/// The whitespace between two things inside a binary literal. Between groups any gap will
/// do; against a bracket only a line break will, which is what keeps `< 0a>` malformed while
/// letting a literal be written as a table across lines.
enum Gap {
    None,
    Inline,
    Line,
}

/// How many characters of the text a failure's message quotes.
const SNIPPET: usize = 24;

/// A type-directed recursive-descent parser over the notation. Unions are ordered choice
/// with per-member backtracking (reset to the saved position); the one place a
/// successfully-parsed member could otherwise be a strict prefix of a sibling is closed by
/// lookahead instead of full backtracking — a bare named-empty tuple is never followed by a
/// glued `[` (that is the named-fields form). Recursive types resolve through a binder
/// stack, as the compatibility checker's do.
///
/// Of the attempts that fail, the decoder keeps the one that got furthest — but only those
/// on the way to the overall failure: a step that succeeds discards what its own attempts
/// recorded, so backtracking within it never leaves a stale failure behind.
struct Decoder<'a, 'b, 'c, E: Effect> {
    ctx: &'a mut BuiltinContext<'b, E>,
    text: Text<'c>,
    stack: BinderStack,
    path: Vec<(usize, usize)>,
    failure: Option<Failure>,
}

impl<E: Effect> Decoder<'_, '_, '_, E> {
    fn decode_type(&mut self, type_id: usize) -> Result<Option<Value>, Error> {
        let outer = self.failure.take();
        let result = self.decode_shape(type_id)?;
        let inner = self.failure.take();
        self.failure = match (&result, outer, inner) {
            (Some(_), outer, _) => outer,
            (None, Some(outer), Some(inner)) if inner.rank() <= outer.rank() => Some(outer),
            (None, outer, inner) => inner.or(outer),
        };
        Ok(result)
    }

    fn decode_shape(&mut self, type_id: usize) -> Result<Option<Value>, Error> {
        let typ = TypeLookup::lookup_type(&*self.ctx.executor, type_id)
            .cloned()
            .ok_or_else(|| Error::InvalidArgument(format!("decode at unknown type {type_id}")))?;
        let result = match typ {
            // Rows are invisible to the data plane: decode as the base shape.
            Type::Annotated { base, .. } => return self.decode_type(base),
            Type::Union(members) => return self.decode_union(type_id, members),
            Type::Cycle(depth) => {
                // Re-enter the target at its own depth, so the references inside it count the
                // binders they were written under.
                let Some((target, cut)) = self.stack.follow(depth) else {
                    return Ok(None);
                };
                let result = self.decode_type(target);
                self.stack.restore(cut);
                return result;
            }
            Type::Integer => self.text.parse_int(),
            Type::Binary => self.text.parse_binary(self.ctx)?,
            Type::Tuple(tuple_id) => self.decode_tuple(tuple_id)?,
            // Not data: partials have no layout to construct, and callables, processes,
            // resources and type variables have no textual form.
            Type::Partial { .. }
            | Type::Callable { .. }
            | Type::Process { .. }
            | Type::Resource(_)
            | Type::Reference
            | Type::Variable(_)
            | Type::Intersection(_)
            | Type::Top => None,
        };
        if result.is_none() {
            self.fail(type_id);
        }
        Ok(result)
    }

    fn decode_union(
        &mut self,
        type_id: usize,
        members: Vec<usize>,
    ) -> Result<Option<Value>, Error> {
        self.stack.enter(type_id);
        let start = self.text.pos;
        let mut result = None;
        for member in members {
            self.text.pos = start;
            if let Some(value) = self.decode_type(member)? {
                result = Some(value);
                break;
            }
        }
        self.stack.leave(type_id);
        if result.is_none() {
            self.text.pos = start;
            // When no member got past the union's own start, the union is what was expected
            // there, rather than whichever member happened to be tried first.
            self.text.skip_ws();
            let at = (self.text.pos, self.path.len());
            self.text.pos = start;
            if let Some(failure) = &mut self.failure
                && failure.rank() == at
            {
                failure.expected = type_id;
            }
        }
        Ok(result)
    }

    /// Record a mismatch against `type_id` where the text stands now.
    fn fail(&mut self, type_id: usize) {
        let failure = Failure {
            path: self.path.clone(),
            position: self.text.pos,
            kind: FailureKind::Mismatch,
            expected: type_id,
        };
        if self
            .failure
            .as_ref()
            .is_none_or(|kept| failure.rank() > kept.rank())
        {
            self.failure = Some(failure);
        }
    }

    fn decode_tuple(&mut self, tuple_id: usize) -> Result<Option<Value>, Error> {
        let Some(info) = TypeLookup::lookup_tuple(&*self.ctx.executor, tuple_id).cloned() else {
            return Ok(None);
        };

        // `Str` sugar: a string literal is a `Str` tuple.
        if is_str_shape(&info, &*self.ctx.executor) {
            self.text.skip_ws();
            if self.text.peek() == Some(b'"') {
                let Some(bytes) = self.text.parse_string() else {
                    return Ok(None);
                };
                let binary = self.ctx.executor.allocate_binary(bytes)?;
                return Ok(Some(Value::tuple(
                    self.ctx.label(tuple_id, &self.stack)?,
                    vec![Value::Binary(binary)],
                )));
            }
        }

        let text = &mut self.text;
        text.skip_ws();
        if let Some(name) = &info.name {
            if !text.eat_exact(name.as_bytes()) {
                return Ok(None);
            }
            // The name must end here — `OkThen` must not satisfy `Ok`.
            if text
                .peek()
                .is_some_and(|b| b.is_ascii_alphanumeric() || b == b'_')
            {
                return Ok(None);
            }
            if info.fields.is_empty() {
                // Bare named-empty form — unless a glued `[` follows, which belongs to
                // a sibling `Name[…]` member (the prefix-lookahead rule).
                if text.peek() == Some(b'[') {
                    return Ok(None);
                }
                return Ok(Some(Value::tuple(
                    self.ctx.label(tuple_id, &self.stack)?,
                    vec![],
                )));
            }
            // Named fields open with a *glued* bracket, as in code.
            if !text.eat(b'[') {
                return Ok(None);
            }
        } else {
            if !text.eat(b'[') {
                return Ok(None);
            }
            if info.fields.is_empty() {
                text.skip_ws();
                if !text.eat(b']') {
                    return Ok(None);
                }
                return Ok(Some(Value::tuple(
                    self.ctx.label(tuple_id, &self.stack)?,
                    vec![],
                )));
            }
        }

        let mut values = Vec::with_capacity(info.fields.len());
        for (i, (label, field_type)) in info.fields.iter().enumerate() {
            let text = &mut self.text;
            text.skip_ws();
            if i > 0 {
                if !text.eat(b',') {
                    return Ok(None);
                }
                text.skip_ws();
            }
            // An optional field label: a data value never starts with a lowercase
            // letter, so one unambiguously introduces a label — which must match the
            // expected field's name.
            if text.peek().is_some_and(|b| b.is_ascii_lowercase()) {
                let Some(written) = text.parse_label() else {
                    return Ok(None);
                };
                if label.as_deref() != Some(written.as_str()) {
                    return Ok(None);
                }
                text.skip_ws();
            }
            self.path.push((tuple_id, i));
            let value = self.decode_type(*field_type)?;
            self.path.pop();
            let Some(value) = value else {
                return Ok(None);
            };
            values.push(value);
        }
        let text = &mut self.text;
        text.skip_ws();
        if text.eat(b',') {
            text.skip_ws();
        }
        if !text.eat(b']') {
            return Ok(None);
        }
        Ok(Some(Value::tuple(
            self.ctx.label(tuple_id, &self.stack)?,
            values,
        )))
    }

    /// The nil the failed decode answers: stamped under `:error` with
    /// `DecodeError[path, kind, message]` describing the failure that got furthest. Without
    /// the vocabulary (bytecode run with no runtime tables), a bare nil.
    fn stamp(mut self) -> Result<Value, Error> {
        let failure = self.failure.take().expect("a failed decode records why");
        let Some(table) = self.ctx.executor.decode_error_table().cloned() else {
            return Ok(Value::nil());
        };
        let lookup = &*self.ctx.executor;
        let expected = crate::format::format_type_by_id(lookup, failure.expected);
        let found = self.text.describe(failure.position);
        let message = match failure.kind {
            FailureKind::Mismatch => format!("expected {expected}, found {found}"),
            FailureKind::Trailing => format!("input left over after the value: {found}"),
        };
        // A field by its label, or its position without one.
        let path: Vec<Result<String, usize>> = failure
            .path
            .iter()
            .map(|&(tuple, index)| {
                lookup
                    .lookup_tuple(tuple)
                    .and_then(|info| info.fields.get(index))
                    .and_then(|(label, _)| label.clone())
                    .ok_or(index)
            })
            .collect();

        let executor = &mut *self.ctx.executor;
        let mut chain = Value::tuple(table.nil_tuple, vec![]);
        for part in path.into_iter().rev() {
            let element = match part {
                Ok(label) => {
                    let binary = executor.allocate_binary(label.into_bytes())?;
                    Value::tuple(table.str_tuple, vec![Value::Binary(binary)])
                }
                Err(index) => Value::int(index as i64),
            };
            chain = Value::tuple(table.cons_tuple, vec![element, chain]);
        }
        let kind = Value::tuple(table.kind_tuples[failure.kind as usize], vec![]);
        let binary = executor.allocate_binary(message.into_bytes())?;
        let message = Value::tuple(table.str_tuple, vec![Value::Binary(binary)]);
        let payload = Value::tuple(table.decode_error_tuple, vec![chain, kind, message]);
        Ok(Value::nil()
            .annotated(table.error_key, payload)
            .expect("a fresh nil carries no annotations"))
    }
}

/// Notation text, and the position the decoder has read to.
struct Text<'a> {
    bytes: &'a [u8],
    pos: usize,
}

impl Text<'_> {
    /// The text at `position` (after whitespace), up to the end of its line, and its offset.
    fn describe(&self, position: usize) -> String {
        let start = position
            + self.bytes[position..]
                .iter()
                .take_while(|b| matches!(b, b' ' | b'\t' | b'\n' | b'\r'))
                .count();
        let rest = &self.bytes[start..];
        if rest.is_empty() {
            return "the end of the input".to_string();
        }
        let line = rest
            .split(|b| matches!(b, b'\n' | b'\r'))
            .next()
            .unwrap_or(rest);
        let text = String::from_utf8_lossy(line);
        let snippet: String = text.chars().take(SNIPPET).collect();
        let ellipsis = if text.chars().count() > SNIPPET || line.len() < rest.len() {
            "…"
        } else {
            ""
        };
        format!("`{snippet}{ellipsis}` at offset {start}")
    }

    fn parse_int(&mut self) -> Option<Value> {
        self.skip_ws();
        let start = self.pos;
        if self.peek() == Some(b'-') {
            self.pos += 1;
        }
        let digits_start = self.pos;
        while self.peek().is_some_and(|b| b.is_ascii_digit()) {
            self.pos += 1;
        }
        if self.pos == digits_start {
            self.pos = start;
            return None;
        }
        let text = std::str::from_utf8(&self.bytes[start..self.pos]).ok()?;
        match text.parse::<i64>() {
            Ok(n) => Some(Value::int(n)),
            // Outside i64: the canonical big form (`Value::integer` re-canonicalises).
            Err(_) => text.parse::<BigInt>().ok().map(Value::integer),
        }
    }

    /// Consume the whitespace between two things inside a binary literal.
    fn eat_binary_gap(&mut self) -> Gap {
        let start = self.pos;
        let mut gap = Gap::None;
        while let Some(byte) = self.peek() {
            match byte {
                // A line break anywhere in the run decides the whole gap, so the
                // indentation that follows one does not downgrade it back to inline.
                b' ' | b'\t' if matches!(gap, Gap::None) => gap = Gap::Inline,
                b' ' | b'\t' => {}
                b'\n' | b'\r' => gap = Gap::Line,
                _ => break,
            }
            self.pos += 1;
        }
        debug_assert!(self.pos > start || matches!(gap, Gap::None));
        gap
    }

    fn parse_binary<E: Effect>(
        &mut self,
        ctx: &mut BuiltinContext<E>,
    ) -> Result<Option<Value>, Error> {
        self.skip_ws();
        let start = self.pos;
        if !self.eat(b'<') {
            self.pos = start;
            return Ok(None);
        }
        // Whole-byte groups separated by whitespace, exactly as a source literal is written:
        // on one line the brackets stay tight against the digits, and only a line break in
        // the gap lets them sit apart. `encode` emits neither grouping nor rows, but the
        // notation is the literal syntax, so text that a person wrote reads back.
        let mut bytes = Vec::new();
        // The gap after `<` pads a bracket rather than separating anything.
        let mut gap = self.eat_binary_gap();
        if matches!(gap, Gap::Inline) {
            self.pos = start;
            return Ok(None);
        }
        loop {
            let group_start = self.pos;
            while self.peek().is_some_and(|b| b.is_ascii_hexdigit()) {
                self.pos += 1;
            }
            if self.pos == group_start {
                // No group follows, so the gap we just crossed pads the closing bracket.
                if matches!(gap, Gap::Inline) {
                    self.pos = start;
                    return Ok(None);
                }
                break;
            }
            let group = &self.bytes[group_start..self.pos];
            if !group.len().is_multiple_of(2) {
                self.pos = start;
                return Ok(None);
            }
            bytes.extend(group.chunks(2).map(|pair| {
                let hi = (pair[0] as char).to_digit(16).unwrap() as u8;
                let lo = (pair[1] as char).to_digit(16).unwrap() as u8;
                (hi << 4) | lo
            }));
            gap = self.eat_binary_gap();
        }
        if !self.eat(b'>') {
            self.pos = start;
            return Ok(None);
        }
        let binary = ctx.executor.allocate_binary(bytes)?;
        Ok(Some(Value::Binary(binary)))
    }

    /// A `"…"` literal's bytes. The recognised escapes are the language's (`\n`, `\r`,
    /// `\t`, `\\`, `\"`, `\{`); there is no interpolation, so a bare `{` is literal. A
    /// raw newline fails (single-line literals only).
    fn parse_string(&mut self) -> Option<Vec<u8>> {
        let start = self.pos;
        if !self.eat(b'"') {
            return None;
        }
        let mut out = Vec::new();
        loop {
            let b = self.peek()?;
            self.pos += 1;
            match b {
                b'"' => return Some(out),
                b'\n' | b'\r' => {
                    self.pos = start;
                    return None;
                }
                b'\\' => {
                    let escaped = self.peek()?;
                    self.pos += 1;
                    match escaped {
                        b'n' => out.push(b'\n'),
                        b'r' => out.push(b'\r'),
                        b't' => out.push(b'\t'),
                        b'\\' => out.push(b'\\'),
                        b'"' => out.push(b'"'),
                        b'{' => out.push(b'{'),
                        _ => {
                            self.pos = start;
                            return None;
                        }
                    }
                }
                b => out.push(b),
            }
        }
    }

    /// A field label: `ident:` (with the identifier's optional `?`/`!` suffixes),
    /// answering the identifier text.
    fn parse_label(&mut self) -> Option<String> {
        let start = self.pos;
        if !self.peek().is_some_and(|b| b.is_ascii_lowercase()) {
            return None;
        }
        self.pos += 1;
        while self
            .peek()
            .is_some_and(|b| b.is_ascii_alphanumeric() || b == b'_')
        {
            self.pos += 1;
        }
        if self.peek() == Some(b'?') {
            self.pos += 1;
        }
        if self.peek() == Some(b'!') {
            self.pos += 1;
        }
        let name = std::str::from_utf8(&self.bytes[start..self.pos])
            .ok()?
            .to_string();
        if !self.eat(b':') {
            self.pos = start;
            return None;
        }
        Some(name)
    }

    fn skip_ws(&mut self) {
        while self
            .peek()
            .is_some_and(|b| matches!(b, b' ' | b'\t' | b'\n' | b'\r'))
        {
            self.pos += 1;
        }
    }

    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.pos).copied()
    }

    fn eat(&mut self, expected: u8) -> bool {
        if self.peek() == Some(expected) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn eat_exact(&mut self, expected: &[u8]) -> bool {
        if self.bytes[self.pos..].starts_with(expected) {
            self.pos += expected.len();
            true
        } else {
            false
        }
    }
}

/// Whether a tuple shape is the `Str` sugar's: named `Str`, one unnamed binary field.
fn is_str_shape(info: &TupleTypeInfo, lookup: &impl TypeLookup) -> bool {
    info.name.as_deref() == Some("Str")
        && matches!(
            info.fields.as_slice(),
            [(None, field_type)] if matches!(lookup.lookup_type(*field_type), Some(Type::Binary))
        )
}
