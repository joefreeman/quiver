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

use super::{BuiltinContext, Completion};
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

/// `__data_decode__<'t>`: notation text → a value of the expected type, or nil. The
/// expected type is the builtin's explicit type argument, read from the call — a bare
/// (un-instantiated) reference that ends up called is a runtime error.
pub fn builtin_data_decode<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let expected = ctx.type_argument().ok_or_else(|| {
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

    let mut decoder = Decoder {
        ctx,
        bytes: &bytes,
        pos: 0,
    };
    let value = decoder.decode_type(expected, &mut BinderStack::default())?;
    // The whole input must be one value: trailing non-whitespace fails the decode.
    decoder.skip_ws();
    let value = match value {
        Some(v) if decoder.pos == decoder.bytes.len() => Some(v),
        _ => None,
    };
    Ok(Completion::Value(value.unwrap_or_else(Value::nil)))
}

/// A type-directed recursive-descent parser over the notation. Unions are ordered
/// choice with per-member backtracking (reset to the saved position); the one place a
/// successfully-parsed member could otherwise be a strict prefix of a sibling is closed
/// by lookahead instead of full backtracking — a bare named-empty tuple is never
/// followed by a glued `[` (that is the named-fields form). Recursive types resolve
/// through a binder stack, as the compatibility checker's do.
/// The whitespace between two things inside a binary literal. Between groups any gap will
/// do; against a bracket only a line break will, which is what keeps `< 0a>` malformed while
/// letting a literal be written as a table across lines.
enum Gap {
    None,
    Inline,
    Line,
}

struct Decoder<'a, 'b, 'c, E: Effect> {
    ctx: &'a mut BuiltinContext<'b, E>,
    bytes: &'c [u8],
    pos: usize,
}

impl<E: Effect> Decoder<'_, '_, '_, E> {
    fn decode_type(
        &mut self,
        type_id: usize,
        stack: &mut BinderStack,
    ) -> Result<Option<Value>, Error> {
        let Some(typ) = TypeLookup::lookup_type(&*self.ctx.executor, type_id).cloned() else {
            return Ok(None);
        };
        match typ {
            // Rows are invisible to the data plane: decode as the base shape.
            Type::Annotated { base, .. } => self.decode_type(base, stack),
            Type::Union(members) => {
                stack.enter(type_id);
                let start = self.pos;
                let mut result = None;
                for member in members {
                    self.pos = start;
                    if let Some(value) = self.decode_type(member, stack)? {
                        result = Some(value);
                        break;
                    }
                }
                stack.leave(type_id);
                if result.is_none() {
                    self.pos = start;
                }
                Ok(result)
            }
            Type::Cycle(depth) => {
                // Re-enter the target at its own depth, so the references inside it count the
                // binders they were written under.
                let Some((target, cut)) = stack.follow(depth) else {
                    return Ok(None);
                };
                let result = self.decode_type(target, stack);
                stack.restore(cut);
                result
            }
            Type::Integer => Ok(self.parse_int()),
            Type::Binary => self.parse_binary(),
            Type::Tuple(tuple_id) => self.decode_tuple(tuple_id, stack),
            // Not data: partials have no layout to construct, and callables, processes,
            // resources and type variables have no textual form.
            Type::Partial { .. }
            | Type::Callable { .. }
            | Type::Process { .. }
            | Type::Resource(_)
            | Type::Reference
            | Type::Variable(_)
            | Type::Intersection(_)
            | Type::Top => Ok(None),
        }
    }

    fn decode_tuple(
        &mut self,
        tuple_id: usize,
        stack: &mut BinderStack,
    ) -> Result<Option<Value>, Error> {
        let Some(info) = TypeLookup::lookup_tuple(&*self.ctx.executor, tuple_id).cloned() else {
            return Ok(None);
        };

        // `Str` sugar: a string literal is a `Str` tuple.
        if is_str_shape(&info, &*self.ctx.executor) {
            self.skip_ws();
            if self.peek() == Some(b'"') {
                let Some(bytes) = self.parse_string() else {
                    return Ok(None);
                };
                let binary = self.ctx.executor.allocate_binary(bytes)?;
                return Ok(Some(Value::tuple(
                    self.ctx.label(tuple_id, stack)?,
                    vec![Value::Binary(binary)],
                )));
            }
        }

        self.skip_ws();
        if let Some(name) = &info.name {
            if !self.eat_exact(name.as_bytes()) {
                return Ok(None);
            }
            // The name must end here — `OkThen` must not satisfy `Ok`.
            if self
                .peek()
                .is_some_and(|b| b.is_ascii_alphanumeric() || b == b'_')
            {
                return Ok(None);
            }
            if info.fields.is_empty() {
                // Bare named-empty form — unless a glued `[` follows, which belongs to
                // a sibling `Name[…]` member (the prefix-lookahead rule).
                if self.peek() == Some(b'[') {
                    return Ok(None);
                }
                return Ok(Some(Value::tuple(self.ctx.label(tuple_id, stack)?, vec![])));
            }
            // Named fields open with a *glued* bracket, as in code.
            if !self.eat(b'[') {
                return Ok(None);
            }
        } else {
            if !self.eat(b'[') {
                return Ok(None);
            }
            if info.fields.is_empty() {
                self.skip_ws();
                if !self.eat(b']') {
                    return Ok(None);
                }
                return Ok(Some(Value::tuple(self.ctx.label(tuple_id, stack)?, vec![])));
            }
        }

        let mut values = Vec::with_capacity(info.fields.len());
        for (i, (label, field_type)) in info.fields.iter().enumerate() {
            self.skip_ws();
            if i > 0 {
                if !self.eat(b',') {
                    return Ok(None);
                }
                self.skip_ws();
            }
            // An optional field label: a data value never starts with a lowercase
            // letter, so one unambiguously introduces a label — which must match the
            // expected field's name.
            if self.peek().is_some_and(|b| b.is_ascii_lowercase()) {
                let Some(written) = self.parse_label() else {
                    return Ok(None);
                };
                if label.as_deref() != Some(written.as_str()) {
                    return Ok(None);
                }
                self.skip_ws();
            }
            let Some(value) = self.decode_type(*field_type, stack)? else {
                return Ok(None);
            };
            values.push(value);
        }
        self.skip_ws();
        if self.eat(b',') {
            self.skip_ws();
        }
        if !self.eat(b']') {
            return Ok(None);
        }
        Ok(Some(Value::tuple(self.ctx.label(tuple_id, stack)?, values)))
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

    fn parse_binary(&mut self) -> Result<Option<Value>, Error> {
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
        let binary = self.ctx.executor.allocate_binary(bytes)?;
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
