use crate::bytecode::Constant;
use crate::program::Program;
use crate::types::{TupleTypeInfo, Type, TypeLookup};
use crate::value::{Binary, Value};
use num_bigint::BigInt;
use num_traits::{One, Signed, Zero};

/// Trait for looking up binary data from Binary references
pub trait BinaryLookup {
    /// The bytes for a binary reference, or `None` if it cannot be resolved. Owned, because a
    /// binary that owns its bytes may be a rope with no contiguous slice to lend.
    fn get_bytes(&self, binary: &Binary) -> Option<Vec<u8>>;
}

/// Helper function to format bytes as a string if they represent valid UTF-8 text
fn try_format_as_string(bytes: &[u8]) -> Option<String> {
    let s = String::from_utf8(bytes.to_vec()).ok()?;

    if s.contains('\0')
        || s.chars()
            .any(|c| c.is_control() && !matches!(c, '\n' | '\r' | '\t'))
    {
        return None;
    }

    // NOT truncated, unlike `format_binary`: a `Str` is text meant to be read, and this
    // rendering is what test assertions and `%data` round trips compare against — HTTP
    // responses and rendered HTML routinely run to hundreds of characters. Bounding the
    // display belongs at the display layer, not here.
    let escaped = s
        .chars()
        .map(|ch| match ch {
            '"' => "\\\"".to_string(),
            '\\' => "\\\\".to_string(),
            '\n' => "\\n".to_string(),
            '\r' => "\\r".to_string(),
            '\t' => "\\t".to_string(),
            c if c.is_control() => format!("\\u{{{:04x}}}", c as u32),
            c => c.to_string(),
        })
        .collect::<String>();

    Some(format!("\"{}\"", escaped))
}

/// Format a type by its ID
pub fn format_type_by_id(lookup: &impl TypeLookup, type_id: usize) -> String {
    if let Some(type_def) = lookup.lookup_type(type_id) {
        format_type(lookup, type_def)
    } else {
        format!("Type{}", type_id)
    }
}

pub fn format_type(lookup: &impl TypeLookup, type_def: &Type) -> String {
    format_type_impl(lookup, type_def, false)
}

fn format_type_impl(lookup: &impl TypeLookup, type_def: &Type, nested: bool) -> String {
    match type_def {
        Type::Integer => "'int".to_string(),
        Type::Binary => "'bin".to_string(),
        Type::Reference => "'ref".to_string(),
        Type::Process {
            send,
            receive,
            state,
        } => {
            // Compose the written clause forms: `@'msg`, `@'msg !'r`, `@!'r`, and the
            // sampling clause ` ?'s` (a sigil glues directly after a bare `@`: `@?'s`).
            let fmt = |id: usize| {
                lookup
                    .lookup_type(id)
                    .map(|t| format_type_impl(lookup, t, true))
                    .unwrap_or_else(|| format!("Type{}", id))
            };
            let mut formatted = "@".to_string();
            if let Some(id) = send {
                formatted.push_str(&fmt(*id));
            }
            if let Some(id) = receive {
                if send.is_some() {
                    formatted.push(' ');
                }
                formatted.push('!');
                formatted.push_str(&fmt(*id));
            }
            if let Some(id) = state {
                if send.is_some() || receive.is_some() {
                    formatted.push(' ');
                }
                formatted.push('?');
                formatted.push_str(&fmt(*id));
            }

            // Only the clause forms need parenthesising in a nested position — nested,
            // their clauses would otherwise bind to the enclosing sigil-head.
            if nested && (receive.is_some() || state.is_some()) {
                format!("({})", formatted)
            } else {
                formatted
            }
        }
        Type::Tuple(tuple_id) => format_tuple_type(lookup, *tuple_id),
        // An annotated type prints its base followed by the attach syntax; openness is
        // not rendered (it is explained in prose by the visibility diagnostics).
        Type::Annotated { base, entries, .. } => {
            // An empty row is invisible in display: render the base exactly as if it
            // were unwrapped (same nesting). A non-empty row parenthesises the base,
            // which is followed by the attach-syntax braces.
            if entries.is_empty() {
                return lookup
                    .lookup_type(*base)
                    .map(|t| format_type_impl(lookup, t, nested))
                    .unwrap_or_else(|| format!("Type{}", base));
            }
            let base_str = lookup
                .lookup_type(*base)
                .map(|t| format_type_impl(lookup, t, true))
                .unwrap_or_else(|| format!("Type{}", base));
            let rendered: Vec<String> = entries
                .iter()
                .map(|(key, value_type)| {
                    let key_name = lookup
                        .lookup_annotation_key_name(*key)
                        .map(str::to_string)
                        .unwrap_or_else(|| format!("key#{}", key));
                    format!(":{} {}", key_name, format_type_by_id(lookup, *value_type))
                })
                .collect();
            format!("{} {{ {} }}", base_str, rendered.join(", "))
        }
        Type::Partial { name, fields } => format_partial_type(lookup, name.as_ref(), fields),
        Type::Callable {
            parameter,
            result,
            receive,
            states,
            omittable,
        } => {
            let fmt = |id: usize| {
                lookup
                    .lookup_type(id)
                    .map(|t| format_type_impl(lookup, t, true))
                    .unwrap_or_else(|| format!("Type{}", id))
            };
            let mut formatted = format!(
                "#{} -> {}",
                format_parameter_type(lookup, *parameter, omittable),
                fmt(*result)
            );
            // The written clause forms: ` !'recv` when the function receives, ` ?'states`
            // when its states go beyond its parameter (the parameter alone is the implicit
            // seed and stays unwritten; an unknown-states callable also renders bare).
            if lookup.lookup_type(*receive).map(|t| t.is_never()) != Some(true) {
                formatted.push_str(" !");
                formatted.push_str(&fmt(*receive));
            }
            if let Some(s) = states
                && s != parameter
            {
                formatted.push_str(" ?");
                formatted.push_str(&fmt(*s));
            }

            if nested {
                format!("({})", formatted)
            } else {
                formatted
            }
        }
        Type::Cycle(depth) => format!("μ{}", depth),
        Type::Union(type_ids) => {
            if type_ids.is_empty() {
                "never".to_string()
            } else {
                let mut type_strs: Vec<String> = type_ids
                    .iter()
                    .map(|&id| {
                        lookup
                            .lookup_type(id)
                            .map(|t| format_type_impl(lookup, t, true))
                            .unwrap_or_else(|| format!("Type{}", id))
                    })
                    .collect();

                // Sort for consistent output
                type_strs.sort();

                if nested {
                    format!("({})", type_strs.join(" | "))
                } else {
                    type_strs.join(" | ")
                }
            }
        }
        Type::Intersection(type_ids) => {
            let formatted = type_ids
                .iter()
                .map(|&id| {
                    lookup
                        .lookup_type(id)
                        .map(|t| format_type_impl(lookup, t, true))
                        .unwrap_or_else(|| format!("Type{}", id))
                })
                .collect::<Vec<_>>()
                .join(" & ");
            if nested {
                format!("({})", formatted)
            } else {
                formatted
            }
        }
        Type::Resource(name) => format!("+{}", name),
        // A `#N` suffix is the compiler's per-definition uniquifier, not part of the
        // source-level name.
        Type::Variable(name) => format!("'{}", name.split('#').next().unwrap_or(name)),
        Type::Top => "_".to_string(),
    }
}

/// Format a tuple type by its tuple_id
/// A function type's parameter, with `(name):` on the labels it lets callers omit — the
/// marks are part of the type, so a displayed signature should show the convention it
/// grants. Renders ordinarily when nothing is marked.
fn format_parameter_type(
    lookup: &impl TypeLookup,
    parameter: usize,
    omittable: &[usize],
) -> String {
    let plain = || {
        lookup
            .lookup_type(parameter)
            .map(|t| format_type_impl(lookup, t, true))
            .unwrap_or_else(|| format!("Type{}", parameter))
    };
    if omittable.is_empty() {
        return plain();
    }
    let Some(Type::Tuple(tuple_id)) = lookup.lookup_type(parameter) else {
        return plain();
    };
    let Some(info) = lookup.lookup_tuple(*tuple_id) else {
        return plain();
    };
    let fields: Vec<String> = info
        .fields
        .iter()
        .enumerate()
        .map(|(index, (field_name, field_type_id))| {
            let rendered = lookup
                .lookup_type(*field_type_id)
                .map(|t| format_type_impl(lookup, t, true))
                .unwrap_or_else(|| format!("Type{}", field_type_id));
            match field_name {
                Some(name) if omittable.contains(&index) => format!("({name}): {rendered}"),
                Some(name) => format!("{name}: {rendered}"),
                None => rendered,
            }
        })
        .collect();
    match &info.name {
        Some(name) => format!("{}[{}]", name, fields.join(", ")),
        None => format!("[{}]", fields.join(", ")),
    }
}

fn format_tuple_type(lookup: &impl TypeLookup, tuple_id: usize) -> String {
    if let Some(type_info) = lookup.lookup_tuple(tuple_id) {
        format_tuple_info(lookup, type_info)
    } else {
        format!("Tuple{}", tuple_id)
    }
}

/// Format a partial type with inline fields
fn format_partial_type(
    lookup: &impl TypeLookup,
    name: Option<&String>,
    fields: &[(String, usize)],
) -> String {
    let field_strs: Vec<String> = fields
        .iter()
        .map(|(field_name, field_type_id)| {
            let field_type_str = lookup
                .lookup_type(*field_type_id)
                .map(|t| format_type_impl(lookup, t, true))
                .unwrap_or_else(|| format!("Type{}", field_type_id));
            format!("{}: {}", field_name, field_type_str)
        })
        .collect();

    if let Some(type_name) = name {
        if field_strs.is_empty() {
            format!("{}()", type_name)
        } else {
            format!("{}({})", type_name, field_strs.join(", "))
        }
    } else {
        format!("({})", field_strs.join(", "))
    }
}

/// Format a TupleTypeInfo using a TypeLookup for field type resolution
pub fn format_tuple_info(lookup: &impl TypeLookup, tuple_info: &TupleTypeInfo) -> String {
    let field_strs: Vec<String> = tuple_info
        .fields
        .iter()
        .map(|(field_name, field_type_id)| {
            let field_type_str = lookup
                .lookup_type(*field_type_id)
                .map(|t| format_type_impl(lookup, t, true))
                .unwrap_or_else(|| format!("Type{}", field_type_id));
            if let Some(field_name) = field_name {
                format!("{}: {}", field_name, field_type_str)
            } else {
                field_type_str
            }
        })
        .collect();

    if let Some(type_name) = &tuple_info.name {
        if field_strs.is_empty() {
            type_name.to_string()
        } else {
            format!("{}[{}]", type_name, field_strs.join(", "))
        }
    } else {
        format!("[{}]", field_strs.join(", "))
    }
}

/// Format a binary value showing its actual content
/// Number of leading bytes shown before a long binary is truncated.
const BINARY_DISPLAY_BYTES: usize = 8;

fn format_binary(bytes: &[u8]) -> String {
    if bytes.len() <= BINARY_DISPLAY_BYTES {
        format!("<{}>", hex::encode(bytes))
    } else {
        // Show a prefix and the total length for long binaries.
        format!(
            "<{}…> ({} bytes)",
            hex::encode(&bytes[..BINARY_DISPLAY_BYTES]),
            bytes.len()
        )
    }
}

/// Standard implementation of BinaryLookup using heap and program constants
pub struct HeapAndProgramLookup<'a> {
    pub heap: &'a [Vec<u8>],
    pub program: &'a Program,
}

impl BinaryLookup for HeapAndProgramLookup<'_> {
    fn get_bytes(&self, binary: &Binary) -> Option<Vec<u8>> {
        match binary {
            Binary::Constant(idx) => match self.program.get_constant(*idx) {
                Some(Constant::Binary(bytes)) => Some(bytes.clone()),
                _ => None,
            },
            Binary::Data(data) => Some(data.to_vec()),
        }
    }
}

/// Implementation of BinaryLookup using bytecode constants and heap
pub struct BytecodeBinaryLookup<'a> {
    pub constants: &'a [Constant],
    pub heap: &'a [Vec<u8>],
}

impl BinaryLookup for BytecodeBinaryLookup<'_> {
    fn get_bytes(&self, binary: &Binary) -> Option<Vec<u8>> {
        match binary {
            Binary::Constant(idx) => self.constants.get(*idx).and_then(|c| match c {
                Constant::Binary(bytes) => Some(bytes.clone()),
                _ => None,
            }),
            Binary::Data(data) => Some(data.to_vec()),
        }
    }
}

/// Describe why a nil result failed: its `:error` payload where it carries one, else the kind of
/// failure its provenance stamp records, followed by the stamped site (debug builds) — e.g.
/// `error DivisionByZero at num:12:9`, or `no branch matched at shapes:12:9`. `None` when the
/// value carries neither (release builds without an error, or nil data).
pub fn describe_failure<T: TypeLookup, B: BinaryLookup>(
    value: &Value,
    annotation_keys: &[String],
    type_lookup: &T,
    binary_lookup: &B,
) -> Option<String> {
    let annotation = |name: &str| {
        let key = annotation_keys.iter().position(|key| key == name)?;
        value.get_annotation(key)
    };
    let error = annotation("error")
        .map(|error| format!("error {}", format_value(error, type_lookup, binary_lookup)));
    let origin =
        annotation("origin").and_then(|site| describe_origin(site, type_lookup, binary_lookup));
    match (error, origin) {
        (Some(error), Some((_, site))) => Some(format!("{error} at {site}")),
        (Some(error), None) => Some(error),
        (None, Some((kind, site))) => Some(format!("{kind} at {site}")),
        (None, None) => None,
    }
}

/// Read an `origin` stamp as the kind of failure it records and its site, e.g.
/// `("no branch matched", "shapes:12:9")`.
fn describe_origin<T: TypeLookup, B: BinaryLookup>(
    site: &Value,
    type_lookup: &T,
    binary_lookup: &B,
) -> Option<(&'static str, String)> {
    let Value::Tuple(_, fields) = site else {
        return None;
    };
    let [module, line, column, kind] = &fields[..] else {
        return None;
    };
    let module = match module {
        Value::Tuple(_, str_fields) => match str_fields.first() {
            Some(Value::Binary(binary)) => binary_lookup
                .get_bytes(binary)
                .map(|bytes| String::from_utf8_lossy(&bytes).into_owned())?,
            _ => return None,
        },
        _ => return None,
    };
    let (Value::Int(line), Value::Int(column)) = (line, column) else {
        return None;
    };
    let describe_kind = match kind {
        Value::Tuple(kind_tuple, _) => match type_lookup
            .lookup_tuple(*kind_tuple)
            .and_then(|info| info.name.as_deref())
        {
            Some("NoMatch") => "match failed",
            Some("BlockExhausted") => "no branch matched",
            _ => "nil result",
        },
        _ => "nil result",
    };
    Some((describe_kind, format!("{module}:{line}:{column}")))
}

/// One step of an iterative value render: a value still to format, or literal text to emit.
enum Emit<'a> {
    Value(&'a Value),
    Text(&'a str),
}

/// Render a value.
///
/// **Iterative.** A value nests arbitrarily deep — a cons list is nested once per element — and
/// a recursive render aborted the process on a 200,000-element list. Displaying a value is the
/// last thing that should be able to kill it, and an abort cannot be caught. The work-list holds
/// pending sub-values and the punctuation between them, so depth costs heap rather than stack.
pub fn format_value<T: TypeLookup, B: BinaryLookup>(
    value: &Value,
    type_lookup: &T,
    binary_lookup: &B,
) -> String {
    let mut out = String::new();
    let mut pending = vec![Emit::Value(value)];
    while let Some(emit) = pending.pop() {
        match emit {
            Emit::Text(text) => out.push_str(text),
            Emit::Value(value) => {
                format_step(value, type_lookup, binary_lookup, &mut out, &mut pending)
            }
        }
    }
    out
}

/// Emit one value: leaves render straight into `out`, a composite emits its opening bracket and
/// pushes its fields (and the punctuation between them) onto `pending` in reverse, so they pop
/// in reading order.
fn format_step<'a, T: TypeLookup, B: BinaryLookup>(
    value: &'a Value,
    type_lookup: &'a T,
    binary_lookup: &B,
    out: &mut String,
    pending: &mut Vec<Emit<'a>>,
) {
    match value {
        Value::Function(function, _) => out.push_str(&format!("#{}", function)),
        Value::Builtin(name, _) => out.push_str(&format!("__{}__", name)),
        Value::Int(n) => out.push_str(&n.to_string()),
        Value::BigInt(n) => out.push_str(&n.to_string()),
        Value::Binary(binary) => match binary_lookup.get_bytes(binary) {
            Some(bytes) => out.push_str(&format_binary(&bytes)),
            None => out.push_str("<binary>"),
        },
        Value::Process(process_id, _) => out.push_str(&format!("@{}", process_id)),
        Value::Resource(resource_id, _) => out.push_str(&format!("+#{}", resource_id)),
        Value::Reference(r) => {
            let worker_id = r >> 48;
            let counter = r & 0xFFFFFFFFFFFF;
            out.push_str(&format!("&{}:{}", worker_id, counter));
        }
        Value::Tuple(tuple_id, elements) => {
            let Some(tuple_info) = type_lookup.lookup_tuple(*tuple_id) else {
                // Fallback: the tuple's type is not in this program's tables.
                if elements.is_empty() {
                    out.push_str(&format!("T{}", tuple_id));
                } else {
                    out.push_str(&format!("T{}[", tuple_id));
                    push_fields(elements, None, pending);
                }
                return;
            };

            // Shapes with their own notation render whole, and are leaves as far as the
            // work-list is concerned.
            if tuple_info.name.as_deref() == Some("Str")
                && let [Value::Binary(binary)] = &elements[..]
                && let Some(bytes) = binary_lookup.get_bytes(binary)
                && let Some(s) = try_format_as_string(&bytes)
            {
                out.push_str(&s);
                return;
            }
            if tuple_info.name.as_deref() == Some("Rational")
                && let [numer, denom] = &elements[..]
                && let (Some(numer), Some(denom)) = (numer.as_int(), denom.as_int())
            {
                out.push_str(&format!("{}/{}", numer, denom));
                return;
            }
            if tuple_info.name.as_deref() == Some("Surd")
                && let [a_val, b_val, radicand] = &elements[..]
                && let Some(radicand) = radicand.as_int()
                && let Some((an, ad)) = coeff_ratio(a_val, type_lookup)
                && let Some((bn, bd)) = coeff_ratio(b_val, type_lookup)
            {
                out.push_str(&format_surd(&an, &ad, &bn, &bd, &radicand.to_bigint()));
                return;
            }

            if tuple_info.name.as_deref() == Some("Tx")
                && let [generator, terms] = &elements[..]
                && let Some(generator) = transcendental_generator(generator, type_lookup)
                && let Some(terms) = term_list(terms, type_lookup)
            {
                out.push_str(&format_tx(&generator, &terms));
                return;
            }
            if tuple_info.name.as_deref() == Some("Log")
                && let [constant, terms] = &elements[..]
                && let Some(constant) = coeff_ratio(constant, type_lookup)
                && let Some(terms) = term_list(terms, type_lookup)
            {
                out.push_str(&format_log(&constant, &terms));
                return;
            }

            let name = tuple_info.name.as_deref();
            if elements.is_empty() {
                out.push_str(name.unwrap_or("[]"));
                return;
            }
            if let Some(name) = name {
                out.push_str(name);
            }
            out.push('[');
            push_fields(elements, Some(tuple_info), pending);
        }
    }
}

/// Push a composite's fields onto the work-list in reverse, with `, ` between them, each
/// field's label (if any) ahead of it, and the closing bracket last — so popping yields
/// `a: 1, b: 2]`.
fn push_fields<'a>(
    elements: &'a [Value],
    tuple_info: Option<&'a TupleTypeInfo>,
    pending: &mut Vec<Emit<'a>>,
) {
    pending.push(Emit::Text("]"));
    for (index, element) in elements.iter().enumerate().rev() {
        pending.push(Emit::Value(element));
        if let Some(Some(field_name)) = tuple_info
            .and_then(|info| info.fields.get(index))
            .map(|(name, _)| name.as_ref())
        {
            pending.push(Emit::Text(": "));
            pending.push(Emit::Text(field_name));
        }
        if index > 0 {
            pending.push(Emit::Text(", "));
        }
    }
}

/// Interpret a surd coefficient — a bare integer or a `Rational[n, d]` tuple — as `(n, d)`.
fn coeff_ratio<T: TypeLookup>(value: &Value, lookup: &T) -> Option<(BigInt, BigInt)> {
    if let Some(n) = value.as_int() {
        return Some((n.to_bigint(), BigInt::one()));
    }
    match value {
        Value::Tuple(tuple_id, elements) => {
            let info = lookup.lookup_tuple(*tuple_id)?;
            if info.name.as_deref() == Some("Rational")
                && let [n, d] = &elements[..]
                && let (Some(n), Some(d)) = (n.as_int(), d.as_int())
            {
                return Some((n.to_bigint(), d.to_bigint()));
            }
            None
        }
        _ => None,
    }
}

/// The generator of a `%num` transcendental polynomial: π, or e^(1/d).
enum Generator {
    Pi,
    E(BigInt),
}

fn transcendental_generator<T: TypeLookup>(value: &Value, lookup: &T) -> Option<Generator> {
    let Value::Tuple(tuple_id, elements) = value else {
        return None;
    };
    match (
        lookup.lookup_tuple(*tuple_id)?.name.as_deref(),
        &elements[..],
    ) {
        (Some("Pi"), []) => Some(Generator::Pi),
        (Some("E"), [d]) => Some(Generator::E(d.as_int()?.to_bigint())),
        _ => None,
    }
}

/// A `%num` term list — `Cons[[key, coeff], …]` ending in `Nil` — as `(key, (n, d))` pairs.
fn term_list<T: TypeLookup>(value: &Value, lookup: &T) -> Option<Vec<(BigInt, (BigInt, BigInt))>> {
    let mut terms = Vec::new();
    let mut current = value;
    loop {
        let Value::Tuple(tuple_id, elements) = current else {
            return None;
        };
        match (
            lookup.lookup_tuple(*tuple_id)?.name.as_deref(),
            &elements[..],
        ) {
            (Some("Nil"), []) => return Some(terms),
            (Some("Cons"), [Value::Tuple(_, pair), rest]) => {
                let [key, coeff] = &pair[..] else {
                    return None;
                };
                terms.push((key.as_int()?.to_bigint(), coeff_ratio(coeff, lookup)?));
                current = rest;
            }
            _ => return None,
        }
    }
}

fn superscript(n: &BigInt) -> String {
    n.to_string()
        .chars()
        .map(|c| match c {
            '-' => '⁻',
            '0' => '⁰',
            '1' => '¹',
            '2' => '²',
            '3' => '³',
            '4' => '⁴',
            '5' => '⁵',
            '6' => '⁶',
            '7' => '⁷',
            '8' => '⁸',
            '9' => '⁹',
            c => unreachable!("integer rendered with non-digit {c:?}"),
        })
        .collect()
}

/// Join signed terms as a sum: the first carries its own sign, later ones a ` + `/` - `
/// connector. Each term is given as (negative, magnitude text).
fn join_terms(terms: impl IntoIterator<Item = (bool, String)>) -> String {
    let mut out = String::new();
    for (i, (negative, magnitude)) in terms.into_iter().enumerate() {
        match (i, negative) {
            (0, true) => out.push('-'),
            (0, false) => {}
            (_, true) => out.push_str(" - "),
            (_, false) => out.push_str(" + "),
        }
        out.push_str(&magnitude);
    }
    out
}

/// A rational's magnitude on its own: `3`, `1/2`.
fn magnitude_of(n: &BigInt, d: &BigInt) -> String {
    if d.is_one() {
        n.abs().to_string()
    } else {
        format!("{}/{d}", n.abs())
    }
}

/// A coefficient's magnitude in front of a symbol, in the surd style: omitted when one, bare
/// when integral (`2π`), parenthesised when a fraction (`(1/2)π`).
fn coefficient_of(n: &BigInt, d: &BigInt, symbol: &str) -> String {
    let n = n.abs();
    if n.is_one() && d.is_one() {
        symbol.to_string()
    } else if d.is_one() {
        format!("{n}{symbol}")
    } else {
        format!("({n}/{d}){symbol}")
    }
}

/// Render a transcendental polynomial `Σ c·gᵏ`, ascending: `2π`, `(1/4)π²`, `180π⁻¹`,
/// `1 + π`, `e^(1/2)`.
fn format_tx(generator: &Generator, terms: &[(BigInt, (BigInt, BigInt))]) -> String {
    join_terms(terms.iter().map(|(k, (n, d))| {
        let power = match generator {
            Generator::Pi => match k {
                k if k.is_one() => "π".to_string(),
                k => format!("π{}", superscript(k)),
            },
            Generator::E(root) => {
                let g = num_integer::Integer::gcd(k, root);
                let (num, den) = (k / &g, root / &g);
                match (num, den) {
                    (num, den) if den.is_one() && num.is_one() => "e".to_string(),
                    (num, den) if den.is_one() => format!("e{}", superscript(&num)),
                    (num, den) => format!("e^({num}/{den})"),
                }
            }
        };
        let magnitude = if k.is_zero() {
            magnitude_of(n, d)
        } else {
            coefficient_of(n, d, &power)
        };
        (n.is_negative(), magnitude)
    }))
}

/// Render a logarithmic form `a + Σ c·ln p`: `ln 2`, `2 ln 2 + ln 3`, `1 - (1/2) ln 3`.
fn format_log(constant: &(BigInt, BigInt), terms: &[(BigInt, (BigInt, BigInt))]) -> String {
    let (an, ad) = constant;
    let head = (!an.is_zero()).then(|| (an.is_negative(), magnitude_of(an, ad)));
    join_terms(head.into_iter().chain(terms.iter().map(|(p, (n, d))| {
        let symbol = format!("ln {p}");
        let magnitude = if n.abs().is_one() && d.is_one() {
            symbol
        } else {
            format!("{} {symbol}", coefficient_of(n, d, ""))
        };
        (n.is_negative(), magnitude)
    })))
}

/// Render a single-radical surd `(an/ad) + (bn/bd)·√radicand` as e.g. `√2`, `2√2`, `(1/2)√2`,
/// `1 + √2`, or `3 - 2√2`. Coefficients are canonical (`bn ≠ 0`, denominators positive).
fn format_surd(an: &BigInt, ad: &BigInt, bn: &BigInt, bd: &BigInt, radicand: &BigInt) -> String {
    let coeff = |n: &BigInt, d: &BigInt| {
        if d.is_one() {
            n.to_string()
        } else {
            format!("{}/{}", n, d)
        }
    };

    // The √ term, using |b| (its sign is carried by the connector below).
    let bn_abs = bn.abs();
    let magnitude = if bn_abs.is_one() && bd.is_one() {
        String::new() // coefficient 1 → just `√n`
    } else if bd.is_one() {
        bn_abs.to_string() // integer coefficient → `k√n`
    } else {
        format!("({})", coeff(&bn_abs, bd)) // rational coefficient → `(p/q)√n`
    };
    let term = format!("{}√{}", magnitude, radicand);

    if an.is_zero() {
        if bn.is_negative() {
            format!("-{}", term)
        } else {
            term
        }
    } else {
        let connector = if bn.is_negative() { " - " } else { " + " };
        format!("{}{}{}", coeff(an, ad), connector, term)
    }
}
