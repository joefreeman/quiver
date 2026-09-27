//! The JSON typed boundary (`%json.decode<'t>` / `%json.encode<'t>`): conversion
//! between the `'%json` document model and ordinary typed values, driven by the
//! builtin's explicit type argument.
//!
//! Both directions walk a **value** in tandem with a type from the runtime tables —
//! the same type-directed interpretation as `%data.decode`, with a value descent in
//! place of its text descent. Decoding constructs results with the *expected type's*
//! tuple ids, so input can never mint a shape the program doesn't already contain;
//! encoding constructs `'%json` shapes with ids read off the builtin's own registered
//! result type, under the same discipline.
//!
//! The mapping: a JSON object ↔ a tuple with labelled fields (keys matched by label,
//! order-free; extra keys ignored; a field type admitting nil makes the key optional —
//! absent or `null` decode as nil, and a nil field is omitted on encode); a JSON array
//! ↔ a `%list`; strings, numbers, `true`/`false`/`null` ↔ `Str`, `'int` /
//! `Rational[…]`, `True`/`False`/`Null`. A `'%json`-typed part passes the raw subtree
//! through verbatim. Unions decode by ordered choice with backtracking, exactly as
//! `%data.decode`'s. Anything outside the mapping — partials, callables, processes,
//! refs, resources, binaries, unlabelled tuples — decodes as nil; encoding such a
//! value is a runtime error, like `%data.encode`'s.

use super::{BuiltinContext, Completion, TypeSpec};
use crate::binders::BinderStack;
use crate::effects::Effect;
use crate::error::Error;
use crate::types::{TupleTypeInfo, Type, TypeLookup};
use crate::value::Value;

// ===== signature ========================================================================

/// The `'%json` union as a signature spec, mirroring std/json.qv's `'`:
/// `Null | True | False | 'int | Rational['int, 'int] | Str['bin] | Array[…] | Object[…]`.
/// Runtime coherence with the std module's own registration is by canonical value
/// shape (names and labels), not id identity, so member order is free to differ.
pub(crate) fn json_spec() -> TypeSpec {
    fn str_spec() -> TypeSpec {
        TypeSpec::Tuple(Some("Str"), vec![(None, TypeSpec::Binary)])
    }
    // A `%list` of the element spec; `Cycle(1)` is the list union itself.
    fn list_of(element: TypeSpec) -> TypeSpec {
        TypeSpec::Union(vec![
            TypeSpec::Tuple(Some("Nil"), vec![]),
            TypeSpec::Tuple(
                Some("Cons"),
                vec![(None, element), (None, TypeSpec::Cycle(1))],
            ),
        ])
    }
    // Inside either list, `Cycle(2)` reads through the list union to the root.
    TypeSpec::Union(vec![
        TypeSpec::Tuple(Some("Null"), vec![]),
        TypeSpec::Tuple(Some("True"), vec![]),
        TypeSpec::Tuple(Some("False"), vec![]),
        TypeSpec::Integer,
        TypeSpec::Tuple(
            Some("Rational"),
            vec![(None, TypeSpec::Integer), (None, TypeSpec::Integer)],
        ),
        str_spec(),
        TypeSpec::Tuple(Some("Array"), vec![(None, list_of(TypeSpec::Cycle(2)))]),
        TypeSpec::Tuple(
            Some("Object"),
            vec![(
                None,
                list_of(TypeSpec::Tuple(
                    None,
                    vec![(None, str_spec()), (None, TypeSpec::Cycle(2))],
                )),
            )],
        ),
    ])
}

// ===== shared value recognition =========================================================

/// The tuple info of a tuple value, or None for non-tuples.
fn tuple_info<'a, E: Effect>(
    ctx: &'a BuiltinContext<E>,
    value: &Value,
) -> Option<(usize, &'a TupleTypeInfo)> {
    let Value::Tuple(tuple_id, _) = value else {
        return None;
    };
    TypeLookup::lookup_tuple(&*ctx.executor, *tuple_id).map(|info| (*tuple_id, info))
}

/// Whether a value is a named empty tuple with this name.
fn is_named_empty<E: Effect>(ctx: &BuiltinContext<E>, value: &Value, name: &str) -> bool {
    let Value::Tuple(_, payload) = value else {
        return false;
    };
    payload.is_empty()
        && tuple_info(ctx, value).is_some_and(|(_, info)| info.name.as_deref() == Some(name))
}

/// Whether a value is nil (the unnamed empty tuple).
fn is_nil_value<E: Effect>(ctx: &BuiltinContext<E>, value: &Value) -> bool {
    let Value::Tuple(_, payload) = value else {
        return false;
    };
    payload.is_empty() && tuple_info(ctx, value).is_some_and(|(_, info)| info.name.is_none())
}

/// An `Array[items]`-shaped value's items chain.
fn as_json_array<E: Effect>(ctx: &BuiltinContext<E>, value: &Value) -> Option<Value> {
    let Value::Tuple(_, payload) = value else {
        return None;
    };
    if payload.len() == 1
        && tuple_info(ctx, value).is_some_and(|(_, info)| info.name.as_deref() == Some("Array"))
    {
        Some(payload[0].clone())
    } else {
        None
    }
}

/// An object's pairs as read off the value: `(key bytes, value)`, in order.
type ObjectPairs = Vec<(Vec<u8>, Value)>;

/// An `Object[pairs]`-shaped value's pairs, in order.
fn as_json_object<E: Effect>(
    ctx: &mut BuiltinContext<E>,
    value: &Value,
) -> Result<Option<ObjectPairs>, Error> {
    let Value::Tuple(_, payload) = value else {
        return Ok(None);
    };
    if payload.len() != 1
        || tuple_info(ctx, value).is_none_or(|(_, info)| info.name.as_deref() != Some("Object"))
    {
        return Ok(None);
    }
    let mut pairs = Vec::new();
    let mut node = payload[0].clone();
    loop {
        let Some((_, info)) = tuple_info(ctx, &node) else {
            return Ok(None);
        };
        match (info.name.as_deref(), &node) {
            (Some("Nil"), _) => return Ok(Some(pairs)),
            (Some("Cons"), Value::Tuple(_, fields)) if fields.len() == 2 => {
                let Value::Tuple(_, pair) = &fields[0] else {
                    return Ok(None);
                };
                if pair.len() != 2 {
                    return Ok(None);
                }
                let Value::Tuple(_, key) = &pair[0] else {
                    return Ok(None);
                };
                let [Value::Binary(binary)] = &key[..] else {
                    return Ok(None);
                };
                let bytes = ctx.executor.get_binary_data(binary)?.to_vec();
                pairs.push((bytes, pair[1].clone()));
                let tail = fields[1].clone();
                node = tail;
            }
            _ => return Ok(None),
        }
    }
}

/// Whether a type admits nil (the unnamed empty tuple) — what makes a field optional.
fn admits_nil<E: Effect>(ctx: &BuiltinContext<E>, type_id: usize, seen: &mut Vec<usize>) -> bool {
    if seen.contains(&type_id) {
        return false;
    }
    seen.push(type_id);
    let result = match TypeLookup::lookup_type(&*ctx.executor, type_id) {
        Some(Type::Annotated { base, .. }) => {
            let base = *base;
            admits_nil(ctx, base, seen)
        }
        Some(Type::Union(members)) => {
            let members = members.clone();
            members.iter().any(|m| admits_nil(ctx, *m, seen))
        }
        Some(Type::Tuple(tuple_id)) => TypeLookup::lookup_tuple(&*ctx.executor, *tuple_id)
            .is_some_and(|info| info.name.is_none() && info.fields.is_empty()),
        _ => false,
    };
    seen.pop();
    result
}

// ===== decode ===========================================================================

/// `__json_decode__<'t>`: a `'%json` value (or nil, which passes through) → a value of
/// the expected type, or nil. The expected type is the builtin's explicit type
/// argument, read from the call.
pub fn builtin_json_decode<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let expected = ctx.type_argument().ok_or_else(|| {
        Error::InvalidArgument(
            "__json_decode__ called without a type argument — a bare reference carries \
             no instantiation; name it with one (`__json_decode__<'t>`) where the type \
             is concrete"
                .to_string(),
        )
    })?;
    let mut stack = BinderStack::default();
    let value = decode(ctx, expected, arg, &mut stack)?;
    Ok(Completion::Value(value.unwrap_or_else(Value::nil)))
}

/// Type-directed descent over a `'%json` value. Unions are ordered choice; recursive
/// types resolve through a binder stack, as the compatibility checker's do.
fn decode<E: Effect>(
    ctx: &mut BuiltinContext<E>,
    type_id: usize,
    value: &Value,
    stack: &mut BinderStack,
) -> Result<Option<Value>, Error> {
    if let Some(decoded) = decode_direct(ctx, type_id, value, stack)? {
        return Ok(Some(decoded));
    }
    // An `Array[items]` may satisfy a list-typed expectation: unwrap and decode the
    // chain itself (whose `Cons`/`Nil` nodes then match structurally).
    if let Some(items) = as_json_array(ctx, value) {
        return decode_direct(ctx, type_id, &items, stack);
    }
    Ok(None)
}

fn decode_direct<E: Effect>(
    ctx: &mut BuiltinContext<E>,
    type_id: usize,
    value: &Value,
    stack: &mut BinderStack,
) -> Result<Option<Value>, Error> {
    let Some(typ) = TypeLookup::lookup_type(&*ctx.executor, type_id).cloned() else {
        return Ok(None);
    };
    match typ {
        // Rows are invisible to the data plane: decode as the base shape.
        Type::Annotated { base, .. } => decode_direct(ctx, base, value, stack),
        Type::Union(members) => {
            stack.enter(type_id);
            let mut result = None;
            for member in &members {
                if let Some(decoded) = decode(ctx, *member, value, stack)? {
                    result = Some(decoded);
                    break;
                }
            }
            // JSON `null` collapses into an admitted nil — the optional-value rule —
            // when no member decodes it directly (a `'%json`-typed part takes it raw).
            if result.is_none()
                && is_named_empty(ctx, value, "Null")
                && admits_nil(ctx, type_id, &mut Vec::new())
            {
                result = Some(Value::nil());
            }
            stack.leave(type_id);
            Ok(result)
        }
        Type::Cycle(depth) => {
            // Re-enter the target at its own depth, so the references inside it count the
            // binders they were written under.
            let Some((target, cut)) = stack.follow(depth) else {
                return Ok(None);
            };
            let result = decode(ctx, target, value, stack);
            stack.restore(cut);
            result
        }
        Type::Integer => Ok(match value {
            Value::Int(_) | Value::BigInt(_) => Some(value.clone()),
            _ => None,
        }),
        Type::Tuple(tuple_id) => decode_tuple(ctx, tuple_id, value, stack),
        // Outside the mapping: partials have no layout to construct; callables,
        // processes, refs, resources and binaries have no JSON form.
        Type::Binary
        | Type::Partial { .. }
        | Type::Callable { .. }
        | Type::Process { .. }
        | Type::Resource(_)
        | Type::Reference
        | Type::Variable(_)
        | Type::Intersection(_)
        | Type::Top => Ok(None),
    }
}

fn decode_tuple<E: Effect>(
    ctx: &mut BuiltinContext<E>,
    expected_id: usize,
    value: &Value,
    stack: &mut BinderStack,
) -> Result<Option<Value>, Error> {
    let Value::Tuple(input_id, payload) = value else {
        return Ok(None);
    };
    // What the expected member is built as here: itself when closed, else closed against the
    // binders this walk entered to reach it.
    let built_as = ctx.label(expected_id, stack)?;
    // Already labelled exactly as a decoded value would be (ids agree on field types too), so
    // the subtree passes through whole. This is what makes a `'%json`-typed part capture raw
    // JSON, and covers `Str`/`Rational`/keyword leaves.
    if *input_id == built_as {
        return Ok(Some(value.clone()));
    }
    let Some(expected) = TypeLookup::lookup_tuple(&*ctx.executor, expected_id).cloned() else {
        return Ok(None);
    };
    let Some(input) = TypeLookup::lookup_tuple(&*ctx.executor, *input_id).cloned() else {
        return Ok(None);
    };

    // Structural rule: same name and labels — fields decode pairwise. This is how the
    // `Cons`/`Nil` nodes of an unwrapped array chain (and pairs, and differently-typed
    // `Str`s) decode across tuple-id families.
    if expected.name == input.name
        && expected.fields.len() == input.fields.len()
        && expected
            .fields
            .iter()
            .zip(input.fields.iter())
            .all(|((a, _), (b, _))| a == b)
    {
        let mut fields = Vec::with_capacity(payload.len());
        let mut matched = true;
        for (field_value, (_, field_type)) in payload.iter().zip(expected.fields.iter()) {
            match decode(ctx, *field_type, field_value, stack)? {
                Some(decoded) => fields.push(decoded),
                None => {
                    matched = false;
                    break;
                }
            }
        }
        if matched {
            return Ok(Some(Value::tuple(built_as, fields)));
        }
    }

    // Object mapping: a JSON object decodes into a tuple whose fields are all
    // labelled, keys matched by label (last binding wins, as in the document model),
    // extra keys ignored. A field type admitting nil makes its key optional: absent —
    // or `null`, via the union collapse — decodes as nil.
    if !expected.fields.is_empty() && expected.fields.iter().all(|(label, _)| label.is_some()) {
        let Some(pairs) = as_json_object(ctx, value)? else {
            return Ok(None);
        };
        let mut fields = Vec::with_capacity(expected.fields.len());
        for (label, field_type) in &expected.fields {
            let label = label.as_deref().unwrap_or_default().as_bytes();
            let entry = pairs.iter().rev().find(|(key, _)| key == label);
            match entry {
                Some((_, field_value)) => match decode(ctx, *field_type, field_value, stack)? {
                    Some(decoded) => fields.push(decoded),
                    None => return Ok(None),
                },
                None => {
                    if admits_nil(ctx, *field_type, &mut Vec::new()) {
                        fields.push(Value::nil());
                    } else {
                        return Ok(None);
                    }
                }
            }
        }
        return Ok(Some(Value::tuple(built_as, fields)));
    }

    Ok(None)
}

// ===== encode ===========================================================================

/// The `'%json` output shapes' tuple ids, read off the builtin's registered result
/// type — the same no-minting discipline as decode's expected-type ids — and labelled at the
/// binders a walk of it reaches each inside.
struct JsonIds {
    /// The registered `'%json` union itself — the target the encoder normalizes
    /// `'%json`-typed subtrees through.
    json_type: usize,
    array: usize,
    array_cons: usize,
    object: usize,
    object_cons: usize,
    pair: usize,
    nil: usize,
    null: usize,
    str: usize,
}

impl JsonIds {
    fn resolve<E: Effect>(ctx: &BuiltinContext<E>, result_type: usize) -> Result<Self, Error> {
        let lookup = &*ctx.executor;
        let missing = || {
            Error::InvalidArgument(
                "__json_encode__'s registered result type is not the '%json union".to_string(),
            )
        };
        let Some(Type::Union(members)) = lookup.lookup_type(result_type) else {
            return Err(missing());
        };
        let mut array = None;
        let mut object = None;
        let mut null = None;
        let mut str_id = None;
        // (list union id, cons id, nil id, element pair id when the element is a pair tuple)
        let list_parts = |field_type: usize| -> Option<(usize, usize, usize, Option<usize>)> {
            let Some(Type::Union(list_members)) = lookup.lookup_type(field_type) else {
                return None;
            };
            let mut cons = None;
            let mut nil = None;
            let mut pair = None;
            for member in list_members {
                let Some(Type::Tuple(tuple_id)) = lookup.lookup_type(*member) else {
                    return None;
                };
                let info = lookup.lookup_tuple(*tuple_id)?;
                match info.name.as_deref() {
                    Some("Nil") => nil = Some(*tuple_id),
                    Some("Cons") => {
                        cons = Some(*tuple_id);
                        if let Some(Type::Tuple(element_id)) =
                            lookup.lookup_type(info.fields.first()?.1)
                        {
                            pair = Some(*element_id);
                        }
                    }
                    _ => return None,
                }
            }
            Some((field_type, cons?, nil?, pair))
        };
        let mut array_parts = None;
        let mut object_parts = None;
        for member in members {
            let Some(Type::Tuple(tuple_id)) = lookup.lookup_type(*member) else {
                continue;
            };
            let Some(info) = lookup.lookup_tuple(*tuple_id) else {
                continue;
            };
            match info.name.as_deref() {
                Some("Array") => {
                    array = Some(*tuple_id);
                    array_parts = info.fields.first().and_then(|f| list_parts(f.1));
                }
                Some("Object") => {
                    object = Some(*tuple_id);
                    object_parts = info.fields.first().and_then(|f| list_parts(f.1));
                }
                Some("Null") => null = Some(*tuple_id),
                Some("Str") => str_id = Some(*tuple_id),
                _ => {}
            }
        }
        let (array_list, array_cons, nil, _) = array_parts.ok_or_else(missing)?;
        let (object_list, object_cons, _, pair) = object_parts.ok_or_else(missing)?;
        let (array, object, pair, null, str_id) = (
            array.ok_or_else(missing)?,
            object.ok_or_else(missing)?,
            pair.ok_or_else(missing)?,
            null.ok_or_else(missing)?,
            str_id.ok_or_else(missing)?,
        );

        let mut stack = BinderStack::default();
        stack.enter(result_type);
        let (array, object, null, str_id) = (
            ctx.label(array, &stack)?,
            ctx.label(object, &stack)?,
            ctx.label(null, &stack)?,
            ctx.label(str_id, &stack)?,
        );
        stack.enter(array_list);
        let (array_cons, nil) = (ctx.label(array_cons, &stack)?, ctx.label(nil, &stack)?);
        stack.leave(array_list);
        stack.enter(object_list);
        let (object_cons, pair) = (ctx.label(object_cons, &stack)?, ctx.label(pair, &stack)?);
        stack.leave(object_list);
        stack.leave(result_type);

        Ok(JsonIds {
            json_type: result_type,
            array,
            array_cons,
            object,
            object_cons,
            pair,
            nil,
            null,
            str: str_id,
        })
    }
}

/// `__json_encode__<'t>`: a value of the explicitly-given type → the `'%json`
/// document representing it. The inverse of decode; a value outside the JSON mapping
/// is a runtime error, since no caller could act on it.
pub fn builtin_json_encode<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let source = ctx.type_argument().ok_or_else(|| {
        Error::InvalidArgument(
            "__json_encode__ called without a type argument — a bare reference carries \
             no instantiation; name it with one (`__json_encode__<'t>`) where the type \
             is concrete"
                .to_string(),
        )
    })?;
    let result_type = ctx.result_type().ok_or_else(|| {
        Error::InvalidArgument(
            "__json_encode__ called without a registered result type".to_string(),
        )
    })?;
    let ids = JsonIds::resolve(ctx, result_type)?;
    let mut stack = BinderStack::default();
    let value = encode(ctx, &ids, source, arg, &mut stack)?.ok_or_else(|| {
        Error::InvalidArgument(
            "cannot encode as JSON: the value does not fit the stated type's mapping \
             (functions, processes, refs, resources, binaries, dicts and unlabelled \
             tuples have no JSON form)"
                .to_string(),
        )
    })?;
    Ok(Completion::Value(value))
}

/// Type-directed descent over the source value, mirroring decode.
fn encode<E: Effect>(
    ctx: &mut BuiltinContext<E>,
    ids: &JsonIds,
    type_id: usize,
    value: &Value,
    stack: &mut BinderStack,
) -> Result<Option<Value>, Error> {
    let Some(typ) = TypeLookup::lookup_type(&*ctx.executor, type_id).cloned() else {
        return Ok(None);
    };
    match typ {
        Type::Annotated { base, .. } => encode(ctx, ids, base, value, stack),
        Type::Union(members) => {
            stack.enter(type_id);
            let mut result = None;
            // A nil value under a nil-admitting union is JSON `null` (its field-level
            // twin — key omission — is handled by the object encoder before recursing).
            if is_nil_value(ctx, value) {
                if admits_nil(ctx, type_id, &mut Vec::new()) {
                    result = Some(Value::tuple(ids.null, vec![]));
                }
            } else if let Some(element_type) = as_list_union(ctx, &members) {
                result = encode_list(ctx, ids, element_type, value, stack)?;
            } else {
                for member in &members {
                    if let Some(encoded) = encode(ctx, ids, *member, value, stack)? {
                        result = Some(encoded);
                        break;
                    }
                }
            }
            stack.leave(type_id);
            Ok(result)
        }
        Type::Cycle(depth) => {
            // Same re-entry rule as decode's: cycles resolve at the target's own depth.
            let Some((target, cut)) = stack.follow(depth) else {
                return Ok(None);
            };
            let result = encode(ctx, ids, target, value, stack);
            stack.restore(cut);
            result
        }
        Type::Integer => Ok(match value {
            Value::Int(_) | Value::BigInt(_) => Some(value.clone()),
            _ => None,
        }),
        Type::Tuple(tuple_id) => encode_tuple(ctx, ids, tuple_id, value, stack),
        Type::Binary
        | Type::Partial { .. }
        | Type::Callable { .. }
        | Type::Process { .. }
        | Type::Resource(_)
        | Type::Reference
        | Type::Variable(_)
        | Type::Intersection(_)
        | Type::Top => Ok(None),
    }
}

fn encode_tuple<E: Effect>(
    ctx: &mut BuiltinContext<E>,
    ids: &JsonIds,
    expected_id: usize,
    value: &Value,
    stack: &mut BinderStack,
) -> Result<Option<Value>, Error> {
    let Value::Tuple(input_id, payload) = value else {
        return Ok(None);
    };
    let Some(expected) = TypeLookup::lookup_tuple(&*ctx.executor, expected_id).cloned() else {
        return Ok(None);
    };

    // A `'%json` leaf in the source type: already a JSON value — pass it through.
    // Leaf shapes (`Str`, `Rational`, and the named-empty keywords) intern to one id
    // program-wide, so id equality is exact.
    if *input_id == expected_id
        && matches!(
            expected.name.as_deref(),
            Some("Str" | "Rational" | "True" | "False" | "Null")
        )
    {
        return Ok(Some(value.clone()));
    }
    if is_str_shape(&expected, &*ctx.executor)
        && payload.len() == 1
        && let Value::Binary(_) = &payload[0]
    {
        return Ok(Some(value.clone()));
    }
    // A `'%json` subtree in the source type — the stated type reaching `'%json`'s own
    // `Array`/`Object` members: the value is already a JSON document, but its tuple
    // ids may sit in a different interned family than the result type's, so it is
    // normalized through the decode walk, whose structural rule bridges families.
    if matches!(expected.name.as_deref(), Some("Array" | "Object"))
        && expected.fields.len() == 1
        && expected.fields[0].0.is_none()
        && tuple_info(ctx, value).is_some_and(|(_, input)| input.name == expected.name)
    {
        return decode(ctx, ids.json_type, value, &mut BinderStack::default());
    }

    // Object mapping: a tuple whose fields are all labelled becomes a JSON object. A
    // nil-valued field whose type admits nil is omitted; the keys appear in field
    // order.
    if !expected.fields.is_empty() && expected.fields.iter().all(|(label, _)| label.is_some()) {
        if payload.len() != expected.fields.len() {
            return Ok(None);
        }
        let mut pairs = Vec::with_capacity(expected.fields.len());
        for ((label, field_type), field_value) in expected.fields.iter().zip(payload.iter()) {
            if is_nil_value(ctx, field_value) && admits_nil(ctx, *field_type, &mut Vec::new()) {
                continue;
            }
            let Some(encoded) = encode(ctx, ids, *field_type, field_value, stack)? else {
                return Ok(None);
            };
            let label = label.as_deref().unwrap_or_default();
            let binary = ctx.executor.allocate_binary(label.as_bytes().to_vec())?;
            let key = Value::tuple(ids.str, vec![Value::Binary(binary)]);
            pairs.push(Value::tuple(ids.pair, vec![key, encoded]));
        }
        let mut chain = Value::tuple(ids.nil, vec![]);
        for pair in pairs.into_iter().rev() {
            chain = Value::tuple(ids.object_cons, vec![pair, chain]);
        }
        return Ok(Some(Value::tuple(ids.object, vec![chain])));
    }

    Ok(None)
}

/// Whether a union is a `%list` — exactly a `Nil` and a `Cons[element, tail]` — and
/// its element type. This is where the type argument resolves what a bare value never
/// could: a `Cons`/`Nil` chain is an array exactly when the type says list.
fn as_list_union<E: Effect>(ctx: &BuiltinContext<E>, members: &[usize]) -> Option<usize> {
    if members.len() != 2 {
        return None;
    }
    let lookup = &*ctx.executor;
    let mut element = None;
    let mut saw_nil = false;
    for member in members {
        let Some(Type::Tuple(tuple_id)) = lookup.lookup_type(*member) else {
            return None;
        };
        let info = lookup.lookup_tuple(*tuple_id)?;
        match (info.name.as_deref(), info.fields.len()) {
            (Some("Nil"), 0) => saw_nil = true,
            (Some("Cons"), 2) if info.fields.iter().all(|(label, _)| label.is_none()) => {
                element = Some(info.fields[0].1)
            }
            _ => return None,
        }
    }
    if saw_nil { element } else { None }
}

/// Encode a `Cons`/`Nil` chain as `Array[…]`, elements at the list's element type.
fn encode_list<E: Effect>(
    ctx: &mut BuiltinContext<E>,
    ids: &JsonIds,
    element_type: usize,
    value: &Value,
    stack: &mut BinderStack,
) -> Result<Option<Value>, Error> {
    let mut elements = Vec::new();
    let mut node = value.clone();
    loop {
        let Some((_, info)) = tuple_info(ctx, &node) else {
            return Ok(None);
        };
        match (info.name.as_deref(), &node) {
            (Some("Nil"), _) => break,
            (Some("Cons"), Value::Tuple(_, fields)) if fields.len() == 2 => {
                let Some(encoded) = encode(ctx, ids, element_type, &fields[0], stack)? else {
                    return Ok(None);
                };
                elements.push(encoded);
                let tail = fields[1].clone();
                node = tail;
            }
            _ => return Ok(None),
        }
    }
    let mut chain = Value::tuple(ids.nil, vec![]);
    for element in elements.into_iter().rev() {
        chain = Value::tuple(ids.array_cons, vec![element, chain]);
    }
    Ok(Some(Value::tuple(ids.array, vec![chain])))
}

/// Whether a tuple shape is the `Str` sugar's: named `Str`, one unnamed binary field.
fn is_str_shape(info: &TupleTypeInfo, lookup: &impl TypeLookup) -> bool {
    info.name.as_deref() == Some("Str")
        && matches!(
            info.fields.as_slice(),
            [(None, field_type)] if matches!(lookup.lookup_type(*field_type), Some(Type::Binary))
        )
}
