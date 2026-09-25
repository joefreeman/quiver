//! Annotation-key interning and row typing.
//!
//! Annotation keys are just interned names — their value types are inferred per attach
//! site and tracked in annotation rows (`Type::Annotated`). The builtin keys keep an
//! attach-side expected type: `doc` (a `Str['bin]`), and the carrier-polymorphic
//! `pre`/`post`, whose contract types are derived from the annotated function.

use quiver_core::program::Program;
use quiver_core::types::{Type, TypeLookup};

use super::{Error, typing};

/// Names of the builtin, carrier-polymorphic contract keys.
pub const PRE: &str = "pre";
pub const POST: &str = "post";

/// Per-field defaults for a function's parameter tuple: a call site fills an omitted
/// field from this entry, read off the callee value exactly as the contract keys are.
/// Checked at attach by [`check_defaults`].
pub const DEFAULTS: &str = "defaults";

/// The failure-provenance key. Debug builds attach it to fresh nil results
/// *row-invisibly* — the value carries it while the rows go on saying exact-empty — so
/// for this one key an exact row lacking the entry proves nothing: both retrieval walks
/// treat that case as open (bare form: erased error; checked form: runtime gate). This
/// keeps typing identical across debug and release builds ('t | [] either way; release
/// simply never stamps, so the checked retrieval is always nil there).
pub const ORIGIN: &str = "origin";

/// The runtime-owned crash-delivery keys, attached row-invisibly in both build modes: a
/// never-lethal await answers a crashed source with a `:crash`-stamped nil, and a select
/// timeout stamps `:timeout` (the ms that fired).
pub const CRASH: &str = "crash";
pub const TIMEOUT: &str = "timeout";

/// Whether an exact row lacking `name` proves the annotation absent. True for ordinary
/// keys (attach is the only writer, and attaches are row-tracked); false for keys with a
/// row-invisible writer — for those, a checked retrieval must keep its runtime gate even
/// on an exact-empty row (a select's nil member is exact-rowed yet may carry the stamp).
fn exactness_proves_absence(name: &str) -> bool {
    !matches!(name, ORIGIN | CRASH | TIMEOUT)
}

/// Intern an annotation key by name. Any name is valid; typos are caught at retrieval
/// by the visibility rule (an always-nil retrieval is an error).
pub fn intern_key(program: &mut Program, name: &str) -> usize {
    program.register_annotation_key(name)
}

/// Mark `type_id` with the exact-empty row: *provably annotation-free* (the type of a
/// fresh construction). Load-bearing for visibility typing — a type left plain reads as
/// an open row ("may carry erased annotations"), which poisons bare retrieval (rule 4)
/// and forces runtime gates on checked retrieval.
pub fn exact_empty(program: &mut Program, type_id: usize) -> usize {
    program.annotate_type(type_id, true, vec![])
}

/// The type of a freshly-minted nil: `[]` with an exact-empty row.
pub fn closed_nil(program: &mut Program) -> usize {
    let nil_type = program.register_type(Type::nil());
    exact_empty(program, nil_type)
}

/// The type of a match verdict's success: `Ok` with an exact-empty row.
pub fn closed_ok(program: &mut Program) -> usize {
    let ok_type = program.register_type(Type::ok());
    exact_empty(program, ok_type)
}

/// The `Str['bin]` type (the type of string literals, and of `:doc` values).
pub fn str_type(program: &mut Program) -> usize {
    let binary_type = program.register_type(Type::Binary);
    let str_tuple = program.register_tuple(Some("Str".to_string()), vec![(None, binary_type)]);
    program.register_type(Type::Tuple(str_tuple))
}

/// Whether every member of a type can carry annotations at runtime (tuples and callables
/// only — for callables the runtime slot exists on both closures and builtins). `Cycle`
/// back-references are tuple-shaped by construction. Type variables are
/// rejected: an unpinned generic could be instantiated with a primitive. This is the
/// *attach* requirement — attaching to a value that cannot carry the annotation is a bug.
pub fn is_annotatable(program: &Program, type_id: usize) -> bool {
    members_annotatable(program, type_id, true)
}

fn members_annotatable(program: &Program, type_id: usize, all: bool) -> bool {
    match program.lookup_type(type_id) {
        Some(
            Type::Tuple(_)
            | Type::Partial { .. }
            | Type::Callable { .. }
            | Type::Cycle(_)
            | Type::Annotated { .. },
        ) => true,
        Some(Type::Union(members)) => {
            let members = members.clone();
            if all {
                members
                    .iter()
                    .all(|&m| members_annotatable(program, m, all))
            } else {
                members
                    .iter()
                    .any(|&m| members_annotatable(program, m, all))
            }
        }
        _ => false,
    }
}

/// The static type of retrieving annotation `name` from a carrier, by the visibility
/// rule. Each union member contributes:
/// 1. its row's entry type, when the entry is visible (definite — no nil);
/// 2. nil, when the member is statically non-annotatable ('int, 'bin, ...);
/// 3. nil, when the member's row is exact and lacks the entry (provably absent);
/// 4. **error**, when the member is open (plain carrier or open row) without the entry —
///    it may hold an erased annotation of unknown type, so no sound answer exists;
/// 5. **error**, when no member contributes an entry (the retrieval is always nil —
///    almost certainly a typo'd or wrong key).
///
/// Returns `(key id, result type)`.
pub fn retrieval_type(
    program: &mut Program,
    carrier_type: usize,
    name: &str,
) -> Result<(usize, usize), Error> {
    let key_id = intern_key(program, name);
    let mut contributions: Vec<usize> = Vec::new();
    let mut any_entry = false;
    let mut any_nil = false;
    walk_retrieval(
        program,
        carrier_type,
        key_id,
        name,
        &mut contributions,
        &mut any_entry,
        &mut any_nil,
    )?;
    if !any_entry {
        return Err(Error::TypeUnresolved(format!(
            "No member of {} can carry annotation :{name} — the retrieval would always be nil",
            quiver_core::format::format_type_by_id(program, carrier_type)
        )));
    }
    if any_nil {
        // The absence nil is freshly minted by GetAnnotation: a closed-empty row.
        contributions.push(closed_nil(program));
    }
    let result_type = typing::union_type_ids(program, contributions);
    Ok((key_id, result_type))
}

/// The static type of a checked retrieval `x:('t)key` — total on any carrier, no
/// visibility requirement. Each union member contributes:
/// - its entry's type, when visible and within the asked shape (the gate can't reject it);
/// - the asked shape itself, when the member is open or its entry only partially overlaps
///   it — the runtime gate admits exactly the values that fit;
/// - nil, when the entry may be absent, may be rejected, or the member can't carry.
///
/// Returns `(key id, result type, needs_check)`; the runtime test is elided when every
/// possible entry is statically within the asked shape. Never errors: the explicit shape
/// is the programmer's declaration that this is beyond static tracking, so neither the
/// erased-row rule nor the always-nil firewall applies.
pub fn checked_retrieval_type(
    program: &mut Program,
    carrier_type: usize,
    name: &str,
    asked: usize,
) -> (usize, usize, bool) {
    let key_id = intern_key(program, name);
    let mut contributions: Vec<usize> = Vec::new();
    let mut any_nil = false;
    let mut needs_check = false;
    walk_checked(
        program,
        carrier_type,
        key_id,
        name,
        asked,
        &mut contributions,
        &mut any_nil,
        &mut needs_check,
    );
    if any_nil || needs_check {
        contributions.push(closed_nil(program));
    }
    let result_type = typing::union_type_ids(program, contributions);
    (key_id, result_type, needs_check)
}

#[allow(clippy::too_many_arguments)]
fn walk_checked(
    program: &mut Program,
    member: usize,
    key_id: usize,
    name: &str,
    asked: usize,
    contributions: &mut Vec<usize>,
    any_nil: &mut bool,
    needs_check: &mut bool,
) {
    match program.lookup_type(member).cloned() {
        Some(Type::Union(members)) => {
            for m in members {
                walk_checked(
                    program,
                    m,
                    key_id,
                    name,
                    asked,
                    contributions,
                    any_nil,
                    needs_check,
                );
            }
        }
        Some(Type::Annotated { exact, entries, .. }) => {
            match entries.binary_search_by_key(&key_id, |(k, _)| *k) {
                Ok(index) => {
                    let entry = entries[index].1;
                    if quiver_core::types::is_compatible(entry, asked, &*program) {
                        contributions.push(entry);
                    } else {
                        // At most a partial overlap: the gate admits what fits (within
                        // the asked shape) and answers nil for the rest.
                        contributions.push(asked);
                        *needs_check = true;
                        *any_nil = true;
                    }
                }
                Err(_) if exact && exactness_proves_absence(name) => {
                    *any_nil = true;
                }
                Err(_) => {
                    // Open row (or a row-invisibly-written key, whose absence exactness
                    // can't prove): the entry may or may not exist/fit — gate at runtime.
                    contributions.push(asked);
                    *needs_check = true;
                    *any_nil = true;
                }
            }
        }
        // Non-annotatable members can never carry the key: always nil.
        Some(
            Type::Integer
            | Type::Binary
            | Type::Reference
            | Type::Process { .. }
            | Type::Resource(_),
        ) => {
            *any_nil = true;
        }
        // A plain carrier is an open-empty row; a bare variable could be anything.
        Some(Type::Tuple(_) | Type::Partial { .. } | Type::Callable { .. } | Type::Cycle(_))
        | Some(Type::Variable(_))
        | None => {
            contributions.push(asked);
            *needs_check = true;
            *any_nil = true;
        }
    }
}

fn walk_retrieval(
    program: &mut Program,
    member: usize,
    key_id: usize,
    name: &str,
    contributions: &mut Vec<usize>,
    any_entry: &mut bool,
    any_nil: &mut bool,
) -> Result<(), Error> {
    match program.lookup_type(member).cloned() {
        Some(Type::Union(members)) => {
            for m in members {
                walk_retrieval(program, m, key_id, name, contributions, any_entry, any_nil)?;
            }
            Ok(())
        }
        Some(Type::Annotated { exact, entries, .. }) => {
            match entries.binary_search_by_key(&key_id, |(k, _)| *k) {
                Ok(index) => {
                    contributions.push(entries[index].1);
                    *any_entry = true;
                    Ok(())
                }
                Err(_) if exact && exactness_proves_absence(name) => {
                    *any_nil = true;
                    Ok(())
                }
                Err(_) => Err(erased_error(program, member, name)),
            }
        }
        // Non-annotatable members can never carry the key: they contribute nil.
        Some(
            Type::Integer
            | Type::Binary
            | Type::Reference
            | Type::Process { .. }
            | Type::Resource(_),
        ) => {
            *any_nil = true;
            Ok(())
        }
        // A plain carrier is an open-empty row: it may hold erased annotations.
        Some(Type::Tuple(_) | Type::Partial { .. } | Type::Callable { .. } | Type::Cycle(_)) => {
            Err(erased_error(program, member, name))
        }
        // A bare type variable could be instantiated with anything.
        Some(Type::Variable(_)) | None => Err(erased_error(program, member, name)),
    }
}

fn erased_error(program: &Program, member: usize, name: &str) -> Error {
    Error::TypeUnresolved(format!(
        "Cannot retrieve :{name} — a value of type {} may carry erased annotations \
         (annotations aren't statically visible through declared parameter/receive \
         types; state the expected shape with a checked retrieval, `:('t){name}`)",
        quiver_core::format::format_type_by_id(program, member)
    ))
}

/// The expected type of a `pre`/`post` contract for a function `#P -> R`:
/// `pre` is `#P -> ok?`, `post` is `#[in: P, out: R] -> ok?`.
pub fn contract_type(program: &mut Program, name: &str, parameter: usize, result: usize) -> usize {
    let ok_type = program.register_type(Type::ok());
    let nil_type = program.register_type(Type::nil());
    let verdict = typing::union_type_ids(program, vec![ok_type, nil_type]);
    let never = program.never();
    let contract_parameter = match name {
        PRE => parameter,
        POST => {
            let in_out = program.register_tuple(
                None,
                vec![
                    (Some("in".to_string()), parameter),
                    (Some("out".to_string()), result),
                ],
            );
            program.register_type(Type::Tuple(in_out))
        }
        _ => unreachable!("contract_type is only for pre/post"),
    };
    program.register_type(Type::Callable {
        parameter: contract_parameter,
        result: verdict,
        receive: never,
        // An expected (declared-shape) type: no states grant.
        states: None,
        omittable: Vec::new(),
    })
}

/// If `type_id` is a callable (or a single-member union of one, or an annotated one),
/// its `(parameter, result)`, opened to stand on their own (`typing::open_callable`).
pub fn single_callable(program: &mut Program, type_id: usize) -> Option<(usize, usize)> {
    match program.lookup_type(type_id)? {
        Type::Callable { .. } => {
            let parts = super::typing::open_callable(type_id, program)?;
            Some((parts.parameter, parts.result))
        }
        &Type::Annotated { base, .. } => single_callable(program, base),
        Type::Union(members) if members.len() == 1 => {
            let member = members[0];
            single_callable(program, member)
        }
        _ => None,
    }
}

/// If `type_id` is a tuple (or a single-member union of one, or an annotated one), its
/// tuple id — the tuple counterpart of [`single_callable`].
pub fn single_tuple(program: &Program, type_id: usize) -> Option<usize> {
    match program.lookup_type(type_id)? {
        Type::Tuple(tuple_id) => Some(*tuple_id),
        Type::Annotated { base, .. } => single_tuple(program, *base),
        Type::Union(members) if members.len() == 1 => single_tuple(program, members[0]),
        _ => None,
    }
}

/// Check a `:defaults` value against the function it is attached to. The entry names
/// per-field defaults that a call site fills omitted fields from, so every field must be
/// labeled with a field of the parameter tuple, and carry a value assignable to it. A
/// default that could never fire — a non-function carrier, a parameter that is not a
/// single tuple, a label naming nothing — is rejected rather than left inert.
pub fn check_defaults(
    program: &mut Program,
    carrier_type: usize,
    value_type: usize,
) -> Result<(), Error> {
    let (parameter, _) = single_callable(program, carrier_type).ok_or_else(|| {
        Error::TypeUnresolved(format!(
            "Annotation :{DEFAULTS} can only be attached to a function"
        ))
    })?;
    let program = &*program;
    let describe = |id| quiver_core::format::format_type_by_id(program, id);
    let parameter_fields = single_tuple(program, parameter)
        .and_then(|id| program.lookup_tuple(id))
        .map(|info| info.fields.clone())
        .ok_or_else(|| {
            Error::TypeUnresolved(format!(
                "Annotation :{DEFAULTS} requires a tuple parameter, but the function takes {}",
                describe(parameter)
            ))
        })?;
    let value_fields = single_tuple(program, value_type)
        .and_then(|id| program.lookup_tuple(id))
        .map(|info| info.fields.clone())
        .ok_or_else(|| {
            Error::TypeUnresolved(format!(
                "Annotation :{DEFAULTS} must be a tuple of per-field defaults, but is {}",
                describe(value_type)
            ))
        })?;

    // Duplicate labels can't reach here: a tuple literal rejects them as `FieldDuplicated`.
    for (name, field_type) in &value_fields {
        let name = name.as_deref().ok_or_else(|| {
            Error::TypeUnresolved(format!(
                "Annotation :{DEFAULTS} takes labeled fields, each naming a parameter field"
            ))
        })?;
        let declared = parameter_fields
            .iter()
            .find(|(field, _)| field.as_deref() == Some(name))
            .map(|(_, ty)| *ty)
            .ok_or_else(|| {
                Error::TypeUnresolved(format!(
                    "Annotation :{DEFAULTS} names '{name}', which is not a field of the parameter {}",
                    describe(parameter)
                ))
            })?;
        if !quiver_core::types::is_compatible(*field_type, declared, program) {
            return Err(Error::TypeMismatch {
                expected: format!(
                    "default for '{name}' compatible with {}",
                    describe(declared)
                ),
                found: describe(*field_type),
            });
        }
    }
    Ok(())
}

/// The nil-shaped members of a type, rows preserved — the values that actually flow on a
/// short-circuit. Used to thread annotated nils through sequence/block fall-through types.
pub fn nil_members(program: &Program, type_id: usize) -> Vec<usize> {
    match program.lookup_type(type_id) {
        Some(Type::Union(members)) => {
            let members = members.clone();
            members
                .iter()
                .flat_map(|&m| nil_members(program, m))
                .collect()
        }
        Some(t) if t.is_nil_deep(program) => vec![type_id],
        _ => vec![],
    }
}
