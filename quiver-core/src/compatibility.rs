//! Runtime type tests: what a `TestType`, a checked retrieval, a registry lookup or a mailbox
//! receive makes of a value, by the value's concrete type — and the tuple tables (canonical
//! shapes, field offsets) the executor reads alongside.
//!
//! Verdicts are decided on demand and cached ([`Verdicts`]), since a program tests against few
//! of its types and meets few of its concrete types at each: deciding every pair up front
//! computed, stored and shipped to every worker a table almost nobody read.

use crate::binders::BinderStack;
use crate::bytecode::{ConcreteType, Function};
use crate::types::{TupleTypeInfo, Type, TypeLookup, is_compatible, types_overlap};
use rustc_hash::FxHashMap;
use std::collections::HashMap;

/// A runtime type test's verdict on the values of one concrete type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Verdict {
    /// Every value of it belongs to the type.
    Admit,
    /// Some may: a tuple built at a wider type than it holds, or in generic code, whose own
    /// type can't say. The test looks at the value's fields.
    Inspect,
}

/// The program tables a verdict is decided from.
#[derive(Clone, Copy)]
pub struct Tables<'a> {
    pub types: &'a [Type],
    pub tuples: &'a [TupleTypeInfo],
    pub functions: &'a [Function],
    /// Each builtin's `(parameter, result)` types.
    pub builtin_signatures: &'a [(usize, usize)],
    /// Resource type names, by resource type id.
    pub resource_names: &'a [String],
}

/// Verdicts decided so far, per tested type and concrete type, and the indexes deciding them
/// needs. Everything here is a function of append-only tables, so it is only ever extended:
/// a holder replacing its tables wholesale (which may reuse ids) must [`Self::clear`] it.
#[derive(Debug, Default)]
pub struct Verdicts {
    /// Per tested type id, the verdict on each concrete type met so far (`None`: rejected).
    rows: Vec<FxHashMap<ConcreteType, Option<Verdict>>>,
    index: TypeIndex,
}

impl Verdicts {
    /// The verdict on values of `concrete` against the type `pattern_id` (`None`: none of
    /// them belongs to it).
    pub fn get(
        &mut self,
        concrete: ConcreteType,
        pattern_id: usize,
        tables: Tables<'_>,
    ) -> Option<Verdict> {
        if let Some(&known) = self.rows.get(pattern_id).and_then(|row| row.get(&concrete)) {
            return known;
        }
        self.index.catch_up(tables);
        let decided = self.index.decide(concrete, pattern_id, tables);
        if self.rows.len() <= pattern_id {
            self.rows
                .resize_with(tables.types.len().max(pattern_id + 1), Default::default);
        }
        self.rows[pattern_id].insert(concrete, decided);
        decided
    }

    /// Forget everything, for tables replaced wholesale.
    pub fn clear(&mut self) {
        *self = Self::default();
    }
}

/// Lookups from concrete types to the type ids representing them, and what each tuple's type
/// says about its values, over the prefix of the tables seen so far.
#[derive(Debug, Default)]
struct TypeIndex {
    types_seen: usize,
    integer: Option<usize>,
    binary: Option<usize>,
    reference: Option<usize>,
    /// tuple_id -> the first type id of `Type::Tuple(tuple_id)`
    tuple_to_type: Vec<Option<usize>>,
    /// `(parameter, result)` -> the first never-receiving callable of that signature, which
    /// represents a builtin
    callable_to_type: HashMap<(usize, usize), usize>,
    /// resource name -> the first type id of `Type::Resource`
    resource_to_type: HashMap<String, usize>,
    tuple_facts: Vec<TupleFacts>,
    facts_memo: FactsMemo,
}

impl TypeIndex {
    /// Extend the index over whatever the tables have grown by.
    fn catch_up(&mut self, tables: Tables<'_>) {
        if self.tuple_to_type.len() < tables.tuples.len() {
            self.tuple_to_type.resize(tables.tuples.len(), None);
        }
        for type_id in self.types_seen..tables.types.len() {
            match &tables.types[type_id] {
                Type::Integer => {
                    self.integer.get_or_insert(type_id);
                }
                Type::Binary => {
                    self.binary.get_or_insert(type_id);
                }
                Type::Reference => {
                    self.reference.get_or_insert(type_id);
                }
                Type::Tuple(tuple_id) => {
                    if let Some(slot) = self.tuple_to_type.get_mut(*tuple_id) {
                        slot.get_or_insert(type_id);
                    }
                }
                // Builtin signatures never mark labels, so a marked callable must not
                // represent one — it would hand a caller a calling convention the builtin
                // never declared.
                Type::Callable {
                    parameter,
                    result,
                    receive,
                    omittable,
                    ..
                } if omittable.is_empty()
                    && tables.types.get(*receive).is_some_and(Type::is_never) =>
                {
                    self.callable_to_type
                        .entry((*parameter, *result))
                        .or_insert(type_id);
                }
                Type::Resource(name) => {
                    self.resource_to_type.entry(name.clone()).or_insert(type_id);
                }
                _ => {}
            }
        }
        self.types_seen = tables.types.len();
        let lookup = Lookup::of(tables);
        for tuple_id in self.tuple_facts.len()..tables.tuples.len() {
            let facts = TupleFacts::of(tuple_id, &lookup, &mut self.facts_memo);
            self.tuple_facts.push(facts);
        }
    }

    fn decide(
        &self,
        concrete: ConcreteType,
        pattern_id: usize,
        tables: Tables<'_>,
    ) -> Option<Verdict> {
        let lookup = Lookup::of(tables);
        // Annotation rows are invisible to pattern matching, so a runtime type check
        // against `T @ row` must behave exactly as against `T`.
        let pattern_id = Type::strip_annotations(pattern_id, &lookup);
        let admit_if = |fits: bool| fits.then_some(Verdict::Admit);
        match concrete {
            // A primitive is tested through a type-table entry representing it: a pattern that
            // could match one names it, so the entry exists whenever the verdict could be
            // positive. The top type is the exception, admitting a primitive it does not name.
            ConcreteType::Integer | ConcreteType::Binary | ConcreteType::Reference => {
                let rep = match concrete {
                    ConcreteType::Integer => self.integer,
                    ConcreteType::Binary => self.binary,
                    _ => self.reference,
                };
                admit_if(
                    matches!(lookup.lookup_type(pattern_id), Some(Type::Top))
                        || rep.is_some_and(|rep| is_compatible(rep, pattern_id, &lookup)),
                )
            }
            ConcreteType::Tuple(tuple_id) => tuple_verdict(
                tuple_id,
                self.tuple_to_type[tuple_id],
                self.tuple_facts[tuple_id],
                pattern_id,
                &lookup,
            ),
            ConcreteType::Function(func_id) => admit_if(is_compatible(
                tables.functions[func_id].type_id,
                pattern_id,
                &lookup,
            )),
            ConcreteType::Builtin(builtin_id) => admit_if(
                self.callable_to_type
                    .get(&tables.builtin_signatures[builtin_id])
                    .is_some_and(|&rep| is_compatible(rep, pattern_id, &lookup)),
            ),
            // A pid is judged by the process type its root function gives it — sends,
            // await result and states alike: its state is what makes a received pid
            // trustworthy for bare `?p`, which has no runtime test of its own.
            ConcreteType::Process(func_id) => {
                let lookup =
                    lookup.with_extra(process_type(&tables.functions[func_id], tables.types));
                admit_if(is_compatible(lookup.extra_id(), pattern_id, &lookup))
            }
            ConcreteType::Resource(resource_id) => admit_if(
                self.resource_to_type
                    .get(&tables.resource_names[resource_id])
                    .is_some_and(|&rep| is_compatible(rep, pattern_id, &lookup)),
            ),
        }
    }
}

/// The process type a function's spawns have: what it receives as sends, its result as the
/// await result, and its states.
fn process_type(func: &Function, types: &[Type]) -> Type {
    match types.get(func.type_id) {
        Some(Type::Callable {
            result,
            receive,
            states,
            ..
        }) => Type::Process {
            send: Some(*receive),
            receive: Some(*result),
            state: *states,
        },
        _ => Type::Process {
            send: None,
            receive: None,
            state: None,
        },
    }
}

/// The tables as a [`TypeLookup`], optionally answering one more type — one the program need
/// not contain, such as the process type of a function nothing named — at the next id.
struct Lookup<'a> {
    types: &'a [Type],
    tuples: &'a [TupleTypeInfo],
    extra: Option<Type>,
}

impl<'a> Lookup<'a> {
    fn of(tables: Tables<'a>) -> Self {
        Lookup {
            types: tables.types,
            tuples: tables.tuples,
            extra: None,
        }
    }

    fn with_extra(self, extra: Type) -> Self {
        Lookup {
            extra: Some(extra),
            ..self
        }
    }

    fn extra_id(&self) -> usize {
        self.types.len()
    }
}

impl TypeLookup for Lookup<'_> {
    fn lookup_type(&self, type_id: usize) -> Option<&Type> {
        match self.types.get(type_id) {
            Some(ty) => Some(ty),
            None if type_id == self.types.len() => self.extra.as_ref(),
            None => None,
        }
    }

    fn lookup_tuple(&self, tuple_id: usize) -> Option<&TupleTypeInfo> {
        self.tuples.get(tuple_id)
    }
}

/// The verdict on a tuple's values, represented by its type `rep` where the table has one.
/// A tuple whose type doesn't settle it is inspected where some of its values could belong:
/// its name and labels could match, and its type overlaps the pattern — or holds a function or
/// pid, whose types can share values without overlapping as the relation reckons it (a
/// function taking `'int | 'bin` is both an `#'int -> _` and a `#'bin -> _`).
fn tuple_verdict(
    tuple_id: usize,
    rep: Option<usize>,
    facts: TupleFacts,
    pattern_id: usize,
    lookup: &impl TypeLookup,
) -> Option<Verdict> {
    let settles = |rep: usize| facts.exact && is_compatible(rep, pattern_id, lookup);
    if rep.is_some_and(settles) || head_decides(tuple_id, pattern_id, lookup) {
        Some(Verdict::Admit)
    } else if head_may_match(tuple_id, pattern_id, lookup)
        && (facts.opaque || rep.is_none_or(|rep| types_overlap(rep, pattern_id, lookup)))
    {
        Some(Verdict::Inspect)
    } else {
        None
    }
}

/// Whether a tuple of this name and these labels belongs to the type whatever its fields
/// hold: the type has a member matching the head whose fields constrain nothing a test can
/// check (`_`, a type variable, or a recursive reference past where the test starts — see
/// `walk_inhabits`). The common case is a pattern telling a recursive union's members apart,
/// such as `=Cons[h, t]` on a generically built list.
fn head_decides(tuple_id: usize, pattern_id: usize, lookup: &impl TypeLookup) -> bool {
    head_decides_within(tuple_id, pattern_id, 0, lookup)
}

/// `head_decides`, `entered` binders into the type the test starts from.
fn head_decides_within(
    tuple_id: usize,
    pattern_id: usize,
    entered: usize,
    lookup: &impl TypeLookup,
) -> bool {
    let vacuous =
        |type_id: usize| match lookup.lookup_type(Type::strip_annotations(type_id, lookup)) {
            Some(Type::Top | Type::Variable(_)) => true,
            Some(Type::Cycle(depth)) => *depth > entered,
            _ => false,
        };
    match lookup.lookup_type(pattern_id) {
        Some(Type::Top | Type::Variable(_)) => true,
        Some(Type::Annotated { base, .. }) => head_decides_within(tuple_id, *base, entered, lookup),
        Some(Type::Union(members)) => members
            .iter()
            .any(|&member| head_decides_within(tuple_id, member, entered + 1, lookup)),
        Some(Type::Tuple(expected)) => {
            head_may_match(tuple_id, pattern_id, lookup)
                && lookup
                    .lookup_tuple(*expected)
                    .is_some_and(|expected| expected.fields.iter().all(|&(_, t)| vacuous(t)))
        }
        Some(Type::Partial { fields, rest, .. }) => {
            head_may_match(tuple_id, pattern_id, lookup)
                && fields.iter().all(|&(_, t)| vacuous(t))
                && rest.is_none_or(vacuous)
        }
        _ => false,
    }
}

/// Whether a tuple of this name and these labels could belong to the type, whatever its
/// fields hold: an over-approximation, so a tuple it rules out needs no inspection.
fn head_may_match(tuple_id: usize, pattern_id: usize, lookup: &impl TypeLookup) -> bool {
    let Some(actual) = lookup.lookup_tuple(tuple_id) else {
        return false;
    };
    match lookup.lookup_type(pattern_id) {
        Some(Type::Top | Type::Variable(_) | Type::Cycle(_)) => true,
        Some(Type::Annotated { base, .. }) => head_may_match(tuple_id, *base, lookup),
        Some(Type::Union(members)) => members
            .iter()
            .any(|&member| head_may_match(tuple_id, member, lookup)),
        Some(Type::Intersection(members)) => members
            .iter()
            .all(|&member| head_may_match(tuple_id, member, lookup)),
        Some(Type::Tuple(expected)) => lookup.lookup_tuple(*expected).is_some_and(|expected| {
            expected.name == actual.name
                && expected.fields.len() == actual.fields.len()
                && expected
                    .fields
                    .iter()
                    .zip(&actual.fields)
                    .all(|((expected, _), (actual, _))| expected == actual)
        }),
        Some(Type::Partial { name, fields, .. }) => {
            (name.is_none() || *name == actual.name)
                && fields.iter().all(|(label, _)| {
                    actual
                        .fields
                        .iter()
                        .any(|(actual, _)| actual.as_deref() == Some(label.as_str()))
                })
        }
        _ => false,
    }
}

/// What a tuple's type says about the values built with it.
#[derive(Debug, Clone, Copy)]
struct TupleFacts {
    /// No type variable (a tuple built in generic code, whose `'t` was whatever the caller
    /// chose) and no recursive reference past the tuple (a member of a recursive union, whose
    /// `^` means nothing on its own): the type describes its values exactly enough for a test
    /// to trust it without looking inside.
    exact: bool,
    /// Holds a function or pid somewhere.
    opaque: bool,
}

/// Memos behind `TupleFacts`, by type id: both facts are context-free.
#[derive(Debug, Clone, Default)]
struct FactsMemo {
    escape_depths: HashMap<usize, Option<usize>>,
    opaque: HashMap<usize, bool>,
}

impl TupleFacts {
    fn of(tuple_id: usize, lookup: &impl TypeLookup, memo: &mut FactsMemo) -> Self {
        let fields: Vec<usize> = lookup
            .lookup_tuple(tuple_id)
            .map(|info| info.fields.iter().map(|&(_, t)| t).collect())
            .unwrap_or_default();
        TupleFacts {
            exact: fields
                .iter()
                .all(|&field| escape_depth(field, lookup, &mut memo.escape_depths) == Some(0)),
            opaque: fields
                .iter()
                .any(|&field| holds_opaque(field, lookup, &mut memo.opaque)),
        }
    }
}

/// Whether a type's values may hold a function or pid.
fn holds_opaque(type_id: usize, lookup: &impl TypeLookup, memo: &mut HashMap<usize, bool>) -> bool {
    if let Some(&known) = memo.get(&type_id) {
        return known;
    }
    let parts: Vec<usize> = match lookup.lookup_type(type_id) {
        Some(Type::Callable { .. } | Type::Process { .. }) => {
            memo.insert(type_id, true);
            return true;
        }
        Some(Type::Tuple(tuple_id)) => lookup
            .lookup_tuple(*tuple_id)
            .map(|info| info.fields.iter().map(|&(_, t)| t).collect())
            .unwrap_or_default(),
        Some(Type::Partial { fields, rest, .. }) => {
            fields.iter().map(|&(_, t)| t).chain(*rest).collect()
        }
        Some(Type::Union(members) | Type::Intersection(members)) => members.clone(),
        Some(Type::Annotated { base, .. }) => vec![*base],
        // A value of the top type or a type variable may be anything, a function included.
        Some(Type::Top | Type::Variable(_)) => {
            memo.insert(type_id, true);
            return true;
        }
        _ => vec![],
    };
    let opaque = parts
        .into_iter()
        .any(|part| holds_opaque(part, lookup, memo));
    memo.insert(type_id, opaque);
    opaque
}

/// How many binders past `type_id` its recursive references reach (0 when it is closed), or
/// `None` when it mentions a type variable. Context-free, so memoised by id.
fn escape_depth(
    type_id: usize,
    lookup: &impl TypeLookup,
    memo: &mut HashMap<usize, Option<usize>>,
) -> Option<usize> {
    if let Some(&known) = memo.get(&type_id) {
        return known;
    }
    // The parts to look through, and whether `type_id` is a binder (which closes a reference
    // to itself).
    let (parts, binder): (Vec<usize>, bool) = match lookup.lookup_type(type_id) {
        None | Some(Type::Variable(_)) => {
            memo.insert(type_id, None);
            return None;
        }
        Some(Type::Cycle(depth)) => return Some(*depth),
        Some(Type::Integer | Type::Binary | Type::Reference | Type::Resource(_) | Type::Top) => {
            (vec![], false)
        }
        Some(Type::Tuple(tuple_id)) => (
            lookup
                .lookup_tuple(*tuple_id)
                .map(|info| info.fields.iter().map(|&(_, t)| t).collect())
                .unwrap_or_default(),
            false,
        ),
        Some(Type::Partial { fields, rest, .. }) => {
            (fields.iter().map(|&(_, t)| t).chain(*rest).collect(), false)
        }
        Some(Type::Intersection(members)) => (members.clone(), false),
        Some(Type::Process {
            send,
            receive,
            state,
        }) => (
            [*send, *receive, *state].into_iter().flatten().collect(),
            false,
        ),
        Some(Type::Annotated { base, .. }) => (vec![*base], false),
        Some(Type::Union(members)) => (members.clone(), true),
        Some(Type::Callable {
            parameter,
            result,
            receive,
            states,
            ..
        }) => (
            [*parameter, *result, *receive]
                .into_iter()
                .chain(*states)
                .collect(),
            true,
        ),
    };
    let mut deepest = Some(0);
    for part in parts {
        deepest = match (deepest, escape_depth(part, lookup, memo)) {
            (Some(a), Some(b)) => Some(a.max(b)),
            _ => None,
        };
    }
    let depth = deepest.map(|depth| {
        if binder {
            depth.saturating_sub(1)
        } else {
            depth
        }
    });
    memo.insert(type_id, depth);
    depth
}

/// Whether a value inhabits a type by its contents: the walk an [`Verdict::Inspect`] calls
/// for, the type's recursive references resolved against the binders it has entered.
/// `opaque` decides the parts a walk can't look inside — functions, builtins and pids, whose
/// concrete types are exact.
pub fn walk_inhabits<V: WalkValue>(
    value: &V,
    pattern_type_id: usize,
    tables: Tables<'_>,
    binders: &mut BinderStack,
    opaque: &mut impl FnMut(&V, usize) -> bool,
) -> bool {
    let lookup = Lookup::of(tables);
    let pattern_type_id = Type::strip_annotations(pattern_type_id, &lookup);
    let Some(pattern) = tables.types.get(pattern_type_id) else {
        return false;
    };
    match pattern {
        Type::Top => true,
        // A type variable left in a runtime test is one the test doesn't decide (the compiler
        // rejects any other): a witness of a union member built in generic code, which the
        // value is already known to belong to.
        Type::Variable(_) => true,
        Type::Integer => value.is_integer(),
        Type::Binary => value.is_binary(),
        Type::Reference => value.is_reference(),
        Type::Resource(name) => value
            .resource_type()
            .is_some_and(|resource_type| tables.resource_names.get(resource_type) == Some(name)),
        Type::Callable { .. } | Type::Process { .. } => opaque(value, pattern_type_id),
        Type::Union(members) => {
            binders.enter(pattern_type_id);
            let found = members
                .iter()
                .any(|&member| walk_inhabits(value, member, tables, binders, opaque));
            binders.leave(pattern_type_id);
            found
        }
        Type::Intersection(members) => members
            .iter()
            .all(|&member| walk_inhabits(value, member, tables, binders, opaque)),
        Type::Cycle(depth) => match binders.follow(*depth) {
            Some((binder, cut)) => {
                let found = walk_inhabits(value, binder, tables, binders, opaque);
                binders.restore(cut);
                found
            }
            // A reference past where the test started (a witness of a recursive union's
            // member): presumed to hold, as the relation presumes it.
            None => true,
        },
        Type::Tuple(pattern_tuple) => {
            let (Some((tuple_id, fields)), Some(expected)) =
                (value.tuple(), tables.tuples.get(*pattern_tuple))
            else {
                return false;
            };
            let Some(actual) = tables.tuples.get(tuple_id) else {
                return false;
            };
            expected.name == actual.name
                && expected.fields.len() == actual.fields.len()
                && expected
                    .fields
                    .iter()
                    .zip(&actual.fields)
                    .all(|((expected, _), (actual, _))| expected == actual)
                && expected
                    .fields
                    .iter()
                    .zip(fields)
                    .all(|(&(_, field_type), field)| {
                        walk_inhabits(field, field_type, tables, binders, opaque)
                    })
        }
        Type::Partial {
            name,
            fields: listed,
            rest,
        } => {
            let Some((tuple_id, fields)) = value.tuple() else {
                return false;
            };
            let Some(actual) = tables.tuples.get(tuple_id) else {
                return false;
            };
            if name.is_some() && *name != actual.name {
                return false;
            }
            let listed_fit = listed.iter().all(|(label, field_type)| {
                actual
                    .fields
                    .iter()
                    .position(|(actual, _)| actual.as_deref() == Some(label.as_str()))
                    .is_some_and(|index| {
                        walk_inhabits(&fields[index], *field_type, tables, binders, opaque)
                    })
            });
            listed_fit
                && rest.is_none_or(|rest| {
                    actual.fields.iter().zip(fields).all(|((label, _), field)| {
                        label
                            .as_ref()
                            .is_some_and(|label| listed.iter().any(|(listed, _)| listed == label))
                            || walk_inhabits(field, rest, tables, binders, opaque)
                    })
                })
        }
        Type::Annotated { .. } => unreachable!("stripped above"),
    }
}

/// What [`walk_inhabits`] needs to know of a value.
pub trait WalkValue: Sized {
    fn is_integer(&self) -> bool;
    fn is_binary(&self) -> bool;
    fn is_reference(&self) -> bool;
    fn resource_type(&self) -> Option<usize>;
    /// The tuple id and fields, for a tuple.
    fn tuple(&self) -> Option<(usize, &[Self])>;
}

// === Tuple tables ============================================================================

/// For each interned field name, each tuple type's offset for a field with that name (or
/// `None` when it has no such field). `GetNamed(name_id)` resolves a field against the
/// value's own tuple id via `offsets[name_id][tuple_id]` — two array lookups, no names at
/// runtime. Emission sites guarantee compatibility statically, so a `None` hit at runtime
/// is a compiler invariant violation.
pub fn compute_field_offsets(
    field_names: &[String],
    tuples: &[TupleTypeInfo],
) -> Vec<Vec<Option<usize>>> {
    field_names
        .iter()
        .map(|name| {
            tuples
                .iter()
                .map(|info| {
                    info.fields
                        .iter()
                        .position(|(fname, _)| fname.as_deref() == Some(name))
                })
                .collect()
        })
        .collect()
}

/// Map each tuple type-id to a canonical *value-shape* id: the lowest tuple-id that shares its
/// name and field labels (field *types* ignored). Two tuple values built via paths that inferred
/// different field types — e.g. a list `Cons` cell from a literal vs. from a recursive helper —
/// then carry shape-equal ids, so structural value equality (`==`) treats them as equal. The
/// executor stores only this mapping (indexed by tuple-id), never the names themselves.
pub fn compute_canonical_tuples(tuples: &[TupleTypeInfo]) -> Vec<usize> {
    let mut by_shape: HashMap<(Option<String>, Vec<Option<String>>), usize> = HashMap::new();
    tuples
        .iter()
        .enumerate()
        .map(|(id, info)| {
            let labels: Vec<Option<String>> =
                info.fields.iter().map(|(label, _)| label.clone()).collect();
            *by_shape.entry((info.name.clone(), labels)).or_insert(id)
        })
        .collect()
}

/// The tuple tables (canonical shapes and field offsets) maintained incrementally across an
/// append-only program's growth, as the environment ships them to its workers. `update`
/// yields exactly what the from-scratch functions produce (`assert_matches_full` checks this,
/// for validation runs).
#[derive(Debug, Clone, Default)]
pub struct TupleTables {
    /// Canonical value-shape id per tuple, as `compute_canonical_tuples` returns.
    pub canonical_tuples: Vec<usize>,
    /// The shape → lowest-id map behind `canonical_tuples`. Kept so appending tuples
    /// costs one lookup each rather than a rebuild: existing entries can never change,
    /// since the map holds the *lowest* id for a shape and ids only ever grow.
    shapes: HashMap<(Option<String>, Vec<Option<String>>), usize>,
    /// `[field name id][tuple id]` offsets, as `compute_field_offsets` returns.
    pub field_offsets: Vec<Vec<Option<usize>>>,
}

/// The extension one [`TupleTables::update`] call made — what a holder of the previous
/// tables needs to reach the new ones, and what a serializing transport ships in place of the
/// whole tables.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct TupleTablesDelta {
    /// Entries appended to `canonical_tuples` (existing entries never change).
    pub canonical_appended: Vec<usize>,
    /// Cells appended to each pre-existing `field_offsets` row, in row order — every
    /// old row grows by the same new-tuple range.
    pub field_offset_extensions: Vec<Vec<Option<usize>>>,
    /// Rows appended to `field_offsets` (full rows over every tuple).
    pub field_offset_rows: Vec<Vec<Option<usize>>>,
}

impl TupleTablesDelta {
    /// Extend a holder's tables — which must be exactly the state the producing
    /// `update` call started from — to the state it ended at.
    pub fn apply(
        self,
        canonical_tuples: &mut Vec<usize>,
        field_offsets: &mut Vec<Vec<Option<usize>>>,
    ) {
        canonical_tuples.extend(self.canonical_appended);
        for (row, cells) in field_offsets.iter_mut().zip(self.field_offset_extensions) {
            row.extend(cells);
        }
        field_offsets.extend(self.field_offset_rows);
    }
}

impl TupleTables {
    /// Extend the tables over whatever the program has grown by — both are pure functions
    /// of append-only tables, so a rebuild per update would be pure waste. With `want_delta`,
    /// also answer the [`TupleTablesDelta`] this call amounts to — requested only when a
    /// serializing transport will ship it.
    pub fn update(
        &mut self,
        tuples: &[TupleTypeInfo],
        field_names: &[String],
        want_delta: bool,
    ) -> Option<TupleTablesDelta> {
        let old_tuples = self.canonical_tuples.len();
        let old_field_rows = self.field_offsets.len();

        for (id, info) in tuples.iter().enumerate().skip(old_tuples) {
            let labels: Vec<Option<String>> =
                info.fields.iter().map(|(label, _)| label.clone()).collect();
            let canonical = *self.shapes.entry((info.name.clone(), labels)).or_insert(id);
            self.canonical_tuples.push(canonical);
        }
        // A new tuple appends one cell to every existing name's row...
        for (name_id, row) in self.field_offsets.iter_mut().enumerate() {
            let name = &field_names[name_id];
            for info in &tuples[row.len()..] {
                row.push(field_offset(info, name));
            }
        }
        // ... and a new name needs a row over every tuple.
        for name in &field_names[old_field_rows..] {
            self.field_offsets
                .push(tuples.iter().map(|info| field_offset(info, name)).collect());
        }

        want_delta.then(|| TupleTablesDelta {
            canonical_appended: self.canonical_tuples[old_tuples..].to_vec(),
            field_offset_extensions: self.field_offsets[..old_field_rows]
                .iter()
                .map(|row| row[old_tuples..].to_vec())
                .collect(),
            field_offset_rows: self.field_offsets[old_field_rows..].to_vec(),
        })
    }

    /// Assert the incremental tables equal a from-scratch computation — a mismatch is a bug
    /// in `update`. For validation runs.
    pub fn assert_matches_full(&self, tuples: &[TupleTypeInfo], field_names: &[String]) {
        assert_eq!(
            self.canonical_tuples,
            compute_canonical_tuples(tuples),
            "incremental canonical_tuples diverged from full recomputation"
        );
        assert_eq!(
            self.field_offsets,
            compute_field_offsets(field_names, tuples),
            "incremental field_offsets diverged from full recomputation"
        );
    }
}

fn field_offset(info: &TupleTypeInfo, name: &str) -> Option<usize> {
    info.fields
        .iter()
        .position(|(field, _)| field.as_deref() == Some(name))
}
