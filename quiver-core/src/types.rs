use serde::{Deserialize, Serialize};
use std::collections::HashSet;

use crate::binders::BinderPair;

/// Index of the NIL tuple type (always at index 0)
pub const NIL: usize = 0;

/// Index of the OK tuple type (always at index 1)
pub const OK: usize = 1;

/// Trait for looking up type information.
/// This trait exists in types.rs to break circular dependencies - it allows
/// Type methods to look up type info without depending on Program.
pub trait TypeLookup {
    fn lookup_type(&self, type_id: usize) -> Option<&Type>;
    fn lookup_tuple(&self, tuple_id: usize) -> Option<&TupleTypeInfo>;
    /// The display name of an annotation key (for rendering `Type::Annotated`).
    fn lookup_annotation_key_name(&self, _key: usize) -> Option<&str> {
        None
    }
    /// Look up a type, seeing through an annotation row to its base shape. Most
    /// structural questions ("is this callable?", "which tuple?") want this — a bare
    /// `lookup_type` on an annotated id sees `Type::Annotated` and fails shape matches.
    fn lookup_base(&self, type_id: usize) -> Option<&Type>
    where
        Self: Sized,
    {
        self.lookup_type(Type::strip_annotations(type_id, self))
    }
}

/// The unified type representation used throughout compiler, runtime, and bytecode.
/// All nested type references use IDs into a type registry.
///
/// Serialized through [`TypeRepr`], which spells the struct-shaped variants positionally.
/// Field names are a third of the bytes in a compiled program's largest table and carry no
/// information a reader of the schema does not already have; the in-memory form keeps them,
/// because that is where they are read.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
#[serde(from = "TypeRepr", into = "TypeRepr")]
pub enum Type {
    #[serde(rename = "int")]
    Integer,
    #[serde(rename = "bin")]
    Binary,
    #[serde(rename = "ref")]
    Reference,
    #[serde(rename = "tuple")]
    Tuple(usize),
    #[serde(rename = "partial")]
    Partial {
        name: Option<String>,
        fields: Vec<(String, usize)>, // (field_name, type_id) - all fields must be named
    },
    #[serde(rename = "fn")]
    Callable {
        parameter: usize,
        result: usize,
        receive: usize,
        /// The states a spawn of this function moves through: the union of parameter
        /// types over its root tail-call closure. `None` means
        /// unknown — a declared type without a `?` clause grants no sampling; inferred
        /// literals always carry `Some` unless poisoned by a `^~` on an unknown callee.
        states: Option<usize>,
        /// Parameter field indices whose label the caller may omit, written `(name):` in
        /// the parameter spelling. A calling convention, so it belongs to the *function
        /// type* and travels into declared boundaries — unlike a default, which is a
        /// value and rides the closure's `:defaults` row. Part of type identity, which is
        /// what keeps a marked spelling from silently granting omission to every
        /// structurally identical function; compatibility ignores it, so a marked and an
        /// unmarked function remain interchangeable as values. Sorted, no duplicates.
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        omittable: Vec<usize>,
    },
    #[serde(rename = "cycle")]
    Cycle(usize),
    #[serde(rename = "union")]
    Union(Vec<usize>),
    /// A carrier type (tuple/partial/callable/cycle — never union or annotated) with an
    /// **annotation row**: the annotations the value is statically known to carry.
    /// `exact` rows carry exactly these entries (freshly constructed or fully attached);
    /// open rows (`exact: false`) carry *at least* these — the value may hold further,
    /// erased annotations (it crossed a declared boundary). A plain, unwrapped type is
    /// equivalent to an open-empty row; that form is never interned. There is no surface
    /// syntax for rows — they are inferred, and print using the attach syntax.
    #[serde(rename = "annotated")]
    Annotated {
        base: usize,
        exact: bool,
        /// `(key id, value type id)`, sorted by key id, unique.
        entries: Vec<(usize, usize)>,
    },
    #[serde(rename = "process")]
    Process {
        send: Option<usize>,
        receive: Option<usize>,
        /// What `?p` samples (the process's observable state type). `None` = not granted:
        /// unlike `send`/`receive`, compatibility is strict in one direction (a state-less
        /// process type never satisfies a stated one) because bare `?p` has no runtime
        /// test — its soundness rests entirely on this component.
        state: Option<usize>,
    },
    #[serde(rename = "resource")]
    Resource(String),
    #[serde(rename = "var")]
    Variable(String),
    /// The top type, written `_`: every value belongs to it. Sound rather than permissive —
    /// nothing may be done with a `_` value but pass it on to another `_` or match it, which
    /// is how it is narrowed back to something usable. Nil is a member, so a `_` result can
    /// short-circuit like any other nilable one.
    #[serde(rename = "top")]
    Top,
    /// The values belonging to every member, `'t & 'u`, kept symbolic only while a type
    /// variable is among them — an intersection of known types is computed outright. It is
    /// what a generic body knows of a `'t` value after a match (`'t & ['int]`), and the nil a
    /// failing `'t` step leaves (`'t & []`: nil exactly when `'t` holds it). Substituting the
    /// variables computes it. Its members are the variables, sorted and distinct, followed by
    /// at most one other type, which is neither a union, an intersection nor a reference.
    #[serde(rename = "meet")]
    Intersection(Vec<usize>),
}

impl Type {
    /// The same type with every table reference rewritten through `remaps` — for
    /// transplanting types between programs. `Cycle` markers are relative (binder
    /// depth, not a table id) and pass through untouched, as do names.
    pub fn remap_ids(&self, remaps: &crate::bytecode::IdRemaps) -> Type {
        let ty = |id: &usize| crate::bytecode::IdRemaps::map(&remaps.types, "type", *id);
        match self {
            Type::Integer | Type::Binary | Type::Reference | Type::Cycle(_) | Type::Top => {
                self.clone()
            }
            Type::Resource(_) | Type::Variable(_) => self.clone(),
            Type::Tuple(tuple_id) => Type::Tuple(crate::bytecode::IdRemaps::map(
                &remaps.tuples,
                "tuple",
                *tuple_id,
            )),
            Type::Partial { name, fields } => Type::Partial {
                name: name.clone(),
                fields: fields
                    .iter()
                    .map(|(field, id)| (field.clone(), ty(id)))
                    .collect(),
            },
            Type::Callable {
                parameter,
                result,
                receive,
                states,
                omittable,
            } => Type::Callable {
                parameter: ty(parameter),
                result: ty(result),
                receive: ty(receive),
                states: states.as_ref().map(ty),
                // Field positions, not ids — nothing to remap.
                omittable: omittable.clone(),
            },
            Type::Union(members) => Type::Union(members.iter().map(ty).collect()),
            Type::Intersection(members) => Type::Intersection(members.iter().map(ty).collect()),
            Type::Annotated {
                base,
                exact,
                entries,
            } => Type::Annotated {
                base: ty(base),
                exact: *exact,
                // Key ids are remapped, so the sort that lookups binary-search on (and that
                // keeps interning canonical) has to be re-established, not assumed to survive.
                entries: {
                    let mut entries: Vec<(usize, usize)> = entries
                        .iter()
                        .map(|(key, value)| {
                            (
                                crate::bytecode::IdRemaps::map(
                                    &remaps.annotation_keys,
                                    "annotation key",
                                    *key,
                                ),
                                ty(value),
                            )
                        })
                        .collect();
                    entries.sort_by_key(|(key, _)| *key);
                    entries
                },
            },
            Type::Process {
                send,
                receive,
                state,
            } => Type::Process {
                send: send.as_ref().map(ty),
                receive: receive.as_ref().map(ty),
                state: state.as_ref().map(ty),
            },
        }
    }
}

/// Type alias for tuple field information: (optional name, type_id)
pub type TupleField = (Option<String>, usize);

/// Tuple type information: name and field definitions.
///
/// Serialized positionally, for the reason [`Type`] is — `"name"`/`"fields"` on every row of
/// the second-largest table in a compiled program.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(from = "TupleTypeRepr", into = "TupleTypeRepr")]
pub struct TupleTypeInfo {
    pub name: Option<String>,
    pub fields: Vec<TupleField>,
}

/// `[name, fields]` — the serialized spelling of [`TupleTypeInfo`].
#[derive(Serialize, Deserialize)]
struct TupleTypeRepr(Option<String>, Vec<TupleField>);

impl From<TupleTypeInfo> for TupleTypeRepr {
    fn from(info: TupleTypeInfo) -> Self {
        TupleTypeRepr(info.name, info.fields)
    }
}

impl From<TupleTypeRepr> for TupleTypeInfo {
    fn from(repr: TupleTypeRepr) -> Self {
        TupleTypeInfo {
            name: repr.0,
            fields: repr.1,
        }
    }
}

impl TupleTypeInfo {
    /// The same tuple info with field type ids rewritten through `remaps`.
    pub fn remap_ids(&self, remaps: &crate::bytecode::IdRemaps) -> TupleTypeInfo {
        TupleTypeInfo {
            name: self.name.clone(),
            fields: self
                .fields
                .iter()
                .map(|(name, id)| {
                    (
                        name.clone(),
                        crate::bytecode::IdRemaps::map(&remaps.types, "type", *id),
                    )
                })
                .collect(),
        }
    }
}

/// Builtin function information with type IDs
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BuiltinInfo {
    pub name: String,
    pub param_type: usize,
    pub result_type: usize,
    /// A type-consuming builtin's explicit type argument (`__data_decode__<'t>`), resolved
    /// to a concrete type at compile time. Such a builtin behaves differently per
    /// instantiation, so each one is a distinct table entry: the pair `(name,
    /// type_argument)` is what registration dedupes on, and the id alone identifies the
    /// instantiation for the `Builtin` instruction that pushes it.
    #[serde(default)]
    pub type_argument: Option<usize>,
    /// For an instantiation, the labels of the tuples it may build: those reached from its
    /// type argument and its result type (see [`crate::labels`]). A function of the pair
    /// registration dedupes on, so never compared.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub labels: Vec<crate::labels::ValueLabel>,
}

/// The serialized spelling of [`Type`]: the same variants, with the struct-shaped ones
/// written as arrays. Purely a wire/disk form — nothing reads it as a value.
///
/// Trailing components that are usually empty (`Callable::omittable`, `Annotated::entries`)
/// are omitted when they are, which is what keeps the common rows short: an exact-empty
/// annotation row is `{"annotated":[3,true]}`, and 82% of them are exactly that.
#[derive(Serialize, Deserialize)]
enum TypeRepr {
    #[serde(rename = "int")]
    Integer,
    #[serde(rename = "bin")]
    Binary,
    #[serde(rename = "ref")]
    Reference,
    #[serde(rename = "tuple")]
    Tuple(usize),
    #[serde(rename = "partial")]
    Partial(Option<String>, Vec<(String, usize)>),
    /// `[parameter, result, receive, states]`, plus `omittable` when non-empty.
    #[serde(rename = "fn")]
    Callable(
        usize,
        usize,
        usize,
        Option<usize>,
        #[serde(default, skip_serializing_if = "Vec::is_empty")] Vec<usize>,
    ),
    #[serde(rename = "cycle")]
    Cycle(usize),
    #[serde(rename = "union")]
    Union(Vec<usize>),
    /// `[base, exact]`, plus `entries` when non-empty.
    #[serde(rename = "annotated")]
    Annotated(
        usize,
        bool,
        #[serde(default, skip_serializing_if = "Vec::is_empty")] Vec<(usize, usize)>,
    ),
    #[serde(rename = "process")]
    Process(Option<usize>, Option<usize>, Option<usize>),
    #[serde(rename = "resource")]
    Resource(String),
    #[serde(rename = "var")]
    Variable(String),
    #[serde(rename = "top")]
    Top,
    #[serde(rename = "meet")]
    Intersection(Vec<usize>),
}

impl From<Type> for TypeRepr {
    fn from(ty: Type) -> Self {
        match ty {
            Type::Integer => TypeRepr::Integer,
            Type::Binary => TypeRepr::Binary,
            Type::Reference => TypeRepr::Reference,
            Type::Tuple(id) => TypeRepr::Tuple(id),
            Type::Partial { name, fields } => TypeRepr::Partial(name, fields),
            Type::Callable {
                parameter,
                result,
                receive,
                states,
                omittable,
            } => TypeRepr::Callable(parameter, result, receive, states, omittable),
            Type::Cycle(depth) => TypeRepr::Cycle(depth),
            Type::Union(members) => TypeRepr::Union(members),
            Type::Annotated {
                base,
                exact,
                entries,
            } => TypeRepr::Annotated(base, exact, entries),
            Type::Process {
                send,
                receive,
                state,
            } => TypeRepr::Process(send, receive, state),
            Type::Resource(name) => TypeRepr::Resource(name),
            Type::Variable(name) => TypeRepr::Variable(name),
            Type::Top => TypeRepr::Top,
            Type::Intersection(members) => TypeRepr::Intersection(members),
        }
    }
}

impl From<TypeRepr> for Type {
    fn from(repr: TypeRepr) -> Self {
        match repr {
            TypeRepr::Integer => Type::Integer,
            TypeRepr::Binary => Type::Binary,
            TypeRepr::Reference => Type::Reference,
            TypeRepr::Tuple(id) => Type::Tuple(id),
            TypeRepr::Partial(name, fields) => Type::Partial { name, fields },
            TypeRepr::Callable(parameter, result, receive, states, omittable) => Type::Callable {
                parameter,
                result,
                receive,
                states,
                omittable,
            },
            TypeRepr::Cycle(depth) => Type::Cycle(depth),
            TypeRepr::Union(members) => Type::Union(members),
            TypeRepr::Annotated(base, exact, entries) => Type::Annotated {
                base,
                exact,
                entries,
            },
            TypeRepr::Process(send, receive, state) => Type::Process {
                send,
                receive,
                state,
            },
            TypeRepr::Resource(name) => Type::Resource(name),
            TypeRepr::Variable(name) => Type::Variable(name),
            TypeRepr::Top => Type::Top,
            TypeRepr::Intersection(members) => Type::Intersection(members),
        }
    }
}

impl Type {
    /// Create a NIL tuple type
    pub fn nil() -> Self {
        Type::Tuple(NIL)
    }

    /// Create an OK tuple type
    pub fn ok() -> Self {
        Type::Tuple(OK)
    }

    /// Create a never type (empty union - bottom type)
    pub fn never() -> Self {
        Type::Union(vec![])
    }

    /// Check if this is the never type (empty union)
    pub fn is_never(&self) -> bool {
        matches!(self, Type::Union(types) if types.is_empty())
    }

    /// Check if this type is NIL
    pub fn is_nil(&self) -> bool {
        matches!(self, Type::Tuple(id) if *id == NIL)
    }

    /// Check if this type is OK
    pub fn is_ok(&self) -> bool {
        matches!(self, Type::Tuple(id) if *id == OK)
    }

    /// This type's parts, in a fixed order: a union's or intersection's members, a tuple's or
    /// partial type's field
    /// types, a function type's parameter, result, receive and (when known) states, a process
    /// type's stated send, receive and state, an annotated type's base and entry types. A
    /// binder's parts sit one binder deeper than it (`binders::is_binder`).
    /// `Program::with_parts` rebuilds a type from replacements in the same order.
    pub fn parts(&self, lookup: &impl TypeLookup) -> Vec<usize> {
        match self {
            Type::Union(members) | Type::Intersection(members) => members.clone(),
            Type::Tuple(tuple_id) => lookup
                .lookup_tuple(*tuple_id)
                .map(|info| info.fields.iter().map(|&(_, field)| field).collect())
                .unwrap_or_default(),
            Type::Partial { fields, .. } => fields.iter().map(|&(_, field)| field).collect(),
            Type::Callable {
                parameter,
                result,
                receive,
                states,
                ..
            } => [Some(*parameter), Some(*result), Some(*receive), *states]
                .into_iter()
                .flatten()
                .collect(),
            Type::Process {
                send,
                receive,
                state,
            } => [*send, *receive, *state].into_iter().flatten().collect(),
            Type::Annotated { base, entries, .. } => std::iter::once(*base)
                .chain(entries.iter().map(|&(_, value)| value))
                .collect(),
            Type::Cycle(_)
            | Type::Variable(_)
            | Type::Integer
            | Type::Binary
            | Type::Reference
            | Type::Resource(_)
            | Type::Top => Vec::new(),
        }
    }

    /// The base type once any annotation row is peeled: `T @ ρ` → `T`, anything else →
    /// itself. Returns the type id.
    pub fn strip_annotations<T: TypeLookup>(type_id: usize, lookup: &T) -> usize {
        match lookup.lookup_type(type_id) {
            Some(Type::Annotated { base, .. }) => *base,
            _ => type_id,
        }
    }

    /// Like [`Type::is_nil`], but sees through annotation rows (annotated nil is nil).
    pub fn is_nil_deep<T: TypeLookup>(&self, lookup: &T) -> bool {
        match self {
            Type::Annotated { base, .. } => lookup
                .lookup_type(*base)
                .is_some_and(|base_type| base_type.is_nil()),
            _ => self.is_nil(),
        }
    }

    /// Extract tuple IDs with type lookup
    pub fn extract_tuples_with_lookup<T: TypeLookup>(&self, lookup: &T) -> Vec<usize> {
        match self {
            Type::Union(type_ids) => type_ids
                .iter()
                .filter_map(|&type_id| {
                    lookup.lookup_type(type_id).and_then(|t| match t {
                        Type::Tuple(id) => Some(*id),
                        Type::Annotated { base, .. } => match lookup.lookup_type(*base) {
                            Some(Type::Tuple(id)) => Some(*id),
                            _ => None,
                        },
                        _ => None,
                    })
                })
                .collect(),
            Type::Tuple(id) => vec![*id],
            Type::Annotated { base, .. } => match lookup.lookup_type(*base) {
                Some(Type::Tuple(id)) => vec![*id],
                _ => vec![],
            },
            _ => vec![],
        }
    }

    /// Get variants for union types, or wrap single type
    /// Returns type IDs for unions, or a single-element vec with the type ID for non-unions
    pub fn variants<T: TypeLookup>(&self, self_id: usize, _lookup: &T) -> Vec<usize> {
        match self {
            Type::Union(type_ids) => type_ids.clone(),
            _ => vec![self_id],
        }
    }

    /// Check if this type contains NIL (either is NIL or contains NIL in union).
    /// Sees through annotation rows: annotated nil is nil for control flow. The top type
    /// holds every value, nil included, as `()` holds every tuple.
    pub fn contains_nil<T: TypeLookup>(&self, lookup: &T) -> bool {
        match self {
            Type::Union(type_ids) => type_ids
                .iter()
                .any(|&id| lookup.lookup_type(id).is_some_and(|t| t.holds_nil(lookup))),
            _ => self.holds_nil(lookup),
        }
    }

    /// Whether this non-union type may have nil among its values: nil itself, the top type,
    /// the unnamed partial with no fields, `()` — the only partial nil satisfies, having
    /// neither a name nor fields — or a type variable, which may be instantiated with a type
    /// holding it. An intersection may if each member may. Sees through annotation rows.
    fn holds_nil<T: TypeLookup>(&self, lookup: &T) -> bool {
        match self {
            Type::Tuple(id) => *id == NIL,
            Type::Top | Type::Variable(_) => true,
            Type::Partial { name, fields } => name.is_none() && fields.is_empty(),
            Type::Intersection(members) => members.iter().all(|&member| {
                lookup
                    .lookup_type(member)
                    .is_some_and(|t| t.holds_nil(lookup))
            }),
            Type::Annotated { base, .. } => lookup
                .lookup_type(*base)
                .is_some_and(|base_type| base_type.holds_nil(lookup)),
            _ => false,
        }
    }

    /// Return a type without NIL variants (annotated nils count as nil). The top type, `()`
    /// and a type variable have no spelling for "everything but nil", so they stay whole, as
    /// does an intersection unless it is some variable's nil (`'t & []`).
    pub fn without_nil<T: TypeLookup>(&self, lookup: &T) -> Type {
        match self {
            Type::Union(type_ids) => {
                let filtered: Vec<usize> = type_ids
                    .iter()
                    .filter(|&&id| !lookup.lookup_type(id).is_some_and(|t| t.is_only_nil(lookup)))
                    .copied()
                    .collect();
                Type::Union(filtered)
            }
            _ if self.is_only_nil(lookup) => Type::never(),
            _ => self.clone(),
        }
    }

    /// Whether every value of this non-union type is nil: nil (annotated or not), or an
    /// intersection with nil among its members.
    fn is_only_nil<T: TypeLookup>(&self, lookup: &T) -> bool {
        match self {
            Type::Intersection(members) => members.iter().any(|&member| {
                lookup
                    .lookup_type(member)
                    .is_some_and(|t| t.is_nil_deep(lookup))
            }),
            _ => self.is_nil_deep(lookup),
        }
    }

    /// Check compatibility with lookup context
    pub fn is_compatible_with<T: TypeLookup>(
        &self,
        self_id: usize,
        pattern_id: usize,
        lookup: &T,
    ) -> bool {
        is_compatible(self_id, pattern_id, lookup)
    }
}

/// Create a Type from a list of type IDs (creates a Union or simplifies to single type)
pub fn from_type_ids(type_ids: Vec<usize>) -> Type {
    match type_ids.len() {
        0 => Type::never(),
        1 => {
            // For a single type, we still wrap in Union for consistency
            // The caller can unwrap if needed
            Type::Union(type_ids)
        }
        _ => Type::Union(type_ids),
    }
}

/// Mode for handling unions in type compatibility checks.
#[derive(Clone, Copy, PartialEq, Eq)]
enum UnionMode {
    /// All variants must be compatible (for type assignment/subtyping).
    /// Used by `is_compatible`: "Is type A assignable to type B?"
    All,
    /// Any variant could match (for pattern matching).
    /// Used by `types_overlap`: "Could a value of type A match pattern B?"
    Any,
    /// `All`, holding only where assignability is proven rather than presumed: a type
    /// variable relates only to itself, a reference the walk cannot resolve relates to
    /// nothing, an ungranted process capability does not satisfy a granted one, and a
    /// function type does not satisfy one that lets its callers omit more labels.
    /// Used by `is_subsumed_by`: "Does every value of type A belong to type B?"
    Subsumption,
}

/// Check if type `self_id` is compatible with (assignable to) type `pattern_id`.
///
/// Uses ALL-variant semantics: for `A | B` to be compatible with `C`,
/// both `A` and `C` must be compatible AND `B` and `C` must be compatible.
///
/// This is used for type checking (can I assign this value to this variable?).
pub fn is_compatible<T: TypeLookup>(self_id: usize, pattern_id: usize, lookup: &T) -> bool {
    let mut assumptions = HashSet::new();
    let mut stacks = BinderPair::default();
    check_type_relation(
        self_id,
        pattern_id,
        lookup,
        UnionMode::All,
        &mut assumptions,
        &mut stacks,
    )
}

/// Whether every value of `self_id` belongs to `pattern_id`, so a union holding both
/// needs only `pattern_id`. Stricter than [`is_compatible`], which gives the benefit of the
/// doubt to type variables, unresolved references and ungranted capabilities — a verdict
/// that only admits a value, where this one deletes a type.
pub fn is_subsumed_by<T: TypeLookup>(self_id: usize, pattern_id: usize, lookup: &T) -> bool {
    let mut assumptions = HashSet::new();
    let mut stacks = BinderPair::default();
    check_type_relation(
        self_id,
        pattern_id,
        lookup,
        UnionMode::Subsumption,
        &mut assumptions,
        &mut stacks,
    )
}

/// Check if types `self_id` and `pattern_id` overlap (have any common values).
///
/// Uses ANY-variant semantics: for `A | B` to overlap with `C`,
/// either `A` overlaps with `C` OR `B` overlaps with `C`.
///
/// This is used for pattern matching (could this value possibly match this pattern?).
pub fn types_overlap<T: TypeLookup>(self_id: usize, pattern_id: usize, lookup: &T) -> bool {
    let mut assumptions = HashSet::new();
    let mut stacks = BinderPair::default();
    check_type_relation(
        self_id,
        pattern_id,
        lookup,
        UnionMode::Any,
        &mut assumptions,
        &mut stacks,
    )
}

/// Row compatibility for `Annotated <= Annotated` under assignability:
/// - target open: every target entry must exist in the source, covariantly;
/// - target exact: only an exact source with the same key set satisfies it, covariantly;
/// - an open source never satisfies an exact target.
fn rows_compatible<T: TypeLookup>(
    (source_exact, source_entries): (bool, &[(usize, usize)]),
    (target_exact, target_entries): (bool, &[(usize, usize)]),
    lookup: &T,
    mode: UnionMode,
    assumptions: &mut HashSet<(usize, usize)>,
    stacks: &mut BinderPair,
) -> bool {
    if target_exact && (!source_exact || source_entries.len() != target_entries.len()) {
        return false;
    }
    target_entries.iter().all(|(key, target_type)| {
        source_entries
            .binary_search_by_key(key, |(k, _)| *k)
            .is_ok_and(|index| {
                check_type_relation(
                    source_entries[index].1,
                    *target_type,
                    lookup,
                    mode,
                    assumptions,
                    stacks,
                )
            })
    })
}

/// Unified implementation of type relation checking.
///
/// When `mode` is `All` (used by `is_compatible`):
/// - Empty union is compatible with anything (bottom type)
/// - Union on left: ALL variants must satisfy the relation
///
/// When `mode` is `Any` (used by `types_overlap`):
/// - Empty union can't match anything
/// - Union on left: ANY variant could satisfy the relation
fn check_type_relation<T: TypeLookup>(
    self_id: usize,
    pattern_id: usize,
    lookup: &T,
    mode: UnionMode,
    assumptions: &mut HashSet<(usize, usize)>,
    stacks: &mut BinderPair,
) -> bool {
    // Fast path: same ID always satisfies the relation
    if self_id == pattern_id {
        return true;
    }

    // Check if we've already assumed this relation holds (coinductive hypothesis)
    let key = (self_id, pattern_id);
    if assumptions.contains(&key) {
        return true;
    }

    let Some(self_type) = lookup.lookup_type(self_id) else {
        return false;
    };
    let Some(pattern_type) = lookup.lookup_type(pattern_id) else {
        return false;
    };

    match (self_type, pattern_type) {
        // Empty union (never type) handling depends on mode
        (Type::Union(variants), _) if variants.is_empty() => {
            match mode {
                UnionMode::All | UnionMode::Subsumption => true, // Bottom type is subtype of everything
                UnionMode::Any => false,                         // Empty type can't match anything
            }
        }

        // Every value belongs to the top type, so everything is assignable to it, and every
        // inhabited type (never is handled above) overlaps it.
        (_, Type::Top) => true,

        // Basic types must match exactly
        (Type::Integer, Type::Integer) => true,
        (Type::Binary, Type::Binary) => true,
        (Type::Reference, Type::Reference) => true,

        // Resource types must have matching identifiers
        (Type::Resource(r1), Type::Resource(r2)) => r1 == r2,

        // An intersection on the right: every member must hold `self`'s values. For overlap, a
        // shared value lies in each member, so each must overlap — a necessary condition, so an
        // over-approximation, which is the safe side for overlap. A union on the left is split
        // first, by its own arm.
        (self_type, Type::Intersection(members)) if !matches!(self_type, Type::Union(_)) => {
            members.iter().all(|&member| {
                check_type_relation(self_id, member, lookup, mode, assumptions, stacks)
            })
        }
        // An intersection on the left: its values lie in every member, so one member fitting
        // the pattern suffices; for overlap, every member must overlap it. A union on the right
        // is tried member by member, by its own arm.
        (Type::Intersection(members), pattern) if !matches!(pattern, Type::Union(_)) => {
            match mode {
                UnionMode::All | UnionMode::Subsumption => members.iter().any(|&member| {
                    check_type_relation(member, pattern_id, lookup, mode, assumptions, stacks)
                }),
                UnionMode::Any => members.iter().all(|&member| {
                    check_type_relation(member, pattern_id, lookup, mode, assumptions, stacks)
                }),
            }
        }

        // Type variables match anything, except when proving subsumption, where a variable
        // is rigid: it stands for one unknown type, so only it is known to hold its values.
        // A union on the other side is split by its own arm.
        (Type::Variable(_), _) | (_, Type::Variable(_)) if mode != UnionMode::Subsumption => {
            true
        }
        (Type::Variable(v1), Type::Variable(v2)) => v1 == v2,
        (Type::Variable(_), other) | (other, Type::Variable(_))
            if !matches!(other, Type::Union(_)) =>
        {
            false
        }

        // When both are cycles with same depth, they refer to the same recursive type —
        // presumed, unless proving subsumption, where both must resolve within the walk.
        (Type::Cycle(d1), Type::Cycle(d2)) if d1 == d2 => {
            mode != UnionMode::Subsumption
                || (stacks.left.resolve(*d1).is_some() && stacks.right.resolve(*d2).is_some())
        }

        // Handle cycles by looking up the type in the stack. Following a `^` *re-enters* the
        // binder it names, so the traversal returns to that binder's own depth rather than
        // nesting one deeper: the enclosing binders of the referenced type are exactly those
        // below it on the stack. Truncating to it (the binder is pushed again when the union
        // arm re-enters it) is what keeps `Cycle(n)` counting the same binders the type was
        // written against — without it, a `^1` back-edge through a list's own knot shifted
        // every outer `^` by one per element.
        //
        // Each side resolves against its *own* binders: a `^` in `self` names a boundary of the
        // type `self` came from, never one of the pattern's. (A single shared stack resolved a
        // left-hand `^` against whatever union the right-hand side had last entered — so a
        // declared `L['int, Nil | Cons[…, ^]]` failed to overlap a union holding an `L` value
        // whenever that union had been entered first.)
        (Type::Cycle(depth), _) => {
            let Some((binder, cut)) = stacks.left.follow(*depth) else {
                // A reference reaching past the walk's roots: presumed to relate, except
                // when proving subsumption.
                return mode != UnionMode::Subsumption;
            };
            let result = check_type_relation(binder, pattern_id, lookup, mode, assumptions, stacks);
            stacks.left.restore(cut);
            result
        }

        (_, Type::Cycle(depth)) => {
            let Some((binder, cut)) = stacks.right.follow(*depth) else {
                return mode != UnionMode::Subsumption;
            };
            let result = check_type_relation(self_id, binder, lookup, mode, assumptions, stacks);
            stacks.right.restore(cut);
            result
        }

        // Annotation rows. Overlap (`Any`) treats rows as transparent — pattern matching
        // ignores annotations. Assignability (`All`) applies the row rules: a plain type
        // is an open-empty row; forgetting on the left is free; an open row never
        // satisfies an exactness claim.
        (
            Type::Annotated {
                base: base1,
                exact: exact1,
                entries: entries1,
            },
            Type::Annotated {
                base: base2,
                exact: exact2,
                entries: entries2,
            },
        ) => {
            if !check_type_relation(*base1, *base2, lookup, mode, assumptions, stacks) {
                return false;
            }
            if mode == UnionMode::Any {
                return true;
            }
            rows_compatible(
                (*exact1, entries1),
                (*exact2, entries2),
                lookup,
                mode,
                assumptions,
                stacks,
            )
        }
        // Annotated on the left vs a non-union pattern: forget the row (any row `<=`
        // open-empty). Unions/cycles on the right are handled by their own arms so the
        // row can still match an annotated member inside them.
        (Type::Annotated { base, .. }, pattern) if !matches!(pattern, Type::Union(_)) => {
            check_type_relation(*base, pattern_id, lookup, mode, assumptions, stacks)
        }
        // Plain (open-empty) on the left vs an annotated pattern: sound only when the
        // pattern demands nothing — but such rows are normalised away, so under `All`
        // this fails. Overlap stays row-transparent.
        (self_type, Type::Annotated { base, .. }) if !matches!(self_type, Type::Union(_)) => {
            match mode {
                UnionMode::Any => {
                    check_type_relation(self_id, *base, lookup, mode, assumptions, stacks)
                }
                UnionMode::All | UnionMode::Subsumption => false,
            }
        }

        // Union on left side: mode determines ALL vs ANY semantics
        (Type::Union(variants), _) => {
            // Insert assumption for recursive types
            assumptions.insert(key);

            // A union is a binder its members' `^` count back to, on this side's stack (see the
            // union-on-right arm for why the push is unconditional).
            stacks.left.enter(self_id);
            let result = match mode {
                UnionMode::All | UnionMode::Subsumption => variants.iter().all(|&variant_id| {
                    check_type_relation(variant_id, pattern_id, lookup, mode, assumptions, stacks)
                }),
                UnionMode::Any => variants.iter().any(|&variant_id| {
                    check_type_relation(variant_id, pattern_id, lookup, mode, assumptions, stacks)
                }),
            };
            stacks.left.leave(self_id);
            result
        }

        // Union on right side: self must match ANY variant (same for both modes)
        (_, Type::Union(variants)) => {
            // Record the coinductive hypothesis (as the union-on-left arm does) so a back-edge
            // that returns to this same pair — e.g. a recursive type reached through a
            // union-on-right then a cycle — terminates at the assumption check above instead of
            // recursing without bound.
            assumptions.insert(key);

            // Push unconditionally, even for a union already on the stack. A `Cycle(n)` is
            // resolved by counting `n` entries back from the top, so the stack must mirror
            // the binder nesting of the path taken through the pattern — re-entering the
            // root union (as a `^` in one member does) is one binder deeper, not the same
            // one. Skipping the push flattened that path, and a `^` reached through a
            // *different* member's binder depth then resolved to the wrong ancestor: a JSON
            // array nested in an object was rejected while an object in an object was not.
            // Termination is the `assumptions` hypothesis above, not stack dedup.
            stacks.right.enter(pattern_id);
            let result = variants.iter().any(|&variant_id| {
                check_type_relation(self_id, variant_id, lookup, mode, assumptions, stacks)
            });
            stacks.right.leave(pattern_id);
            result
        }

        // The top type fits nothing narrower (`_` on the right, and unions and references
        // that may reach it, are handled above), but it overlaps anything inhabited.
        (Type::Top, _) => mode == UnionMode::Any,

        // Tuple vs Tuple: structural in both modes. Two tuples are related iff they share a
        // name and arity and every field pair is related under the same mode — for ALL that
        // is field-wise assignability, for ANY (overlap) it is field-wise overlap (the tuples
        // share a value iff every field can). Recursing rather than comparing ids lets ANY mode
        // see that, e.g., `[Rational, 'n]` overlaps `[Rational, 'int]`.
        (Type::Tuple(id1), Type::Tuple(id2)) => {
            if id1 == id2 {
                return true;
            }

            let Some(info1) = lookup.lookup_tuple(*id1) else {
                return false;
            };
            let Some(info2) = lookup.lookup_tuple(*id2) else {
                return false;
            };

            info1.name == info2.name
                && info1.fields.len() == info2.fields.len()
                && info1.fields.iter().zip(info2.fields.iter()).all(
                    |((fname1, ftype1), (fname2, ftype2))| {
                        fname1 == fname2
                            && check_type_relation(
                                *ftype1,
                                *ftype2,
                                lookup,
                                mode,
                                assumptions,
                                stacks,
                            )
                    },
                )
        }

        // Concrete tuple vs partial type
        (
            Type::Tuple(concrete_id),
            Type::Partial {
                name: partial_name,
                fields: partial_fields,
            },
        ) => {
            let Some(concrete_info) = lookup.lookup_tuple(*concrete_id) else {
                return false;
            };

            // If partial has a name, concrete must match it
            if let Some(pname) = partial_name
                && concrete_info.name.as_ref() != Some(pname)
            {
                return false;
            }

            // Check that all partial fields exist in concrete with compatible types
            partial_fields.iter().all(|(partial_fname, partial_ftype)| {
                concrete_info
                    .fields
                    .iter()
                    .any(|(concrete_fname, concrete_ftype)| {
                        concrete_fname.as_ref() == Some(partial_fname)
                            && check_type_relation(
                                *concrete_ftype,
                                *partial_ftype,
                                lookup,
                                mode,
                                assumptions,
                                stacks,
                            )
                    })
            })
        }

        // Partial vs partial - check structural compatibility
        (
            Type::Partial {
                name: name1,
                fields: fields1,
            },
            Type::Partial {
                name: name2,
                fields: fields2,
            },
        ) => {
            // Names must match if both have names. Proving subsumption, a named pattern also
            // needs the name on the left: an unnamed partial holds tuples of any name.
            if name1.is_some() && name2.is_some() && name1 != name2 {
                return false;
            }
            if mode == UnionMode::Subsumption && name2.is_some() && name1.is_none() {
                return false;
            }

            match mode {
                // Assignability: every field the pattern constrains must be constrained by
                // self, compatibly.
                UnionMode::All | UnionMode::Subsumption => {
                    fields2.iter().all(|(fname2, ftype2)| {
                        fields1.iter().any(|(fname1, ftype1)| {
                            fname1 == fname2
                                && check_type_relation(
                                    *ftype1,
                                    *ftype2,
                                    lookup,
                                    mode,
                                    assumptions,
                                    stacks,
                                )
                        })
                    })
                }
                // Overlap: a field only one side constrains is free on the other, so only the
                // fields both constrain must overlap.
                UnionMode::Any => fields2.iter().all(|(fname2, ftype2)| {
                    fields1
                        .iter()
                        .filter(|(fname1, _)| fname1 == fname2)
                        .all(|(_, ftype1)| {
                            check_type_relation(*ftype1, *ftype2, lookup, mode, assumptions, stacks)
                        })
                }),
            }
        }

        // Partial vs concrete tuple. A partial holds tuples with fields it says nothing about,
        // so it is never assignable to a concrete tuple; it overlaps one whose name it allows
        // and which has every field it constrains, overlapping.
        (
            Type::Partial {
                name: partial_name,
                fields: partial_fields,
            },
            Type::Tuple(concrete_id),
        ) => {
            if mode != UnionMode::Any {
                return false;
            }
            let Some(concrete_info) = lookup.lookup_tuple(*concrete_id) else {
                return false;
            };
            if let Some(pname) = partial_name
                && concrete_info.name.as_ref() != Some(pname)
            {
                return false;
            }
            partial_fields.iter().all(|(partial_fname, partial_ftype)| {
                concrete_info
                    .fields
                    .iter()
                    .any(|(concrete_fname, concrete_ftype)| {
                        concrete_fname.as_ref() == Some(partial_fname)
                            && check_type_relation(
                                *partial_ftype,
                                *concrete_ftype,
                                lookup,
                                mode,
                                assumptions,
                                stacks,
                            )
                    })
            })
        }

        // Process types
        (
            Type::Process {
                send: send1,
                receive: receive1,
                state: state1,
            },
            Type::Process {
                send: send2,
                receive: receive2,
                state: state2,
            },
        ) => {
            // Send is CONTRAVARIANT: the declared type promises what may be sent
            // through the handle, so the actual process must accept at least that —
            // a handle that receives MORE is safe wherever fewer sends are promised
            // (e.g. a supervisor's self-handle, whose inferred receive is its whole
            // message union, flowing into a `@'down` report parameter). An EMPTY
            // actual send is treated as unknown rather than rejected: it arises as
            // an inference artifact when `&.` is taken before the enclosing
            // function's receive has been widened by its calls (the receive
            // pre-pass only sees syntactic selects).
            //
            // Proving subsumption, neither leniency applies: an empty send is taken at its
            // word, and an ungranted send or await does not satisfy a granted one.
            let strict = mode == UnionMode::Subsumption;
            let send_ok = match (send1, send2) {
                (Some(s1), Some(s2)) => {
                    (!strict && lookup.lookup_type(*s1).is_some_and(|t| t.is_never()))
                        || stacks.swapped(|stacks| {
                            check_type_relation(*s2, *s1, lookup, mode, assumptions, stacks)
                        })
                }
                (None, Some(_)) => !strict,
                (_, None) => true,
            };

            // Receive is the AWAIT RESULT (`-> 'r`), covariant like any result.
            let receive_ok = match (receive1, receive2) {
                (Some(r1), Some(r2)) => {
                    check_type_relation(*r1, *r2, lookup, mode, assumptions, stacks)
                }
                (None, Some(_)) => !strict,
                (_, None) => true,
            };

            // State is covariant and strict against a stated expectation: `?p` has no
            // runtime test, so a state-less process value must never satisfy a process
            // type that grants sampling. Dropping the grant (Some → None) is fine.
            let state_ok = match (state1, state2) {
                (Some(s1), Some(s2)) => {
                    check_type_relation(*s1, *s2, lookup, mode, assumptions, stacks)
                }
                (_, None) => true,
                (None, Some(_)) => false,
            };

            send_ok && receive_ok && state_ok
        }

        // Callable types. Omittable labels are deliberately not compared: they are a
        // calling convention, so a marked and an unmarked function of the same shape are
        // interchangeable as values — what the marks govern is how a *call written
        // against this type* may spell its argument, which the declared type decides.
        // Proving subsumption is the exception, as the type kept then governs calls on
        // values of both: it may not let callers omit a label the other did not.
        (
            Type::Callable {
                parameter: param1,
                result: result1,
                receive: receive1,
                states: states1,
                omittable: omittable1,
            },
            Type::Callable {
                parameter: param2,
                result: result2,
                receive: receive2,
                states: states2,
                omittable: omittable2,
            },
        ) => {
            if mode == UnionMode::Subsumption
                && !omittable2.iter().all(|index| omittable1.contains(index))
            {
                return false;
            }

            // A function type is a binder too, on each side's own stack.
            stacks.left.enter(self_id);
            stacks.right.enter(pattern_id);

            // States are covariant and strict against a stated expectation, exactly as a
            // process type's state component (a spawn of this function inherits it).
            let states_ok = match (states1, states2) {
                (Some(s1), Some(s2)) => {
                    check_type_relation(*s1, *s2, lookup, mode, assumptions, stacks)
                }
                (_, None) => true,
                (None, Some(_)) => false,
            };

            // Parameters are contravariant, results are covariant, receive is contravariant
            let result = states_ok
                && stacks.swapped(|stacks| {
                    check_type_relation(*param2, *param1, lookup, mode, assumptions, stacks)
                })
                && check_type_relation(*result1, *result2, lookup, mode, assumptions, stacks)
                && stacks.swapped(|stacks| {
                    check_type_relation(*receive2, *receive1, lookup, mode, assumptions, stacks)
                });

            stacks.right.leave(pattern_id);
            stacks.left.leave(self_id);
            result
        }

        _ => false,
    }
}

#[cfg(test)]
mod annotation_row_tests {
    use super::*;
    use crate::program::Program;

    /// Annotation lookups binary-search a row by key id, so when linking renumbers the keys,
    /// the row must be re-sorted under the new ids rather than keep its old order.
    #[test]
    fn annotated_entries_resort_after_remap() {
        let mut remaps = crate::bytecode::IdRemaps::default();
        remaps.types.extend([(0, 10), (3, 30), (4, 40)]);
        remaps.annotation_keys.extend([(1, 9), (2, 8)]);
        let row = Type::Annotated {
            base: 0,
            exact: false,
            entries: vec![(1, 3), (2, 4)],
        };
        assert_eq!(
            row.remap_ids(&remaps),
            Type::Annotated {
                base: 10,
                exact: false,
                entries: vec![(8, 40), (9, 30)],
            }
        );
    }

    fn setup() -> (Program, usize, usize, usize, usize) {
        let mut p = Program::new();
        let nil = p.register_type(Type::nil());
        let int = p.register_type(Type::Integer);
        let bin = p.register_type(Type::Binary);
        let tuple_a = {
            let id = p.register_tuple(Some("A".to_string()), vec![(None, int)]);
            p.register_type(Type::Tuple(id))
        };
        (p, nil, int, bin, tuple_a)
    }

    #[test]
    fn open_empty_rows_are_never_interned() {
        let (mut p, nil, ..) = setup();
        assert_eq!(p.annotate_type(nil, false, vec![]), nil);
    }

    #[test]
    fn annotating_distributes_over_unions_and_folds_nested_rows() {
        let (mut p, nil, int, ..) = setup();
        let union = p.register_type(Type::Union(vec![nil, int]));
        let annotated = p.annotate_type(union, true, vec![(0, int)]);
        let Some(Type::Union(members)) = p.lookup_type(annotated).cloned() else {
            panic!("expected union");
        };
        assert!(members.iter().all(|&m| matches!(
            p.lookup_type(m),
            // Non-carrier members (int) keep no row only if annotate wraps them too —
            // distribution wraps each member; carrier-ness is the caller's check.
            Some(Type::Annotated { .. })
        )));

        // Re-annotating replaces the entry rather than nesting.
        let wrapped = p.annotate_type(nil, true, vec![(0, int)]);
        let replaced = p.annotate_type(wrapped, true, vec![(0, nil)]);
        let Some(Type::Annotated { base, entries, .. }) = p.lookup_type(replaced) else {
            panic!("expected annotated");
        };
        assert_eq!(*base, nil);
        assert_eq!(entries, &vec![(0, nil)]);
    }

    #[test]
    fn row_subtyping_rules() {
        let (mut p, nil, int, bin, tuple_a) = setup();
        let exact_x = p.annotate_type(nil, true, vec![(0, int)]);
        let exact_wider = p.annotate_type(nil, true, vec![(0, bin)]);
        let exact_two = p.annotate_type(nil, true, vec![(0, int), (1, int)]);
        let open_x = p.annotate_type(nil, false, vec![(0, int)]);
        let closed_empty = p.annotate_type(nil, true, vec![]);

        // Forgetting on the left: any row <= plain (open-empty).
        assert!(is_compatible(exact_x, nil, &p));
        assert!(is_compatible(open_x, nil, &p));
        assert!(is_compatible(closed_empty, nil, &p));
        // Plain (open) never satisfies a row demand under All.
        assert!(!is_compatible(nil, exact_x, &p));
        assert!(!is_compatible(nil, closed_empty, &p));
        // exact <= open with the entries present.
        assert!(is_compatible(exact_x, open_x, &p));
        // open <= exact never.
        assert!(!is_compatible(open_x, exact_x, &p));
        // exact <= exact: same key set, entrywise covariant.
        assert!(is_compatible(exact_x, exact_x, &p));
        assert!(!is_compatible(exact_x, exact_wider, &p)); // int vs bin entry
        assert!(!is_compatible(exact_two, exact_x, &p)); // extra key breaks exactness
        assert!(!is_compatible(exact_x, exact_two, &p));
        // Base mismatch fails regardless of rows.
        let exact_on_a = p.annotate_type(tuple_a, true, vec![(0, int)]);
        assert!(!is_compatible(exact_on_a, exact_x, &p));

        // Annotated member matches inside a union on the right.
        let union = p.register_type(Type::Union(vec![int, exact_x]));
        assert!(is_compatible(exact_x, union, &p));
    }

    #[test]
    fn overlap_is_row_transparent() {
        let (mut p, nil, int, ..) = setup();
        let exact_x = p.annotate_type(nil, true, vec![(0, int)]);
        // Pattern matching ignores rows: annotated nil overlaps plain nil both ways.
        assert!(types_overlap(exact_x, nil, &p));
        assert!(types_overlap(nil, exact_x, &p));
        assert!(!types_overlap(exact_x, int, &p));
    }

    #[test]
    fn nil_helpers_see_through_rows() {
        let (mut p, nil, int, ..) = setup();
        let annotated_nil = p.annotate_type(nil, true, vec![(0, int)]);
        let t = p.lookup_type(annotated_nil).unwrap().clone();
        assert!(t.is_nil_deep(&p));
        assert!(t.contains_nil(&p));
        assert!(t.without_nil(&p).is_never());
        let union = Type::Union(vec![int, annotated_nil]);
        assert!(union.contains_nil(&p));
    }

    /// Every variant must survive the positional serialized spelling, including the
    /// components that are omitted when empty and the `Option`s that are `null` when absent.
    /// A hand-shaped codec is exactly where a variant gets forgotten, so this enumerates them.
    #[test]
    fn types_round_trip_through_their_serialized_form() {
        let types = vec![
            Type::Integer,
            Type::Binary,
            Type::Reference,
            Type::Tuple(7),
            Type::Partial {
                name: None,
                fields: vec![],
            },
            Type::Partial {
                name: Some("Point".to_string()),
                fields: vec![("x".to_string(), 1), ("y".to_string(), 2)],
            },
            Type::Callable {
                parameter: 1,
                result: 2,
                receive: 3,
                states: None,
                omittable: vec![],
            },
            Type::Callable {
                parameter: 1,
                result: 2,
                receive: 3,
                states: Some(4),
                omittable: vec![0, 2],
            },
            Type::Cycle(2),
            Type::Union(vec![]),
            Type::Union(vec![1, 2, 3]),
            Type::Annotated {
                base: 5,
                exact: true,
                entries: vec![],
            },
            Type::Annotated {
                base: 5,
                exact: false,
                entries: vec![(1, 2), (3, 4)],
            },
            Type::Process {
                send: None,
                receive: None,
                state: None,
            },
            Type::Process {
                send: Some(1),
                receive: Some(2),
                state: Some(3),
            },
            Type::Resource("TcpSocket".to_string()),
            Type::Variable("t#1".to_string()),
        ];
        let json = serde_json::to_string(&types).expect("serialize");
        let restored: Vec<Type> = serde_json::from_str(&json).expect("deserialize");
        assert_eq!(restored, types);

        // The shape the size win rests on: an exact-empty annotation row, which is 82% of
        // them, and a states-less callable.
        assert_eq!(
            serde_json::to_string(&Type::Annotated {
                base: 3,
                exact: true,
                entries: vec![]
            })
            .unwrap(),
            r#"{"annotated":[3,true]}"#
        );
        assert_eq!(
            serde_json::to_string(&Type::Callable {
                parameter: 1,
                result: 2,
                receive: 3,
                states: None,
                omittable: vec![]
            })
            .unwrap(),
            r#"{"fn":[1,2,3,null]}"#
        );
    }

    #[test]
    fn tuple_infos_round_trip_through_their_serialized_form() {
        let infos = vec![
            TupleTypeInfo {
                name: None,
                fields: vec![],
            },
            TupleTypeInfo {
                name: Some("Cons".to_string()),
                fields: vec![(None, 1), (Some("rest".to_string()), 2)],
            },
        ];
        let json = serde_json::to_string(&infos).expect("serialize");
        assert_eq!(json, r#"[[null,[]],["Cons",[[null,1],["rest",2]]]]"#);
        let restored: Vec<TupleTypeInfo> = serde_json::from_str(&json).expect("deserialize");
        assert_eq!(restored, infos);
    }
}
