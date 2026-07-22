use serde::{Deserialize, Serialize};
use std::collections::HashSet;

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
    /// Whether a tuple field's label was written omittable (`[(foo): 'int]`), letting a
    /// positional literal checked against the tuple type adopt it. Spelling metadata,
    /// never part of type identity.
    fn label_omittable(&self, _tuple_id: usize, _field_index: usize) -> bool {
        false
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
#[derive(Debug, PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
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
}

/// Type alias for tuple field information: (optional name, type_id)
pub type TupleField = (Option<String>, usize);

/// Tuple type information: name and field definitions
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TupleTypeInfo {
    pub name: Option<String>,
    pub fields: Vec<TupleField>,
}

/// Builtin function information with type IDs
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BuiltinInfo {
    pub name: String,
    pub param_type: usize,
    pub result_type: usize,
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
    /// Sees through annotation rows: annotated nil is nil for control flow.
    pub fn contains_nil<T: TypeLookup>(&self, lookup: &T) -> bool {
        match self {
            Type::Tuple(id) if *id == NIL => true,
            Type::Annotated { .. } => self.is_nil_deep(lookup),
            Type::Union(type_ids) => type_ids.iter().any(|&id| {
                lookup
                    .lookup_type(id)
                    .map(|t| t.is_nil_deep(lookup))
                    .unwrap_or(false)
            }),
            _ => false,
        }
    }

    /// Return a type without NIL variants (annotated nils count as nil).
    pub fn without_nil<T: TypeLookup>(&self, lookup: &T) -> Type {
        match self {
            Type::Tuple(id) if *id == NIL => Type::never(),
            Type::Annotated { .. } if self.is_nil_deep(lookup) => Type::never(),
            Type::Union(type_ids) => {
                let filtered: Vec<usize> = type_ids
                    .iter()
                    .filter(|&&id| {
                        !lookup
                            .lookup_type(id)
                            .map(|t| t.is_nil_deep(lookup))
                            .unwrap_or(false)
                    })
                    .copied()
                    .collect();
                Type::Union(filtered)
            }
            _ => self.clone(),
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
}

/// Check if type `self_id` is compatible with (assignable to) type `pattern_id`.
///
/// Uses ALL-variant semantics: for `A | B` to be compatible with `C`,
/// both `A` and `C` must be compatible AND `B` and `C` must be compatible.
///
/// This is used for type checking (can I assign this value to this variable?).
pub fn is_compatible<T: TypeLookup>(self_id: usize, pattern_id: usize, lookup: &T) -> bool {
    let mut assumptions = HashSet::new();
    let mut type_stack = Vec::new();
    check_type_relation(
        self_id,
        pattern_id,
        lookup,
        UnionMode::All,
        &mut assumptions,
        &mut type_stack,
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
    let mut type_stack = Vec::new();
    check_type_relation(
        self_id,
        pattern_id,
        lookup,
        UnionMode::Any,
        &mut assumptions,
        &mut type_stack,
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
    type_stack: &mut Vec<usize>,
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
                    type_stack,
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
    type_stack: &mut Vec<usize>,
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
                UnionMode::All => true,  // Bottom type is subtype of everything
                UnionMode::Any => false, // Empty type can't match anything
            }
        }

        // Basic types must match exactly
        (Type::Integer, Type::Integer) => true,
        (Type::Binary, Type::Binary) => true,
        (Type::Reference, Type::Reference) => true,

        // Resource types must have matching identifiers
        (Type::Resource(r1), Type::Resource(r2)) => r1 == r2,

        // Type variables match anything
        (Type::Variable(_), _) | (_, Type::Variable(_)) => true,

        // When both are cycles with same depth, they refer to the same recursive type
        (Type::Cycle(d1), Type::Cycle(d2)) if d1 == d2 => true,

        // Handle cycles by looking up the type in the stack
        (Type::Cycle(depth), _) => {
            if type_stack.len() < *depth {
                return true; // Coinductive reasoning
            }
            let lookup_index = type_stack.len() - *depth;
            if let Some(&stack_id) = type_stack.get(lookup_index) {
                check_type_relation(stack_id, pattern_id, lookup, mode, assumptions, type_stack)
            } else {
                true
            }
        }

        (_, Type::Cycle(depth)) => {
            if type_stack.len() < *depth {
                return true;
            }
            let lookup_index = type_stack.len() - *depth;
            if let Some(&stack_id) = type_stack.get(lookup_index) {
                check_type_relation(self_id, stack_id, lookup, mode, assumptions, type_stack)
            } else {
                true
            }
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
            if !check_type_relation(*base1, *base2, lookup, mode, assumptions, type_stack) {
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
                type_stack,
            )
        }
        // Annotated on the left vs a non-union pattern: forget the row (any row `<=`
        // open-empty). Unions/cycles on the right are handled by their own arms so the
        // row can still match an annotated member inside them.
        (Type::Annotated { base, .. }, pattern) if !matches!(pattern, Type::Union(_)) => {
            check_type_relation(*base, pattern_id, lookup, mode, assumptions, type_stack)
        }
        // Plain (open-empty) on the left vs an annotated pattern: sound only when the
        // pattern demands nothing — but such rows are normalised away, so under `All`
        // this fails. Overlap stays row-transparent.
        (self_type, Type::Annotated { base, .. }) if !matches!(self_type, Type::Union(_)) => {
            match mode {
                UnionMode::Any => {
                    check_type_relation(self_id, *base, lookup, mode, assumptions, type_stack)
                }
                UnionMode::All => false,
            }
        }

        // Union on left side: mode determines ALL vs ANY semantics
        (Type::Union(variants), _) => {
            // Insert assumption for recursive types
            assumptions.insert(key);

            match mode {
                UnionMode::All => variants.iter().all(|&variant_id| {
                    check_type_relation(
                        variant_id,
                        pattern_id,
                        lookup,
                        mode,
                        assumptions,
                        type_stack,
                    )
                }),
                UnionMode::Any => variants.iter().any(|&variant_id| {
                    check_type_relation(
                        variant_id,
                        pattern_id,
                        lookup,
                        mode,
                        assumptions,
                        type_stack,
                    )
                }),
            }
        }

        // Union on right side: self must match ANY variant (same for both modes)
        (_, Type::Union(variants)) => {
            // Record the coinductive hypothesis (as the union-on-left arm does) so a back-edge
            // that returns to this same pair — e.g. a recursive type reached through a
            // union-on-right then a cycle — terminates at the assumption check above instead of
            // recursing without bound.
            assumptions.insert(key);

            let already_on_stack = type_stack.contains(&pattern_id);
            if !already_on_stack {
                type_stack.push(pattern_id);
            }
            let result = variants.iter().any(|&variant_id| {
                check_type_relation(self_id, variant_id, lookup, mode, assumptions, type_stack)
            });
            if !already_on_stack {
                type_stack.pop();
            }
            result
        }

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
                                type_stack,
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
                                type_stack,
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
            // Names must match if both have names
            if name1.is_some() && name2.is_some() && name1 != name2 {
                return false;
            }

            // All fields in pattern must exist in self with compatible types
            fields2.iter().all(|(fname2, ftype2)| {
                fields1.iter().any(|(fname1, ftype1)| {
                    fname1 == fname2
                        && check_type_relation(
                            *ftype1,
                            *ftype2,
                            lookup,
                            mode,
                            assumptions,
                            type_stack,
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
            let send_ok = match (send1, send2) {
                (Some(s1), Some(s2)) => {
                    lookup.lookup_type(*s1).is_some_and(|t| t.is_never())
                        || check_type_relation(*s2, *s1, lookup, mode, assumptions, type_stack)
                }
                (None, _) | (_, None) => true,
            };

            // Receive is the AWAIT RESULT (`-> 'r`), covariant like any result.
            let receive_ok = match (receive1, receive2) {
                (Some(r1), Some(r2)) => {
                    check_type_relation(*r1, *r2, lookup, mode, assumptions, type_stack)
                }
                (None, _) | (_, None) => true,
            };

            // State is covariant and strict against a stated expectation: `?p` has no
            // runtime test, so a state-less process value must never satisfy a process
            // type that grants sampling. Dropping the grant (Some → None) is fine.
            let state_ok = match (state1, state2) {
                (Some(s1), Some(s2)) => {
                    check_type_relation(*s1, *s2, lookup, mode, assumptions, type_stack)
                }
                (_, None) => true,
                (None, Some(_)) => false,
            };

            send_ok && receive_ok && state_ok
        }

        // Callable types
        (
            Type::Callable {
                parameter: param1,
                result: result1,
                receive: receive1,
                states: states1,
            },
            Type::Callable {
                parameter: param2,
                result: result2,
                receive: receive2,
                states: states2,
            },
        ) => {
            let already_on_stack = type_stack.contains(&pattern_id);
            if !already_on_stack {
                type_stack.push(pattern_id);
            }

            // States are covariant and strict against a stated expectation, exactly as a
            // process type's state component (a spawn of this function inherits it).
            let states_ok = match (states1, states2) {
                (Some(s1), Some(s2)) => {
                    check_type_relation(*s1, *s2, lookup, mode, assumptions, type_stack)
                }
                (_, None) => true,
                (None, Some(_)) => false,
            };

            // Parameters are contravariant, results are covariant, receive is contravariant
            let result = states_ok
                && check_type_relation(*param2, *param1, lookup, mode, assumptions, type_stack)
                && check_type_relation(*result1, *result2, lookup, mode, assumptions, type_stack)
                && check_type_relation(*receive2, *receive1, lookup, mode, assumptions, type_stack);

            if !already_on_stack {
                type_stack.pop();
            }
            result
        }

        _ => false,
    }
}

#[cfg(test)]
mod annotation_row_tests {
    use super::*;
    use crate::program::Program;

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
}
