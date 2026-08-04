//! Type narrowing helper functions for union type refinement.
//!
//! These functions support narrowing union types based on runtime checks,
//! field access patterns, and computing complement types for branch conditions.
//! All type references use type IDs into the Program's type registry.

use quiver_core::program::Program;
use quiver_core::types::{Type, TypeLookup, is_compatible, types_overlap};

use super::provenance::Provenance;
use super::scopes::{Scope, lookup_variable};
use super::typing::union_type_ids;

/// Narrowing information recorded during condition compilation.
/// Used to compute complement types for subsequent branches.
/// All type references are type IDs.
#[derive(Debug, Clone)]
pub enum Narrowing {
    /// No narrowing recorded yet
    Empty,
    /// Narrowing recorded, complement is possible
    Active {
        provenance: Provenance,
        original_type_id: usize,
        narrowed_type_id: usize,
    },
    /// Complement disabled (non-type failable term encountered or multiple provenances)
    Disabled,
}

impl Narrowing {
    /// Create a new empty narrowing state.
    pub fn new() -> Self {
        Narrowing::Empty
    }

    /// Record a type narrowing event.
    /// - If Empty: transitions to Active
    /// - If Active with same provenance: updates narrowed_type (intersection)
    /// - If Active with different provenance: transitions to Disabled
    /// - If Disabled: no-op
    pub fn record(
        &mut self,
        provenance: &Provenance,
        original_id: usize,
        narrowed_id: usize,
        program: &mut Program,
    ) {
        // Ignore unknown provenances
        if matches!(provenance, Provenance::Unknown) {
            return;
        }

        match self {
            Narrowing::Empty => {
                *self = Narrowing::Active {
                    provenance: provenance.clone(),
                    original_type_id: original_id,
                    narrowed_type_id: narrowed_id,
                };
            }
            Narrowing::Active {
                provenance: existing,
                narrowed_type_id,
                ..
            } if existing == provenance => {
                // Same provenance - update narrowed type (intersection)
                *narrowed_type_id = intersect_types(*narrowed_type_id, narrowed_id, program);
            }
            Narrowing::Active { .. } => {
                // Different provenance - disable complement
                *self = Narrowing::Disabled;
            }
            Narrowing::Disabled => {
                // Already disabled - no-op
            }
        }
    }

    /// Mark that complement narrowing is disabled (non-type failable term encountered).
    pub fn disable(&mut self) {
        *self = Narrowing::Disabled;
    }

    /// Take the narrowing info if active, consuming it.
    /// Returns None if Empty or Disabled.
    pub fn take(&mut self) -> Option<(Provenance, usize, usize)> {
        match std::mem::replace(self, Narrowing::Disabled) {
            Narrowing::Active {
                provenance,
                original_type_id,
                narrowed_type_id,
            } => Some((provenance, original_type_id, narrowed_type_id)),
            _ => None,
        }
    }
}

impl Default for Narrowing {
    fn default() -> Self {
        Self::new()
    }
}

/// Apply a type narrowing based on provenance.
///
/// When a type check succeeds on a value with known provenance, this function
/// records the narrowed type in the current scope so subsequent lookups return
/// the narrowed type.
/// Whether a provenance chain is rooted at `Provenance::Parameter`. Such a chain is
/// scope-relative — created where `Parameter` named the then-current scope's parameter —
/// so it cannot be resolved through `scopes` from a different (inner) scope, which would
/// read that scope's own parameter instead. See `apply_narrowing`'s Parameter arm.
fn rooted_at_parameter(provenance: &Provenance) -> bool {
    match provenance {
        Provenance::Parameter => true,
        Provenance::Field(parent, _) => rooted_at_parameter(parent),
        _ => false,
    }
}

pub fn apply_narrowing(
    scopes: &mut [Scope],
    provenance: &Provenance,
    narrowed_to_id: usize,
    program: &mut Program,
) {
    // Get the never type ID early, before any complex borrowing
    let never_id = program.never();

    match provenance {
        Provenance::Variable(name) => {
            // Get current type (narrowed or original)
            if let Some((current_type_id, _)) = lookup_variable(scopes, name, &[]) {
                let intersected = intersect_types(current_type_id, narrowed_to_id, program);
                if let Some(scope) = scopes.last_mut() {
                    scope.narrowings.variables.insert(name.clone(), intersected);
                }
            }
        }

        Provenance::Field(parent, field_idx) => {
            // Check if parent is a single tuple type (not a union of tuples).
            // If so, store field-specific narrowing for tuple pattern complement narrowing.
            let parent_type_id = get_type_for_provenance(scopes, parent, program);
            let is_single_tuple = program
                .lookup_base(parent_type_id)
                .map(|t| matches!(t, Type::Tuple(_)))
                .unwrap_or(false);

            if is_single_tuple {
                // Get current field type, considering existing narrowings. (Not an `or_else`
                // closure: `get_field_type` now needs `&mut program`, which can't be captured
                // alongside the `scopes` borrow.)
                let current_field_type_id = match get_field_narrowing(scopes, parent, *field_idx) {
                    Some(t) => t,
                    None => get_field_type(parent_type_id, *field_idx, program).unwrap_or(never_id),
                };
                let intersected = intersect_types(current_field_type_id, narrowed_to_id, program);
                set_field_narrowing(scopes, parent, *field_idx, intersected);
                return;
            }

            // Standard case: Filter parent variants based on which have compatible field types
            let filtered =
                filter_variants_by_field(parent_type_id, *field_idx, narrowed_to_id, program);
            // Recursively narrow the parent
            apply_narrowing(scopes, parent, filtered, program);
        }

        Provenance::Parameter => {
            // Get the necessary data from the scope first
            let narrowing_info = scopes.last_mut().and_then(|scope| {
                let param = scope.parameter.as_ref()?;
                let current = scope.narrowings.parameter.unwrap_or(param.ty);
                let intersected = intersect_types(current, narrowed_to_id, program);
                scope.narrowings.parameter = Some(intersected);
                let source_prov = param.provenance.clone();
                Some((source_prov, current, intersected))
            });

            // Now recurse with the borrow released. Only propagate the narrowing up the parameter's
            // source provenance when it actually changed the type: once it reaches a fixpoint there
            // is nothing left to tighten, and continuing would not terminate.
            //
            // A `Parameter`-ROOTED source provenance is never propagated: provenance is
            // scope-relative, and such a chain was minted where `Parameter` meant the
            // *enclosing* scope's parameter. Resolving it here reads the current block's
            // parameter instead (e.g. `$xs ~> { =Nil => … | =Cons[[k, v], t] => … }`:
            // the block parameter's source is `Field(Parameter, 0)`, whose root names
            // the function parameter — resolved against the block scope it denotes the
            // union being matched, and filtering *its* variants by field annihilates
            // them, narrowing the scrutinee to never or corrupting its reconstructed
            // type). Skipping the hop costs only precision on the outer field, never
            // soundness. Variable-rooted chains resolve by name across the scope stack,
            // which is unambiguous, so they still propagate.
            if let Some((source_prov, current, intersected)) = narrowing_info
                && intersected != current
                && !matches!(source_prov, Provenance::Unknown)
                && !rooted_at_parameter(&source_prov)
            {
                apply_narrowing(scopes, &source_prov, intersected, program);
            }
        }

        Provenance::Tuple(_) | Provenance::Unknown => {
            // Cannot narrow unknown or tuple-as-a-whole
        }
    }

    // Also narrow any bindings in the current scope whose provenance matches.
    if !matches!(provenance, Provenance::Unknown)
        && let Some(scope) = scopes.last_mut()
    {
        // Collect bindings to narrow first to avoid borrow conflicts
        let bindings_to_narrow: Vec<(String, usize)> = scope
            .bindings
            .variables
            .iter()
            .filter_map(|(name, variable)| {
                if &variable.provenance == provenance {
                    let current = scope
                        .narrowings
                        .variables
                        .get(name)
                        .copied()
                        .unwrap_or(variable.ty);
                    let intersection = intersect_types(current, narrowed_to_id, program);
                    // Only narrow if not never type
                    if intersection != never_id {
                        return Some((name.clone(), intersection));
                    }
                }
                None
            })
            .collect();

        for (name, narrowed_type_id) in bindings_to_narrow {
            scope.narrowings.variables.insert(name, narrowed_type_id);
        }
    }
}

/// Get the type ID for a provenance by resolving it through the scope chain.
pub fn get_type_for_provenance(
    scopes: &[Scope],
    provenance: &Provenance,
    program: &mut Program,
) -> usize {
    let never_id = program.never();
    match provenance {
        Provenance::Variable(name) => lookup_variable(scopes, name, &[])
            .map(|(ty, _)| ty)
            .unwrap_or(never_id),
        Provenance::Field(parent, idx) => {
            let parent_type_id = get_type_for_provenance(scopes, parent, program);
            get_field_type(parent_type_id, *idx, program).unwrap_or(never_id)
        }
        Provenance::Parameter => scopes
            .last()
            .and_then(|s| {
                s.parameter
                    .as_ref()
                    .map(|p| s.narrowings.parameter.unwrap_or(p.ty))
            })
            .unwrap_or(never_id),
        Provenance::Tuple(_) | Provenance::Unknown => never_id,
    }
}

/// The *declared* (un-narrowed) type for a provenance — i.e. ignoring runtime narrowings, unlike
/// [`get_type_for_provenance`]. Used to resolve a recursive field's `Cycle` to the boundary fixed
/// by the type *definition*: complement narrowing can drop sibling variants from the scrutinee
/// (e.g. `Nil` after a `=Nil` branch), which would leave a kept variant's recursive field a
/// dangling cycle. The field's type is fixed by the definition, so resolve against that.
pub fn get_declared_type_for_provenance(
    scopes: &[Scope],
    provenance: &Provenance,
    program: &mut Program,
) -> Option<usize> {
    match provenance {
        Provenance::Variable(name) => super::scopes::lookup_declared_variable_type(scopes, name),
        Provenance::Field(parent, idx) => {
            let parent_id = get_declared_type_for_provenance(scopes, parent, program)?;
            get_field_type(parent_id, *idx, program)
        }
        Provenance::Parameter => scopes
            .last()
            .and_then(|s| s.parameter.as_ref())
            .map(|p| p.ty),
        Provenance::Tuple(_) | Provenance::Unknown => None,
    }
}

/// Whether a type transitively contains a `Cycle` node (i.e. is recursive) — the public
/// guard for every compile-time decision that trusts `is_compatible`/`types_overlap`:
/// those traverse a `Cycle` optimistically, so a verdict over a cycle-bearing type must
/// not elide a runtime check or subtract from a complement.
pub fn has_cycles(type_id: usize, program: &Program) -> bool {
    contains_cycle(type_id, program, &mut Vec::new())
}

/// Re-root a type that is escaping its boundary context: `Cycle` references pointing
/// *above* the type's own root are replaced with the enclosing boundary types they
/// referred to (innermost last in `enclosing`), so the type stays meaningful on its
/// own. This is what keeps a binding taken from a recursive position matchable later:
/// a list element's `^` (the enclosing definition's root) would otherwise dangle once
/// the binding leaves the match that knew the root, and every later pattern against it
/// would be statically dead. Internal cycles (resolving within the walked type) are
/// kept, as are ones reaching beyond the provided context and anything inside callable
/// or process types (function-boundary cycles are a different numbering).
pub fn close_cycles(type_id: usize, boundary: usize, program: &mut Program) -> usize {
    // A *top-level* `Cycle(1)` is the immediate self-reference and resolves to the
    // scrutinee root whatever its kind — including the `#[&f, …]` self-recursion
    // tuples, whose fields refer to the (non-union) parameter tuple itself.
    if let Some(Type::Cycle(1)) = program.lookup_type(type_id) {
        return boundary;
    }
    // Descending, only a *union* root is a boundary the registered depths count
    // (parameter-position types register cycles one boundary further out, so a
    // non-union root would mis-close references that actually target the enclosing
    // function boundary — those stay dangling, as before).
    if matches!(program.lookup_type(boundary), Some(Type::Union(_))) {
        close_cycles_at(type_id, &[boundary], 0, program)
    } else {
        type_id
    }
}

fn close_cycles_at(
    type_id: usize,
    enclosing: &[usize],
    self_depth: usize,
    program: &mut Program,
) -> usize {
    let Some(typ) = program.lookup_type(type_id).cloned() else {
        return type_id;
    };
    match typ {
        Type::Cycle(n) => {
            if n <= self_depth {
                // Resolves within the walked type: still meaningful, keep.
                type_id
            } else {
                let outer = n - self_depth;
                if outer <= enclosing.len() {
                    enclosing[enclosing.len() - outer]
                } else {
                    // Beyond the known context (e.g. an enclosing function boundary).
                    type_id
                }
            }
        }
        // Unions are the boundaries cycles count.
        Type::Union(members) => {
            let new_members: Vec<usize> = members
                .iter()
                .map(|&m| close_cycles_at(m, enclosing, self_depth + 1, program))
                .collect();
            if new_members == members {
                type_id
            } else {
                // Register structurally (no flatten/dedup): nested unions are the
                // boundaries inner cycles count, so canonicalization would corrupt
                // their depths.
                program.register_type(Type::Union(new_members))
            }
        }
        Type::Tuple(tuple_id) => {
            let Some(info) = program.lookup_tuple(tuple_id).cloned() else {
                return type_id;
            };
            let new_fields: Vec<(Option<String>, usize)> = info
                .fields
                .iter()
                .map(|(label, field)| {
                    (
                        label.clone(),
                        close_cycles_at(*field, enclosing, self_depth, program),
                    )
                })
                .collect();
            if new_fields == info.fields {
                type_id
            } else {
                let new_tuple = program.register_tuple(info.name.clone(), new_fields);
                program.register_type(Type::Tuple(new_tuple))
            }
        }
        Type::Partial { name, fields } => {
            let new_fields: Vec<(String, usize)> = fields
                .iter()
                .map(|(label, field)| {
                    (
                        label.clone(),
                        close_cycles_at(*field, enclosing, self_depth, program),
                    )
                })
                .collect();
            if new_fields == fields {
                type_id
            } else {
                program.register_type(Type::Partial {
                    name,
                    fields: new_fields,
                })
            }
        }
        Type::Annotated {
            base,
            exact,
            entries,
        } => {
            let new_base = close_cycles_at(base, enclosing, self_depth, program);
            if new_base == base {
                type_id
            } else {
                program.register_type(Type::Annotated {
                    base: new_base,
                    exact,
                    entries,
                })
            }
        }
        // Function-boundary cycles inside callables/processes use their own numbering;
        // primitives carry nothing to close.
        _ => type_id,
    }
}

/// Whether a type transitively contains a `Cycle` node (i.e. is recursive). Used to gate the
/// structural narrowing operations, which would otherwise call `is_compatible`/`types_overlap`
/// on a bare `Cycle` — those answer optimistically without the enclosing `type_stack`, which is
/// unsound for subtraction. The `seen` set guards against malformed self-referential registries.
fn contains_cycle(type_id: usize, program: &Program, seen: &mut Vec<usize>) -> bool {
    if seen.contains(&type_id) {
        return false;
    }
    seen.push(type_id);
    let children: Vec<usize> = match program.lookup_type(type_id) {
        Some(Type::Cycle(_)) => return true,
        Some(Type::Union(ids)) => ids.clone(),
        Some(Type::Tuple(tuple_id)) => match program.lookup_tuple(*tuple_id) {
            Some(info) => info.fields.iter().map(|(_, t)| *t).collect(),
            None => return false,
        },
        Some(Type::Partial { fields, .. }) => fields.iter().map(|(_, t)| *t).collect(),
        Some(Type::Annotated { base, entries, .. }) => std::iter::once(*base)
            .chain(entries.iter().map(|(_, t)| *t))
            .collect(),
        _ => return false,
    };
    children
        .iter()
        .any(|&child| contains_cycle(child, program, seen))
}

/// Compute the intersection of two types (by type ID): the type whose values belong to both.
///
/// Distributes over unions and recurses into tuple fields, so a single tuple with union-typed
/// fields is narrowed field-wise (`['n, 'n] ∩ [Rational, 'n]` is `[Rational, 'n]`). Recursive
/// types (and shapes not modelled precisely) keep the left operand when the two could overlap —
/// a wider intersection never excludes a valid value, so it is sound for narrowing.
pub fn intersect_types(a_id: usize, b_id: usize, program: &mut Program) -> usize {
    let a_variants = get_type_variants(a_id, program);
    let b_variants = get_type_variants(b_id, program);
    let never = program.never();

    let mut pieces = Vec::new();
    for &av in &a_variants {
        for &bv in &b_variants {
            let piece = intersect_pair(av, bv, program);
            if piece != never {
                pieces.push(piece);
            }
        }
    }
    union_type_ids(program, pieces)
}

/// Intersect two single (non-union) types. Returns the never type when provably disjoint.
fn intersect_pair(a: usize, b: usize, program: &mut Program) -> usize {
    if a == b {
        return a;
    }

    let never = program.never();
    let (Some(ta), Some(tb)) = (
        program.lookup_type(a).cloned(),
        program.lookup_type(b).cloned(),
    ) else {
        return never;
    };

    match (&ta, &tb) {
        // A type variable is opaque; keep the value's own type rather than discard genericity.
        (Type::Variable(_), _) | (_, Type::Variable(_)) => a,
        // A bare recursive reference can't be compared soundly without its enclosing context;
        // keep `a` (sound, `a ∩ b ⊆ a`). This keeps recursive *fields* whole when recursing.
        (Type::Cycle(_), _) | (_, Type::Cycle(_)) => a,
        (Type::Integer, Type::Integer)
        | (Type::Binary, Type::Binary)
        | (Type::Reference, Type::Reference) => a,
        // Two partials constrain by name, so their intersection constrains by the union of
        // their names: fields in both must intersect, fields in one carry over. Stated tuple
        // names must agree; an unnamed partial adopts the other's.
        (
            Type::Partial {
                name: n1,
                fields: f1,
            },
            Type::Partial {
                name: n2,
                fields: f2,
            },
        ) => {
            if let (Some(a), Some(b)) = (n1, n2)
                && a != b
            {
                return never;
            }
            let mut fields = f1.clone();
            for (name, bty) in f2 {
                match fields.iter_mut().find(|(n, _)| n == name) {
                    Some((_, aty)) => {
                        let intersected = intersect_types(*aty, *bty, program);
                        if intersected == program.never() {
                            return never;
                        }
                        *aty = intersected;
                    }
                    None => fields.push((name.clone(), *bty)),
                }
            }
            program.register_type(Type::Partial {
                name: n1.clone().or_else(|| n2.clone()),
                fields,
            })
        }
        // A partial meeting a concrete tuple keeps the tuple — the more specific of the two —
        // with each constrained field narrowed. A tuple missing a constrained field, or whose
        // name the partial contradicts, satisfies neither.
        (Type::Partial { name, fields }, Type::Tuple(tuple_id))
        | (Type::Tuple(tuple_id), Type::Partial { name, fields }) => {
            let Some(info) = program.lookup_tuple(*tuple_id).cloned() else {
                return never;
            };
            if let Some(name) = name
                && info.name.as_ref() != Some(name)
            {
                return never;
            }
            let mut tuple_fields = info.fields.clone();
            for (name, constraint) in fields {
                let Some((_, field)) = tuple_fields
                    .iter_mut()
                    .find(|(n, _)| n.as_deref() == Some(name.as_str()))
                else {
                    return never;
                };
                let intersected = intersect_types(*field, *constraint, program);
                if intersected == program.never() {
                    return never;
                }
                *field = intersected;
            }
            let narrowed = program.register_tuple(info.name.clone(), tuple_fields);
            program.register_type(Type::Tuple(narrowed))
        }
        (Type::Tuple(id1), Type::Tuple(id2)) => {
            let (Some(i1), Some(i2)) = (
                program.lookup_tuple(*id1).cloned(),
                program.lookup_tuple(*id2).cloned(),
            ) else {
                return never;
            };
            if i1.name != i2.name || i1.fields.len() != i2.fields.len() {
                return never;
            }
            let mut fields = Vec::with_capacity(i1.fields.len());
            for ((name, f1), (_, f2)) in i1.fields.iter().zip(i2.fields.iter()) {
                let fi = intersect_types(*f1, *f2, program);
                if fi == program.never() {
                    return never;
                }
                fields.push((name.clone(), fi));
            }
            let tuple_id = program.register_tuple(i1.name.clone(), fields);
            program.register_type(Type::Tuple(tuple_id))
        }
        _ => {
            if types_overlap(a, b, program) {
                a
            } else {
                never
            }
        }
    }
}

/// Filter parent type to variants where a specific field is compatible with a given type.
pub fn filter_variants_by_field(
    parent_type_id: usize,
    field_idx: usize,
    field_must_be_id: usize,
    program: &mut Program,
) -> usize {
    let variants = get_type_variants(parent_type_id, program);

    // A for-loop rather than `.filter`, since `get_field_type` now borrows `&mut program`.
    let mut filtered = Vec::new();
    for variant_id in variants {
        if let Some(field_type_id) = get_field_type(variant_id, field_idx, program)
            && is_compatible(field_type_id, field_must_be_id, program)
        {
            filtered.push(variant_id);
        }
    }

    union_type_ids(program, filtered)
}

/// Compute the complement type: the values of `original` that are NOT in `narrowed`.
///
/// Structural over tuples — `[A, B] ∖ [a, b]` is `[A∖a, B] | [A, B∖b]` — so a match splitting on
/// a combination of fields can be proven exhaustive (the complement reduces to never) rather than
/// left as the whole original. Distributes over unions on both sides. Recursive types fall back
/// to keeping the original whole (sound: an over-large remainder only makes a block look *less*
/// exhaustive, never more — and never narrows a later branch to exclude a valid value).
pub fn compute_complement(original_id: usize, narrowed_id: usize, program: &mut Program) -> usize {
    let narrowed_variants = get_type_variants(narrowed_id, program);
    let mut pieces = get_type_variants(original_id, program);
    for nv in narrowed_variants {
        let mut next = Vec::new();
        for piece in pieces {
            next.extend(subtract_one(piece, nv, program));
        }
        pieces = next;
    }
    union_type_ids(program, pieces)
}

/// Subtract single type `b` from single type `a`, returning the variants whose union is `a ∖ b`.
fn subtract_one(a: usize, b: usize, program: &mut Program) -> Vec<usize> {
    if a == b {
        return vec![];
    }

    let (Some(ta), Some(tb)) = (
        program.lookup_type(a).cloned(),
        program.lookup_type(b).cloned(),
    ) else {
        return vec![a];
    };

    // A bare recursive reference can't be subtracted soundly without its enclosing context;
    // keep it whole (`a ∖ b ⊆ a`). This is what lets a `Node[^, ^]` survive a subtraction:
    // when a recursive field reaches here, the field's difference is the field unchanged.
    if matches!(ta, Type::Cycle(_)) || matches!(tb, Type::Cycle(_)) {
        return vec![a];
    }

    // `is_compatible`/`types_overlap` are exact only for cycle-free types; on a tuple with
    // recursive fields they traverse the `Cycle` optimistically (matching anything), which would
    // unsoundly empty the difference. For cycle-bearing types, skip these shortcuts and rely on
    // the structural tuple difference below, which keeps recursive fields whole.
    let cyclic = contains_cycle(a, &*program, &mut Vec::new())
        || contains_cycle(b, &*program, &mut Vec::new());
    if !cyclic {
        if is_compatible(a, b, program) {
            return vec![];
        }
        if !types_overlap(a, b, program) {
            return vec![a];
        }
    }

    let never = program.never();
    match (&ta, &tb) {
        (Type::Tuple(id1), Type::Tuple(id2)) => {
            let (Some(i1), Some(i2)) = (
                program.lookup_tuple(*id1).cloned(),
                program.lookup_tuple(*id2).cloned(),
            ) else {
                return vec![a];
            };
            if i1.name != i2.name || i1.fields.len() != i2.fields.len() {
                return vec![a];
            }
            // `[A] ∖ [b]` = union over i of `[A₀, …, Aᵢ∖bᵢ, …, Aₙ]`.
            let mut out = Vec::new();
            for (i, ((_, f1), (_, f2))) in i1.fields.iter().zip(i2.fields.iter()).enumerate() {
                let field_complement = compute_complement(*f1, *f2, program);
                if field_complement == never {
                    continue;
                }
                let mut fields = i1.fields.clone();
                fields[i].1 = field_complement;
                let tuple_id = program.register_tuple(i1.name.clone(), fields);
                out.push(program.register_type(Type::Tuple(tuple_id)));
            }
            out
        }
        _ => vec![a],
    }
}

/// Get the type ID of a field at a given index, unioned across every tuple/partial variant of
/// `type_id` that has that field. Returns None if no variant has the field.
///
/// Takes `&mut Program` so a multi-variant field can be returned as a true union, rather than
/// the first variant as an approximation — important when a narrowed value is a union of tuples
/// (e.g. `Leaf['int] | Node[…]`), where collapsing to one variant is unsound.
pub fn get_field_type(type_id: usize, field_idx: usize, program: &mut Program) -> Option<usize> {
    let variants = get_type_variants_readonly(type_id, program);

    let field_type_ids: Vec<usize> = variants
        .into_iter()
        .filter_map(|variant_id| {
            let ty = program.lookup_base(variant_id)?;
            match ty {
                Type::Tuple(tuple_id) => {
                    let tuple_info = program.lookup_tuple(*tuple_id)?;
                    tuple_info
                        .fields
                        .get(field_idx)
                        .map(|(_, ftype_id)| *ftype_id)
                }
                Type::Partial { fields, .. } => {
                    fields.get(field_idx).map(|(_, ftype_id)| *ftype_id)
                }
                _ => None,
            }
        })
        .collect();

    if field_type_ids.is_empty() {
        None
    } else {
        Some(union_type_ids(program, field_type_ids))
    }
}

/// Whether `pattern` constrains a field whose type is recursive (transitively contains a
/// `Cycle`) with a sub-pattern more specific than a plain binding/placeholder.
///
/// Such a constraint cannot be reflected soundly in the reconstructed narrowed type: recursive
/// fields are kept whole (to avoid materializing an infinite type), so the narrowed type
/// *over-approximates* what actually matched. Complement narrowing on an over-approximation
/// subtracts more than matched — unsound — so it must be disabled for these patterns. (The
/// sound recursive case, a single flat constraining field like `=[Cons[h, t], ys]`, is handled
/// separately by `analyze_tuple_pattern_for_complement` and is unaffected.)
pub fn pattern_constrains_recursive_field(
    pattern: &ast::Match,
    value_type: usize,
    program: &mut Program,
) -> bool {
    let ast::Match::Tuple(tuple) = pattern else {
        return false;
    };
    for (idx, field) in tuple.fields.iter().enumerate() {
        let constraining = !matches!(
            field.pattern,
            ast::Match::Identifier(_, _) | ast::Match::Placeholder
        );
        if !constraining {
            continue;
        }
        let Some(field_type) = get_field_type(value_type, idx, program) else {
            continue;
        };
        if contains_cycle(field_type, &*program, &mut Vec::new())
            || pattern_constrains_recursive_field(&field.pattern, field_type, program)
        {
            return true;
        }
    }
    false
}

/// Get variant type IDs from a type (readonly version for use with &Program)
fn get_type_variants_readonly(type_id: usize, program: &Program) -> Vec<usize> {
    let Some(ty) = program.lookup_type(type_id) else {
        return vec![];
    };
    match ty {
        Type::Union(ids) => ids.clone(),
        _ => vec![type_id],
    }
}

/// Get variant type IDs from a type
fn get_type_variants(type_id: usize, program: &Program) -> Vec<usize> {
    get_type_variants_readonly(type_id, program)
}

// =============================================================================
// Tuple Pattern Complement Narrowing
// =============================================================================

use crate::ast;

/// Result of analyzing a field pattern for complement narrowing.
#[derive(Debug)]
enum FieldPatternKind {
    /// Pattern always succeeds (identifier, placeholder)
    AlwaysBinds,
    /// Pattern constrains to a specific tuple type
    TypeConstraining(usize), // tuple_id
    /// Pattern is too complex for complement
    Complex,
}

/// Classify a field pattern for complement narrowing analysis.
fn classify_field_pattern(
    pattern: &ast::Match,
    field_type_id: usize,
    program: &Program,
) -> FieldPatternKind {
    match pattern {
        ast::Match::Identifier(_, _) | ast::Match::Placeholder => FieldPatternKind::AlwaysBinds,

        ast::Match::Tuple(tuple) => {
            let is_flat = tuple.fields.iter().all(|f| {
                matches!(
                    f.pattern,
                    ast::Match::Identifier(_, _) | ast::Match::Placeholder
                )
            });

            if !is_flat {
                return FieldPatternKind::Complex;
            }

            if let Some(tuple_id) = find_matching_tuple_type(tuple, field_type_id, program) {
                FieldPatternKind::TypeConstraining(tuple_id)
            } else {
                FieldPatternKind::Complex
            }
        }

        ast::Match::Reference(..) => {
            // Reference patterns are type-checking patterns
            FieldPatternKind::Complex
        }

        _ => FieldPatternKind::Complex,
    }
}

/// If `pattern` names a concrete tuple type present in `field_type_id` (matching name and
/// arity), return that tuple type's id registered as a `Type::Tuple`. Used to compute the
/// positive narrowing a field sub-pattern imposes on a dispatch branch's guard type.
pub fn matching_tuple_type(
    pattern: &ast::MatchTuple,
    field_type_id: usize,
    program: &mut Program,
) -> Option<usize> {
    let tuple_id = find_matching_tuple_type(pattern, field_type_id, program)?;
    Some(program.register_type(Type::Tuple(tuple_id)))
}

/// Find a matching tuple type for a pattern in the given type.
fn find_matching_tuple_type(
    pattern: &ast::MatchTuple,
    field_type_id: usize,
    program: &Program,
) -> Option<usize> {
    let tuples = extract_tuple_ids(field_type_id, program);

    for tuple_id in tuples {
        let tuple_info = program.lookup_tuple(tuple_id)?;
        if tuple_info.name.as_ref() == pattern.name.as_ref()
            && tuple_info.fields.len() == pattern.fields.len()
        {
            return Some(tuple_id);
        }
    }
    None
}

/// Extract tuple IDs from a type (only concrete tuples, not partials)
fn extract_tuple_ids(type_id: usize, program: &Program) -> Vec<usize> {
    let Some(ty) = program.lookup_type(type_id) else {
        return vec![];
    };
    match ty {
        Type::Annotated { base, .. } => extract_tuple_ids(*base, program),
        Type::Tuple(id) => vec![*id],
        Type::Union(type_ids) => type_ids
            .iter()
            .filter_map(|&tid| {
                program.lookup_type(tid).and_then(|t| match t {
                    Type::Tuple(id) => Some(*id),
                    Type::Annotated { base, .. } => match program.lookup_type(*base) {
                        Some(Type::Tuple(id)) => Some(*id),
                        _ => None,
                    },
                    _ => None,
                })
            })
            .collect(),
        _ => vec![],
    }
}

/// Analyze a tuple bind pattern to find a single type-constraining field.
pub fn analyze_tuple_pattern_for_complement(
    pattern: &ast::Match,
    value_type_id: usize,
    program: &mut Program,
) -> Option<(usize, usize)> {
    let tuple_pattern = match pattern {
        ast::Match::Tuple(t) => t,
        _ => return None,
    };

    // Field-specific complement narrowing asserts "the failed branch rules these field values
    // out" — sound only when the tuple check itself (name/arity/field names) cannot be the
    // reason the branch failed. That requires the scrutinee to *be* that single tuple shape
    // statically; on a union, a failure may just mean "a different variant", and narrowing a
    // field from it would wrongly prune sibling variants' branches.
    let tuple_id = match program.lookup_base(value_type_id)? {
        Type::Tuple(id) => *id,
        _ => return None,
    };
    let tuple_info = program.lookup_tuple(tuple_id)?;
    if tuple_pattern.name.as_ref() != tuple_info.name.as_ref()
        || tuple_pattern.fields.len() != tuple_info.fields.len()
        || tuple_pattern
            .fields
            .iter()
            .zip(tuple_info.fields.iter())
            .any(|(pf, (fname, _))| pf.name.as_ref() != fname.as_ref())
    {
        return None;
    }
    let field_type_ids: Vec<usize> = tuple_info.fields.iter().map(|(_, t)| *t).collect();

    let mut constraining: Option<(usize, usize)> = None;

    for (idx, field) in tuple_pattern.fields.iter().enumerate() {
        let field_type_id = *field_type_ids.get(idx)?;
        match classify_field_pattern(&field.pattern, field_type_id, program) {
            FieldPatternKind::AlwaysBinds => continue,
            FieldPatternKind::TypeConstraining(tuple_id) => {
                if constraining.is_some() {
                    return None;
                }
                let type_id = program.register_type(Type::Tuple(tuple_id));
                constraining = Some((idx, type_id));
            }
            FieldPatternKind::Complex => return None,
        }
    }

    constraining
}

/// Get the narrowed type ID for a field of a provenance, if any.
pub fn get_field_narrowing(
    scopes: &[Scope],
    provenance: &Provenance,
    field_idx: usize,
) -> Option<usize> {
    scopes.last().and_then(|scope| {
        scope
            .narrowings
            .fields
            .iter()
            .find(|(prov, idx, _)| prov == provenance && *idx == field_idx)
            .map(|(_, _, ty)| *ty)
    })
}

/// The **declared** scrutinee's same-shaped tuple member, as the id a union
/// discriminator's runtime test should use in place of a complement-narrowed one.
///
/// A branch's complement refines a wrapped union member's *field types* (`Ev['w]`
/// minus `Ev[A]` is `Ev[B | C]`), but a runtime value's tuple id still carries the
/// declared field type (`Ev['w]`), and the id-level `IsType` test computed from the
/// narrowed member would wrongly reject it — a later sibling pattern then misses
/// values it must match (the field sub-checks, which do the real member
/// discrimination, never run). The discriminator's job is only to separate this
/// member's *shape* from the union's other members, so it tests against the declared
/// type's member with the same name and field labels. When several declared members
/// share a shape the deep test is what tells them apart, so only a unique shape
/// answers; `None` keeps the caller's (narrowed) id and today's behavior.
pub fn declared_shape_witness(
    scopes: &[Scope],
    provenance: &Provenance,
    tuple_id: usize,
    program: &mut Program,
) -> Option<usize> {
    let declared = get_declared_type_for_provenance(scopes, provenance, program)?;
    let target = program.lookup_tuple(tuple_id)?.clone();
    let mut witness = None;
    for member_tuple_id in tuple_members_of(declared, program) {
        let Some(info) = program.lookup_tuple(member_tuple_id) else {
            continue;
        };
        let same_shape = info.name == target.name
            && info.fields.len() == target.fields.len()
            && info
                .fields
                .iter()
                .zip(&target.fields)
                .all(|((label, _), (target_label, _))| label == target_label);
        if same_shape {
            if witness.is_some() {
                return None;
            }
            witness = Some(member_tuple_id);
        }
    }
    witness
}

/// The tuple ids a type's values can carry at its top level: union members and
/// annotation rows are seen through (one flat level — unions intern flattened).
fn tuple_members_of(type_id: usize, program: &Program) -> Vec<usize> {
    fn base_tuple(type_id: usize, program: &Program) -> Option<usize> {
        match program.lookup_type(type_id)? {
            Type::Tuple(tuple_id) => Some(*tuple_id),
            Type::Annotated { base, .. } => base_tuple(*base, program),
            _ => None,
        }
    }
    match program.lookup_type(type_id) {
        Some(Type::Union(members)) => members
            .clone()
            .into_iter()
            .filter_map(|m| base_tuple(m, program))
            .collect(),
        Some(Type::Annotated { base, .. }) => tuple_members_of(*base, program),
        _ => base_tuple(type_id, program).into_iter().collect(),
    }
}

/// Record a field narrowing for a provenance.
pub fn set_field_narrowing(
    scopes: &mut [Scope],
    provenance: &Provenance,
    field_idx: usize,
    narrowed_type_id: usize,
) {
    if let Some(scope) = scopes.last_mut() {
        if let Some(entry) = scope
            .narrowings
            .fields
            .iter_mut()
            .find(|(prov, idx, _)| prov == provenance && *idx == field_idx)
        {
            entry.2 = narrowed_type_id;
        } else {
            scope
                .narrowings
                .fields
                .push((provenance.clone(), field_idx, narrowed_type_id));
        }
    }
}
