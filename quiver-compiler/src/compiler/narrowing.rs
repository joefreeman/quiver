//! Type narrowing helper functions for union type refinement.
//!
//! These functions support narrowing union types based on runtime checks,
//! field access patterns, and computing complement types for branch conditions.

use quiver_core::{
    program::Program,
    types::{TupleLookup, Type},
};

use super::provenance::Provenance;
use super::scopes::{Binding, Scope, lookup_variable};

/// Narrowing information recorded during condition compilation.
/// Used to compute complement types for subsequent branches.
#[derive(Debug, Clone)]
pub enum Narrowing {
    /// No narrowing recorded yet
    Empty,
    /// Narrowing recorded, complement is possible
    Active {
        provenance: Provenance,
        original_type: Type,
        narrowed_type: Type,
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
        original: &Type,
        narrowed: &Type,
        program: &Program,
    ) {
        // Ignore unknown provenances
        if matches!(provenance, Provenance::Unknown) {
            return;
        }

        match self {
            Narrowing::Empty => {
                *self = Narrowing::Active {
                    provenance: provenance.clone(),
                    original_type: original.clone(),
                    narrowed_type: narrowed.clone(),
                };
            }
            Narrowing::Active {
                provenance: existing,
                narrowed_type,
                ..
            } if existing == provenance => {
                // Same provenance - update narrowed type (intersection)
                *narrowed_type = intersect_types(narrowed_type, narrowed, program);
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
    pub fn take(&mut self) -> Option<(Provenance, Type, Type)> {
        match std::mem::replace(self, Narrowing::Disabled) {
            Narrowing::Active {
                provenance,
                original_type,
                narrowed_type,
            } => Some((provenance, original_type, narrowed_type)),
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
///
/// NOTE: This function assumes provenance chains do not cycle. A cycle would cause
/// infinite recursion. This is guaranteed by construction: provenances are built
/// from the AST structure (variables, field access, parameters) which cannot form
/// cycles.
pub fn apply_narrowing(
    scopes: &mut [Scope],
    provenance: &Provenance,
    narrowed_to: &Type,
    program: &Program,
) {
    match provenance {
        Provenance::Variable(name) => {
            // Get current type (narrowed or original)
            if let Some((current_type, _)) = lookup_variable(scopes, name, &[]) {
                let intersected = intersect_types(&current_type, narrowed_to, program);
                if let Some(scope) = scopes.last_mut() {
                    scope.narrowings.variables.insert(name.clone(), intersected);
                }
            }
        }

        Provenance::Field(parent, field_idx) => {
            // Check if parent is a single tuple type (not a union of tuples).
            // If so, store field-specific narrowing for tuple pattern complement narrowing.
            let parent_type = get_type_for_provenance(scopes, parent, program);
            let is_single_tuple = matches!(parent_type, Type::Tuple(_));

            if is_single_tuple {
                // Get current field type, considering existing narrowings
                let current_field_type = get_field_narrowing(scopes, parent, *field_idx)
                    .or_else(|| get_field_type(&parent_type, *field_idx, program))
                    .unwrap_or_else(Type::never);
                let intersected = intersect_types(&current_field_type, narrowed_to, program);
                set_field_narrowing(scopes, parent, *field_idx, intersected);
                return;
            }

            // Standard case: Filter parent variants based on which have compatible field types
            let filtered = filter_variants_by_field(&parent_type, *field_idx, narrowed_to, program);
            // Recursively narrow the parent
            apply_narrowing(scopes, parent, &filtered, program);
        }

        Provenance::Parameter => {
            // Get the necessary data from the scope first
            let narrowing_info = scopes.last_mut().and_then(|scope| {
                let param = scope.parameter.as_ref()?;
                let current = scope
                    .narrowings
                    .parameter
                    .clone()
                    .unwrap_or_else(|| param.ty.clone());
                let intersected = intersect_types(&current, narrowed_to, program);
                scope.narrowings.parameter = Some(intersected.clone());
                let source_prov = param.provenance.clone();
                Some((source_prov, intersected))
            });

            // Now recurse with the borrow released
            if let Some((source_prov, intersected)) = narrowing_info
                && !matches!(source_prov, Provenance::Parameter | Provenance::Unknown)
            {
                apply_narrowing(scopes, &source_prov, &intersected, program);
            }
        }

        Provenance::Tuple(_) | Provenance::Unknown => {
            // Cannot narrow unknown or tuple-as-a-whole
        }
    }

    // Also narrow any bindings in the current scope whose provenance matches.
    // This handles cases like `a ~> =x` where narrowing `a` should also narrow `x`,
    // since `x` was bound to `a` and shares its provenance.
    //
    // We only narrow if the binding's type is compatible with narrowed_to, which ensures
    // we don't incorrectly narrow field bindings that happen to share a parent provenance
    // (e.g., `ys` in `=[Nil, ys]` has provenance Parameter but type list<t>, not tuple type).
    if !matches!(provenance, Provenance::Unknown)
        && let Some(scope) = scopes.last_mut()
    {
        // Collect bindings to narrow first to avoid borrow conflicts
        let bindings_to_narrow: Vec<(String, Type)> = scope
            .bindings
            .iter()
            .filter_map(|(name, binding)| {
                // Only process Variable bindings with matching provenance
                if let Binding::Variable {
                    ty,
                    provenance: prov,
                    ..
                } = binding
                    && prov == provenance
                {
                    let current = scope
                        .narrowings
                        .variables
                        .get(name)
                        .cloned()
                        .unwrap_or_else(|| ty.clone());
                    // Only narrow if the types are compatible - this prevents
                    // narrowing field bindings with incompatible types
                    let intersection = intersect_types(&current, narrowed_to, program);
                    if !intersection.is_never() {
                        return Some((name.clone(), intersection));
                    }
                }
                None
            })
            .collect();

        for (name, narrowed_type) in bindings_to_narrow {
            scope.narrowings.variables.insert(name, narrowed_type);
        }
    }
}

/// Get the type for a provenance by resolving it through the scope chain.
///
/// Returns `Type::never()` on failure (missing variable, out-of-bounds field, etc.)
/// rather than returning an error. This is intentional: this function is used during
/// narrowing propagation where graceful degradation is preferred. If the provenance
/// can't be resolved, narrowing simply doesn't apply rather than failing compilation.
/// This differs from `scopes::get_parameter` which returns errors because it's called
/// in contexts where the parameter *must* exist (e.g., `~>` operator usage).
pub fn get_type_for_provenance(
    scopes: &[Scope],
    provenance: &Provenance,
    program: &Program,
) -> Type {
    match provenance {
        Provenance::Variable(name) => lookup_variable(scopes, name, &[])
            .map(|(ty, _)| ty)
            .unwrap_or_else(Type::never),
        Provenance::Field(parent, idx) => {
            let parent_type = get_type_for_provenance(scopes, parent, program);
            get_field_type(&parent_type, *idx, program).unwrap_or_else(Type::never)
        }
        Provenance::Parameter => scopes
            .last()
            .and_then(|s| {
                s.parameter.as_ref().map(|p| {
                    s.narrowings
                        .parameter
                        .clone()
                        .unwrap_or_else(|| p.ty.clone())
                })
            })
            .unwrap_or_else(Type::never),
        Provenance::Tuple(_) | Provenance::Unknown => Type::never(),
    }
}

/// Compute the intersection of two types.
///
/// Filters variants from `a` that are compatible with `b`.
/// This is used when multiple type checks are applied to the same value,
/// e.g., `x ~> ^t ~> ^u` narrows x to the intersection of t and u.
pub fn intersect_types(a: &Type, b: &Type, program: &Program) -> Type {
    let intersected: Vec<Type> = a
        .variants()
        .iter()
        .filter(|variant| variant.is_compatible(b, program))
        .cloned()
        .collect();

    Type::from_types(intersected)
}

/// Filter parent type to variants where a specific field is compatible with a given type.
///
/// This is used for field narrowing propagation: when we narrow `x.a` to type `t`,
/// we filter `x` to only those variants where field `a` is compatible with `t`.
pub fn filter_variants_by_field(
    parent_type: &Type,
    field_idx: usize,
    field_must_be: &Type,
    program: &Program,
) -> Type {
    let filtered: Vec<Type> = parent_type
        .variants()
        .iter()
        .filter(|variant| {
            if let Some(field_type) = get_field_type(variant, field_idx, program) {
                field_type.is_compatible(field_must_be, program)
            } else {
                false
            }
        })
        .cloned()
        .collect();

    Type::from_types(filtered)
}

/// Compute the complement type (variants NOT compatible with the narrowed type).
///
/// This is used for branch narrowing: when a branch condition narrows to type `t`,
/// subsequent branches see the complement (original minus `t`).
pub fn compute_complement(original: &Type, narrowed: &Type, program: &Program) -> Type {
    let complement: Vec<Type> = original
        .variants()
        .iter()
        .filter(|variant| !variant.is_compatible(narrowed, program))
        .cloned()
        .collect();

    Type::from_types(complement)
}

/// Get the type of a field at a given index from a type.
///
/// For tuple types, returns the field type at that index.
/// For union types, collects field types from all variants and returns their union.
/// Returns `None` if no variants have the field.
pub fn get_field_type(ty: &Type, field_idx: usize, program: &Program) -> Option<Type> {
    let field_types: Vec<Type> = ty
        .variants()
        .iter()
        .filter_map(|variant| match variant {
            Type::Tuple(tuple_id) | Type::Partial(tuple_id) => {
                let tuple_info = program.lookup_tuple(*tuple_id)?;
                tuple_info
                    .fields
                    .get(field_idx)
                    .map(|(_, field_type)| field_type.clone())
            }
            _ => None,
        })
        .collect();

    if field_types.is_empty() {
        None
    } else {
        Some(Type::from_types(field_types))
    }
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
///
/// A field pattern is:
/// - `AlwaysBinds` if it's a simple identifier or placeholder (always matches)
/// - `TypeConstraining` if it's a flat tuple pattern (e.g., `Nil`, `Cons[x, y]`)
/// - `Complex` if it has nested patterns or other complex structure
fn classify_field_pattern(
    pattern: &ast::Match,
    field_type: &Type,
    program: &Program,
) -> FieldPatternKind {
    match pattern {
        // Simple bindings always succeed
        ast::Match::Identifier(_) | ast::Match::Placeholder => FieldPatternKind::AlwaysBinds,

        // Tuple patterns may constrain to a specific type
        ast::Match::Tuple(tuple) => {
            // Check if all field patterns are simple bindings (flat tuple)
            let is_flat = tuple.fields.iter().all(|f| {
                matches!(
                    f.pattern,
                    ast::Match::Identifier(_) | ast::Match::Placeholder
                )
            });

            if !is_flat {
                return FieldPatternKind::Complex;
            }

            // Find matching tuple type in field_type
            if let Some(tuple_id) = find_matching_tuple_type(tuple, field_type, program) {
                FieldPatternKind::TypeConstraining(tuple_id)
            } else {
                FieldPatternKind::Complex
            }
        }

        // Bind/Pin wrappers - check inner pattern
        ast::Match::Bind(inner) | ast::Match::Pin(inner) => {
            classify_field_pattern(inner, field_type, program)
        }

        // Everything else is complex
        _ => FieldPatternKind::Complex,
    }
}

/// Find a matching tuple type for a pattern in the given type.
///
/// Returns the tuple_id if the pattern matches exactly one tuple variant.
fn find_matching_tuple_type(
    pattern: &ast::MatchTuple,
    field_type: &Type,
    program: &Program,
) -> Option<usize> {
    let tuples = field_type.extract_tuples();

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

/// Analyze a tuple bind pattern to find a single type-constraining field.
///
/// For complement narrowing to work on tuple patterns like `=[Nil, ys]`:
/// - We need exactly ONE field that constrains the type (e.g., `Nil`)
/// - All other fields must be simple bindings (e.g., `ys`)
///
/// Returns `Some((field_index, constrained_type))` if suitable for complement narrowing.
pub fn analyze_tuple_pattern_for_complement(
    pattern: &ast::Match,
    value_type: &Type,
    program: &Program,
) -> Option<(usize, Type)> {
    // Extract the tuple pattern (may be wrapped in Bind)
    let tuple_pattern = match pattern {
        ast::Match::Tuple(t) => t,
        ast::Match::Bind(inner) => match inner.as_ref() {
            ast::Match::Tuple(t) => t,
            _ => return None,
        },
        _ => return None,
    };

    // Get field types from value_type
    let field_types = get_tuple_field_types(value_type, program)?;

    // Ensure pattern has same number of fields
    if tuple_pattern.fields.len() != field_types.len() {
        return None;
    }

    let mut constraining: Option<(usize, Type)> = None;

    for (idx, field) in tuple_pattern.fields.iter().enumerate() {
        let field_type = field_types.get(idx)?;
        match classify_field_pattern(&field.pattern, field_type, program) {
            FieldPatternKind::AlwaysBinds => continue,
            FieldPatternKind::TypeConstraining(tuple_id) => {
                if constraining.is_some() {
                    // Multiple constraining fields - too complex for complement
                    return None;
                }
                constraining = Some((idx, Type::Tuple(tuple_id)));
            }
            FieldPatternKind::Complex => return None,
        }
    }

    constraining
}

/// Get field types from a tuple value type.
///
/// For union types with multiple tuple variants, all variants must have the same
/// number of fields. Returns field types from the first tuple variant.
fn get_tuple_field_types(value_type: &Type, program: &Program) -> Option<Vec<Type>> {
    let tuples = value_type.extract_tuples();
    let first_tuple = tuples.first()?;
    let tuple_info = program.lookup_tuple(*first_tuple)?;
    Some(tuple_info.fields.iter().map(|(_, ty)| ty.clone()).collect())
}

/// Get the narrowed type for a field of a provenance, if any.
///
/// Returns the narrowed field type if one has been recorded in the current scope,
/// otherwise returns None.
pub fn get_field_narrowing(
    scopes: &[Scope],
    provenance: &Provenance,
    field_idx: usize,
) -> Option<Type> {
    scopes.last().and_then(|scope| {
        scope
            .narrowings
            .fields
            .iter()
            .find(|(prov, idx, _)| prov == provenance && *idx == field_idx)
            .map(|(_, _, ty)| ty.clone())
    })
}

/// Record a field narrowing for a provenance.
///
/// Updates the field narrowing if it already exists, otherwise adds a new entry.
pub fn set_field_narrowing(
    scopes: &mut [Scope],
    provenance: &Provenance,
    field_idx: usize,
    narrowed_type: Type,
) {
    if let Some(scope) = scopes.last_mut() {
        if let Some(entry) = scope
            .narrowings
            .fields
            .iter_mut()
            .find(|(prov, idx, _)| prov == provenance && *idx == field_idx)
        {
            entry.2 = narrowed_type;
        } else {
            scope
                .narrowings
                .fields
                .push((provenance.clone(), field_idx, narrowed_type));
        }
    }
}

/// Narrow nil from bindings created after a checkpoint.
///
/// When a chain/expression succeeds (doesn't short-circuit on nil), any bindings
/// created during that chain that contain nil in their type can have nil removed,
/// since we know execution continued past the potential failure point.
///
/// # Arguments
/// * `scopes` - The scope stack (only the last scope is examined)
/// * `bindings_before` - Set of binding names that existed before the chain
///
/// # Behavior
/// For each binding created after the checkpoint (not in `bindings_before`):
/// - If the binding is a variable with a nil-containing type
/// - Add a narrowing that removes nil from its type
pub fn narrow_nil_from_new_bindings(
    scopes: &mut [Scope],
    bindings_before: &std::collections::HashSet<String>,
) {
    if let Some(scope) = scopes.last_mut() {
        let new_bindings_to_narrow: Vec<(String, Type)> = scope
            .bindings
            .iter()
            .filter_map(|(name, binding)| {
                if bindings_before.contains(name) {
                    return None;
                }
                if let Binding::Variable { ty, .. } = binding
                    && ty.contains_nil()
                {
                    return Some((name.clone(), ty.without_nil()));
                }
                None
            })
            .collect();
        for (name, narrowed) in new_bindings_to_narrow {
            scope.narrowings.variables.insert(name, narrowed);
        }
    }
}
