//! Type narrowing helper functions for union type refinement.
//!
//! These functions support narrowing union types based on runtime checks,
//! field access patterns, and computing complement types for branch conditions.
//! All type references use type IDs into the Program's type registry.

use quiver_core::program::Program;
use quiver_core::types::{Type, TypeLookup, is_compatible};

use super::provenance::Provenance;
use super::scopes::{Binding, Scope, lookup_variable};

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
                .lookup_type(parent_type_id)
                .map(|t| matches!(t, Type::Tuple(_)))
                .unwrap_or(false);

            if is_single_tuple {
                // Get current field type, considering existing narrowings
                let current_field_type_id = get_field_narrowing(scopes, parent, *field_idx)
                    .or_else(|| get_field_type(parent_type_id, *field_idx, program))
                    .unwrap_or(never_id);
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
                Some((source_prov, intersected))
            });

            // Now recurse with the borrow released
            if let Some((source_prov, intersected)) = narrowing_info
                && !matches!(source_prov, Provenance::Parameter | Provenance::Unknown)
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
            .iter()
            .filter_map(|(name, binding)| {
                if let Binding::Variable {
                    ty,
                    provenance: prov,
                    ..
                } = binding
                    && prov == provenance
                {
                    let current = scope.narrowings.variables.get(name).copied().unwrap_or(*ty);
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

/// Compute the intersection of two types (by type ID).
///
/// Filters variants from `a` that are compatible with `b`.
pub fn intersect_types(a_id: usize, b_id: usize, program: &mut Program) -> usize {
    let a_variants = get_type_variants(a_id, program);

    let intersected: Vec<usize> = a_variants
        .into_iter()
        .filter(|&variant_id| is_compatible(variant_id, b_id, program))
        .collect();

    union_type_ids(program, intersected)
}

/// Filter parent type to variants where a specific field is compatible with a given type.
pub fn filter_variants_by_field(
    parent_type_id: usize,
    field_idx: usize,
    field_must_be_id: usize,
    program: &mut Program,
) -> usize {
    let variants = get_type_variants(parent_type_id, program);

    let filtered: Vec<usize> = variants
        .into_iter()
        .filter(|&variant_id| {
            if let Some(field_type_id) = get_field_type(variant_id, field_idx, program) {
                is_compatible(field_type_id, field_must_be_id, program)
            } else {
                false
            }
        })
        .collect();

    union_type_ids(program, filtered)
}

/// Compute the complement type (variants NOT compatible with the narrowed type).
pub fn compute_complement(original_id: usize, narrowed_id: usize, program: &mut Program) -> usize {
    let variants = get_type_variants(original_id, program);

    let complement: Vec<usize> = variants
        .into_iter()
        .filter(|&variant_id| !is_compatible(variant_id, narrowed_id, program))
        .collect();

    union_type_ids(program, complement)
}

/// Get the type ID of a field at a given index from a type.
pub fn get_field_type(type_id: usize, field_idx: usize, program: &Program) -> Option<usize> {
    let variants = get_type_variants_readonly(type_id, program);

    let field_type_ids: Vec<usize> = variants
        .into_iter()
        .filter_map(|variant_id| {
            let ty = program.lookup_type(variant_id)?;
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
    } else if field_type_ids.len() == 1 {
        Some(field_type_ids[0])
    } else {
        // Multiple field types - would need to create union, but we don't have &mut Program
        // Return the first one as approximation (this is a read-only context)
        Some(field_type_ids[0])
    }
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

/// Create a union type from a list of type IDs, simplifying single-element lists
fn union_type_ids(program: &mut Program, type_ids: Vec<usize>) -> usize {
    // Deduplicate type IDs
    let mut seen = std::collections::HashSet::new();
    let unique: Vec<usize> = type_ids.into_iter().filter(|id| seen.insert(*id)).collect();

    match unique.len() {
        0 => program.never(),
        1 => unique[0],
        _ => program.register_type(Type::Union(unique)),
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
fn classify_field_pattern(
    pattern: &ast::Match,
    field_type_id: usize,
    program: &Program,
) -> FieldPatternKind {
    match pattern {
        ast::Match::Identifier(_) | ast::Match::Placeholder => FieldPatternKind::AlwaysBinds,

        ast::Match::Tuple(tuple) => {
            let is_flat = tuple.fields.iter().all(|f| {
                matches!(
                    f.pattern,
                    ast::Match::Identifier(_) | ast::Match::Placeholder
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

        ast::Match::Bind(inner) | ast::Match::Pin(inner) => {
            classify_field_pattern(inner, field_type_id, program)
        }

        _ => FieldPatternKind::Complex,
    }
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
        Type::Tuple(id) => vec![*id],
        Type::Union(type_ids) => type_ids
            .iter()
            .filter_map(|&tid| {
                program.lookup_type(tid).and_then(|t| match t {
                    Type::Tuple(id) => Some(*id),
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
        ast::Match::Bind(inner) => match inner.as_ref() {
            ast::Match::Tuple(t) => t,
            _ => return None,
        },
        _ => return None,
    };

    let field_type_ids = get_tuple_field_types(value_type_id, program)?;

    if tuple_pattern.fields.len() != field_type_ids.len() {
        return None;
    }

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

/// Get field type IDs from a tuple value type.
fn get_tuple_field_types(value_type_id: usize, program: &Program) -> Option<Vec<usize>> {
    let tuples = extract_tuple_ids(value_type_id, program);
    let first_tuple = tuples.first()?;
    let tuple_info = program.lookup_tuple(*first_tuple)?;
    Some(
        tuple_info
            .fields
            .iter()
            .map(|(_, type_id)| *type_id)
            .collect(),
    )
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

/// Narrow nil from bindings created after a checkpoint.
pub fn narrow_nil_from_new_bindings(
    scopes: &mut [Scope],
    bindings_before: &std::collections::HashSet<String>,
    program: &mut Program,
) {
    if let Some(scope) = scopes.last_mut() {
        let new_bindings_to_narrow: Vec<(String, usize)> = scope
            .bindings
            .iter()
            .filter_map(|(name, binding)| {
                if bindings_before.contains(name) {
                    return None;
                }
                if let Binding::Variable { ty, .. } = binding {
                    let ty_ref = program.lookup_type(*ty)?;
                    if ty_ref.contains_nil(program) {
                        let without_nil = ty_ref.without_nil(program);
                        let new_id = program.register_type(without_nil);
                        return Some((name.clone(), new_id));
                    }
                }
                None
            })
            .collect();
        for (name, narrowed) in new_bindings_to_narrow {
            scope.narrowings.variables.insert(name, narrowed);
        }
    }
}
