use crate::ast;
use quiver_core::{
    program::Program,
    types::{Type, TypeLookup},
};

use super::scopes::Scope;
use super::typing::TypeEnv;
use super::{Error, typing::union_type_ids};

/// Resolve the type ID of an accessor path applied to a given type
/// Used to determine the resulting type after accessing nested fields
pub fn resolve_accessor_type(
    env: &mut TypeEnv,
    scopes: &[Scope],
    program: &mut Program,
    mut current_type_id: usize,
    accessors: &[ast::AccessPath],
    target_name: &str,
) -> Result<usize, Error> {
    for accessor in accessors {
        // Annotation retrieval works on tuples *and* callables and bypasses field lookup.
        if let ast::AccessPath::Annotation(name, expected) = accessor {
            current_type_id = match expected {
                None => super::annotations::retrieval_type(program, current_type_id, name)?.1,
                Some(ast_type) => {
                    let asked =
                        super::typing::resolve_ast_type(env, scopes, ast_type.clone(), program)?;
                    super::annotations::checked_retrieval_type(
                        program,
                        current_type_id,
                        name,
                        asked,
                    )
                    .1
                }
            };
            continue;
        }

        // Get field sources from the current type (both tuples and partials)
        let sources = extract_field_sources(program, current_type_id);

        if sources.is_empty() {
            return Err(Error::MemberAccessOnNonTuple {
                target: target_name.to_string(),
            });
        }

        let field_type_ids: Vec<usize> = match accessor {
            ast::AccessPath::Field(field_name) => {
                let mut results = Vec::new();
                for source in &sources {
                    if let Some((_, ftype)) = get_field_from_source(program, source, field_name) {
                        results.push(ftype);
                    }
                }
                if results.is_empty() {
                    return Err(Error::MemberFieldNotFound {
                        field_name: field_name.clone(),
                        target: target_name.to_string(),
                    });
                }
                results
            }
            ast::AccessPath::Index(index) => {
                let mut results = Vec::new();
                for source in &sources {
                    if matches!(source, FieldSource::Partial { .. }) {
                        return Err(Error::PositionalAccessOnPartial { index: *index });
                    }
                    if let Some(ftype) = get_field_at_position_from_source(program, source, *index)
                    {
                        results.push(ftype);
                    }
                }
                if results.is_empty() {
                    return Err(Error::MemberAccessOnNonTuple {
                        target: target_name.to_string(),
                    });
                }
                results
            }
            ast::AccessPath::Annotation(..) => unreachable!("handled above"),
        };

        current_type_id = union_type_ids(program, field_type_ids);
    }

    Ok(current_type_id)
}

/// Represents field information from either a tuple or partial type
#[derive(Clone)]
enum FieldSource {
    Tuple(usize), // tuple_id
    Partial {
        fields: Vec<(String, usize)>, // (field_name, type_id)
    },
}

/// Extract field sources from a type (tuples and partials)
fn extract_field_sources(program: &Program, type_id: usize) -> Vec<FieldSource> {
    let Some(ty) = program.lookup_type(type_id) else {
        return vec![];
    };
    match ty {
        Type::Annotated { base, .. } => extract_field_sources(program, *base),
        Type::Tuple(id) => vec![FieldSource::Tuple(*id)],
        Type::Partial { fields, .. } => vec![FieldSource::Partial {
            fields: fields.clone(),
        }],
        Type::Union(type_ids) => type_ids
            .iter()
            .flat_map(|&tid| extract_field_sources(program, tid))
            .collect(),
        // A rigid type variable's values have the fields its bound promises.
        Type::Variable(name) => match program.rigid_bound(name) {
            Some(bound) => extract_field_sources(program, bound),
            None => vec![],
        },
        _ => vec![],
    }
}

/// Get field type ID by name from a field source
fn get_field_from_source(
    program: &Program,
    source: &FieldSource,
    field_name: &str,
) -> Option<(usize, usize)> {
    // Returns (index, type_id)
    match source {
        FieldSource::Tuple(tuple_id) => {
            let tuple_info = program.lookup_tuple(*tuple_id)?;
            for (idx, (fname, ftype)) in tuple_info.fields.iter().enumerate() {
                if fname.as_ref() == Some(&field_name.to_string()) {
                    return Some((idx, *ftype));
                }
            }
            None
        }
        FieldSource::Partial { fields } => {
            for (idx, (fname, ftype)) in fields.iter().enumerate() {
                if fname == field_name {
                    return Some((idx, *ftype));
                }
            }
            None
        }
    }
}

/// Get field type ID at position from a field source. A partial has no positions — it
/// constrains fields by name only, so the runtime layout (and any position) is unknown.
fn get_field_at_position_from_source(
    program: &Program,
    source: &FieldSource,
    position: usize,
) -> Option<usize> {
    match source {
        FieldSource::Tuple(tuple_id) => {
            let tuple_info = program.lookup_tuple(*tuple_id)?;
            tuple_info.fields.get(position).map(|(_, ftype)| *ftype)
        }
        FieldSource::Partial { .. } => None,
    }
}

/// How a compiled field access locates its field at runtime.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FieldAccess {
    /// Every field source is a concrete tuple and they agree on the position: a direct
    /// positional `Get`.
    Position(usize),
    /// The position isn't statically determined — a partial source (whose runtime layout
    /// is unknown) or concrete sources that disagree — so a `GetNamed` resolves the
    /// interned name against the value's tuple id at runtime. `static_index` is the
    /// field's index in the static type's field list when the sources agree on one (the
    /// key provenance/narrowing uses); it says nothing about the runtime layout.
    Named {
        name: usize,
        static_index: Option<usize>,
    },
}

impl FieldAccess {
    /// The field's agreed index in the static type's field list, if any.
    pub fn static_index(&self) -> Option<usize> {
        match self {
            FieldAccess::Position(index) => Some(*index),
            FieldAccess::Named { static_index, .. } => *static_index,
        }
    }
}

/// Get field info by name from a type (supports both tuples and partials)
/// Returns (access, field_type_ids) where access says how to locate the field at
/// runtime and field_type_ids are the possible types from all sources
/// For union types, ALL variants must have the field (not just some)
pub fn get_field_by_name(
    program: &mut Program,
    type_id: usize,
    field_name: &str,
    target_name: &str,
) -> Result<(FieldAccess, Vec<usize>), Error> {
    let sources = extract_field_sources(program, type_id);

    if sources.is_empty() {
        return Err(Error::MemberAccessOnNonTuple {
            target: target_name.to_string(),
        });
    }

    let mut results = Vec::new();
    let mut indices = Vec::new();
    let mut any_partial = false;

    for source in &sources {
        // ALL sources must have the field for a union type
        let (idx, ftype) = get_field_from_source(program, source, field_name).ok_or_else(|| {
            Error::MemberFieldNotFound {
                field_name: field_name.to_string(),
                target: target_name.to_string(),
            }
        })?;

        any_partial |= matches!(source, FieldSource::Partial { .. });
        indices.push(idx);
        results.push(ftype);
    }

    let static_index = indices
        .iter()
        .all(|&idx| idx == indices[0])
        .then(|| indices[0]);

    // A partial source's declared order says nothing about the runtime layout, and
    // concrete sources may disagree on the position: both resolve by name at runtime.
    let access = match static_index {
        Some(index) if !any_partial => FieldAccess::Position(index),
        _ => FieldAccess::Named {
            name: program.register_field_name(field_name),
            static_index,
        },
    };

    Ok((access, results))
}

/// Get field info at position from a type (supports both tuples and partials)
/// Returns the possible field types from all sources
/// For union types, ALL variants must have the field at that position
pub fn get_field_at_index(
    program: &Program,
    type_id: usize,
    position: usize,
    target_name: &str,
) -> Result<Vec<usize>, Error> {
    let sources = extract_field_sources(program, type_id);

    if sources.is_empty() {
        return Err(Error::MemberAccessOnNonTuple {
            target: target_name.to_string(),
        });
    }

    let mut results = Vec::new();

    for source in &sources {
        if matches!(source, FieldSource::Partial { .. }) {
            return Err(Error::PositionalAccessOnPartial { index: position });
        }
        // ALL sources must have the field at this position for a union type
        let ftype = get_field_at_position_from_source(program, source, position)
            .ok_or(Error::PositionalIndexOutOfBounds { index: position })?;
        results.push(ftype);
    }

    if results.is_empty() {
        return Err(Error::PositionalIndexOutOfBounds { index: position });
    }

    Ok(results)
}
