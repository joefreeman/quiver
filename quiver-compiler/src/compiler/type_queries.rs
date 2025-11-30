use crate::ast;
use quiver_core::{
    program::Program,
    types::{Type, TypeLookup},
};

use super::{Error, typing::union_type_ids};

/// Resolve the type ID of an accessor path applied to a given type
/// Used to determine the resulting type after accessing nested fields
pub fn resolve_accessor_type(
    program: &mut Program,
    mut current_type_id: usize,
    accessors: &[ast::AccessPath],
    target_name: &str,
) -> Result<usize, Error> {
    for accessor in accessors {
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
        Type::Tuple(id) => vec![FieldSource::Tuple(*id)],
        Type::Partial { fields, .. } => vec![FieldSource::Partial {
            fields: fields.clone(),
        }],
        Type::Union(type_ids) => type_ids
            .iter()
            .flat_map(|&tid| extract_field_sources(program, tid))
            .collect(),
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

/// Get field type ID at position from a field source
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
        FieldSource::Partial { fields } => fields.get(position).map(|(_, ftype)| *ftype),
    }
}

/// Get field count and first field types from a type (supports both tuples and partials)
/// Returns (field_count, first_field_type_ids) for equality comparison
/// All sources must have the same field count
pub fn get_field_count_and_first_types(
    program: &Program,
    type_id: usize,
) -> Result<(usize, Vec<usize>), Error> {
    let sources = extract_field_sources(program, type_id);

    if sources.is_empty() {
        return Err(Error::TypeMismatch {
            expected: "tuple".to_string(),
            found: "unknown".to_string(),
        });
    }

    let mut common_field_count = None;
    let mut first_field_types = Vec::new();

    for source in &sources {
        let (field_count, first_type) = match source {
            FieldSource::Tuple(tuple_id) => {
                let tuple_info =
                    program
                        .lookup_tuple(*tuple_id)
                        .ok_or(Error::TupleNotInRegistry {
                            tuple_id: *tuple_id,
                        })?;
                let count = tuple_info.fields.len();
                let first = tuple_info.fields.first().map(|(_, t)| *t);
                (count, first)
            }
            FieldSource::Partial { fields } => {
                let count = fields.len();
                let first = fields.first().map(|(_, t)| *t);
                (count, first)
            }
        };

        if field_count == 0 {
            return Err(Error::TypeMismatch {
                expected: "non-empty tuple".to_string(),
                found: "empty tuple".to_string(),
            });
        }

        if let Some(prev_count) = common_field_count {
            if prev_count != field_count {
                return Err(Error::TypeMismatch {
                    expected: format!("tuple with {} fields", prev_count),
                    found: format!("tuple with {} fields", field_count),
                });
            }
        } else {
            common_field_count = Some(field_count);
            if let Some(ft) = first_type {
                first_field_types.push(ft);
            }
        }
    }

    Ok((common_field_count.unwrap(), first_field_types))
}

/// Get field info by name from a type (supports both tuples and partials)
/// Returns (index, field_type_ids) where index is the field position and
/// field_type_ids are the possible types from all sources
/// For union types, ALL variants must have the field (not just some)
pub fn get_field_by_name(
    program: &Program,
    type_id: usize,
    field_name: &str,
    target_name: &str,
) -> Result<(usize, Vec<usize>), Error> {
    let sources = extract_field_sources(program, type_id);

    if sources.is_empty() {
        return Err(Error::MemberAccessOnNonTuple {
            target: target_name.to_string(),
        });
    }

    let mut results = Vec::new();
    let mut common_index = None;

    for source in &sources {
        // ALL sources must have the field for a union type
        let (idx, ftype) = get_field_from_source(program, source, field_name).ok_or_else(|| {
            Error::MemberFieldNotFound {
                field_name: field_name.to_string(),
                target: target_name.to_string(),
            }
        })?;

        // Verify all sources have the field at the same index
        if let Some(prev_index) = common_index {
            if prev_index != idx {
                return Err(Error::MemberFieldNotFound {
                    field_name: field_name.to_string(),
                    target: target_name.to_string(),
                });
            }
        } else {
            common_index = Some(idx);
        }
        results.push(ftype);
    }

    if results.is_empty() {
        return Err(Error::MemberFieldNotFound {
            field_name: field_name.to_string(),
            target: target_name.to_string(),
        });
    }

    Ok((common_index.unwrap(), results))
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
