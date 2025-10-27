use crate::ast;
use quiver_core::{
    bytecode::TypeId,
    program::Program,
    types::{Type, TypeLookup},
};

use super::Error;

/// Get field type at the given position for all tuple types in the list
/// Returns error if any tuple doesn't have the field or if tuples have different structures
pub fn get_field_types_at_position(
    program: &Program,
    tuple_types: &[TypeId],
    position: usize,
) -> Result<Vec<Type>, Error> {
    let mut field_types = Vec::new();

    for type_id in tuple_types {
        let type_info = program
            .lookup_type(type_id)
            .ok_or(Error::TypeNotInRegistry { type_id: *type_id })?;
        let fields = &type_info.fields;

        if let Some((_, field_type)) = fields.get(position) {
            field_types.push(field_type.clone());
        } else {
            return Err(Error::PositionalIndexOutOfBounds { index: position });
        }
    }

    Ok(field_types)
}

/// Get field type by name for all tuple types in the list
/// Returns error if any tuple doesn't have the field
/// Also ensures all tuples have the field at the same position
pub fn get_field_types_by_name(
    program: &Program,
    tuple_types: &[TypeId],
    field_name: &str,
) -> Result<Vec<(usize, Type)>, Error> {
    let mut results = Vec::new();
    let mut common_index = None;

    for type_id in tuple_types {
        // Look up the field in this tuple type
        let tuple_type = program
            .lookup_type(type_id)
            .ok_or(Error::TypeNotInRegistry { type_id: *type_id })?;
        let (index, (_, field_type)) = tuple_type
            .fields
            .iter()
            .enumerate()
            .find(|(_, field)| field.0.as_deref() == Some(field_name))
            .ok_or(Error::FieldNotFound {
                field_name: field_name.to_string(),
                type_name: format!("{:?}", type_id),
            })?;

        // Verify all tuples have the field at the same index
        if let Some(prev_index) = common_index {
            if prev_index != index {
                return Err(Error::MemberFieldNotFound {
                    field_name: field_name.to_string(),
                    target: "multiple tuple types".to_string(),
                });
            }
        } else {
            common_index = Some(index);
        }

        results.push((index, field_type.clone()));
    }

    Ok(results)
}

/// Resolve the type of an accessor path applied to a given type
/// Used to determine the resulting type after accessing nested fields
pub fn resolve_accessor_type(
    program: &Program,
    mut current_type: Type,
    accessors: &[ast::AccessPath],
    target_name: &str,
) -> Result<Type, Error> {
    for accessor in accessors {
        let tuple_types = current_type.extract_tuple_types();

        if tuple_types.is_empty() {
            return Err(Error::MemberAccessOnNonTuple {
                target: target_name.to_string(),
            });
        }

        let field_types =
            match accessor {
                ast::AccessPath::Field(field_name) => {
                    let results = get_field_types_by_name(program, &tuple_types, field_name)
                        .map_err(|_| Error::MemberFieldNotFound {
                            field_name: field_name.clone(),
                            target: target_name.to_string(),
                        })?;
                    if results.is_empty() {
                        return Err(Error::MemberFieldNotFound {
                            field_name: field_name.clone(),
                            target: target_name.to_string(),
                        });
                    }
                    results.into_iter().map(|(_, t)| t).collect()
                }
                ast::AccessPath::Index(index) => {
                    get_field_types_at_position(program, &tuple_types, *index).map_err(|_| {
                        Error::MemberAccessOnNonTuple {
                            target: target_name.to_string(),
                        }
                    })?
                }
            };

        current_type = Type::from_types(field_types);
    }

    Ok(current_type)
}
