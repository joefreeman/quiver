use std::collections::HashMap;

use crate::{
    ast,
    bytecode::TypeId,
    types,
};

use super::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Resolved(types::Type),
    Unresolved(Vec<types::Type>),
}

#[derive(Debug, Clone)]
pub enum TupleAccessor {
    Field(String),
    Position(usize),
}

impl Type {
    pub fn to_type_vec(&self) -> Vec<types::Type> {
        match self {
            Type::Resolved(t) => vec![t.clone()],
            Type::Unresolved(types) => types.clone(),
        }
    }
}

pub fn narrow_types(types: Vec<Type>) -> Result<Type, Error> {
    let mut flattened = Vec::new();
    for t in types {
        match t {
            Type::Unresolved(ts) => flattened.extend(ts),
            Type::Resolved(t) => flattened.push(t),
        }
    }

    flattened.dedup();

    match flattened.len() {
        0 => Err(Error::TypeUnresolved(
            "No valid types found in narrowing".to_string(),
        )),
        1 => Ok(Type::Resolved(flattened.get(0).unwrap().clone())),
        _ => Ok(Type::Unresolved(flattened)),
    }
}

pub struct TypeContext<'a> {
    pub type_aliases: HashMap<String, Type>,
    pub type_registry: &'a mut types::TypeRegistry,
}

impl<'a> TypeContext<'a> {
    pub fn new(type_registry: &'a mut types::TypeRegistry) -> Self {
        Self {
            type_aliases: HashMap::new(),
            type_registry,
        }
    }

    pub fn resolve_ast_type(&mut self, ast_type: ast::Type) -> Result<Type, Error> {
        match ast_type {
            ast::Type::Primitive(ast::PrimitiveType::Int) => {
                Ok(Type::Resolved(types::Type::Integer))
            }
            ast::Type::Primitive(ast::PrimitiveType::Bin) => {
                Ok(Type::Resolved(types::Type::Binary))
            }
            ast::Type::Tuple(tuple) => {
                // Collect all possible types for each field
                let mut field_variants: Vec<(Option<String>, Vec<types::Type>)> = Vec::new();

                for field in tuple.fields {
                    let field_possibilities = match self.resolve_ast_type(field.type_def)? {
                        Type::Resolved(t) => vec![t],
                        Type::Unresolved(ts) => {
                            if ts.is_empty() {
                                return Err(Error::TypeUnresolved(
                                    "Empty field type possibilities".to_string(),
                                ));
                            }
                            ts
                        }
                    };
                    field_variants.push((field.name, field_possibilities));
                }

                // Generate cartesian product of all field type combinations
                let tuple_variants =
                    self.cartesian_product_tuple_types(&tuple.name, field_variants);

                Ok(match tuple_variants.len() {
                    0 => {
                        return Err(Error::TypeUnresolved(
                            "No valid tuple type variants found".to_string(),
                        ));
                    }
                    1 => Type::Resolved(tuple_variants.into_iter().next().unwrap()),
                    _ => Type::Unresolved(tuple_variants),
                })
            }
            ast::Type::Function(function) => {
                let input_possibilities = match self.resolve_ast_type(*function.input)? {
                    Type::Resolved(t) => vec![t],
                    Type::Unresolved(ts) => {
                        if ts.is_empty() {
                            return Err(Error::TypeUnresolved(
                                "Empty function input type possibilities".to_string(),
                            ));
                        }
                        ts
                    }
                };
                let output_possibilities = match self.resolve_ast_type(*function.output)? {
                    Type::Resolved(t) => vec![t],
                    Type::Unresolved(ts) => {
                        if ts.is_empty() {
                            return Err(Error::TypeUnresolved(
                                "Empty function output type possibilities".to_string(),
                            ));
                        }
                        ts
                    }
                };

                // Generate cartesian product of input × output types
                let function_variants: Vec<types::Type> = input_possibilities
                    .into_iter()
                    .flat_map(|input_type| {
                        output_possibilities.iter().map(move |output_type| {
                            types::Type::Function(Box::new(types::FunctionType {
                                parameter: vec![input_type.clone()],
                                result: vec![output_type.clone()],
                            }))
                        })
                    })
                    .collect();

                Ok(match function_variants.len() {
                    0 => {
                        return Err(Error::TypeUnresolved(
                            "No valid function type variants found".to_string(),
                        ));
                    }
                    1 => Type::Resolved(function_variants.into_iter().next().unwrap()),
                    _ => Type::Unresolved(function_variants),
                })
            }
            ast::Type::Union(union) => {
                // Resolve all union member types
                let mut resolved_types = Vec::new();
                for member_type in union.types {
                    match self.resolve_ast_type(member_type)? {
                        Type::Resolved(t) => resolved_types.push(t),
                        Type::Unresolved(ts) => {
                            if ts.is_empty() {
                                return Err(Error::TypeUnresolved(
                                    "Empty union member type".to_string(),
                                ));
                            }
                            resolved_types.extend(ts);
                        }
                    }
                }

                // Return as unresolved union type
                if resolved_types.is_empty() {
                    return Err(Error::TypeUnresolved("Empty union type".to_string()));
                }
                Ok(Type::Unresolved(resolved_types))
            }
            ast::Type::Identifier(alias) => {
                // Look up type alias
                if let Some(aliased_type) = self.type_aliases.get(&alias) {
                    Ok(aliased_type.clone())
                } else {
                    Err(Error::TypeAliasMissing(alias))
                }
            }
        }
    }

    pub fn get_tuple_field_type_by_name(
        &self,
        type_id: &TypeId,
        field_name: &str,
    ) -> Result<(usize, types::Type), Error> {
        let tuple_type = self.type_registry.lookup_type(type_id).unwrap();
        let (index, (_, field_type)) = tuple_type
            .1
            .iter()
            .enumerate()
            .find(|(_, field)| field.0.as_deref() == Some(field_name))
            .ok_or(Error::FieldNotFound {
                field_name: field_name.to_string(),
                type_name: format!("{:?}", type_id),
            })?;
        Ok((index, field_type.clone()))
    }

    pub fn get_tuple_field_type_by_position(
        &self,
        type_id: &TypeId,
        position: usize,
    ) -> Result<types::Type, Error> {
        let tuple_type = self.type_registry.lookup_type(type_id).unwrap();
        if position >= tuple_type.1.len() {
            return Err(Error::PositionalAccessOnNonTuple { index: position });
        }
        Ok(tuple_type.1[position].1.clone())
    }

    pub fn cartesian_product_tuple_types(
        &mut self,
        tuple_name: &Option<String>,
        field_variants: Vec<(Option<String>, Vec<types::Type>)>,
    ) -> Vec<types::Type> {
        // Handle empty fields case
        if field_variants.is_empty() {
            let type_id = self.type_registry.register_type(tuple_name.clone(), vec![]);
            return vec![types::Type::Tuple(type_id)];
        }

        // Generate all combinations using recursive cartesian product
        let combinations = self.cartesian_product_recursive(&field_variants, 0, vec![]);

        // Register each combination as a tuple type
        combinations
            .into_iter()
            .map(|field_types| {
                let type_id = self
                    .type_registry
                    .register_type(tuple_name.clone(), field_types);
                types::Type::Tuple(type_id)
            })
            .collect()
    }

    fn cartesian_product_recursive(
        &self,
        field_variants: &[(Option<String>, Vec<types::Type>)],
        index: usize,
        current: Vec<(Option<String>, types::Type)>,
    ) -> Vec<Vec<(Option<String>, types::Type)>> {
        if index >= field_variants.len() {
            return vec![current];
        }

        let (field_name, type_options) = &field_variants[index];
        let mut results = Vec::new();

        for type_option in type_options {
            let mut new_current = current.clone();
            new_current.push((field_name.clone(), type_option.clone()));

            let sub_results =
                self.cartesian_product_recursive(field_variants, index + 1, new_current);
            results.extend(sub_results);
        }

        results
    }
}