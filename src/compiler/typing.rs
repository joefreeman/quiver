use std::collections::HashMap;

use crate::{
    ast,
    bytecode::TypeId,
    types::{FunctionType, Type, TypeRegistry},
};

use super::Error;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeSet(pub Vec<Type>);

impl TypeSet {
    pub fn new(types: Vec<Type>) -> Result<Self, Error> {
        if types.is_empty() {
            Err(Error::TypeUnresolved("Empty type set".to_string()))
        } else {
            Ok(TypeSet(types))
        }
    }

    pub fn resolved(t: Type) -> Self {
        TypeSet(vec![t])
    }

    pub fn unresolved(types: Vec<Type>) -> Result<Self, Error> {
        if types.is_empty() {
            Err(Error::TypeUnresolved("Empty type set".to_string()))
        } else {
            Ok(TypeSet(types))
        }
    }

    pub fn is_resolved(&self) -> bool {
        self.0.len() == 1
    }

    pub fn single(&self) -> Option<&Type> {
        if self.0.len() == 1 {
            Some(&self.0[0])
        } else {
            None
        }
    }

    pub fn to_vec(&self) -> Vec<Type> {
        self.0.clone()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> std::slice::Iter<Type> {
        self.0.iter()
    }
}

#[derive(Debug, Clone)]
pub enum TupleAccessor {
    Field(String),
    Position(usize),
}

pub fn narrow_types(types: Vec<TypeSet>) -> Result<TypeSet, Error> {
    let mut flattened = Vec::new();
    for type_set in types {
        flattened.extend(type_set.to_vec());
    }

    flattened.dedup();

    if flattened.is_empty() {
        Err(Error::TypeUnresolved(
            "No valid types found in narrowing".to_string(),
        ))
    } else {
        Ok(TypeSet(flattened))
    }
}

pub struct TypeContext<'a> {
    pub type_aliases: HashMap<String, TypeSet>,
    pub type_registry: &'a mut TypeRegistry,
}

impl<'a> TypeContext<'a> {
    pub fn new(
        type_registry: &'a mut TypeRegistry,
        existing_aliases: &HashMap<String, Vec<Type>>,
    ) -> Self {
        let mut type_aliases = HashMap::new();
        for (name, types) in existing_aliases.iter() {
            if !types.is_empty() {
                type_aliases.insert(name.clone(), TypeSet(types.clone()));
            }
        }
        Self {
            type_aliases,
            type_registry,
        }
    }

    pub fn resolve_ast_type(&mut self, ast_type: ast::Type) -> Result<TypeSet, Error> {
        match ast_type {
            ast::Type::Primitive(ast::PrimitiveType::Int) => Ok(TypeSet::resolved(Type::Integer)),
            ast::Type::Primitive(ast::PrimitiveType::Bin) => Ok(TypeSet::resolved(Type::Binary)),
            ast::Type::Tuple(tuple) => {
                // Collect all possible types for each field
                let mut field_variants: Vec<(Option<String>, Vec<Type>)> = Vec::new();

                for field in tuple.fields {
                    let field_type_set = self.resolve_ast_type(field.type_def)?;
                    let field_possibilities = field_type_set.to_vec();
                    field_variants.push((field.name, field_possibilities));
                }

                // Generate cartesian product of all field type combinations
                let tuple_variants =
                    self.cartesian_product_tuple_types(&tuple.name, field_variants);

                if tuple_variants.is_empty() {
                    return Err(Error::TypeUnresolved(
                        "No valid tuple type variants found".to_string(),
                    ));
                }
                Ok(TypeSet(tuple_variants))
            }
            ast::Type::Function(function) => {
                let input_type_set = self.resolve_ast_type(*function.input)?;
                let input_possibilities = input_type_set.to_vec();

                let output_type_set = self.resolve_ast_type(*function.output)?;
                let output_possibilities = output_type_set.to_vec();

                // Generate cartesian product of input × output types
                let function_variants: Vec<Type> = input_possibilities
                    .into_iter()
                    .flat_map(|input_type| {
                        output_possibilities.iter().map(move |output_type| {
                            Type::Function(Box::new(FunctionType {
                                parameter: vec![input_type.clone()],
                                result: vec![output_type.clone()],
                            }))
                        })
                    })
                    .collect();

                if function_variants.is_empty() {
                    return Err(Error::TypeUnresolved(
                        "No valid function type variants found".to_string(),
                    ));
                }
                Ok(TypeSet(function_variants))
            }
            ast::Type::Union(union) => {
                // Resolve all union member types
                let mut resolved_types = Vec::new();
                for member_type in union.types {
                    let member_type_set = self.resolve_ast_type(member_type)?;
                    resolved_types.extend(member_type_set.to_vec());
                }

                // Return as unresolved union type
                if resolved_types.is_empty() {
                    return Err(Error::TypeUnresolved("Empty union type".to_string()));
                }
                Ok(TypeSet(resolved_types))
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
    ) -> Result<(usize, Type), Error> {
        let tuple_type = self
            .type_registry
            .lookup_type(type_id)
            .ok_or_else(|| Error::TypeNotInRegistry { type_id: *type_id })?;
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
    ) -> Result<Type, Error> {
        let tuple_type = self
            .type_registry
            .lookup_type(type_id)
            .ok_or_else(|| Error::TypeNotInRegistry { type_id: *type_id })?;
        if position >= tuple_type.1.len() {
            return Err(Error::PositionalAccessOnNonTuple { index: position });
        }
        Ok(tuple_type.1[position].1.clone())
    }

    pub fn cartesian_product_tuple_types(
        &mut self,
        tuple_name: &Option<String>,
        field_variants: Vec<(Option<String>, Vec<Type>)>,
    ) -> Vec<Type> {
        // Handle empty fields case
        if field_variants.is_empty() {
            let type_id = self.type_registry.register_type(tuple_name.clone(), vec![]);
            return vec![Type::Tuple(type_id)];
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
                Type::Tuple(type_id)
            })
            .collect()
    }

    fn cartesian_product_recursive(
        &self,
        field_variants: &[(Option<String>, Vec<Type>)],
        index: usize,
        current: Vec<(Option<String>, Type)>,
    ) -> Vec<Vec<(Option<String>, Type)>> {
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
