use std::collections::HashMap;

use crate::{
    ast,
    bytecode::TypeId,
    types::{CallableType, Type},
    vm::VM,
};

use super::Error;

#[derive(Debug, Clone)]
pub enum TupleAccessor {
    Field(String),
    Position(usize),
}

pub fn union_types(types: Vec<Type>) -> Result<Type, Error> {
    let mut flattened = Vec::new();
    for typ in types {
        match typ {
            Type::Union(variants) => flattened.extend(variants),
            t => flattened.push(t),
        }
    }

    if flattened.is_empty() {
        Ok(Type::Union(vec![]))
    } else {
        Ok(Type::from_types(flattened))
    }
}

pub struct TypeContext<'a> {
    pub type_aliases: &'a mut HashMap<String, Type>,
    pub resolution_stack: Vec<String>,
}

impl<'a> TypeContext<'a> {
    pub fn new(type_aliases: &'a mut HashMap<String, Type>) -> Self {
        Self {
            type_aliases,
            resolution_stack: Vec::new(),
        }
    }

    pub fn resolve_ast_type(&mut self, ast_type: ast::Type, vm: &mut VM) -> Result<Type, Error> {
        match ast_type {
            ast::Type::Primitive(ast::PrimitiveType::Int) => Ok(Type::Integer),
            ast::Type::Primitive(ast::PrimitiveType::Bin) => Ok(Type::Binary),
            ast::Type::Tuple(tuple) => {
                // Resolve field types without distributing unions
                let mut fields = Vec::new();

                for field in tuple.fields {
                    let field_type = self.resolve_ast_type(field.type_def, vm)?;
                    fields.push((field.name, field_type));
                }

                // Register the tuple type with potentially union field types
                let type_id = vm.register_type(tuple.name, fields);
                Ok(Type::Tuple(type_id))
            }
            ast::Type::Function(function) => {
                let input_type = self.resolve_ast_type(*function.input, vm)?;
                let output_type = self.resolve_ast_type(*function.output, vm)?;

                // Create function type without distributing unions
                Ok(Type::Callable(Box::new(CallableType {
                    parameter: input_type,
                    result: output_type,
                })))
            }
            ast::Type::Union(union) => {
                // Resolve all union member types
                let mut resolved_types = Vec::new();
                for member_type in union.types {
                    let member_type = self.resolve_ast_type(member_type, vm)?;
                    match member_type {
                        Type::Union(variants) => resolved_types.extend(variants),
                        t => resolved_types.push(t),
                    }
                }

                // Return as union type
                if resolved_types.is_empty() {
                    return Err(Error::TypeUnresolved("Empty union type".to_string()));
                }
                Ok(Type::from_types(resolved_types))
            }
            ast::Type::Identifier(alias) => {
                // Check if this creates a cycle
                if let Some(depth) = self.resolution_stack.iter().rev().position(|x| x == &alias) {
                    // Found a cycle! Return a Cycle type
                    return Ok(Type::Cycle(depth + 1));
                }

                // Look up type alias
                if let Some(aliased_type) = self.type_aliases.get(&alias) {
                    // Check if it's a placeholder (Union with empty vec)
                    if let Type::Union(types) = aliased_type {
                        if types.is_empty() {
                            // This is a forward reference to a type being defined
                            // For now, return a cycle pointing to the current level
                            return Ok(Type::Cycle(self.resolution_stack.len()));
                        }
                    }

                    // Push to stack for cycle detection
                    self.resolution_stack.push(alias.clone());
                    let result = Ok(aliased_type.clone());
                    self.resolution_stack.pop();
                    result
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
        vm: &VM,
    ) -> Result<(usize, Type), Error> {
        let tuple_type = vm
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
        vm: &VM,
    ) -> Result<Type, Error> {
        let tuple_type = vm
            .lookup_type(type_id)
            .ok_or_else(|| Error::TypeNotInRegistry { type_id: *type_id })?;
        if position >= tuple_type.1.len() {
            return Err(Error::PositionalAccessOnNonTuple { index: position });
        }
        Ok(tuple_type.1[position].1.clone())
    }
}
