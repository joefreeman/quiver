use std::collections::HashMap;

use crate::{
    ast,
    bytecode::TypeId,
    program::Program,
    types::{CallableType, Type, TypeLookup},
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
        Ok(Type::never())
    } else {
        Ok(Type::from_types(flattened))
    }
}

pub struct TypeContext<'a> {
    pub type_aliases: &'a mut HashMap<String, Type>,
    pub union_stack: Vec<()>, // Track union boundaries for cycle depth calculation
}

impl<'a> TypeContext<'a> {
    pub fn new(type_aliases: &'a mut HashMap<String, Type>) -> Self {
        Self {
            type_aliases,
            union_stack: Vec::new(),
        }
    }

    /// Check if an AST type contains any cycle references
    fn ast_contains_cycle(typ: &ast::Type) -> bool {
        match typ {
            ast::Type::Cycle(_) => true,
            ast::Type::Union(union) => union.types.iter().any(|v| Self::ast_contains_cycle(v)),
            ast::Type::Function(func) => {
                Self::ast_contains_cycle(&func.input) || Self::ast_contains_cycle(&func.output)
            }
            ast::Type::Tuple(tuple) => tuple
                .fields
                .iter()
                .any(|f| Self::ast_contains_cycle(&f.type_def)),
            ast::Type::Process(process) => Self::ast_contains_cycle(&process.receive_type),
            ast::Type::Primitive(_) | ast::Type::Identifier(_) => false,
        }
    }

    /// Validate that a union has at least one non-recursive variant (base case)
    fn validate_union_has_base_case(variants: &[ast::Type]) -> Result<(), Error> {
        let has_base_case = variants.iter().any(|v| !Self::ast_contains_cycle(v));
        if !has_base_case {
            return Err(Error::TypeUnresolved(
                "Union must have a base case with a non-recursive variant".to_string(),
            ));
        }
        Ok(())
    }

    pub fn resolve_ast_type(
        &mut self,
        ast_type: ast::Type,
        program: &mut Program,
    ) -> Result<Type, Error> {
        match ast_type {
            ast::Type::Primitive(ast::PrimitiveType::Int) => Ok(Type::Integer),
            ast::Type::Primitive(ast::PrimitiveType::Bin) => Ok(Type::Binary),
            ast::Type::Tuple(tuple) => {
                // Resolve field types without distributing unions
                let mut fields = Vec::new();

                for field in tuple.fields {
                    let field_type = self.resolve_ast_type(field.type_def, program)?;
                    fields.push((field.name, field_type));
                }

                // Register the tuple type with potentially union field types
                let type_id = program.register_type(tuple.name, fields);
                Ok(Type::Tuple(type_id))
            }
            ast::Type::Function(function) => {
                let input_type = self.resolve_ast_type(*function.input, program)?;
                let output_type = self.resolve_ast_type(*function.output, program)?;

                // Create function type without distributing unions
                Ok(Type::Callable(Box::new(CallableType {
                    parameter: input_type,
                    result: output_type,
                    receive: Type::never(),
                })))
            }
            ast::Type::Union(union) => {
                // Validate that union has at least one base case before resolution
                if union.types.is_empty() {
                    return Err(Error::TypeUnresolved("Empty union type".to_string()));
                }
                Self::validate_union_has_base_case(&union.types)?;

                // Push union boundary marker
                self.union_stack.push(());

                // Resolve all union member types
                let mut resolved_types = Vec::new();
                for member_type in union.types {
                    let member_type = self.resolve_ast_type(member_type, program)?;
                    match member_type {
                        Type::Union(variants) => resolved_types.extend(variants),
                        t => resolved_types.push(t),
                    }
                }

                // Pop union boundary marker
                self.union_stack.pop();

                Ok(Type::from_types(resolved_types))
            }
            ast::Type::Cycle(target_depth) => {
                // & or &N syntax for cycle references
                let target_depth = target_depth.unwrap_or(0);

                // Validate: cycles require enclosing union
                if self.union_stack.is_empty() {
                    return Err(Error::TypeUnresolved(
                        "Cycle reference '&' requires enclosing union type".to_string(),
                    ));
                }

                // Validate: target_depth must be <= union_stack.len()
                if target_depth >= self.union_stack.len() {
                    return Err(Error::TypeUnresolved(format!(
                        "Invalid cycle reference &{}: only {} union level(s) deep",
                        target_depth,
                        self.union_stack.len()
                    )));
                }

                // Calculate actual depth for Cycle(depth)
                // target_depth is downward from root (0 = root, 1 = first nested union)
                // Cycle(depth) is upward from current position
                let cycle_depth = self.union_stack.len() - target_depth;

                Ok(Type::Cycle(cycle_depth))
            }
            ast::Type::Process(process) => {
                let receive_type = self.resolve_ast_type(*process.receive_type, program)?;
                Ok(Type::Process(Box::new(receive_type)))
            }
            ast::Type::Identifier(alias) => {
                // Look up already-defined type alias
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
        type_lookup: &impl TypeLookup,
    ) -> Result<(usize, Type), Error> {
        let tuple_type = type_lookup
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
        type_lookup: &impl TypeLookup,
    ) -> Result<Type, Error> {
        let tuple_type = type_lookup
            .lookup_type(type_id)
            .ok_or_else(|| Error::TypeNotInRegistry { type_id: *type_id })?;
        if position >= tuple_type.1.len() {
            return Err(Error::PositionalAccessOnNonTuple { index: position });
        }
        Ok(tuple_type.1[position].1.clone())
    }
}
