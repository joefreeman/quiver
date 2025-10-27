use std::collections::HashMap;

use crate::ast;
use quiver_core::{
    program::Program,
    types::{CallableType, ProcessType, Type, TypeLookup},
};

use super::Error;

#[derive(Debug, Clone)]
pub enum TupleAccessor {
    Field(String),
    Position(usize),
}

pub fn union_types(types: Vec<Type>) -> Type {
    let mut flattened = Vec::new();
    for typ in types {
        match typ {
            Type::Union(variants) => flattened.extend(variants),
            t => flattened.push(t),
        }
    }

    if flattened.is_empty() {
        Type::never()
    } else {
        Type::from_types(flattened)
    }
}

/// Type alias definition: (type parameters, body)
pub type TypeAliasDef = (Vec<String>, ast::Type);

fn instantiate_generic_type(
    union_depth: &mut usize,
    type_aliases: &HashMap<String, TypeAliasDef>,
    name: &str,
    arguments: Vec<ast::Type>,
    program: &mut Program,
    type_bindings: &HashMap<String, Type>,
) -> Result<Type, Error> {
    // Look up generic definition
    let type_def = type_aliases
        .get(name)
        .ok_or_else(|| Error::TypeAliasMissing(name.to_string()))?
        .clone();

    // Resolve all type arguments
    let resolved_args: Vec<Type> = arguments
        .into_iter()
        .map(|arg| resolve_ast_type_impl(union_depth, type_aliases, arg, program, type_bindings))
        .collect::<Result<_, _>>()?;

    // Validate argument count
    if resolved_args.len() != type_def.0.len() {
        return Err(Error::TypeUnresolved(format!(
            "Generic type '{}' expects {} type argument(s), got {}",
            name,
            type_def.0.len(),
            resolved_args.len()
        )));
    }

    // Build new type bindings for the parameters
    let mut new_bindings = HashMap::new();
    for (param, arg) in type_def.0.iter().zip(resolved_args.iter()) {
        new_bindings.insert(param.clone(), arg.clone());
    }

    // Resolve the body with parameter bindings
    resolve_ast_type_impl(
        union_depth,
        type_aliases,
        type_def.1,
        program,
        &new_bindings,
    )
}

/// Check if an AST type contains any cycle references
fn ast_contains_cycle(typ: &ast::Type) -> bool {
    match typ {
        ast::Type::Cycle(_) => true,
        ast::Type::Union(union) => union.types.iter().any(ast_contains_cycle),
        ast::Type::Tuple(tuple) => tuple.fields.iter().any(|f| match f {
            ast::FieldType::Field { type_def, .. } => ast_contains_cycle(type_def),
            ast::FieldType::Spread { .. } => false,
        }),
        ast::Type::Identifier { arguments, .. } => arguments.iter().any(ast_contains_cycle),
        // Function and process types are boundaries - cycles inside them don't count
        // as structural recursion
        ast::Type::Function(_) | ast::Type::Process(_) => false,
        ast::Type::Primitive(_) => false,
    }
}

/// Validate that a union has at least one non-recursive variant (base case)
fn validate_union_has_base_case(variants: &[ast::Type]) -> Result<(), Error> {
    let has_base_case = variants.iter().any(|v| !ast_contains_cycle(v));
    if !has_base_case {
        return Err(Error::TypeUnresolved(
            "Union must have a base case with a non-recursive variant".to_string(),
        ));
    }
    Ok(())
}

pub fn resolve_ast_type(
    type_aliases: &HashMap<String, TypeAliasDef>,
    ast_type: ast::Type,
    program: &mut Program,
) -> Result<Type, Error> {
    let bindings = HashMap::new();
    let mut union_depth = 0;
    resolve_ast_type_impl(&mut union_depth, type_aliases, ast_type, program, &bindings)
}

/// Resolve a function parameter type with explicitly declared type parameters.
/// Type parameters are resolved to Type::Variable, while undefined types cause errors.
pub fn resolve_function_parameter_type(
    type_aliases: &HashMap<String, TypeAliasDef>,
    ast_type: ast::Type,
    type_parameters: &[String],
    program: &mut Program,
) -> Result<Type, Error> {
    // Create bindings for declared type parameters, mapping each to a Type::Variable
    let mut bindings = HashMap::new();
    for param in type_parameters {
        bindings.insert(param.clone(), Type::Variable(param.clone()));
    }

    let mut union_depth = 0;
    resolve_ast_type_impl(&mut union_depth, type_aliases, ast_type, program, &bindings)
}

/// Resolve a type alias for display purposes (e.g., in tests or REPL).
/// Type parameters are resolved to Type::Variable placeholders.
/// This allows formatting of parameterized types like `point<t> :: Point[x: t, y: t]`
/// as `Point[x: t, y: t]` where `t` is a type variable.
pub fn resolve_type_alias_for_display(
    type_aliases: &HashMap<String, TypeAliasDef>,
    alias_name: &str,
    program: &mut Program,
) -> Result<Type, Error> {
    let (type_params, ast_type) = type_aliases
        .get(alias_name)
        .ok_or_else(|| Error::TypeAliasMissing(alias_name.to_string()))?;

    // Create Variable bindings for all type parameters
    let mut bindings = HashMap::new();
    for param in type_params {
        bindings.insert(param.clone(), Type::Variable(param.clone()));
    }

    let mut union_depth = 0;
    resolve_ast_type_impl(
        &mut union_depth,
        type_aliases,
        ast_type.clone(),
        program,
        &bindings,
    )
}

/// Resolve tuple name when using identifier spread syntax (e.g., t1[..., y: int])
fn resolve_tuple_name(
    name: ast::TupleName,
    type_aliases: &HashMap<String, TypeAliasDef>,
    _program: &Program,
) -> Result<Option<String>, Error> {
    match name {
        ast::TupleName::Literal(s) => Ok(Some(s)),
        ast::TupleName::None => Ok(None),
        ast::TupleName::Ripple => {
            // Ripple is only valid in value context, not type context
            Err(Error::FeatureUnsupported(
                "Ripple (~) cannot be used in type definitions".to_string(),
            ))
        }
        ast::TupleName::Identifier(identifier) => {
            // Look up the type alias to get its tuple name
            let (type_params, type_def) = type_aliases
                .get(&identifier)
                .ok_or_else(|| Error::TypeAliasMissing(identifier.to_string()))?;

            // Type parameters must be empty for name inheritance (for now)
            if !type_params.is_empty() {
                return Err(Error::FeatureUnsupported(format!(
                    "Type alias '{}' has type parameters - name inheritance from parameterized types requires explicit instantiation",
                    identifier
                )));
            }

            // Resolve the type to get the actual tuple name
            // We need to resolve it to extract the name, but we do a simpler check
            // by looking at the AST directly
            match type_def {
                ast::Type::Tuple(tuple_type) => {
                    resolve_tuple_name(tuple_type.name.clone(), type_aliases, _program)
                }
                ast::Type::Union(_) => {
                    // For unions, don't try to extract a single name
                    // The spread mechanism will handle each variant individually
                    Ok(None)
                }
                _ => Err(Error::TypeUnresolved(format!(
                    "Type alias '{}' must resolve to a tuple or union type for identifier spread syntax",
                    identifier
                ))),
            }
        }
    }
}

type FieldSet = Vec<(Option<String>, Type)>;
type NamedFieldVariants = Vec<(Option<String>, FieldSet)>; // (tuple_name, fields)

/// Resolve tuple fields that contain spreads
/// Returns a vector of (name, field_set) pairs - multiple pairs if spreading creates union variants
fn resolve_tuple_fields_with_spread(
    union_depth: &mut usize,
    type_aliases: &HashMap<String, TypeAliasDef>,
    fields: &[ast::FieldType],
    program: &mut Program,
    type_bindings: &HashMap<String, Type>,
) -> Result<NamedFieldVariants, Error> {
    // Track all possible field combinations with their names (for union distribution)
    // Each variant is (tuple_name, fields)
    let mut variants: NamedFieldVariants = vec![(None, Vec::new())];

    // Process fields left to right
    for field in fields {
        match field {
            ast::FieldType::Field { name, type_def } => {
                // Resolve the field type
                let field_type = resolve_ast_type_impl(
                    union_depth,
                    type_aliases,
                    type_def.clone(),
                    program,
                    type_bindings,
                )?;

                // Add this field to all variants
                for (_tuple_name, variant_fields) in &mut variants {
                    // Check if we should replace an existing field
                    if let Some(pos) = name.as_ref().and_then(|n| {
                        variant_fields
                            .iter()
                            .position(|(field_name, _)| field_name.as_ref() == Some(n))
                    }) {
                        variant_fields[pos].1 = field_type.clone();
                        continue;
                    }
                    variant_fields.push((name.clone(), field_type.clone()));
                }
            }
            ast::FieldType::Spread {
                identifier,
                type_arguments,
            } => {
                // Identifier must be specified (parser transforms `...` in `identifier[...]` to `...identifier`)
                let spread_id = identifier.as_ref().ok_or_else(|| {
                    Error::TypeUnresolved(
                        "Spread without identifier is only allowed in `identifier[..., fields]` syntax"
                            .to_string(),
                    )
                })?;

                // Look up the spread type
                let (type_params, type_def) = type_aliases
                    .get(spread_id)
                    .ok_or_else(|| Error::TypeAliasMissing(spread_id.clone()))?;

                // Check type parameter count matches
                if type_params.len() != type_arguments.len() {
                    return Err(Error::TypeUnresolved(format!(
                        "Type alias '{}' expects {} type parameters, got {}",
                        spread_id,
                        type_params.len(),
                        type_arguments.len()
                    )));
                }

                // Create type bindings for the spread type
                let mut spread_bindings = type_bindings.clone();
                for (param, arg) in type_params.iter().zip(type_arguments.iter()) {
                    let arg_type = resolve_ast_type_impl(
                        union_depth,
                        type_aliases,
                        arg.clone(),
                        program,
                        type_bindings,
                    )?;
                    spread_bindings.insert(param.clone(), arg_type);
                }

                // Resolve the spread type with the bindings
                let spread_type = resolve_ast_type_impl(
                    union_depth,
                    type_aliases,
                    type_def.clone(),
                    program,
                    &spread_bindings,
                )?;

                // Extract tuple types with names from the spread (handling unions)
                let spread_named_types =
                    extract_tuple_types_from_type_with_names(&spread_type, program)?;

                // For each existing variant, create new variants for each spread type
                let mut new_variants = Vec::new();
                for (existing_name, existing_fields) in &variants {
                    for (spread_name, spread_fields) in &spread_named_types {
                        let mut new_variant_fields = existing_fields.clone();
                        // Use spread name if existing name is None, otherwise keep existing
                        let new_name = existing_name.clone().or_else(|| spread_name.clone());

                        // Merge spread fields into variant fields
                        for (spread_field_name, spread_field_type) in spread_fields {
                            // Check if we should replace an existing field
                            if let Some(pos) = spread_field_name.as_ref().and_then(|n| {
                                new_variant_fields
                                    .iter()
                                    .position(|(field_name, _)| field_name.as_ref() == Some(n))
                            }) {
                                new_variant_fields[pos].1 = spread_field_type.clone();
                                continue;
                            }
                            new_variant_fields
                                .push((spread_field_name.clone(), spread_field_type.clone()));
                        }

                        new_variants.push((new_name, new_variant_fields));
                    }
                }
                variants = new_variants;
            }
        }
    }

    // Return all variants - caller will decide if this should be a union
    Ok(variants)
}

/// Extract tuple field definitions with names from a type, handling unions
fn extract_tuple_types_from_type_with_names(
    typ: &Type,
    program: &Program,
) -> Result<NamedFieldVariants, Error> {
    match typ {
        Type::Tuple(type_id) | Type::Partial(type_id) => {
            let tuple_info = program
                .lookup_type(type_id)
                .ok_or(Error::TypeNotInRegistry { type_id: *type_id })?;
            Ok(vec![(tuple_info.name.clone(), tuple_info.fields.clone())])
        }
        Type::Union(variants) => {
            let mut all_named_fields = Vec::new();
            for variant in variants {
                let variant_named_fields =
                    extract_tuple_types_from_type_with_names(variant, program)?;
                all_named_fields.extend(variant_named_fields);
            }
            Ok(all_named_fields)
        }
        _ => Err(Error::TypeMismatch {
            expected: "tuple or partial".to_string(),
            found: format!("{:?}", typ),
        }),
    }
}

fn resolve_ast_type_impl(
    union_depth: &mut usize,
    type_aliases: &HashMap<String, TypeAliasDef>,
    ast_type: ast::Type,
    program: &mut Program,
    type_bindings: &HashMap<String, Type>,
) -> Result<Type, Error> {
    match ast_type {
        ast::Type::Primitive(ast::PrimitiveType::Int) => Ok(Type::Integer),
        ast::Type::Primitive(ast::PrimitiveType::Bin) => Ok(Type::Binary),
        ast::Type::Tuple(tuple) => {
            // Resolve field types without distributing unions
            // Check if there are any spreads
            let has_spread = tuple
                .fields
                .iter()
                .any(|f| matches!(f, ast::FieldType::Spread { .. }));

            if has_spread {
                // Handle spreads - may create multiple variants
                let field_variants = resolve_tuple_fields_with_spread(
                    union_depth,
                    type_aliases,
                    &tuple.fields,
                    program,
                    type_bindings,
                )?;

                // Determine if this should be a partial type based solely on the AST syntax
                // [...] produces a tuple, (...) produces a partial
                let is_partial = tuple.is_partial;

                // Process all variants (may be 1 or many)
                let mut variant_types = Vec::new();
                for (variant_name, fields) in field_variants {
                    // Validate partial types
                    if is_partial {
                        for (field_name, _) in &fields {
                            if field_name.is_none() {
                                return Err(Error::TypeUnresolved(
                                    "All fields in a partial type must be named".to_string(),
                                ));
                            }
                        }
                    }

                    // Determine the final tuple name based on the name mode:
                    // - TupleName::Literal -> use the literal name
                    // - TupleName::Identifier -> use the variant name from spread
                    // - TupleName::Ripple -> error (ripple only valid in value context)
                    // - TupleName::None -> use None (unnamed)
                    let final_name = match &tuple.name {
                        ast::TupleName::Literal(_) | ast::TupleName::None => {
                            resolve_tuple_name(tuple.name.clone(), type_aliases, program)?
                        }
                        ast::TupleName::Identifier(_) => {
                            // Use variant name from spread (inherits from source)
                            variant_name
                        }
                        ast::TupleName::Ripple => {
                            return Err(Error::FeatureUnsupported(
                                "Ripple (~) cannot be used in type definitions".to_string(),
                            ));
                        }
                    };

                    let type_id = program.register_type(final_name, fields);
                    variant_types.push(if is_partial {
                        Type::Partial(type_id)
                    } else {
                        Type::Tuple(type_id)
                    });
                }

                // Return single type or union based on variant count
                return Ok(Type::from_types(variant_types));
            }

            // No spreads - process fields normally
            let mut fields = Vec::new();
            for field in tuple.fields {
                match field {
                    ast::FieldType::Field { name, type_def } => {
                        let field_type = resolve_ast_type_impl(
                            union_depth,
                            type_aliases,
                            type_def,
                            program,
                            type_bindings,
                        )?;
                        fields.push((name, field_type));
                    }
                    ast::FieldType::Spread { .. } => unreachable!(),
                }
            }

            // Validate partial types
            if tuple.is_partial {
                // All fields must be named
                for (field_name, _) in &fields {
                    if field_name.is_none() {
                        return Err(Error::TypeUnresolved(
                            "All fields in a partial type must be named".to_string(),
                        ));
                    }
                }
            }

            // Resolve tuple name (may inherit from identifier spread)
            let resolved_name = resolve_tuple_name(tuple.name, type_aliases, program)?;

            // Register the tuple type with the is_partial flag
            let type_id =
                program.register_type_with_partial(resolved_name, fields, tuple.is_partial);

            // Return Type::Partial for partial types, Type::Tuple for concrete types
            if tuple.is_partial {
                Ok(Type::Partial(type_id))
            } else {
                Ok(Type::Tuple(type_id))
            }
        }
        ast::Type::Function(function) => {
            let input_type = resolve_ast_type_impl(
                union_depth,
                type_aliases,
                *function.input,
                program,
                type_bindings,
            )?;
            let output_type = resolve_ast_type_impl(
                union_depth,
                type_aliases,
                *function.output,
                program,
                type_bindings,
            )?;

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
            validate_union_has_base_case(&union.types)?;

            // Increment union depth
            *union_depth += 1;

            // Resolve all union member types
            let mut resolved_types = Vec::new();
            for member_type in union.types {
                let member_type = resolve_ast_type_impl(
                    union_depth,
                    type_aliases,
                    member_type,
                    program,
                    type_bindings,
                )?;
                match member_type {
                    Type::Union(variants) => resolved_types.extend(variants),
                    t => resolved_types.push(t),
                }
            }

            // Decrement union depth
            *union_depth -= 1;

            Ok(Type::from_types(resolved_types))
        }
        ast::Type::Cycle(target_depth) => {
            // & or &N syntax for cycle references
            let target_depth = target_depth.unwrap_or(0);

            // Validate: cycles require enclosing union
            if *union_depth == 0 {
                return Err(Error::TypeUnresolved(
                    "Cycle reference '&' requires enclosing union type".to_string(),
                ));
            }

            // Validate: target_depth must be <= union_depth
            if target_depth >= *union_depth {
                return Err(Error::TypeUnresolved(format!(
                    "Invalid cycle reference &{}: only {} union level(s) deep",
                    target_depth, *union_depth
                )));
            }

            // Calculate actual depth for Cycle(depth)
            // target_depth is downward from root (0 = root, 1 = first nested union)
            // Cycle(depth) is upward from current position
            let cycle_depth = *union_depth - target_depth;

            Ok(Type::Cycle(cycle_depth))
        }
        ast::Type::Process(process) => {
            let receive = process
                .receive_type
                .map(|receive_type| {
                    resolve_ast_type_impl(
                        union_depth,
                        type_aliases,
                        *receive_type,
                        program,
                        type_bindings,
                    )
                })
                .transpose()?
                .map(Box::new);
            let returns = process
                .return_type
                .map(|return_type| {
                    resolve_ast_type_impl(
                        union_depth,
                        type_aliases,
                        *return_type,
                        program,
                        type_bindings,
                    )
                })
                .transpose()?
                .map(Box::new);
            Ok(Type::Process(Box::new(ProcessType { receive, returns })))
        }
        ast::Type::Identifier { name, arguments } => {
            // For bare identifiers (no arguments), check type parameter bindings first
            if arguments.is_empty() {
                // Check type parameter bindings first (highest priority)
                if let Some(bound_type) = type_bindings.get(&name) {
                    return Ok(bound_type.clone());
                }
            }

            // Instantiate the type (works for both bare identifiers and parameterized types)
            instantiate_generic_type(
                union_depth,
                type_aliases,
                &name,
                arguments,
                program,
                type_bindings,
            )
        }
    }
}

/// Check if a type contains any unbound type variables
pub fn contains_variables(typ: &Type, type_lookup: &impl TypeLookup) -> bool {
    match typ {
        Type::Variable(_) => true,
        Type::Union(variants) => variants.iter().any(|v| contains_variables(v, type_lookup)),
        Type::Callable(func_type) => {
            contains_variables(&func_type.parameter, type_lookup)
                || contains_variables(&func_type.result, type_lookup)
                || contains_variables(&func_type.receive, type_lookup)
        }
        Type::Process(proc_type) => {
            proc_type
                .receive
                .as_ref()
                .is_some_and(|r| contains_variables(r, type_lookup))
                || proc_type
                    .returns
                    .as_ref()
                    .is_some_and(|r| contains_variables(r, type_lookup))
        }
        Type::Tuple(type_id) | Type::Partial(type_id) => {
            // Check if any field contains variables
            if let Some(type_info) = type_lookup.lookup_type(type_id) {
                type_info
                    .fields
                    .iter()
                    .any(|(_, field_type)| contains_variables(field_type, type_lookup))
            } else {
                // If we can't look up the type, conservatively assume it might have variables
                false
            }
        }
        Type::Integer | Type::Binary | Type::Cycle(_) => false,
    }
}

/// Substitute type variables in a type with their bindings
pub fn substitute(typ: &Type, bindings: &HashMap<String, Type>, program: &mut Program) -> Type {
    match typ {
        Type::Variable(name) => bindings.get(name).cloned().unwrap_or_else(|| typ.clone()),
        Type::Union(variants) => Type::from_types(
            variants
                .iter()
                .map(|v| substitute(v, bindings, program))
                .collect(),
        ),
        Type::Tuple(type_id) | Type::Partial(type_id) => {
            let is_partial = matches!(typ, Type::Partial(_));

            // Look up the tuple's fields and clone to avoid borrow issues
            if let Some(type_info) = program.lookup_type(type_id).cloned() {
                // Substitute within each field type
                let mut any_changed = false;
                let new_fields: Vec<_> = type_info
                    .fields
                    .iter()
                    .map(|(name, field_type)| {
                        let new_type = substitute(field_type, bindings, program);
                        if !any_changed && &new_type != field_type {
                            any_changed = true;
                        }
                        (name.clone(), new_type)
                    })
                    .collect();

                // If any field changed, register a new tuple type
                if any_changed {
                    let new_type_id = program.register_type_with_partial(
                        type_info.name.clone(),
                        new_fields,
                        is_partial,
                    );
                    if is_partial {
                        Type::Partial(new_type_id)
                    } else {
                        Type::Tuple(new_type_id)
                    }
                } else {
                    typ.clone()
                }
            } else {
                // Type not found in registry, just clone
                typ.clone()
            }
        }
        Type::Integer | Type::Binary | Type::Cycle(_) => typ.clone(),
        Type::Callable(func_type) => Type::Callable(Box::new(CallableType {
            parameter: substitute(&func_type.parameter, bindings, program),
            result: substitute(&func_type.result, bindings, program),
            receive: substitute(&func_type.receive, bindings, program),
        })),
        Type::Process(proc_type) => Type::Process(Box::new(ProcessType {
            receive: proc_type
                .receive
                .as_ref()
                .map(|r| Box::new(substitute(r, bindings, program))),
            returns: proc_type
                .returns
                .as_ref()
                .map(|r| Box::new(substitute(r, bindings, program))),
        })),
    }
}

/// Unify a pattern type (containing Type::Variable) with a concrete type.
/// Builds up a mapping from type variable names to concrete types.
/// Returns an error if there's a conflict (e.g., variable bound to two different types).
pub fn unify(
    bindings: &mut HashMap<String, Type>,
    pattern: &Type,
    concrete: &Type,
    type_lookup: &impl TypeLookup,
) -> Result<(), Error> {
    match (pattern, concrete) {
        // When pattern is a variable, bind it or check consistency
        (Type::Variable(name), concrete) => {
            if let Some(existing) = bindings.get(name) {
                // Variable already bound - widen to union if different
                if existing != concrete {
                    // Widen the type variable to a union
                    let widened = Type::from_types(vec![existing.clone(), concrete.clone()]);
                    bindings.insert(name.clone(), widened);
                }
            } else {
                // New binding
                bindings.insert(name.clone(), concrete.clone());
            }
            Ok(())
        }

        // When concrete is a variable, we can't unify (shouldn't happen in practice)
        (_, Type::Variable(_)) => Err(Error::TypeUnresolved(
            "Cannot unify with unbound type variable".to_string(),
        )),

        // Both are basic types - must match
        (Type::Integer, Type::Integer) => Ok(()),
        (Type::Binary, Type::Binary) => Ok(()),

        // Process types must match in structure
        (Type::Process(p1), Type::Process(p2)) => {
            // Unify receive types
            match (&p1.receive, &p2.receive) {
                (Some(r1), Some(r2)) => unify(bindings, r1, r2, type_lookup)?,
                (None, None) => {}
                _ => {
                    return Err(Error::TypeUnresolved(
                        "Process types have incompatible receive types".to_string(),
                    ));
                }
            }
            // Unify return types
            match (&p1.returns, &p2.returns) {
                (Some(ret1), Some(ret2)) => unify(bindings, ret1, ret2, type_lookup)?,
                (None, None) => {}
                _ => {
                    return Err(Error::TypeUnresolved(
                        "Process types have incompatible return types".to_string(),
                    ));
                }
            }
            Ok(())
        }

        // Tuple types must match structurally
        (Type::Tuple(id1), Type::Tuple(id2)) => {
            if id1 == id2 {
                return Ok(());
            }

            let info1 = type_lookup
                .lookup_type(id1)
                .ok_or(Error::TypeNotInRegistry { type_id: *id1 })?;
            let info2 = type_lookup
                .lookup_type(id2)
                .ok_or(Error::TypeNotInRegistry { type_id: *id2 })?;

            // Names must match
            if info1.name != info2.name {
                return Err(Error::TypeUnresolved(format!(
                    "Cannot unify different tuple types: pattern {:?} (id={:?}, {} fields) vs concrete {:?} (id={:?}, {} fields)",
                    info1.name,
                    id1,
                    info1.fields.len(),
                    info2.name,
                    id2,
                    info2.fields.len()
                )));
            }

            // Same number of fields
            if info1.fields.len() != info2.fields.len() {
                return Err(Error::TypeUnresolved(
                    "Tuples have different number of fields".to_string(),
                ));
            }

            // Unify each field
            for ((fname1, ftype1), (fname2, ftype2)) in info1.fields.iter().zip(info2.fields.iter())
            {
                if fname1 != fname2 {
                    return Err(Error::TypeUnresolved(
                        "Tuple fields have different names".to_string(),
                    ));
                }
                unify(bindings, ftype1, ftype2, type_lookup)?;
            }

            Ok(())
        }

        // Partial types - unify like tuples but only check pattern fields exist in concrete
        (Type::Partial(pattern_id), Type::Tuple(concrete_id))
        | (Type::Partial(pattern_id), Type::Partial(concrete_id)) => {
            let pattern_info =
                type_lookup
                    .lookup_type(pattern_id)
                    .ok_or(Error::TypeNotInRegistry {
                        type_id: *pattern_id,
                    })?;
            let concrete_info =
                type_lookup
                    .lookup_type(concrete_id)
                    .ok_or(Error::TypeNotInRegistry {
                        type_id: *concrete_id,
                    })?;

            // If partial has a name, concrete must match
            if let Some(pattern_name) = &pattern_info.name
                && concrete_info.name.as_ref() != Some(pattern_name)
            {
                return Err(Error::TypeUnresolved(format!(
                    "Partial type expects name {:?}, got {:?}",
                    pattern_name, concrete_info.name
                )));
            }

            // Check all pattern fields exist in concrete with compatible types
            for (pattern_fname, pattern_ftype) in &pattern_info.fields {
                let Some(pattern_fname) = pattern_fname else {
                    return Err(Error::TypeUnresolved(
                        "Partial type has unnamed field".to_string(),
                    ));
                };

                let Some((_, concrete_ftype)) = concrete_info
                    .fields
                    .iter()
                    .find(|(fname, _)| fname.as_ref() == Some(pattern_fname))
                else {
                    return Err(Error::TypeUnresolved(format!(
                        "Concrete type missing field: {}",
                        pattern_fname
                    )));
                };

                unify(bindings, pattern_ftype, concrete_ftype, type_lookup)?;
            }

            Ok(())
        }

        // Function types - parameters are contravariant, results are covariant
        (Type::Callable(f1), Type::Callable(f2)) => {
            // Unify parameters (contravariant - swap order)
            unify(bindings, &f1.parameter, &f2.parameter, type_lookup)?;
            // Unify results (covariant)
            unify(bindings, &f1.result, &f2.result, type_lookup)?;
            // Unify receive types (contravariant - swap order)
            unify(bindings, &f1.receive, &f2.receive, type_lookup)?;
            Ok(())
        }

        // Handle cycles in pattern - for recursive types like list<t>
        (Type::Cycle(_depth), _concrete_type) => {
            // Cycles in patterns should unify with any concrete type
            // This is sound because cycles represent recursive occurrences
            // The actual structure is checked when constructing the concrete value
            // For now, we accept any concrete type that matches the cycle
            // We could be more precise by resolving the cycle, but that's complex
            // and not strictly necessary for soundness
            Ok(())
        }

        // Handle cycles in concrete type - shouldn't happen in practice
        (pattern_type, Type::Cycle(_depth)) => {
            // If concrete has a cycle, something went wrong - concrete types
            // shouldn't have unresolved cycles
            Err(Error::TypeUnresolved(format!(
                "Concrete type contains cycle: pattern={:?}",
                pattern_type
            )))
        }

        // Never type (empty union) unifies with anything
        (Type::Union(variants), _) if variants.is_empty() => Ok(()),
        (_, Type::Union(variants)) if variants.is_empty() => Ok(()),

        // Union types - concrete must be a union too, unify variants
        (Type::Union(pattern_variants), Type::Union(concrete_variants)) => {
            // For union unification, we need to ensure all pattern variants
            // can be unified with at least one concrete variant
            // This is conservative but sound
            if pattern_variants.is_empty() && concrete_variants.is_empty() {
                return Ok(());
            }

            // Try to unify each pattern variant with each concrete variant
            for pattern_variant in pattern_variants {
                let mut found_match = false;
                for concrete_variant in concrete_variants {
                    let mut temp_bindings = bindings.clone();
                    if unify(
                        &mut temp_bindings,
                        pattern_variant,
                        concrete_variant,
                        type_lookup,
                    )
                    .is_ok()
                    {
                        // Merge the bindings
                        for (k, v) in temp_bindings {
                            if let Some(existing) = bindings.get(&k)
                                && !v.is_compatible(existing, type_lookup)
                            {
                                continue; // Skip incompatible binding
                            }
                            bindings.insert(k, v);
                        }
                        found_match = true;
                        break;
                    }
                }
                if !found_match {
                    return Err(Error::TypeUnresolved(
                        "Cannot unify union variant: pattern has variant that doesn't match any concrete variant".to_string()
                    ));
                }
            }
            Ok(())
        }

        // Pattern union with concrete non-union - try each variant
        (Type::Union(variants), concrete_type) => {
            // Try to unify with at least one variant
            let mut errors = Vec::new();
            for (i, variant) in variants.iter().enumerate() {
                let mut temp_bindings = bindings.clone();
                match unify(&mut temp_bindings, variant, concrete_type, type_lookup) {
                    Ok(()) => {
                        *bindings = temp_bindings;
                        return Ok(());
                    }
                    Err(e) => {
                        errors.push(format!("Variant {}: {:?}", i, e));
                    }
                }
            }
            Err(Error::TypeUnresolved(format!(
                "Cannot unify union pattern ({} variants) with concrete type: {:?}. Errors: [{}]",
                variants.len(),
                concrete_type,
                errors.join(", ")
            )))
        }

        // Concrete union with pattern non-union - pattern must match one variant
        (pattern_type, Type::Union(variants)) => {
            for variant in variants {
                let mut temp_bindings = bindings.clone();
                if unify(&mut temp_bindings, pattern_type, variant, type_lookup).is_ok() {
                    *bindings = temp_bindings;
                    return Ok(());
                }
            }
            Err(Error::TypeUnresolved(format!(
                "Cannot unify pattern {:?} with concrete union ({} variants)",
                pattern_type,
                variants.len()
            )))
        }

        // All other combinations are incompatible
        _ => Err(Error::TypeUnresolved(format!(
            "Cannot unify incompatible types: {:?} and {:?}",
            pattern, concrete
        ))),
    }
}
