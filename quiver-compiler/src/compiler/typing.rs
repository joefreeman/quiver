use std::collections::HashMap;

use crate::ast;
use crate::resolver::{ModuleResolver, PackageId};
use quiver_core::{
    program::Program,
    types::{Type, TypeLookup},
};

use super::modules::{self, ModuleCache};
use super::{Error, Scope, scopes};

/// Capabilities for resolving `'%mod` / `'%mod.name` module-type references: the package
/// to resolve module paths against, plus the resolver and module cache used to load and
/// build the target module's type namespace. Threaded through type resolution because a
/// module type can appear anywhere a type can.
pub struct TypeEnv<'a> {
    pub resolver: &'a dyn ModuleResolver,
    pub module_cache: &'a mut ModuleCache,
    pub package: &'a PackageId,
}

#[derive(Debug, Clone)]
pub enum TupleAccessor {
    Field(String),
    Position(usize),
}

/// Create a union type from a list of type IDs
pub fn union_type_ids(program: &mut Program, type_ids: Vec<usize>) -> usize {
    // Flatten any unions and deduplicate
    let mut flattened = Vec::new();
    for type_id in type_ids {
        if let Some(Type::Union(variants)) = program.lookup_type(type_id) {
            flattened.extend(variants.iter().cloned());
        } else {
            flattened.push(type_id);
        }
    }

    // Deduplicate
    let mut seen = std::collections::HashSet::new();
    let unique: Vec<usize> = flattened
        .into_iter()
        .filter(|id| seen.insert(*id))
        .collect();

    match unique.len() {
        0 => program.never(),
        1 => unique[0],
        _ => program.register_type(Type::Union(unique)),
    }
}

/// Type alias definition - a pre-resolved type ID with type parameters.
/// The type_id may contain Type::Variable for generic parameters.
#[derive(Debug, Clone)]
pub struct TypeAliasDef {
    pub parameters: Vec<String>,
    pub type_id: usize,
}

/// Scope key under which a module's nameless default type (`' = ...`) is stored, so that a
/// bare `'` (`ast::Type::SelfDefault`) can resolve to it. A lone `'` is never a valid user
/// type-alias name, so this key cannot collide with one.
pub const SELF_DEFAULT_KEY: &str = "'";

fn instantiate_generic_type(
    recursion_depth: &mut usize,
    env: &mut TypeEnv,
    scopes_ref: &[Scope],
    name: &str,
    arguments: Vec<ast::Type>,
    program: &mut Program,
    type_bindings: &HashMap<String, usize>,
) -> Result<usize, Error> {
    // Look up type alias definition
    let type_def = scopes::lookup_type_alias(scopes_ref, name)
        .ok_or_else(|| Error::TypeAliasMissing(name.to_string()))?;

    instantiate_alias_def(
        recursion_depth,
        env,
        scopes_ref,
        name,
        &type_def,
        arguments,
        program,
        type_bindings,
    )
}

/// Instantiate a (possibly parameterised) type alias definition with the given type
/// arguments, substituting its `Type::Variable` placeholders. Shared between named alias
/// references and module-type references.
#[allow(clippy::too_many_arguments)]
fn instantiate_alias_def(
    recursion_depth: &mut usize,
    env: &mut TypeEnv,
    scopes_ref: &[Scope],
    name: &str,
    type_def: &TypeAliasDef,
    arguments: Vec<ast::Type>,
    program: &mut Program,
    type_bindings: &HashMap<String, usize>,
) -> Result<usize, Error> {
    // Resolve all type arguments
    let resolved_args: Vec<usize> = arguments
        .into_iter()
        .map(|arg| {
            resolve_ast_type_impl(
                recursion_depth,
                env,
                scopes_ref,
                arg,
                program,
                type_bindings,
            )
        })
        .collect::<Result<_, _>>()?;

    // Validate argument count
    if resolved_args.len() != type_def.parameters.len() {
        return Err(Error::TypeUnresolved(format!(
            "Generic type '{}' expects {} type argument(s), got {}",
            name,
            type_def.parameters.len(),
            resolved_args.len()
        )));
    }

    // Build type bindings for substitution
    let mut new_bindings = HashMap::new();
    for (param, arg) in type_def.parameters.iter().zip(resolved_args.iter()) {
        new_bindings.insert(param.clone(), *arg);
    }

    // Substitute Type::Variable placeholders with concrete types
    Ok(substitute(type_def.type_id, &new_bindings, program))
}

/// Check if an AST type contains any cycle references
fn ast_contains_cycle(typ: &ast::Type) -> bool {
    match typ {
        ast::Type::Cycle(_) => true,
        ast::Type::Union(union) => union.types.iter().any(ast_contains_cycle),
        ast::Type::Intersection(members) => members.iter().any(ast_contains_cycle),
        ast::Type::Tuple(tuple) => tuple.fields.iter().any(|f| match f {
            ast::FieldType::Field { type_def, .. } => ast_contains_cycle(type_def),
            ast::FieldType::Spread { .. } => false,
        }),
        ast::Type::Identifier { arguments, .. } => arguments.iter().any(ast_contains_cycle),
        ast::Type::ModuleType { arguments, .. } => arguments.iter().any(ast_contains_cycle),
        ast::Type::SelfDefault { arguments } => arguments.iter().any(ast_contains_cycle),
        // Function and process types are boundaries - cycles inside them don't count
        // as structural recursion
        ast::Type::Function(_) | ast::Type::Process(_) => false,
        ast::Type::Primitive(_) | ast::Type::Resource(_) => false,
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
    env: &mut TypeEnv,
    scopes_ref: &[Scope],
    ast_type: ast::Type,
    program: &mut Program,
) -> Result<usize, Error> {
    let bindings = HashMap::new();
    let mut recursion_depth = 0;
    resolve_ast_type_impl(
        &mut recursion_depth,
        env,
        scopes_ref,
        ast_type,
        program,
        &bindings,
    )
}

/// Resolve an AST type with pre-defined type variable bindings.
/// Used when importing types from modules where type parameters should be
/// resolved as Type::Variable.
pub fn resolve_ast_type_with_bindings(
    env: &mut TypeEnv,
    scopes_ref: &[Scope],
    ast_type: ast::Type,
    program: &mut Program,
    bindings: &HashMap<String, usize>,
) -> Result<usize, Error> {
    let mut recursion_depth = 0;
    resolve_ast_type_impl(
        &mut recursion_depth,
        env,
        scopes_ref,
        ast_type,
        program,
        bindings,
    )
}

/// Resolve a function parameter type with explicitly declared type parameters.
/// Type parameters are resolved to Type::Variable, while undefined types cause errors.
pub fn resolve_function_parameter_type(
    env: &mut TypeEnv,
    scopes_ref: &[Scope],
    ast_type: ast::Type,
    type_parameters: &[String],
    program: &mut Program,
) -> Result<usize, Error> {
    // Create bindings for declared type parameters, mapping each to a Type::Variable
    let mut bindings = HashMap::new();
    for param in type_parameters {
        let var_type_id = program.register_type(Type::Variable(param.clone()));
        bindings.insert(param.clone(), var_type_id);
    }

    // Start at depth 1 since this is a function parameter (the function creates a recursion boundary)
    // This allows the parameter type to use & to refer to the enclosing function
    let mut recursion_depth = 1;
    resolve_ast_type_impl(
        &mut recursion_depth,
        env,
        scopes_ref,
        ast_type,
        program,
        &bindings,
    )
}

/// Resolve a type alias for display purposes (e.g., in tests or REPL).
/// Returns the type ID which already has Type::Variable placeholders for type parameters.
pub fn resolve_type_alias_for_display(
    scopes_ref: &[Scope],
    alias_name: &str,
) -> Result<usize, Error> {
    let type_def = scopes::lookup_type_alias(scopes_ref, alias_name)
        .ok_or_else(|| Error::TypeAliasMissing(alias_name.to_string()))?;

    Ok(type_def.type_id)
}

/// Resolve tuple name - distinguishes between literal names (capitalized) and type aliases (lowercase)
fn resolve_tuple_name(
    name: Option<String>,
    scopes_ref: &[Scope],
    program: &Program,
) -> Result<Option<String>, Error> {
    match name {
        None => Ok(None),
        Some(s) if s.chars().next().is_some_and(|c| c.is_ascii_uppercase()) => {
            // Capitalized: literal tuple name like "Point"
            Ok(Some(s))
        }
        Some(identifier) => {
            // Lowercase: type alias reference like "event[..., x: int]"
            let type_alias = scopes::lookup_type_alias(scopes_ref, &identifier)
                .ok_or_else(|| Error::TypeAliasMissing(identifier.to_string()))?;

            // Type parameters must be empty for name inheritance (for now)
            if !type_alias.parameters.is_empty() {
                return Err(Error::FeatureUnsupported(format!(
                    "Type alias '{}' has type parameters - name inheritance from parameterized types requires explicit instantiation",
                    identifier
                )));
            }

            // Look up the resolved type to get the tuple name
            match program.lookup_type(type_alias.type_id) {
                Some(Type::Tuple(tuple_id)) => {
                    if let Some(info) = program.lookup_tuple(*tuple_id) {
                        Ok(info.name.clone())
                    } else {
                        Ok(None)
                    }
                }
                Some(Type::Union(_)) => {
                    // For unions, don't try to extract a single name
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

type FieldSet = Vec<(Option<String>, usize)>;
type NamedFieldVariants = Vec<(Option<String>, FieldSet)>; // (tuple_name, fields)

/// Resolve tuple fields that contain spreads
/// Returns a vector of (name, field_set) pairs - multiple pairs if spreading creates union variants
fn resolve_tuple_fields_with_spread(
    recursion_depth: &mut usize,
    env: &mut TypeEnv,
    scopes_ref: &[Scope],
    fields: &[ast::FieldType],
    program: &mut Program,
    type_bindings: &HashMap<String, usize>,
) -> Result<NamedFieldVariants, Error> {
    // Track all possible field combinations with their names (for union distribution)
    // Each variant is (tuple_name, fields)
    let mut variants: NamedFieldVariants = vec![(None, Vec::new())];

    // Process fields left to right
    for field in fields {
        match field {
            ast::FieldType::Field { name, type_def } => {
                // Resolve the field type
                let field_type_id = resolve_ast_type_impl(
                    recursion_depth,
                    env,
                    scopes_ref,
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
                        variant_fields[pos].1 = field_type_id;
                        continue;
                    }
                    variant_fields.push((name.clone(), field_type_id));
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
                let type_alias = scopes::lookup_type_alias(scopes_ref, spread_id)
                    .ok_or_else(|| Error::TypeAliasMissing(spread_id.clone()))?;

                // Check type parameter count matches
                if type_alias.parameters.len() != type_arguments.len() {
                    return Err(Error::TypeUnresolved(format!(
                        "Type alias '{}' expects {} type parameters, got {}",
                        spread_id,
                        type_alias.parameters.len(),
                        type_arguments.len()
                    )));
                }

                // Create type bindings for the spread type
                let mut spread_bindings = HashMap::new();
                for (param, arg) in type_alias.parameters.iter().zip(type_arguments.iter()) {
                    let arg_type_id = resolve_ast_type_impl(
                        recursion_depth,
                        env,
                        scopes_ref,
                        arg.clone(),
                        program,
                        type_bindings,
                    )?;
                    spread_bindings.insert(param.clone(), arg_type_id);
                }

                // Substitute type variables in the resolved type
                let spread_type_id = substitute(type_alias.type_id, &spread_bindings, program);

                // Extract tuple types with names from the spread (handling unions)
                let spread_named_types =
                    extract_tuples_from_type_with_names(spread_type_id, program)?;

                // For each existing variant, create new variants for each spread type
                let mut new_variants = Vec::new();
                for (existing_name, existing_fields) in &variants {
                    for (spread_name, spread_fields) in &spread_named_types {
                        let mut new_variant_fields = existing_fields.clone();
                        // Use spread name if existing name is None, otherwise keep existing
                        let new_name = existing_name.clone().or_else(|| spread_name.clone());

                        // Merge spread fields into variant fields
                        for (spread_field_name, spread_field_type_id) in spread_fields {
                            // Check if we should replace an existing field
                            if let Some(pos) = spread_field_name.as_ref().and_then(|n| {
                                new_variant_fields
                                    .iter()
                                    .position(|(field_name, _)| field_name.as_ref() == Some(n))
                            }) {
                                new_variant_fields[pos].1 = *spread_field_type_id;
                                continue;
                            }
                            new_variant_fields
                                .push((spread_field_name.clone(), *spread_field_type_id));
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
fn extract_tuples_from_type_with_names(
    type_id: usize,
    program: &Program,
) -> Result<NamedFieldVariants, Error> {
    let Some(typ) = program.lookup_type(type_id) else {
        return Err(Error::TypeUnresolved("Type not found".to_string()));
    };

    match typ {
        Type::Tuple(tuple_id) => {
            let tuple_id = *tuple_id;
            let tuple_info = program
                .lookup_tuple(tuple_id)
                .ok_or(Error::TupleNotInRegistry { tuple_id })?;
            Ok(vec![(tuple_info.name.clone(), tuple_info.fields.clone())])
        }
        Type::Partial { name, fields } => {
            // Convert partial fields (all named) to tuple field format
            let tuple_fields: Vec<(Option<String>, usize)> = fields
                .iter()
                .map(|(fname, ftype)| (Some(fname.clone()), *ftype))
                .collect();
            Ok(vec![(name.clone(), tuple_fields)])
        }
        Type::Union(variants) => {
            let variants = variants.clone();
            let mut all_named_fields = Vec::new();
            for variant_id in variants {
                let variant_named_fields =
                    extract_tuples_from_type_with_names(variant_id, program)?;
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
    recursion_depth: &mut usize,
    env: &mut TypeEnv,
    scopes_ref: &[Scope],
    ast_type: ast::Type,
    program: &mut Program,
    type_bindings: &HashMap<String, usize>,
) -> Result<usize, Error> {
    match ast_type {
        ast::Type::Primitive(ast::PrimitiveType::Int) => Ok(program.register_type(Type::Integer)),
        ast::Type::Primitive(ast::PrimitiveType::Bin) => Ok(program.register_type(Type::Binary)),
        ast::Type::Primitive(ast::PrimitiveType::Ref) => Ok(program.register_type(Type::Reference)),
        ast::Type::Resource(name) => Ok(program.register_type(Type::Resource(name))),
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
                    recursion_depth,
                    env,
                    scopes_ref,
                    &tuple.fields,
                    program,
                    type_bindings,
                )?;

                // Determine if this should be a partial type based solely on the AST syntax
                // [...] produces a tuple, (...) produces a partial
                let is_partial = tuple.is_partial;

                // Process all variants (may be 1 or many)
                let mut variant_type_ids = Vec::new();
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

                    // Determine the final tuple name:
                    // - None or capitalized name -> resolve directly
                    // - Lowercase name (type alias) -> use variant name from spread
                    let is_type_alias = tuple
                        .name
                        .as_ref()
                        .is_some_and(|n| n.chars().next().is_some_and(|c| c.is_ascii_lowercase()));
                    let final_name = if is_type_alias {
                        // Use variant name from spread (inherits from source)
                        variant_name
                    } else {
                        resolve_tuple_name(tuple.name.clone(), scopes_ref, program)?
                    };

                    let type_id = if is_partial {
                        // Create inline partial type (not in tuples registry)
                        let partial_fields: Vec<(String, usize)> = fields
                            .into_iter()
                            .map(|(name, type_id)| {
                                (name.expect("Partial fields must be named"), type_id)
                            })
                            .collect();
                        program.register_type(Type::Partial {
                            name: final_name,
                            fields: partial_fields,
                        })
                    } else {
                        let tuple_id = program.register_tuple(final_name, fields);
                        program.register_type(Type::Tuple(tuple_id))
                    };
                    variant_type_ids.push(type_id);
                }

                // Return single type or union based on variant count
                return Ok(union_type_ids(program, variant_type_ids));
            }

            // No spreads - process fields normally
            let mut fields = Vec::new();
            for field in tuple.fields {
                match field {
                    ast::FieldType::Field { name, type_def } => {
                        let field_type_id = resolve_ast_type_impl(
                            recursion_depth,
                            env,
                            scopes_ref,
                            type_def,
                            program,
                            type_bindings,
                        )?;
                        fields.push((name, field_type_id));
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
            let resolved_name = resolve_tuple_name(tuple.name, scopes_ref, program)?;

            // Return Type::Partial for partial types, Type::Tuple for concrete types
            if tuple.is_partial {
                // Create inline partial type (not in tuples registry)
                let partial_fields: Vec<(String, usize)> = fields
                    .into_iter()
                    .map(|(name, type_id)| (name.expect("Partial fields must be named"), type_id))
                    .collect();
                Ok(program.register_type(Type::Partial {
                    name: resolved_name,
                    fields: partial_fields,
                }))
            } else {
                let tuple_id = program.register_tuple(resolved_name, fields);
                Ok(program.register_type(Type::Tuple(tuple_id)))
            }
        }
        ast::Type::Function(function) => {
            // Increment recursion depth for function boundary
            *recursion_depth += 1;

            let input_type_id = resolve_ast_type_impl(
                recursion_depth,
                env,
                scopes_ref,
                *function.input,
                program,
                type_bindings,
            )?;
            let output_type_id = resolve_ast_type_impl(
                recursion_depth,
                env,
                scopes_ref,
                *function.output,
                program,
                type_bindings,
            )?;

            // Decrement recursion depth
            *recursion_depth -= 1;

            // Create function type without distributing unions
            let never_id = program.never();
            Ok(program.register_type(Type::Callable {
                parameter: input_type_id,
                result: output_type_id,
                receive: never_id,
            }))
        }
        ast::Type::Union(union) => {
            // Validate that union has at least one base case before resolution
            if union.types.is_empty() {
                return Err(Error::TypeUnresolved("Empty union type".to_string()));
            }
            validate_union_has_base_case(&union.types)?;

            // Increment recursion depth for union boundary
            *recursion_depth += 1;

            // Resolve all union member types
            let mut resolved_type_ids = Vec::new();
            for member_type in union.types {
                let member_type_id = resolve_ast_type_impl(
                    recursion_depth,
                    env,
                    scopes_ref,
                    member_type,
                    program,
                    type_bindings,
                )?;
                // Flatten nested unions
                if let Some(Type::Union(variants)) = program.lookup_type(member_type_id) {
                    resolved_type_ids.extend(variants.iter().cloned());
                } else {
                    resolved_type_ids.push(member_type_id);
                }
            }

            // Decrement recursion depth
            *recursion_depth -= 1;

            Ok(union_type_ids(program, resolved_type_ids))
        }
        ast::Type::Intersection(members) => {
            if members.is_empty() {
                return Err(Error::TypeUnresolved("Empty intersection type".to_string()));
            }
            // Resolve each member and fold with `intersect_types`: the result is the most specific
            // type satisfying all members (`never` when they're disjoint, e.g. `'int & 'bin`).
            let mut resolved: Option<usize> = None;
            for member_type in members {
                let member_id = resolve_ast_type_impl(
                    recursion_depth,
                    env,
                    scopes_ref,
                    member_type,
                    program,
                    type_bindings,
                )?;
                resolved = Some(match resolved {
                    None => member_id,
                    Some(acc) => super::narrowing::intersect_types(acc, member_id, program),
                });
            }
            Ok(resolved.expect("intersection has at least one member"))
        }
        ast::Type::Cycle(target_depth) => {
            // ^ or ^N syntax for cycle references
            let target_depth = target_depth.unwrap_or(0);

            // Validate: cycles require enclosing union or function
            if *recursion_depth == 0 {
                return Err(Error::TypeUnresolved(
                    "Cycle reference '^' requires enclosing union or function type".to_string(),
                ));
            }

            // Validate: target_depth must be < recursion_depth
            if target_depth >= *recursion_depth {
                return Err(Error::TypeUnresolved(format!(
                    "Invalid cycle reference ^{}: only {} recursion level(s) deep",
                    target_depth, *recursion_depth
                )));
            }

            // Calculate actual depth for Cycle(depth)
            // target_depth is downward from root (0 = root, 1 = first nested boundary)
            // Cycle(depth) is upward from current position
            let cycle_depth = *recursion_depth - target_depth;

            Ok(program.register_type(Type::Cycle(cycle_depth)))
        }
        ast::Type::Process(process) => {
            let receive_id = process
                .receive_type
                .map(|receive_type| {
                    resolve_ast_type_impl(
                        recursion_depth,
                        env,
                        scopes_ref,
                        *receive_type,
                        program,
                        type_bindings,
                    )
                })
                .transpose()?;
            let returns_id = process
                .return_type
                .map(|return_type| {
                    resolve_ast_type_impl(
                        recursion_depth,
                        env,
                        scopes_ref,
                        *return_type,
                        program,
                        type_bindings,
                    )
                })
                .transpose()?;
            Ok(program.register_type(Type::Process {
                send: receive_id,
                receive: returns_id,
            }))
        }
        ast::Type::Identifier { name, arguments } => {
            // For bare identifiers (no arguments), check type parameter bindings first
            if arguments.is_empty() {
                // Check type parameter bindings first (highest priority)
                if let Some(&bound_type_id) = type_bindings.get(&name) {
                    return Ok(bound_type_id);
                }
            }

            // Instantiate the type (works for both bare identifiers and parameterized types)
            instantiate_generic_type(
                recursion_depth,
                env,
                scopes_ref,
                &name,
                arguments,
                program,
                type_bindings,
            )
        }
        ast::Type::ModuleType {
            module,
            member,
            arguments,
        } => {
            // Build the target module's type namespace and select the default type
            // (`'%mod`) or a named one (`'%mod.name`).
            let namespace = modules::module_type_namespace(
                &module,
                env.resolver,
                env.module_cache,
                env.package,
                program,
            )?;
            let display = display_module_type(&module, member.as_deref());
            let type_def =
                match &member {
                    None => namespace.default.ok_or_else(|| Error::ModuleTypeMissing {
                        type_name: display.clone(),
                        module: module.join("/"),
                    })?,
                    Some(name) => namespace.named.get(name).cloned().ok_or_else(|| {
                        Error::ModuleTypeMissing {
                            type_name: name.clone(),
                            module: module.join("/"),
                        }
                    })?,
                };

            instantiate_alias_def(
                recursion_depth,
                env,
                scopes_ref,
                &display,
                &type_def,
                arguments,
                program,
                type_bindings,
            )
        }
        ast::Type::SelfDefault { arguments } => {
            // Bare `'`: the enclosing module's own default type, stored in scope under the
            // reserved key when its `' = ...` marker was compiled.
            let type_def =
                scopes::lookup_type_alias(scopes_ref, SELF_DEFAULT_KEY).ok_or_else(|| {
                    Error::TypeUnresolved(
                        "`'` refers to the module's default type, but this module defines none"
                            .to_string(),
                    )
                })?;
            instantiate_alias_def(
                recursion_depth,
                env,
                scopes_ref,
                "'",
                &type_def,
                arguments,
                program,
                type_bindings,
            )
        }
    }
}

/// Human-readable form of a module type reference, e.g. `'%mathx/vec` or `'%shapes.circle`.
fn display_module_type(module: &[String], member: Option<&str>) -> String {
    let path = module.join("/");
    match member {
        Some(name) => format!("'%{path}.{name}"),
        None => format!("'%{path}"),
    }
}

/// Check if a type contains any unbound type variables
pub fn contains_variables(type_id: usize, lookup: &impl TypeLookup) -> bool {
    let Some(typ) = lookup.lookup_type(type_id) else {
        return false;
    };

    match typ {
        Type::Variable(_) => true,
        Type::Union(variants) => {
            let variants = variants.clone();
            variants.iter().any(|&v| contains_variables(v, lookup))
        }
        Type::Callable {
            parameter,
            result,
            receive,
        } => {
            contains_variables(*parameter, lookup)
                || contains_variables(*result, lookup)
                || contains_variables(*receive, lookup)
        }
        Type::Process { send, receive } => {
            send.is_some_and(|s| contains_variables(s, lookup))
                || receive.is_some_and(|r| contains_variables(r, lookup))
        }
        Type::Tuple(tuple_id) => {
            // Check if any field contains variables
            if let Some(type_info) = lookup.lookup_tuple(*tuple_id) {
                type_info
                    .fields
                    .iter()
                    .any(|(_, field_type_id)| contains_variables(*field_type_id, lookup))
            } else {
                // If we can't look up the type, conservatively assume it might have variables
                false
            }
        }
        Type::Partial { fields, .. } => {
            // Check if any field in the partial contains variables
            fields
                .iter()
                .any(|(_, field_type_id)| contains_variables(*field_type_id, lookup))
        }
        Type::Integer | Type::Binary | Type::Reference | Type::Cycle(_) | Type::Resource(_) => {
            false
        }
    }
}

/// Substitute type variables in a type with their bindings
pub fn substitute(
    type_id: usize,
    bindings: &HashMap<String, usize>,
    program: &mut Program,
) -> usize {
    let Some(typ) = program.lookup_type(type_id).cloned() else {
        return type_id;
    };

    match typ {
        Type::Variable(name) => bindings.get(&name).copied().unwrap_or(type_id),
        Type::Union(variants) => {
            let new_variants: Vec<usize> = variants
                .iter()
                .map(|&v| substitute(v, bindings, program))
                .collect();
            union_type_ids(program, new_variants)
        }
        Type::Tuple(tuple_id) => {
            // Look up the tuple's fields and clone to avoid borrow issues
            if let Some(type_info) = program.lookup_tuple(tuple_id).cloned() {
                // Substitute within each field type
                let mut any_changed = false;
                let new_fields: Vec<(Option<String>, usize)> = type_info
                    .fields
                    .iter()
                    .map(|(name, field_type_id)| {
                        let new_type_id = substitute(*field_type_id, bindings, program);
                        if !any_changed && new_type_id != *field_type_id {
                            any_changed = true;
                        }
                        (name.clone(), new_type_id)
                    })
                    .collect();

                // If any field changed, register a new tuple type
                if any_changed {
                    let new_tuple_id = program.register_tuple(type_info.name.clone(), new_fields);
                    program.register_type(Type::Tuple(new_tuple_id))
                } else {
                    type_id
                }
            } else {
                // Type not found in registry, just return original
                type_id
            }
        }
        Type::Partial { name, fields } => {
            // Substitute within each field type
            let mut any_changed = false;
            let new_fields: Vec<(String, usize)> = fields
                .iter()
                .map(|(fname, field_type_id)| {
                    let new_type_id = substitute(*field_type_id, bindings, program);
                    if !any_changed && new_type_id != *field_type_id {
                        any_changed = true;
                    }
                    (fname.clone(), new_type_id)
                })
                .collect();

            // If any field changed, register a new partial type
            if any_changed {
                program.register_type(Type::Partial {
                    name: name.clone(),
                    fields: new_fields,
                })
            } else {
                type_id
            }
        }
        Type::Integer | Type::Binary | Type::Reference | Type::Cycle(_) | Type::Resource(_) => {
            type_id
        }
        Type::Callable {
            parameter,
            result,
            receive,
        } => {
            let new_param = substitute(parameter, bindings, program);
            let new_result = substitute(result, bindings, program);
            let new_receive = substitute(receive, bindings, program);
            if new_param == parameter && new_result == result && new_receive == receive {
                type_id
            } else {
                program.register_type(Type::Callable {
                    parameter: new_param,
                    result: new_result,
                    receive: new_receive,
                })
            }
        }
        Type::Process { send, receive } => {
            let new_send = send.map(|s| substitute(s, bindings, program));
            let new_receive = receive.map(|r| substitute(r, bindings, program));
            if new_send == send && new_receive == receive {
                type_id
            } else {
                program.register_type(Type::Process {
                    send: new_send,
                    receive: new_receive,
                })
            }
        }
    }
}

/// Unify a pattern type (containing Type::Variable) with a concrete type.
/// Builds up a mapping from type variable names to concrete type IDs.
/// Returns an error if there's a conflict (e.g., variable bound to two different types).
pub fn unify(
    bindings: &mut HashMap<String, usize>,
    pattern_id: usize,
    concrete_id: usize,
    program: &mut Program,
) -> Result<(), Error> {
    let pattern = program.lookup_type(pattern_id).cloned();
    let concrete = program.lookup_type(concrete_id).cloned();

    let (Some(pattern), Some(concrete)) = (pattern, concrete) else {
        return Ok(()); // If we can't look up either type, assume compatible
    };

    match (&pattern, &concrete) {
        // When pattern is a variable, bind it or check consistency
        (Type::Variable(name), _) => {
            // First, resolve concrete if it's also a variable
            let resolved_concrete_id = if let Type::Variable(concrete_name) = &concrete {
                bindings.get(concrete_name).copied().unwrap_or(concrete_id)
            } else {
                concrete_id
            };

            if let Some(existing_id) = bindings.get(name).copied() {
                // Variable already bound - widen to union if different
                if existing_id != resolved_concrete_id {
                    // Widen the type variable to a union
                    let widened = union_type_ids(program, vec![existing_id, resolved_concrete_id]);
                    bindings.insert(name.clone(), widened);
                }
            } else {
                // New binding - but make sure we're not binding a variable to itself
                if let Some(Type::Variable(resolved_name)) =
                    program.lookup_type(resolved_concrete_id)
                    && resolved_name == name
                {
                    // Don't bind a variable to itself
                    return Ok(());
                }
                bindings.insert(name.clone(), resolved_concrete_id);
            }
            Ok(())
        }

        // When concrete is a variable, resolve it and try unifying with the resolved type
        (_, Type::Variable(name)) => {
            if let Some(&resolved_id) = bindings.get(name) {
                // Concrete variable is bound - unify with its binding
                unify(bindings, pattern_id, resolved_id, program)
            } else {
                // Concrete variable is unbound - this shouldn't happen in normal unification
                Err(Error::TypeUnresolved(
                    "Cannot unify with unbound type variable in concrete position".to_string(),
                ))
            }
        }

        // Both are basic types - must match
        (Type::Integer, Type::Integer) => Ok(()),
        (Type::Binary, Type::Binary) => Ok(()),

        // Process types must match in structure
        (
            Type::Process {
                send: send1,
                receive: receive1,
            },
            Type::Process {
                send: send2,
                receive: receive2,
            },
        ) => {
            // Unify send types (what can be sent TO the process)
            match (send1, send2) {
                (Some(s1), Some(s2)) => unify(bindings, *s1, *s2, program)?,
                (None, None) => {}
                _ => {
                    return Err(Error::TypeUnresolved(
                        "Process types have incompatible send types".to_string(),
                    ));
                }
            }
            // Unify receive types (what you GET from the process)
            match (receive1, receive2) {
                (Some(ret1), Some(ret2)) => unify(bindings, *ret1, *ret2, program)?,
                (None, None) => {}
                _ => {
                    return Err(Error::TypeUnresolved(
                        "Process types have incompatible receive types".to_string(),
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

            let info1 = program
                .lookup_tuple(*id1)
                .ok_or(Error::TupleNotInRegistry { tuple_id: *id1 })?;
            let info2 = program
                .lookup_tuple(*id2)
                .ok_or(Error::TupleNotInRegistry { tuple_id: *id2 })?;

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
            let fields1 = info1.fields.clone();
            let fields2 = info2.fields.clone();
            for ((fname1, ftype1_id), (fname2, ftype2_id)) in fields1.iter().zip(fields2.iter()) {
                if fname1 != fname2 {
                    return Err(Error::TypeUnresolved(
                        "Tuple fields have different names".to_string(),
                    ));
                }
                unify(bindings, *ftype1_id, *ftype2_id, program)?;
            }

            Ok(())
        }

        // Partial type vs concrete tuple - check pattern fields exist in concrete
        (
            Type::Partial {
                name: pattern_name,
                fields: pattern_fields,
            },
            Type::Tuple(concrete_tuple_id),
        ) => {
            let concrete_info =
                program
                    .lookup_tuple(*concrete_tuple_id)
                    .ok_or(Error::TupleNotInRegistry {
                        tuple_id: *concrete_tuple_id,
                    })?;

            // If partial has a name, concrete must match
            if let Some(pname) = pattern_name
                && concrete_info.name.as_ref() != Some(pname)
            {
                return Err(Error::TypeUnresolved(format!(
                    "Partial type expects name {:?}, got {:?}",
                    pname, concrete_info.name
                )));
            }

            // Check all pattern fields exist in concrete with compatible types
            let concrete_fields = concrete_info.fields.clone();
            for (pattern_fname, pattern_ftype_id) in pattern_fields {
                let Some((_, concrete_ftype_id)) = concrete_fields
                    .iter()
                    .find(|(fname, _)| fname.as_ref() == Some(pattern_fname))
                else {
                    return Err(Error::TypeUnresolved(format!(
                        "Concrete type missing field: {}",
                        pattern_fname
                    )));
                };

                unify(bindings, *pattern_ftype_id, *concrete_ftype_id, program)?;
            }

            Ok(())
        }

        // Partial type vs partial type - check pattern fields exist in concrete partial
        (
            Type::Partial {
                name: pattern_name,
                fields: pattern_fields,
            },
            Type::Partial {
                name: concrete_name,
                fields: concrete_fields,
            },
        ) => {
            // If partial has a name, concrete must match
            if let Some(pname) = pattern_name
                && concrete_name.as_ref() != Some(pname)
            {
                return Err(Error::TypeUnresolved(format!(
                    "Partial type expects name {:?}, got {:?}",
                    pname, concrete_name
                )));
            }

            // Check all pattern fields exist in concrete with compatible types
            for (pattern_fname, pattern_ftype_id) in pattern_fields {
                let Some((_, concrete_ftype_id)) = concrete_fields
                    .iter()
                    .find(|(fname, _)| fname == pattern_fname)
                else {
                    return Err(Error::TypeUnresolved(format!(
                        "Concrete partial missing field: {}",
                        pattern_fname
                    )));
                };

                unify(bindings, *pattern_ftype_id, *concrete_ftype_id, program)?;
            }

            Ok(())
        }

        // Function types - parameters are contravariant, results are covariant
        (
            Type::Callable {
                parameter: param1,
                result: result1,
                receive: receive1,
            },
            Type::Callable {
                parameter: param2,
                result: result2,
                receive: receive2,
            },
        ) => {
            // Unify parameters (contravariant - swap order)
            unify(bindings, *param1, *param2, program)?;
            // Unify results (covariant)
            unify(bindings, *result1, *result2, program)?;
            // Unify receive types (contravariant - swap order)
            unify(bindings, *receive1, *receive2, program)?;
            Ok(())
        }

        // Handle cycles on either side - for recursive types like list<t>.
        // A `Cycle` is a back-reference to an enclosing μ-binder, i.e. a recursive
        // occurrence of the surrounding type. The two sides can legitimately be unrolled to
        // different depths (one shows the expanded union, the other still holds the cycle),
        // so a cycle unifies with whatever stands at the same position on the other side.
        // This is sound because the recursive structure is checked at every non-cyclic
        // position; the back-edge carries no additional constraint to verify here.
        (Type::Cycle(_depth), _) | (_, Type::Cycle(_depth)) => Ok(()),

        // Never type (empty union) unifies with anything
        (Type::Union(variants), _) if variants.is_empty() => Ok(()),
        (_, Type::Union(variants)) if variants.is_empty() => Ok(()),

        // Union types - the argument's type (`concrete`) must be a subtype of the parameter's
        // type (`pattern`): every concrete variant has to unify with at least one pattern
        // variant, binding the pattern's type variables. The reverse — requiring every pattern
        // variant to appear in the argument — would wrongly reject a narrower argument (e.g. a
        // list builder that only ever returns `Cons` flowing where `Cons | Nil` is expected).
        (Type::Union(pattern_variants), Type::Union(concrete_variants)) => {
            if concrete_variants.is_empty() {
                return Ok(()); // the empty union (NEVER) is a subtype of anything
            }

            let pattern_variants = pattern_variants.clone();
            let concrete_variants = concrete_variants.clone();

            for &concrete_variant in &concrete_variants {
                let mut found_match = false;
                for &pattern_variant in &pattern_variants {
                    let mut temp_bindings = bindings.clone();
                    if unify(
                        &mut temp_bindings,
                        pattern_variant,
                        concrete_variant,
                        program,
                    )
                    .is_ok()
                    {
                        // Merge the bindings
                        for (k, v) in temp_bindings {
                            if let Some(&existing) = bindings.get(&k)
                                && !quiver_core::types::is_compatible(v, existing, program)
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
                        "Cannot unify union variant: concrete has variant that doesn't match any pattern variant".to_string()
                    ));
                }
            }
            Ok(())
        }

        // Pattern union with concrete non-union - try each variant
        (Type::Union(variants), _) => {
            let variants = variants.clone();
            // Try to unify with at least one variant
            let mut errors = Vec::new();
            for (i, &variant) in variants.iter().enumerate() {
                let mut temp_bindings = bindings.clone();
                match unify(&mut temp_bindings, variant, concrete_id, program) {
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
                "Cannot unify union pattern ({} variants) with concrete type. Errors: [{}]",
                variants.len(),
                errors.join(", ")
            )))
        }

        // Concrete union with pattern non-union - pattern must match one variant
        (_, Type::Union(variants)) => {
            let variants = variants.clone();
            for &variant in &variants {
                let mut temp_bindings = bindings.clone();
                if unify(&mut temp_bindings, pattern_id, variant, program).is_ok() {
                    *bindings = temp_bindings;
                    return Ok(());
                }
            }
            Err(Error::TypeUnresolved(format!(
                "Cannot unify pattern with concrete union ({} variants)",
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
