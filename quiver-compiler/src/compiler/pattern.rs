use std::collections::{HashMap, HashSet};

use crate::ast;
use quiver_core::{
    bytecode::{Constant, Instruction, TypeId},
    program::Program,
    types::{Type, TypeLookup},
};

use super::{Error, codegen::InstructionBuilder, typing::TypeContext};

// Type aliases for complex pattern matching types
type PatternAnalysisResult = (MatchCertainty, Vec<(String, Type)>, Vec<BindingSet>);
type TupleMatchResult = Vec<(TypeId, Vec<(usize, usize)>)>;

/// Represents the certainty of a pattern match
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MatchCertainty {
    /// Pattern cannot possibly match the value type
    WontMatch,
    /// Pattern might match, requires runtime checks
    MightMatch,
    /// Pattern will definitely match, no runtime checks needed
    WillMatch,
}

/// Represents the mode of pattern matching
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PatternMode {
    /// Bind mode - identifiers create new bindings (default)
    Bind,
    /// Pin mode - identifiers are checked against existing variables
    Pin,
}

/// Represents a check that must be performed at runtime
#[derive(Debug, Clone)]
enum RuntimeCheck {
    TupleType(TypeId),
    Literal(ast::Literal),
    Variable(String),
    Path(AccessPath),
}

/// A requirement that must be satisfied for a pattern to match
#[derive(Debug, Clone)]
struct Requirement {
    path: AccessPath,
    check: RuntimeCheck,
}

/// Represents a path to access a value within a data structure
/// Empty vector means root, otherwise it's a sequence of field indices
type AccessPath = Vec<usize>;

/// Tracks information about identifiers encountered during pattern analysis
#[derive(Debug, Clone)]
struct Indentifier {
    first_path: AccessPath,
    is_pin_mode: bool,
    is_repeated: bool,
}

/// Information about a variable binding
#[derive(Debug, Clone)]
struct Binding {
    name: String,
    path: AccessPath,
    var_type: Type,
}

/// A set of bindings that can be created if certain requirements are met
#[derive(Debug, Clone)]
pub struct BindingSet {
    requirements: Vec<Requirement>, // Requirements that must be satisfied
    bindings: Vec<Binding>,         // Variable bindings to create if requirements are met
}

/// Analyze pattern without generating code
pub fn analyze_pattern(
    type_context: &TypeContext,
    type_lookup: &impl TypeLookup,
    pattern: &ast::Match,
    value_type: &Type,
    mode: PatternMode,
    variables: &HashSet<String>,
) -> Result<PatternAnalysisResult, Error> {
    let mut identifiers = HashMap::new();
    let binding_sets = analyze_match_pattern(
        type_context,
        type_lookup,
        pattern,
        value_type,
        vec![],
        mode,
        &mut identifiers,
        variables,
    )?;

    if binding_sets.is_empty() {
        return Ok((MatchCertainty::WontMatch, Vec::new(), Vec::new()));
    }

    // Validate: check for single-occurrence pins with no variable in scope
    for (name, info) in &identifiers {
        if info.is_pin_mode && !info.is_repeated && !variables.contains(name) {
            return Err(Error::InternalError {
                message: format!("Pin variable '{}' not found in scope", name),
            });
        }
    }

    let certainty = if binding_sets.iter().any(|bs| bs.requirements.is_empty()) {
        MatchCertainty::WillMatch
    } else {
        MatchCertainty::MightMatch
    };

    let all_bindings = if let Some(first_set) = binding_sets.first() {
        first_set
            .bindings
            .iter()
            .map(|b| (b.name.clone(), b.var_type.clone()))
            .collect()
    } else {
        Vec::new()
    };

    Ok((certainty, all_bindings, binding_sets))
}

/// Generate bytecode for pattern matching
pub fn generate_pattern_code(
    codegen: &mut InstructionBuilder,
    program: &mut Program,
    local_count: &mut usize,
    bindings_map: Option<&HashMap<String, usize>>,
    variables: &HashMap<String, usize>,
    binding_sets: &[BindingSet],
    fail_addr: usize,
) -> Result<(), Error> {
    let mut end_jumps = Vec::new();
    let mut next_set_jumps = Vec::new();

    for (i, binding_set) in binding_sets.iter().enumerate() {
        // Patch jumps from previous iteration that should skip to this binding set
        for jump in next_set_jumps.drain(..) {
            codegen.patch_jump_to_here(jump);
        }

        let is_last = i == binding_sets.len() - 1;

        // Check all requirements for this binding set
        for requirement in &binding_set.requirements {
            match &requirement.check {
                RuntimeCheck::Path(other_path) => {
                    codegen.add_instruction(Instruction::Duplicate);
                    for &index in &requirement.path {
                        codegen.add_instruction(Instruction::Get(index));
                    }
                    codegen.add_instruction(Instruction::Pick(1));
                    for &index in other_path {
                        codegen.add_instruction(Instruction::Get(index));
                    }
                    codegen.add_instruction(Instruction::Equal(2));
                }
                RuntimeCheck::TupleType(type_id) => {
                    generate_value_access(codegen, &requirement.path);
                    codegen.add_instruction(Instruction::IsTuple(*type_id));
                }
                RuntimeCheck::Literal(literal) => {
                    generate_value_access(codegen, &requirement.path);
                    match literal {
                        ast::Literal::Integer(val) => {
                            let idx = program.register_constant(Constant::Integer(*val));
                            codegen.add_instruction(Instruction::Constant(idx));
                        }
                        ast::Literal::Binary(bytes) => {
                            let idx = program.register_constant(Constant::Binary(bytes.clone()));
                            codegen.add_instruction(Instruction::Constant(idx));
                        }
                    }
                    codegen.add_instruction(Instruction::Equal(2));
                }
                RuntimeCheck::Variable(name) => {
                    generate_value_access(codegen, &requirement.path);
                    let var_index = variables.get(name).ok_or_else(|| Error::InternalError {
                        message: format!("Pin variable '{}' not found in scope", name),
                    })?;
                    codegen.add_instruction(Instruction::Load(*var_index));
                    codegen.add_instruction(Instruction::Equal(2));
                }
            }

            codegen.add_instruction(Instruction::Not);
            if is_last {
                codegen.emit_jump_if_to_addr(fail_addr);
            } else {
                let skip = codegen.emit_jump_if_placeholder();
                next_set_jumps.push(skip);
            }
        }

        // If we get here, all checks passed - extract bindings
        for binding in binding_set.bindings.iter() {
            generate_value_access(codegen, &binding.path);

            // Get local index from bindings_map if provided, otherwise allocate new
            let local_index = if let Some(map) = bindings_map {
                *map.get(&binding.name).ok_or_else(|| Error::InternalError {
                    message: format!("Binding '{}' not found in bindings_map", binding.name),
                })?
            } else {
                let idx = *local_count;
                *local_count += 1;
                idx
            };

            codegen.add_instruction(Instruction::Store(local_index));
        }

        // Jump to end (unless this is the last set)
        if !is_last {
            let end_jump = codegen.emit_jump_placeholder();
            end_jumps.push(end_jump);
        }
    }

    // Patch any remaining next_set_jumps to fail
    for jump in next_set_jumps {
        codegen.patch_jump_to_addr(jump, fail_addr);
    }

    // Patch all end jumps to here
    for end_jump in end_jumps {
        codegen.patch_jump_to_here(end_jump);
    }

    Ok(())
}

fn generate_value_access(codegen: &mut InstructionBuilder, path: &AccessPath) {
    codegen.add_instruction(Instruction::Duplicate);
    for &index in path {
        codegen.add_instruction(Instruction::Get(index));
    }
}

#[allow(clippy::too_many_arguments)]
fn analyze_match_pattern(
    type_context: &TypeContext,
    type_lookup: &impl TypeLookup,
    pattern: &ast::Match,
    value_type: &Type,
    path: AccessPath,
    mode: PatternMode,
    identifiers: &mut HashMap<String, Indentifier>,
    variables: &HashSet<String>,
) -> Result<Vec<BindingSet>, Error> {
    match pattern {
        ast::Match::Identifier(name) => analyze_identifier_pattern(
            name.clone(),
            value_type.clone(),
            path,
            mode,
            identifiers,
            variables,
        ),
        ast::Match::Literal(literal) => analyze_literal_pattern(literal.clone(), path),
        ast::Match::Tuple(tuple) => analyze_match_tuple_pattern(
            type_context,
            type_lookup,
            tuple,
            value_type,
            path,
            mode,
            identifiers,
            variables,
        ),
        ast::Match::Partial(partial) => {
            analyze_partial_pattern(type_lookup, partial, value_type, path, identifiers)
        }
        ast::Match::Star => analyze_star_pattern(type_lookup, value_type, path, identifiers),
        ast::Match::Placeholder => Ok(vec![BindingSet {
            requirements: vec![],
            bindings: vec![],
        }]),
        ast::Match::Pin(inner) => analyze_match_pattern(
            type_context,
            type_lookup,
            inner,
            value_type,
            path,
            PatternMode::Pin,
            identifiers,
            variables,
        ),
        ast::Match::Bind(inner) => analyze_match_pattern(
            type_context,
            type_lookup,
            inner,
            value_type,
            path,
            PatternMode::Bind,
            identifiers,
            variables,
        ),
    }
}

#[allow(clippy::too_many_arguments)]
fn analyze_match_tuple_pattern(
    type_context: &TypeContext,
    type_lookup: &impl TypeLookup,
    tuple: &ast::MatchTuple,
    value_type: &Type,
    path: AccessPath,
    mode: PatternMode,
    identifiers: &mut HashMap<String, Indentifier>,
    variables: &HashSet<String>,
) -> Result<Vec<BindingSet>, Error> {
    let mut binding_sets = vec![];

    // Find matching tuple types
    let matching_types = find_matching_match_tuple_types(type_lookup, tuple, value_type)?;

    // For each matching type, create binding sets
    for (type_id, field_mappings) in &matching_types {
        // Clone identifiers only if there are multiple variants to avoid cross-contamination
        // For a single variant, use the parent's identifiers directly
        let mut variant_identifiers_storage;
        let variant_identifiers = if matching_types.len() > 1 {
            variant_identifiers_storage = identifiers.clone();
            &mut variant_identifiers_storage
        } else {
            &mut *identifiers
        };

        // Get tuple info and extract field types upfront to avoid borrow issues
        let tuple_fields: Vec<Type> = {
            let tuple_info = type_lookup
                .lookup_type(type_id)
                .ok_or(Error::TypeNotInRegistry { type_id: *type_id })?;
            tuple_info.fields.iter().map(|(_, t)| t.clone()).collect()
        };

        // Start with a binding set for this type
        let mut base_requirements = vec![];
        // Add runtime check if needed
        if value_type.extract_tuple_types().len() > 1 {
            base_requirements.push(Requirement {
                path: path.clone(),
                check: RuntimeCheck::TupleType(*type_id),
            });
        }

        let mut current_binding_sets = vec![BindingSet {
            requirements: base_requirements,
            bindings: vec![],
        }];

        // Process each field pattern
        for (pattern_idx, actual_idx) in field_mappings {
            let field = &tuple.fields[*pattern_idx];
            let raw_field_type = &tuple_fields[*actual_idx];

            // Resolve Type::Cycle references to the actual type
            let field_type = if let Type::Cycle(_) = raw_field_type {
                value_type.clone()
            } else {
                raw_field_type.clone()
            };

            let mut field_path = path.clone();
            field_path.push(*actual_idx);

            // Recursively analyze the field pattern
            let field_binding_sets = analyze_match_pattern(
                type_context,
                type_lookup,
                &field.pattern,
                &field_type,
                field_path,
                mode,
                variant_identifiers,
                variables,
            )?;

            if field_binding_sets.is_empty() {
                // This type variant can't match, skip it entirely
                current_binding_sets.clear();
                break;
            }

            // Combine field binding sets with current binding sets (cartesian product)
            let mut new_binding_sets = vec![];
            for current_set in &current_binding_sets {
                for field_set in &field_binding_sets {
                    let mut combined_requirements = current_set.requirements.clone();
                    combined_requirements.extend(field_set.requirements.clone());

                    let mut combined_bindings = current_set.bindings.clone();
                    combined_bindings.extend(field_set.bindings.clone());

                    new_binding_sets.push(BindingSet {
                        requirements: combined_requirements,
                        bindings: combined_bindings,
                    });
                }
            }
            current_binding_sets = new_binding_sets;
        }

        // Add all binding sets from this type to the result
        binding_sets.extend(current_binding_sets);
    }

    if matches!(&value_type, Type::Union(types) if types.is_empty()) {
        return Err(Error::InternalError {
            message: format!(
                "analyze_assignment_tuple_pattern received empty Type for tuple: {:?}",
                tuple
            ),
        });
    }

    Ok(binding_sets)
}

fn find_matching_match_tuple_types(
    type_lookup: &impl TypeLookup,
    tuple: &ast::MatchTuple,
    value_type: &Type,
) -> Result<TupleMatchResult, Error> {
    let mut matching_types = Vec::new();

    let types_to_check = match value_type {
        Type::Union(types) => types.as_slice(),
        single => std::slice::from_ref(single),
    };

    for typ in types_to_check {
        if let Type::Tuple(type_id) = typ
            && let Some(field_mappings) = check_match_tuple_match(type_lookup, tuple, *type_id)?
        {
            matching_types.push((*type_id, field_mappings));
        }
    }

    Ok(matching_types)
}

fn check_match_tuple_match(
    type_lookup: &impl TypeLookup,
    tuple: &ast::MatchTuple,
    type_id: TypeId,
) -> Result<Option<Vec<(usize, usize)>>, Error> {
    let tuple_info = type_lookup
        .lookup_type(&type_id)
        .ok_or(Error::TypeNotInRegistry { type_id })?;

    if tuple.name.as_ref() != tuple_info.name.as_ref() || tuple.fields.len() != tuple_info.fields.len() {
        return Ok(None);
    }

    let mut field_mappings = Vec::new();
    for (pattern_idx, field) in tuple.fields.iter().enumerate() {
        let tuple_field = &tuple_info.fields[pattern_idx];
        if field.name.as_ref() != tuple_field.0.as_ref() {
            return Ok(None);
        }

        field_mappings.push((pattern_idx, pattern_idx));
    }

    Ok(Some(field_mappings))
}

fn analyze_literal_pattern(
    literal: ast::Literal,
    path: AccessPath,
) -> Result<Vec<BindingSet>, Error> {
    Ok(vec![BindingSet {
        requirements: vec![Requirement {
            path,
            check: RuntimeCheck::Literal(literal),
        }],
        bindings: vec![],
    }])
}

fn analyze_identifier_pattern(
    name: String,
    value_type: Type,
    path: AccessPath,
    mode: PatternMode,
    identifiers: &mut HashMap<String, Indentifier>,
    variables: &HashSet<String>,
) -> Result<Vec<BindingSet>, Error> {
    // Empty Type should never happen - it indicates an internal error
    if matches!(&value_type, Type::Union(types) if types.is_empty()) {
        return Err(Error::InternalError {
            message: format!(
                "analyze_identifier_pattern received empty Type for identifier '{}'",
                name
            ),
        });
    }

    // Check if we've seen this identifier before
    if let Some(info) = identifiers.get_mut(&name) {
        // Second or later occurrence - mark as repeated and create Path requirement
        info.is_repeated = true;
        Ok(vec![BindingSet {
            requirements: vec![Requirement {
                path: info.first_path.clone(),
                check: RuntimeCheck::Path(path),
            }],
            bindings: vec![],
        }])
    } else {
        // First occurrence - record it
        identifiers.insert(
            name.clone(),
            Indentifier {
                first_path: path.clone(),
                is_pin_mode: mode == PatternMode::Pin,
                is_repeated: false,
            },
        );

        match mode {
            PatternMode::Bind => {
                // Create a binding for this identifier
                Ok(vec![BindingSet {
                    requirements: vec![],
                    bindings: vec![Binding {
                        name,
                        path,
                        var_type: value_type,
                    }],
                }])
            }
            PatternMode::Pin => {
                // In pin mode, only create Variable requirement if variable exists in scope
                // If it doesn't exist and this is the only occurrence, we'll error later
                let mut requirements = vec![];
                if variables.contains(&name) {
                    requirements.push(Requirement {
                        path,
                        check: RuntimeCheck::Variable(name),
                    });
                }
                Ok(vec![BindingSet {
                    requirements,
                    bindings: vec![],
                }])
            }
        }
    }
}

fn analyze_partial_pattern(
    type_lookup: &impl TypeLookup,
    partial_pattern: &ast::PartialPattern,
    value_type: &Type,
    path: AccessPath,
    identifiers: &mut HashMap<String, Indentifier>,
) -> Result<Vec<BindingSet>, Error> {
    let mut binding_sets = vec![];

    // Find types that have all required fields (and optionally matching tuple name)
    let matching_types = find_types_with_fields_and_name(
        type_lookup,
        &partial_pattern.fields,
        partial_pattern.name.as_ref(),
        value_type,
    )?;

    // Create a binding set for each matching type
    for (type_id, field_indices) in &matching_types {
        // Clone identifiers only if there are multiple variants to avoid cross-contamination
        let mut variant_identifiers_storage;
        let variant_identifiers = if matching_types.len() > 1 {
            variant_identifiers_storage = identifiers.clone();
            &mut variant_identifiers_storage
        } else {
            &mut *identifiers
        };

        let mut requirements = vec![];

        if value_type.extract_tuple_types().len() > 1 {
            requirements.push(Requirement {
                path: path.clone(),
                check: RuntimeCheck::TupleType(*type_id),
            });
        }

        let tuple_info = type_lookup
            .lookup_type(type_id)
            .ok_or(Error::TypeNotInRegistry { type_id: *type_id })?;

        let mut bindings = Vec::new();
        for (i, field_name) in partial_pattern.fields.iter().enumerate() {
            let idx = field_indices[i];
            let field_type = &tuple_info.fields[idx].1;
            let mut field_path = path.clone();
            field_path.push(idx);

            // Check if we've seen this identifier before
            if let Some(info) = variant_identifiers.get_mut(field_name) {
                // Repeated identifier - mark as repeated and add Path requirement
                info.is_repeated = true;
                requirements.push(Requirement {
                    path: info.first_path.clone(),
                    check: RuntimeCheck::Path(field_path),
                });
            } else {
                // First occurrence - record it and create binding
                variant_identifiers.insert(
                    field_name.clone(),
                    Indentifier {
                        first_path: field_path.clone(),
                        is_pin_mode: false,
                        is_repeated: false,
                    },
                );
                bindings.push(Binding {
                    name: field_name.clone(),
                    path: field_path,
                    var_type: field_type.clone(),
                });
            }
        }

        binding_sets.push(BindingSet {
            requirements,
            bindings,
        });
    }

    Ok(binding_sets)
}

fn analyze_star_pattern(
    type_lookup: &impl TypeLookup,
    value_type: &Type,
    path: AccessPath,
    identifiers: &mut HashMap<String, Indentifier>,
) -> Result<Vec<BindingSet>, Error> {
    // Collect all tuple types
    let tuple_types = value_type.extract_tuple_types();

    // Create a binding set for each tuple type
    let mut binding_sets = vec![];
    for type_id in &tuple_types {
        // Clone identifiers only if there are multiple variants to avoid cross-contamination
        let mut variant_identifiers_storage;
        let variant_identifiers = if tuple_types.len() > 1 {
            variant_identifiers_storage = identifiers.clone();
            &mut variant_identifiers_storage
        } else {
            &mut *identifiers
        };

        let tuple_info = type_lookup
            .lookup_type(type_id)
            .ok_or(Error::TypeNotInRegistry { type_id: *type_id })?;

        // Add type check if there are multiple tuple types
        let mut requirements = vec![];
        if tuple_types.len() > 1 {
            requirements.push(Requirement {
                path: path.clone(),
                check: RuntimeCheck::TupleType(*type_id),
            });
        }

        let mut bindings = Vec::new();
        for (idx, (name, field_type)) in tuple_info.fields.iter().enumerate() {
            if let Some(field_name) = name {
                let mut field_path = path.clone();
                field_path.push(idx);

                // Check if we've seen this identifier before
                if let Some(info) = variant_identifiers.get_mut(field_name) {
                    // Repeated identifier - mark as repeated and add Path requirement
                    info.is_repeated = true;
                    requirements.push(Requirement {
                        path: info.first_path.clone(),
                        check: RuntimeCheck::Path(field_path),
                    });
                } else {
                    // First occurrence - record it and create binding
                    variant_identifiers.insert(
                        field_name.clone(),
                        Indentifier {
                            first_path: field_path.clone(),
                            is_pin_mode: false,
                            is_repeated: false,
                        },
                    );
                    bindings.push(Binding {
                        name: field_name.clone(),
                        path: field_path,
                        var_type: field_type.clone(),
                    });
                }
            }
        }

        binding_sets.push(BindingSet {
            requirements,
            bindings,
        });
    }

    Ok(binding_sets)
}

fn find_types_with_fields_and_name(
    type_lookup: &impl TypeLookup,
    field_names: &[String],
    tuple_name: Option<&String>,
    value_type: &Type,
) -> Result<Vec<(TypeId, Vec<usize>)>, Error> {
    let mut matching_types = Vec::new();

    let types_to_check = match value_type {
        Type::Union(types) => types.as_slice(),
        single => std::slice::from_ref(single),
    };

    for typ in types_to_check {
        if let Type::Tuple(type_id) | Type::Partial(type_id) = typ {
            let tuple_info = type_lookup
                .lookup_type(type_id)
                .ok_or(Error::TypeNotInRegistry { type_id: *type_id })?;

            // Check if tuple name matches (if specified)
            if let Some(expected_name) = tuple_name
                && tuple_info.name.as_ref() != Some(expected_name)
            {
                continue; // Skip this type if name doesn't match
            }

            if let Some(indices) = find_field_indices(field_names, &tuple_info.fields) {
                matching_types.push((*type_id, indices));
            }
        }
    }

    Ok(matching_types)
}

/// Find indices of specified fields in a tuple type
fn find_field_indices(
    field_names: &[String],
    tuple_fields: &[(Option<String>, Type)],
) -> Option<Vec<usize>> {
    let mut indices = Vec::new();

    for field_name in field_names {
        match tuple_fields
            .iter()
            .position(|(name, _)| name.as_deref() == Some(field_name))
        {
            Some(idx) => indices.push(idx),
            None => return None,
        }
    }

    Some(indices)
}
