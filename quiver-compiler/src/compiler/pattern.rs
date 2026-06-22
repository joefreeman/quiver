use std::collections::HashMap;

use crate::ast;
use quiver_core::{
    bytecode::{Constant, Instruction},
    program::Program,
    types::{Type, TypeLookup},
};

use super::{
    Error, codegen::InstructionBuilder, narrowing::intersect_types, typing::union_type_ids,
};

// Type aliases for complex pattern matching types (using type IDs)
type PatternAnalysisResult = (Vec<(String, usize)>, Vec<BindingSet>, usize);
type TupleMatchResult = Vec<(usize, Vec<(usize, usize)>)>;

/// Helper for managing identifiers across variants
/// Clones identifiers when processing multiple variants to avoid cross-contamination
enum IdentifierScope<'a> {
    /// Borrowed reference to parent identifiers (single variant case)
    Borrowed(&'a mut HashMap<String, Identifier>),
    /// Owned clone (multiple variants case)
    Owned(HashMap<String, Identifier>),
}

impl<'a> IdentifierScope<'a> {
    /// Create an appropriate scope based on whether we have multiple variants
    fn new(identifiers: &'a mut HashMap<String, Identifier>, has_multiple_variants: bool) -> Self {
        if has_multiple_variants {
            Self::Owned(identifiers.clone())
        } else {
            Self::Borrowed(identifiers)
        }
    }

    /// Get mutable access to the identifier map
    fn get_mut(&mut self) -> &mut HashMap<String, Identifier> {
        match self {
            Self::Borrowed(map) => map,
            Self::Owned(map) => map,
        }
    }
}

/// Represents a check that must be performed at runtime
#[derive(Debug, Clone)]
enum RuntimeCheck {
    TypeId(usize), // Type ID to check against
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
struct Identifier {
    first_path: AccessPath,
    is_repeated: bool,
}

/// Information about a variable binding
#[derive(Debug, Clone)]
struct Binding {
    name: String,
    path: AccessPath,
    var_type_id: usize,
}

/// A set of bindings that can be created if certain requirements are met
#[derive(Debug, Clone)]
pub struct BindingSet {
    requirements: Vec<Requirement>, // Requirements that must be satisfied
    bindings: Vec<Binding>,         // Variable bindings to create if requirements are met
}

/// Check if any binding set has requirements that prevent complement narrowing.
///
/// Returns true if the pattern cannot use complement narrowing because:
/// 1. It has value-based requirements (literals, variable pins, path equality) — their negation
///    is not a type, so the structural complement cannot represent it.
/// 2. It has a partial type check — a partial like `(x: A)` may fail because a field value
///    doesn't match, not because the tuple type is wrong.
///
/// Concrete type checks at *any* depth are fine: `compute_complement` is structural over tuple
/// fields, so a failed inner check soundly refines the outer type. (Constraints on *recursive*
/// fields are a separate concern — the narrowed type can't capture them — and are handled by
/// `narrowing::pattern_constrains_recursive_field` at the call site.)
///
/// For example:
/// - `=Node[x, y]`, `=Node[Leaf[x], _]` - safe, concrete type checks
/// - `=5` - NOT safe, literal check
/// - `=(x: A)` - NOT safe, partial type check (field value could fail)
pub fn prevents_complement_narrowing(binding_sets: &[BindingSet], program: &Program) -> bool {
    binding_sets.iter().any(|bs| {
        bs.requirements.iter().any(|req| {
            match &req.check {
                // Value-based checks prevent complement narrowing
                RuntimeCheck::Literal(_) | RuntimeCheck::Variable(_) | RuntimeCheck::Path(_) => {
                    true
                }
                // Concrete type checks — at any depth — are fine: `compute_complement` is
                // structural over tuple fields (and sound on recursive types), so a failed inner
                // check soundly refines the outer type. Partial checks remain an exception, as
                // they constrain field compatibility rather than concrete identity.
                RuntimeCheck::TypeId(type_id) => {
                    matches!(program.lookup_type(*type_id), Some(Type::Partial { .. }))
                }
            }
        })
    })
}

/// Analyze pattern without generating code
pub fn analyze_pattern(
    env: &mut super::typing::TypeEnv,
    program: &mut Program,
    pattern: &ast::Match,
    value_type_id: usize,
    scopes: &[super::scopes::Scope],
    value_provenance: &super::provenance::Provenance,
) -> Result<PatternAnalysisResult, Error> {
    let mut identifiers = HashMap::new();
    let (binding_sets, narrowed_type_id) = analyze_match_pattern(
        env,
        program,
        pattern,
        value_type_id,
        vec![],
        &mut identifiers,
        scopes,
        value_provenance,
    )?;

    if binding_sets.is_empty() {
        // Won't match - return never type (empty union)
        return Ok((Vec::new(), Vec::new(), program.never()));
    }

    // Check if all binding sets have requirements (might match) or some have none (will match)
    let will_match = binding_sets.iter().any(|bs| bs.requirements.is_empty());

    // Collect all bindings and union their types across all binding sets
    let mut bindings_map: HashMap<String, Vec<usize>> = HashMap::new();
    for binding_set in &binding_sets {
        for binding in &binding_set.bindings {
            bindings_map
                .entry(binding.name.clone())
                .or_default()
                .push(binding.var_type_id);
        }
    }

    // Sort by name to ensure consistent ordering (must match generate_pattern_code)
    let mut all_bindings: Vec<(String, usize)> = bindings_map
        .into_iter()
        .map(|(name, types)| (name, union_type_ids(program, types)))
        .collect();
    all_bindings.sort_by(|a, b| a.0.cmp(&b.0));

    // Include [] in the result type if there are runtime requirements (might match)
    let result_type_id = if will_match {
        narrowed_type_id
    } else {
        let nil_id = program.register_type(Type::nil());
        union_type_ids(program, vec![nil_id, narrowed_type_id])
    };

    Ok((all_bindings, binding_sets, result_type_id))
}

/// Generate bytecode for pattern matching
pub fn generate_pattern_code(
    codegen: &mut InstructionBuilder,
    program: &mut Program,
    scopes: &[super::scopes::Scope],
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
                RuntimeCheck::TypeId(type_id) => {
                    generate_value_access(codegen, &requirement.path);
                    codegen.add_instruction(Instruction::IsType(*type_id));
                }
                RuntimeCheck::Literal(literal) => {
                    generate_value_access(codegen, &requirement.path);
                    match literal {
                        ast::Literal::Integer(val) => {
                            let idx = program.register_constant(Constant::Integer(val.clone()));
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
                    let (_var_type, var_index) = super::scopes::lookup_variable(scopes, name, &[])
                        .ok_or_else(|| Error::InternalError {
                            message: format!("Pin variable '{}' not found in scope", name),
                        })?;
                    codegen.add_instruction(Instruction::Load(var_index));
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
        // Sort by name to ensure consistent ordering across binding sets (important for unions
        // where different variants may have bindings in different field orders)
        let mut sorted_bindings: Vec<_> = binding_set.bindings.iter().collect();
        sorted_bindings.sort_by(|a, b| a.name.cmp(&b.name));
        for binding in sorted_bindings {
            generate_value_access(codegen, &binding.path);
            codegen.add_instruction(Instruction::Store);
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
    env: &mut super::typing::TypeEnv,
    program: &mut Program,
    pattern: &ast::Match,
    value_type_id: usize,
    path: AccessPath,
    identifiers: &mut HashMap<String, Identifier>,
    scopes: &[super::scopes::Scope],
    value_provenance: &super::provenance::Provenance,
) -> Result<(Vec<BindingSet>, usize), Error> {
    match pattern {
        ast::Match::Identifier(name, _) => {
            analyze_identifier_pattern(name.clone(), value_type_id, path, identifiers)
        }
        ast::Match::Literal(literal) => {
            analyze_literal_pattern(literal.clone(), path, value_type_id, program)
        }
        ast::Match::Tuple(tuple) => analyze_match_tuple_pattern(
            env,
            program,
            tuple,
            value_type_id,
            path,
            identifiers,
            scopes,
            value_provenance,
        ),
        ast::Match::Partial(partial) => analyze_partial_pattern(
            env,
            program,
            partial,
            value_type_id,
            path,
            identifiers,
            scopes,
            value_provenance,
        ),
        ast::Match::Star(name) => {
            analyze_star_pattern(program, name.as_ref(), value_type_id, path, identifiers)
        }
        ast::Match::Placeholder => Ok((
            vec![BindingSet {
                requirements: vec![],
                bindings: vec![],
            }],
            value_type_id,
        )),
        ast::Match::Reference(ast_type) => {
            // Reference pattern: &<type-expression>
            // Checks against the type (or variable if it's an identifier)

            // Special case: if it's a bare identifier, check if it's a variable first
            if let ast::Type::Identifier { name, arguments } = &ast_type
                && arguments.is_empty()
            {
                // Try to find variable in scopes (with no accessors)
                if let Some((var_type_id, _var_index)) =
                    super::scopes::lookup_variable(scopes, name, &[])
                {
                    // It's a variable reference - check against the variable's value at runtime
                    let requirements = vec![Requirement {
                        path,
                        check: RuntimeCheck::Variable(name.clone()),
                    }];

                    // Narrow the type by filtering compatible variants with the variable's type
                    let narrowed_type_id = intersect_types(value_type_id, var_type_id, program);

                    return Ok((
                        vec![BindingSet {
                            requirements,
                            bindings: vec![],
                        }],
                        narrowed_type_id,
                    ));
                }
            }

            // It's a type reference - resolve the ast::Type to a type ID
            let resolved_type_id =
                super::typing::resolve_ast_type(env, scopes, ast_type.clone(), program)?;

            // Narrow the type by filtering compatible variants
            let narrowed_type_id = intersect_types(value_type_id, resolved_type_id, program);

            // Only add runtime check if value_type is not already exactly the resolved type
            let requirements = if is_compatible(value_type_id, resolved_type_id, program)
                && narrowed_type_id == value_type_id
            {
                vec![] // No runtime check needed - value_type is already compatible
            } else {
                vec![Requirement {
                    path,
                    check: RuntimeCheck::TypeId(resolved_type_id),
                }]
            };

            Ok((
                vec![BindingSet {
                    requirements,
                    bindings: vec![],
                }],
                narrowed_type_id,
            ))
        }
        ast::Match::Type(ast_type) => {
            // Inline type expression without & prefix
            // Resolve the ast::Type to a type ID
            let resolved_type_id =
                super::typing::resolve_ast_type(env, scopes, ast_type.clone(), program)?;

            // Narrow the type by filtering compatible variants
            let narrowed_type_id = intersect_types(value_type_id, resolved_type_id, program);

            // Only add runtime check if value_type is not already exactly the resolved type
            let requirements = if is_compatible(value_type_id, resolved_type_id, program)
                && narrowed_type_id == value_type_id
            {
                vec![] // No runtime check needed - value_type is already compatible
            } else {
                vec![Requirement {
                    path,
                    check: RuntimeCheck::TypeId(resolved_type_id),
                }]
            };

            Ok((
                vec![BindingSet {
                    requirements,
                    bindings: vec![],
                }],
                narrowed_type_id,
            ))
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn analyze_match_tuple_pattern(
    env: &mut super::typing::TypeEnv,
    program: &mut Program,
    tuple: &ast::MatchTuple,
    value_type_id: usize,
    path: AccessPath,
    identifiers: &mut HashMap<String, Identifier>,
    scopes: &[super::scopes::Scope],
    value_provenance: &super::provenance::Provenance,
) -> Result<(Vec<BindingSet>, usize), Error> {
    let mut binding_sets = vec![];
    let mut successful_tuple_ids = vec![];

    // Find matching tuple types
    let matching_types = find_matching_match_tuples(program, tuple, value_type_id)?;

    // For each matching type, create binding sets
    for (tuple_id, field_mappings) in &matching_types {
        // Clone identifiers only if there are multiple variants to avoid cross-contamination
        // For a single variant, use the parent's identifiers directly
        let mut variant_identifiers_scope =
            IdentifierScope::new(identifiers, matching_types.len() > 1);
        let variant_identifiers = variant_identifiers_scope.get_mut();

        // Tuple name and field defs upfront. `narrowed_fields` starts as the static defs and is
        // refined per field as sub-patterns narrow them, so the reconstructed narrowed type
        // carries field-level precision (e.g. `[True, True]`) instead of the opaque tuple — which
        // is what lets `compute_complement` reason about field combinations.
        let (tuple_name, mut narrowed_fields): (Option<String>, Vec<(Option<String>, usize)>) = {
            let tuple_info = program
                .lookup_tuple(*tuple_id)
                .ok_or(Error::TupleNotInRegistry {
                    tuple_id: *tuple_id,
                })?;
            (tuple_info.name.clone(), tuple_info.fields.clone())
        };
        let tuple_fields: Vec<usize> = narrowed_fields
            .iter()
            .map(|(_, type_id)| *type_id)
            .collect();

        // Start with a binding set for this type
        let mut base_requirements = vec![];
        // Add runtime check if needed
        // We need a runtime check if value_type is a union (even if it contains only one tuple type)
        // because the value could be a non-tuple type (like int or bin)
        if is_union(value_type_id, program) || matching_types.len() > 1 {
            // Need to check the type at runtime since value could be one of multiple types
            let tuple_type_id = program.register_type(Type::Tuple(*tuple_id));
            base_requirements.push(Requirement {
                path: path.clone(),
                check: RuntimeCheck::TypeId(tuple_type_id),
            });
        }

        let mut current_binding_sets = vec![BindingSet {
            requirements: base_requirements,
            bindings: vec![],
        }];

        // Process each field pattern
        for (pattern_idx, actual_idx) in field_mappings {
            let field = &tuple.fields[*pattern_idx];
            let raw_field_type_id = tuple_fields[*actual_idx];

            // Resolve Type::Cycle references to the actual type
            // Only Cycle(1) points to the immediate boundary (value_type)
            // Higher depths point to outer boundaries (e.g., enclosing function types)
            // and should be kept as-is since they refer to types outside this tuple
            let mut field_type_id =
                if let Some(Type::Cycle(1)) = program.lookup_type(raw_field_type_id) {
                    value_type_id
                } else {
                    raw_field_type_id
                };

            let mut field_path = path.clone();
            field_path.push(*actual_idx);

            // Check for narrowed field type from complement narrowing.
            // This enables patterns like `=[Cons[...], ys]` in the second branch to know
            // that field 0 has been narrowed to Cons (from previous branch's `=[Nil, ys]`).
            // Only applies when path is empty (we're at the root).
            if path.is_empty()
                && let Some(narrowed_id) =
                    super::narrowing::get_field_narrowing(scopes, value_provenance, *actual_idx)
            {
                // Intersect with the narrowed type
                field_type_id = intersect_types(field_type_id, narrowed_id, program);
            }

            // Recursively analyze the field pattern
            let field_provenance = value_provenance.field(*actual_idx);
            let (field_binding_sets, field_narrowed_type_id) = analyze_match_pattern(
                env,
                program,
                &field.pattern,
                field_type_id,
                field_path,
                variant_identifiers,
                scopes,
                &field_provenance,
            )?;

            if field_binding_sets.is_empty() {
                // This type variant can't match, skip it entirely
                current_binding_sets.clear();
                break;
            }

            // Record the field's narrowed type for the reconstructed tuple. Recursive `Cycle(1)`
            // fields keep their original reference rather than the resolved value type, to avoid
            // materializing an infinite type.
            if !matches!(program.lookup_type(raw_field_type_id), Some(Type::Cycle(1))) {
                narrowed_fields[*actual_idx].1 = field_narrowed_type_id;
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

        // Track this tuple as successful if it produced binding sets, reconstructing it with the
        // narrowed field types so the narrowed result carries field-level precision.
        if !current_binding_sets.is_empty() {
            let narrowed_tuple_id = program.register_tuple(tuple_name, narrowed_fields);
            successful_tuple_ids.push(program.register_type(Type::Tuple(narrowed_tuple_id)));
            binding_sets.extend(current_binding_sets);
        }
    }

    if is_never(value_type_id, program) {
        return Err(Error::InternalError {
            message: format!(
                "analyze_assignment_tuple_pattern received empty type for tuple: {:?}",
                tuple
            ),
        });
    }

    // `successful_tuple_ids` already holds reconstructed `Type::Tuple` ids (narrowed per field).
    let narrowed_type_id = if successful_tuple_ids.is_empty() {
        program.never()
    } else {
        union_type_ids(program, successful_tuple_ids)
    };

    Ok((binding_sets, narrowed_type_id))
}

fn find_matching_match_tuples(
    program: &mut Program,
    tuple: &ast::MatchTuple,
    value_type_id: usize,
) -> Result<TupleMatchResult, Error> {
    let mut matching_types = Vec::new();

    let tuple_ids = extract_tuple_ids(program, value_type_id);

    for tuple_id in tuple_ids {
        if let Some(field_mappings) = check_match_tuple_match(program, tuple, tuple_id)? {
            matching_types.push((tuple_id, field_mappings));
        }
    }

    Ok(matching_types)
}

fn check_match_tuple_match(
    program: &mut Program,
    tuple: &ast::MatchTuple,
    tuple_id: usize,
) -> Result<Option<Vec<(usize, usize)>>, Error> {
    let tuple_info = program
        .lookup_tuple(tuple_id)
        .ok_or(Error::TupleNotInRegistry { tuple_id })?;

    if tuple.name.as_ref() != tuple_info.name.as_ref()
        || tuple.fields.len() != tuple_info.fields.len()
    {
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
    value_type_id: usize,
    program: &mut Program,
) -> Result<(Vec<BindingSet>, usize), Error> {
    // Determine the type of the literal
    let literal_type_id = match &literal {
        ast::Literal::Integer(_) => program.register_type(Type::Integer),
        ast::Literal::Binary(_) => program.register_type(Type::Binary),
    };

    // Narrow the type by filtering compatible variants
    let narrowed_type_id = intersect_types(value_type_id, literal_type_id, program);

    Ok((
        vec![BindingSet {
            requirements: vec![Requirement {
                path,
                check: RuntimeCheck::Literal(literal),
            }],
            bindings: vec![],
        }],
        narrowed_type_id,
    ))
}

fn analyze_identifier_pattern(
    name: String,
    value_type_id: usize,
    path: AccessPath,
    identifiers: &mut HashMap<String, Identifier>,
) -> Result<(Vec<BindingSet>, usize), Error> {
    // Check if we've seen this identifier before
    if let Some(info) = identifiers.get_mut(&name) {
        // Second or later occurrence - mark as repeated and create Path requirement
        // No type narrowing for repeated identifiers (equality check)
        info.is_repeated = true;
        Ok((
            vec![BindingSet {
                requirements: vec![Requirement {
                    path: info.first_path.clone(),
                    check: RuntimeCheck::Path(path),
                }],
                bindings: vec![],
            }],
            value_type_id,
        ))
    } else {
        // First occurrence - record it and create binding
        identifiers.insert(
            name.clone(),
            Identifier {
                first_path: path.clone(),
                is_repeated: false,
            },
        );

        // Binding gets the full value type, including nil if present.
        // The binding executes regardless of whether the value is nil.
        let var_type_id = value_type_id;
        let narrowed_type_id = var_type_id;

        // No runtime requirements for identifier bindings - they match any value
        Ok((
            vec![BindingSet {
                requirements: vec![],
                bindings: vec![Binding {
                    name,
                    path,
                    var_type_id,
                }],
            }],
            narrowed_type_id,
        ))
    }
}

#[allow(clippy::too_many_arguments)]
fn analyze_partial_pattern(
    env: &mut super::typing::TypeEnv,
    program: &mut Program,
    partial_pattern: &ast::PartialPattern,
    value_type_id: usize,
    path: AccessPath,
    identifiers: &mut HashMap<String, Identifier>,
    scopes: &[super::scopes::Scope],
    value_provenance: &super::provenance::Provenance,
) -> Result<(Vec<BindingSet>, usize), Error> {
    let mut binding_sets = vec![];

    // Extract field names from the partial pattern fields
    let field_names: Vec<String> = partial_pattern
        .fields
        .iter()
        .map(|f| f.name.clone())
        .collect();

    // Find types that have all required fields (and optionally matching type name)
    let matching_types = find_types_with_fields_and_name(
        program,
        &field_names,
        partial_pattern.name.as_ref(),
        value_type_id,
    )?;

    // Check if the original value type could be multiple types (for adding type checks)
    let value_type_sources = extract_field_sources(program, value_type_id);
    let needs_type_check = value_type_sources.len() > 1;

    // Create a binding set for each matching type
    for field_match in &matching_types {
        // Clone identifiers only if there are multiple variants to avoid cross-contamination
        let mut variant_identifiers_scope =
            IdentifierScope::new(identifiers, matching_types.len() > 1);
        let variant_identifiers = variant_identifiers_scope.get_mut();

        let mut requirements = vec![];

        // Get field info based on match type
        let (fields, field_indices): (Vec<(Option<String>, usize)>, &Vec<usize>) = match field_match
        {
            FieldMatch::Tuple {
                tuple_id,
                field_indices,
            } => {
                // Add type check if the value could be multiple types
                if needs_type_check {
                    let tuple_type_id = program.register_type(Type::Tuple(*tuple_id));
                    requirements.push(Requirement {
                        path: path.clone(),
                        check: RuntimeCheck::TypeId(tuple_type_id),
                    });
                }

                let tuple_fields = program
                    .lookup_tuple(*tuple_id)
                    .ok_or(Error::TupleNotInRegistry {
                        tuple_id: *tuple_id,
                    })?
                    .fields
                    .clone();
                (tuple_fields, field_indices)
            }
            FieldMatch::Partial {
                fields,
                field_indices,
            } => {
                // Convert partial fields to (Option<String>, usize) format
                let converted: Vec<(Option<String>, usize)> = fields
                    .iter()
                    .map(|(name, type_id)| (Some(name.clone()), *type_id))
                    .collect();
                // No type check needed for partials - they're type constraints, not concrete types
                (converted, field_indices)
            }
        };

        let mut bindings = Vec::new();
        for (i, partial_field) in partial_pattern.fields.iter().enumerate() {
            let field_name = &partial_field.name;
            let nested_pattern = &partial_field.pattern;
            let idx = field_indices[i];
            let field_type_id = fields[idx].1;
            let mut field_path = path.clone();
            field_path.push(idx);

            // If the field has a nested pattern, analyze it recursively
            if let Some(nested_pattern) = nested_pattern {
                let field_provenance = value_provenance.field(idx);
                let (nested_binding_sets, _) = analyze_match_pattern(
                    env,
                    program,
                    nested_pattern,
                    field_type_id,
                    field_path,
                    variant_identifiers,
                    scopes,
                    &field_provenance,
                )?;

                // Merge nested binding sets into current requirements and bindings
                for nested_set in nested_binding_sets {
                    requirements.extend(nested_set.requirements);
                    bindings.extend(nested_set.bindings);
                }
            } else {
                // No nested pattern - use field name for binding/checking

                // Check if we've seen this identifier before
                if let Some(info) = variant_identifiers.get_mut(field_name) {
                    // Repeated identifier - mark as repeated and add Path requirement
                    info.is_repeated = true;
                    requirements.push(Requirement {
                        path: info.first_path.clone(),
                        check: RuntimeCheck::Path(field_path),
                    });
                } else {
                    // First occurrence - record it
                    variant_identifiers.insert(
                        field_name.clone(),
                        Identifier {
                            first_path: field_path.clone(),
                            is_repeated: false,
                        },
                    );

                    // Create a binding for this identifier
                    // Strip [] from binding type because if the binding executes, the value is not []
                    let var_type_id = without_nil(field_type_id, program);
                    bindings.push(Binding {
                        name: field_name.clone(),
                        path: field_path,
                        var_type_id,
                    });
                }
            }
        }

        binding_sets.push(BindingSet {
            requirements,
            bindings,
        });
    }

    // Construct narrowed type from matching types
    // For tuples, narrow to concrete tuple types; for partials, keep the input type
    let narrowed_type_id = if matching_types.is_empty() {
        program.never()
    } else {
        let mut narrowed_type_ids: Vec<usize> = Vec::new();
        for field_match in &matching_types {
            match field_match {
                FieldMatch::Tuple { tuple_id, .. } => {
                    narrowed_type_ids.push(program.register_type(Type::Tuple(*tuple_id)));
                }
                FieldMatch::Partial { .. } => {
                    // For partials, keep the original partial type
                    narrowed_type_ids.push(value_type_id);
                }
            }
        }
        union_type_ids(program, narrowed_type_ids)
    };

    Ok((binding_sets, narrowed_type_id))
}

fn analyze_star_pattern(
    program: &mut Program,
    name: Option<&String>,
    value_type_id: usize,
    path: AccessPath,
    identifiers: &mut HashMap<String, Identifier>,
) -> Result<(Vec<BindingSet>, usize), Error> {
    // Collect all field sources (tuples and partials)
    let all_sources = extract_field_sources(program, value_type_id);

    // When a name is given, keep only the variants carrying that tuple name (`Config*`).
    let field_sources: Vec<FieldSource> = match name {
        None => all_sources.clone(),
        Some(expected) => all_sources
            .iter()
            .filter(|source| field_source_name(program, source).as_ref() == Some(expected))
            .cloned()
            .collect(),
    };

    // If the value could be one of several variants at runtime, a type check is needed to
    // discriminate the matching variant (and, for a named star, to enforce the name).
    let needs_type_check = all_sources.len() > 1;

    // Create a binding set for each matching field source
    let mut binding_sets = vec![];
    let mut narrowed_type_ids: Vec<usize> = vec![];
    for source in &field_sources {
        // Clone identifiers only if there are multiple variants to avoid cross-contamination
        let mut variant_identifiers_scope =
            IdentifierScope::new(identifiers, field_sources.len() > 1);
        let variant_identifiers = variant_identifiers_scope.get_mut();

        // Get fields from the source
        let (fields, type_check): (Vec<(Option<String>, usize)>, Option<usize>) = match source {
            FieldSource::Tuple(tuple_id) => {
                let tuple_fields = program
                    .lookup_tuple(*tuple_id)
                    .ok_or(Error::TupleNotInRegistry {
                        tuple_id: *tuple_id,
                    })?
                    .fields
                    .clone();
                let tuple_type_id = program.register_type(Type::Tuple(*tuple_id));
                narrowed_type_ids.push(tuple_type_id);
                let type_check = needs_type_check.then_some(tuple_type_id);
                (tuple_fields, type_check)
            }
            FieldSource::Partial { fields, .. } => {
                // Partial fields are all named, convert to (Option<String>, usize) format
                let converted: Vec<(Option<String>, usize)> = fields
                    .iter()
                    .map(|(name, type_id)| (Some(name.clone()), *type_id))
                    .collect();
                // No type check needed for partials since they're type constraints
                narrowed_type_ids.push(value_type_id);
                (converted, None)
            }
        };

        // Add type check if needed
        let mut requirements = vec![];
        if let Some(type_id) = type_check {
            requirements.push(Requirement {
                path: path.clone(),
                check: RuntimeCheck::TypeId(type_id),
            });
        }

        let mut bindings = Vec::new();
        for (idx, (name, field_type_id)) in fields.iter().enumerate() {
            if let Some(field_name) = name {
                let mut field_path = path.clone();
                field_path.push(idx);

                // Check if we've seen this identifier before
                if let Some(info) = variant_identifiers.get_mut(field_name.as_str()) {
                    // Repeated identifier - mark as repeated and add Path requirement
                    info.is_repeated = true;
                    requirements.push(Requirement {
                        path: info.first_path.clone(),
                        check: RuntimeCheck::Path(field_path),
                    });
                } else {
                    // First occurrence - record it
                    variant_identifiers.insert(
                        field_name.clone(),
                        Identifier {
                            first_path: field_path.clone(),
                            is_repeated: false,
                        },
                    );

                    // Create a binding for this identifier
                    // Strip [] from binding type because if the binding executes, the value is not []
                    let var_type_id = without_nil(*field_type_id, program);
                    bindings.push(Binding {
                        name: field_name.clone(),
                        path: field_path,
                        var_type_id,
                    });
                }
            }
        }

        binding_sets.push(BindingSet {
            requirements,
            bindings,
        });
    }

    // An unnamed star matches every variant, so the type is unchanged. A named star narrows
    // to the matching variants (or never, if none carry the name).
    let narrowed_type_id = match name {
        None => value_type_id,
        Some(_) if narrowed_type_ids.is_empty() => program.never(),
        Some(_) => union_type_ids(program, narrowed_type_ids),
    };
    Ok((binding_sets, narrowed_type_id))
}

/// The tuple name carried by a field source, if any.
fn field_source_name(program: &Program, source: &FieldSource) -> Option<String> {
    match source {
        FieldSource::Tuple(tuple_id) => {
            program.lookup_tuple(*tuple_id).and_then(|t| t.name.clone())
        }
        FieldSource::Partial { name, .. } => name.clone(),
    }
}

/// Represents a match against either a concrete tuple or a partial type
enum FieldMatch {
    Tuple {
        tuple_id: usize,
        field_indices: Vec<usize>,
    },
    Partial {
        fields: Vec<(String, usize)>, // (field_name, type_id)
        field_indices: Vec<usize>,
    },
}

fn find_types_with_fields_and_name(
    program: &mut Program,
    field_names: &[String],
    type_name: Option<&String>,
    value_type_id: usize,
) -> Result<Vec<FieldMatch>, Error> {
    let mut matches = Vec::new();

    let field_sources = extract_field_sources(program, value_type_id);

    for source in field_sources {
        match source {
            FieldSource::Tuple(tuple_id) => {
                let tuple_info = program
                    .lookup_tuple(tuple_id)
                    .ok_or(Error::TupleNotInRegistry { tuple_id })?;

                // Check if tuple name matches (if specified)
                if let Some(expected_name) = type_name
                    && tuple_info.name.as_ref() != Some(expected_name)
                {
                    continue; // Skip this type if name doesn't match
                }

                if let Some(indices) = find_field_indices(field_names, &tuple_info.fields) {
                    matches.push(FieldMatch::Tuple {
                        tuple_id,
                        field_indices: indices,
                    });
                }
            }
            FieldSource::Partial { name, fields } => {
                // Check if partial name matches (if specified)
                if let Some(expected_name) = type_name
                    && name.as_ref() != Some(expected_name)
                {
                    continue; // Skip this partial if name doesn't match
                }

                // Convert partial fields to (Option<String>, usize) format for find_field_indices
                let converted_fields: Vec<(Option<String>, usize)> = fields
                    .iter()
                    .map(|(name, type_id)| (Some(name.clone()), *type_id))
                    .collect();

                if let Some(indices) = find_field_indices(field_names, &converted_fields) {
                    matches.push(FieldMatch::Partial {
                        fields,
                        field_indices: indices,
                    });
                }
            }
        }
    }

    Ok(matches)
}

/// Find indices of specified fields in a tuple type
fn find_field_indices(
    field_names: &[String],
    tuple_fields: &[(Option<String>, usize)],
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

// ============================================================================
// Helper functions for ID-based type operations
// ============================================================================

/// Represents field information from either a tuple or partial type
#[derive(Clone)]
enum FieldSource {
    Tuple(usize), // tuple_id
    Partial {
        name: Option<String>,
        fields: Vec<(String, usize)>, // (field_name, type_id) - all partial fields are named
    },
}

/// Extract field sources from a type (tuples and partials)
fn extract_field_sources(program: &Program, type_id: usize) -> Vec<FieldSource> {
    let Some(ty) = program.lookup_type(type_id) else {
        return vec![];
    };
    match ty {
        Type::Tuple(id) => vec![FieldSource::Tuple(*id)],
        Type::Partial { name, fields } => vec![FieldSource::Partial {
            name: name.clone(),
            fields: fields.clone(),
        }],
        Type::Union(type_ids) => type_ids
            .iter()
            .flat_map(|&tid| extract_field_sources(program, tid))
            .collect(),
        _ => vec![],
    }
}

/// Extract tuple IDs from a type (only concrete tuples, not partials)
fn extract_tuple_ids(program: &Program, type_id: usize) -> Vec<usize> {
    let Some(ty) = program.lookup_type(type_id) else {
        return vec![];
    };
    match ty {
        Type::Tuple(id) => vec![*id],
        Type::Union(type_ids) => type_ids
            .iter()
            .filter_map(|&tid| {
                program.lookup_type(tid).and_then(|t| match t {
                    Type::Tuple(id) => Some(*id),
                    _ => None,
                })
            })
            .collect(),
        _ => vec![],
    }
}

/// Check if a type is a union
fn is_union(type_id: usize, program: &Program) -> bool {
    matches!(program.lookup_type(type_id), Some(Type::Union(_)))
}

/// Check if a type is the never type (empty union)
fn is_never(type_id: usize, program: &Program) -> bool {
    matches!(program.lookup_type(type_id), Some(Type::Union(ids)) if ids.is_empty())
}

/// Check if type a is compatible with type b (simplified version for pattern matching)
fn is_compatible(a_id: usize, b_id: usize, program: &Program) -> bool {
    if a_id == b_id {
        return true;
    }

    let (Some(a), Some(b)) = (program.lookup_type(a_id), program.lookup_type(b_id)) else {
        return false;
    };

    match (a, b) {
        (Type::Integer, Type::Integer) => true,
        (Type::Binary, Type::Binary) => true,
        (Type::Tuple(id1), Type::Tuple(id2)) => id1 == id2,
        (Type::Union(ids), _) => ids.iter().all(|&id| is_compatible(id, b_id, program)),
        (_, Type::Union(ids)) => ids.iter().any(|&id| is_compatible(a_id, id, program)),
        // For partial compatibility, use the full is_compatible from types module
        _ => quiver_core::types::is_compatible(a_id, b_id, program),
    }
}

/// Remove nil from a type (for bindings that strip nil)
fn without_nil(type_id: usize, program: &mut Program) -> usize {
    let Some(ty) = program.lookup_type(type_id) else {
        return type_id;
    };

    match ty {
        Type::Union(ids) => {
            let ids = ids.clone();
            let filtered: Vec<usize> = ids
                .into_iter()
                .filter(|&id| {
                    if let Some(Type::Tuple(tuple_id)) = program.lookup_type(id) {
                        // Check if this is the nil tuple (empty tuple with no name)
                        if let Some(info) = program.lookup_tuple(*tuple_id) {
                            !(info.fields.is_empty() && info.name.is_none())
                        } else {
                            true
                        }
                    } else {
                        true
                    }
                })
                .collect();
            union_type_ids(program, filtered)
        }
        Type::Tuple(tuple_id) => {
            // Check if this is nil
            if let Some(info) = program.lookup_tuple(*tuple_id)
                && info.fields.is_empty()
                && info.name.is_none()
            {
                return program.never();
            }
            type_id
        }
        _ => type_id,
    }
}
