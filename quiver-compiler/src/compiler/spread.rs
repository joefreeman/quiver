use crate::ast;
use quiver_core::{
    bytecode::{Instruction, TypeId},
    program::Program,
    types::{Type, TypeLookup},
};

use super::{Compiler, Error};

pub fn compile_tuple_with_spread(
    compiler: &mut Compiler,
    tuple_name: Option<String>,
    fields: Vec<ast::TupleField>,
    value_type: Option<Type>,
) -> Result<Type, Error> {
    let contains_ripple = Compiler::tuple_contains_ripple(&fields);
    let has_chained_spread = fields.iter().any(|f| match &f.value {
        ast::FieldValue::Spread(None) => true,
        ast::FieldValue::Spread(Some(id)) if id == "~" => true,
        _ => false,
    });

    if has_chained_spread && value_type.is_none() {
        return Err(Error::FeatureUnsupported(
            "Chained spread (...) can only be used when a value is piped into the tuple"
                .to_string(),
        ));
    }

    // Set up ripple context if needed (for both ripples and chained spreads)
    let has_ripple_value = if (contains_ripple || has_chained_spread) && value_type.is_some() {
        compiler.ripple_types.push(value_type.clone().unwrap());
        true
    } else {
        false
    };

    // Step 1: Compile all field values and track their types
    #[derive(Debug)]
    enum CompiledValue {
        Field {
            name: Option<String>,
            ty: Type,
            stack_offset: usize,
        },
        Spread {
            ty: Type,
            stack_offset: usize,
        },
    }

    let mut compiled_values = Vec::new();
    let mut stack_size = 0;

    // Helper to get stack offset from CompiledValue
    let get_stack_offset = |cv: &CompiledValue| -> usize {
        match cv {
            CompiledValue::Field { stack_offset, .. } => *stack_offset,
            CompiledValue::Spread { stack_offset, .. } => *stack_offset,
        }
    };

    for field in &fields {
        match &field.value {
            ast::FieldValue::Chain(chain) => {
                let ty = compiler.compile_chain(chain.clone(), None)?;
                compiled_values.push(CompiledValue::Field {
                    name: field.name.clone(),
                    ty,
                    stack_offset: stack_size,
                });
                stack_size += 1;
            }
            ast::FieldValue::Ripple => {
                let ripple_type = compiler.ripple_types.last().ok_or_else(|| {
                    Error::FeatureUnsupported(
                        "Ripple cannot be used outside of an operation context".to_string(),
                    )
                })?;
                compiler
                    .codegen
                    .add_instruction(Instruction::Pick(stack_size));
                compiled_values.push(CompiledValue::Field {
                    name: field.name.clone(),
                    ty: ripple_type.clone(),
                    stack_offset: stack_size,
                });
                stack_size += 1;
            }
            ast::FieldValue::Spread(identifier) => {
                // Get the type of the value to spread
                let spread_type = if let Some(id) = identifier {
                    if id == "~" {
                        // Spread the ripple value (~)
                        let ripple_type = compiler.ripple_types.last().ok_or_else(|| {
                            Error::FeatureUnsupported(
                                "Ripple spread (~) requires a piped value".to_string(),
                            )
                        })?;
                        compiler
                            .codegen
                            .add_instruction(Instruction::Pick(stack_size));
                        ripple_type.clone()
                    } else {
                        // Spread a variable
                        let (var_type, var_index) = compiler
                            .lookup_variable(&compiler.scopes, id, &[])
                            .ok_or_else(|| Error::VariableUndefined(id.clone()))?;
                        compiler
                            .codegen
                            .add_instruction(Instruction::Load(var_index));
                        var_type
                    }
                } else {
                    // Spread the chained value (...)
                    let ripple_type = compiler.ripple_types.last().ok_or_else(|| {
                        Error::FeatureUnsupported(
                            "Chained spread (...) requires a piped value".to_string(),
                        )
                    })?;
                    compiler
                        .codegen
                        .add_instruction(Instruction::Pick(stack_size));
                    ripple_type.clone()
                };

                compiled_values.push(CompiledValue::Spread {
                    ty: spread_type,
                    stack_offset: stack_size,
                });
                stack_size += 1;
            }
        }
    }

    // Step 2: Resolve final field layout
    // For union spreads, we need to compute all possible result variants (cartesian product)

    // Build index mappings for cleaner lookup
    let mut field_to_compiled: Vec<usize> = Vec::new();
    let mut spread_to_compiled: Vec<usize> = Vec::new();

    for (compiled_idx, compiled) in compiled_values.iter().enumerate() {
        match compiled {
            CompiledValue::Field { .. } => field_to_compiled.push(compiled_idx),
            CompiledValue::Spread { .. } => spread_to_compiled.push(compiled_idx),
        }
    }

    // Structure to hold information about each result variant
    #[derive(Debug, Clone)]
    struct VariantInfo {
        fields: Vec<(Option<String>, Type)>,
        spread_type_ids: Vec<TypeId>, // Which type ID each spread resolves to for this variant
    }

    let mut variants = vec![VariantInfo {
        fields: Vec::new(),
        spread_type_ids: Vec::new(),
    }];

    // Process fields left to right, maintaining insertion order
    let mut field_count = 0;
    let mut spread_count = 0;

    for field in &fields {
        match &field.value {
            ast::FieldValue::Chain(_) | ast::FieldValue::Ripple => {
                let compiled_idx = field_to_compiled[field_count];
                field_count += 1;

                if let CompiledValue::Field { name, ty, .. } = &compiled_values[compiled_idx] {
                    // Add this field to all variants
                    for variant in &mut variants {
                        // Check if this named field should replace an existing one
                        if let Some(field_name) = name
                            && let Some(pos) = variant
                                .fields
                                .iter()
                                .position(|(n, _)| n.as_ref() == Some(field_name))
                        {
                            variant.fields[pos].1 = ty.clone();
                            continue;
                        }
                        variant.fields.push((name.clone(), ty.clone()));
                    }
                }
            }
            ast::FieldValue::Spread(_) => {
                let compiled_idx = spread_to_compiled[spread_count];
                spread_count += 1;

                if let CompiledValue::Spread { ty, .. } = &compiled_values[compiled_idx] {
                    let spread_type_ids = ty.extract_tuple_types();

                    if spread_type_ids.is_empty() {
                        return Err(Error::TypeMismatch {
                            expected: "tuple".to_string(),
                            found: format!("{:?}", ty),
                        });
                    }

                    // For each existing variant, create new variants for each spread type
                    let mut new_variants = Vec::new();

                    for existing_variant in &variants {
                        for &spread_type_id in &spread_type_ids {
                            let spread_info = compiler.program.lookup_type(&spread_type_id).ok_or(
                                Error::TypeNotInRegistry {
                                    type_id: spread_type_id,
                                },
                            )?;

                            let mut new_variant = existing_variant.clone();
                            new_variant.spread_type_ids.push(spread_type_id);

                            // Track where to insert new fields (at current position)
                            let insertion_point = new_variant.fields.len();
                            let mut inserted_count = 0;

                            // Add each field from the spread
                            for (field_name, field_type) in &spread_info.fields {
                                if let Some(name) = field_name
                                    && let Some(pos) = new_variant
                                        .fields
                                        .iter()
                                        .position(|(n, _)| n.as_ref() == Some(name))
                                {
                                    // Field exists - replace it
                                    new_variant.fields[pos].1 = field_type.clone();
                                    continue;
                                }
                                // Field doesn't exist - insert at insertion point
                                new_variant.fields.insert(
                                    insertion_point + inserted_count,
                                    (field_name.clone(), field_type.clone()),
                                );
                                inserted_count += 1;
                            }

                            new_variants.push(new_variant);
                        }
                    }

                    variants = new_variants;
                }
            }
        }
    }

    // Step 3: Generate bytecode for spread operations

    // Track which final field comes from which source
    #[derive(Debug, Clone)]
    enum FieldSource {
        CompiledField(usize), // Index into compiled_values (and stack)
        SpreadField { spread_idx: usize, field_idx: usize },
    }

    // Helper to build field sources for a specific variant
    let build_field_sources = |variant: &VariantInfo,
                               program: &Program|
     -> Vec<Option<FieldSource>> {
        let mut field_sources: Vec<Option<FieldSource>> = vec![None; variant.fields.len()];
        let mut unnamed_sources: Vec<FieldSource> = Vec::new();

        // Collect all unnamed field sources in order
        let mut spread_variant_idx = 0;
        for (compiled_idx, compiled) in compiled_values.iter().enumerate() {
            match compiled {
                CompiledValue::Field { name: None, .. } => {
                    unnamed_sources.push(FieldSource::CompiledField(compiled_idx));
                }
                CompiledValue::Spread { .. } => {
                    let spread_type_id = variant.spread_type_ids[spread_variant_idx];
                    let spread_info = program.lookup_type(&spread_type_id).unwrap();

                    for (field_idx, (spread_field_name, _)) in spread_info.fields.iter().enumerate()
                    {
                        if spread_field_name.is_none() {
                            unnamed_sources.push(FieldSource::SpreadField {
                                spread_idx: compiled_idx,
                                field_idx,
                            });
                        }
                    }
                    spread_variant_idx += 1;
                }
                _ => {}
            }
        }

        // Map final fields to sources
        let mut unnamed_idx = 0;
        for (final_idx, (final_name, _)) in variant.fields.iter().enumerate() {
            if let Some(name) = final_name {
                // Named field - find the rightmost source
                let mut local_spread_idx = 0;
                for (compiled_idx, compiled) in compiled_values.iter().enumerate() {
                    match compiled {
                        CompiledValue::Field {
                            name: field_name, ..
                        } => {
                            if field_name.as_ref() == Some(name) {
                                field_sources[final_idx] =
                                    Some(FieldSource::CompiledField(compiled_idx));
                            }
                        }
                        CompiledValue::Spread { .. } => {
                            let spread_type_id = variant.spread_type_ids[local_spread_idx];
                            let spread_info = program.lookup_type(&spread_type_id).unwrap();

                            for (field_idx, (spread_field_name, _)) in
                                spread_info.fields.iter().enumerate()
                            {
                                if spread_field_name.as_ref() == Some(name) {
                                    field_sources[final_idx] = Some(FieldSource::SpreadField {
                                        spread_idx: compiled_idx,
                                        field_idx,
                                    });
                                    break;
                                }
                            }
                            local_spread_idx += 1;
                        }
                    }
                }
            } else {
                // Unnamed field - use next from unnamed_sources
                if unnamed_idx < unnamed_sources.len() {
                    field_sources[final_idx] = Some(unnamed_sources[unnamed_idx].clone());
                    unnamed_idx += 1;
                }
            }
        }

        field_sources
    };

    // Helper to emit field extraction instructions
    let emit_field_extraction = |compiler: &mut Compiler,
                                 field_sources: &[Option<FieldSource>]|
     -> Result<(), Error> {
        let mut values_added = 0;
        for source in field_sources {
            let source = source.as_ref().ok_or_else(|| {
                Error::FeatureUnsupported("Failed to resolve field source in spread".to_string())
            })?;

            match source {
                FieldSource::CompiledField(idx) => {
                    let depth =
                        stack_size + values_added - 1 - get_stack_offset(&compiled_values[*idx]);
                    compiler.codegen.add_instruction(Instruction::Pick(depth));
                    values_added += 1;
                }
                FieldSource::SpreadField {
                    spread_idx,
                    field_idx,
                } => {
                    let depth = stack_size + values_added
                        - 1
                        - get_stack_offset(&compiled_values[*spread_idx]);
                    compiler.codegen.add_instruction(Instruction::Pick(depth));
                    compiler
                        .codegen
                        .add_instruction(Instruction::Get(*field_idx));
                    values_added += 1;
                }
            }
        }
        Ok(())
    };

    // Helper to emit stack cleanup instructions
    let emit_stack_cleanup = |compiler: &mut Compiler, count: usize| {
        for _ in 0..count {
            compiler.codegen.add_instruction(Instruction::Swap);
            compiler.codegen.add_instruction(Instruction::Pop);
        }
    };

    // Emit instructions based on number of variants
    let result_type = if variants.len() == 1 {
        // Single variant - generate simple linear code
        let variant = &variants[0];
        let field_sources = build_field_sources(variant, &compiler.program);
        emit_field_extraction(compiler, &field_sources)?;

        // Register and create tuple
        let type_id = compiler
            .program
            .register_type(tuple_name.clone(), variant.fields.clone());
        compiler
            .codegen
            .add_instruction(Instruction::Tuple(type_id));

        // Clean up original values
        emit_stack_cleanup(compiler, stack_size);

        Type::Tuple(type_id)
    } else {
        // Multiple variants - generate branching code
        let mut end_jumps = Vec::new();

        for (variant_idx, variant) in variants.iter().enumerate() {
            let is_last = variant_idx == variants.len() - 1;
            let field_sources = build_field_sources(variant, &compiler.program);

            if !is_last {
                // Check if spreads match this variant's types
                // We need to check ALL spreads - if ANY doesn't match, try next variant
                let mut fail_jumps = Vec::new();

                for (spread_field_idx, spread_compiled_idx) in compiled_values
                    .iter()
                    .enumerate()
                    .filter(|(_, cv)| matches!(cv, CompiledValue::Spread { .. }))
                    .enumerate()
                {
                    let spread_type_id = variant.spread_type_ids[spread_field_idx];
                    let spread_stack_idx =
                        get_stack_offset(&compiled_values[spread_compiled_idx.0]);

                    // Pick the spread value and check its type
                    let depth = stack_size - 1 - spread_stack_idx;
                    compiler.codegen.add_instruction(Instruction::Pick(depth));
                    compiler
                        .codegen
                        .add_instruction(Instruction::IsTuple(spread_type_id));

                    // IsTuple returns Ok if match, [] if no match
                    // We want to jump if it DOESN'T match (returns [])
                    // So negate and then JumpIf (which consumes the value)
                    compiler.codegen.add_instruction(Instruction::Not);

                    // JumpIf consumes the boolean, so no Pop needed
                    let fail_jump = compiler.codegen.emit_jump_if_placeholder();
                    fail_jumps.push(fail_jump);
                }

                // All checks passed - construct this variant
                emit_field_extraction(compiler, &field_sources)?;

                // Create tuple for this variant
                let type_id = compiler
                    .program
                    .register_type(tuple_name.clone(), variant.fields.clone());
                compiler
                    .codegen
                    .add_instruction(Instruction::Tuple(type_id));

                // Jump to end
                end_jumps.push(compiler.codegen.emit_jump_placeholder());

                // Patch all fail jumps to here (next variant)
                for fail_jump in fail_jumps {
                    compiler.codegen.patch_jump_to_here(fail_jump);
                }
            } else {
                // Last variant - no need to check, just construct it
                emit_field_extraction(compiler, &field_sources)?;

                let type_id = compiler
                    .program
                    .register_type(tuple_name.clone(), variant.fields.clone());
                compiler
                    .codegen
                    .add_instruction(Instruction::Tuple(type_id));
            }
        }

        // Patch all end jumps
        for jump in end_jumps {
            compiler.codegen.patch_jump_to_here(jump);
        }

        // Clean up original values
        emit_stack_cleanup(compiler, stack_size);

        // Build union type from all variants
        let union_types: Vec<Type> = variants
            .iter()
            .map(|variant| {
                let type_id = compiler
                    .program
                    .register_type(tuple_name.clone(), variant.fields.clone());
                Type::Tuple(type_id)
            })
            .collect();

        Type::from_types(union_types)
    };

    // Clean up ripple context
    if has_ripple_value {
        compiler.codegen.add_instruction(Instruction::Swap);
        compiler.codegen.add_instruction(Instruction::Pop);
        compiler.ripple_types.pop();
    }

    Ok(result_type)
}
