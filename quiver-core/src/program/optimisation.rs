use crate::bytecode::{Bytecode, Constant, Function, Instruction, TypeId};
use crate::types::TupleTypeInfo;
use std::collections::HashMap;

/// Perform tree shaking on program data to remove unused functions, constants, types, and builtins
/// Returns the collections needed to build an optimized Bytecode
pub fn tree_shake(
    functions: &[Function],
    constants: &[Constant],
    types: &[TupleTypeInfo],
    builtins: &[String],
    entry_fn: usize,
) -> Bytecode {
    // Mark phase: find all reachable functions, constants, types, and builtins
    let mut used_functions = vec![false; functions.len()];
    let mut used_constants = vec![false; constants.len()];
    let mut used_types = vec![false; types.len()];
    let mut used_builtins = vec![false; builtins.len()];

    // Always mark NIL and OK as used (they're built-in control flow types)
    if TypeId::NIL.0 < used_types.len() {
        used_types[TypeId::NIL.0] = true;
    }
    if TypeId::OK.0 < used_types.len() {
        used_types[TypeId::OK.0] = true;
    }

    let mut queue = vec![entry_fn];

    while let Some(fn_id) = queue.pop() {
        // Skip if already processed
        if fn_id >= used_functions.len() || used_functions[fn_id] {
            continue;
        }
        used_functions[fn_id] = true;

        // Get the function (if it exists)
        let Some(function) = functions.get(fn_id) else {
            continue;
        };

        // Scan instructions for references
        for instruction in &function.instructions {
            match instruction {
                Instruction::Function(id) => {
                    queue.push(*id);
                }
                Instruction::Constant(id) => {
                    if *id < used_constants.len() {
                        used_constants[*id] = true;
                    }
                }
                Instruction::Tuple(type_id) | Instruction::IsTuple(type_id) => {
                    if type_id.0 < used_types.len() {
                        used_types[type_id.0] = true;
                    }
                }
                Instruction::Builtin(id) => {
                    if *id < used_builtins.len() {
                        used_builtins[*id] = true;
                    }
                }
                _ => {}
            }
        }
    }

    // If nothing is used (shouldn't happen), return original without tree shaking
    if !used_functions.iter().any(|&used| used) {
        return Bytecode {
            constants: constants.to_vec(),
            functions: functions.to_vec(),
            builtins: builtins.to_vec(),
            entry: Some(entry_fn),
            types: types.to_vec(),
        };
    }

    // Sweep phase: build remapping tables and collect used items
    let mut function_remap = HashMap::new();
    let mut new_functions = Vec::new();

    for (old_id, function) in functions.iter().enumerate() {
        if used_functions[old_id] {
            function_remap.insert(old_id, new_functions.len());
            new_functions.push(function.clone());
        }
    }

    let mut constant_remap = HashMap::new();
    let mut new_constants = Vec::new();

    for (old_id, constant) in constants.iter().enumerate() {
        if used_constants[old_id] {
            constant_remap.insert(old_id, new_constants.len());
            new_constants.push(constant.clone());
        }
    }

    let mut builtin_remap = HashMap::new();
    let mut new_builtins = Vec::new();

    for (old_id, builtin) in builtins.iter().enumerate() {
        if used_builtins[old_id] {
            builtin_remap.insert(old_id, new_builtins.len());
            new_builtins.push(builtin.clone());
        }
    }

    let mut type_remap = HashMap::new();
    let mut new_types = Vec::new();

    for (old_id, type_info) in types.iter().enumerate() {
        if used_types[old_id] {
            type_remap.insert(TypeId(old_id), TypeId(new_types.len()));
            new_types.push(type_info.clone());
        }
    }

    // Remap phase: update function, constant, builtin, and type indices
    let remapped_functions = new_functions
        .into_iter()
        .map(|mut function| {
            function.instructions = function
                .instructions
                .into_iter()
                .map(|instruction| match instruction {
                    Instruction::Function(old_id) => {
                        Instruction::Function(*function_remap.get(&old_id).unwrap_or(&old_id))
                    }
                    Instruction::Constant(old_id) => {
                        Instruction::Constant(*constant_remap.get(&old_id).unwrap_or(&old_id))
                    }
                    Instruction::Builtin(old_id) => {
                        Instruction::Builtin(*builtin_remap.get(&old_id).unwrap_or(&old_id))
                    }
                    Instruction::Tuple(old_type_id) => {
                        Instruction::Tuple(*type_remap.get(&old_type_id).unwrap_or(&old_type_id))
                    }
                    Instruction::IsTuple(old_type_id) => {
                        Instruction::IsTuple(*type_remap.get(&old_type_id).unwrap_or(&old_type_id))
                    }
                    other => other,
                })
                .collect();
            function
        })
        .collect();

    // Remap entry point
    let new_entry = function_remap.get(&entry_fn).copied();

    Bytecode {
        constants: new_constants,
        functions: remapped_functions,
        builtins: new_builtins,
        entry: new_entry,
        types: new_types,
    }
}
