use super::{Bytecode, Instruction, TypeId};
use std::collections::{HashMap, HashSet};

pub fn tree_shake(bytecode: Bytecode) -> Bytecode {
    // If there's no entry point, return as-is
    let Some(entry_fn) = bytecode.entry else {
        return bytecode;
    };

    // Mark phase: find all reachable functions, constants, and types
    let mut used_functions = HashSet::new();
    let mut used_constants = HashSet::new();
    let mut used_types = HashSet::new();

    // Always mark NIL and OK as used (they're built-in control flow types)
    used_types.insert(TypeId::NIL);
    used_types.insert(TypeId::OK);

    let mut queue = vec![entry_fn];

    while let Some(fn_id) = queue.pop() {
        // Skip if already processed
        if !used_functions.insert(fn_id) {
            continue;
        }

        // Get the function (if it exists)
        let Some(function) = bytecode.functions.get(fn_id) else {
            continue;
        };

        // Scan instructions for references
        for instruction in &function.instructions {
            match instruction {
                Instruction::Function(id) => {
                    queue.push(*id);
                }
                Instruction::Constant(id) => {
                    used_constants.insert(*id);
                }
                Instruction::Tuple(type_id) | Instruction::IsTuple(type_id) => {
                    used_types.insert(*type_id);
                }
                _ => {}
            }
        }
    }

    // If nothing is used (shouldn't happen), return original
    if used_functions.is_empty() {
        return bytecode;
    }

    // Sweep phase: build remapping tables
    let mut function_remap = HashMap::new();
    let mut new_functions = Vec::new();

    for (old_id, function) in bytecode.functions.into_iter().enumerate() {
        if used_functions.contains(&old_id) {
            function_remap.insert(old_id, new_functions.len());
            new_functions.push(function);
        }
    }

    let mut constant_remap = HashMap::new();
    let mut new_constants = Vec::new();

    for (old_id, constant) in bytecode.constants.into_iter().enumerate() {
        if used_constants.contains(&old_id) {
            constant_remap.insert(old_id, new_constants.len());
            new_constants.push(constant);
        }
    }

    // Filter types: keep only used types without remapping IDs
    // This avoids having to update TypeId references within type definitions
    let new_types: HashMap<_, _> = bytecode
        .types
        .into_iter()
        .filter(|(type_id, _)| used_types.contains(type_id))
        .collect();

    // Remap phase: update function and constant indices only
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
                    other => other,
                })
                .collect();
            function
        })
        .collect();

    // Remap entry point
    let new_entry = bytecode
        .entry
        .and_then(|old_id| function_remap.get(&old_id).copied());

    Bytecode {
        constants: new_constants,
        functions: remapped_functions,
        builtins: bytecode.builtins,
        entry: new_entry,
        types: new_types,
    }
}
