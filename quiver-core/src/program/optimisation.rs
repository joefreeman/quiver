use crate::bytecode::{Bytecode, Constant, Function, Instruction, TypeId};
use crate::types::TupleTypeInfo;
use std::collections::{HashMap, HashSet};

/// Perform tree shaking on program data to remove unused functions, constants, types, and builtins
/// Returns the collections needed to build an optimized Bytecode
pub fn tree_shake(
    functions: &[Function],
    constants: &[Constant],
    types: &HashMap<TypeId, TupleTypeInfo>,
    builtins: &[String],
    entry_fn: usize,
) -> Bytecode {
    // Mark phase: find all reachable functions, constants, types, and builtins
    let mut used_functions = HashSet::new();
    let mut used_constants = HashSet::new();
    let mut used_types = HashSet::new();
    let mut used_builtins = HashSet::new();

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
                    used_constants.insert(*id);
                }
                Instruction::Tuple(type_id) | Instruction::IsTuple(type_id) => {
                    used_types.insert(*type_id);
                }
                Instruction::Builtin(id) => {
                    used_builtins.insert(*id);
                }
                _ => {}
            }
        }
    }

    // If nothing is used (shouldn't happen), return original without tree shaking
    if used_functions.is_empty() {
        return Bytecode {
            constants: constants.to_vec(),
            functions: functions.to_vec(),
            builtins: builtins.to_vec(),
            entry: Some(entry_fn),
            types: types.clone(),
        };
    }

    // Sweep phase: build remapping tables and collect used items
    let mut function_remap = HashMap::new();
    let mut new_functions = Vec::new();

    for (old_id, function) in functions.iter().enumerate() {
        if used_functions.contains(&old_id) {
            function_remap.insert(old_id, new_functions.len());
            new_functions.push(function.clone());
        }
    }

    let mut constant_remap = HashMap::new();
    let mut new_constants = Vec::new();

    for (old_id, constant) in constants.iter().enumerate() {
        if used_constants.contains(&old_id) {
            constant_remap.insert(old_id, new_constants.len());
            new_constants.push(constant.clone());
        }
    }

    let mut builtin_remap = HashMap::new();
    let mut new_builtins = Vec::new();

    for (old_id, builtin) in builtins.iter().enumerate() {
        if used_builtins.contains(&old_id) {
            builtin_remap.insert(old_id, new_builtins.len());
            new_builtins.push(builtin.clone());
        }
    }

    // Filter types: keep only used types without remapping IDs
    let new_types: HashMap<_, _> = types
        .iter()
        .filter(|(type_id, _)| used_types.contains(type_id))
        .map(|(k, v)| (*k, v.clone()))
        .collect();

    // Remap phase: update function, constant, and builtin indices
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
