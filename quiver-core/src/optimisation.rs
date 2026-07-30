use crate::bytecode::{Bytecode, Function, Id, Instruction, Site, SiteTable};
use crate::types::{BuiltinInfo, TupleTypeInfo, Type};
use std::collections::{HashMap, HashSet, VecDeque};

/// Look up an instruction operand in a `usize`-keyed remap table. The tables are keyed by
/// `usize` because they also renumber the tables themselves; only the operands are [`Id`].
/// Panics on a missing entry, like the direct indexing it replaces — a stripped id reaching
/// here is a bug in the reachability walk, not a recoverable condition.
fn remap(table: &std::collections::HashMap<usize, usize>, id: Id) -> Id {
    table[&(id as usize)] as Id
}

/// Tree shake bytecode to remove unreachable code.
/// Returns an optimized Bytecode with only reachable functions, constants, builtins, tuples, and types.
pub fn tree_shake(bytecode: Bytecode, entry: usize) -> Bytecode {
    // Collect the type ids referenced transitively from a Type, descending through tuples into
    // their field types. Types and tuples are mutually recursive, so this recurses with
    // `collect_tuple_refs`; the `used_types` / `used_tuples` insert-guards bound the recursion
    // (the id graph is a DAG — recursive types are expressed with `Type::Cycle`, not id cycles).
    fn collect_type_refs(
        type_id: usize,
        types: &[Type],
        tuples: &[TupleTypeInfo],
        used_types: &mut HashSet<usize>,
        used_tuples: &mut HashSet<usize>,
        used_resources: &mut HashSet<String>,
    ) {
        if !used_types.insert(type_id) {
            return; // Already processed
        }
        let Some(typ) = types.get(type_id) else {
            return;
        };
        match typ {
            Type::Integer | Type::Binary | Type::Reference | Type::Variable(_) | Type::Cycle(_) => {
            }
            Type::Annotated { base, entries, .. } => {
                collect_type_refs(
                    *base,
                    types,
                    tuples,
                    used_types,
                    used_tuples,
                    used_resources,
                );
                for (_, value_type) in entries {
                    collect_type_refs(
                        *value_type,
                        types,
                        tuples,
                        used_types,
                        used_tuples,
                        used_resources,
                    );
                }
            }
            Type::Tuple(tuple_id) => {
                collect_tuple_refs(
                    *tuple_id,
                    types,
                    tuples,
                    used_types,
                    used_tuples,
                    used_resources,
                );
            }
            Type::Partial { fields, .. } => {
                // Collect type references from partial fields
                for (_, field_type_id) in fields {
                    collect_type_refs(
                        *field_type_id,
                        types,
                        tuples,
                        used_types,
                        used_tuples,
                        used_resources,
                    );
                }
            }
            Type::Callable {
                parameter,
                result,
                receive,
                states,
            } => {
                collect_type_refs(
                    *parameter,
                    types,
                    tuples,
                    used_types,
                    used_tuples,
                    used_resources,
                );
                collect_type_refs(
                    *result,
                    types,
                    tuples,
                    used_types,
                    used_tuples,
                    used_resources,
                );
                collect_type_refs(
                    *receive,
                    types,
                    tuples,
                    used_types,
                    used_tuples,
                    used_resources,
                );
                if let Some(states) = states {
                    collect_type_refs(
                        *states,
                        types,
                        tuples,
                        used_types,
                        used_tuples,
                        used_resources,
                    );
                }
            }
            Type::Union(type_ids) => {
                for &tid in type_ids {
                    collect_type_refs(tid, types, tuples, used_types, used_tuples, used_resources);
                }
            }
            Type::Process {
                send,
                receive,
                state,
            } => {
                if let Some(tid) = send {
                    collect_type_refs(*tid, types, tuples, used_types, used_tuples, used_resources);
                }
                if let Some(tid) = state {
                    collect_type_refs(*tid, types, tuples, used_types, used_tuples, used_resources);
                }
                if let Some(tid) = receive {
                    collect_type_refs(*tid, types, tuples, used_types, used_tuples, used_resources);
                }
            }
            Type::Resource(name) => {
                used_resources.insert(name.clone());
            }
        }
    }

    // Mark a tuple as used and descend into its field types. Field types may reference further
    // tuples, so reaching every transitively-referenced tuple here (rather than in a single
    // snapshot pass) is what keeps the type/tuple closure complete — an incomplete closure drops
    // a still-referenced type and leaves a dangling id after remapping.
    fn collect_tuple_refs(
        tuple_id: usize,
        types: &[Type],
        tuples: &[TupleTypeInfo],
        used_types: &mut HashSet<usize>,
        used_tuples: &mut HashSet<usize>,
        used_resources: &mut HashSet<String>,
    ) {
        if !used_tuples.insert(tuple_id) {
            return; // Already processed
        }
        let Some(tuple_info) = tuples.get(tuple_id) else {
            return;
        };
        for (_, field_type_id) in &tuple_info.fields {
            collect_type_refs(
                *field_type_id,
                types,
                tuples,
                used_types,
                used_tuples,
                used_resources,
            );
        }
    }

    // Mark phase: find all reachable items
    let mut used_functions: HashSet<usize> = HashSet::new();
    let mut used_constants: HashSet<usize> = HashSet::new();
    let mut used_tuples: HashSet<usize> = HashSet::new();
    let mut used_types: HashSet<usize> = HashSet::new();
    let mut used_builtins: HashSet<usize> = HashSet::new();
    let mut used_resources: HashSet<String> = HashSet::new();

    // Always keep NIL and OK tuples (indices 0 and 1)
    collect_tuple_refs(
        0,
        &bytecode.types,
        &bytecode.tuples,
        &mut used_types,
        &mut used_tuples,
        &mut used_resources,
    );
    collect_tuple_refs(
        1,
        &bytecode.types,
        &bytecode.tuples,
        &mut used_types,
        &mut used_tuples,
        &mut used_resources,
    );

    // The debug site table's values are built by the executor, not by instructions, so
    // its module-name constants and value tuples must be kept (and later remapped) here.
    if let Some(table) = &bytecode.debug {
        for tuple_id in [table.site_tuple, table.str_tuple]
            .into_iter()
            .chain(table.kind_tuples.iter().copied())
        {
            collect_tuple_refs(
                tuple_id,
                &bytecode.types,
                &bytecode.tuples,
                &mut used_types,
                &mut used_tuples,
                &mut used_resources,
            );
        }
        for site in &table.sites {
            used_constants.insert(site.module_constant);
        }
    }

    // BFS through reachable functions
    let mut queue: VecDeque<usize> = VecDeque::new();
    queue.push_back(entry);

    while let Some(fn_id) = queue.pop_front() {
        if !used_functions.insert(fn_id) {
            continue; // Already processed
        }

        let Some(function) = bytecode.functions.get(fn_id) else {
            continue;
        };

        // Collect type_ids from function's type
        collect_type_refs(
            function.type_id,
            &bytecode.types,
            &bytecode.tuples,
            &mut used_types,
            &mut used_tuples,
            &mut used_resources,
        );

        for instruction in &function.instructions {
            match instruction {
                Instruction::Function(id) => {
                    queue.push_back(*id as usize);
                }
                Instruction::Constant(id) => {
                    used_constants.insert(*id as usize);
                }
                Instruction::Tuple(id) => {
                    let id = &(*id as usize);
                    collect_tuple_refs(
                        *id,
                        &bytecode.types,
                        &bytecode.tuples,
                        &mut used_types,
                        &mut used_tuples,
                        &mut used_resources,
                    );
                    // Also mark the corresponding Type::Tuple as used for IsType compatibility checks
                    if let Some(type_id) = bytecode
                        .types
                        .iter()
                        .position(|t| matches!(t, Type::Tuple(tid) if *tid == *id))
                    {
                        collect_type_refs(
                            type_id,
                            &bytecode.types,
                            &bytecode.tuples,
                            &mut used_types,
                            &mut used_tuples,
                            &mut used_resources,
                        );
                    }
                }
                Instruction::IsType(id) | Instruction::GetAnnotation(_, Some(id)) => {
                    collect_type_refs(
                        *id as usize,
                        &bytecode.types,
                        &bytecode.tuples,
                        &mut used_types,
                        &mut used_tuples,
                        &mut used_resources,
                    );
                }
                Instruction::Builtin(id, type_argument) => {
                    used_builtins.insert(*id as usize);
                    // A type-consuming builtin's explicit type argument is a type
                    // reference like IsType's: keep its closure alive through stripping.
                    if let Some(type_id) = type_argument {
                        collect_type_refs(
                            *type_id as usize,
                            &bytecode.types,
                            &bytecode.tuples,
                            &mut used_types,
                            &mut used_tuples,
                            &mut used_resources,
                        );
                    }
                }
                Instruction::Process(_, func_id) => {
                    queue.push_back(*func_id as usize);
                }
                _ => {}
            }
        }
    }

    // Collect types from builtins. `collect_type_refs` descends through tuples into their field
    // types, so the type/tuple closure is complete after this — no separate tuple-field pass is
    // needed.
    for &builtin_id in &used_builtins {
        if let Some(builtin) = bytecode.builtins.get(builtin_id) {
            collect_type_refs(
                builtin.param_type,
                &bytecode.types,
                &bytecode.tuples,
                &mut used_types,
                &mut used_tuples,
                &mut used_resources,
            );
            collect_type_refs(
                builtin.result_type,
                &bytecode.types,
                &bytecode.tuples,
                &mut used_types,
                &mut used_tuples,
                &mut used_resources,
            );
        }
    }

    // Build remap tables
    let mut sorted_functions: Vec<usize> = used_functions.into_iter().collect();
    sorted_functions.sort();
    let function_remap: HashMap<usize, usize> = sorted_functions
        .iter()
        .enumerate()
        .map(|(new_id, &old_id)| (old_id, new_id))
        .collect();

    let mut sorted_constants: Vec<usize> = used_constants.into_iter().collect();
    sorted_constants.sort();
    let constant_remap: HashMap<usize, usize> = sorted_constants
        .iter()
        .enumerate()
        .map(|(new_id, &old_id)| (old_id, new_id))
        .collect();

    let mut sorted_tuples: Vec<usize> = used_tuples.into_iter().collect();
    sorted_tuples.sort();
    let tuple_remap: HashMap<usize, usize> = sorted_tuples
        .iter()
        .enumerate()
        .map(|(new_id, &old_id)| (old_id, new_id))
        .collect();

    let mut sorted_types: Vec<usize> = used_types.into_iter().collect();
    sorted_types.sort();
    let type_remap: HashMap<usize, usize> = sorted_types
        .iter()
        .enumerate()
        .map(|(new_id, &old_id)| (old_id, new_id))
        .collect();

    let mut sorted_builtins: Vec<usize> = used_builtins.into_iter().collect();
    sorted_builtins.sort();
    let builtin_remap: HashMap<usize, usize> = sorted_builtins
        .iter()
        .enumerate()
        .map(|(new_id, &old_id)| (old_id, new_id))
        .collect();

    // Collect sorted resource names (resources are now strings, not IDs)
    let mut sorted_resources: Vec<String> = used_resources.into_iter().collect();
    sorted_resources.sort();

    // Helper to remap a Type
    let remap_type = |typ: &Type| -> Type {
        match typ {
            Type::Integer => Type::Integer,
            Type::Binary => Type::Binary,
            Type::Reference => Type::Reference,
            Type::Tuple(id) => Type::Tuple(*tuple_remap.get(id).unwrap_or(id)),
            Type::Partial { name, fields } => Type::Partial {
                name: name.clone(),
                fields: fields
                    .iter()
                    .map(|(fname, ftype)| (fname.clone(), *type_remap.get(ftype).unwrap_or(ftype)))
                    .collect(),
            },
            Type::Callable {
                parameter,
                result,
                receive,
                states,
            } => Type::Callable {
                parameter: *type_remap.get(parameter).unwrap_or(parameter),
                result: *type_remap.get(result).unwrap_or(result),
                receive: *type_remap.get(receive).unwrap_or(receive),
                states: states.map(|id| *type_remap.get(&id).unwrap_or(&id)),
            },
            Type::Cycle(depth) => Type::Cycle(*depth),
            Type::Union(type_ids) => Type::Union(
                type_ids
                    .iter()
                    .map(|id| *type_remap.get(id).unwrap_or(id))
                    .collect(),
            ),
            Type::Process {
                send,
                receive,
                state,
            } => Type::Process {
                send: send.map(|id| *type_remap.get(&id).unwrap_or(&id)),
                receive: receive.map(|id| *type_remap.get(&id).unwrap_or(&id)),
                state: state.map(|id| *type_remap.get(&id).unwrap_or(&id)),
            },
            Type::Resource(name) => Type::Resource(name.clone()),
            Type::Variable(name) => Type::Variable(name.clone()),
            Type::Annotated {
                base,
                exact,
                entries,
            } => Type::Annotated {
                base: *type_remap.get(base).unwrap_or(base),
                exact: *exact,
                entries: entries
                    .iter()
                    .map(|(key, value_type)| {
                        (*key, *type_remap.get(value_type).unwrap_or(value_type))
                    })
                    .collect(),
            },
        }
    };

    // Build new functions with remapped instructions and type_id
    let new_functions: Vec<Function> = sorted_functions
        .iter()
        .map(|&old_id| {
            let old_func = &bytecode.functions[old_id];
            let new_instructions: Vec<Instruction> = old_func
                .instructions
                .iter()
                .map(|instr| match instr {
                    Instruction::Function(id) => Instruction::Function(remap(&function_remap, *id)),
                    Instruction::Constant(id) => Instruction::Constant(remap(&constant_remap, *id)),
                    Instruction::Tuple(id) => Instruction::Tuple(remap(&tuple_remap, *id)),
                    Instruction::IsType(id) => Instruction::IsType(remap(&type_remap, *id)),
                    Instruction::GetAnnotation(key, Some(id)) => {
                        Instruction::GetAnnotation(*key, Some(remap(&type_remap, *id)))
                    }
                    Instruction::Builtin(id, type_argument) => Instruction::Builtin(
                        remap(&builtin_remap, *id),
                        type_argument.map(|t| remap(&type_remap, t)),
                    ),
                    Instruction::Process(pid, fid) => {
                        Instruction::Process(*pid, remap(&function_remap, *fid))
                    }
                    other => *other,
                })
                .collect();
            Function {
                instructions: new_instructions,
                captures: old_func.captures,
                type_id: *type_remap
                    .get(&old_func.type_id)
                    .unwrap_or(&old_func.type_id),
            }
        })
        .collect();

    // Build new constants
    let new_constants: Vec<_> = sorted_constants
        .iter()
        .map(|&old_id| bytecode.constants[old_id].clone())
        .collect();

    // Build new tuples with remapped field type_ids
    let new_tuples: Vec<TupleTypeInfo> = sorted_tuples
        .iter()
        .map(|&old_id| {
            let old_tuple = &bytecode.tuples[old_id];
            TupleTypeInfo {
                name: old_tuple.name.clone(),
                fields: old_tuple
                    .fields
                    .iter()
                    .map(|(name, type_id)| {
                        (name.clone(), *type_remap.get(type_id).unwrap_or(type_id))
                    })
                    .collect(),
            }
        })
        .collect();

    // Build new builtins with remapped type_ids
    let new_builtins: Vec<BuiltinInfo> = sorted_builtins
        .iter()
        .map(|&old_id| {
            let old_builtin = &bytecode.builtins[old_id];
            BuiltinInfo {
                name: old_builtin.name.clone(),
                param_type: *type_remap
                    .get(&old_builtin.param_type)
                    .unwrap_or(&old_builtin.param_type),
                result_type: *type_remap
                    .get(&old_builtin.result_type)
                    .unwrap_or(&old_builtin.result_type),
            }
        })
        .collect();

    // Build new types with remapped references
    let new_types: Vec<Type> = sorted_types
        .iter()
        .map(|&old_id| remap_type(&bytecode.types[old_id]))
        .collect();

    // Resources are now strings directly (no remapping needed)
    let new_resources: Vec<String> = sorted_resources;

    // Remap the debug site table's ids (sites themselves are not shaken — `Stamp`
    // instructions keep their indices).
    let new_debug = bytecode.debug.as_ref().map(|table| SiteTable {
        origin_key: table.origin_key,
        site_tuple: *tuple_remap.get(&table.site_tuple).unwrap(),
        str_tuple: *tuple_remap.get(&table.str_tuple).unwrap(),
        kind_tuples: table
            .kind_tuples
            .iter()
            .map(|id| *tuple_remap.get(id).unwrap())
            .collect(),
        sites: table
            .sites
            .iter()
            .map(|site| Site {
                module_constant: *constant_remap.get(&site.module_constant).unwrap(),
                ..site.clone()
            })
            .collect(),
    });

    Bytecode {
        constants: new_constants,
        functions: new_functions,
        builtins: new_builtins,
        entry: Some(*function_remap.get(&entry).unwrap()),
        tuples: new_tuples,
        types: new_types,
        resources: new_resources,
        // Annotation keys are not tree-shaken: the table is tiny and key ids embedded in
        // Annotate/GetAnnotation instructions stay valid without a remap.
        annotation_keys: bytecode.annotation_keys.clone(),
        // Field names likewise: GetNamed ids stay valid without a remap.
        field_names: bytecode.field_names.clone(),
        debug: new_debug,
    }
}
