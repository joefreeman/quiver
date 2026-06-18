use crate::bytecode::{ConcreteType, Function, Instruction};
use crate::types::{BuiltinInfo, TupleTypeInfo, Type, TypeLookup, is_compatible};
use std::collections::{HashMap, HashSet};

/// TypeLookup implementation for compatibility computation
pub struct TypeLookupImpl<'a> {
    types: &'a [Type],
    tuples: &'a [TupleTypeInfo],
}

impl<'a> TypeLookupImpl<'a> {
    pub fn new(types: &'a [Type], tuples: &'a [TupleTypeInfo]) -> Self {
        TypeLookupImpl { types, tuples }
    }
}

impl<'a> TypeLookup for TypeLookupImpl<'a> {
    fn lookup_type(&self, type_id: usize) -> Option<&Type> {
        self.types.get(type_id)
    }

    fn lookup_tuple(&self, tuple_id: usize) -> Option<&TupleTypeInfo> {
        self.tuples.get(tuple_id)
    }
}

/// Information needed to compute compatibility for a program.
/// Function type information is derived from Function::type_id and the types registry.
pub struct CompatibilityInput<'a> {
    /// Types registered (index = type_id)
    pub types: &'a [Type],
    /// Full tuple type information
    pub tuples: &'a [TupleTypeInfo],
    /// Functions (to scan for IsType instructions and derive function types)
    pub functions: &'a [Function],
    /// Builtin information
    pub builtins: &'a [BuiltinInfo],
    /// Resource type names (index = resource_type_id)
    pub resource_names: &'a [String],
}

/// Extract function type components from a function's type_id.
/// Returns (parameter_type_id, callable_type_id, process_send, process_receive).
fn extract_function_type_info(
    func: &Function,
    types: &[Type],
) -> (usize, usize, Option<usize>, Option<usize>) {
    let type_id = func.type_id;
    match types.get(type_id) {
        Some(Type::Callable {
            parameter,
            result,
            receive,
        }) => (*parameter, type_id, Some(*receive), Some(*result)),
        _ => (0, type_id, None, None),
    }
}

/// Compute type compatibility table for all pattern types used in the program.
/// This precomputes which ConcreteTypes are compatible with which pattern types,
/// allowing O(1) runtime type checking instead of recursive type traversal.
/// Returns a Vec where index is type_id and value is the set of compatible concrete types.
pub fn compute_type_compatibility(input: &CompatibilityInput) -> Vec<HashSet<ConcreteType>> {
    let lookup = TypeLookupImpl::new(input.types, input.tuples);
    let index = TypeIndex::build(input, &lookup);

    // Collect all pattern type IDs (types used in IsType instructions)
    let mut pattern_type_ids = HashSet::new();

    for function in input.functions {
        for instruction in &function.instructions {
            if let Instruction::IsType(type_id) = instruction {
                pattern_type_ids.insert(*type_id);
            }
        }
    }

    // Initialize the compatibility table with entries for all pattern types
    let mut compatible_with: Vec<HashSet<ConcreteType>> = vec![HashSet::new(); input.types.len()];

    // For each pattern type, compute which ConcreteTypes are compatible with it
    for &pattern_id in &pattern_type_ids {
        if pattern_id >= input.types.len() {
            continue; // Skip invalid IDs
        }

        compatible_with[pattern_id] =
            compute_compatible_concrete_types(pattern_id, input, &lookup, &index);
    }

    compatible_with
}

/// Compute parameter compatibility for mailbox filtering.
/// For each function and builtin, computes which ConcreteTypes are compatible with its parameter type.
/// Returns (function_param_compatibility, builtin_param_compatibility).
pub fn compute_param_compatibility(
    input: &CompatibilityInput,
) -> (Vec<HashSet<ConcreteType>>, Vec<HashSet<ConcreteType>>) {
    let lookup = TypeLookupImpl::new(input.types, input.tuples);
    let index = TypeIndex::build(input, &lookup);

    // Many functions share a parameter type, so memoise the result by parameter type id.
    let mut memo: HashMap<usize, HashSet<ConcreteType>> = HashMap::new();
    let mut compatible_for = |param: usize| -> HashSet<ConcreteType> {
        memo.entry(param)
            .or_insert_with(|| compute_compatible_concrete_types(param, input, &lookup, &index))
            .clone()
    };

    // Compute function parameter compatibility by extracting type info from each function
    let function_params: Vec<HashSet<ConcreteType>> = input
        .functions
        .iter()
        .map(|func| {
            let (parameter, _, _, _) = extract_function_type_info(func, input.types);
            compatible_for(parameter)
        })
        .collect();

    // Compute builtin parameter compatibility
    let builtin_params: Vec<HashSet<ConcreteType>> = input
        .builtins
        .iter()
        .map(|builtin_info| compatible_for(builtin_info.param_type))
        .collect();

    (function_params, builtin_params)
}

/// Precomputed lookups from concrete-type shapes to their type id, so that
/// `compute_compatible_concrete_types` avoids re-scanning the whole type table on every call.
struct TypeIndex {
    integer: Option<usize>,
    binary: Option<usize>,
    reference: Option<usize>,
    /// tuple_id -> type id of `Type::Tuple(tuple_id)`
    tuple_to_type: Vec<Option<usize>>,
    /// (parameter, result) -> type id of a never-receiving `Type::Callable` (for builtins)
    callable_to_type: HashMap<(usize, usize), usize>,
    /// (send, receive) -> type id of `Type::Process`
    process_to_type: HashMap<(Option<usize>, Option<usize>), usize>,
    /// resource name -> type id of `Type::Resource`
    resource_to_type: HashMap<String, usize>,
}

impl TypeIndex {
    fn build(input: &CompatibilityInput, lookup: &TypeLookupImpl) -> Self {
        let mut index = TypeIndex {
            integer: None,
            binary: None,
            reference: None,
            tuple_to_type: vec![None; input.tuples.len()],
            callable_to_type: HashMap::new(),
            process_to_type: HashMap::new(),
            resource_to_type: HashMap::new(),
        };
        // Single pass over the type table, keeping the first occurrence of each shape
        // (matching the previous `.position()` behaviour).
        for (type_id, ty) in input.types.iter().enumerate() {
            match ty {
                Type::Integer => {
                    index.integer.get_or_insert(type_id);
                }
                Type::Binary => {
                    index.binary.get_or_insert(type_id);
                }
                Type::Reference => {
                    index.reference.get_or_insert(type_id);
                }
                Type::Tuple(tuple_id) => {
                    if let Some(slot) = index.tuple_to_type.get_mut(*tuple_id)
                        && slot.is_none()
                    {
                        *slot = Some(type_id);
                    }
                }
                Type::Callable {
                    parameter,
                    result,
                    receive,
                } => {
                    if lookup.lookup_type(*receive).map(|t| t.is_never()) == Some(true) {
                        index
                            .callable_to_type
                            .entry((*parameter, *result))
                            .or_insert(type_id);
                    }
                }
                Type::Process { send, receive } => {
                    index
                        .process_to_type
                        .entry((*send, *receive))
                        .or_insert(type_id);
                }
                Type::Resource(name) => {
                    index.resource_to_type.entry(name.clone()).or_insert(type_id);
                }
                _ => {}
            }
        }
        index
    }
}

/// Compute which ConcreteTypes are compatible with a given pattern type ID.
fn compute_compatible_concrete_types(
    pattern_id: usize,
    input: &CompatibilityInput,
    lookup: &TypeLookupImpl,
    index: &TypeIndex,
) -> HashSet<ConcreteType> {
    let mut compat_set = HashSet::new();

    // We need to register primitive types to check compatibility
    // For Integer and Binary, we check by constructing their type IDs on-the-fly
    // and using is_compatible

    // Check Integer
    if let Some(int_id) = index.integer {
        if is_compatible(int_id, pattern_id, lookup) {
            compat_set.insert(ConcreteType::Integer);
        }
    } else if let Some(pattern) = lookup.lookup_type(pattern_id)
        && (matches!(pattern, Type::Integer) || matches!(pattern, Type::Union(v) if v.is_empty()))
    {
        // Integer not in types vec, check if pattern matches Integer directly
        compat_set.insert(ConcreteType::Integer);
    }

    // Check Binary
    if let Some(bin_id) = index.binary {
        if is_compatible(bin_id, pattern_id, lookup) {
            compat_set.insert(ConcreteType::Binary);
        }
    } else if let Some(pattern) = lookup.lookup_type(pattern_id)
        && (matches!(pattern, Type::Binary) || matches!(pattern, Type::Union(v) if v.is_empty()))
    {
        compat_set.insert(ConcreteType::Binary);
    }

    // Check Reference
    if let Some(ref_id) = index.reference {
        if is_compatible(ref_id, pattern_id, lookup) {
            compat_set.insert(ConcreteType::Reference);
        }
    } else if let Some(pattern) = lookup.lookup_type(pattern_id)
        && (matches!(pattern, Type::Reference) || matches!(pattern, Type::Union(v) if v.is_empty()))
    {
        compat_set.insert(ConcreteType::Reference);
    }

    // Check all Tuples
    for (tuple_id, &found_type_id) in index.tuple_to_type.iter().enumerate() {
        if let Some(type_id) = found_type_id
            && is_compatible(type_id, pattern_id, lookup)
        {
            compat_set.insert(ConcreteType::Tuple(tuple_id));
        }
    }

    // Check all Functions - use their callable type ID from type_id
    for (func_id, func) in input.functions.iter().enumerate() {
        let (_, callable, _, _) = extract_function_type_info(func, input.types);
        if is_compatible(callable, pattern_id, lookup) {
            compat_set.insert(ConcreteType::Function(func_id));
        }
    }

    // Check all Builtins - look up their (param, result) callable type
    for (builtin_id, builtin_info) in input.builtins.iter().enumerate() {
        if let Some(&callable_id) = index
            .callable_to_type
            .get(&(builtin_info.param_type, builtin_info.result_type))
            && is_compatible(callable_id, pattern_id, lookup)
        {
            compat_set.insert(ConcreteType::Builtin(builtin_id));
        }
    }

    // Check all Processes - derive process type from function's send/receive
    for (func_id, func) in input.functions.iter().enumerate() {
        let (_, _, process_send, process_receive) = extract_function_type_info(func, input.types);
        if let Some(&process_id) = index.process_to_type.get(&(process_send, process_receive))
            && is_compatible(process_id, pattern_id, lookup)
        {
            compat_set.insert(ConcreteType::Process(func_id));
        }
    }

    // Check all Resources
    for (resource_id, resource_name) in input.resource_names.iter().enumerate() {
        if let Some(&type_id) = index.resource_to_type.get(resource_name)
            && is_compatible(type_id, pattern_id, lookup)
        {
            compat_set.insert(ConcreteType::Resource(resource_id));
        }
    }

    compat_set
}
