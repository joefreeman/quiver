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

/// A `Type::Process`'s components — (send, receive, state) — as an index key.
type ProcessTypeKey = (Option<usize>, Option<usize>, Option<usize>);

/// Extract function type components from a function's type_id.
/// Returns (parameter, callable_type_id, process_send, process_receive, process_state).
fn extract_function_type_info(
    func: &Function,
    types: &[Type],
) -> (usize, usize, Option<usize>, Option<usize>, Option<usize>) {
    let type_id = func.type_id;
    match types.get(type_id) {
        Some(Type::Callable {
            parameter,
            result,
            receive,
            states,
        }) => (*parameter, type_id, Some(*receive), Some(*result), *states),
        _ => (0, type_id, None, None, None),
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
            match instruction {
                Instruction::IsType(type_id) | Instruction::GetAnnotation(_, Some(type_id)) => {
                    pattern_type_ids.insert(*type_id);
                }
                _ => {}
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

/// For each interned field name, each tuple type's offset for a field with that name (or
/// `None` when it has no such field). `GetNamed(name_id)` resolves a field against the
/// value's own tuple id via `offsets[name_id][tuple_id]` — two array lookups, no names at
/// runtime. Emission sites guarantee compatibility statically, so a `None` hit at runtime
/// is a compiler invariant violation.
pub fn compute_field_offsets(
    field_names: &[String],
    tuples: &[TupleTypeInfo],
) -> Vec<Vec<Option<usize>>> {
    field_names
        .iter()
        .map(|name| {
            tuples
                .iter()
                .map(|info| {
                    info.fields
                        .iter()
                        .position(|(fname, _)| fname.as_deref() == Some(name))
                })
                .collect()
        })
        .collect()
}

/// Map each tuple type-id to a canonical *value-shape* id: the lowest tuple-id that shares its
/// name and field labels (field *types* ignored). Two tuple values built via paths that inferred
/// different field types — e.g. a list `Cons` cell from a literal vs. from a recursive helper —
/// then carry shape-equal ids, so structural value equality (`==`) treats them as equal. The
/// executor stores only this mapping (indexed by tuple-id), never the names themselves.
pub fn compute_canonical_tuples(tuples: &[TupleTypeInfo]) -> Vec<usize> {
    let mut by_shape: HashMap<(Option<String>, Vec<Option<String>>), usize> = HashMap::new();
    tuples
        .iter()
        .enumerate()
        .map(|(id, info)| {
            let labels: Vec<Option<String>> =
                info.fields.iter().map(|(label, _)| label.clone()).collect();
            *by_shape.entry((info.name.clone(), labels)).or_insert(id)
        })
        .collect()
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
            let (parameter, _, _, _, _) = extract_function_type_info(func, input.types);
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

/// Compatibility tables maintained incrementally across an append-only program's growth.
///
/// The environment merges a REPL line's bytecode into its program and ships refreshed
/// tables to the workers on every line; recomputing them from scratch each time is
/// quadratic in session length. Registries only ever grow, and `is_compatible` over
/// existing ids never changes, so the tables can be *extended* instead: new concrete
/// types are tested against the already-known pattern types, and new pattern types get
/// one full scan. `update` yields exactly what the from-scratch functions produce
/// (`assert_matches_full` checks this, for validation runs).
#[derive(Debug, Clone, Default)]
pub struct CompatibilityTables {
    /// Registry sizes as of the last `update`; entries beyond these are the new items.
    types_len: usize,
    tuples_len: usize,
    functions_len: usize,
    builtins_len: usize,
    resources_len: usize,
    /// Pattern type ids (`IsType` / checked `GetAnnotation` targets) already computed.
    pattern_ids: HashSet<usize>,
    /// Per-type compatibility, as `compute_type_compatibility` returns.
    pub type_compatibility: Vec<HashSet<ConcreteType>>,
    /// Per-function parameter compatibility, as `compute_param_compatibility` returns.
    pub function_params: Vec<HashSet<ConcreteType>>,
    /// Per-builtin parameter compatibility, as `compute_param_compatibility` returns.
    pub builtin_params: Vec<HashSet<ConcreteType>>,
}

impl CompatibilityTables {
    /// Extend the tables to cover `input`, which must describe an append-only extension
    /// of the program covered by the previous call (the environment's merged program).
    pub fn update(&mut self, input: &CompatibilityInput) {
        let lookup = TypeLookupImpl::new(input.types, input.tuples);
        let index = TypeIndex::build(input, &lookup);

        self.type_compatibility
            .resize(input.types.len(), HashSet::new());

        // Newly testable concrete types, with the type id representing each in
        // `is_compatible` checks. A concrete becomes testable when its own id is new
        // (tuple/function/builtin/resource) or when the type-table entry the checks go
        // through first appears (`TypeIndex` keeps first occurrences, so an index entry
        // with a new type id means there was none before).
        let mut new_concretes: Vec<(ConcreteType, usize)> = Vec::new();

        for (concrete, slot) in [
            (ConcreteType::Integer, index.integer),
            (ConcreteType::Binary, index.binary),
            (ConcreteType::Reference, index.reference),
        ] {
            if let Some(id) = slot
                && id >= self.types_len
            {
                new_concretes.push((concrete, id));
            }
        }

        for (tuple_id, slot) in index.tuple_to_type.iter().enumerate() {
            if let Some(id) = slot
                && *id >= self.types_len
            {
                new_concretes.push((ConcreteType::Tuple(tuple_id), *id));
            }
        }

        for (func_id, func) in input.functions.iter().enumerate().skip(self.functions_len) {
            let (_, callable, _, _, _) = extract_function_type_info(func, input.types);
            new_concretes.push((ConcreteType::Function(func_id), callable));
        }

        // Process concretes exist where a function's (send, receive, state) key has a
        // `Type::Process` entry: new functions against the index, plus old functions
        // whose key entry only just appeared.
        for (func_id, func) in input.functions.iter().enumerate() {
            let (_, _, send, receive, state) = extract_function_type_info(func, input.types);
            if let Some(&process_id) = index.process_to_type.get(&(send, receive, state))
                && (func_id >= self.functions_len || process_id >= self.types_len)
            {
                new_concretes.push((ConcreteType::Process(func_id), process_id));
            }
        }

        // Builtin concretes mirror processes, keyed by (param, result) callable entries.
        for (builtin_id, info) in input.builtins.iter().enumerate() {
            if let Some(&callable_id) = index
                .callable_to_type
                .get(&(info.param_type, info.result_type))
                && (builtin_id >= self.builtins_len || callable_id >= self.types_len)
            {
                new_concretes.push((ConcreteType::Builtin(builtin_id), callable_id));
            }
        }

        // Resource names derive from the type table in first-occurrence order, so a new
        // name always has a new `Type::Resource` entry behind it.
        for (resource_id, name) in input
            .resource_names
            .iter()
            .enumerate()
            .skip(self.resources_len)
        {
            if let Some(&type_id) = index.resource_to_type.get(name) {
                new_concretes.push((ConcreteType::Resource(resource_id), type_id));
            }
        }

        // Extend existing pattern entries and parameter rows with the new concretes,
        // memoising verdicts per pattern id (parameters repeat heavily).
        if !new_concretes.is_empty() {
            let mut verdicts: HashMap<usize, Vec<bool>> = HashMap::new();
            let mut verdicts_for = |pattern_id: usize| -> Vec<bool> {
                verdicts
                    .entry(pattern_id)
                    .or_insert_with(|| {
                        let stripped = Type::strip_annotations(pattern_id, &lookup);
                        new_concretes
                            .iter()
                            .map(|&(_, rep)| is_compatible(rep, stripped, &lookup))
                            .collect()
                    })
                    .clone()
            };

            for &pattern_id in &self.pattern_ids {
                for (hit, &(concrete, _)) in verdicts_for(pattern_id).iter().zip(&new_concretes) {
                    if *hit {
                        self.type_compatibility[pattern_id].insert(concrete);
                    }
                }
            }
            for (func_id, func) in input.functions.iter().enumerate().take(self.functions_len) {
                let (parameter, _, _, _, _) = extract_function_type_info(func, input.types);
                for (hit, &(concrete, _)) in verdicts_for(parameter).iter().zip(&new_concretes) {
                    if *hit {
                        self.function_params[func_id].insert(concrete);
                    }
                }
            }
            for (builtin_id, info) in input.builtins.iter().enumerate().take(self.builtins_len) {
                for (hit, &(concrete, _)) in
                    verdicts_for(info.param_type).iter().zip(&new_concretes)
                {
                    if *hit {
                        self.builtin_params[builtin_id].insert(concrete);
                    }
                }
            }
        }

        // New pattern types (only new functions can introduce them) get a full scan.
        for function in &input.functions[self.functions_len..] {
            for instruction in &function.instructions {
                if let Instruction::IsType(type_id) | Instruction::GetAnnotation(_, Some(type_id)) =
                    instruction
                    && *type_id < input.types.len()
                    && self.pattern_ids.insert(*type_id)
                {
                    self.type_compatibility[*type_id] =
                        compute_compatible_concrete_types(*type_id, input, &lookup, &index);
                }
            }
        }

        // New parameter rows get a full scan too, shared per parameter type.
        let mut memo: HashMap<usize, HashSet<ConcreteType>> = HashMap::new();
        let mut compatible_for = |param: usize| -> HashSet<ConcreteType> {
            memo.entry(param)
                .or_insert_with(|| compute_compatible_concrete_types(param, input, &lookup, &index))
                .clone()
        };
        for func in &input.functions[self.functions_len..] {
            let (parameter, _, _, _, _) = extract_function_type_info(func, input.types);
            self.function_params.push(compatible_for(parameter));
        }
        for info in &input.builtins[self.builtins_len..] {
            self.builtin_params.push(compatible_for(info.param_type));
        }

        self.types_len = input.types.len();
        self.tuples_len = input.tuples.len();
        self.functions_len = input.functions.len();
        self.builtins_len = input.builtins.len();
        self.resources_len = input.resource_names.len();
    }

    /// Assert the incremental tables equal a from-scratch computation over `input`.
    /// For validation runs (opt-in, e.g. behind an environment variable) — a mismatch
    /// is a bug in `update`.
    pub fn assert_matches_full(&self, input: &CompatibilityInput) {
        assert_eq!(
            self.type_compatibility,
            compute_type_compatibility(input),
            "incremental type_compatibility diverged from full recomputation"
        );
        let (function_params, builtin_params) = compute_param_compatibility(input);
        assert_eq!(
            self.function_params, function_params,
            "incremental function_params diverged from full recomputation"
        );
        assert_eq!(
            self.builtin_params, builtin_params,
            "incremental builtin_params diverged from full recomputation"
        );
    }
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
    /// (send, receive, state) -> type id of `Type::Process`
    process_to_type: HashMap<ProcessTypeKey, usize>,
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
                    states: _,
                } => {
                    if lookup.lookup_type(*receive).map(|t| t.is_never()) == Some(true) {
                        index
                            .callable_to_type
                            .entry((*parameter, *result))
                            .or_insert(type_id);
                    }
                }
                Type::Process {
                    send,
                    receive,
                    state,
                } => {
                    index
                        .process_to_type
                        .entry((*send, *receive, *state))
                        .or_insert(type_id);
                }
                Type::Resource(name) => {
                    index
                        .resource_to_type
                        .entry(name.clone())
                        .or_insert(type_id);
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

    // Annotation rows are invisible to pattern matching, so a runtime type check
    // against `T @ row` must behave exactly as against `T`.
    let pattern_id = Type::strip_annotations(pattern_id, lookup);

    // A primitive is testable only through a type-table entry representing it (the
    // `TypeIndex` slot): a pattern that could match a primitive names it, so the
    // entry exists whenever the verdict could be positive. No fallback — in
    // particular an empty union matches nothing, consistent with `is_compatible`.
    for (slot, concrete) in [
        (index.integer, ConcreteType::Integer),
        (index.binary, ConcreteType::Binary),
        (index.reference, ConcreteType::Reference),
    ] {
        if let Some(type_id) = slot
            && is_compatible(type_id, pattern_id, lookup)
        {
            compat_set.insert(concrete);
        }
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
        let (_, callable, _, _, _) = extract_function_type_info(func, input.types);
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

    // Check all Processes - derive process type from function's send/receive/states. The
    // state component rides this check: it is what makes a received pid trustworthy for
    // bare `?p` (which has no runtime test of its own).
    for (func_id, func) in input.functions.iter().enumerate() {
        let (_, _, process_send, process_receive, process_state) =
            extract_function_type_info(func, input.types);
        if let Some(&process_id) =
            index
                .process_to_type
                .get(&(process_send, process_receive, process_state))
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
