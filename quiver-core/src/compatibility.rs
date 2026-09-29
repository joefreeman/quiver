use crate::bytecode::{ConcreteType, Function, Opcode};
use crate::types::{BuiltinInfo, TupleTypeInfo, Type, TypeLookup, is_compatible, types_overlap};
use std::collections::{HashMap, HashSet};

/// What a runtime type test makes of the values of each concrete type: what it looks a
/// value's concrete type up in. A concrete type absent from the row can hold no value of the
/// type. Fx-hashed, because the test runs on every pattern match and the keys are small
/// integers that need no protection against collision attacks.
///
/// Serialized as a list of pairs, since a serializing transport's format may only key maps
/// by strings.
#[derive(Debug, Clone, Default, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(
    from = "Vec<(ConcreteType, Verdict)>",
    into = "Vec<(ConcreteType, Verdict)>"
)]
pub struct ConcreteTypes(rustc_hash::FxHashMap<ConcreteType, Verdict>);

impl std::ops::Deref for ConcreteTypes {
    type Target = rustc_hash::FxHashMap<ConcreteType, Verdict>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for ConcreteTypes {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<Vec<(ConcreteType, Verdict)>> for ConcreteTypes {
    fn from(entries: Vec<(ConcreteType, Verdict)>) -> Self {
        Self(entries.into_iter().collect())
    }
}

impl From<ConcreteTypes> for Vec<(ConcreteType, Verdict)> {
    fn from(row: ConcreteTypes) -> Self {
        row.0.into_iter().collect()
    }
}

/// A runtime type test's verdict on the values of one concrete type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum Verdict {
    /// Every value of it belongs to the type.
    Admit,
    /// Some may: a tuple built at a wider type than it holds, or in generic code, whose own
    /// type can't say. The test looks at the value's fields.
    Inspect,
}

/// The verdict on values of a concrete type represented by `rep`. Anything but a tuple is
/// admitted exactly when its type fits. A tuple (`tuple` names it, with its facts) whose type
/// doesn't settle it is inspected where some of its values could belong: its name and labels
/// could match, and its type overlaps the pattern — or holds a function or pid, whose types
/// can share values without overlapping as the relation reckons it (a function taking
/// `'int | 'bin` is both an `#'int -> _` and a `#'bin -> _`).
fn verdict(
    rep: usize,
    tuple: Option<(usize, TupleFacts)>,
    pattern_id: usize,
    lookup: &impl TypeLookup,
) -> Option<Verdict> {
    let Some((tuple_id, facts)) = tuple else {
        return is_compatible(rep, pattern_id, lookup).then_some(Verdict::Admit);
    };
    if (facts.exact && is_compatible(rep, pattern_id, lookup))
        || head_decides(tuple_id, pattern_id, lookup)
    {
        Some(Verdict::Admit)
    } else if head_may_match(tuple_id, pattern_id, lookup)
        && (facts.opaque || types_overlap(rep, pattern_id, lookup))
    {
        Some(Verdict::Inspect)
    } else {
        None
    }
}

/// Whether a tuple of this name and these labels belongs to the type whatever its fields
/// hold: the type has a member matching the head whose fields constrain nothing a test can
/// check (`_`, a type variable, or a recursive reference past where the test starts — see
/// `walk_inhabits`). The common case is a pattern telling a recursive union's members apart,
/// such as `=Cons[h, t]` on a generically built list.
fn head_decides(tuple_id: usize, pattern_id: usize, lookup: &impl TypeLookup) -> bool {
    head_decides_within(tuple_id, pattern_id, 0, lookup)
}

/// `head_decides`, `entered` binders into the type the test starts from.
fn head_decides_within(
    tuple_id: usize,
    pattern_id: usize,
    entered: usize,
    lookup: &impl TypeLookup,
) -> bool {
    let vacuous =
        |type_id: usize| match lookup.lookup_type(Type::strip_annotations(type_id, lookup)) {
            Some(Type::Top | Type::Variable(_)) => true,
            Some(Type::Cycle(depth)) => *depth > entered,
            _ => false,
        };
    match lookup.lookup_type(pattern_id) {
        Some(Type::Top | Type::Variable(_)) => true,
        Some(Type::Annotated { base, .. }) => head_decides_within(tuple_id, *base, entered, lookup),
        Some(Type::Union(members)) => members
            .iter()
            .any(|&member| head_decides_within(tuple_id, member, entered + 1, lookup)),
        Some(Type::Tuple(expected)) => {
            head_may_match(tuple_id, pattern_id, lookup)
                && lookup
                    .lookup_tuple(*expected)
                    .is_some_and(|expected| expected.fields.iter().all(|&(_, t)| vacuous(t)))
        }
        Some(Type::Partial { fields, rest, .. }) => {
            head_may_match(tuple_id, pattern_id, lookup)
                && fields.iter().all(|&(_, t)| vacuous(t))
                && rest.is_none_or(vacuous)
        }
        _ => false,
    }
}

/// Whether a tuple of this name and these labels could belong to the type, whatever its
/// fields hold: an over-approximation, so a tuple it rules out needs no inspection.
fn head_may_match(tuple_id: usize, pattern_id: usize, lookup: &impl TypeLookup) -> bool {
    let Some(actual) = lookup.lookup_tuple(tuple_id) else {
        return false;
    };
    match lookup.lookup_type(pattern_id) {
        Some(Type::Top | Type::Variable(_) | Type::Cycle(_)) => true,
        Some(Type::Annotated { base, .. }) => head_may_match(tuple_id, *base, lookup),
        Some(Type::Union(members)) => members
            .iter()
            .any(|&member| head_may_match(tuple_id, member, lookup)),
        Some(Type::Intersection(members)) => members
            .iter()
            .all(|&member| head_may_match(tuple_id, member, lookup)),
        Some(Type::Tuple(expected)) => lookup.lookup_tuple(*expected).is_some_and(|expected| {
            expected.name == actual.name
                && expected.fields.len() == actual.fields.len()
                && expected
                    .fields
                    .iter()
                    .zip(&actual.fields)
                    .all(|((expected, _), (actual, _))| expected == actual)
        }),
        Some(Type::Partial { name, fields, .. }) => {
            (name.is_none() || *name == actual.name)
                && fields.iter().all(|(label, _)| {
                    actual
                        .fields
                        .iter()
                        .any(|(actual, _)| actual.as_deref() == Some(label.as_str()))
                })
        }
        _ => false,
    }
}

/// TypeLookup implementation for compatibility computation.
///
/// Answers ids beyond the program's own type table from [`Self::derived`]: the process
/// types this computation needs but a program need not contain (see
/// [`Self::process_type_id`]).
pub struct TypeLookupImpl<'a> {
    types: &'a [Type],
    tuples: &'a [TupleTypeInfo],
    /// Process types derived from the functions, for triples the type table lacks. Their
    /// ids continue after `types`, and are only ever used as the *value* side of a
    /// compatibility test — never as a pattern, and never stored in a table indexed by
    /// type id — so they need not be stable across calls.
    derived: Vec<Type>,
    /// Every process triple's type id, real where the table has one and derived otherwise.
    process_ids: HashMap<ProcessTypeKey, usize>,
}

impl<'a> TypeLookupImpl<'a> {
    pub fn new(types: &'a [Type], tuples: &'a [TupleTypeInfo], functions: &'a [Function]) -> Self {
        // Real entries first, so a triple the program does name resolves to its own id.
        let mut process_ids: HashMap<ProcessTypeKey, usize> = HashMap::new();
        for (type_id, ty) in types.iter().enumerate() {
            if let Type::Process {
                send,
                receive,
                state,
            } = ty
            {
                process_ids
                    .entry((*send, *receive, *state))
                    .or_insert(type_id);
            }
        }

        // Then one per function triple the program doesn't already name. Whether a
        // program happens to contain the process type for a function it spawns is
        // incidental — it depends on some site having *written* that exact type — but a
        // pid's compatibility must not be: a received pid is admitted to a mailbox only
        // if its type fits, and that test is what keeps bare `?p` sound.
        let mut derived = Vec::new();
        for func in functions {
            let (_, _, send, receive, state) = extract_function_type_info(func, types);
            process_ids
                .entry((send, receive, state))
                .or_insert_with(|| {
                    derived.push(Type::Process {
                        send,
                        receive,
                        state,
                    });
                    types.len() + derived.len() - 1
                });
        }

        TypeLookupImpl {
            types,
            tuples,
            derived,
            process_ids,
        }
    }

    /// The type id standing for a process with this `(send, receive, state)` triple.
    /// Total over the functions this was built from.
    fn process_type_id(&self, triple: ProcessTypeKey) -> Option<usize> {
        self.process_ids.get(&triple).copied()
    }

    /// Whether `type_id` is one of the derived entries rather than the program's own.
    fn is_derived(&self, type_id: usize) -> bool {
        type_id >= self.types.len()
    }
}

impl<'a> TypeLookup for TypeLookupImpl<'a> {
    fn lookup_type(&self, type_id: usize) -> Option<&Type> {
        match self.types.get(type_id) {
            Some(ty) => Some(ty),
            None => self.derived.get(type_id - self.types.len()),
        }
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
    /// Functions (to scan for `TestType` instructions and derive function types)
    pub functions: &'a [Function],
    /// Builtin information
    pub builtins: &'a [BuiltinInfo],
    /// Resource type names (index = resource_type_id)
    pub resource_names: &'a [String],
    /// Interned field names (index = field name id), for `field_offsets`.
    pub field_names: &'a [String],
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
            ..
        }) => (*parameter, type_id, Some(*receive), Some(*result), *states),
        _ => (0, type_id, None, None, None),
    }
}

/// Compute type compatibility table for all pattern types used in the program.
/// This precomputes which ConcreteTypes are compatible with which pattern types,
/// allowing O(1) runtime type checking instead of recursive type traversal.
/// Returns a Vec where index is type_id and value is the set of compatible concrete types.
pub fn compute_type_compatibility(input: &CompatibilityInput) -> Vec<ConcreteTypes> {
    let lookup = TypeLookupImpl::new(input.types, input.tuples, input.functions);
    let index = TypeIndex::build(input, &lookup, all_tuple_facts(input.tuples.len(), &lookup));

    // Collect all pattern type IDs (types used in `TestType` instructions)
    let mut pattern_type_ids = HashSet::new();

    for function in input.functions {
        for instruction in &function.instructions {
            if instruction.opcode() == Opcode::TestType {
                let type_id = instruction.operand();
                // Widen here, at the edge of the instruction stream; everything
                // downstream indexes tables and stays `usize`.
                pattern_type_ids.insert(type_id as usize);
            }
        }
    }

    // A type-consuming builtin's type argument is a runtime test too
    // (`%registry.lookup<'p>` tests a stored pid against it), so it seeds a pattern
    // row exactly as an `TestType` operand does.
    for info in input.builtins {
        if let Some(type_id) = info.type_argument {
            pattern_type_ids.insert(type_id);
        }
    }

    // Initialize the compatibility table with entries for all pattern types
    let mut compatible_with: Vec<ConcreteTypes> = vec![ConcreteTypes::default(); input.types.len()];

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

impl CompatibilityTables {
    /// Extend `canonical_tuples` and `field_offsets` over whatever the program has
    /// grown by. Both are pure functions of append-only tables, so a rebuild per update
    /// is pure waste — and a costly one once whole modules link, where the tables are
    /// several times larger than a tree-shaken merge left them.
    fn extend_tuple_tables(&mut self, input: &CompatibilityInput) {
        for id in self.canonical_tuples.len()..input.tuples.len() {
            let info = &input.tuples[id];
            let labels: Vec<Option<String>> =
                info.fields.iter().map(|(label, _)| label.clone()).collect();
            let canonical = *self.shapes.entry((info.name.clone(), labels)).or_insert(id);
            self.canonical_tuples.push(canonical);
        }

        // A new tuple appends one cell to every existing name's row...
        for (name_id, row) in self.field_offsets.iter_mut().enumerate() {
            let name = &input.field_names[name_id];
            for id in row.len()..input.tuples.len() {
                row.push(field_offset(&input.tuples[id], name));
            }
        }
        // ... and a new name needs a row over every tuple.
        for name_id in self.field_offsets.len()..input.field_names.len() {
            let name = &input.field_names[name_id];
            self.field_offsets.push(
                input
                    .tuples
                    .iter()
                    .map(|info| field_offset(info, name))
                    .collect(),
            );
        }
    }
}

fn field_offset(info: &TupleTypeInfo, name: &str) -> Option<usize> {
    info.fields
        .iter()
        .position(|(field, _)| field.as_deref() == Some(name))
}

/// Compute parameter compatibility for mailbox filtering.
/// For each function and builtin, computes which ConcreteTypes are compatible with its parameter type.
/// Returns (function_param_compatibility, builtin_param_compatibility).
pub fn compute_param_compatibility(
    input: &CompatibilityInput,
) -> (Vec<ConcreteTypes>, Vec<ConcreteTypes>) {
    let lookup = TypeLookupImpl::new(input.types, input.tuples, input.functions);
    let index = TypeIndex::build(input, &lookup, all_tuple_facts(input.tuples.len(), &lookup));

    // Many functions share a parameter type, so memoise the result by parameter type id.
    let mut memo: HashMap<usize, ConcreteTypes> = HashMap::new();
    let mut compatible_for = |param: usize| -> ConcreteTypes {
        memo.entry(param)
            .or_insert_with(|| compute_compatible_concrete_types(param, input, &lookup, &index))
            .clone()
    };

    // Compute function parameter compatibility by extracting type info from each function
    let function_params: Vec<ConcreteTypes> = input
        .functions
        .iter()
        .map(|func| {
            let (parameter, _, _, _, _) = extract_function_type_info(func, input.types);
            compatible_for(parameter)
        })
        .collect();

    // Compute builtin parameter compatibility
    let builtin_params: Vec<ConcreteTypes> = input
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
    /// Pattern type ids (`TestType` / checked `GetAnnotation` targets) already computed.
    pattern_ids: HashSet<usize>,
    /// Per-type compatibility, as `compute_type_compatibility` returns.
    pub type_compatibility: Vec<ConcreteTypes>,
    /// Per-function parameter compatibility, as `compute_param_compatibility` returns.
    pub function_params: Vec<ConcreteTypes>,
    /// Per-builtin parameter compatibility, as `compute_param_compatibility` returns.
    pub builtin_params: Vec<ConcreteTypes>,
    /// Canonical value-shape id per tuple, as `compute_canonical_tuples` returns.
    pub canonical_tuples: Vec<usize>,
    /// The shape → lowest-id map behind `canonical_tuples`. Kept so appending tuples
    /// costs one lookup each rather than a rebuild: existing entries can never change,
    /// since the map holds the *lowest* id for a shape and ids only ever grow.
    shapes: HashMap<(Option<String>, Vec<Option<String>>), usize>,
    /// What each tuple's type says about its values (see `TupleFacts`), and the memo behind
    /// it; both are functions of append-only tables, so they only ever extend.
    tuple_facts: Vec<TupleFacts>,
    facts_memo: FactsMemo,
    /// `[field name id][tuple id]` offsets, as `compute_field_offsets` returns.
    pub field_offsets: Vec<Vec<Option<usize>>>,
}

/// The extension one [`CompatibilityTables::update`] call made — everything a holder of
/// the previous tables needs to reach the new ones. This is what a serializing transport
/// ships: the tables are replaced wholesale on every update, so re-encoding them per
/// worker per update would swamp the code payload once whole modules link (measured at
/// 3.9 KB → 128 KB per update at module scale).
///
/// Not purely an append: an *existing* row gains members when a newly registered
/// concrete type satisfies its pattern or parameter type, and a row below the old
/// length is written whole when an old type is first used as a pattern — so additions
/// carry row indices rather than assuming the tail.
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct CompatibilityDelta {
    /// New `type_compatibility` length; rows created by the resize start empty.
    pub types_len: usize,
    /// Members inserted into `type_compatibility` rows (new or pre-existing).
    pub type_additions: Vec<(usize, Vec<(ConcreteType, Verdict)>)>,
    /// Rows appended to `function_params`, in order.
    pub function_rows: Vec<ConcreteTypes>,
    /// Members inserted into pre-existing `function_params` rows.
    pub function_additions: Vec<(usize, Vec<(ConcreteType, Verdict)>)>,
    /// Rows appended to `builtin_params`, in order.
    pub builtin_rows: Vec<ConcreteTypes>,
    /// Members inserted into pre-existing `builtin_params` rows.
    pub builtin_additions: Vec<(usize, Vec<(ConcreteType, Verdict)>)>,
    /// Entries appended to `canonical_tuples` (existing entries never change).
    pub canonical_appended: Vec<usize>,
    /// Cells appended to each pre-existing `field_offsets` row, in row order — every
    /// old row grows by the same new-tuple range.
    pub field_offset_extensions: Vec<Vec<Option<usize>>>,
    /// Rows appended to `field_offsets` (full rows over every tuple).
    pub field_offset_rows: Vec<Vec<Option<usize>>>,
}

impl CompatibilityDelta {
    /// Extend a holder's tables — which must be exactly the state the producing
    /// `update` call started from — to the state it ended at.
    pub fn apply(
        self,
        type_compatibility: &mut Vec<ConcreteTypes>,
        function_params: &mut Vec<ConcreteTypes>,
        builtin_params: &mut Vec<ConcreteTypes>,
        canonical_tuples: &mut Vec<usize>,
        field_offsets: &mut Vec<Vec<Option<usize>>>,
    ) {
        type_compatibility.resize(self.types_len, ConcreteTypes::default());
        for (row, members) in self.type_additions {
            type_compatibility[row].extend(members);
        }
        for (row, members) in self.function_additions {
            function_params[row].extend(members);
        }
        function_params.extend(self.function_rows);
        for (row, members) in self.builtin_additions {
            builtin_params[row].extend(members);
        }
        builtin_params.extend(self.builtin_rows);
        canonical_tuples.extend(self.canonical_appended);
        for (row, cells) in field_offsets.iter_mut().zip(self.field_offset_extensions) {
            row.extend(cells);
        }
        field_offsets.extend(self.field_offset_rows);
    }
}

impl CompatibilityTables {
    /// Extend the tables to cover `input`, which must describe an append-only extension
    /// of the program covered by the previous call (the environment's merged program).
    /// With `want_delta`, also answer the [`CompatibilityDelta`] this call amounts to —
    /// requested only when a serializing transport will ship it, since capturing the
    /// appended rows costs clones the shared-memory path has no use for.
    pub fn update(
        &mut self,
        input: &CompatibilityInput,
        want_delta: bool,
    ) -> Option<CompatibilityDelta> {
        let old_tuples = self.canonical_tuples.len();
        let old_field_rows = self.field_offsets.len();
        let old_function_rows = self.function_params.len();
        let old_builtin_rows = self.builtin_params.len();
        let mut type_additions: Vec<(usize, Vec<(ConcreteType, Verdict)>)> = Vec::new();
        let mut function_additions: Vec<(usize, Vec<(ConcreteType, Verdict)>)> = Vec::new();
        let mut builtin_additions: Vec<(usize, Vec<(ConcreteType, Verdict)>)> = Vec::new();

        self.extend_tuple_tables(input);
        let lookup = TypeLookupImpl::new(input.types, input.tuples, input.functions);
        for tuple_id in self.tuple_facts.len()..input.tuples.len() {
            self.tuple_facts
                .push(TupleFacts::of(tuple_id, &lookup, &mut self.facts_memo));
        }
        let index = TypeIndex::build(input, &lookup, self.tuple_facts.clone());

        self.type_compatibility
            .resize(input.types.len(), ConcreteTypes::default());

        // Newly testable concrete types, with the type id representing each in
        // `is_compatible` checks. A concrete becomes testable when its own id is new
        // (tuple/function/builtin/resource) or when the type-table entry the checks go
        // through first appears (`TypeIndex` keeps first occurrences, so an index entry
        // with a new type id means there was none before).
        let mut new_concretes: Vec<(ConcreteType, usize)> = Vec::new();
        let tuple_exact = |concrete: ConcreteType| match concrete {
            ConcreteType::Tuple(tuple_id) => Some((tuple_id, index.tuple_facts[tuple_id])),
            _ => None,
        };

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

        // Every function has a process type: new functions, plus old functions whose type
        // the program itself only just named. A derived id carries no such news — it
        // exists for as long as its function does — so it counts only when the function
        // is new, which is what keeps this from re-adding every process every round.
        for (func_id, func) in input.functions.iter().enumerate() {
            let (_, _, send, receive, state) = extract_function_type_info(func, input.types);
            if let Some(process_id) = lookup.process_type_id((send, receive, state))
                && (func_id >= self.functions_len
                    || (!lookup.is_derived(process_id) && process_id >= self.types_len))
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
            let mut verdicts: HashMap<usize, Vec<Option<Verdict>>> = HashMap::new();
            let mut verdicts_for = |pattern_id: usize| -> Vec<Option<Verdict>> {
                verdicts
                    .entry(pattern_id)
                    .or_insert_with(|| {
                        let stripped = Type::strip_annotations(pattern_id, &lookup);
                        new_concretes
                            .iter()
                            .map(|&(concrete, rep)| {
                                verdict(rep, tuple_exact(concrete), stripped, &lookup)
                            })
                            .collect()
                    })
                    .clone()
            };

            for &pattern_id in &self.pattern_ids {
                let mut added = Vec::new();
                for (hit, &(concrete, _)) in verdicts_for(pattern_id).iter().zip(&new_concretes) {
                    if let Some(verdict) = *hit
                        && self.type_compatibility[pattern_id]
                            .insert(concrete, verdict)
                            .is_none()
                        && want_delta
                    {
                        added.push((concrete, verdict));
                    }
                }
                if !added.is_empty() {
                    type_additions.push((pattern_id, added));
                }
            }
            for (func_id, func) in input.functions.iter().enumerate().take(self.functions_len) {
                let (parameter, _, _, _, _) = extract_function_type_info(func, input.types);
                let mut added = Vec::new();
                for (hit, &(concrete, _)) in verdicts_for(parameter).iter().zip(&new_concretes) {
                    if let Some(verdict) = *hit
                        && self.function_params[func_id]
                            .insert(concrete, verdict)
                            .is_none()
                        && want_delta
                    {
                        added.push((concrete, verdict));
                    }
                }
                if !added.is_empty() {
                    function_additions.push((func_id, added));
                }
            }
            for (builtin_id, info) in input.builtins.iter().enumerate().take(self.builtins_len) {
                let mut added = Vec::new();
                for (hit, &(concrete, _)) in
                    verdicts_for(info.param_type).iter().zip(&new_concretes)
                {
                    if let Some(verdict) = *hit
                        && self.builtin_params[builtin_id]
                            .insert(concrete, verdict)
                            .is_none()
                        && want_delta
                    {
                        added.push((concrete, verdict));
                    }
                }
                if !added.is_empty() {
                    builtin_additions.push((builtin_id, added));
                }
            }
        }

        // New pattern types (only new functions can introduce them) get a full scan, as does
        // a new builtin instantiation's type argument (a runtime test like a fresh `TestType`
        // operand). The pattern id may be an *old* type id first used as a pattern now, so its
        // delta entry is an addition at that row, not an append.
        let mut roots: Vec<usize> = Vec::new();
        for function in &input.functions[self.functions_len..] {
            for instruction in &function.instructions {
                if instruction.opcode() == Opcode::TestType {
                    roots.push(instruction.operand() as usize);
                }
            }
        }
        roots.extend(
            input.builtins[self.builtins_len..]
                .iter()
                .filter_map(|info| info.type_argument),
        );
        for type_id in roots {
            if type_id < input.types.len() && self.pattern_ids.insert(type_id) {
                let compatible = compute_compatible_concrete_types(type_id, input, &lookup, &index);
                if want_delta && !compatible.is_empty() {
                    type_additions
                        .push((type_id, compatible.iter().map(|(&c, &v)| (c, v)).collect()));
                }
                self.type_compatibility[type_id] = compatible;
            }
        }

        // New parameter rows get a full scan too, shared per parameter type.
        let mut memo: HashMap<usize, ConcreteTypes> = HashMap::new();
        let mut compatible_for = |param: usize| -> ConcreteTypes {
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

        want_delta.then(|| CompatibilityDelta {
            types_len: self.type_compatibility.len(),
            type_additions,
            function_rows: self.function_params[old_function_rows..].to_vec(),
            function_additions,
            builtin_rows: self.builtin_params[old_builtin_rows..].to_vec(),
            builtin_additions,
            canonical_appended: self.canonical_tuples[old_tuples..].to_vec(),
            field_offset_extensions: self.field_offsets[..old_field_rows]
                .iter()
                .map(|row| row[old_tuples..].to_vec())
                .collect(),
            field_offset_rows: self.field_offsets[old_field_rows..].to_vec(),
        })
    }

    /// Assert the incremental tables equal a from-scratch computation over `input`.
    /// For validation runs (opt-in, e.g. behind an environment variable) — a mismatch
    /// is a bug in `update`.
    ///
    /// `reclaimed` says the program has had code stubbed. `type_compatibility` is keyed
    /// by the pattern types `TestType` instructions name, and reclamation empties the
    /// instructions while leaving the types in place — so the incremental table keeps
    /// entries a recompute over the stubbed program no longer derives. That is not drift
    /// but the point: reviving stubbed code must not have to rebuild them. The check
    /// weakens to containment rather than lapsing. Every other table is untouched by
    /// reclamation — a stub keeps its type, and tuples and field names are never
    /// reclaimed — so those stay exact either way.
    pub fn assert_matches_full(&self, input: &CompatibilityInput, reclaimed: bool) {
        let type_compatibility = compute_type_compatibility(input);
        assert_eq!(
            self.type_compatibility.len(),
            type_compatibility.len(),
            "incremental type_compatibility covers a different type table"
        );
        if reclaimed {
            for (pattern, expected) in type_compatibility.iter().enumerate() {
                assert!(
                    expected
                        .iter()
                        .all(
                            |(concrete, verdict)| self.type_compatibility[pattern].get(concrete)
                                == Some(verdict)
                        ),
                    "incremental type_compatibility is missing entries for pattern type \
                     {pattern}: {:?}",
                    expected
                        .iter()
                        .filter(|(concrete, _)| !self.type_compatibility[pattern]
                            .contains_key(concrete))
                        .collect::<Vec<_>>()
                );
            }
        } else {
            assert_eq!(
                self.type_compatibility, type_compatibility,
                "incremental type_compatibility diverged from full recomputation"
            );
        }
        let (function_params, builtin_params) = compute_param_compatibility(input);
        assert_eq!(
            self.function_params, function_params,
            "incremental function_params diverged from full recomputation"
        );
        assert_eq!(
            self.builtin_params, builtin_params,
            "incremental builtin_params diverged from full recomputation"
        );
        assert_eq!(
            self.canonical_tuples,
            compute_canonical_tuples(input.tuples),
            "incremental canonical_tuples diverged from full recomputation"
        );
        assert_eq!(
            self.field_offsets,
            compute_field_offsets(input.field_names, input.tuples),
            "incremental field_offsets diverged from full recomputation"
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
    /// tuple_id -> what its type says about its values
    tuple_facts: Vec<TupleFacts>,
    /// (parameter, result) -> type id of a never-receiving `Type::Callable` (for builtins)
    callable_to_type: HashMap<(usize, usize), usize>,
    /// resource name -> type id of `Type::Resource`
    resource_to_type: HashMap<String, usize>,
}

impl TypeIndex {
    fn build(
        input: &CompatibilityInput,
        lookup: &TypeLookupImpl,
        tuple_facts: Vec<TupleFacts>,
    ) -> Self {
        let mut index = TypeIndex {
            integer: None,
            binary: None,
            reference: None,
            tuple_to_type: vec![None; input.tuples.len()],
            tuple_facts,
            callable_to_type: HashMap::new(),
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
                    omittable,
                } => {
                    // Builtin signatures never mark labels, so a marked callable must not
                    // claim this slot — it would hand a caller a calling convention the
                    // builtin never declared.
                    if omittable.is_empty()
                        && lookup.lookup_type(*receive).map(|t| t.is_never()) == Some(true)
                    {
                        index
                            .callable_to_type
                            .entry((*parameter, *result))
                            .or_insert(type_id);
                    }
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
) -> ConcreteTypes {
    let mut compat_set = ConcreteTypes::default();

    // Annotation rows are invisible to pattern matching, so a runtime type check
    // against `T @ row` must behave exactly as against `T`.
    let pattern_id = Type::strip_annotations(pattern_id, lookup);

    // A primitive is testable only through a type-table entry representing it (the
    // `TypeIndex` slot): a pattern that could match a primitive names it, so the
    // entry exists whenever the verdict could be positive. No fallback — in
    // particular an empty union matches nothing, consistent with `is_compatible`. The
    // top type is the exception, admitting a primitive it does not name.
    let top = matches!(lookup.lookup_type(pattern_id), Some(Type::Top));
    for (slot, concrete) in [
        (index.integer, ConcreteType::Integer),
        (index.binary, ConcreteType::Binary),
        (index.reference, ConcreteType::Reference),
    ] {
        if top || slot.is_some_and(|type_id| is_compatible(type_id, pattern_id, lookup)) {
            compat_set.insert(concrete, Verdict::Admit);
        }
    }

    // Check all Tuples
    for (tuple_id, &found_type_id) in index.tuple_to_type.iter().enumerate() {
        if let Some(type_id) = found_type_id
            && let Some(verdict) = verdict(
                type_id,
                Some((tuple_id, index.tuple_facts[tuple_id])),
                pattern_id,
                lookup,
            )
        {
            compat_set.insert(ConcreteType::Tuple(tuple_id), verdict);
        }
    }

    // Check all Functions - use their callable type ID from type_id
    for (func_id, func) in input.functions.iter().enumerate() {
        let (_, callable, _, _, _) = extract_function_type_info(func, input.types);
        if is_compatible(callable, pattern_id, lookup) {
            compat_set.insert(ConcreteType::Function(func_id), Verdict::Admit);
        }
    }

    // Check all Builtins - look up their (param, result) callable type
    for (builtin_id, builtin_info) in input.builtins.iter().enumerate() {
        if let Some(&callable_id) = index
            .callable_to_type
            .get(&(builtin_info.param_type, builtin_info.result_type))
            && is_compatible(callable_id, pattern_id, lookup)
        {
            compat_set.insert(ConcreteType::Builtin(builtin_id), Verdict::Admit);
        }
    }

    // Check all Processes - derive process type from function's send/receive/states. The
    // state component rides this check: it is what makes a received pid trustworthy for
    // bare `?p` (which has no runtime test of its own).
    for (func_id, func) in input.functions.iter().enumerate() {
        let (_, _, process_send, process_receive, process_state) =
            extract_function_type_info(func, input.types);
        if let Some(process_id) =
            lookup.process_type_id((process_send, process_receive, process_state))
            && is_compatible(process_id, pattern_id, lookup)
        {
            compat_set.insert(ConcreteType::Process(func_id), Verdict::Admit);
        }
    }

    // Check all Resources
    for (resource_id, resource_name) in input.resource_names.iter().enumerate() {
        if let Some(&type_id) = index.resource_to_type.get(resource_name)
            && is_compatible(type_id, pattern_id, lookup)
        {
            compat_set.insert(ConcreteType::Resource(resource_id), Verdict::Admit);
        }
    }

    compat_set
}

/// What a tuple's type says about the values built with it.
#[derive(Debug, Clone, Copy)]
struct TupleFacts {
    /// No type variable (a tuple built in generic code, whose `'t` was whatever the caller
    /// chose) and no recursive reference past the tuple (a member of a recursive union, whose
    /// `^` means nothing on its own): the type describes its values exactly enough for a test
    /// to trust it without looking inside.
    exact: bool,
    /// Holds a function or pid somewhere.
    opaque: bool,
}

/// Memos behind `TupleFacts`, by type id: both facts are context-free.
#[derive(Debug, Clone, Default)]
struct FactsMemo {
    escape_depths: HashMap<usize, Option<usize>>,
    opaque: HashMap<usize, bool>,
}

impl TupleFacts {
    fn of(tuple_id: usize, lookup: &impl TypeLookup, memo: &mut FactsMemo) -> Self {
        let fields: Vec<usize> = lookup
            .lookup_tuple(tuple_id)
            .map(|info| info.fields.iter().map(|&(_, t)| t).collect())
            .unwrap_or_default();
        TupleFacts {
            exact: fields
                .iter()
                .all(|&field| escape_depth(field, lookup, &mut memo.escape_depths) == Some(0)),
            opaque: fields
                .iter()
                .any(|&field| holds_opaque(field, lookup, &mut memo.opaque)),
        }
    }
}

/// `TupleFacts` for every tuple.
fn all_tuple_facts(tuples: usize, lookup: &impl TypeLookup) -> Vec<TupleFacts> {
    let mut memo = FactsMemo::default();
    (0..tuples)
        .map(|tuple_id| TupleFacts::of(tuple_id, lookup, &mut memo))
        .collect()
}

/// Whether a type's values may hold a function or pid.
fn holds_opaque(type_id: usize, lookup: &impl TypeLookup, memo: &mut HashMap<usize, bool>) -> bool {
    if let Some(&known) = memo.get(&type_id) {
        return known;
    }
    let parts: Vec<usize> = match lookup.lookup_type(type_id) {
        Some(Type::Callable { .. } | Type::Process { .. }) => {
            memo.insert(type_id, true);
            return true;
        }
        Some(Type::Tuple(tuple_id)) => lookup
            .lookup_tuple(*tuple_id)
            .map(|info| info.fields.iter().map(|&(_, t)| t).collect())
            .unwrap_or_default(),
        Some(Type::Partial { fields, rest, .. }) => {
            fields.iter().map(|&(_, t)| t).chain(*rest).collect()
        }
        Some(Type::Union(members) | Type::Intersection(members)) => members.clone(),
        Some(Type::Annotated { base, .. }) => vec![*base],
        // A value of the top type or a type variable may be anything, a function included.
        Some(Type::Top | Type::Variable(_)) => {
            memo.insert(type_id, true);
            return true;
        }
        _ => vec![],
    };
    let opaque = parts
        .into_iter()
        .any(|part| holds_opaque(part, lookup, memo));
    memo.insert(type_id, opaque);
    opaque
}

/// How many binders past `type_id` its recursive references reach (0 when it is closed), or
/// `None` when it mentions a type variable. Context-free, so memoised by id.
fn escape_depth(
    type_id: usize,
    lookup: &impl TypeLookup,
    memo: &mut HashMap<usize, Option<usize>>,
) -> Option<usize> {
    if let Some(&known) = memo.get(&type_id) {
        return known;
    }
    // The parts to look through, and whether `type_id` is a binder (which closes a reference
    // to itself).
    let (parts, binder): (Vec<usize>, bool) = match lookup.lookup_type(type_id) {
        None | Some(Type::Variable(_)) => {
            memo.insert(type_id, None);
            return None;
        }
        Some(Type::Cycle(depth)) => return Some(*depth),
        Some(Type::Integer | Type::Binary | Type::Reference | Type::Resource(_) | Type::Top) => {
            (vec![], false)
        }
        Some(Type::Tuple(tuple_id)) => (
            lookup
                .lookup_tuple(*tuple_id)
                .map(|info| info.fields.iter().map(|&(_, t)| t).collect())
                .unwrap_or_default(),
            false,
        ),
        Some(Type::Partial { fields, rest, .. }) => {
            (fields.iter().map(|&(_, t)| t).chain(*rest).collect(), false)
        }
        Some(Type::Intersection(members)) => (members.clone(), false),
        Some(Type::Process {
            send,
            receive,
            state,
        }) => (
            [*send, *receive, *state].into_iter().flatten().collect(),
            false,
        ),
        Some(Type::Annotated { base, .. }) => (vec![*base], false),
        Some(Type::Union(members)) => (members.clone(), true),
        Some(Type::Callable {
            parameter,
            result,
            receive,
            states,
            ..
        }) => (
            [*parameter, *result, *receive]
                .into_iter()
                .chain(*states)
                .collect(),
            true,
        ),
    };
    let mut deepest = Some(0);
    for part in parts {
        deepest = match (deepest, escape_depth(part, lookup, memo)) {
            (Some(a), Some(b)) => Some(a.max(b)),
            _ => None,
        };
    }
    let depth = deepest.map(|depth| {
        if binder {
            depth.saturating_sub(1)
        } else {
            depth
        }
    });
    memo.insert(type_id, depth);
    depth
}

/// Whether a function, builtin or pid of this concrete type fits a type: what a walk over a
/// value asks of a part it can't look inside. Only walks need it, so the executor computes it
/// on demand rather than the tables holding a row for every function type a tested type
/// contains. `builtin_signature` is a builtin's `(parameter, result)`.
pub fn opaque_fits(
    concrete: ConcreteType,
    pattern_id: usize,
    types: &[Type],
    tuples: &[TupleTypeInfo],
    functions: &[Function],
    builtin_signature: Option<(usize, usize)>,
) -> bool {
    let lookup = TypeLookupImpl::new(types, tuples, functions);
    let rep = match concrete {
        ConcreteType::Function(func_id) => functions.get(func_id).map(|func| func.type_id),
        ConcreteType::Process(func_id) => functions.get(func_id).and_then(|func| {
            let (_, _, send, receive, state) = extract_function_type_info(func, types);
            lookup.process_type_id((send, receive, state))
        }),
        // A builtin is represented as `TypeIndex` does: a plain callable of its signature.
        ConcreteType::Builtin(_) => builtin_signature.and_then(|(parameter, result)| {
            types.iter().position(|ty| {
                matches!(ty, Type::Callable { parameter: p, result: r, receive, omittable, .. }
                    if *p == parameter
                        && *r == result
                        && omittable.is_empty()
                        && lookup.lookup_type(*receive).is_some_and(Type::is_never))
            })
        }),
        _ => None,
    };
    rep.is_some_and(|rep| is_compatible(rep, pattern_id, &lookup))
}
