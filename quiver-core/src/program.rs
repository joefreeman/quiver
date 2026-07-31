use crate::executor::Executor;
use crate::types::{BuiltinInfo, NIL, OK, TupleTypeInfo, Type, TypeLookup};
use crate::value::{Binary, Payload, Value};
use serde::{Deserialize, Serialize};

// Re-export bytecode types
pub use crate::bytecode::{Bytecode, Constant, Function, Id, Instruction};

/// Program represents the compiled program data that is static during execution.
/// It contains constants, functions, builtins, and type information.
/// The compiler writes to this structure, and the VM reads from it.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Program {
    constants: Vec<Constant>,
    functions: Vec<Function>,
    builtins: Vec<BuiltinInfo>,
    tuples: Vec<TupleTypeInfo>,
    types: Vec<Type>,
    /// Annotation key names, interned by `register_annotation_key`; the index is the key
    /// id carried by `Annotate`/`GetAnnotation`. Keys are just names — their value types
    /// are inferred per attach site and tracked in annotation rows (`Type::Annotated`).
    #[serde(default)]
    annotation_keys: Vec<String>,
    /// Field names interned by `register_field_name`; the index is the field-name id
    /// carried by `GetNamed`. Only load-time table construction reads the names.
    #[serde(default)]
    field_names: Vec<String>,
    /// `(tuple_id, field_index)` pairs whose field label was written omittable
    /// (`[(foo): 'int]`). Metadata about written spellings, consulted when a positional
    /// tuple literal is checked against the tuple type — never part of type identity,
    /// so structurally identical spellings share one entry (marking any marks all).
    #[serde(default)]
    omittable_labels: std::collections::BTreeSet<(usize, usize)>,
    /// Failure-provenance table (debug builds only): sites indexed by `Stamp`
    /// instructions, plus the tuple/key ids the executor needs to prebuild the values.
    #[serde(default)]
    debug: Option<crate::bytecode::SiteTable>,
    /// Functions below this index are invisible to `register_function`'s structural
    /// interning (see there). Transient compile state, 0 outside module compiles.
    #[serde(skip)]
    function_dedup_floor: usize,
    /// Interning indexes (value → first-occurrence id) for the registries above, so the
    /// `register_*` methods are hash lookups instead of scans of the whole table. Skipped
    /// by serde and lazily rebuilt on first registration after deserialization; mapping
    /// to the first occurrence matches the scan behaviour they replace.
    #[serde(skip)]
    constant_index: std::collections::HashMap<Constant, usize>,
    #[serde(skip)]
    type_index: std::collections::HashMap<Type, usize>,
    #[serde(skip)]
    tuple_index: std::collections::HashMap<TupleTypeInfo, usize>,
    #[serde(skip)]
    annotation_key_index: std::collections::HashMap<String, usize>,
    #[serde(skip)]
    field_name_index: std::collections::HashMap<String, usize>,
}

impl TypeLookup for Program {
    fn lookup_type(&self, type_id: usize) -> Option<&Type> {
        self.types.get(type_id)
    }

    fn lookup_tuple(&self, tuple_id: usize) -> Option<&TupleTypeInfo> {
        self.tuples.get(tuple_id)
    }

    fn lookup_annotation_key_name(&self, key: usize) -> Option<&str> {
        self.annotation_keys.get(key).map(|name| name.as_str())
    }

    fn label_omittable(&self, tuple_id: usize, field_index: usize) -> bool {
        self.omittable_labels.contains(&(tuple_id, field_index))
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

impl Program {
    pub fn get_constant(&self, index: usize) -> Option<&Constant> {
        self.constants.get(index)
    }

    pub fn new() -> Self {
        let mut program = Self {
            constants: Vec::new(),
            functions: Vec::new(),
            builtins: Vec::new(),
            tuples: Vec::new(),
            types: Vec::new(),
            annotation_keys: Vec::new(),
            field_names: Vec::new(),
            omittable_labels: std::collections::BTreeSet::new(),
            debug: None,
            function_dedup_floor: 0,
            constant_index: std::collections::HashMap::new(),
            type_index: std::collections::HashMap::new(),
            tuple_index: std::collections::HashMap::new(),
            annotation_key_index: std::collections::HashMap::new(),
            field_name_index: std::collections::HashMap::new(),
        };

        // Register built-in tuple types
        let nil_tuple_id = program.register_tuple(None, vec![]);
        assert_eq!(nil_tuple_id, NIL);

        let ok_tuple_id = program.register_tuple(Some("Ok".to_string()), vec![]);
        assert_eq!(ok_tuple_id, OK);

        program
    }

    pub fn register_constant(&mut self, constant: Constant) -> usize {
        if self.constant_index.is_empty() && !self.constants.is_empty() {
            for (index, existing) in self.constants.iter().enumerate() {
                self.constant_index.entry(existing.clone()).or_insert(index);
            }
        }
        if let Some(&index) = self.constant_index.get(&constant) {
            return index;
        }
        let index = self.constants.len();
        self.constants.push(constant.clone());
        self.constant_index.insert(constant, index);
        index
    }

    /// The number of registered types. Monotonically increasing, so it doubles as a
    /// persistent uniquifier seed (e.g. for type-parameter names) that survives across
    /// compiler instances sharing this program.
    pub fn type_count(&self) -> usize {
        self.types.len()
    }

    pub fn get_constants(&self) -> &Vec<Constant> {
        &self.constants
    }

    /// Register a function and return its index.
    /// Deduplicates based on full equality (instructions, captures, type_id).
    pub fn register_function(&mut self, function: Function) -> usize {
        // Structural interning never reaches below the dedup floor: while a module
        // compiles (or links), its functions must not collapse onto other modules'
        // structurally identical ones — function identity is attributed per module,
        // and cross-module collapse would make that attribution depend on session
        // history. Collapsing across the whole program is the runtime merge's job.
        if let Some(index) = self.functions[self.function_dedup_floor..]
            .iter()
            .position(|f| f == &function)
        {
            self.function_dedup_floor + index
        } else {
            self.functions.push(function);
            self.functions.len() - 1
        }
    }

    /// Set the function-interning floor (see [`Self::register_function`]), returning
    /// the previous one so callers can restore it stack-fashion around a module
    /// compile.
    pub fn set_function_dedup_floor(&mut self, floor: usize) -> usize {
        std::mem::replace(&mut self.function_dedup_floor, floor)
    }

    /// Append a function without structural interning — the linker's registration
    /// primitive. Ids are sequential, so a caller can precompute where a batch of
    /// functions will land and remap mutually-referencing bodies before pushing any.
    pub fn push_function(&mut self, function: Function) -> usize {
        self.functions.push(function);
        self.functions.len() - 1
    }

    pub fn get_functions(&self) -> &Vec<Function> {
        &self.functions
    }

    pub fn get_function(&self, index: usize) -> Option<&Function> {
        self.functions.get(index)
    }

    pub fn register_builtin<E: crate::effects::Effect>(
        &mut self,
        name: String,
        registry: &crate::builtins::BuiltinRegistry<E>,
    ) -> usize {
        // Check if builtin already exists
        if let Some(index) = self.builtins.iter().position(|b| b.name == name) {
            return index;
        }

        // Look up type specs from the builtin registry and resolve them
        let builtin_info = if let Some((param_spec, result_spec)) = registry.get_specs(&name) {
            let param_type = param_spec.resolve_to_id(self);
            let result_type = result_spec.resolve_to_id(self);

            BuiltinInfo {
                name: name.clone(),
                param_type,
                result_type,
            }
        } else {
            // Builtin not found in registry - this shouldn't happen in well-formed programs
            // Create a placeholder with bottom types (never type)
            let never_id = self.register_type(Type::Union(vec![]));
            BuiltinInfo {
                name,
                param_type: never_id,
                result_type: never_id,
            }
        };

        self.builtins.push(builtin_info);
        self.builtins.len() - 1
    }

    /// Register a builtin with pre-resolved type information.
    /// Used when loading bytecode that already has resolved builtin types.
    pub fn register_builtin_info(&mut self, info: BuiltinInfo) -> usize {
        // Check if builtin already exists
        if let Some(index) = self.builtins.iter().position(|b| b.name == info.name) {
            return index;
        }

        self.builtins.push(info);
        self.builtins.len() - 1
    }

    pub fn get_builtins(&self) -> &Vec<BuiltinInfo> {
        &self.builtins
    }

    /// Register a tuple type with field type IDs
    pub fn register_tuple(
        &mut self,
        name: Option<String>,
        fields: Vec<(Option<String>, usize)>,
    ) -> usize {
        if self.tuple_index.is_empty() && !self.tuples.is_empty() {
            for (index, existing) in self.tuples.iter().enumerate() {
                self.tuple_index.entry(existing.clone()).or_insert(index);
            }
        }
        let info = TupleTypeInfo { name, fields };
        if let Some(&index) = self.tuple_index.get(&info) {
            return index;
        }
        let tuple_id = self.tuples.len();
        self.tuples.push(info.clone());
        self.tuple_index.insert(info, tuple_id);
        tuple_id
    }

    /// Mark a tuple field's label as omittable at checked literals (`[(foo): 'int]`).
    pub fn mark_label_omittable(&mut self, tuple_id: usize, field_index: usize) {
        self.omittable_labels.insert((tuple_id, field_index));
    }

    /// Register a type for use with IsType instruction
    pub fn register_type(&mut self, typ: Type) -> usize {
        if self.type_index.is_empty() && !self.types.is_empty() {
            for (index, existing) in self.types.iter().enumerate() {
                self.type_index.entry(existing.clone()).or_insert(index);
            }
        }
        if let Some(&index) = self.type_index.get(&typ) {
            return index;
        }
        let type_id = self.types.len();
        self.types.push(typ.clone());
        self.type_index.insert(typ, type_id);
        type_id
    }

    /// Intern an annotation key name, returning its key id.
    pub fn register_annotation_key(&mut self, name: &str) -> usize {
        if self.annotation_key_index.is_empty() && !self.annotation_keys.is_empty() {
            for (index, existing) in self.annotation_keys.iter().enumerate() {
                self.annotation_key_index
                    .entry(existing.clone())
                    .or_insert(index);
            }
        }
        if let Some(&index) = self.annotation_key_index.get(name) {
            return index;
        }
        let key_id = self.annotation_keys.len();
        self.annotation_keys.push(name.to_string());
        self.annotation_key_index.insert(name.to_string(), key_id);
        key_id
    }

    pub fn get_annotation_keys(&self) -> &Vec<String> {
        &self.annotation_keys
    }

    /// Intern a field name for `GetNamed`, returning its field-name id.
    pub fn register_field_name(&mut self, name: &str) -> usize {
        if self.field_name_index.is_empty() && !self.field_names.is_empty() {
            for (index, existing) in self.field_names.iter().enumerate() {
                self.field_name_index
                    .entry(existing.clone())
                    .or_insert(index);
            }
        }
        if let Some(&index) = self.field_name_index.get(name) {
            return index;
        }
        let name_id = self.field_names.len();
        self.field_names.push(name.to_string());
        self.field_name_index.insert(name.to_string(), name_id);
        name_id
    }

    pub fn get_field_names(&self) -> &Vec<String> {
        &self.field_names
    }

    /// Get the type ID for the NEVER type (empty union / bottom type).
    /// Registers it if not already present.
    pub fn never(&mut self) -> usize {
        self.register_type(Type::Union(vec![]))
    }

    /// Intern an annotated type, normalising: entries sorted and unique by key; an
    /// open-empty row is the plain base and is never interned; annotating an already
    /// annotated base folds into its row (later entries replace); annotating a union
    /// distributes over its members.
    pub fn annotate_type(
        &mut self,
        base: usize,
        exact: bool,
        entries: Vec<(usize, usize)>,
    ) -> usize {
        match self.types.get(base).cloned() {
            Some(Type::Annotated {
                base: inner,
                exact: inner_exact,
                entries: inner_entries,
            }) => {
                let mut merged: Vec<(usize, usize)> = inner_entries
                    .into_iter()
                    .filter(|(key, _)| !entries.iter().any(|(new_key, _)| new_key == key))
                    .collect();
                merged.extend(entries);
                // Attaching preserves the base's exactness: entries are added/replaced,
                // and what is (un)known about the rest of the row is unchanged.
                self.annotate_type(inner, inner_exact, merged)
            }
            Some(Type::Union(members)) => {
                let annotated: Vec<usize> = members
                    .iter()
                    .map(|&member| self.annotate_type(member, exact, entries.clone()))
                    .collect();
                self.register_type(Type::Union(annotated))
            }
            _ => {
                if entries.is_empty() && !exact {
                    return base;
                }
                let mut entries = entries;
                entries.sort_by_key(|(key, _)| *key);
                entries.dedup_by_key(|(key, _)| *key);
                self.register_type(Type::Annotated {
                    base,
                    exact,
                    entries,
                })
            }
        }
    }

    pub fn get_tuples(&self) -> &Vec<TupleTypeInfo> {
        &self.tuples
    }

    pub fn get_types(&self) -> &Vec<Type> {
        &self.types
    }

    /// Collect unique resource type names from the types registry
    pub fn collect_resource_names(&self) -> Vec<String> {
        let mut names = Vec::new();
        for typ in &self.types {
            if let Type::Resource(name) = typ
                && !names.contains(name)
            {
                names.push(name.clone());
            }
        }
        names
    }

    /// Convert this program to bytecode format with an optional entry point.
    /// Does not perform tree shaking - use `to_bytecode_optimized` for that.
    /// Type compatibility is computed when the bytecode is loaded for execution.
    pub fn to_bytecode(&self, entry: Option<usize>) -> Bytecode {
        // Collect resource names for bytecode
        let resource_names = self.collect_resource_names();

        Bytecode {
            constants: self.constants.clone(),
            functions: self.functions.clone(),
            builtins: self.builtins.clone(),
            entry,
            tuples: self.tuples.clone(),
            types: self.types.clone(),
            resources: resource_names,
            annotation_keys: self.annotation_keys.clone(),
            field_names: self.field_names.clone(),
            debug: self.debug.clone(),
        }
    }

    /// The failure-provenance table of a debug build, if any.
    pub fn debug_sites(&self) -> Option<&crate::bytecode::SiteTable> {
        self.debug.as_ref()
    }

    /// Resolve the host's runtime-delivered vocabulary against this (merged) program,
    /// demand-scoped: crash shapes whenever declared (any process can crash), the
    /// `Changed` wakeup only when its demanding builtin (`track`) is referenced, and
    /// stream events for the stream resource kinds the program names. Registration is
    /// content-addressed — repeated calls (e.g. per REPL merge) return stable ids,
    /// and the shapes share ids with any source-level twins (`std/proc.qv`,
    /// `std/tcp.qv`).
    pub fn runtime_tables(
        &mut self,
        declarations: &crate::builtins::RuntimeDeclarations,
    ) -> Result<crate::bytecode::RuntimeTables, crate::error::Error> {
        let crash = match &declarations.crash {
            Some(decl) => {
                let crash_key = self.register_annotation_key(&decl.crash_key);
                let timeout_key = self.register_annotation_key(&decl.timeout_key);
                let str_tuple = self.vocabulary_tuple(&decl.str)?;
                let error_tuple = self.vocabulary_tuple(&decl.error)?;
                let panic_tuple = self.vocabulary_tuple(&decl.panic)?;
                let killed_tuple = self.vocabulary_tuple(&decl.killed)?;
                // Give the crash union a type-table presence: checked retrievals
                // (`x:('t)crash`) enumerate compatible concrete types from it.
                let member_types: Vec<usize> = [error_tuple, panic_tuple, killed_tuple]
                    .into_iter()
                    .map(|tuple_id| self.register_type(Type::Tuple(tuple_id)))
                    .collect();
                self.register_type(Type::Union(member_types));
                Some(crate::bytecode::CrashTable {
                    crash_key,
                    timeout_key,
                    error_tuple,
                    panic_tuple,
                    killed_tuple,
                    str_tuple,
                })
            }
            None => None,
        };

        let changed = match &declarations.changed {
            Some(decl) if self.references_builtin(&decl.demand_builtin) => {
                let tuple = self.vocabulary_tuple(&decl.tuple)?;
                // Type-table presence so a delivered `Changed` pattern-matches.
                self.register_type(Type::Tuple(tuple));
                Some(tuple)
            }
            _ => None,
        };

        let streams = self.derive_stream_table(&declarations.streams)?;

        Ok(crate::bytecode::RuntimeTables {
            crash,
            changed,
            streams,
        })
    }

    /// Whether this program references the named builtin (the demand signal for
    /// gated runtime vocabulary — builtins are registered on use).
    fn references_builtin(&self, name: &str) -> bool {
        self.builtins.iter().any(|b| b.name == name)
    }

    /// Resolve a vocabulary declaration's tuple to its tuple id (registering it —
    /// content-addressed, so it matches any source-level twin of the same shape).
    fn vocabulary_tuple(
        &mut self,
        spec: &crate::builtins::TypeSpec,
    ) -> Result<usize, crate::error::Error> {
        match spec.resolve(self) {
            Type::Tuple(tuple_id) => Ok(tuple_id),
            other => Err(crate::error::Error::InvalidArgument(format!(
                "vocabulary spec must be a tuple, got {other:?}"
            ))),
        }
    }

    /// Resolve a stream declaration's event tuple to its tuple id (registering it —
    /// content-addressed, so it matches any source-level twin of the same shape).
    fn stream_event_tuple(
        &mut self,
        spec: &crate::builtins::TypeSpec,
    ) -> Result<usize, crate::error::Error> {
        match spec.resolve(self) {
            Type::Tuple(tuple_id) => Ok(tuple_id),
            other => Err(crate::error::Error::InvalidArgument(format!(
                "stream event spec must be a tuple, got {other:?}"
            ))),
        }
    }

    /// Derive the runtime stream table from the registry's stream declarations, for
    /// the resource kinds this program actually names — a program touching no stream
    /// resources gets an empty table and registers no event tuples. Indexed by
    /// resource type id (`collect_resource_names` order, the same ids `Value::Resource`
    /// carries).
    fn derive_stream_table(
        &mut self,
        specs: &std::collections::HashMap<String, crate::builtins::StreamSpec>,
    ) -> Result<crate::bytecode::StreamTable, crate::error::Error> {
        // Two passes: resolving a spec's tuples can itself register resource names
        // (a listener's spec names the socket kind it produces), so resolve first,
        // then index against the final name order — the same order `Value::Resource`
        // type ids use.
        // (data tuple, resource tuple + produced kind, end tuple) per stream name.
        type ResolvedSpec = (Option<usize>, Option<(usize, String)>, usize);
        let initial = self.collect_resource_names();
        let mut resolved: std::collections::HashMap<String, ResolvedSpec> =
            std::collections::HashMap::new();
        for name in &initial {
            let Some(spec) = specs.get(name) else {
                continue;
            };
            let data_tuple = match &spec.data {
                Some(s) => Some(self.stream_event_tuple(s)?),
                None => None,
            };
            let resource_tuple = match &spec.resource {
                Some((s, produced)) => Some((self.stream_event_tuple(s)?, produced.clone())),
                None => None,
            };
            let end_tuple = self.stream_event_tuple(&spec.end)?;
            resolved.insert(name.clone(), (data_tuple, resource_tuple, end_tuple));
        }
        let names = self.collect_resource_names();
        let mut streams = Vec::with_capacity(names.len());
        for name in &names {
            let Some((data_tuple, resource_tuple, end_tuple)) = resolved.get(name) else {
                streams.push(None);
                continue;
            };
            let resource_tuple = match resource_tuple {
                Some((tuple, produced)) => {
                    let produced_type =
                        names.iter().position(|n| n == produced).ok_or_else(|| {
                            crate::error::Error::InvalidArgument(format!(
                                "stream `{name}` produces unregistered resource `{produced}`"
                            ))
                        })?;
                    Some((*tuple, produced_type))
                }
                None => None,
            };
            streams.push(Some(crate::bytecode::StreamInfo {
                data_tuple: *data_tuple,
                resource_tuple,
                end_tuple: *end_tuple,
            }));
        }
        Ok(crate::bytecode::StreamTable { streams })
    }

    /// Register a failure-provenance site (debug builds), creating the table — its
    /// `origin` key, `Site` tuple shape and kind markers — on first use. Returns the site
    /// id a `Stamp` instruction carries.
    pub fn register_debug_site(&mut self, site: crate::bytecode::Site) -> usize {
        if self.debug.is_none() {
            let origin_key = self.register_annotation_key("origin");
            let binary_type = self.register_type(Type::Binary);
            let str_tuple = self.register_tuple(Some("Str".to_string()), vec![(None, binary_type)]);
            let str_type = self.register_type(Type::Tuple(str_tuple));
            let integer_type = self.register_type(Type::Integer);
            let kind_tuples: Vec<usize> = crate::bytecode::SiteKind::ALL
                .iter()
                .map(|kind| self.register_tuple(Some(kind.name().to_string()), vec![]))
                .collect();
            let kind_type_ids: Vec<usize> = kind_tuples
                .iter()
                .map(|&tuple_id| self.register_type(Type::Tuple(tuple_id)))
                .collect();
            let kind_type = self.register_type(Type::Union(kind_type_ids));
            let site_tuple = self.register_tuple(
                Some("Site".to_string()),
                vec![
                    (Some("module".to_string()), str_type),
                    (Some("line".to_string()), integer_type),
                    (Some("column".to_string()), integer_type),
                    (Some("kind".to_string()), kind_type),
                ],
            );
            // Give the Site tuple a type-table presence: checked retrievals
            // (`x:((line: 'int))origin`) enumerate compatible concrete types from the
            // types table, and imports walk it types-first.
            self.register_type(Type::Tuple(site_tuple));
            self.debug = Some(crate::bytecode::SiteTable {
                origin_key,
                site_tuple,
                str_tuple,
                kind_tuples,
                sites: Vec::new(),
            });
        }
        let table = self.debug.as_mut().expect("just initialised");
        // Dedup, like the other register_* methods: re-registration (e.g. the REPL
        // re-merging its accumulated program every line) must return stable ids, or the
        // shifted `Stamp` operands defeat function dedup and snowball each merge.
        if let Some(existing) = table.sites.iter().position(|s| *s == site) {
            return existing;
        }
        table.sites.push(site);
        table.sites.len() - 1
    }

    /// Convert this program to optimized bytecode format.
    /// Performs tree shaking to remove unused functions, constants, and types.
    pub fn to_bytecode_optimized(&self, entry: usize) -> Bytecode {
        crate::optimisation::tree_shake(self.to_bytecode(Some(entry)), entry)
    }

    /// Inject function captures into a function, returning a new function index.
    /// Creates a new function that converts each capture value to instructions,
    /// stores them in locals, then executes the original function's instructions.
    pub fn inject_function_captures<E: crate::effects::Effect>(
        &mut self,
        function_index: usize,
        captures: Vec<Value>,
        executor: &Executor<E>,
    ) -> usize {
        let mut instructions = Vec::new();

        for capture_value in captures.iter() {
            instructions.extend(self.value_to_instructions(capture_value, executor));
            instructions.push(Instruction::Store);
        }

        let func = self
            .get_function(function_index)
            .expect("Function should exist during capture injection");
        instructions.extend(func.instructions.clone());
        let type_id = func.type_id;

        let new_func = Function {
            instructions,
            captures: 0,
            type_id,
        };

        self.register_function(new_func)
    }

    /// Append instructions reconstructing a payload's annotations onto the value the
    /// preceding instructions left on the stack (helper for `value_to_instructions`).
    fn annotations_to_instructions<E: crate::effects::Effect>(
        &mut self,
        instrs: &mut Vec<Instruction>,
        payload: &crate::value::Payload,
        executor: &Executor<E>,
    ) {
        for (key, value) in payload.annotations() {
            instrs.extend(self.value_to_instructions(value, executor));
            instrs.push(Instruction::Annotate(*key as Id));
        }
    }

    /// Convert a runtime value to instructions that reconstruct it.
    /// This is used for serializing values (like captures) back into bytecode.
    fn value_to_instructions<E: crate::effects::Effect>(
        &mut self,
        value: &Value,
        executor: &Executor<E>,
    ) -> Vec<Instruction> {
        match value {
            Value::Int(n) => {
                let const_idx = self.register_constant(Constant::Integer((*n).into()));
                vec![Instruction::Constant(const_idx as Id)]
            }
            Value::BigInt(n) => {
                let const_idx = self.register_constant(Constant::Integer((**n).clone()));
                vec![Instruction::Constant(const_idx as Id)]
            }
            Value::Binary(binary) => {
                match binary {
                    Binary::Constant(const_idx) => {
                        // Already a constant, just reference it
                        vec![Instruction::Constant(*const_idx as Id)]
                    }
                    Binary::Data(data) => {
                        // Owns its bytes — register them as a constant of this program.
                        let const_idx = self.register_constant(Constant::Binary(data.to_vec()));
                        vec![Instruction::Constant(const_idx as Id)]
                    }
                }
            }
            Value::Tuple(tuple_id, elements) => {
                let mut instrs = Vec::new();
                for elem in elements.iter() {
                    instrs.extend(self.value_to_instructions(elem, executor));
                }
                instrs.push(Instruction::Tuple(*tuple_id as Id));
                self.annotations_to_instructions(&mut instrs, elements, executor);
                instrs
            }
            Value::Function(function, captures) => {
                let func_index = if !captures.is_empty() {
                    self.inject_function_captures(*function, captures.to_vec(), executor)
                } else {
                    *function
                };
                let mut instrs = vec![Instruction::Function(func_index as Id)];
                self.annotations_to_instructions(&mut instrs, captures, executor);
                instrs
            }
            Value::Builtin(builtin_id, payload) => {
                // An instantiated builtin re-emits its type argument on the push.
                let type_argument = payload.as_deref().and_then(Payload::type_argument);
                let mut instrs = vec![Instruction::Builtin(
                    *builtin_id as Id,
                    type_argument.map(|id| id as Id),
                )];
                if let Some(payload) = payload {
                    self.annotations_to_instructions(&mut instrs, payload, executor);
                }
                instrs
            }
            Value::Process(_, _) => {
                panic!("Cannot convert pid to instructions")
            }
            Value::Resource(..) => {
                panic!("Cannot convert resource to instructions")
            }
            Value::Reference(_) => {
                panic!("Cannot convert ref to instructions")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builtins::BuiltinRegistry;
    use crate::effects::Effect;
    use crate::value::ResourceId;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    struct TestEffect;
    impl Effect for TestEffect {
        fn resource_id(&self) -> Option<ResourceId> {
            None
        }
    }

    fn registry() -> BuiltinRegistry<TestEffect> {
        let mut registry = BuiltinRegistry::with_modules(&crate::builtins::core_modules());
        for module in crate::builtins::io_modules() {
            module(&mut registry);
        }
        registry
    }

    #[test]
    fn runtime_tables_are_demand_scoped() {
        let registry = registry();
        let declarations = registry.runtime_declarations().clone();

        // A program touching nothing gated: crash vocabulary always resolves; the
        // Changed wakeup and stream events do not.
        let mut bare = Program::new();
        let tables = bare.runtime_tables(&declarations).unwrap();
        assert!(tables.crash.is_some());
        assert!(tables.changed.is_none());
        assert!(tables.streams.streams.iter().all(|s| s.is_none()));

        // Referencing `track` demands the Changed vocabulary.
        let mut tracking = Program::new();
        tracking.register_builtin("track".to_string(), &registry);
        let tables = tracking.runtime_tables(&declarations).unwrap();
        assert!(tables.changed.is_some());
        assert!(tables.streams.streams.iter().all(|s| s.is_none()));

        // Referencing a network builtin names the stream resource kinds, demanding
        // their event vocabulary — and the produced-kind link resolves.
        let mut networked = Program::new();
        networked.register_builtin("tcp_listen".to_string(), &registry);
        let tables = networked.runtime_tables(&declarations).unwrap();
        let names = networked.collect_resource_names();
        let listener = names.iter().position(|n| n == "TcpListener").unwrap();
        let socket = names.iter().position(|n| n == "TcpSocket").unwrap();
        let info = tables.streams.streams[listener].as_ref().unwrap();
        assert!(info.data_tuple.is_none());
        assert_eq!(info.resource_tuple.unwrap().1, socket);
        assert!(tables.changed.is_none());
    }

    #[test]
    fn runtime_tables_ids_are_stable_across_calls() {
        // Content-addressing: repeated derivation (per REPL merge) returns the same ids.
        let registry = registry();
        let declarations = registry.runtime_declarations().clone();
        let mut program = Program::new();
        program.register_builtin("track".to_string(), &registry);
        let first = program.runtime_tables(&declarations).unwrap();
        let second = program.runtime_tables(&declarations).unwrap();
        assert_eq!(first, second);
    }
}
