use crate::types::{BuiltinInfo, NIL, OK, TupleTypeInfo, Type, TypeLookup};
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
    ///
    /// Functions and constants — the reclaimable tables — are keyed by a 128-bit content
    /// digest rather than by the content itself: the key survives `reclaim_code` dropping
    /// an entry's weight, which is what lets an identical re-registration *revive* the
    /// original slot instead of minting a new one (id-stable revival). The digest is also
    /// far cheaper to hold than a cloned key. On a digest hit the content is verified
    /// against the table when the entry is live; a stubbed entry's content is gone, so
    /// there the digest alone is trusted — the 2×64-bit SipHash keys are random per
    /// program instance, making an accidental (or engineered) collision negligible.
    #[serde(skip)]
    constant_index: std::collections::HashMap<u128, Vec<usize>>,
    #[serde(skip)]
    function_index: std::collections::HashMap<u128, Vec<usize>>,
    #[serde(skip)]
    digest_keys: (
        std::collections::hash_map::RandomState,
        std::collections::hash_map::RandomState,
    ),
    /// Slots whose weight `reclaim_code` dropped. Env-side state (a compiler-side
    /// program is never reclaimed); intentionally not serialized.
    #[serde(skip)]
    stubbed_functions: std::collections::HashSet<usize>,
    #[serde(skip)]
    stubbed_constants: std::collections::HashSet<usize>,
    /// Stubs revived in place since the last `take_revived` — the environment ships
    /// their content to workers whose tables only ever receive appends.
    #[serde(skip)]
    revived_functions: Vec<usize>,
    #[serde(skip)]
    revived_constants: Vec<usize>,
    #[serde(skip)]
    type_index: std::collections::HashMap<Type, usize>,
    #[serde(skip)]
    tuple_index: std::collections::HashMap<TupleTypeInfo, usize>,
    #[serde(skip)]
    annotation_key_index: std::collections::HashMap<String, usize>,
    #[serde(skip)]
    field_name_index: std::collections::HashMap<String, usize>,
}

/// Why a value has no constant form. Reported in the caller's own vocabulary — the compiler
/// turns these into its own errors, naming the site.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InternError {
    /// A value carrying identity (a ref, a pid, a resource): `Value::type_name` of the
    /// offender.
    Identity(&'static str),
    /// A builtin id with no entry in this program's table.
    BuiltinUndefined(usize),
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
            debug: None,
            function_dedup_floor: 0,
            constant_index: std::collections::HashMap::new(),
            function_index: std::collections::HashMap::new(),
            digest_keys: Default::default(),
            stubbed_functions: std::collections::HashSet::new(),
            stubbed_constants: std::collections::HashSet::new(),
            revived_functions: Vec::new(),
            revived_constants: Vec::new(),
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

    /// A 128-bit content digest under this program's two per-instance SipHash keys.
    fn digest<T: std::hash::Hash>(&self, value: &T) -> u128 {
        use std::hash::BuildHasher;
        let first = self.digest_keys.0.hash_one(value);
        let second = self.digest_keys.1.hash_one(value);
        ((first as u128) << 64) | second as u128
    }

    fn ensure_constant_index(&mut self) {
        if self.constant_index.is_empty() && !self.constants.is_empty() {
            for index in 0..self.constants.len() {
                let digest = self.digest(&self.constants[index]);
                self.constant_index.entry(digest).or_default().push(index);
            }
        }
    }

    pub fn register_constant(&mut self, constant: Constant) -> usize {
        self.ensure_constant_index();
        let digest = self.digest(&constant);
        if let Some(indices) = self.constant_index.get(&digest) {
            // A live entry with equal content wins (the digest is verified); failing
            // that, a stubbed slot with this digest is the same content reclaimed —
            // revive it in place, keeping the id stable.
            for &index in indices {
                if !self.stubbed_constants.contains(&index) && self.constants[index] == constant {
                    return index;
                }
            }
            for &index in indices {
                if self.stubbed_constants.remove(&index) {
                    self.constants[index] = constant;
                    self.revived_constants.push(index);
                    return index;
                }
            }
        }
        let index = self.constants.len();
        self.constants.push(constant);
        self.constant_index.entry(digest).or_default().push(index);
        index
    }

    /// Intern a compile-time value into the constants table, answering its index — the
    /// inverse of the executor's materialisation. Fails for a value carrying identity (a ref,
    /// a pid, a resource), which has no constant form; the caller reports that in its own
    /// vocabulary.
    ///
    /// Bottom-up, which is what makes [`Self::register_constant`]'s content interning exact:
    /// a child is registered before the parent naming it, so structurally identical subtrees
    /// reach identical indices and their parents then digest identically and collapse. That
    /// is also why there is no pointer-identity memo — sharing is re-established here by
    /// content, not carried over from the source value, so a value that lost its `Rc` sharing
    /// on the way through an artifact interns to the same graph as one that never did.
    ///
    /// **Iterative**, like every other walk over value structure: a compile-time value is
    /// normally shallow, but nothing enforces that, and the failure mode is an uncatchable
    /// abort.
    pub fn intern_value<E: crate::effects::Effect>(
        &mut self,
        value: &crate::value::Value,
        registry: &crate::builtins::BuiltinRegistry<E>,
    ) -> Result<usize, InternError> {
        // `(value, its children, how many of them are interned)`. A node is revisited once
        // per child and then assembled, at which point its children's indices are the tail
        // of `done`. Children are computed on the way down, once per node.
        let mut stack: Vec<(&crate::value::Value, Vec<&crate::value::Value>, usize)> =
            vec![(value, Self::constant_children(value), 0)];
        let mut done: Vec<usize> = Vec::new();

        while let Some((value, children, visited)) = stack.pop() {
            if let Some(child) = children.get(visited).copied() {
                stack.push((value, children, visited + 1));
                stack.push((child, Self::constant_children(child), 0));
                continue;
            }
            let child_indices = done.split_off(done.len() - children.len());
            done.push(self.assemble_constant(value, child_indices, registry)?);
        }
        Ok(done.pop().expect("the root assembles last"))
    }

    /// The sub-values a constant form must carry: a payload's elements, then its annotation
    /// values (the order [`crate::value::Payload::all_values`] yields, which is the order
    /// [`Self::assemble_constant`] reads them back in).
    fn constant_children(value: &crate::value::Value) -> Vec<&crate::value::Value> {
        use crate::value::Value;
        match value {
            Value::Tuple(_, payload) | Value::Function(_, payload) => {
                payload.all_values().collect()
            }
            Value::Builtin(_, Some(payload)) => payload.all_values().collect(),
            _ => Vec::new(),
        }
    }

    /// Register one node, given its already-interned children. Annotations wrap the carrier
    /// in a separate `Annotated` node, so the bare carrier stays a slot other uses can share.
    fn assemble_constant<E: crate::effects::Effect>(
        &mut self,
        value: &crate::value::Value,
        children: Vec<usize>,
        registry: &crate::builtins::BuiltinRegistry<E>,
    ) -> Result<usize, InternError> {
        use crate::value::{Binary, Value};

        let (base, annotations) = match value {
            Value::Int(int) => (
                self.register_constant(Constant::Integer((*int).into())),
                None,
            ),
            Value::BigInt(int) => (
                self.register_constant(Constant::Integer((**int).clone())),
                None,
            ),
            // Already a constant; nothing to register.
            Value::Binary(Binary::Constant(index)) => (*index, None),
            Value::Binary(Binary::Data(data)) => (
                self.register_constant(Constant::Binary(data.to_vec())),
                None,
            ),
            Value::Tuple(id, payload) => (
                self.register_constant(Constant::Tuple {
                    id: *id,
                    fields: children[..payload.len()].to_vec(),
                }),
                Some(payload.as_ref()),
            ),
            Value::Function(id, payload) => (
                self.register_constant(Constant::Function {
                    id: *id,
                    captures: children[..payload.len()].to_vec(),
                }),
                Some(payload.as_ref()),
            ),
            Value::Builtin(id, payload) => {
                // A cached instantiated builtin resolves to *this* program's entry for that
                // instantiation; registration is idempotent, so an id that is already the
                // right entry answers itself.
                let info = self
                    .builtins
                    .get(*id)
                    .ok_or(InternError::BuiltinUndefined(*id))?;
                let name = info.name.clone();
                let type_argument = payload
                    .as_deref()
                    .and_then(crate::value::Payload::type_argument);
                let id = self.register_builtin_instantiated(name, type_argument, registry);
                (
                    self.register_constant(Constant::Builtin { id }),
                    payload.as_deref(),
                )
            }
            Value::Reference(_) | Value::Process(..) | Value::Resource(..) => {
                return Err(InternError::Identity(value.type_name()));
            }
        };

        let Some(payload) = annotations.filter(|payload| !payload.annotations().is_empty()) else {
            return Ok(base);
        };
        // The annotation values are the children after the elements, in the same order.
        let mut entries: Vec<(usize, usize)> = payload
            .annotations()
            .iter()
            .map(|(key, _)| *key)
            .zip(children[payload.len()..].iter().copied())
            .collect();
        entries.sort_by_key(|(key, _)| *key);
        Ok(self.register_constant(Constant::Annotated {
            value: base,
            entries,
        }))
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

    fn ensure_function_index(&mut self) {
        if self.function_index.is_empty() && !self.functions.is_empty() {
            for index in 0..self.functions.len() {
                let digest = self.digest(&self.functions[index]);
                self.function_index.entry(digest).or_default().push(index);
            }
        }
    }

    /// Register a function and return its index.
    /// Deduplicates based on full equality (instructions, captures, type_id).
    pub fn register_function(&mut self, function: Function) -> usize {
        // Structural interning never reaches below the dedup floor: while a module
        // compiles (or links), its functions must not collapse onto other modules'
        // structurally identical ones — function identity is attributed per module,
        // and cross-module collapse would make that attribution depend on session
        // history. Collapsing across the whole program is the runtime merge's job.
        self.ensure_function_index();
        let digest = self.digest(&function);
        if let Some(indices) = self.function_index.get(&digest) {
            for &index in indices {
                if index >= self.function_dedup_floor
                    && !self.stubbed_functions.contains(&index)
                    && self.functions[index] == function
                {
                    return index;
                }
            }
            // A stubbed slot with this digest is the same content reclaimed — revive
            // it in place, keeping the id stable (see the index fields' doc).
            for &index in indices {
                if index >= self.function_dedup_floor && self.stubbed_functions.remove(&index) {
                    self.functions[index] = function;
                    self.revived_functions.push(index);
                    return index;
                }
            }
        }
        let index = self.functions.len();
        self.functions.push(function);
        self.function_index.entry(digest).or_default().push(index);
        index
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
    /// Still recorded in the digest index, so later `register_function` calls dedup
    /// onto pushed entries exactly as the old whole-table scan did.
    pub fn push_function(&mut self, function: Function) -> usize {
        self.ensure_function_index();
        let digest = self.digest(&function);
        let index = self.functions.len();
        self.functions.push(function);
        self.function_index.entry(digest).or_default().push(index);
        index
    }

    pub fn get_functions(&self) -> &Vec<Function> {
        &self.functions
    }

    /// Reclaim dead code: keep identity, drop weight. A stubbed function keeps its
    /// `type_id` (live pids carry root-function indices used for process type tests)
    /// while its body becomes a single `Reclaimed` trap — executing one is a liveness
    /// bug and aborts the process loudly. A stubbed constant keeps its slot with the
    /// cheapest same-variant payload. The digest-index entries survive, so an identical
    /// re-registration revives the slot in place. Callers guarantee the dead sets are
    /// unreferenced; already-stubbed ids are skipped.
    pub fn reclaim_code(&mut self, dead_functions: &[usize], dead_constants: &[usize]) {
        // The maps must exist before content disappears — a lazy rebuild afterwards
        // would digest stub payloads instead of the original content.
        self.ensure_function_index();
        self.ensure_constant_index();
        for &index in dead_functions {
            if !self.stubbed_functions.insert(index) {
                continue;
            }
            let function = &mut self.functions[index];
            function.instructions = vec![Instruction::reclaimed()];
            function.captures = 0;
        }
        for &index in dead_constants {
            if !self.stubbed_constants.insert(index) {
                continue;
            }
            // Composites carry almost no weight — a `Vec` of indices — but they are stubbed
            // all the same, because the marker is what `register_constant` consults: an
            // unstubbed slot reads as live content, so an identical re-registration would
            // hand back a composite whose *children* had been reclaimed underneath it.
            self.constants[index] = match &self.constants[index] {
                Constant::Integer(_) => Constant::Integer(0.into()),
                Constant::Binary(_) => Constant::Binary(Vec::new()),
                Constant::Tuple { id, .. } => Constant::Tuple {
                    id: *id,
                    fields: Vec::new(),
                },
                Constant::Function { id, .. } => Constant::Function {
                    id: *id,
                    captures: Vec::new(),
                },
                Constant::Builtin { id } => Constant::Builtin { id: *id },
                Constant::Annotated { value, .. } => Constant::Annotated {
                    value: *value,
                    entries: Vec::new(),
                },
            };
        }
    }

    /// The ids `register_*` revived since the last call — content the environment must
    /// re-ship to workers whose tables only ever receive appends.
    pub fn take_revived(&mut self) -> (Vec<usize>, Vec<usize>) {
        (
            std::mem::take(&mut self.revived_functions),
            std::mem::take(&mut self.revived_constants),
        )
    }

    /// Whether a function slot is currently stubbed (a test/metric hook).
    pub fn function_stubbed(&self, index: usize) -> bool {
        self.stubbed_functions.contains(&index)
    }

    /// Currently-stubbed slot counts, `(functions, constants)` (a test/metric hook).
    pub fn stubbed_counts(&self) -> (usize, usize) {
        (self.stubbed_functions.len(), self.stubbed_constants.len())
    }

    pub fn get_function(&self, index: usize) -> Option<&Function> {
        self.functions.get(index)
    }

    pub fn register_builtin<E: crate::effects::Effect>(
        &mut self,
        name: String,
        registry: &crate::builtins::BuiltinRegistry<E>,
    ) -> usize {
        self.register_builtin_instantiated(name, None, registry)
    }

    /// Register a builtin, optionally at an explicit type argument. Each distinct
    /// instantiation of a type-consuming builtin is its own entry, so that the id alone
    /// tells the runtime which one a `Builtin` instruction means.
    pub fn register_builtin_instantiated<E: crate::effects::Effect>(
        &mut self,
        name: String,
        type_argument: Option<usize>,
        registry: &crate::builtins::BuiltinRegistry<E>,
    ) -> usize {
        // Check if this instantiation already exists
        if let Some(index) = self
            .builtins
            .iter()
            .position(|b| b.name == name && b.type_argument == type_argument)
        {
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
                type_argument,
            }
        } else {
            // Builtin not found in registry - this shouldn't happen in well-formed programs
            // Create a placeholder with bottom types (never type)
            let never_id = self.register_type(Type::Union(vec![]));
            BuiltinInfo {
                name,
                param_type: never_id,
                result_type: never_id,
                type_argument,
            }
        };

        self.builtins.push(builtin_info);
        self.builtins.len() - 1
    }

    /// Register a builtin with pre-resolved type information.
    /// Used when loading bytecode that already has resolved builtin types.
    pub fn register_builtin_info(&mut self, info: BuiltinInfo) -> usize {
        // Check if this instantiation already exists
        if let Some(index) = self
            .builtins
            .iter()
            .position(|b| b.name == info.name && b.type_argument == info.type_argument)
        {
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

    /// `type_id` with its parts replaced by `parts`, given in `Type::parts` order, or
    /// `type_id` itself when none changed. The rebuild is structural: a union is not
    /// re-flattened, nor a row re-normalised, since a walk rewriting parts keeps the shape.
    pub fn with_parts(&mut self, type_id: usize, parts: &[usize]) -> usize {
        let typ = self.types[type_id].clone();
        if typ.parts(&*self) == parts {
            return type_id;
        }
        let mut parts = parts.iter().copied();
        let mut next = || parts.next().expect("a part for every part of the type");
        let rebuilt = match typ {
            Type::Union(members) => Type::Union(members.iter().map(|_| next()).collect()),
            Type::Tuple(tuple_id) => {
                let info = self.tuples[tuple_id].clone();
                let fields = info
                    .fields
                    .into_iter()
                    .map(|(label, _)| (label, next()))
                    .collect();
                Type::Tuple(self.register_tuple(info.name, fields))
            }
            Type::Partial { name, fields } => Type::Partial {
                name,
                fields: fields
                    .into_iter()
                    .map(|(label, _)| (label, next()))
                    .collect(),
            },
            Type::Callable {
                states, omittable, ..
            } => Type::Callable {
                parameter: next(),
                result: next(),
                receive: next(),
                states: states.map(|_| next()),
                omittable,
            },
            Type::Process {
                send,
                receive,
                state,
            } => Type::Process {
                send: send.map(|_| next()),
                receive: receive.map(|_| next()),
                state: state.map(|_| next()),
            },
            Type::Annotated { exact, entries, .. } => Type::Annotated {
                base: next(),
                exact,
                entries: entries.into_iter().map(|(key, _)| (key, next())).collect(),
            },
            Type::Cycle(_)
            | Type::Variable(_)
            | Type::Integer
            | Type::Binary
            | Type::Reference
            | Type::Resource(_)
            | Type::Top => unreachable!("a type without parts is unchanged"),
        };
        assert!(parts.next().is_none(), "more parts than the type has");
        self.register_type(rebuilt)
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

        let error = match &declarations.error {
            Some(decl) => {
                let error_key = self.register_annotation_key(&decl.error_key);
                let str_tuple = self.vocabulary_tuple(&decl.str)?;
                let io_error_tuple = self.vocabulary_tuple(&decl.io_error)?;
                let kind_tuples = decl
                    .kinds
                    .iter()
                    .map(|kind| self.vocabulary_tuple(kind))
                    .collect::<Result<Vec<_>, _>>()?;
                // Type-table presence, so a checked retrieval (`x:('%io.error)error`) can
                // enumerate the payload — and so a match on a single kind resolves.
                self.register_type(Type::Tuple(io_error_tuple));
                let kind_types: Vec<usize> = kind_tuples
                    .iter()
                    .map(|tuple_id| self.register_type(Type::Tuple(*tuple_id)))
                    .collect();
                self.register_type(Type::Union(kind_types));
                Some(crate::bytecode::ErrorTable {
                    error_key,
                    io_error_tuple,
                    kind_tuples,
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
            error,
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
