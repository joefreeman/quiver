use crate::executor::Executor;
use crate::types::{BuiltinInfo, NIL, OK, TupleTypeInfo, Type, TypeLookup};
use crate::value::{Binary, Value};
use serde::{Deserialize, Serialize};

// Re-export bytecode types
pub use crate::bytecode::{Bytecode, Constant, Function, Instruction};

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
        };

        // Register built-in tuple types
        let nil_tuple_id = program.register_tuple(None, vec![]);
        assert_eq!(nil_tuple_id, NIL);

        let ok_tuple_id = program.register_tuple(Some("Ok".to_string()), vec![]);
        assert_eq!(ok_tuple_id, OK);

        program
    }

    pub fn register_constant(&mut self, constant: Constant) -> usize {
        if let Some(index) = self.constants.iter().position(|c| c == &constant) {
            index
        } else {
            self.constants.push(constant);
            self.constants.len() - 1
        }
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
        if let Some(index) = self.functions.iter().position(|f| f == &function) {
            index
        } else {
            self.functions.push(function);
            self.functions.len() - 1
        }
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
        // Check if type already exists
        for (index, existing_type) in self.tuples.iter().enumerate() {
            if existing_type.name == name && existing_type.fields == fields {
                return index;
            }
        }

        let tuple_id = self.tuples.len();
        self.tuples.push(TupleTypeInfo { name, fields });
        tuple_id
    }

    /// Register a type for use with IsType instruction
    pub fn register_type(&mut self, typ: Type) -> usize {
        // Check if type already exists
        if let Some(index) = self.types.iter().position(|t| t == &typ) {
            return index;
        }

        let type_id = self.types.len();
        self.types.push(typ);
        type_id
    }

    /// Intern an annotation key name, returning its key id.
    pub fn register_annotation_key(&mut self, name: &str) -> usize {
        if let Some(index) = self.annotation_keys.iter().position(|k| k == name) {
            return index;
        }
        let key_id = self.annotation_keys.len();
        self.annotation_keys.push(name.to_string());
        key_id
    }

    pub fn get_annotation_keys(&self) -> &Vec<String> {
        &self.annotation_keys
    }

    /// Intern a field name for `GetNamed`, returning its field-name id.
    pub fn register_field_name(&mut self, name: &str) -> usize {
        if let Some(index) = self.field_names.iter().position(|n| n == name) {
            return index;
        }
        let name_id = self.field_names.len();
        self.field_names.push(name.to_string());
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

    /// The crash-delivery table (both build modes): the key and tuple/type ids the
    /// executor needs to build `:crash` / `:timeout` stamped nils.
    /// Registers the shapes on first call — everything dedups
    /// by content, so repeated calls (e.g. per REPL merge) return stable ids. Not
    /// memoised for the same reason: registration is a handful of table lookups.
    pub fn crash_table(&mut self) -> crate::bytecode::CrashTable {
        let crash_key = self.register_annotation_key("crash");
        let timeout_key = self.register_annotation_key("timeout");
        let binary_type = self.register_type(Type::Binary);
        let str_tuple = self.register_tuple(Some("Str".to_string()), vec![(None, binary_type)]);
        let str_type = self.register_type(Type::Tuple(str_tuple));
        // The capability-less process type `(@)` — the pid field grants identity only.
        let pid_type = self.register_type(Type::Process {
            send: None,
            receive: None,
            state: None,
        });
        let crash_fields = vec![
            (Some("pid".to_string()), pid_type),
            (Some("message".to_string()), str_type),
        ];
        let error_tuple = self.register_tuple(Some("Error".to_string()), crash_fields.clone());
        let panic_tuple = self.register_tuple(Some("Panic".to_string()), crash_fields);
        let killed_tuple = self.register_tuple(Some("Killed".to_string()), vec![]);
        // Give each shape a type-table presence: checked retrievals (`x:('t)crash`)
        // enumerate compatible concrete types from the types table.
        let member_types: Vec<usize> = [error_tuple, panic_tuple, killed_tuple]
            .into_iter()
            .map(|tuple_id| self.register_type(Type::Tuple(tuple_id)))
            .collect();
        self.register_type(Type::Union(member_types));
        // The reactive `Changed` wakeup. Content-addressed, so it
        // shares the id of `std/proc.qv`'s `'changed = Changed`; given a type-table
        // presence so `'%proc.changed` resolves and pattern-matching a delivered value
        // works.
        let changed_tuple = self.register_tuple(Some("Changed".to_string()), vec![]);
        self.register_type(Type::Tuple(changed_tuple));
        crate::bytecode::CrashTable {
            crash_key,
            timeout_key,
            error_tuple,
            panic_tuple,
            killed_tuple,
            str_tuple,
            changed_tuple,
        }
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
            instrs.push(Instruction::Annotate(*key));
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
                vec![Instruction::Constant(const_idx)]
            }
            Value::BigInt(n) => {
                let const_idx = self.register_constant(Constant::Integer((**n).clone()));
                vec![Instruction::Constant(const_idx)]
            }
            Value::Binary(binary) => {
                match binary {
                    Binary::Constant(const_idx) => {
                        // Already a constant, just reference it
                        vec![Instruction::Constant(*const_idx)]
                    }
                    Binary::Heap(heap_idx) => {
                        // Get bytes from heap and create a new constant
                        let binary_data = executor
                            .get_heap_binary(*heap_idx)
                            .expect("Heap binary index should be valid");
                        let bytes = binary_data.to_vec();
                        let const_idx = self.register_constant(Constant::Binary(bytes));
                        vec![Instruction::Constant(const_idx)]
                    }
                }
            }
            Value::Tuple(tuple_id, elements) => {
                let mut instrs = Vec::new();
                for elem in elements.iter() {
                    instrs.extend(self.value_to_instructions(elem, executor));
                }
                instrs.push(Instruction::Tuple(*tuple_id));
                self.annotations_to_instructions(&mut instrs, elements, executor);
                instrs
            }
            Value::Function(function, captures) => {
                let func_index = if !captures.is_empty() {
                    self.inject_function_captures(*function, captures.to_vec(), executor)
                } else {
                    *function
                };
                let mut instrs = vec![Instruction::Function(func_index)];
                self.annotations_to_instructions(&mut instrs, captures, executor);
                instrs
            }
            Value::Builtin(builtin_id, payload) => {
                let mut instrs = vec![Instruction::Builtin(*builtin_id)];
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
