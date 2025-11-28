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
}

impl TypeLookup for Program {
    fn lookup_type(&self, type_id: usize) -> Option<&Type> {
        self.types.get(type_id)
    }

    fn lookup_tuple(&self, tuple_id: usize) -> Option<&TupleTypeInfo> {
        self.tuples.get(tuple_id)
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

    /// Get the type ID for the NEVER type (empty union / bottom type).
    /// Registers it if not already present.
    pub fn never(&mut self) -> usize {
        self.register_type(Type::Union(vec![]))
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
        }
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

    /// Convert a runtime value to instructions that reconstruct it.
    /// This is used for serializing values (like captures) back into bytecode.
    fn value_to_instructions<E: crate::effects::Effect>(
        &mut self,
        value: &Value,
        executor: &Executor<E>,
    ) -> Vec<Instruction> {
        match value {
            Value::Integer(n) => {
                let const_idx = self.register_constant(Constant::Integer(*n));
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
                for elem in elements {
                    instrs.extend(self.value_to_instructions(elem, executor));
                }
                instrs.push(Instruction::Tuple(*tuple_id));
                instrs
            }
            Value::Function(function, captures) => {
                let func_index = if !captures.is_empty() {
                    self.inject_function_captures(*function, captures.clone(), executor)
                } else {
                    *function
                };
                vec![Instruction::Function(func_index)]
            }
            Value::Builtin(builtin_id) => {
                // Value::Builtin now contains the builtin_id directly
                vec![Instruction::Builtin(*builtin_id)]
            }
            Value::Process(_, _) => {
                panic!("Cannot convert pid to instructions")
            }
            Value::Resource(..) => {
                panic!("Cannot convert resource to instructions")
            }
        }
    }
}
