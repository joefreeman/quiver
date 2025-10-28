use crate::bytecode::{BuiltinInfo, Bytecode, Constant, Function, Instruction};
use crate::error::Error;
use crate::executor::Executor;
use crate::types::{NIL, OK, TupleLookup, TupleTypeInfo, Type};
use crate::value::{Binary, MAX_BINARY_SIZE, Value};
use serde::{Deserialize, Serialize};

mod optimisation;

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

impl TupleLookup for Program {
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

    pub fn with_binary_bytes<F, R>(&self, binary: &Binary, f: F) -> Result<R, Error>
    where
        F: FnOnce(&[u8]) -> Result<R, Error>,
    {
        match binary {
            Binary::Constant(index) => match self.constants.get(*index) {
                Some(Constant::Binary(bytes)) => f(bytes),
                Some(_) => Err(Error::TypeMismatch {
                    expected: "binary constant".to_string(),
                    found: "non-binary constant".to_string(),
                }),
                None => Err(Error::ConstantUndefined(*index)),
            },
            Binary::Heap(_) => Err(Error::InvalidArgument(
                "Heap binaries cannot be accessed from Program (no scheduler context)".to_string(),
            )),
        }
    }

    /// Convenience method that clones the binary data
    /// For cases where the closure API would be cumbersome (e.g., multiple binary accesses)
    pub fn get_binary_bytes(&self, binary: &Binary) -> Result<Vec<u8>, Error> {
        self.with_binary_bytes(binary, |bytes| Ok(bytes.to_vec()))
    }

    pub fn allocate_binary(&mut self, bytes: Vec<u8>) -> Result<Binary, Error> {
        if bytes.len() > MAX_BINARY_SIZE {
            return Err(Error::InvalidArgument(format!(
                "Binary size {} exceeds maximum {}",
                bytes.len(),
                MAX_BINARY_SIZE
            )));
        }
        let index = self.register_constant(Constant::Binary(bytes));
        Ok(Binary::Constant(index))
    }

    pub fn new() -> Self {
        let mut program = Self {
            constants: Vec::new(),
            functions: Vec::new(),
            builtins: Vec::new(),
            tuples: Vec::new(),
            types: Vec::new(),
        };

        // Register built-in types
        let nil_tuple_id = program.register_tuple(None, vec![], false);
        assert_eq!(nil_tuple_id, NIL);

        let ok_tuple_id = program.register_tuple(Some("Ok".to_string()), vec![], false);
        assert_eq!(ok_tuple_id, OK);

        program
    }

    /// Create a new Program seeded with existing types
    /// If types is empty or doesn't include NIL/OK, they will be added automatically
    pub fn with_types(tuples: Vec<TupleTypeInfo>) -> Self {
        // Ensure NIL and OK are at indices 0 and 1
        if tuples.is_empty() {
            // Start from scratch
            return Self::new();
        }

        // Validate that NIL and OK are at the expected indices if types are provided
        assert!(
            tuples.len() >= 2,
            "Types must include NIL and OK at indices 0 and 1"
        );
        assert_eq!(
            tuples[0].name, None,
            "Type at index 0 must be NIL (unnamed)"
        );
        assert_eq!(tuples[0].fields.len(), 0, "NIL type must have no fields");
        assert_eq!(
            tuples[1].name,
            Some("Ok".to_string()),
            "Type at index 1 must be OK"
        );
        assert_eq!(tuples[1].fields.len(), 0, "OK type must have no fields");

        Self {
            constants: Vec::new(),
            functions: Vec::new(),
            builtins: Vec::new(),
            tuples,
            types: Vec::new(),
        }
    }

    pub fn from_bytecode(bytecode: Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            functions: bytecode.functions,
            builtins: bytecode.builtins,
            tuples: bytecode.tuples,
            types: bytecode.types,
        }
    }

    pub fn register_constant(&mut self, constant: Constant) -> usize {
        if let Some(index) = self.constants.iter().position(|c| c == &constant) {
            index
        } else {
            self.constants.push(constant);
            self.constants.len() - 1
        }
    }

    /// Register a constant at a specific index, expanding the vector if necessary.
    /// Used for syncing constants from executor threads back to the main program.
    pub fn register_constant_at(&mut self, index: usize, constant: Constant) {
        if index >= self.constants.len() {
            self.constants.resize(index + 1, Constant::Integer(0));
        }
        self.constants[index] = constant;
    }

    pub fn get_constants(&self) -> &Vec<Constant> {
        &self.constants
    }

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

    pub fn register_builtin(&mut self, name: String) -> usize {
        // Check if builtin already exists
        if let Some(index) = self.builtins.iter().position(|b| b.name == name) {
            return index;
        }

        // Look up type specs from the builtin registry and resolve them
        let builtin_info = if let Some((param_spec, result_spec)) =
            crate::builtins::BUILTIN_REGISTRY.get_specs(&name)
        {
            BuiltinInfo {
                name: name.clone(),
                parameter_type: param_spec.resolve(self),
                result_type: result_spec.resolve(self),
            }
        } else {
            // Builtin not found in registry - this shouldn't happen in well-formed programs
            // Create a placeholder with bottom types
            BuiltinInfo {
                name,
                parameter_type: Type::Union(vec![]),
                result_type: Type::Union(vec![]),
            }
        };

        self.builtins.push(builtin_info);
        self.builtins.len() - 1
    }

    pub fn get_builtins(&self) -> &Vec<BuiltinInfo> {
        &self.builtins
    }

    pub fn register_tuple(
        &mut self,
        name: Option<String>,
        fields: Vec<(Option<String>, Type)>,
        is_partial: bool,
    ) -> usize {
        // Check if type already exists
        for (index, existing_type) in self.tuples.iter().enumerate() {
            if existing_type.name == name
                && existing_type.fields == fields
                && existing_type.is_partial == is_partial
            {
                return index;
            }
        }

        let tuple_id = self.tuples.len();
        self.tuples.push(TupleTypeInfo {
            name,
            fields,
            is_partial,
        });
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

    pub fn get_tuples(&self) -> &Vec<TupleTypeInfo> {
        &self.tuples
    }

    pub fn get_types(&self) -> &Vec<Type> {
        &self.types
    }

    /// Convert this program to bytecode format with an optional entry point
    /// Automatically performs tree shaking to remove unused functions, constants, and types
    pub fn to_bytecode(&self, entry: Option<usize>) -> Bytecode {
        // If there's no entry point, return as-is (nothing to tree shake)
        let Some(entry_fn) = entry else {
            return Bytecode {
                constants: self.constants.clone(),
                functions: self.functions.clone(),
                builtins: self.builtins.clone(),
                entry: None,
                tuples: self.tuples.clone(),
                types: self.types.clone(),
            };
        };

        optimisation::tree_shake(
            &self.functions,
            &self.constants,
            &self.tuples,
            &self.types,
            &self.builtins,
            entry_fn,
        )
    }

    /// Inject function captures into a function, returning a new function index.
    /// Creates a new function that allocates locals for the captures, converts each
    /// capture value to instructions, stores them in locals, then executes the original
    /// function's instructions.
    pub fn inject_function_captures(
        &mut self,
        function_index: usize,
        captures: Vec<Value>,
        executor: &Executor,
    ) -> usize {
        let mut instructions = Vec::new();
        instructions.push(Instruction::Allocate(captures.len()));

        for (i, capture_value) in captures.iter().enumerate() {
            instructions.extend(self.value_to_instructions(capture_value, executor));
            instructions.push(Instruction::Store(i));
        }

        let func = self
            .get_function(function_index)
            .expect("Function should exist during capture injection");
        instructions.extend(func.instructions.clone());
        let function_type = func.function_type.clone();

        self.register_function(Function {
            instructions,
            function_type,
            captures: Vec::new(),
        })
    }

    /// Convert a runtime value to instructions that reconstruct it.
    /// This is used for serializing values (like captures) back into bytecode.
    fn value_to_instructions(&mut self, value: &Value, executor: &Executor) -> Vec<Instruction> {
        match value {
            Value::Integer(n) => {
                let const_idx = self.register_constant(Constant::Integer(*n));
                vec![Instruction::Constant(const_idx)]
            }
            Value::Binary(bin_ref) => {
                let bytes = executor
                    .get_binary_bytes(bin_ref)
                    .expect("Binary should be valid during value serialization");
                let const_idx = self.register_constant(Constant::Binary(bytes));
                vec![Instruction::Constant(const_idx)]
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
            Value::Builtin(name) => {
                let builtin_idx = self.register_builtin(name.clone());
                vec![Instruction::Builtin(builtin_idx)]
            }
            Value::Process(_, _) => {
                panic!("Cannot convert pid to instructions")
            }
        }
    }
}
