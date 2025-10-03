use crate::bytecode::{Bytecode, Constant, Function, Instruction, TypeId};
use crate::types::{TupleTypeInfo, Type, TypeLookup};
use crate::vm::{BinaryRef, Error, Value};
use std::collections::HashMap;

mod optimisation;

/// Program represents the compiled program data that is static during execution.
/// It contains constants, functions, builtins, and type information.
/// The compiler writes to this structure, and the VM reads from it.
#[derive(Debug, Clone)]
pub struct Program {
    constants: Vec<Constant>,
    functions: Vec<Function>,
    builtins: Vec<String>,
    types: HashMap<TypeId, TupleTypeInfo>,
    next_type_id: usize,
}

impl TypeLookup for Program {
    fn lookup_type(&self, type_id: &TypeId) -> Option<&TupleTypeInfo> {
        self.types.get(type_id)
    }
}

impl Program {
    pub fn new() -> Self {
        let mut program = Self {
            constants: Vec::new(),
            functions: Vec::new(),
            builtins: Vec::new(),
            types: HashMap::new(),
            next_type_id: 0,
        };

        // Register built-in types
        let nil_type_id = program.register_type(None, vec![]);
        assert_eq!(nil_type_id, TypeId::NIL);

        let ok_type_id = program.register_type(Some("Ok".to_string()), vec![]);
        assert_eq!(ok_type_id, TypeId::OK);

        program
    }

    pub fn from_bytecode(bytecode: Bytecode) -> Self {
        let next_type_id = bytecode.types.keys().map(|id| id.0).max().unwrap_or(0) + 1;

        Self {
            constants: bytecode.constants,
            functions: bytecode.functions,
            builtins: bytecode.builtins,
            types: bytecode.types,
            next_type_id,
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

    pub fn get_constant(&self, index: usize) -> Option<&Constant> {
        self.constants.get(index)
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

    pub fn inject_function_captures(&mut self, function_index: usize, captures: Vec<Value>) {
        let mut instructions = Vec::new();
        instructions.push(Instruction::Allocate(captures.len()));

        for (i, capture_value) in captures.iter().enumerate() {
            instructions.extend(self.value_to_instructions(capture_value));
            instructions.push(Instruction::Store(i));
        }

        let func = &self.functions[function_index];
        instructions.extend(func.instructions.clone());

        self.functions[function_index] = Function {
            instructions,
            function_type: func.function_type.clone(),
            captures: Vec::new(),
        };
    }

    fn value_to_instructions(&mut self, value: &Value) -> Vec<Instruction> {
        match value {
            Value::Integer(n) => {
                let const_idx = self.register_constant(Constant::Integer(*n));
                vec![Instruction::Constant(const_idx)]
            }
            Value::Binary(bin_ref) => {
                let bytes = Self::get_binary_bytes_static(bin_ref, &self.constants)
                    .expect("Binary should be valid during capture injection")
                    .to_vec();
                let const_idx = self.register_constant(Constant::Binary(bytes));
                vec![Instruction::Constant(const_idx)]
            }
            Value::Builtin(name) => {
                let builtin_idx = self.register_builtin(name.clone());
                vec![Instruction::Builtin(builtin_idx)]
            }
            Value::Function(function, captures) => {
                if !captures.is_empty() {
                    self.inject_function_captures(*function, captures.clone());
                }
                vec![Instruction::Function(*function)]
            }
            Value::Tuple(type_id, elements) => {
                let mut instrs = Vec::new();
                for elem in elements {
                    instrs.extend(self.value_to_instructions(elem));
                }
                instrs.push(Instruction::Tuple(*type_id));
                instrs
            }
        }
    }

    // Helper method for getting binary bytes without &self (for use in value_to_instructions)
    fn get_binary_bytes_static<'a>(
        binary_ref: &'a BinaryRef,
        constants: &'a [Constant],
    ) -> Result<&'a [u8], Error> {
        match binary_ref {
            BinaryRef::Constant(index) => match constants.get(*index) {
                Some(Constant::Binary(bytes)) => Ok(bytes),
                Some(_) => Err(Error::TypeMismatch {
                    expected: "binary constant".to_string(),
                    found: "non-binary constant".to_string(),
                }),
                None => Err(Error::ConstantUndefined(*index)),
            },
            BinaryRef::Heap(rc_bytes) => Ok(rc_bytes),
        }
    }

    pub fn register_builtin(&mut self, function: String) -> usize {
        if let Some(index) = self.builtins.iter().position(|b| b == &function) {
            index
        } else {
            self.builtins.push(function);
            self.builtins.len() - 1
        }
    }

    pub fn get_builtins(&self) -> &Vec<String> {
        &self.builtins
    }

    pub fn get_builtin(&self, index: usize) -> Option<&String> {
        self.builtins.get(index)
    }

    pub fn register_type(
        &mut self,
        name: Option<String>,
        fields: Vec<(Option<String>, Type)>,
    ) -> TypeId {
        // Check if type already exists
        for (&existing_id, existing_type) in &self.types {
            if existing_type.0 == name && existing_type.1 == fields {
                return existing_id;
            }
        }

        let type_id = TypeId(self.next_type_id);
        self.next_type_id += 1;

        self.types.insert(type_id, (name, fields));
        type_id
    }

    pub fn get_types(&self) -> HashMap<TypeId, TupleTypeInfo> {
        self.types.clone()
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
                types: self.types.clone(),
            };
        };

        optimisation::tree_shake(
            &self.functions,
            &self.constants,
            &self.types,
            &self.builtins,
            entry_fn,
        )
    }

    /// Get the actual bytes of a binary value
    pub fn get_binary_bytes<'a>(&'a self, binary_ref: &'a BinaryRef) -> Result<&'a [u8], Error> {
        match binary_ref {
            BinaryRef::Constant(index) => match self.constants.get(*index) {
                Some(Constant::Binary(bytes)) => Ok(bytes),
                Some(_) => Err(Error::TypeMismatch {
                    expected: "binary constant".to_string(),
                    found: "non-binary constant".to_string(),
                }),
                None => Err(Error::ConstantUndefined(*index)),
            },
            BinaryRef::Heap(rc_bytes) => Ok(rc_bytes),
        }
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}
