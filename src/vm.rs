use crate::builtins::BUILTIN_REGISTRY;
use crate::bytecode::{Constant, Instruction, TypeId};
use crate::program::Program;
use crate::types::{TupleTypeInfo, Type, TypeLookup};
use std::collections::HashMap;
use std::rc::Rc;

/// Maximum binary size in bytes (16MB)
pub const MAX_BINARY_SIZE: usize = 16 * 1024 * 1024;

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryRef {
    /// Reference to a binary stored in the constants table
    Constant(usize),
    /// Reference-counted heap-allocated binary
    Heap(Rc<Vec<u8>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Binary(BinaryRef),
    Tuple(TypeId, Vec<Value>),
    Function(usize, Vec<Value>),
    Builtin(String),
}

impl Value {
    /// Create a NIL tuple value
    pub fn nil() -> Self {
        Value::Tuple(TypeId::NIL, vec![])
    }

    /// Create an OK tuple value
    pub fn ok() -> Self {
        Value::Tuple(TypeId::OK, vec![])
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "integer",
            Value::Binary(_) => "binary",
            Value::Tuple(_, _) => "tuple",
            Value::Function(_, _) => "function",
            Value::Builtin(_) => "builtin",
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    // Stack operation errors
    StackUnderflow,

    // Function and call errors
    CallInvalid,
    FunctionUndefined(usize),
    BuiltinUndefined(usize),
    FrameUnderflow,

    // Variable and constant access errors
    VariableUndefined(String),
    ConstantUndefined(usize),

    // Data access errors
    FieldAccessInvalid(usize),

    // Type system errors
    TypeMismatch { expected: String, found: String },
    ArityMismatch { expected: usize, found: usize },
    InvalidArgument(String),

    // Tuple and structure errors
    TupleEmpty,

    // Scope management errors
    ScopeCountInvalid { expected: usize, found: usize },
    ScopeUnderflow,
}

#[derive(Debug, Clone)]
pub struct Frame {
    instructions: Vec<Instruction>,
    locals_base: usize,
    captures_count: usize,
    counter: usize,
}

impl Frame {
    pub fn new(instructions: Vec<Instruction>, locals_base: usize, captures_count: usize) -> Self {
        Self {
            instructions,
            locals_base,
            captures_count,
            counter: 0,
        }
    }
}

#[derive(Debug)]
pub struct VM<'a> {
    program: &'a Program,
    stack: Vec<Value>,
    locals: Vec<Value>,
    frames: Vec<Frame>,
}

impl<'a> TypeLookup for VM<'a> {
    fn lookup_type(&self, type_id: &TypeId) -> Option<&TupleTypeInfo> {
        self.program.lookup_type(type_id)
    }
}

impl<'a> VM<'a> {
    pub fn new(program: &'a Program) -> Self {
        Self {
            program,
            stack: Vec::new(),
            locals: Vec::new(),
            frames: Vec::new(),
        }
    }

    /// Create a new heap-allocated binary
    pub fn create_heap_binary(&mut self, bytes: Vec<u8>) -> Result<BinaryRef, Error> {
        if bytes.len() > MAX_BINARY_SIZE {
            return Err(Error::InvalidArgument(format!(
                "Binary size {} exceeds maximum {}",
                bytes.len(),
                MAX_BINARY_SIZE
            )));
        }
        Ok(BinaryRef::Heap(Rc::new(bytes)))
    }

    /// Clone a binary reference (cheap operation)
    pub fn clone_binary(&self, binary_ref: &BinaryRef) -> BinaryRef {
        binary_ref.clone()
    }

    pub fn execute_instructions(
        &mut self,
        instructions: Vec<Instruction>,
        parameter: Option<Value>,
    ) -> Result<Option<Value>, Error> {
        if instructions.is_empty() {
            return Ok(None);
        }

        // Push parameter onto stack if provided
        if let Some(param) = parameter {
            self.stack.push(param);
        }

        let frame = Frame::new(instructions.clone(), 0, 0);
        self.frames.push(frame);

        let result = self.run();
        self.frames.pop();

        result
    }

    pub fn execute_function(&mut self, entry: usize) -> Result<Option<Value>, Error> {
        let function = self
            .program
            .get_function(entry)
            .ok_or(Error::FunctionUndefined(entry))?;
        let instructions = function.instructions.clone();

        self.stack.push(Value::nil());
        self.frames.push(Frame::new(instructions, 0, 0));
        self.run()
    }

    fn run(&mut self) -> Result<Option<Value>, Error> {
        while let Some(instruction) = self.get_instruction() {
            match instruction {
                Instruction::Constant(index) => self.handle_constant(index)?,
                Instruction::Pop => self.handle_pop()?,
                Instruction::Duplicate => self.handle_duplicate()?,
                Instruction::Copy(depth) => self.handle_copy(depth)?,
                Instruction::Swap => self.handle_swap()?,
                Instruction::Load(index) => self.handle_load(index)?,
                Instruction::Store(index) => self.handle_store(index)?,
                Instruction::Tuple(type_id) => self.handle_tuple(type_id)?,
                Instruction::Get(index) => self.handle_get(index)?,
                Instruction::IsInteger => self.handle_is_integer()?,
                Instruction::IsBinary => self.handle_is_binary()?,
                Instruction::IsTuple(type_id) => self.handle_is_tuple(type_id)?,
                Instruction::Jump(offset) => self.handle_jump(offset)?,
                Instruction::JumpIf(offset) => self.handle_jump_if(offset)?,
                Instruction::Call => self.handle_call()?,
                Instruction::TailCall(recurse) => self.handle_tail_call(recurse)?,
                Instruction::Return => self.handle_return()?,
                Instruction::Function(function_index) => self.handle_function(function_index)?,
                Instruction::Clear(count) => self.handle_clear(count)?,
                Instruction::Allocate(count) => self.handle_allocate(count)?,
                Instruction::Builtin(index) => self.handle_builtin(index)?,
                Instruction::Equal(count) => self.handle_equal(count)?,
                Instruction::Not => self.handle_not()?,
            }
            if !matches!(instruction, Instruction::Call | Instruction::TailCall(_)) {
                if let Some(frame) = self.frames.last_mut() {
                    frame.counter += 1;
                }
            }
        }
        Ok(self.stack.pop())
    }

    fn get_instruction(&mut self) -> Option<Instruction> {
        if let Some(frame) = self.frames.last() {
            frame.instructions.get(frame.counter).cloned()
        } else {
            None
        }
    }

    fn handle_constant(&mut self, index: usize) -> Result<(), Error> {
        let constant = self
            .program
            .get_constant(index)
            .ok_or(Error::ConstantUndefined(index))?;
        let value = match constant {
            Constant::Integer(integer) => Value::Integer(*integer),
            Constant::Binary(_) => Value::Binary(BinaryRef::Constant(index)),
        };
        self.stack.push(value);
        Ok(())
    }

    fn handle_pop(&mut self) -> Result<(), Error> {
        self.stack.pop().ok_or(Error::StackUnderflow)?;
        Ok(())
    }

    fn handle_duplicate(&mut self) -> Result<(), Error> {
        let value = self.stack.last().ok_or(Error::StackUnderflow)?;
        self.stack.push(value.clone());
        Ok(())
    }

    fn handle_copy(&mut self, depth: usize) -> Result<(), Error> {
        if depth >= self.stack.len() {
            return Err(Error::StackUnderflow);
        }
        let stack_index = self.stack.len() - 1 - depth;
        let value = self.stack[stack_index].clone();
        self.stack.push(value);
        Ok(())
    }

    fn handle_swap(&mut self) -> Result<(), Error> {
        let a = self.stack.pop().ok_or(Error::StackUnderflow)?;
        let b = self.stack.pop().ok_or(Error::StackUnderflow)?;
        self.stack.push(a);
        self.stack.push(b);
        Ok(())
    }

    fn handle_load(&mut self, index: usize) -> Result<(), Error> {
        let frame = self.frames.last().ok_or(Error::FrameUnderflow)?;
        let locals_index = frame.locals_base + index;
        let value = self
            .locals
            .get(locals_index)
            .ok_or(Error::VariableUndefined(format!("local {}", index)))?
            .clone();
        self.stack.push(value);
        Ok(())
    }

    fn handle_store(&mut self, index: usize) -> Result<(), Error> {
        let value = self.stack.pop().ok_or(Error::StackUnderflow)?;
        let frame = self.frames.last().ok_or(Error::FrameUnderflow)?;
        let locals_index = frame.locals_base + index;

        // Check that local was reserved
        if locals_index >= self.locals.len() {
            return Err(Error::VariableUndefined(format!(
                "local {} not allocated",
                index
            )));
        }

        self.locals[locals_index] = value;
        Ok(())
    }

    fn handle_tuple(&mut self, type_id: TypeId) -> Result<(), Error> {
        // Look up the type to get the field count
        let Some((_, fields)) = self.program.lookup_type(&type_id) else {
            return Err(Error::TypeMismatch {
                expected: "known tuple type".to_string(),
                found: format!("unknown TypeId({:?})", type_id),
            });
        };

        let size = fields.len();

        let mut values = Vec::new();
        for _ in 0..size {
            let value = self.stack.pop().ok_or(Error::StackUnderflow)?;
            values.push(value);
        }
        values.reverse();
        self.stack.push(Value::Tuple(type_id, values));
        Ok(())
    }

    fn handle_get(&mut self, index: usize) -> Result<(), Error> {
        let value = self.stack.pop().ok_or(Error::StackUnderflow)?;
        match value {
            Value::Tuple(_type_id, fields) => {
                fields.get(index).ok_or(Error::FieldAccessInvalid(index))?;
                self.stack.push(fields[index].clone());
                Ok(())
            }
            value => Err(Error::TypeMismatch {
                expected: "tuple".to_string(),
                found: value.type_name().to_string(),
            }),
        }
    }

    fn handle_is_integer(&mut self) -> Result<(), Error> {
        let value = self.stack.pop().ok_or(Error::StackUnderflow)?;
        let is_match = matches!(value, Value::Integer(_));
        self.stack.push(Value::Tuple(
            if is_match { TypeId::OK } else { TypeId::NIL },
            vec![],
        ));
        Ok(())
    }

    fn handle_is_binary(&mut self) -> Result<(), Error> {
        let value = self.stack.pop().ok_or(Error::StackUnderflow)?;
        let is_match = matches!(value, Value::Binary(_));
        self.stack.push(Value::Tuple(
            if is_match { TypeId::OK } else { TypeId::NIL },
            vec![],
        ));
        Ok(())
    }

    fn handle_is_tuple(&mut self, expected_type_id: TypeId) -> Result<(), Error> {
        let value = self.stack.pop().ok_or(Error::StackUnderflow)?;

        let is_match = if let Value::Tuple(actual_type_id, _) = &value {
            if actual_type_id == &expected_type_id {
                // Fast path: exact match
                true
            } else {
                // Use Type::is_compatible for structural checking
                let actual_type = Type::Tuple(*actual_type_id);
                let expected_type = Type::Tuple(expected_type_id);
                actual_type.is_compatible(&expected_type, self)
            }
        } else {
            false
        };

        self.stack.push(Value::Tuple(
            if is_match { TypeId::OK } else { TypeId::NIL },
            vec![],
        ));
        Ok(())
    }

    fn handle_jump(&mut self, offset: isize) -> Result<(), Error> {
        self.jump(offset);
        Ok(())
    }

    fn handle_jump_if(&mut self, offset: isize) -> Result<(), Error> {
        let value = self.stack.pop().ok_or(Error::StackUnderflow)?;
        // Jump if value is truthy (NOT NIL)
        if !matches!(value, Value::Tuple(TypeId::NIL, _)) {
            self.jump(offset);
        }
        Ok(())
    }

    fn handle_call(&mut self) -> Result<(), Error> {
        match self.stack.pop().ok_or(Error::StackUnderflow)? {
            Value::Function(function, captures) => {
                let func = self
                    .program
                    .get_function(function)
                    .ok_or(Error::FunctionUndefined(function))?;
                let instructions = func.instructions.clone();

                // Set up new frame's locals_base at current end of locals
                let locals_base = self.locals.len();

                // Extend locals with captures
                let captures_count = captures.len();
                self.locals.extend(captures);

                self.frames
                    .push(Frame::new(instructions, locals_base, captures_count));
                Ok(())
            }
            Value::Builtin(name) => {
                let argument = self.stack.pop().ok_or(Error::StackUnderflow)?;
                let builtin_fn =
                    BUILTIN_REGISTRY
                        .get_implementation(&name)
                        .ok_or(Error::InvalidArgument(format!(
                            "Unrecognised builtin: {}",
                            name
                        )))?;
                let result = builtin_fn(&argument, self.program)?;
                self.stack.push(result);
                // Unlike regular calls, builtins don't create a new frame
                // So we need to manually increment the counter
                if let Some(frame) = self.frames.last_mut() {
                    frame.counter += 1;
                }
                Ok(())
            }
            _ => Err(Error::CallInvalid),
        }
    }

    fn handle_tail_call(&mut self, recurse: bool) -> Result<(), Error> {
        if recurse {
            let argument = self.stack.pop().ok_or(Error::StackUnderflow)?;
            let frame = self.frames.last().ok_or(Error::FrameUnderflow)?;
            let locals_base = frame.locals_base;
            let captures_count = frame.captures_count;
            let instructions = frame.instructions.clone();

            // Clear current frame's locals, but keep captures
            self.locals.truncate(locals_base + captures_count);

            self.stack.push(argument);
            *self.frames.last_mut().unwrap() =
                Frame::new(instructions, locals_base, captures_count);
            Ok(())
        } else {
            match self.stack.pop().ok_or(Error::StackUnderflow)? {
                Value::Function(function, captures) => {
                    let argument = self.stack.pop().ok_or(Error::StackUnderflow)?;
                    let func = self
                        .program
                        .get_function(function)
                        .ok_or(Error::FunctionUndefined(function))?;
                    let instructions = func.instructions.clone();

                    let frame = self.frames.last().ok_or(Error::FrameUnderflow)?;
                    let locals_base = frame.locals_base;

                    // Clear current frame's locals
                    self.locals.truncate(locals_base);

                    // Extend with captures for new function
                    let captures_count = captures.len();
                    self.locals.extend(captures);

                    self.stack.push(argument);
                    *self.frames.last_mut().unwrap() =
                        Frame::new(instructions, locals_base, captures_count);
                    Ok(())
                }
                _ => Err(Error::CallInvalid),
            }
        }
    }

    fn handle_return(&mut self) -> Result<(), Error> {
        let frame = self.frames.pop().ok_or(Error::FrameUnderflow)?;
        // Clear frame's locals
        self.locals.truncate(frame.locals_base);
        Ok(())
    }

    fn handle_function(&mut self, function_index: usize) -> Result<(), Error> {
        let func = self
            .program
            .get_function(function_index)
            .ok_or(Error::FunctionUndefined(function_index))?;
        let capture_locals = func.captures.clone();

        let frame = self.frames.last().ok_or(Error::FrameUnderflow)?;
        let locals_base = frame.locals_base;

        // Collect captured values from current frame's locals
        let captures = capture_locals
            .iter()
            .map(|&index| {
                self.locals
                    .get(locals_base + index)
                    .cloned()
                    .ok_or(Error::VariableUndefined(format!("capture local {}", index)))
            })
            .collect::<Result<Vec<_>, _>>()?;

        self.stack.push(Value::Function(function_index, captures));
        Ok(())
    }

    fn handle_builtin(&mut self, index: usize) -> Result<(), Error> {
        let function_name = self
            .program
            .get_builtin(index)
            .ok_or(Error::BuiltinUndefined(index))?
            .clone();
        self.stack.push(Value::Builtin(function_name));
        Ok(())
    }

    fn handle_equal(&mut self, count: usize) -> Result<(), Error> {
        if count == 0 {
            return Err(Error::InvalidArgument(
                "Cannot compare zero values".to_string(),
            ));
        }

        // Pop the specified number of values from the stack
        let mut values = Vec::with_capacity(count);
        for _ in 0..count {
            values.push(self.stack.pop().ok_or(Error::StackUnderflow)?);
        }

        // Reverse to get them in the original order (since we popped in reverse)
        values.reverse();

        // Check if all elements are equal to the first element
        let first = &values[0];
        let all_equal = values.iter().all(|value| values_equal(first, value, self));

        let result = if all_equal {
            first.clone() // Return the first value if all are equal
        } else {
            Value::nil() // Return NIL if not all equal
        };

        self.stack.push(result);
        Ok(())
    }

    fn handle_not(&mut self) -> Result<(), Error> {
        let value = self.stack.pop().ok_or(Error::StackUnderflow)?;

        let result = match value {
            Value::Tuple(type_id, fields) => {
                // Check if it's NIL (empty tuple with TypeId::NIL)
                if type_id == TypeId::NIL && fields.is_empty() {
                    Value::ok() // Return Ok
                } else {
                    Value::nil() // Return NIL
                }
            }
            _ => Value::nil(), // Any non-tuple becomes NIL
        };

        self.stack.push(result);
        Ok(())
    }

    fn handle_clear(&mut self, count: usize) -> Result<(), Error> {
        if count > self.locals.len() {
            return Err(Error::StackUnderflow);
        }
        self.locals.truncate(self.locals.len() - count);
        Ok(())
    }

    fn handle_allocate(&mut self, count: usize) -> Result<(), Error> {
        self.locals
            .extend(std::iter::repeat(Value::nil()).take(count));
        Ok(())
    }

    fn jump(&mut self, offset: isize) {
        let frame = self.frames.last_mut().unwrap();
        frame.counter = frame.counter.wrapping_add_signed(offset);
    }

    pub fn get_stack(&self) -> Vec<Value> {
        self.stack.clone()
    }

    pub fn frame_count(&self) -> usize {
        self.frames.len()
    }

    pub fn cleanup_locals(&mut self, variables: &HashMap<String, usize>) -> HashMap<String, usize> {
        // Collect referenced indices in sorted order
        let mut referenced: Vec<(String, usize)> = variables
            .iter()
            .map(|(name, &index)| (name.clone(), index))
            .collect();
        referenced.sort_by_key(|(_, index)| *index);

        // Build new locals array with only referenced values and create remapping
        let mut new_locals = Vec::new();
        let mut new_variables = HashMap::new();

        for (name, old_index) in referenced {
            if old_index < self.locals.len() {
                new_variables.insert(name, new_locals.len());
                new_locals.push(self.locals[old_index].clone());
            }
        }

        self.locals = new_locals;
        new_variables
    }

    pub fn get_variables(
        &self,
        mapping: &HashMap<String, usize>,
    ) -> Result<HashMap<String, Value>, Error> {
        let mut variables = HashMap::new();
        for (name, &index) in mapping {
            if index >= self.locals.len() {
                return Err(Error::VariableUndefined(format!(
                    "local index {} out of bounds (locals.len = {})",
                    index,
                    self.locals.len()
                )));
            }
            variables.insert(name.clone(), self.locals[index].clone());
        }
        Ok(variables)
    }

    pub fn format_value(&self, value: &Value) -> String {
        crate::format::format_value(self.program, value)
    }

    pub fn format_type(&self, type_def: &Type) -> String {
        crate::format::format_type(self.program, type_def)
    }
}

/// Helper function to compare values for equality
fn values_equal(a: &Value, b: &Value, vm: &VM) -> bool {
    match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => a == b,
        (Value::Binary(a), Value::Binary(b)) => {
            match (
                vm.program.get_binary_bytes(a),
                vm.program.get_binary_bytes(b),
            ) {
                (Ok(bytes_a), Ok(bytes_b)) => bytes_a == bytes_b,
                _ => false, // If we can't access bytes, consider unequal
            }
        }
        (Value::Tuple(type_a, elems_a), Value::Tuple(type_b, elems_b)) => {
            type_a == type_b
                && elems_a.len() == elems_b.len()
                && elems_a
                    .iter()
                    .zip(elems_b.iter())
                    .all(|(x, y)| values_equal(x, y, vm))
        }
        (Value::Function(a, cap_a), Value::Function(b, cap_b)) => {
            a == b
                && cap_a.len() == cap_b.len()
                && cap_a
                    .iter()
                    .zip(cap_b.iter())
                    .all(|(x, y)| values_equal(x, y, vm))
        }
        _ => false,
    }
}
