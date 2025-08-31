use crate::bytecode::{Constant, Function, Instruction, TypeId};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Binary(usize),
    // TODO: support binaries on heap?
    Tuple(TypeId, Vec<Value>),
    Function {
        function: usize,
        captures: Vec<Value>,
    },
}

impl Value {
    fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "integer",
            Value::Binary(_) => "binary",
            Value::Tuple(_, _) => "tuple",
            Value::Function { .. } => "function",
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(i) => write!(f, "{}", i),
            Value::Binary(_) => write!(f, "<binary>"), // TODO: show actual binary content
            Value::Tuple(type_id, elements) => {
                if *type_id == TypeId::NIL {
                    write!(f, "[]")
                } else if *type_id == TypeId::OK {
                    write!(f, "Ok")
                } else if elements.is_empty() {
                    write!(f, "[]")
                } else {
                    write!(f, "[")?;
                    for (i, element) in elements.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", element)?;
                    }
                    write!(f, "]")
                }
            }
            Value::Function { .. } => write!(f, "<function>"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Scope {
    parameter: Value,
    variables: HashMap<String, Value>,
}

impl Scope {
    pub fn new(parameter: Value) -> Self {
        Self {
            variables: HashMap::new(),
            parameter,
        }
    }
}

#[derive(Debug)]
pub enum Error {
    StackUnderflow,
    InvalidCall,
    TypeMismatch { expected: String, found: String },
    UndefinedVariable(String),
    UndefinedConstant(usize),
    UndefinedFunction(usize),
    InvalidFieldAccess(usize),
    EmptyTuple,
}

#[derive(Debug, Clone)]
pub struct Frame {
    instructions: Vec<Instruction>,
    captures: HashMap<String, Value>,
    counter: usize,
}

impl Frame {
    pub fn new(instructions: Vec<Instruction>, captures: HashMap<String, Value>) -> Self {
        Self {
            instructions,
            captures,
            counter: 0,
        }
    }
}

#[derive(Clone)]
pub struct VM {
    constants: Vec<Constant>,
    functions: Vec<Function>,
    stack: Vec<Value>,
    scopes: Vec<Scope>,
    frames: Vec<Frame>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            functions: Vec::new(),
            stack: Vec::new(),
            scopes: vec![Scope::new(Value::Tuple(TypeId::NIL, vec![]))],
            frames: Vec::new(),
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

    pub fn register_function(&mut self, function: Function) -> usize {
        if let Some(index) = self.functions.iter().position(|f| f == &function) {
            index
        } else {
            self.functions.push(function);
            self.functions.len() - 1
        }
    }

    pub fn execute_instructions(
        &mut self,
        instructions: Vec<Instruction>,
        new_scope: bool,
    ) -> Result<Option<Value>, Error> {
        if new_scope {
            self.scopes
                .push(Scope::new(Value::Tuple(TypeId::NIL, vec![])));
        }
        self.frames.push(Frame::new(instructions, HashMap::new()));
        let result = self.run();
        self.frames.pop();
        if new_scope {
            self.scopes.pop();
        }
        result
    }

    pub fn execute_function(&mut self, entry: usize) -> Result<Option<Value>, Error> {
        let function = self
            .functions
            .get(entry)
            .ok_or(Error::UndefinedFunction(entry))?;
        let instructions = function.instructions.clone();
        self.scopes
            .push(Scope::new(Value::Tuple(TypeId::NIL, vec![])));
        self.frames.push(Frame::new(instructions, HashMap::new()));
        let result = self.run();
        self.frames.pop();
        self.scopes.pop();
        result
    }

    fn run(&mut self) -> Result<Option<Value>, Error> {
        while let Some(instruction) = self.get_instruction() {
            match instruction {
                Instruction::Constant(index) => self.handle_constant(index)?,
                Instruction::Pop => self.handle_pop()?,
                Instruction::Duplicate => self.handle_duplicate()?,
                Instruction::Swap => self.handle_swap()?,
                Instruction::Add(tuple_size) => self.handle_arithmetic(tuple_size, |a, b| a + b)?,
                Instruction::Subtract(tuple_size) => {
                    self.handle_arithmetic(tuple_size, |a, b| a - b)?
                }
                Instruction::Multiply(tuple_size) => {
                    self.handle_arithmetic(tuple_size, |a, b| a * b)?
                }
                Instruction::Divide(tuple_size) => {
                    self.handle_arithmetic(tuple_size, |a, b| a / b)?
                }
                Instruction::Modulo(tuple_size) => {
                    self.handle_arithmetic(tuple_size, |a, b| a % b)?
                }
                Instruction::Equal(tuple_size) => {
                    self.handle_comparison(tuple_size, |a, b| a == b)?
                }
                Instruction::NotEqual(tuple_size) => {
                    self.handle_comparison(tuple_size, |a, b| a != b)?
                }
                Instruction::Less(tuple_size) => {
                    self.handle_comparison(tuple_size, |a, b| a < b)?
                }
                Instruction::LessEqual(tuple_size) => {
                    self.handle_comparison(tuple_size, |a, b| a <= b)?
                }
                Instruction::Greater(tuple_size) => {
                    self.handle_comparison(tuple_size, |a, b| a > b)?
                }
                Instruction::GreaterEqual(tuple_size) => {
                    self.handle_comparison(tuple_size, |a, b| a >= b)?
                }
                Instruction::Load(ref name) => self.handle_load(name)?,
                Instruction::Store(ref name) => self.handle_store(name)?,
                Instruction::Tuple(type_id, size) => self.handle_tuple(type_id, size)?,
                Instruction::Get(index) => self.handle_get(index)?,
                Instruction::IsInteger => self.handle_is_integer()?,
                Instruction::IsBinary => self.handle_is_binary()?,
                Instruction::IsTuple(type_id) => self.handle_is_tuple(type_id)?,
                Instruction::Jump(offset) => self.handle_jump(offset)?,
                Instruction::JumpIfNil(offset) => self.handle_jump_if_nil(offset)?,
                Instruction::JumpIfNotNil(offset) => self.handle_jump_if_not_nil(offset)?,
                Instruction::Call => self.handle_call()?,
                Instruction::TailCall(recurse) => self.handle_tail_call(recurse)?,
                Instruction::Return => self.handle_return()?,
                Instruction::Parameter => self.handle_parameter()?,
                Instruction::Function(index) => self.handle_function(index)?,
                Instruction::Enter => self.handle_enter()?,
                Instruction::Exit => self.handle_exit()?,
            }
            if !matches!(instruction, Instruction::Call | Instruction::TailCall(_)) {
                self.frames.last_mut().unwrap().counter += 1;
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
            .constants
            .get(index)
            .ok_or(Error::UndefinedConstant(index))?;
        let value = match constant {
            Constant::Integer(integer) => Value::Integer(*integer),
            Constant::Binary(_) => Value::Binary(index),
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

    fn handle_swap(&mut self) -> Result<(), Error> {
        let a = self.stack.pop().ok_or(Error::StackUnderflow)?;
        let b = self.stack.pop().ok_or(Error::StackUnderflow)?;
        self.stack.push(a);
        self.stack.push(b);
        Ok(())
    }

    fn handle_arithmetic<F>(&mut self, tuple_size: usize, op: F) -> Result<(), Error>
    where
        F: Fn(i64, i64) -> i64,
    {
        if tuple_size == 0 {
            return Err(Error::EmptyTuple);
        }
        let mut values = vec![0; tuple_size];
        for i in 0..tuple_size {
            match self.stack.pop() {
                Some(Value::Integer(v)) => values[i] = v,
                Some(value) => {
                    return Err(Error::TypeMismatch {
                        expected: "integer".to_string(),
                        found: value.type_name().to_string(),
                    });
                }
                None => return Err(Error::StackUnderflow),
            }
        }
        let result = values.into_iter().reduce(op).unwrap();
        self.stack.push(Value::Integer(result));
        Ok(())
    }

    fn handle_comparison<F>(&mut self, tuple_size: usize, test: F) -> Result<(), Error>
    where
        F: Fn(i64, i64) -> bool,
    {
        if tuple_size == 0 {
            return Err(Error::EmptyTuple);
        }
        let mut values = vec![0; tuple_size];
        for i in (0..tuple_size).rev() {
            match self.stack.pop() {
                Some(Value::Integer(v)) => values[i] = v,
                Some(value) => {
                    return Err(Error::TypeMismatch {
                        expected: "integer".to_string(),
                        found: value.type_name().to_string(),
                    });
                }
                None => return Err(Error::StackUnderflow),
            }
        }
        let result = values.windows(2).all(|w| test(w[0], w[1]));
        self.stack.push(if result {
            Value::Tuple(TypeId::OK, vec![])
        } else {
            Value::Tuple(TypeId::NIL, vec![])
        });
        Ok(())
    }

    fn handle_load(&mut self, name: &str) -> Result<(), Error> {
        let value = self.get_variable(name)?;
        self.stack.push(value);
        Ok(())
    }

    fn handle_store(&mut self, name: &str) -> Result<(), Error> {
        let value = self.stack.pop().ok_or(Error::StackUnderflow)?;
        let scope = self.scopes.last_mut().unwrap();
        scope.variables.insert(name.to_string(), value);
        Ok(())
    }

    fn handle_tuple(&mut self, type_id: TypeId, size: usize) -> Result<(), Error> {
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
                fields.get(index).ok_or(Error::InvalidFieldAccess(index))?;
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
        let value = self.stack.last().ok_or(Error::StackUnderflow)?;
        let is_match = matches!(value, Value::Integer(_));
        self.stack.push(Value::Tuple(
            if is_match { TypeId::OK } else { TypeId::NIL },
            vec![],
        ));
        Ok(())
    }

    fn handle_is_binary(&mut self) -> Result<(), Error> {
        let value = self.stack.last().ok_or(Error::StackUnderflow)?;
        let is_match = matches!(value, Value::Binary(_));
        self.stack.push(Value::Tuple(
            if is_match { TypeId::OK } else { TypeId::NIL },
            vec![],
        ));
        Ok(())
    }

    fn handle_is_tuple(&mut self, expected_type_id: TypeId) -> Result<(), Error> {
        let value = self.stack.last().ok_or(Error::StackUnderflow)?;
        let is_match = matches!(value, Value::Tuple(type_id, _) if (type_id == &expected_type_id));
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

    fn handle_jump_if_nil(&mut self, offset: isize) -> Result<(), Error> {
        let value = self.stack.pop().ok_or(Error::StackUnderflow)?;
        if matches!(value, Value::Tuple(TypeId::NIL, _)) {
            self.jump(offset);
        }
        Ok(())
    }

    fn handle_jump_if_not_nil(&mut self, offset: isize) -> Result<(), Error> {
        let value = self.stack.pop().ok_or(Error::StackUnderflow)?;
        if !matches!(value, Value::Tuple(TypeId::NIL, _)) {
            self.jump(offset);
        }
        Ok(())
    }

    fn handle_call(&mut self) -> Result<(), Error> {
        match self.stack.pop() {
            Some(Value::Function { function, captures }) => {
                let argument = self.stack.pop().ok_or(Error::StackUnderflow)?;
                self.scopes.push(Scope::new(argument));
                let func = self
                    .functions
                    .get(function)
                    .ok_or(Error::UndefinedFunction(function))?;
                let instructions = func.instructions.clone();
                let capture_map = func
                    .captures
                    .iter()
                    .zip(captures.iter())
                    .map(|(name, value)| (name.clone(), value.clone()))
                    .collect::<HashMap<String, Value>>();
                self.frames.push(Frame::new(instructions, capture_map));
                Ok(())
            }
            Some(_) => Err(Error::InvalidCall),
            None => Err(Error::StackUnderflow),
        }
    }

    fn handle_tail_call(&mut self, recurse: bool) -> Result<(), Error> {
        if recurse {
            let argument = self.stack.pop().ok_or(Error::StackUnderflow)?;
            let frame = self.frames.last_mut().unwrap();
            frame.counter = 0;
            *self.scopes.last_mut().unwrap() = Scope::new(argument);
            Ok(())
        } else {
            match self.stack.pop() {
                Some(Value::Function { function, captures }) => {
                    let argument = self.stack.pop().ok_or(Error::StackUnderflow)?;
                    let func = self
                        .functions
                        .get(function)
                        .ok_or(Error::UndefinedFunction(function))?;
                    let instructions = func.instructions.clone();
                    let capture_map = func
                        .captures
                        .iter()
                        .zip(captures.iter())
                        .map(|(name, value)| (name.clone(), value.clone()))
                        .collect::<HashMap<String, Value>>();
                    *self.frames.last_mut().unwrap() = Frame::new(instructions, capture_map);
                    *self.scopes.last_mut().unwrap() = Scope::new(argument);
                    Ok(())
                }
                Some(_) => Err(Error::InvalidCall),
                None => Err(Error::StackUnderflow),
            }
        }
    }

    fn handle_return(&mut self) -> Result<(), Error> {
        self.frames.pop();
        Ok(())
    }

    fn handle_parameter(&mut self) -> Result<(), Error> {
        let scope = self.scopes.last().unwrap();
        self.stack.push(scope.parameter.clone());
        Ok(())
    }

    fn handle_function(&mut self, index: usize) -> Result<(), Error> {
        let captures = self
            .functions
            .get(index)
            .ok_or(Error::UndefinedFunction(index))?
            .captures
            .iter()
            .map(|n| self.get_variable(n))
            .collect::<Result<Vec<_>, _>>()?;
        self.stack.push(Value::Function {
            function: index,
            captures: captures,
        });
        Ok(())
    }

    fn handle_enter(&mut self) -> Result<(), Error> {
        let parameter = self.stack.pop().ok_or(Error::StackUnderflow)?;
        self.scopes.push(Scope {
            parameter,
            variables: HashMap::new(),
        });
        Ok(())
    }

    fn handle_exit(&mut self) -> Result<(), Error> {
        // TODO: keep track of number of scopes for each frame, and prevent exiting above frame?
        self.scopes.pop();
        Ok(())
    }

    fn get_variable(&self, name: &str) -> Result<Value, Error> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.variables.get(name))
            .or_else(|| self.frames.last()?.captures.get(name))
            .cloned()
            .ok_or_else(|| Error::UndefinedVariable(name.to_string()))
    }

    fn jump(&mut self, offset: isize) {
        let frame = self.frames.last_mut().unwrap();
        frame.counter = frame.counter.wrapping_add_signed(offset);
    }

    pub fn list_variables(&self) -> Vec<(String, Value)> {
        let mut variables = Vec::new();

        // Collect from all scopes (inner scopes override outer scopes)
        for scope in &self.scopes {
            for (name, value) in &scope.variables {
                variables.push((name.clone(), value.clone()));
            }
        }

        // Also include frame captures if we're in a function call
        if let Some(frame) = self.frames.last() {
            for (name, value) in &frame.captures {
                variables.push((name.clone(), value.clone()));
            }
        }

        variables.sort_by(|a, b| a.0.cmp(&b.0));
        variables
    }
}
