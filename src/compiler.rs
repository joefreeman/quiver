
use crate::ast::*;
use crate::bytecode::{Constant, Function, Instruction, TypeId};
use crate::vm::VM;
use crate::modules::{ModuleLoader, ModuleError};
use std::collections::HashMap;
use std::path::Path;

pub struct Compiler {
    type_registry: TypeRegistry,
    next_type_id: usize,
}

impl Compiler {
    pub fn new() -> Self {
        let mut compiler = Self {
            type_registry: TypeRegistry::new(),
            next_type_id: 2, // Reserve 0 and 1 for NIL and OK
        };
        
        // Register built-in types
        compiler.type_registry.register_builtin(TypeId::NIL, "[]".to_string());
        compiler.type_registry.register_builtin(TypeId::OK, "Ok".to_string());
        
        compiler
    }
    
    pub fn compile(
        &mut self,
        program: &Program,
        vm: &mut VM,
        module_loader: Box<dyn ModuleLoader>,
        current_path: Option<&Path>,
    ) -> Result<Vec<Instruction>, Error> {
        let mut context = CompilerContext::new(vm, module_loader, current_path);
        
        // Compile all statements
        for statement in &program.statements {
            self.compile_statement(statement, &mut context)?;
        }
        
        Ok(context.instructions)
    }
    
    fn compile_statement(
        &mut self,
        statement: &Statement,
        context: &mut CompilerContext,
    ) -> Result<(), Error> {
        match statement {
            Statement::TypeAlias { name, type_def } => {
                let type_id = self.register_type(type_def)?;
                context.symbol_table.define_type(name.clone(), type_id);
                Ok(())
            }
            Statement::TypeImport { pattern, module_path } => {
                self.compile_type_import(pattern, module_path, context)
            }
            Statement::Expression(expr) => {
                self.compile_expression(expr, context)?;
                // Don't pop the result - it might be the final value
                Ok(())
            }
        }
    }
    
    fn register_type(&mut self, type_def: &Type) -> Result<TypeId, Error> {
        match type_def {
            Type::Primitive(PrimitiveType::Int) => Ok(self.get_or_create_type_id("int")),
            Type::Primitive(PrimitiveType::Bin) => Ok(self.get_or_create_type_id("bin")),
            Type::Tuple(tuple_type) => {
                let type_name = self.format_tuple_type(tuple_type);
                Ok(self.get_or_create_type_id(&type_name))
            }
            Type::Function(_) => Ok(self.get_or_create_type_id("function")),
            Type::Union(_) => Ok(self.get_or_create_type_id("union")),
            Type::Identifier(name) => Ok(self.get_or_create_type_id(name)),
        }
    }
    
    fn format_tuple_type(&self, tuple_type: &TupleType) -> String {
        let name = tuple_type.name.as_deref().unwrap_or("");
        let fields: Vec<String> = tuple_type
            .fields
            .iter()
            .map(|f| {
                let field_name = f.name.as_deref().unwrap_or("");
                if field_name.is_empty() {
                    self.format_type_name(&f.type_def)
                } else {
                    format!("{}:{}", field_name, self.format_type_name(&f.type_def))
                }
            })
            .collect();
        
        if name.is_empty() {
            format!("[{}]", fields.join(","))
        } else {
            format!("{}[{}]", name, fields.join(","))
        }
    }
    
    fn format_type_name(&self, type_def: &Type) -> String {
        match type_def {
            Type::Primitive(PrimitiveType::Int) => "int".to_string(),
            Type::Primitive(PrimitiveType::Bin) => "bin".to_string(),
            Type::Tuple(t) => self.format_tuple_type(t),
            Type::Function(_) => "function".to_string(),
            Type::Union(_) => "union".to_string(),
            Type::Identifier(name) => name.clone(),
        }
    }
    
    fn get_or_create_type_id(&mut self, type_name: &str) -> TypeId {
        if let Some(type_id) = self.type_registry.get_type_id(type_name) {
            type_id
        } else {
            let type_id = TypeId(self.next_type_id);
            self.next_type_id += 1;
            self.type_registry.register_type(type_id, type_name.to_string());
            type_id
        }
    }
    
    fn compile_type_import(
        &mut self,
        _pattern: &TypeImportPattern,
        _module_path: &str,
        _context: &mut CompilerContext,
    ) -> Result<(), Error> {
        // TODO: Implement type imports
        Err(Error::NotImplemented("Type imports not yet implemented".to_string()))
    }
    
    fn compile_expression(
        &mut self,
        expression: &Expression,
        context: &mut CompilerContext,
    ) -> Result<(), Error> {
        if expression.branches.is_empty() {
            context.instructions.push(Instruction::Constant(
                context.vm.register_constant(Constant::Integer(0))
            ));
            return Ok(());
        }
        
        // For now, compile first branch only
        // TODO: Implement full alternative operator logic
        let branch = &expression.branches[0];
        
        self.compile_sequence(&branch.condition, context)?;
        
        if let Some(consequence) = &branch.consequence {
            // If we have a consequence, we need conditional logic
            context.instructions.push(Instruction::JumpIfNil(0)); // Placeholder
            let jump_pos = context.instructions.len() - 1;
            
            // Pop the condition result and compile consequence
            context.instructions.push(Instruction::Pop);
            self.compile_sequence(consequence, context)?;
            
            // Update jump offset
            let offset = (context.instructions.len() - jump_pos - 1) as isize;
            context.instructions[jump_pos] = Instruction::JumpIfNil(offset);
        }
        
        Ok(())
    }
    
    fn compile_sequence(
        &mut self,
        sequence: &Sequence,
        context: &mut CompilerContext,
    ) -> Result<(), Error> {
        for (i, term) in sequence.terms.iter().enumerate() {
            self.compile_term(term, context)?;
            
            // For comma operator: if any term is nil, skip remaining terms
            if i < sequence.terms.len() - 1 {
                context.instructions.push(Instruction::Duplicate);
                context.instructions.push(Instruction::JumpIfNil(0)); // Placeholder
                let jump_pos = context.instructions.len() - 1;
                
                // Calculate jump offset to end of sequence
                let remaining_terms = sequence.terms.len() - i - 1;
                // This is an approximation - we'll need better jump calculation
                let offset = remaining_terms as isize * 5; // Rough estimate
                context.instructions[jump_pos] = Instruction::JumpIfNil(offset);
            }
        }
        
        Ok(())
    }
    
    fn compile_term(
        &mut self,
        term: &Term,
        context: &mut CompilerContext,
    ) -> Result<(), Error> {
        match term {
            Term::Assignment { pattern, value } => {
                self.compile_chain(value, context)?;
                self.compile_pattern_assignment(pattern, context)?;
                Ok(())
            }
            Term::Chain(chain) => {
                self.compile_chain(chain, context)
            }
        }
    }
    
    fn compile_chain(
        &mut self,
        chain: &Chain,
        context: &mut CompilerContext,
    ) -> Result<(), Error> {
        // Compile the base value
        self.compile_value(&chain.value, context)?;
        
        // Apply operations in sequence
        for operation in &chain.operations {
            self.compile_operation(operation, context)?;
        }
        
        Ok(())
    }
    
    fn compile_value(
        &mut self,
        value: &Value,
        context: &mut CompilerContext,
    ) -> Result<(), Error> {
        match value {
            Value::Literal(literal) => self.compile_literal(literal, context),
            Value::Identifier(name) => {
                context.instructions.push(Instruction::Load(name.clone()));
                Ok(())
            }
            Value::Parameter(param) => {
                match param {
                    Parameter::Self_ => {
                        context.instructions.push(Instruction::Parameter);
                    }
                    Parameter::Indexed(index) => {
                        context.instructions.push(Instruction::Parameter);
                        context.instructions.push(Instruction::Get(*index));
                    }
                }
                Ok(())
            }
            Value::TupleConstruction(tuple) => self.compile_tuple_construction(tuple, context),
            Value::FunctionDefinition(func) => self.compile_function_definition(func, context),
            Value::Block(block) => self.compile_block(block, context),
            Value::MemberAccess(access) => self.compile_member_access(access, context),
            Value::Import(path) => self.compile_import(path, context),
            _ => Err(Error::NotImplemented(format!("Value type {:?} not implemented", value))),
        }
    }
    
    fn compile_literal(
        &mut self,
        literal: &Literal,
        context: &mut CompilerContext,
    ) -> Result<(), Error> {
        match literal {
            Literal::Integer(value) => {
                let constant_id = context.vm.register_constant(Constant::Integer(*value));
                context.instructions.push(Instruction::Constant(constant_id));
                Ok(())
            }
            Literal::Binary(bytes) => {
                let constant_id = context.vm.register_constant(Constant::Binary(bytes.clone()));
                context.instructions.push(Instruction::Constant(constant_id));
                Ok(())
            }
            Literal::String(_) => {
                // Strings are syntactic sugar for binaries
                Err(Error::NotImplemented("String literals not yet implemented".to_string()))
            }
        }
    }
    
    fn compile_tuple_construction(
        &mut self,
        tuple: &TupleConstruction,
        context: &mut CompilerContext,
    ) -> Result<(), Error> {
        // Compile field values
        for field in &tuple.fields {
            match &field.value {
                TupleFieldValue::Chain(chain) => {
                    self.compile_chain(chain, context)?;
                }
                TupleFieldValue::Ripple => {
                    // Ripple uses the previous value on stack
                    context.instructions.push(Instruction::Duplicate);
                }
            }
        }
        
        // Create tuple
        let type_id = if let Some(name) = &tuple.name {
            self.get_or_create_type_id(name)
        } else {
            self.get_or_create_type_id("[]")
        };
        
        context.instructions.push(Instruction::Tuple(type_id, tuple.fields.len()));
        Ok(())
    }
    
    fn compile_function_definition(
        &mut self,
        func_def: &FunctionDefinition,
        context: &mut CompilerContext,
    ) -> Result<(), Error> {
        // Compile function body in a separate step
        let mut func_instructions = Vec::new();
        let func_captures = Vec::new();
        
        // Compile function body directly into instructions vector
        func_instructions.push(Instruction::Enter);
        let mut func_context = CompilerContext {
            vm: context.vm,
            module_loader: context.module_loader.clone_box(),
            current_path: context.current_path,
            symbol_table: SymbolTable::new(),
            instructions: Vec::new(),
            captures: Vec::new(),
        };
        
        self.compile_expression(&func_def.body.expression, &mut func_context)?;
        func_instructions.extend(func_context.instructions);
        func_instructions.push(Instruction::Exit);
        func_instructions.push(Instruction::Return);
        
        // Register function with VM
        let function = Function {
            captures: func_captures,
            instructions: func_instructions,
        };
        let func_id = context.vm.register_function(function);
        
        // Push function reference
        context.instructions.push(Instruction::Function(func_id));
        Ok(())
    }
    
    fn compile_block(
        &mut self,
        block: &Block,
        context: &mut CompilerContext,
    ) -> Result<(), Error> {
        context.instructions.push(Instruction::Enter);
        self.compile_expression(&block.expression, context)?;
        context.instructions.push(Instruction::Exit);
        Ok(())
    }
    
    fn compile_member_access(
        &mut self,
        access: &MemberAccess,
        context: &mut CompilerContext,
    ) -> Result<(), Error> {
        // Load the object
        context.instructions.push(Instruction::Load(access.object.clone()));
        
        // Apply access path
        for path_element in &access.path {
            match path_element {
                AccessPath::Field(_name) => {
                    // For named field access, we need type information
                    // This is a simplification - real implementation would need field index lookup
                    return Err(Error::NotImplemented("Named field access not yet implemented".to_string()));
                }
                AccessPath::Index(index) => {
                    context.instructions.push(Instruction::Get(*index));
                }
            }
        }
        
        Ok(())
    }
    
    fn compile_operation(
        &mut self,
        operation: &Operation,
        context: &mut CompilerContext,
    ) -> Result<(), Error> {
        match operation {
            Operation::Operator(op) => self.compile_operator(op, context),
            Operation::Identifier(name) => {
                // Function application
                context.instructions.push(Instruction::Load(name.clone()));
                context.instructions.push(Instruction::Swap);
                context.instructions.push(Instruction::Call);
                Ok(())
            }
            Operation::PositionalAccess(index) => {
                context.instructions.push(Instruction::Get(*index));
                Ok(())
            }
            Operation::TailCall(name) => {
                context.instructions.push(Instruction::Load(name.clone()));
                context.instructions.push(Instruction::Swap);
                context.instructions.push(Instruction::TailCall(name == "&"));
                Ok(())
            }
            _ => Err(Error::NotImplemented(format!("Operation {:?} not implemented", operation))),
        }
    }
    
    fn compile_operator(
        &mut self,
        operator: &Operator,
        context: &mut CompilerContext,
    ) -> Result<(), Error> {
        // The operand should be a tuple on the stack
        // We need to determine tuple size - for now assume it's known
        let tuple_size = 2; // Default assumption
        
        let instruction = match operator {
            Operator::Add => Instruction::Add(tuple_size),
            Operator::Subtract => Instruction::Subtract(tuple_size),
            Operator::Multiply => Instruction::Multiply(tuple_size),
            Operator::Divide => Instruction::Divide(tuple_size),
            Operator::Modulo => Instruction::Modulo(tuple_size),
            Operator::Equal => Instruction::Equal(tuple_size),
            Operator::NotEqual => Instruction::NotEqual(tuple_size),
            Operator::LessThan => Instruction::Less(tuple_size),
            Operator::LessThanOrEqual => Instruction::LessEqual(tuple_size),
            Operator::GreaterThan => Instruction::Greater(tuple_size),
            Operator::GreaterThanOrEqual => Instruction::GreaterEqual(tuple_size),
        };
        
        context.instructions.push(instruction);
        Ok(())
    }
    
    fn compile_pattern_assignment(
        &mut self,
        pattern: &Pattern,
        context: &mut CompilerContext,
    ) -> Result<(), Error> {
        match pattern {
            Pattern::Identifier(name) => {
                context.instructions.push(Instruction::Store(name.clone()));
                Ok(())
            }
            Pattern::TuplePattern(tuple_pattern) => {
                self.compile_tuple_pattern_assignment(tuple_pattern, context)
            }
            _ => Err(Error::NotImplemented(format!("Pattern {:?} not implemented", pattern))),
        }
    }
    
    fn compile_tuple_pattern_assignment(
        &mut self,
        tuple_pattern: &TuplePattern,
        context: &mut CompilerContext,
    ) -> Result<(), Error> {
        // For each field in the pattern, extract and store
        for (i, field) in tuple_pattern.fields.iter().enumerate() {
            context.instructions.push(Instruction::Duplicate);
            context.instructions.push(Instruction::Get(i));
            self.compile_pattern_assignment(&field.pattern, context)?;
        }
        
        // Pop the original tuple
        context.instructions.push(Instruction::Pop);
        Ok(())
    }
    
    fn compile_import(
        &mut self,
        path: &str,
        context: &mut CompilerContext,
    ) -> Result<(), Error> {
        // Load module source
        let source = context.module_loader.load(path, context.current_path)
            .map_err(Error::ModuleError)?;
        
        // Parse and compile module
        let program = crate::parser::parse(&source)
            .map_err(Error::ParseError)?;
        
        // Compile module
        let mut module_compiler = Compiler::new();
        let module_path = context.module_loader.resolve(path, context.current_path)
            .map_err(Error::ModuleError)?;
        
        let instructions = module_compiler.compile(
            &program,
            context.vm,
            context.module_loader.clone_box(),
            Some(&module_path),
        )?;
        
        // Execute module to get result
        let result = context.vm.execute_instructions(instructions)
            .map_err(Error::RuntimeError)?;
        
        // Push result as constant
        if let Some(value) = result {
            let constant_id = match value {
                crate::vm::Value::Integer(i) => context.vm.register_constant(Constant::Integer(i)),
                crate::vm::Value::Binary(_b) => context.vm.register_constant(Constant::Binary(vec![])), // TODO: handle binaries
                _ => return Err(Error::NotImplemented("Complex module results not implemented".to_string())),
            };
            context.instructions.push(Instruction::Constant(constant_id));
        } else {
            // Push nil
            context.instructions.push(Instruction::Tuple(TypeId::NIL, 0));
        }
        
        Ok(())
    }
    
    pub fn list_type_aliases(&self) -> Vec<(String, TypeId)> {
        self.type_registry.list_types()
    }
}

#[derive(Debug)]
struct TypeRegistry {
    type_to_id: HashMap<String, TypeId>,
    id_to_type: HashMap<TypeId, String>,
}

impl TypeRegistry {
    fn new() -> Self {
        Self {
            type_to_id: HashMap::new(),
            id_to_type: HashMap::new(),
        }
    }
    
    fn register_builtin(&mut self, type_id: TypeId, type_name: String) {
        self.type_to_id.insert(type_name.clone(), type_id);
        self.id_to_type.insert(type_id, type_name);
    }
    
    fn register_type(&mut self, type_id: TypeId, type_name: String) {
        self.type_to_id.insert(type_name.clone(), type_id);
        self.id_to_type.insert(type_id, type_name);
    }
    
    fn get_type_id(&self, type_name: &str) -> Option<TypeId> {
        self.type_to_id.get(type_name).copied()
    }
    
    fn list_types(&self) -> Vec<(String, TypeId)> {
        let mut types: Vec<_> = self.type_to_id.iter()
            .map(|(name, id)| (name.clone(), *id))
            .collect();
        types.sort_by(|a, b| a.0.cmp(&b.0));
        types
    }
}

#[derive(Debug)]
struct SymbolTable {
    variables: HashMap<String, TypeId>,
    types: HashMap<String, TypeId>,
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
            types: HashMap::new(),
        }
    }
    
    fn define_variable(&mut self, name: String, type_id: TypeId) {
        self.variables.insert(name, type_id);
    }
    
    fn define_type(&mut self, name: String, type_id: TypeId) {
        self.types.insert(name, type_id);
    }
    
    fn get_variable_type(&self, name: &str) -> Option<TypeId> {
        self.variables.get(name).copied()
    }
}

struct CompilerContext<'a> {
    vm: &'a mut VM,
    module_loader: Box<dyn ModuleLoader>,
    current_path: Option<&'a Path>,
    symbol_table: SymbolTable,
    instructions: Vec<Instruction>,
    captures: Vec<String>,
}

impl<'a> CompilerContext<'a> {
    fn new(
        vm: &'a mut VM,
        module_loader: Box<dyn ModuleLoader>,
        current_path: Option<&'a Path>,
    ) -> Self {
        Self {
            vm,
            module_loader,
            current_path,
            symbol_table: SymbolTable::new(),
            instructions: Vec::new(),
            captures: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    NotImplemented(String),
    ParseError(crate::parser::Error),
    ModuleError(ModuleError),
    RuntimeError(crate::vm::Error),
    TypeError(String),
    UndefinedVariable(String),
    UndefinedType(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::NotImplemented(msg) => write!(f, "Not implemented: {}", msg),
            Error::ParseError(err) => write!(f, "Parse error: {}", err),
            Error::ModuleError(err) => write!(f, "Module error: {:?}", err),
            Error::RuntimeError(err) => write!(f, "Runtime error: {:?}", err),
            Error::TypeError(msg) => write!(f, "Type error: {}", msg),
            Error::UndefinedVariable(var) => write!(f, "Undefined variable: {}", var),
            Error::UndefinedType(typ) => write!(f, "Undefined type: {}", typ),
        }
    }
}

impl std::error::Error for Error {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::modules::InMemoryModuleLoader;

    #[test]
    fn test_compile_integer_literal() {
        let mut compiler = Compiler::new();
        let mut vm = VM::new();
        let module_loader = Box::new(InMemoryModuleLoader::new());
        
        // Parse a simple integer literal
        let program = crate::parser::parse("42").expect("Failed to parse");
        
        let instructions = compiler
            .compile(&program, &mut vm, module_loader, None)
            .expect("Failed to compile");
        
        assert!(!instructions.is_empty());
        // Should have at least a Constant instruction
        assert!(matches!(instructions[0], Instruction::Constant(_)));
    }

    #[test]
    fn test_compile_simple_assignment() {
        let mut compiler = Compiler::new();
        let mut vm = VM::new();
        let module_loader = Box::new(InMemoryModuleLoader::new());
        
        // Parse a simple assignment
        let program = crate::parser::parse("x = 42").expect("Failed to parse");
        
        let instructions = compiler
            .compile(&program, &mut vm, module_loader, None)
            .expect("Failed to compile");
        
        assert!(!instructions.is_empty());
        // Should have Constant and Store instructions
        assert!(instructions.iter().any(|i| matches!(i, Instruction::Constant(_))));
        assert!(instructions.iter().any(|i| matches!(i, Instruction::Store(_))));
    }

    #[test]
    fn test_parse_arithmetic() {
        // Test parsing separately first
        let result = crate::parser::parse("[3, 4] ~> +");
        match result {
            Ok(program) => {
                println!("Successfully parsed: {:#?}", program);
            }
            Err(e) => {
                println!("Failed to parse: {}", e);
                panic!("Parse failed");
            }
        }
    }

    #[test]
    fn test_compile_arithmetic() {
        let mut compiler = Compiler::new();
        let mut vm = VM::new();
        let module_loader = Box::new(InMemoryModuleLoader::new());
        
        // Parse simple arithmetic: [3, 4] ~> +
        let program = crate::parser::parse("[3, 4] ~> +").expect("Failed to parse");
        
        let instructions = compiler
            .compile(&program, &mut vm, module_loader, None)
            .expect("Failed to compile");
        
        assert!(!instructions.is_empty());
        // Should have constants for 3 and 4, tuple construction, and add operation
        assert!(instructions.iter().any(|i| matches!(i, Instruction::Constant(_))));
        assert!(instructions.iter().any(|i| matches!(i, Instruction::Tuple(_, _))));
        assert!(instructions.iter().any(|i| matches!(i, Instruction::Add(_))));
    }

    #[test]
    fn test_compile_function_definition() {
        let mut compiler = Compiler::new();
        let mut vm = VM::new();
        let module_loader = Box::new(InMemoryModuleLoader::new());
        
        // Parse a simple function: #int { $ }
        let program = crate::parser::parse("#int { $ }").expect("Failed to parse");
        
        let instructions = compiler
            .compile(&program, &mut vm, module_loader, None)
            .expect("Failed to compile");
        
        assert!(!instructions.is_empty());
        // Should have Function instruction
        assert!(instructions.iter().any(|i| matches!(i, Instruction::Function(_))));
    }
}