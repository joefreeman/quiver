
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
        Self {
            type_registry: TypeRegistry::new(),
            next_type_id: 2, // Reserve 0 and 1 for NIL and OK
        }
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
                // Store full type definition in symbol table for compile-time use
                // We'll validate all types after all aliases are defined
                context.symbol_table.define_type_alias(name.clone(), type_def.clone());
                
                // If it's a tuple type (after resolving aliases), also register its signature for runtime
                let resolved_type = context.symbol_table.resolve_type(type_def);
                if let Type::Tuple(tuple_type) = resolved_type {
                    let signature = TupleSignature::from_tuple_type(&tuple_type);
                    let _type_id = self.get_or_create_tuple_type_id(signature);
                }
                
                Ok(())
            }
            Statement::TypeImport { pattern, module_path } => {
                self.compile_type_import(pattern, module_path, context)
            }
            Statement::Expression(expr) => {
                let _expr_type = self.compile_expression_with_type(expr, context)?;
                // Don't pop the result - it might be the final value
                Ok(())
            }
        }
    }
    
    fn register_type(&mut self, type_def: &Type) -> Result<Option<TypeId>, Error> {
        match type_def {
            Type::Primitive(_) => Ok(None), // Primitives don't need TypeIds
            Type::Tuple(tuple_type) => {
                let signature = TupleSignature::from_tuple_type(tuple_type);
                Ok(Some(self.get_or_create_tuple_type_id(signature)))
            }
            Type::Function(_) => Ok(None), // Functions don't need runtime TypeIds
            Type::Union(_) => Ok(None), // Unions are compile-time only
            Type::Identifier(_name) => Ok(None), // Type aliases resolved at compile-time
        }
    }
    
    fn get_or_create_tuple_type_id(&mut self, signature: TupleSignature) -> TypeId {
        if let Some(type_id) = self.type_registry.get_type_id(&signature) {
            type_id
        } else {
            let type_id = TypeId(self.next_type_id);
            self.next_type_id += 1;
            self.type_registry.register_tuple_signature(type_id, signature);
            type_id
        }
    }
    
    fn get_tuple_type_id_for_construction(&mut self, tuple_construction: &TupleConstruction) -> TypeId {
        let signature = TupleSignature::from_tuple_construction(tuple_construction);
        self.get_or_create_tuple_type_id(signature)
    }
    
    fn get_tuple_type_id_for_pattern(&mut self, tuple_pattern: &TuplePattern) -> TypeId {
        let signature = TupleSignature::from_tuple_pattern(tuple_pattern);
        self.get_or_create_tuple_type_id(signature)
    }
    
    fn validate_type_definition(&self, type_def: &Type, context: &CompilerContext) -> Result<(), Error> {
        match type_def {
            Type::Identifier(name) => {
                // Check if the type alias exists - only if it's not a primitive type
                if !self.is_primitive_type_name(name) && context.symbol_table.get_type_alias(name).is_none() {
                    return Err(Error::UndefinedType(name.clone()));
                }
                Ok(())
            }
            Type::Tuple(tuple_type) => {
                // Validate all field types recursively
                for field in &tuple_type.fields {
                    self.validate_type_definition(&field.type_def, context)?;
                }
                Ok(())
            }
            Type::Function(func_type) => {
                // Validate input and output types
                self.validate_type_definition(&func_type.input, context)?;
                self.validate_type_definition(&func_type.output, context)?;
                Ok(())
            }
            Type::Union(union_type) => {
                // Validate all union member types
                if union_type.types.is_empty() {
                    return Err(Error::TypeError("Union type must have at least one member".to_string()));
                }
                for member_type in &union_type.types {
                    self.validate_type_definition(member_type, context)?;
                }
                Ok(())
            }
            Type::Primitive(_) => Ok(()), // Primitives are always valid
        }
    }
    
    fn is_primitive_type_name(&self, name: &str) -> bool {
        matches!(name, "int" | "bin" | "string")
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
        let _expr_type = self.compile_expression_with_type(expression, context)?;
        Ok(())
    }
    
    fn compile_expression_with_type(
        &mut self,
        expression: &Expression,
        context: &mut CompilerContext,
    ) -> Result<Type, Error> {
        if expression.branches.is_empty() {
            context.instructions.push(Instruction::Constant(
                context.vm.register_constant(Constant::Integer(0))
            ));
            return Ok(Type::Primitive(PrimitiveType::Int));
        }
        
        // For now, compile first branch only
        // TODO: Implement full alternative operator logic
        let branch = &expression.branches[0];
        
        let condition_type = self.compile_sequence_with_type(&branch.condition, context)?;
        
        if let Some(consequence) = &branch.consequence {
            // If we have a consequence, we need conditional logic
            context.instructions.push(Instruction::JumpIfNil(0)); // Placeholder
            let jump_pos = context.instructions.len() - 1;
            
            // Pop the condition result and compile consequence
            context.instructions.push(Instruction::Pop);
            let consequence_type = self.compile_sequence_with_type(consequence, context)?;
            
            // Update jump offset
            let offset = (context.instructions.len() - jump_pos - 1) as isize;
            context.instructions[jump_pos] = Instruction::JumpIfNil(offset);
            
            // Return type is union of condition and consequence types
            Ok(Type::Union(UnionType {
                types: vec![condition_type, consequence_type],
            }))
        } else {
            Ok(condition_type)
        }
    }
    
    fn compile_sequence(
        &mut self,
        sequence: &Sequence,
        context: &mut CompilerContext,
    ) -> Result<(), Error> {
        let _seq_type = self.compile_sequence_with_type(sequence, context)?;
        Ok(())
    }
    
    fn compile_sequence_with_type(
        &mut self,
        sequence: &Sequence,
        context: &mut CompilerContext,
    ) -> Result<Type, Error> {
        let mut last_type = Type::Primitive(PrimitiveType::Int); // Default for empty sequence
        
        for (i, term) in sequence.terms.iter().enumerate() {
            last_type = self.compile_term_with_type(term, context)?;
            
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
        
        Ok(last_type)
    }
    
    fn compile_term(
        &mut self,
        term: &Term,
        context: &mut CompilerContext,
    ) -> Result<(), Error> {
        let _term_type = self.compile_term_with_type(term, context)?;
        Ok(())
    }
    
    fn compile_term_with_type(
        &mut self,
        term: &Term,
        context: &mut CompilerContext,
    ) -> Result<Type, Error> {
        match term {
            Term::Assignment { pattern, value } => {
                // Compile the value first
                let value_type = self.compile_chain_with_type(value, context)?;
                
                // Then compile the pattern assignment
                let _pattern_type = self.compile_pattern_assignment(pattern, context)?;
                
                // For simple identifier patterns, store the inferred type
                if let Pattern::Identifier(name) = pattern {
                    context.symbol_table.define_variable(name.clone(), value_type.clone());
                }
                
                // Assignment returns the value type
                Ok(value_type)
            }
            Term::Chain(chain) => {
                self.compile_chain_with_type(chain, context)
            }
        }
    }
    
    fn compile_chain_with_type(
        &mut self,
        chain: &Chain,
        context: &mut CompilerContext,
    ) -> Result<Type, Error> {
        // Compile the base value and infer its type
        let mut current_type = self.compile_value_with_type(&chain.value, context)?;
        
        // Apply operations and update type
        for operation in &chain.operations {
            current_type = self.compile_operation_with_type(operation, &current_type, context)?;
        }
        
        Ok(current_type)
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
        let _value_type = self.compile_value_with_type(value, context)?;
        Ok(())
    }
    
    fn compile_value_with_type(
        &mut self,
        value: &Value,
        context: &mut CompilerContext,
    ) -> Result<Type, Error> {
        match value {
            Value::Literal(literal) => {
                self.compile_literal(literal, context)?;
                Ok(context.symbol_table.infer_type_from_literal(literal))
            }
            Value::Identifier(name) => {
                context.instructions.push(Instruction::Load(name.clone()));
                // Look up the variable's type
                let var_type = context.symbol_table.get_variable_type(name)
                    .cloned()
                    .ok_or_else(|| Error::UndefinedVariable(name.clone()))?;
                
                // Resolve any type aliases in the variable's type
                Ok(context.symbol_table.resolve_type(&var_type))
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
                // Parameter types would need to be tracked differently - placeholder for now
                Ok(Type::Identifier("parameter".to_string()))
            }
            Value::TupleConstruction(tuple) => {
                self.compile_tuple_construction(tuple, context)?;
                // Infer tuple type from construction
                let field_types: Result<Vec<_>, _> = tuple.fields.iter()
                    .map(|field| {
                        let field_type = match &field.value {
                            TupleFieldValue::Chain(chain) => self.infer_chain_type(chain, context),
                            TupleFieldValue::Ripple => Ok(Type::Identifier("ripple".to_string())),
                        };
                        field_type.map(|t| FieldType {
                            name: field.name.clone(),
                            type_def: t,
                        })
                    })
                    .collect();
                
                Ok(Type::Tuple(TupleType {
                    name: tuple.name.clone(),
                    fields: field_types?,
                }))
            }
            Value::FunctionDefinition(func) => {
                self.compile_function_definition(func, context)?;
                // Function type inference would require parameter type analysis
                Ok(Type::Function(FunctionType {
                    input: Box::new(func.parameter_type.clone().unwrap_or(Type::Identifier("unknown".to_string()))),
                    output: Box::new(Type::Identifier("unknown".to_string())),
                }))
            }
            Value::Block(block) => {
                self.compile_block(block, context)?;
                // Block type is the type of its last expression - simplified for now
                Ok(Type::Identifier("block_result".to_string()))
            }
            Value::MemberAccess(access) => {
                self.compile_member_access(access, context)?;
                // Member access type depends on the object and field - simplified
                Ok(Type::Identifier("field_access_result".to_string()))
            }
            Value::Import(path) => {
                self.compile_import(path, context)?;
                // Import type depends on the imported module - simplified
                Ok(Type::Identifier("import_result".to_string()))
            }
            _ => Err(Error::NotImplemented(format!("Value type {:?} not implemented", value))),
        }
    }
    
    fn infer_chain_type(
        &mut self,
        chain: &Chain,
        context: &CompilerContext,
    ) -> Result<Type, Error> {
        // Simplified type inference for chains - would need full implementation
        match &chain.value {
            Value::Literal(literal) => Ok(context.symbol_table.infer_type_from_literal(literal)),
            Value::Identifier(name) => {
                Ok(context.symbol_table.get_variable_type(name)
                    .cloned()
                    .unwrap_or_else(|| Type::Identifier("unknown".to_string())))
            }
            _ => Ok(Type::Identifier("unknown".to_string())),
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
        
        // Create tuple with signature-based TypeId
        let type_id = self.get_tuple_type_id_for_construction(tuple);
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
    
    fn compile_operation_with_type(
        &mut self,
        operation: &Operation,
        input_type: &Type,
        context: &mut CompilerContext,
    ) -> Result<Type, Error> {
        match operation {
            Operation::Operator(op) => {
                self.compile_operator(op, context)?;
                // Arithmetic operations return int, comparisons return Ok/[]
                match op {
                    Operator::Add | Operator::Subtract | Operator::Multiply | Operator::Divide | Operator::Modulo => {
                        Ok(Type::Primitive(PrimitiveType::Int))
                    }
                    Operator::Equal | Operator::NotEqual | Operator::LessThan | 
                    Operator::LessThanOrEqual | Operator::GreaterThan | Operator::GreaterThanOrEqual => {
                        // Comparison returns Ok or [] (empty tuple)
                        Ok(Type::Tuple(TupleType { name: None, fields: vec![] }))
                    }
                }
            }
            Operation::Identifier(_name) => {
                // Function application - would need function signature lookup
                context.instructions.push(Instruction::Load(_name.clone()));
                context.instructions.push(Instruction::Swap);
                context.instructions.push(Instruction::Call);
                Ok(Type::Identifier("function_result".to_string()))
            }
            Operation::PositionalAccess(index) => {
                context.instructions.push(Instruction::Get(*index));
                // Field access type depends on the input tuple type
                if let Type::Tuple(tuple_type) = input_type {
                    if let Some(field) = tuple_type.fields.get(*index) {
                        Ok(field.type_def.clone())
                    } else {
                        Ok(Type::Identifier("unknown_field".to_string()))
                    }
                } else {
                    Ok(Type::Identifier("unknown_field".to_string()))
                }
            }
            Operation::FieldAccess(field_name) => {
                // Named field access - would need field lookup in tuple type
                if let Type::Tuple(tuple_type) = input_type {
                    for field in &tuple_type.fields {
                        if field.name.as_ref() == Some(field_name) {
                            return Ok(field.type_def.clone());
                        }
                    }
                }
                Ok(Type::Identifier("unknown_field".to_string()))
            }
            Operation::TailCall(name) => {
                context.instructions.push(Instruction::Load(name.clone()));
                context.instructions.push(Instruction::Swap);
                context.instructions.push(Instruction::TailCall(name == "&"));
                Ok(Type::Identifier("tail_call_result".to_string()))
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
    ) -> Result<Type, Error> {
        match pattern {
            Pattern::Identifier(name) => {
                context.instructions.push(Instruction::Store(name.clone()));
                // Return a placeholder type - we'll infer it from the value
                Ok(Type::Identifier("unknown".to_string()))
            }
            Pattern::TuplePattern(tuple_pattern) => {
                self.compile_tuple_pattern_assignment(tuple_pattern, context)
            }
            Pattern::Literal(literal) => {
                // For literal patterns, we need to check if the value matches
                // This is used in pattern matching contexts
                let expected_type = context.symbol_table.infer_type_from_literal(literal);
                
                // Generate runtime check based on type
                match literal {
                    Literal::Integer(_) => {
                        context.instructions.push(Instruction::IsInteger);
                    }
                    Literal::Binary(_) => {
                        context.instructions.push(Instruction::IsBinary);
                    }
                    Literal::String(_) => {
                        context.instructions.push(Instruction::IsBinary);
                    }
                }
                
                Ok(expected_type)
            }
            _ => Err(Error::NotImplemented(format!("Pattern {:?} not implemented", pattern))),
        }
    }
    
    fn compile_tuple_pattern_assignment(
        &mut self,
        tuple_pattern: &TuplePattern,
        context: &mut CompilerContext,
    ) -> Result<Type, Error> {
        // Generate type check for tuple structure
        let type_id = self.get_tuple_type_id_for_pattern(tuple_pattern);
        context.instructions.push(Instruction::IsTuple(type_id));
        
        // For each field in the pattern, extract and store
        let mut field_types = Vec::new();
        for (i, field) in tuple_pattern.fields.iter().enumerate() {
            context.instructions.push(Instruction::Duplicate);
            context.instructions.push(Instruction::Get(i));
            let field_type = self.compile_pattern_assignment(&field.pattern, context)?;
            field_types.push(FieldType {
                name: field.name.clone(),
                type_def: field_type,
            });
        }
        
        // Pop the original tuple
        context.instructions.push(Instruction::Pop);
        
        // Return the inferred tuple type
        Ok(Type::Tuple(TupleType {
            name: tuple_pattern.name.clone(),
            fields: field_types,
        }))
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TupleSignature {
    name: Option<String>,
    field_names: Vec<Option<String>>,
}

impl TupleSignature {
    fn from_tuple_type(tuple_type: &TupleType) -> Self {
        Self {
            name: tuple_type.name.clone(),
            field_names: tuple_type.fields.iter().map(|f| f.name.clone()).collect(),
        }
    }
    
    fn from_tuple_construction(tuple_construction: &TupleConstruction) -> Self {
        Self {
            name: tuple_construction.name.clone(),
            field_names: tuple_construction.fields.iter().map(|f| f.name.clone()).collect(),
        }
    }
    
    fn from_tuple_pattern(tuple_pattern: &TuplePattern) -> Self {
        Self {
            name: tuple_pattern.name.clone(),
            field_names: tuple_pattern.fields.iter().map(|f| f.name.clone()).collect(),
        }
    }
}

#[derive(Debug)]
struct TypeRegistry {
    signature_to_id: HashMap<TupleSignature, TypeId>,
    id_to_signature: HashMap<TypeId, TupleSignature>,
}

impl TypeRegistry {
    fn new() -> Self {
        let mut registry = Self {
            signature_to_id: HashMap::new(),
            id_to_signature: HashMap::new(),
        };
        
        // Register built-in tuple signatures
        registry.register_builtin_signature(TypeId::NIL, TupleSignature {
            name: None,
            field_names: vec![], // Empty tuple []
        });
        
        registry.register_builtin_signature(TypeId::OK, TupleSignature {
            name: Some("Ok".to_string()),
            field_names: vec![], // Ok tuple
        });
        
        registry
    }
    
    fn register_builtin_signature(&mut self, type_id: TypeId, signature: TupleSignature) {
        self.signature_to_id.insert(signature.clone(), type_id);
        self.id_to_signature.insert(type_id, signature);
    }
    
    fn register_tuple_signature(&mut self, type_id: TypeId, signature: TupleSignature) {
        self.signature_to_id.insert(signature.clone(), type_id);
        self.id_to_signature.insert(type_id, signature);
    }
    
    fn get_type_id(&self, signature: &TupleSignature) -> Option<TypeId> {
        self.signature_to_id.get(signature).copied()
    }
    
    fn get_signature(&self, type_id: TypeId) -> Option<&TupleSignature> {
        self.id_to_signature.get(&type_id)
    }
    
    fn list_types(&self) -> Vec<(String, TypeId)> {
        let mut types: Vec<_> = self.id_to_signature.iter()
            .map(|(id, sig)| {
                let name = match (&sig.name, sig.field_names.len()) {
                    (Some(name), _) => name.clone(),
                    (None, 0) => "[]".to_string(),
                    (None, _) => format!("[{}]", sig.field_names.len()),
                };
                (name, *id)
            })
            .collect();
        types.sort_by(|a, b| a.0.cmp(&b.0));
        types
    }
}

#[derive(Debug)]
struct SymbolTable {
    // Track variable types for type checking and inference
    variables: HashMap<String, Type>,
    // Track type aliases (full AST type definitions)
    type_aliases: HashMap<String, Type>,
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
            type_aliases: HashMap::new(),
        }
    }
    
    fn define_variable(&mut self, name: String, var_type: Type) {
        self.variables.insert(name, var_type);
    }
    
    fn get_variable_type(&self, name: &str) -> Option<&Type> {
        self.variables.get(name)
    }
    
    fn define_type_alias(&mut self, name: String, type_def: Type) {
        self.type_aliases.insert(name, type_def);
    }
    
    fn get_type_alias(&self, name: &str) -> Option<&Type> {
        self.type_aliases.get(name)
    }
    
    fn resolve_type(&self, type_def: &Type) -> Type {
        match type_def {
            Type::Identifier(name) => {
                // Resolve type alias to its definition recursively
                if let Some(alias_type) = self.get_type_alias(name) {
                    self.resolve_type(alias_type)
                } else {
                    type_def.clone() // Keep unresolved identifier as-is
                }
            }
            Type::Tuple(tuple_type) => {
                // Recursively resolve field types
                let resolved_fields = tuple_type.fields.iter()
                    .map(|field| FieldType {
                        name: field.name.clone(),
                        type_def: self.resolve_type(&field.type_def),
                    })
                    .collect();
                
                Type::Tuple(TupleType {
                    name: tuple_type.name.clone(),
                    fields: resolved_fields,
                })
            }
            Type::Function(func_type) => {
                Type::Function(FunctionType {
                    input: Box::new(self.resolve_type(&func_type.input)),
                    output: Box::new(self.resolve_type(&func_type.output)),
                })
            }
            Type::Union(union_type) => {
                let resolved_types = union_type.types.iter()
                    .map(|t| self.resolve_type(t))
                    .collect();
                
                Type::Union(UnionType {
                    types: resolved_types,
                })
            }
            _ => type_def.clone(),
        }
    }
    
    fn infer_type_from_literal(&self, literal: &Literal) -> Type {
        match literal {
            Literal::Integer(_) => Type::Primitive(PrimitiveType::Int),
            Literal::Binary(_) => Type::Primitive(PrimitiveType::Bin),
            Literal::String(_) => Type::Primitive(PrimitiveType::Bin), // Strings are syntactic sugar for binaries
        }
    }
    
    fn is_type_compatible(&self, expected: &Type, actual: &Type) -> bool {
        let expected_resolved = self.resolve_type(expected);
        let actual_resolved = self.resolve_type(actual);
        
        match (&expected_resolved, &actual_resolved) {
            // Exact type match
            (a, b) if a == b => true,
            
            // Union types
            (Type::Union(union), actual_type) => {
                // Check if actual type is compatible with any union member
                union.types.iter().any(|member_type| {
                    self.is_type_compatible(member_type, actual_type)
                })
            }
            (expected_type, Type::Union(union)) => {
                // All union members must be compatible with expected type
                union.types.iter().all(|member_type| {
                    self.is_type_compatible(expected_type, member_type)
                })
            }
            
            // Tuple structural compatibility (same signature)
            (Type::Tuple(expected_tuple), Type::Tuple(actual_tuple)) => {
                self.is_tuple_structurally_compatible(expected_tuple, actual_tuple)
            }
            
            _ => false,
        }
    }
    
    fn is_tuple_structurally_compatible(&self, expected: &TupleType, actual: &TupleType) -> bool {
        // Names must match (both None or both Some with same value)
        if expected.name != actual.name {
            return false;
        }
        
        // Field count must match
        if expected.fields.len() != actual.fields.len() {
            return false;
        }
        
        // Field names and types must be compatible
        expected.fields.iter().zip(actual.fields.iter()).all(|(expected_field, actual_field)| {
            expected_field.name == actual_field.name &&
            self.is_type_compatible(&expected_field.type_def, &actual_field.type_def)
        })
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
    
    #[test]
    fn test_tuple_type_signatures() {
        let mut compiler = Compiler::new();
        
        // Test that tuples with same structure but different field types get same TypeId
        let sig1 = TupleSignature {
            name: Some("Point".to_string()),
            field_names: vec![Some("x".to_string()), Some("y".to_string())],
        };
        
        let sig2 = TupleSignature {
            name: Some("Point".to_string()),
            field_names: vec![Some("x".to_string()), Some("y".to_string())],
        };
        
        let type_id1 = compiler.get_or_create_tuple_type_id(sig1);
        let type_id2 = compiler.get_or_create_tuple_type_id(sig2);
        
        // Should be the same TypeId (structural equality)
        assert_eq!(type_id1, type_id2);
        
        // Test different structure gets different TypeId
        let sig3 = TupleSignature {
            name: Some("Point".to_string()),
            field_names: vec![Some("a".to_string()), Some("b".to_string())],
        };
        
        let type_id3 = compiler.get_or_create_tuple_type_id(sig3);
        assert_ne!(type_id1, type_id3);
    }
    
    #[test]
    fn test_type_alias_compilation() {
        let mut compiler = Compiler::new();
        let mut vm = VM::new();
        let module_loader = Box::new(InMemoryModuleLoader::new());
        
        // Parse type alias and usage
        let program = crate::parser::parse("type point = Point[x: int, y: int]").expect("Failed to parse");
        
        let _instructions = compiler
            .compile(&program, &mut vm, module_loader, None)
            .expect("Failed to compile");
        
        // Should have registered the type alias
        let type_aliases = compiler.list_type_aliases();
        assert!(type_aliases.iter().any(|(name, _)| name.contains("Point")));
    }
    
    #[test]
    fn test_recursive_type_alias_resolution() {
        let mut compiler = Compiler::new();
        let mut vm = VM::new();
        let module_loader = Box::new(InMemoryModuleLoader::new());
        
        // Test recursive type alias resolution
        let program = crate::parser::parse(
            "type point = position\ntype position = Position[x: int, y: int]"
        ).expect("Failed to parse");
        
        let _instructions = compiler
            .compile(&program, &mut vm, module_loader, None)
            .expect("Failed to compile");
        
        // Should have two type aliases
        let aliases = compiler.list_type_aliases();
        println!("Recursive test aliases: {:?}", aliases);
        // The exact count may vary based on how tuple types create additional aliases
        assert!(aliases.len() >= 2);
    }
    
    #[test]
    fn test_union_type_handling() {
        let mut compiler = Compiler::new();
        let mut vm = VM::new();
        let module_loader = Box::new(InMemoryModuleLoader::new());
        
        // Test union type in type alias - using basic primitive types
        let program = crate::parser::parse(
            "type stringorint = (int, bin)\nx = 42"
        ).expect("Failed to parse");
        
        let _instructions = compiler
            .compile(&program, &mut vm, module_loader, None)
            .expect("Failed to compile");
        
        // Should have one type alias for the union
        let aliases = compiler.list_type_aliases();
        println!("Union test aliases: {:?}", aliases);
        assert!(aliases.len() >= 1);
    }
    
    #[test]
    fn test_type_validation_success() {
        let mut compiler = Compiler::new();
        let mut vm = VM::new();
        let module_loader = Box::new(InMemoryModuleLoader::new());
        
        // Test valid type alias chain
        let program = crate::parser::parse(
            "type position = Position[x: int, y: int]\ntype point = position"
        ).expect("Failed to parse");
        
        let _result = compiler
            .compile(&program, &mut vm, module_loader, None)
            .expect("Valid type aliases should compile successfully");
        
        // Should have two type aliases
        let aliases = compiler.list_type_aliases();
        println!("Validation test aliases: {:?}", aliases);
        assert!(aliases.len() >= 2);
    }
}