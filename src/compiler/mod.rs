use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

mod codegen;
mod modules;
mod pattern;
mod typing;
mod variables;

pub use codegen::InstructionBuilder;
pub use modules::{ModuleCache, compile_type_import};
pub use pattern::{MatchCertainty, PatternCompiler};
pub use typing::{TupleAccessor, union_types};

use crate::{
    ast,
    builtins::BUILTIN_REGISTRY,
    bytecode::{Constant, Function, Instruction, TypeId},
    modules::{ModuleError, ModuleLoader},
    types::{CallableType, Type},
    vm::{BinaryRef, VM, Value},
};

use typing::TypeContext;

#[derive(Debug, PartialEq)]
pub enum Error {
    // Undefined errors
    VariableUndefined(String),
    BuiltinUndefined(String),
    FunctionUndefined(usize),

    // Type system errors
    TypeUnresolved(String),
    TypeAliasMissing(String),
    TypeMismatch {
        expected: String,
        found: String,
    },
    TypeNotInRegistry {
        type_id: TypeId,
    },

    // Structure errors
    FieldDuplicated(String),
    TupleFieldTypeUnresolved {
        field_index: usize,
    },

    // Access errors
    FieldNotFound {
        field_name: String,
        type_name: String,
    },
    FieldAccessOnNonTuple {
        field_name: String,
    },
    PositionalAccessOnNonTuple {
        index: usize,
    },

    // Member access
    MemberFieldNotFound {
        field_name: String,
        target: String,
    },
    MemberAccessOnNonTuple {
        target: String,
    },

    // Parameter access
    ParameterIndexOutOfBounds {
        index: usize,
    },
    ParameterAccessOnNonTuple {
        index: usize,
    },

    // Operator errors
    OperatorTypeNotInRegistry {
        type_id: String,
    },
    OperatorOnNonTuple {
        operator: String,
    },

    // Module errors
    ModuleLoad(ModuleError),
    ModuleParse {
        module_path: String,
        error: crate::parser::Error,
    },
    ModuleExecution {
        module_path: String,
        error: crate::vm::Error,
    },
    ModuleTypeMissing {
        type_name: String,
        module_path: String,
    },

    // Language feature errors
    FeatureUnsupported(String),

    // Compilation flow errors
    ChainValueUnused,

    // Destructuring errors
    DestructuringOnNonTuple(String),
    DestructuringFieldMissing {
        type_name: String,
        field_name: String,
    },

    // Pattern matching errors
    PatternNoMatchingTypes {
        pattern: String,
    },

    // Internal consistency errors
    InternalError {
        message: String,
    },
}

pub struct Compiler<'a> {
    // Core components
    codegen: InstructionBuilder,
    type_context: TypeContext<'a>,
    module_cache: ModuleCache,

    // State management
    scopes: Vec<HashMap<String, Type>>,
    vm: &'a mut VM,
    module_loader: &'a dyn ModuleLoader,
    module_path: Option<PathBuf>,

    // Parameter field tracking for nested function definitions
    parameter_fields_stack: Vec<HashMap<String, (usize, Type)>>,

    // Counter for nested ripple contexts in tuple construction
    ripple_depth: usize,
}

impl<'a> Compiler<'a> {
    pub fn compile(
        program: ast::Program,
        type_aliases: &'a mut HashMap<String, Type>,
        module_loader: &'a dyn ModuleLoader,
        vm: &'a mut VM,
        module_path: Option<PathBuf>,
    ) -> Result<Vec<Instruction>, Error> {
        // Initialize compiler scope with existing VM variables
        let existing_variables = vm.list_variables();

        let mut compiler = Self {
            codegen: InstructionBuilder::new(),
            type_context: TypeContext::new(type_aliases),
            module_cache: ModuleCache::new(),
            scopes: vec![HashMap::new()],
            vm,
            module_loader,
            module_path,
            parameter_fields_stack: Vec::new(),
            ripple_depth: 0,
        };

        for (name, value) in existing_variables {
            let var_type = compiler.value_to_type(&value)?;
            compiler.define_variable(&name, var_type);
        }

        // Phase 1: Pre-register all type aliases as placeholders
        for statement in &program.statements {
            if let ast::Statement::TypeAlias { name, .. } = statement {
                // Register placeholder for forward references
                compiler.type_context.type_aliases.insert(
                    name.clone(),
                    Type::from_types(vec![]), // Empty placeholder
                );
            }
        }

        // Phase 2: Compile all statements (now forward references work)
        let num_statements = program.statements.len();
        for (i, statement) in program.statements.into_iter().enumerate() {
            let is_last = i == num_statements - 1;
            let is_expression = matches!(&statement, ast::Statement::Expression(_));

            compiler.compile_statement(statement)?;

            // Pop intermediate expression results, keeping only the last one
            if !is_last && is_expression {
                compiler.codegen.add_instruction(Instruction::Pop);
            }
        }

        Ok(compiler.codegen.instructions)
    }

    fn compile_statement(&mut self, statement: ast::Statement) -> Result<(), Error> {
        match statement {
            ast::Statement::TypeAlias {
                name,
                type_definition,
            } => self.compile_type_alias(&name, type_definition),
            ast::Statement::TypeImport {
                pattern,
                module_path,
            } => self.compile_type_import(pattern, &module_path),
            ast::Statement::Expression(expression) => {
                self.compile_expression(expression, Type::nil())?;
                Ok(())
            }
        }
    }

    fn compile_type_alias(&mut self, name: &str, type_definition: ast::Type) -> Result<(), Error> {
        // Push the type name onto the resolution stack to detect cycles
        self.type_context.resolution_stack.push(name.to_string());
        let resolved_type = self
            .type_context
            .resolve_ast_type(type_definition, self.vm)?;
        self.type_context.resolution_stack.pop();

        self.type_context
            .type_aliases
            .insert(name.to_string(), resolved_type);
        Ok(())
    }

    fn compile_type_import(
        &mut self,
        pattern: ast::TypeImportPattern,
        module_path: &str,
    ) -> Result<(), Error> {
        compile_type_import(
            pattern,
            module_path,
            &mut self.module_cache,
            self.module_loader,
            self.module_path.as_ref(),
            &mut self.type_context,
            self.vm,
        )
    }

    fn compile_literal(&mut self, literal: ast::Literal) -> Result<Type, Error> {
        match literal {
            ast::Literal::Integer(integer) => {
                let index = self.vm.register_constant(Constant::Integer(integer));
                self.codegen.add_instruction(Instruction::Constant(index));
                Ok(Type::Integer)
            }
            ast::Literal::Binary(bytes) => {
                // TODO: is clone bad?
                let index = self.vm.register_constant(Constant::Binary(bytes.clone()));
                self.codegen.add_instruction(Instruction::Constant(index));
                Ok(Type::Binary)
            }
            ast::Literal::String(string) => {
                let bytes = string.as_bytes().to_vec();
                let index = self.vm.register_constant(Constant::Binary(bytes));
                self.codegen.add_instruction(Instruction::Constant(index));
                Ok(Type::Binary)
            }
        }
    }

    fn compile_value_tuple(
        &mut self,
        tuple_name: Option<String>,
        fields: Vec<ast::TupleField>,
        parameter_type: Type,
    ) -> Result<Type, Error> {
        Self::check_field_name_duplicates(&fields, |f| f.name.as_ref())?;

        // Compile field values and collect their types
        let mut field_types = Vec::new();
        for field in &fields {
            let field_type = self.compile_chain(field.value.clone(), parameter_type.clone())?;
            field_types.push((field.name.clone(), field_type));
        }

        // Register the tuple type with potentially union field types
        let type_id = self.vm.register_type(tuple_name, field_types);
        self.codegen.add_instruction(Instruction::Tuple(type_id));

        Ok(Type::Tuple(type_id))
    }

    fn compile_operation_tuple(
        &mut self,
        name: Option<String>,
        fields: Vec<ast::TupleField>,
        value_type: Type,
        parameter_type: Type,
    ) -> Result<Type, Error> {
        // The parser ensures only tuples with ripples are parsed as Operation::Tuple
        // so we don't need to check for ripples here

        // Use a unique variable name for each ripple context to avoid conflicts
        let ripple_var_name = format!("~{}", self.ripple_depth);
        self.ripple_depth += 1;
        self.codegen
            .add_instruction(Instruction::Store(ripple_var_name.clone()));

        Self::check_field_name_duplicates(&fields, |f| f.name.as_ref())?;

        // Compile field values and collect their types
        let mut field_types = Vec::new();
        for field in &fields {
            // Pass the ripple value so nested tuples can access it
            // If the chain starts with Ripple, it needs the ripple value
            let input_type = match &field.value.input {
                ast::ChainInput::Ripple => value_type.clone(),
                _ => parameter_type.clone(),
            };
            let field_type = self.compile_chain(field.value.clone(), input_type)?;

            field_types.push((field.name.clone(), field_type));
        }

        // Pop the ripple context
        self.ripple_depth -= 1;

        // Register the tuple type with potentially union field types
        let type_id = self.vm.register_type(name, field_types);
        self.codegen.add_instruction(Instruction::Tuple(type_id));

        Ok(Type::Tuple(type_id))
    }

    fn compile_function_definition(
        &mut self,
        function_definition: ast::FunctionDefinition,
    ) -> Result<Type, Error> {
        let mut function_params: HashSet<String> = HashSet::new();

        if let Some(ast::Type::Tuple(tuple_type)) = &function_definition.parameter_type {
            for field in &tuple_type.fields {
                if let Some(field_name) = &field.name {
                    function_params.insert(field_name.clone());
                }
            }
        }

        let captures = variables::collect_free_variables(
            &function_definition.body,
            &function_params,
            &|name| self.lookup_variable(name).is_some(),
        );

        let parameter_type = match &function_definition.parameter_type {
            Some(t) => self.type_context.resolve_ast_type(t.clone(), self.vm)?,
            None => Type::nil(),
        };

        let saved_instructions = std::mem::take(&mut self.codegen.instructions);
        let saved_scopes = std::mem::take(&mut self.scopes);

        self.scopes = vec![HashMap::new()];
        self.codegen.instructions = Vec::new();

        for capture_name in &captures {
            if let Some(var_type) = self.lookup_variable_in_scopes(&saved_scopes, capture_name) {
                self.define_variable(capture_name, var_type);
            }
        }

        let mut parameter_fields = HashMap::new();
        if let Some(ast::Type::Tuple(tuple_type)) = &function_definition.parameter_type {
            for (field_index, field) in tuple_type.fields.iter().enumerate() {
                if let Some(field_name) = &field.name {
                    let field_type = self
                        .type_context
                        .resolve_ast_type(field.type_def.clone(), self.vm)?;
                    parameter_fields.insert(field_name.clone(), (field_index, field_type));
                }
            }
        }
        self.parameter_fields_stack.push(parameter_fields);

        let body_type = self.compile_block(function_definition.body, parameter_type.clone())?;
        self.codegen.add_instruction(Instruction::Return);

        self.parameter_fields_stack.pop();

        let func_type = CallableType {
            parameter: parameter_type,
            result: body_type,
        };

        let function_instructions = std::mem::take(&mut self.codegen.instructions);
        let function_index = self.vm.register_function(Function {
            instructions: function_instructions,
            captures: captures.into_iter().collect(),
            function_type: Some(func_type.clone()),
        });

        self.codegen.instructions = saved_instructions;
        self.scopes = saved_scopes;

        self.codegen
            .add_instruction(Instruction::Function(function_index));

        let function_type = Type::Callable(Box::new(func_type));

        Ok(function_type)
    }

    fn compile_match(&mut self, pattern: ast::Pattern, value_type: Type) -> Result<Type, Error> {
        let start_jump_addr = self.codegen.emit_jump_placeholder();
        let fail_jump_addr = self.codegen.emit_jump_placeholder();

        self.codegen.patch_jump_to_here(start_jump_addr);

        let mut pattern_compiler =
            PatternCompiler::new(&mut self.codegen, &mut self.type_context, self.vm);
        let (certainty, bindings) =
            pattern_compiler.compile_pattern_match(&pattern, &value_type, fail_jump_addr)?;

        match certainty {
            MatchCertainty::WontMatch => {
                self.codegen.add_instruction(Instruction::Pop);
                self.codegen
                    .add_instruction(Instruction::Tuple(TypeId::NIL));
                Ok(Type::nil())
            }
            MatchCertainty::WillMatch | MatchCertainty::MightMatch => {
                for (variable_name, variable_type) in &bindings {
                    self.define_variable(&variable_name, variable_type.clone());
                }
                self.codegen.add_instruction(Instruction::Pop);
                self.codegen.add_instruction(Instruction::Tuple(TypeId::OK));
                let success_jump_addr = self.codegen.emit_jump_placeholder();

                self.codegen.patch_jump_to_here(fail_jump_addr);
                self.codegen.add_instruction(Instruction::Pop);
                self.codegen
                    .add_instruction(Instruction::Tuple(TypeId::NIL));

                self.codegen.patch_jump_to_here(success_jump_addr);

                if certainty == MatchCertainty::WillMatch {
                    Ok(Type::ok())
                } else {
                    Ok(Type::from_types(vec![Type::ok(), Type::nil()]))
                }
            }
        }
    }

    fn compile_block(&mut self, block: ast::Block, parameter_type: Type) -> Result<Type, Error> {
        let mut next_branch_jumps = Vec::new();
        let mut end_jumps = Vec::new();
        let mut branch_types = Vec::new();
        let mut branch_starts = Vec::new();

        self.codegen.add_instruction(Instruction::Enter);
        self.scopes.push(HashMap::new());

        for (i, branch) in block.branches.iter().enumerate() {
            let is_last_branch = i == block.branches.len() - 1;

            branch_starts.push(self.codegen.instructions.len());

            if i > 0 {
                self.codegen.add_instruction(Instruction::Pop);
                self.codegen.add_instruction(Instruction::Reset);
                self.scopes
                    .last_mut()
                    .ok_or_else(|| Error::InternalError {
                        message: "No scope available when compiling block branch".to_string(),
                    })?
                    .clear();
            }

            let condition_type =
                self.compile_expression(branch.condition.clone(), parameter_type.clone())?;

            // If condition is compile-time NIL (won't match), skip this branch entirely
            if condition_type.as_concrete() == Some(&Type::nil()) {
                // Don't add to branch_types, continue to next branch
                continue;
            }

            if let Some(ref consequence) = branch.consequence {
                // Branch has a consequence - compile it
                let next_branch_jump = self.codegen.emit_duplicate_jump_if_nil_pop();
                next_branch_jumps.push((next_branch_jump, i + 1));

                let consequence_type =
                    self.compile_expression(consequence.clone(), parameter_type.clone())?;
                branch_types.push(consequence_type);

                // If this is the last branch and the condition can be nil,
                // the block can also return nil (when condition fails)
                if is_last_branch {
                    let condition_can_be_nil = match &condition_type {
                        Type::Union(types) => types.iter().any(|t| t == &Type::nil()),
                        Type::Tuple(id) if *id == TypeId::NIL => true,
                        _ => false,
                    };

                    if condition_can_be_nil {
                        branch_types.push(Type::nil());
                    }
                }
            } else {
                // No consequence - use condition type
                branch_types.push(condition_type);
            }

            if !is_last_branch {
                if branch.consequence.is_some() {
                    let end_jump = self.codegen.emit_jump_placeholder();
                    end_jumps.push(end_jump);
                } else {
                    self.codegen.add_instruction(Instruction::Duplicate);
                    let success_jump = self.codegen.emit_jump_if_placeholder();
                    end_jumps.push(success_jump);
                }
            }
        }

        let end_addr = self.codegen.instructions.len();

        self.codegen.add_instruction(Instruction::Exit);
        self.scopes.pop();

        for (jump_addr, next_branch_idx) in next_branch_jumps {
            let target_addr = if next_branch_idx >= branch_starts.len() {
                end_addr
            } else {
                branch_starts[next_branch_idx]
            };
            self.codegen.patch_jump_to_addr(jump_addr, target_addr);
        }

        for jump_addr in end_jumps {
            self.codegen.patch_jump_to_addr(jump_addr, end_addr);
        }

        if branch_types.is_empty() {
            // If no branches produced types (e.g., all were compile-time NIL),
            // return NIL as the block type
            Ok(Type::nil())
        } else {
            union_types(branch_types)
        }
    }

    fn compile_expression(
        &mut self,
        expression: ast::Expression,
        parameter_type: Type,
    ) -> Result<Type, Error> {
        let mut last_type = None;
        let mut end_jumps = Vec::new();

        for (i, chain) in expression.chains.iter().enumerate() {
            let chain_type = self.compile_chain(chain.clone(), parameter_type.clone())?;

            // Check if the previous type contained nil and we're not on the first chain
            let should_propagate_nil = if i > 0 {
                if let Some(ref prev_type) = last_type {
                    match prev_type {
                        Type::Union(types) => types.iter().any(|t| t == &Type::nil()),
                        Type::Tuple(id) if *id == TypeId::NIL => true,
                        _ => false,
                    }
                } else {
                    false
                }
            } else {
                false
            };

            // If previous type could be nil and we're past the first chain, propagate nil possibility
            last_type = Some(if should_propagate_nil {
                Type::from_types(vec![chain_type, Type::nil()])
            } else {
                chain_type
            });

            // If last_type is NIL, subsequent chains are unreachable - break early
            if let Some(ref last_type) = last_type {
                if last_type.as_concrete() == Some(&Type::nil()) {
                    break;
                }
            }

            if i < expression.chains.len() - 1 {
                let end_jump = self.codegen.emit_duplicate_jump_if_nil_pop();
                end_jumps.push(end_jump);
            }
        }

        let end_addr = self.codegen.instructions.len();
        for jump_addr in end_jumps {
            self.codegen.patch_jump_to_addr(jump_addr, end_addr);
        }

        last_type.ok_or_else(|| Error::InternalError {
            message: "Expression compiled with no chains".to_string(),
        })
    }

    fn compile_chain(&mut self, chain: ast::Chain, input_type: Type) -> Result<Type, Error> {
        let mut value_type = match chain.input {
            ast::ChainInput::Value(ast::Value::Literal(literal)) => self.compile_literal(literal),
            ast::ChainInput::Value(ast::Value::Tuple(tuple)) => {
                self.compile_value_tuple(tuple.name, tuple.fields, input_type.clone())
            }
            ast::ChainInput::Value(ast::Value::FunctionDefinition(function_definition)) => {
                self.compile_function_definition(function_definition)
            }
            ast::ChainInput::Value(ast::Value::Block(block)) => {
                self.codegen
                    .add_instruction(Instruction::Tuple(TypeId::NIL));
                self.compile_block(block, Type::nil())
            }
            ast::ChainInput::Value(ast::Value::MemberAccess(member_access)) => self
                .compile_value_member_access(
                    &member_access.target,
                    member_access.accessors,
                    input_type.clone(),
                ),
            ast::ChainInput::Value(ast::Value::Import(path)) => self.compile_value_import(&path),
            ast::ChainInput::Value(ast::Value::Builtin(name)) => self.compile_value_builtin(&name),
            ast::ChainInput::Parameter => {
                self.codegen.add_instruction(Instruction::Parameter);
                Ok(input_type.clone())
            }
            ast::ChainInput::Ripple => {
                // Use the ripple value from the enclosing context
                // ripple_depth - 1 because we incremented it when storing
                let ripple_var_name = format!("~{}", self.ripple_depth - 1);
                self.codegen
                    .add_instruction(Instruction::Load(ripple_var_name));
                Ok(input_type.clone())
            }
        }?;

        for operation in chain.operations {
            value_type = self.compile_operation(operation, value_type, input_type.clone())?;
        }

        Ok(value_type)
    }

    fn value_to_instructions(&mut self, value: &Value) -> Result<Type, Error> {
        match value {
            Value::Integer(int_value) => {
                let index = self.vm.register_constant(Constant::Integer(*int_value));
                self.codegen.add_instruction(Instruction::Constant(index));
                Ok(Type::Integer)
            }
            Value::Binary(binary_ref) => {
                match binary_ref {
                    BinaryRef::Constant(const_index) => {
                        self.codegen
                            .add_instruction(Instruction::Constant(*const_index));
                    }
                    BinaryRef::Heap(bytes) => {
                        // Create a constant from the heap binary
                        let constant = crate::bytecode::Constant::Binary(bytes.to_vec());
                        let index = self.vm.register_constant(constant);
                        self.codegen.add_instruction(Instruction::Constant(index));
                    }
                }
                Ok(Type::Binary)
            }
            Value::Tuple(type_id, fields) => {
                for field in fields {
                    self.value_to_instructions(field)?;
                }
                self.codegen.add_instruction(Instruction::Tuple(*type_id));
                Ok(Type::Tuple(*type_id))
            }
            Value::Function { function, captures } => {
                // Get the function definition
                let func_def = self.vm.get_functions().get(*function).cloned().ok_or(
                    Error::FeatureUnsupported("Invalid function reference".to_string()),
                )?;

                let func_index = self.vm.register_function(func_def.clone());

                // If we have captures, create a temporary scope with them
                if !captures.is_empty() && !func_def.captures.is_empty() {
                    // Enter a new scope for the captures
                    // Enter expects a parameter value on the stack, push NIL since we don't need it
                    self.codegen
                        .add_instruction(Instruction::Tuple(TypeId::NIL));
                    self.codegen.add_instruction(Instruction::Enter);

                    // Set up each capture in the scope
                    for (name, value) in func_def.captures.iter().zip(captures.iter()) {
                        // Recursively convert the captured value to instructions
                        self.value_to_instructions(value)?;
                        // Store it with the capture's name
                        self.codegen
                            .add_instruction(Instruction::Store(name.clone()));
                    }

                    // Emit the function (will capture from our scope)
                    self.codegen
                        .add_instruction(Instruction::Function(func_index));

                    // Exit the temporary scope
                    self.codegen.add_instruction(Instruction::Exit);
                } else {
                    // No captures - use the simple approach
                    self.codegen
                        .add_instruction(Instruction::Function(func_index));
                }

                self.function_to_type(func_index)
            }
            Value::Builtin(name) => {
                let builtin_index = self.vm.register_builtin(name.to_string());
                self.codegen
                    .add_instruction(Instruction::Builtin(builtin_index));

                if let Some((param_type, result_type)) =
                    BUILTIN_REGISTRY.resolve_signature(name, self.vm)
                {
                    Ok(Type::Callable(Box::new(CallableType {
                        parameter: param_type,
                        result: result_type,
                    })))
                } else {
                    Err(Error::BuiltinUndefined(name.to_string()))
                }
            }
        }
    }

    fn compile_value_import(&mut self, module_path: &str) -> Result<Type, Error> {
        // Check for circular imports
        if self
            .module_cache
            .import_stack
            .contains(&module_path.to_string())
        {
            return Err(Error::FeatureUnsupported(
                "Circular import detected".to_string(),
            ));
        }

        // Check if we already have the evaluated module cached
        let value = if let Some(cached_value) =
            self.module_cache.evaluation_cache.get(module_path).cloned()
        {
            cached_value
        } else {
            // Add to import stack to track circular imports
            self.module_cache.import_stack.push(module_path.to_string());
            let value = self.import_module_internal(module_path)?;
            self.module_cache.import_stack.pop();

            // Cache the evaluated module
            self.module_cache
                .evaluation_cache
                .insert(module_path.to_string(), value.clone());

            value
        };

        // Convert the runtime value back to instructions
        self.value_to_instructions(&value)
    }

    fn import_module_internal(&mut self, module_path: &str) -> Result<Value, Error> {
        // Parse the module
        let parsed = self.module_cache.load_and_cache_ast(
            module_path,
            self.module_loader,
            self.module_path.as_ref(),
        )?;

        // Save current compiler state
        let saved_instructions = std::mem::take(&mut self.codegen.instructions);
        let saved_scopes = std::mem::take(&mut self.scopes);
        let saved_type_aliases = self.type_context.type_aliases.clone();

        // Reset to clean state for module compilation
        self.scopes = vec![HashMap::new()];
        self.type_context.type_aliases.clear();

        // Compile the module
        for statement in parsed.statements {
            self.compile_statement(statement)?;
        }

        // Get the compiled module instructions
        let module_instructions = std::mem::take(&mut self.codegen.instructions);

        // Restore original compiler state
        self.codegen.instructions = saved_instructions;
        self.scopes = saved_scopes;
        *self.type_context.type_aliases = saved_type_aliases;

        // Execute the module instructions to get the runtime value
        // If module returns None (no value), default to NIL tuple - this is intentional
        let value = self
            .vm
            .execute_instructions(module_instructions)
            .map_err(|e| Error::ModuleExecution {
                module_path: module_path.to_string(),
                error: e,
            })?
            .unwrap_or(Value::nil());

        Ok(value)
    }

    fn compile_operation(
        &mut self,
        operation: ast::Operation,
        value_type: Type,
        parameter_type: Type,
    ) -> Result<Type, Error> {
        match operation {
            ast::Operation::Tuple(tuple) => {
                self.compile_operation_tuple(tuple.name, tuple.fields, value_type, parameter_type)
            }
            ast::Operation::Block(block) => self.compile_block(block, value_type),
            ast::Operation::FunctionCall(member_access) => self.compile_function_call(
                &member_access.target,
                member_access.accessors,
                value_type,
                parameter_type,
            ),
            ast::Operation::FieldAccess(field) => {
                self.compile_tuple_element_access(TupleAccessor::Field(field), value_type)
            }
            ast::Operation::PositionalAccess(position) => {
                self.compile_tuple_element_access(TupleAccessor::Position(position), value_type)
            }
            ast::Operation::TailCall(identifier) => self.compile_operation_tail_call(&identifier),
            ast::Operation::Equality => self.compile_operation_equality(value_type),
            ast::Operation::Not => self.compile_operation_not(value_type),
            ast::Operation::Match(pattern) => {
                // When match appears as an operation, it matches the piped value
                self.compile_match(pattern, value_type)
            }
        }
    }

    fn compile_tuple_element_access(
        &mut self,
        accessor: TupleAccessor,
        value_type: Type,
    ) -> Result<Type, Error> {
        let tuple_types = value_type.extract_tuple_types();

        if tuple_types.is_empty() {
            match accessor {
                TupleAccessor::Field(field_name) => {
                    return Err(Error::FieldAccessOnNonTuple { field_name });
                }
                TupleAccessor::Position(index) => {
                    return Err(Error::PositionalAccessOnNonTuple { index });
                }
            }
        }

        let (index, field_types) = match accessor {
            TupleAccessor::Field(field_name) => {
                let results = self.get_field_types_by_name(&tuple_types, &field_name)?;
                if results.is_empty() {
                    return Err(Error::FieldAccessOnNonTuple { field_name });
                }
                let index = results[0].0;
                let field_types: Vec<Type> = results.into_iter().map(|(_, t)| t).collect();
                (index, field_types)
            }
            TupleAccessor::Position(position) => {
                let field_types = self
                    .get_field_types_at_position(&tuple_types, position)
                    .map_err(|_| Error::PositionalAccessOnNonTuple { index: position })?;
                (position, field_types)
            }
        };

        self.codegen.add_instruction(Instruction::Get(index));
        Ok(Type::from_types(field_types))
    }

    fn compile_operation_tail_call(&mut self, identifier: &str) -> Result<Type, Error> {
        if identifier.is_empty() {
            self.codegen.add_instruction(Instruction::TailCall(true));
            Ok(Type::Union(vec![]))
        } else {
            self.codegen
                .add_instruction(Instruction::Load(identifier.to_string()));
            self.codegen.add_instruction(Instruction::TailCall(false));

            match self.lookup_variable(identifier) {
                Some(Type::Callable(func_type)) => Ok(func_type.result),
                Some(other_type) => Err(Error::TypeMismatch {
                    expected: "function".to_string(),
                    found: format!("{:?}", other_type),
                }),
                None => Err(Error::VariableUndefined(identifier.to_string())),
            }
        }
    }

    fn compile_value_builtin(&mut self, name: &str) -> Result<Type, Error> {
        let (param_type, result_type) = BUILTIN_REGISTRY
            .resolve_signature(name, self.vm)
            .ok_or_else(|| Error::BuiltinUndefined(name.to_string()))?;

        let builtin_index = self.vm.register_builtin(name.to_string());

        self.codegen
            .add_instruction(Instruction::Builtin(builtin_index));

        Ok(Type::Callable(Box::new(crate::types::CallableType {
            parameter: param_type,
            result: result_type,
        })))
    }

    fn compile_operation_equality(&mut self, value_type: Type) -> Result<Type, Error> {
        // The == operator works with a tuple on the stack
        // We need to extract the tuple elements and call Equal(count)

        // Extract tuple type IDs from the value type
        let tuple_types = value_type.extract_tuple_types();

        if tuple_types.is_empty() {
            return Err(Error::TypeMismatch {
                expected: "tuple".to_string(),
                found: "unknown".to_string(),
            });
        }

        // Verify all tuples have the same field count and collect field types
        let mut common_field_count = None;
        let mut first_field_types = Vec::new();

        for type_id in &tuple_types {
            let (_, fields) = self
                .vm
                .lookup_type(&type_id)
                .ok_or_else(|| Error::TypeNotInRegistry { type_id: *type_id })?;

            let field_count = fields.len();

            if field_count == 0 {
                return Err(Error::TypeMismatch {
                    expected: "non-empty tuple".to_string(),
                    found: "empty tuple".to_string(),
                });
            }

            if let Some(prev_count) = common_field_count {
                if prev_count != field_count {
                    return Err(Error::TypeMismatch {
                        expected: format!("tuple with {} fields", prev_count),
                        found: format!("tuple with {} fields", field_count),
                    });
                }
            } else {
                common_field_count = Some(field_count);
                // Collect first field types for return type
                if let Some((_, first_field_type)) = fields.get(0) {
                    first_field_types.push(first_field_type.clone());
                }
            }
        }

        if let Some(field_count) = common_field_count {
            // Extract tuple fields and push them individually to the stack
            // Following the established pattern from compile_operator
            for i in 0..field_count {
                if i < field_count - 1 {
                    self.codegen.add_instruction(Instruction::Duplicate);
                }
                self.codegen.add_instruction(Instruction::Get(i));
                if i < field_count - 1 {
                    self.codegen.add_instruction(Instruction::Swap);
                }
            }

            // Now compare the field_count individual values on the stack
            self.codegen
                .add_instruction(Instruction::Equal(field_count));

            // Return type is union of field types and NIL
            let mut result_types = first_field_types;
            result_types.push(Type::nil());
            Ok(Type::from_types(result_types))
        } else {
            // For unresolved types or non-tuple types, we can't know the field count at compile time
            // This would require runtime inspection - for now, return an error
            Err(Error::TypeMismatch {
                expected: "tuple".to_string(),
                found: "unknown".to_string(),
            })
        }
    }

    fn compile_operation_not(&mut self, _value_type: Type) -> Result<Type, Error> {
        // The ! operator works with any value on the stack
        // It converts [] to Ok and everything else to []
        self.codegen.add_instruction(Instruction::Not);

        // The Not instruction returns either Ok or NIL
        Ok(Type::from_types(vec![Type::ok(), Type::nil()]))
    }

    fn compile_target_access(&mut self, target: &str) -> Result<Type, Error> {
        if let Some(var_type) = self.lookup_variable(target) {
            self.codegen
                .add_instruction(Instruction::Load(target.to_string()));
            return Ok(var_type);
        }

        if let Some(parameter_fields) = self.parameter_fields_stack.last() {
            if let Some(&(field_index, ref field_type)) = parameter_fields.get(target) {
                let field_index_copy = field_index;
                let field_type_copy: Type = field_type.clone();
                self.codegen.add_instruction(Instruction::Parameter);
                self.codegen
                    .add_instruction(Instruction::Get(field_index_copy));
                return Ok(field_type_copy);
            }
        }

        Err(Error::VariableUndefined(target.to_string()))
    }

    fn compile_accessor_chain(
        &mut self,
        mut last_type: Type,
        accessors: Vec<ast::AccessPath>,
        target_name: &str,
    ) -> Result<Type, Error> {
        for accessor in accessors {
            let tuple_types = last_type.extract_tuple_types();

            if tuple_types.is_empty() {
                return Err(Error::MemberAccessOnNonTuple {
                    target: target_name.to_string(),
                });
            }

            let (index, field_types) = match accessor {
                ast::AccessPath::Field(field_name) => {
                    let results = self
                        .get_field_types_by_name(&tuple_types, &field_name)
                        .map_err(|_| Error::MemberFieldNotFound {
                            field_name: field_name.clone(),
                            target: target_name.to_string(),
                        })?;
                    if results.is_empty() {
                        return Err(Error::MemberFieldNotFound {
                            field_name,
                            target: target_name.to_string(),
                        });
                    }
                    let index = results[0].0;
                    let field_types: Vec<Type> = results.into_iter().map(|(_, t)| t).collect();
                    (index, field_types)
                }
                ast::AccessPath::Index(index) => {
                    let field_types = self
                        .get_field_types_at_position(&tuple_types, index)
                        .map_err(|_| Error::MemberAccessOnNonTuple {
                            target: target_name.to_string(),
                        })?;
                    (index, field_types)
                }
            };

            self.codegen.add_instruction(Instruction::Get(index));
            last_type = Type::from_types(field_types);
        }

        Ok(last_type)
    }

    fn compile_member_access_chain(
        &mut self,
        target: &str,
        accessors: Vec<ast::AccessPath>,
        emit_load: bool,
    ) -> Result<Type, Error> {
        let last_type = if emit_load {
            self.codegen
                .add_instruction(Instruction::Load(target.to_string()));
            self.lookup_variable(target)
                .ok_or(Error::VariableUndefined(target.to_string()))?
        } else {
            self.compile_target_access(target)?
        };

        self.compile_accessor_chain(last_type, accessors, target)
    }

    fn compile_value_member_access(
        &mut self,
        target: &ast::MemberTarget,
        accessors: Vec<ast::AccessPath>,
        parameter_type: Type,
    ) -> Result<Type, Error> {
        match target {
            ast::MemberTarget::Identifier(name) => {
                self.compile_member_access_chain(name, accessors, false)
            }
            ast::MemberTarget::Parameter => {
                self.codegen.add_instruction(Instruction::Parameter);
                self.compile_accessor_chain(parameter_type, accessors, "$")
            }
            ast::MemberTarget::Builtin(name) => {
                // Builtins can't be accessed via member access (only called)
                return Err(Error::FeatureUnsupported(format!(
                    "Builtin '{}' cannot be used in member access context",
                    name
                )));
            }
        }
    }

    fn compile_function_call(
        &mut self,
        target: &ast::MemberTarget,
        accessors: Vec<ast::AccessPath>,
        value_type: Type,
        parameter_type: Type,
    ) -> Result<Type, Error> {
        let function_type = match target {
            ast::MemberTarget::Identifier(name) => {
                self.compile_member_access_chain(name, accessors, true)?
            }
            ast::MemberTarget::Parameter => {
                self.codegen.add_instruction(Instruction::Parameter);
                self.compile_accessor_chain(parameter_type, accessors, "$")?
            }
            ast::MemberTarget::Builtin(name) => {
                // Compile the builtin as a value (which returns its function type)
                let builtin_type = self.compile_value_builtin(name)?;
                // Builtins don't support accessor chains
                if !accessors.is_empty() {
                    return Err(Error::FeatureUnsupported(
                        "Accessor chains on builtins are not supported".to_string(),
                    ));
                }
                builtin_type
            }
        };

        let func_type = match function_type {
            Type::Callable(func_type) => func_type,
            _ => {
                return Err(Error::TypeMismatch {
                    expected: "function".to_string(),
                    found: format!("{:?}", function_type),
                });
            }
        };

        if !value_type.is_compatible(&func_type.parameter, self.vm.type_registry()) {
            return Err(Error::TypeMismatch {
                expected: format!(
                    "function parameter compatible with {:?}",
                    func_type.parameter
                ),
                found: format!("{:?}", value_type),
            });
        }

        self.codegen.add_instruction(Instruction::Call);

        Ok(func_type.result)
    }

    fn define_variable(&mut self, name: &str, var_type: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), var_type);
        }
    }

    fn function_to_type(&self, function_index: usize) -> Result<Type, Error> {
        if let Some(func_def) = self.vm.get_functions().get(function_index) {
            if let Some(func_type) = &func_def.function_type {
                Ok(Type::Callable(Box::new(func_type.clone())))
            } else {
                Err(Error::FunctionUndefined(function_index))
            }
        } else {
            Err(Error::FunctionUndefined(function_index))
        }
    }

    fn value_to_type(&mut self, value: &Value) -> Result<Type, Error> {
        match value {
            Value::Integer(_) => Ok(Type::Integer),
            Value::Binary(_) => Ok(Type::Binary),
            Value::Tuple(type_id, _) => Ok(Type::Tuple(*type_id)),
            Value::Function { function, .. } => self.function_to_type(*function),
            Value::Builtin(name) => {
                if let Some((param_type, result_type)) =
                    BUILTIN_REGISTRY.resolve_signature(name, self.vm)
                {
                    Ok(Type::Callable(Box::new(CallableType {
                        parameter: param_type,
                        result: result_type,
                    })))
                } else {
                    Err(Error::BuiltinUndefined(name.clone()))
                }
            }
        }
    }

    fn lookup_variable(&self, name: &str) -> Option<Type> {
        self.lookup_variable_in_scopes(&self.scopes, name)
    }

    fn lookup_variable_in_scopes(
        &self,
        scopes: &[HashMap<String, Type>],
        name: &str,
    ) -> Option<Type> {
        for scope in scopes.iter().rev() {
            if let Some(variable_type) = scope.get(name) {
                return Some(variable_type.clone());
            }
        }
        None
    }

    fn check_field_name_duplicates<T>(
        fields: &[T],
        get_name: impl Fn(&T) -> Option<&String>,
    ) -> Result<(), Error> {
        let mut seen_names = HashSet::new();
        for field in fields {
            if let Some(field_name) = get_name(field) {
                if !seen_names.insert(field_name.clone()) {
                    return Err(Error::FieldDuplicated(field_name.clone()));
                }
            }
        }
        Ok(())
    }

    /// Get field type at the given position for all tuple types in the list
    /// Returns error if any tuple doesn't have the field or if tuples have different structures
    fn get_field_types_at_position(
        &self,
        tuple_types: &[TypeId],
        position: usize,
    ) -> Result<Vec<Type>, Error> {
        let mut field_types = Vec::new();

        for type_id in tuple_types {
            let (_, fields) = self
                .vm
                .lookup_type(type_id)
                .ok_or_else(|| Error::TypeNotInRegistry { type_id: *type_id })?;

            if let Some((_, field_type)) = fields.get(position) {
                field_types.push(field_type.clone());
            } else {
                return Err(Error::ParameterIndexOutOfBounds { index: position });
            }
        }

        Ok(field_types)
    }

    /// Get field type by name for all tuple types in the list
    /// Returns error if any tuple doesn't have the field
    fn get_field_types_by_name(
        &self,
        tuple_types: &[TypeId],
        field_name: &str,
    ) -> Result<Vec<(usize, Type)>, Error> {
        let mut results = Vec::new();
        let mut common_index = None;

        for type_id in tuple_types {
            let (index, field_type) = self
                .type_context
                .get_tuple_field_type_by_name(type_id, field_name, self.vm)?;

            // Verify all tuples have the field at the same index
            if let Some(prev_index) = common_index {
                if prev_index != index {
                    return Err(Error::MemberFieldNotFound {
                        field_name: field_name.to_string(),
                        target: "multiple tuple types".to_string(),
                    });
                }
            } else {
                common_index = Some(index);
            }

            results.push((index, field_type));
        }

        Ok(results)
    }
}
