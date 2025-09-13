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
pub use pattern::PatternCompiler;
pub use typing::{TupleAccessor, TypeSet, narrow_types};

use crate::{
    ast,
    builtins::BUILTIN_REGISTRY,
    bytecode::{Constant, Function, Instruction, TypeId},
    modules::{ModuleError, ModuleLoader},
    types::{FunctionType, Type},
    vm::{BinaryRef, VM, Value},
};

use typing::TypeContext;

#[derive(Debug, PartialEq)]
pub enum Error {
    // Variable & scope errors
    VariableUndefined(String),

    // Builtin errors
    BuiltinUndefined(String),

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
    ModuleParse(String),
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
    type_context: TypeContext,
    module_cache: ModuleCache,

    // State management
    scopes: Vec<HashMap<String, TypeSet>>,
    vm: &'a mut VM,
    module_loader: &'a dyn ModuleLoader,
    module_path: Option<PathBuf>,

    // Parameter field tracking for nested function definitions
    parameter_fields_stack: Vec<HashMap<String, (usize, TypeSet)>>,
}

impl<'a> Compiler<'a> {
    pub fn compile(
        program: ast::Program,
        type_aliases: &'a mut HashMap<String, Vec<Type>>,
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
        };

        for (name, value) in existing_variables {
            let var_type = compiler.value_to_type(&value);
            compiler.define_variable(&name, var_type);
        }

        // Phase 1: Pre-register all type aliases as placeholders
        for statement in &program.statements {
            if let ast::Statement::TypeAlias { name, .. } = statement {
                // Register placeholder for forward references
                compiler.type_context.type_aliases.insert(
                    name.clone(),
                    TypeSet(vec![]), // Empty placeholder
                );
            }
        }

        // Phase 2: Compile all statements (now forward references work)
        for statement in program.statements {
            compiler.compile_statement(statement)?;
        }

        // Save type aliases back to the provided HashMap
        for (name, type_set) in compiler.type_context.type_aliases.iter() {
            type_aliases.insert(name.clone(), type_set.to_vec());
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
                self.compile_expression(expression, TypeSet::resolved(Type::Tuple(TypeId::NIL)))?;
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

    fn compile_literal(&mut self, literal: ast::Literal) -> Result<TypeSet, Error> {
        match literal {
            ast::Literal::Integer(integer) => {
                let index = self.vm.register_constant(Constant::Integer(integer));
                self.codegen.add_instruction(Instruction::Constant(index));
                Ok(TypeSet::resolved(Type::Integer))
            }
            ast::Literal::Binary(bytes) => {
                // TODO: is clone bad?
                let index = self.vm.register_constant(Constant::Binary(bytes.clone()));
                self.codegen.add_instruction(Instruction::Constant(index));
                Ok(TypeSet::resolved(Type::Binary))
            }
            ast::Literal::String(string) => {
                let bytes = string.as_bytes().to_vec();
                let index = self.vm.register_constant(Constant::Binary(bytes));
                self.codegen.add_instruction(Instruction::Constant(index));
                Ok(TypeSet::resolved(Type::Binary))
            }
        }
    }

    fn compile_value_tuple(
        &mut self,
        tuple_name: Option<String>,
        fields: Vec<ast::ValueTupleField>,
        parameter_type: TypeSet,
    ) -> Result<TypeSet, Error> {
        Self::check_field_name_duplicates(&fields, |f| f.name.as_ref())?;

        let mut field_types = Vec::new();
        for field in &fields {
            let field_type = self.compile_chain(field.value.clone(), parameter_type.clone())?;
            if let Some(resolved_type) = field_type.single() {
                field_types.push((field.name.clone(), resolved_type.clone()));
            } else {
                return Err(Error::TupleFieldTypeUnresolved {
                    field_index: field_types.len(),
                });
            }
        }

        let type_id = self.vm.register_type(tuple_name, field_types);
        self.codegen.add_instruction(Instruction::Tuple(type_id));
        Ok(TypeSet::resolved(Type::Tuple(type_id)))
    }

    fn compile_operation_tuple(
        &mut self,
        name: Option<String>,
        fields: Vec<ast::OperationTupleField>,
        value_type: TypeSet,
        parameter_type: TypeSet,
    ) -> Result<TypeSet, Error> {
        let has_ripple = fields
            .iter()
            .any(|field| matches!(field.value, ast::OperationTupleFieldValue::Ripple));

        if !has_ripple {
            return Err(Error::ChainValueUnused);
        }

        self.codegen
            .add_instruction(Instruction::Store("~".to_string()));

        Self::check_field_name_duplicates(&fields, |f| f.name.as_ref())?;

        let mut field_types = Vec::new();
        for field in &fields {
            let field_type = match &field.value {
                ast::OperationTupleFieldValue::Ripple => {
                    // TODO: avoid using variable?
                    self.codegen
                        .add_instruction(Instruction::Load("~".to_string()));
                    value_type.clone()
                }
                ast::OperationTupleFieldValue::Chain(chain) => {
                    self.compile_chain(chain.clone(), parameter_type.clone())?
                }
            };

            if let Some(resolved_type) = field_type.single() {
                field_types.push((field.name.clone(), resolved_type.clone()));
            } else {
                return Err(Error::TupleFieldTypeUnresolved {
                    field_index: field_types.len(),
                });
            }
        }

        let type_id = self.vm.register_type(name, field_types);
        self.codegen.add_instruction(Instruction::Tuple(type_id));
        Ok(TypeSet::resolved(Type::Tuple(type_id)))
    }

    fn compile_function_definition(
        &mut self,
        function_definition: ast::FunctionDefinition,
    ) -> Result<TypeSet, Error> {
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
            None => TypeSet::resolved(Type::Tuple(TypeId::NIL)),
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

        let param_types = parameter_type.to_vec();
        let result_types = body_type.to_vec();

        if param_types.is_empty() {
            return Err(Error::TypeUnresolved(
                "Function parameter type cannot be resolved".to_string(),
            ));
        }
        if result_types.is_empty() {
            return Err(Error::TypeUnresolved(
                "Function body type cannot be resolved".to_string(),
            ));
        }

        let func_type = FunctionType {
            parameter: param_types,
            result: result_types,
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

        let function_type = Type::Function(Box::new(func_type));

        Ok(TypeSet::resolved(function_type))
    }

    fn compile_assigment(
        &mut self,
        pattern: ast::Pattern,
        value: ast::Chain,
        parameter_type: TypeSet,
    ) -> Result<TypeSet, Error> {
        let value_type = self.compile_chain(value, parameter_type)?;

        // Duplicate the value on the stack for pattern matching
        self.codegen.add_instruction(Instruction::Duplicate);

        // Try to match the pattern against the value
        let match_success_addr = self.codegen.emit_jump_placeholder();

        // Pattern matching - if it fails, we jump here (to cleanup section)
        let cleanup_jump_addr = self.codegen.emit_jump_placeholder();

        // Pattern matching success - if it succeeds, we jump here
        self.codegen.patch_jump_to_here(match_success_addr);

        // Attempt pattern matching
        match self.compile_pattern_match(&pattern, &value_type, cleanup_jump_addr)? {
            None => {
                // Pattern definitely won't match - cleanup directly
                self.codegen.emit_pattern_match_cleanup(0);
                return Ok(TypeSet::resolved(Type::Tuple(TypeId::NIL)));
            }
            Some(pending_assignments) => {
                // Pattern can match - generate normal assignment code

                // Pattern matched successfully - commit all variable assignments
                self.codegen
                    .emit_pattern_match_success(&pending_assignments);
                for (name, var_type) in &pending_assignments {
                    self.define_variable(name, var_type.clone());
                }

                // Jump to end
                let success_end_jump_addr = self.codegen.emit_jump_placeholder();

                // Cleanup section - if pattern matching failed, we come here
                self.codegen.patch_jump_to_here(cleanup_jump_addr);
                self.codegen
                    .emit_pattern_match_cleanup(pending_assignments.len());

                // End of assignment
                self.codegen.patch_jump_to_here(success_end_jump_addr);
            }
        }

        Ok(TypeSet(vec![
            Type::Tuple(TypeId::OK),
            Type::Tuple(TypeId::NIL),
        ]))
    }

    fn compile_pattern_match(
        &mut self,
        pattern: &ast::Pattern,
        value_type: &TypeSet,
        fail_addr: usize,
    ) -> Result<Option<Vec<(String, TypeSet)>>, Error> {
        let mut pattern_compiler =
            PatternCompiler::new(&mut self.codegen, &mut self.type_context, self.vm);
        pattern_compiler.compile_pattern_match(pattern, value_type, fail_addr)
    }

    fn compile_block(
        &mut self,
        block: ast::Block,
        parameter_type: TypeSet,
    ) -> Result<TypeSet, Error> {
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
            if condition_type.single() == Some(&Type::Tuple(TypeId::NIL)) {
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

        narrow_types(branch_types)
    }

    fn compile_expression(
        &mut self,
        expression: ast::Expression,
        parameter_type: TypeSet,
    ) -> Result<TypeSet, Error> {
        let mut last_type = None;
        let mut end_jumps = Vec::new();

        for (i, term) in expression.terms.iter().enumerate() {
            last_type = Some(match term {
                ast::Term::Assignment { pattern, value } => {
                    self.compile_assigment(pattern.clone(), value.clone(), parameter_type.clone())
                }
                ast::Term::Chain(chain) => {
                    self.compile_chain(chain.clone(), parameter_type.clone())
                }
            }?);

            // If last_type is NIL, subsequent terms are unreachable - break early
            if let Some(ref last_type) = last_type {
                if last_type.single() == Some(&Type::Tuple(TypeId::NIL)) {
                    break;
                }
            }

            if i < expression.terms.len() - 1 {
                let end_jump = self.codegen.emit_duplicate_jump_if_nil_pop();
                end_jumps.push(end_jump);
            }
        }

        let end_addr = self.codegen.instructions.len();
        for jump_addr in end_jumps {
            self.codegen.patch_jump_to_addr(jump_addr, end_addr);
        }

        last_type.ok_or_else(|| Error::InternalError {
            message: "Expression compiled with no terms".to_string(),
        })
    }

    fn compile_parameter(
        &mut self,
        parameter: ast::Parameter,
        parameter_type: TypeSet,
    ) -> Result<TypeSet, Error> {
        match parameter {
            ast::Parameter::Self_ => {
                self.codegen.add_instruction(Instruction::Parameter);
                Ok(parameter_type)
            }
            ast::Parameter::Indexed(index) => {
                self.codegen.add_instruction(Instruction::Parameter);
                self.codegen.add_instruction(Instruction::Get(index));

                self.validate_tuple_field_access_single(&parameter_type, index)
            }
        }
    }

    fn compile_chain(
        &mut self,
        chain: ast::Chain,
        parameter_type: TypeSet,
    ) -> Result<TypeSet, Error> {
        let mut value_type = match chain.value {
            ast::Value::Literal(literal) => self.compile_literal(literal),
            ast::Value::Tuple(tuple) => {
                self.compile_value_tuple(tuple.name, tuple.fields, parameter_type.clone())
            }
            ast::Value::FunctionDefinition(function_definition) => {
                self.compile_function_definition(function_definition)
            }
            ast::Value::Block(block) => {
                self.codegen
                    .add_instruction(Instruction::Tuple(TypeId::NIL));
                self.compile_block(block, TypeSet::resolved(Type::Tuple(TypeId::NIL)))
            }
            ast::Value::Parameter(parameter) => {
                self.compile_parameter(parameter, parameter_type.clone())
            }
            ast::Value::MemberAccess(member_access) => {
                self.compile_value_member_access(&member_access.target, member_access.accessors)
            }
            ast::Value::Import(path) => self.compile_value_import(&path),
        }?;

        for operation in chain.operations {
            value_type = self.compile_operation(operation, value_type, parameter_type.clone())?;
        }

        Ok(value_type)
    }

    fn value_to_instructions(&mut self, value: &Value) -> Result<TypeSet, Error> {
        match value {
            Value::Integer(int_value) => {
                let index = self.vm.register_constant(Constant::Integer(*int_value));
                self.codegen.add_instruction(Instruction::Constant(index));
                Ok(TypeSet::resolved(Type::Integer))
            }
            Value::Binary(binary_ref) => {
                match binary_ref {
                    BinaryRef::Constant(const_index) => {
                        self.codegen
                            .add_instruction(Instruction::Constant(*const_index));
                    }
                    BinaryRef::Heap(_) => {
                        // Heap binaries need to be handled differently
                        // For now, we'll create a constant from the heap binary
                        if let Ok(bytes) = self.vm.get_binary_bytes(binary_ref) {
                            let constant = crate::bytecode::Constant::Binary(bytes.to_vec());
                            let index = self.vm.register_constant(constant);
                            self.codegen.add_instruction(Instruction::Constant(index));
                        } else {
                            return Err(Error::TypeMismatch {
                                expected: "valid binary".to_string(),
                                found: "invalid binary reference".to_string(),
                            });
                        }
                    }
                }
                Ok(TypeSet::resolved(Type::Binary))
            }
            Value::Tuple(type_id, fields) => {
                for field in fields {
                    self.value_to_instructions(field)?;
                }
                self.codegen.add_instruction(Instruction::Tuple(*type_id));
                Ok(TypeSet::resolved(Type::Tuple(*type_id)))
            }
            Value::Function { function, captures } => {
                for capture in captures {
                    self.value_to_instructions(capture)?;
                }
                if let Some(func_def) = self.vm.get_functions().get(*function).cloned() {
                    let func_index = self.vm.register_function(func_def);
                    self.codegen
                        .add_instruction(Instruction::Function(func_index));
                    Ok(TypeSet::resolved(self.function_to_type(func_index)))
                } else {
                    Err(Error::FeatureUnsupported(
                        "Invalid function reference".to_string(),
                    ))
                }
            }
        }
    }

    fn compile_value_import(&mut self, module_path: &str) -> Result<TypeSet, Error> {
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
        if let Some(cached_value) = self.module_cache.evaluation_cache.get(module_path).cloned() {
            return self.value_to_instructions(&cached_value);
        }

        // Add to import stack to track circular imports
        self.module_cache.import_stack.push(module_path.to_string());
        let result = self.import_module_internal(module_path);
        self.module_cache.import_stack.pop();

        result
    }

    fn import_module_internal(&mut self, module_path: &str) -> Result<TypeSet, Error> {
        // Parse the module
        let parsed = self.module_cache.load_and_cache_ast(
            module_path,
            self.module_loader,
            self.module_path.as_ref(),
        )?;

        // Save current compiler state
        let saved_instructions = std::mem::take(&mut self.codegen.instructions);
        let saved_scopes = std::mem::take(&mut self.scopes);
        let saved_type_aliases = std::mem::take(&mut self.type_context.type_aliases);

        // Reset to clean state for module compilation
        self.scopes = vec![HashMap::new()];
        self.type_context.type_aliases = HashMap::new();

        // Compile the module
        for statement in parsed.statements {
            self.compile_statement(statement)?;
        }

        // Get the compiled module instructions
        let module_instructions = std::mem::take(&mut self.codegen.instructions);

        // Restore original compiler state
        self.codegen.instructions = saved_instructions;
        self.scopes = saved_scopes;
        self.type_context.type_aliases = saved_type_aliases;

        // Execute the module instructions to get the runtime value
        // If module returns None (no value), default to NIL tuple - this is intentional
        let value = self
            .vm
            .execute_instructions(module_instructions)
            .map_err(|_e| Error::FeatureUnsupported("Module execution failed".to_string()))?
            .unwrap_or(Value::Tuple(TypeId::NIL, vec![]));

        // Cache the evaluated module
        self.module_cache
            .evaluation_cache
            .insert(module_path.to_string(), value.clone());

        // Convert the runtime value back to instructions
        self.value_to_instructions(&value)
    }

    fn compile_operation(
        &mut self,
        operation: ast::Operation,
        value_type: TypeSet,
        parameter_type: TypeSet,
    ) -> Result<TypeSet, Error> {
        match operation {
            ast::Operation::Tuple(tuple) => {
                self.compile_operation_tuple(tuple.name, tuple.fields, value_type, parameter_type)
            }
            ast::Operation::Block(block) => self.compile_block(block, value_type),
            ast::Operation::MemberAccess(member_access) => self.compile_operation_member_access(
                &member_access.target,
                member_access.accessors,
                value_type,
            ),
            ast::Operation::FieldAccess(field) => {
                self.compile_tuple_element_access(TupleAccessor::Field(field), value_type)
            }
            ast::Operation::PositionalAccess(position) => {
                self.compile_tuple_element_access(TupleAccessor::Position(position), value_type)
            }
            ast::Operation::TailCall(identifier) => self.compile_operation_tail_call(&identifier),
            ast::Operation::Parameter(parameter) => {
                self.compile_operation_parameter(parameter, value_type, parameter_type)
            }
            ast::Operation::Builtin(name) => self.compile_operation_builtin(&name, value_type),
            ast::Operation::Equality => self.compile_operation_equality(value_type),
            ast::Operation::Not => self.compile_operation_not(value_type),
        }
    }

    fn compile_tuple_element_access(
        &mut self,
        accessor: TupleAccessor,
        value_type: TypeSet,
    ) -> Result<TypeSet, Error> {
        if let Some(Type::Tuple(type_id)) = value_type.single() {
            let (index, result_type) = match accessor {
                TupleAccessor::Field(field_name) => self
                    .type_context
                    .get_tuple_field_type_by_name(&type_id, &field_name, self.vm)?,
                TupleAccessor::Position(position) => {
                    let field_type = self
                        .type_context
                        .get_tuple_field_type_by_position(&type_id, position, self.vm)?;
                    (position, field_type)
                }
            };
            self.codegen.add_instruction(Instruction::Get(index));
            Ok(TypeSet::resolved(result_type))
        } else {
            match accessor {
                TupleAccessor::Field(field_name) => {
                    Err(Error::FieldAccessOnNonTuple { field_name })
                }
                TupleAccessor::Position(index) => Err(Error::PositionalAccessOnNonTuple { index }),
            }
        }
    }

    fn compile_operation_tail_call(&mut self, identifier: &str) -> Result<TypeSet, Error> {
        if identifier.is_empty() {
            self.codegen.add_instruction(Instruction::TailCall(true));
            // Recursive tail call - the control flow doesn't return here, so the type is NIL
            // TODO: Ideally we'd return the current function's return type, but that would
            // require tracking more context during compilation
            Ok(TypeSet::resolved(Type::Tuple(TypeId::NIL)))
        } else {
            self.codegen
                .add_instruction(Instruction::Load(identifier.to_string()));
            self.codegen.add_instruction(Instruction::TailCall(false));

            match self.lookup_variable(identifier) {
                Some(function_type) => Ok(function_type),
                None => Err(Error::VariableUndefined(identifier.to_string())),
            }
        }
    }

    fn compile_operation_builtin(
        &mut self,
        name: &str,
        _value_type: TypeSet,
    ) -> Result<TypeSet, Error> {
        // Check if the builtin function exists by getting its signature
        let (_, result_types) = BUILTIN_REGISTRY
            .get_signature(name)
            .ok_or_else(|| Error::BuiltinUndefined(name.to_string()))?;

        // Register the builtin in the bytecode
        let builtin_index = self.vm.register_builtin(name.to_string());

        // Generate the builtin instruction (which will execute immediately)
        self.codegen
            .add_instruction(Instruction::Builtin(builtin_index));

        // Return the result type
        narrow_types(
            result_types
                .into_iter()
                .map(|t| TypeSet::resolved(t))
                .collect(),
        )
    }

    fn compile_operation_equality(&mut self, value_type: TypeSet) -> Result<TypeSet, Error> {
        // The == operator works with a tuple on the stack
        // We need to extract the tuple elements and call Equal(count)
        if let Some(Type::Tuple(type_id)) = value_type.single() {
            // Get the number of fields in this tuple type
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

            // Return type is union of field type and NIL
            if let Some((_, first_field_type)) = fields.get(0) {
                Ok(TypeSet(vec![
                    first_field_type.clone(),
                    Type::Tuple(TypeId::NIL),
                ]))
            } else {
                Ok(TypeSet::resolved(Type::Tuple(TypeId::NIL)))
            }
        } else {
            // For unresolved types or non-tuple types, we can't know the field count at compile time
            // This would require runtime inspection - for now, return an error
            Err(Error::TypeMismatch {
                expected: "tuple".to_string(),
                found: "unknown".to_string(),
            })
        }
    }

    fn compile_operation_not(&mut self, _value_type: TypeSet) -> Result<TypeSet, Error> {
        // The ! operator works with any value on the stack
        // It converts [] to Ok and everything else to []
        self.codegen.add_instruction(Instruction::Not);

        // The Not instruction returns either Ok or NIL
        Ok(TypeSet(vec![
            Type::Tuple(TypeId::OK),
            Type::Tuple(TypeId::NIL),
        ]))
    }

    fn compile_operation_parameter(
        &mut self,
        parameter: ast::Parameter,
        value_type: TypeSet,
        parameter_type: TypeSet,
    ) -> Result<TypeSet, Error> {
        // Get the operation type from the parameter (could be function or other callable)
        let function_type = match parameter {
            ast::Parameter::Self_ => {
                self.codegen.add_instruction(Instruction::Parameter);
                parameter_type
            }
            ast::Parameter::Indexed(index) => {
                self.codegen.add_instruction(Instruction::Parameter);
                self.codegen.add_instruction(Instruction::Get(index));

                self.validate_tuple_field_access_multiple(&parameter_type, index)?
            }
        };

        // Use the unified function call handler
        self.compile_function_call(function_type, value_type)
    }

    fn compile_target_access(&mut self, target: &str) -> Result<TypeSet, Error> {
        if let Some(var_type) = self.lookup_variable(target) {
            self.codegen
                .add_instruction(Instruction::Load(target.to_string()));
            return Ok(var_type);
        }

        if let Some(parameter_fields) = self.parameter_fields_stack.last() {
            if let Some(&(field_index, ref field_type)) = parameter_fields.get(target) {
                let field_index_copy = field_index;
                let field_type_copy: TypeSet = field_type.clone();
                self.codegen.add_instruction(Instruction::Parameter);
                self.codegen
                    .add_instruction(Instruction::Get(field_index_copy));
                return Ok(field_type_copy);
            }
        }

        Err(Error::VariableUndefined(target.to_string()))
    }

    fn compile_member_access_chain(
        &mut self,
        target: &str,
        accessors: Vec<ast::AccessPath>,
        emit_load: bool,
    ) -> Result<TypeSet, Error> {
        let mut last_type = if emit_load {
            self.codegen
                .add_instruction(Instruction::Load(target.to_string()));
            self.lookup_variable(target)
                .ok_or(Error::VariableUndefined(target.to_string()))?
        } else {
            self.compile_target_access(target)?
        };

        for accessor in accessors {
            if let Some(Type::Tuple(type_id)) = last_type.single() {
                let (index, result_type) = match accessor {
                    ast::AccessPath::Field(field_name) => self
                        .type_context
                        .get_tuple_field_type_by_name(&type_id, &field_name, self.vm)
                        .map_err(|_| Error::MemberFieldNotFound {
                            field_name: field_name.clone(),
                            target: target.to_string(),
                        })?,
                    ast::AccessPath::Index(index) => {
                        let field_type = self
                            .type_context
                            .get_tuple_field_type_by_position(&type_id, index, self.vm)
                            .map_err(|_| Error::MemberAccessOnNonTuple {
                                target: target.to_string(),
                            })?;
                        (index, field_type)
                    }
                };
                self.codegen.add_instruction(Instruction::Get(index));
                last_type = TypeSet::resolved(result_type);
            } else {
                return Err(Error::MemberAccessOnNonTuple {
                    target: target.to_string(),
                });
            }
        }

        Ok(last_type)
    }

    fn compile_value_member_access(
        &mut self,
        target: &str,
        accessors: Vec<ast::AccessPath>,
    ) -> Result<TypeSet, Error> {
        self.compile_member_access_chain(target, accessors, false)
    }

    fn compile_operation_member_access(
        &mut self,
        target: &str,
        accessors: Vec<ast::AccessPath>,
        value_type: TypeSet,
    ) -> Result<TypeSet, Error> {
        let function_type = self.compile_member_access_chain(target, accessors, true)?;
        // Use the unified function call handler
        self.compile_function_call(function_type, value_type)
    }

    fn compile_function_call(
        &mut self,
        operation_type: TypeSet,
        value_type: TypeSet,
    ) -> Result<TypeSet, Error> {
        let operation_types = operation_type.to_vec();
        let value_types = value_type.to_vec();

        // Separate function types from non-function types
        let mut function_variants = Vec::new();
        let mut non_function_types = Vec::new();

        for op_type in &operation_types {
            match op_type {
                Type::Function(func_type) => function_variants.push(func_type),
                _ => non_function_types.push(op_type),
            }
        }

        // Error if we have any non-function types in what should be callable
        if !non_function_types.is_empty() {
            return Err(Error::ChainValueUnused);
        }

        // Error if we have no function types at all
        if function_variants.is_empty() {
            return Err(Error::ChainValueUnused);
        }

        // Check compatibility and collect return types
        let mut all_return_types = Vec::new();

        for func_type in &function_variants {
            // Check if this function accepts any of the value types
            let is_compatible = func_type.parameter.iter().any(|func_param_type| {
                value_types.iter().any(|value_type| {
                    // Use coinductive compatibility checking for recursive types
                    let mut assumptions = std::collections::HashSet::new();

                    value_type.is_compatible_with(
                        func_param_type,
                        &|type_id| self.vm.lookup_type(type_id).cloned(),
                        &mut assumptions,
                    )
                })
            });

            if !is_compatible {
                return Err(Error::TypeMismatch {
                    expected: format!("function parameter compatible with {:?}", value_types),
                    found: format!("function parameter {:?}", func_type.parameter),
                });
            }

            // Collect return types from this compatible function
            all_return_types.extend(
                func_type
                    .result
                    .iter()
                    .map(|t| TypeSet::resolved(t.clone())),
            );
        }

        // Generate the call instruction
        self.codegen.add_instruction(Instruction::Call);

        // Combine return types using narrow_types
        narrow_types(all_return_types)
    }

    fn define_variable(&mut self, name: &str, var_type: TypeSet) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), var_type);
        }
    }

    fn function_to_type(&self, function_index: usize) -> Type {
        if let Some(func_def) = self.vm.get_functions().get(function_index) {
            if let Some(func_type) = &func_def.function_type {
                Type::Function(Box::new(func_type.clone()))
            } else {
                // Fallback if no type information is available
                Type::Function(Box::new(FunctionType {
                    parameter: vec![Type::Tuple(TypeId::NIL)],
                    result: vec![Type::Tuple(TypeId::NIL)],
                }))
            }
        } else {
            Type::Tuple(TypeId::NIL)
        }
    }

    fn value_to_type(&self, value: &Value) -> TypeSet {
        match value {
            Value::Integer(_) => TypeSet::resolved(Type::Integer),
            Value::Binary(_) => TypeSet::resolved(Type::Binary),
            Value::Tuple(type_id, _) => TypeSet::resolved(Type::Tuple(*type_id)),
            Value::Function { function, .. } => TypeSet::resolved(self.function_to_type(*function)),
        }
    }

    fn lookup_variable(&self, name: &str) -> Option<TypeSet> {
        self.lookup_variable_in_scopes(&self.scopes, name)
    }

    fn lookup_variable_in_scopes(
        &self,
        scopes: &[HashMap<String, TypeSet>],
        name: &str,
    ) -> Option<TypeSet> {
        for scope in scopes.iter().rev() {
            if let Some(variable_type) = scope.get(name) {
                return Some(variable_type.clone());
            }
        }
        None
    }

    fn validate_tuple_field_access_single(
        &self,
        tuple_type: &TypeSet,
        index: usize,
    ) -> Result<TypeSet, Error> {
        if let Some(Type::Tuple(type_id)) = tuple_type.single() {
            let type_info = self
                .vm
                .lookup_type(type_id)
                .ok_or_else(|| Error::TypeNotInRegistry { type_id: *type_id })?;

            if let Some(field) = type_info.1.get(index) {
                Ok(TypeSet::resolved(field.1.clone()))
            } else {
                Err(Error::ParameterIndexOutOfBounds { index })
            }
        } else {
            Err(Error::ParameterAccessOnNonTuple { index })
        }
    }

    fn validate_tuple_field_access_multiple(
        &self,
        parameter_type: &TypeSet,
        index: usize,
    ) -> Result<TypeSet, Error> {
        let param_types = parameter_type.to_vec();
        let mut field_type_results = Vec::new();

        for param_type in param_types {
            match param_type {
                Type::Tuple(type_id) => {
                    let type_info = self
                        .vm
                        .lookup_type(&type_id)
                        .ok_or_else(|| Error::TypeNotInRegistry { type_id })?;

                    if let Some(field) = type_info.1.get(index) {
                        field_type_results.push(TypeSet::resolved(field.1.clone()));
                    } else {
                        return Err(Error::ParameterIndexOutOfBounds { index });
                    }
                }
                _ => return Err(Error::ParameterAccessOnNonTuple { index }),
            }
        }

        if field_type_results.is_empty() {
            return Err(Error::ParameterIndexOutOfBounds { index });
        }

        narrow_types(field_type_results)
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
}
