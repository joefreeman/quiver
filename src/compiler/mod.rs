use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

mod free_variables;
mod type_system;
mod codegen;
mod pattern;
mod modules;
mod expression;

pub use type_system::{Type, TupleAccessor, narrow_types};
pub use codegen::InstructionBuilder;
pub use pattern::PatternCompiler;
pub use modules::{ModuleCache, compile_type_import};
pub use expression::ExpressionCompiler;

use crate::{
    ast,
    bytecode::{Constant, Function, Instruction, TypeId},
    modules::{ModuleError, ModuleLoader},
    parser, types, vm,
};

use type_system::TypeContext;

#[derive(Debug)]
pub enum Error {
    // Variable & scope errors
    VariableUndefined(String),

    // Type system errors
    TypeUnresolved(String),
    TypeAliasMissing(String),
    TypeMismatch {
        expected: String,
        found: String,
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
    ParameterTypeNotInRegistry {
        type_id: String,
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
}




pub struct Compiler<'a> {
    // Core components
    codegen: InstructionBuilder,
    type_context: TypeContext<'a>,
    module_cache: ModuleCache,
    
    // State management
    scopes: Vec<HashMap<String, Type>>,
    vm: &'a mut vm::VM,
    module_loader: &'a dyn ModuleLoader,
    module_path: Option<PathBuf>,
    
    // Parameter field tracking for nested function definitions
    parameter_fields_stack: Vec<HashMap<String, (usize, Type)>>,
}

impl<'a> Compiler<'a> {
    pub fn compile(
        program: ast::Program,
        type_registry: &'a mut types::TypeRegistry,
        module_loader: &'a dyn ModuleLoader,
        vm: &'a mut vm::VM,
        module_path: Option<PathBuf>,
    ) -> Result<Vec<Instruction>, Error> {
        // Initialize compiler scope with existing VM variables
        let existing_variables = vm.list_variables();

        let mut compiler = Self {
            codegen: InstructionBuilder::new(),
            type_context: TypeContext::new(type_registry),
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

        for statement in program.statements {
            compiler.compile_statement(statement)?;
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
                self.compile_expression(
                    expression,
                    Type::Resolved(types::Type::Tuple(TypeId::NIL)),
                )?;
                Ok(())
            }
        }
    }

    fn compile_type_alias(&mut self, name: &str, type_definition: ast::Type) -> Result<(), Error> {
        let resolved_type = self.type_context.resolve_ast_type(type_definition)?;
        self.type_context.type_aliases.insert(name.to_string(), resolved_type);
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
        )
    }

    fn compile_literal(&mut self, literal: ast::Literal) -> Result<Type, Error> {
        match literal {
            ast::Literal::Integer(integer) => {
                let index = self.vm.register_constant(Constant::Integer(integer));
                self.codegen.add_instruction(Instruction::Constant(index));
                Ok(Type::Resolved(types::Type::Integer))
            }
            ast::Literal::Binary(bytes) => {
                // TODO: is clone bad?
                let index = self.vm.register_constant(Constant::Binary(bytes.clone()));
                self.codegen.add_instruction(Instruction::Constant(index));
                Ok(Type::Resolved(types::Type::Binary))
            }
            ast::Literal::String(string) => {
                let bytes = string.as_bytes().to_vec();
                let index = self.vm.register_constant(Constant::Binary(bytes));
                self.codegen.add_instruction(Instruction::Constant(index));
                Ok(Type::Resolved(types::Type::Binary))
            }
        }
    }

    fn compile_value_tuple(
        &mut self,
        tuple_name: Option<String>,
        fields: Vec<ast::ValueTupleField>,
        parameter_type: Type,
    ) -> Result<Type, Error> {
        let mut field_types = Vec::new();
        let mut seen_names = HashSet::new();
        for field in &fields {
            if let Some(ref field_name) = field.name {
                if !seen_names.insert(field_name.clone()) {
                    return Err(Error::FieldDuplicated(field_name.clone()));
                }
            }
            let field_type = self.compile_chain(field.value.clone(), parameter_type.clone())?;
            if let Type::Resolved(resolved_type) = field_type {
                field_types.push((field.name.clone(), resolved_type));
            } else {
                return Err(Error::TupleFieldTypeUnresolved {
                    field_index: field_types.len(),
                });
            }
        }

        let type_id = self.type_context.type_registry.register_type(tuple_name, field_types);
        self.codegen.add_instruction(Instruction::Tuple(type_id, fields.len()));
        Ok(Type::Resolved(types::Type::Tuple(type_id)))
    }

    fn compile_operation_tuple(
        &mut self,
        name: Option<String>,
        fields: Vec<ast::OperationTupleField>,
        value_type: Type,
        parameter_type: Type,
    ) -> Result<Type, Error> {
        let has_ripple = fields
            .iter()
            .any(|field| matches!(field.value, ast::OperationTupleFieldValue::Ripple));

        if !has_ripple {
            return Err(Error::ChainValueUnused);
        }

        self.codegen.add_instruction(Instruction::Store("~".to_string()));

        let mut field_types = Vec::new();
        let mut seen_names = HashSet::new();
        for field in &fields {
            if let Some(ref name) = field.name {
                if !seen_names.insert(name.clone()) {
                    return Err(Error::FieldDuplicated(name.clone()));
                }
            }
            let field_type = match &field.value {
                ast::OperationTupleFieldValue::Ripple => {
                    // TODO: avoid using variable?
                    self.codegen.add_instruction(Instruction::Load("~".to_string()));
                    value_type.clone()
                }
                ast::OperationTupleFieldValue::Chain(chain) => {
                    self.compile_chain(chain.clone(), parameter_type.clone())?
                }
            };

            if let Type::Resolved(resolved_type) = field_type {
                field_types.push((field.name.clone(), resolved_type));
            } else {
                return Err(Error::TupleFieldTypeUnresolved {
                    field_index: field_types.len(),
                });
            }
        }

        let type_id = self.type_context.type_registry.register_type(name, field_types);
        self.codegen.add_instruction(Instruction::Tuple(type_id, fields.len()));
        Ok(Type::Resolved(types::Type::Tuple(type_id)))
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

        let captures = free_variables::collect_free_variables(
            &function_definition.body,
            &function_params,
            &|name| self.lookup_variable(name).is_some(),
        );

        let parameter_type = match &function_definition.parameter_type {
            Some(t) => self.type_context.resolve_ast_type(t.clone())?,
            None => Type::Resolved(types::Type::Tuple(TypeId::NIL)),
        };

        let saved_instructions = std::mem::take(&mut self.codegen.instructions);
        let saved_scopes = std::mem::take(&mut self.scopes);

        self.scopes = vec![HashMap::new()];
        self.codegen.instructions = Vec::new();

        for capture_name in &captures {
            if let Some(var_type) = self.lookup_variable_in_scopes(&saved_scopes, &capture_name) {
                self.define_variable(&capture_name, var_type);
            }
        }

        let mut parameter_fields = HashMap::new();
        if let Some(ast::Type::Tuple(tuple_type)) = &function_definition.parameter_type {
            for (field_index, field) in tuple_type.fields.iter().enumerate() {
                if let Some(field_name) = &field.name {
                    let field_type = self.type_context.resolve_ast_type(field.type_def.clone())?;
                    parameter_fields.insert(field_name.clone(), (field_index, field_type));
                }
            }
        }
        self.parameter_fields_stack.push(parameter_fields);

        let body_type = self.compile_block(function_definition.body, parameter_type.clone())?;
        self.codegen.add_instruction(Instruction::Return);

        self.parameter_fields_stack.pop();

        let param_types = parameter_type.to_type_vec();
        let result_types = body_type.to_type_vec();

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

        let func_type = types::FunctionType {
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

        self.codegen.add_instruction(Instruction::Function(function_index));

        let function_type = types::Type::Function(Box::new(func_type));

        Ok(Type::Resolved(function_type))
    }


    fn compile_assigment(
        &mut self,
        pattern: ast::Pattern,
        value: ast::Chain,
        parameter_type: Type,
    ) -> Result<Type, Error> {
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
                return Ok(Type::Resolved(types::Type::Tuple(TypeId::NIL)));
            }
            Some(pending_assignments) => {
                // Pattern can match - generate normal assignment code

                // Pattern matched successfully - commit all variable assignments
                self.codegen.emit_pattern_match_success(&pending_assignments);
                for (name, var_type) in &pending_assignments {
                    self.define_variable(name, var_type.clone());
                }

                // Jump to end
                let success_end_jump_addr = self.codegen.emit_jump_placeholder();

                // Cleanup section - if pattern matching failed, we come here
                self.codegen.patch_jump_to_here(cleanup_jump_addr);
                self.codegen.emit_pattern_match_cleanup(pending_assignments.len());

                // End of assignment
                self.codegen.patch_jump_to_here(success_end_jump_addr);
            }
        }

        Ok(Type::Unresolved(vec![
            types::Type::Tuple(TypeId::OK),
            types::Type::Tuple(TypeId::NIL),
        ]))
    }

    fn compile_pattern_match(
        &mut self,
        pattern: &ast::Pattern,
        value_type: &Type,
        fail_addr: usize,
    ) -> Result<Option<Vec<(String, Type)>>, Error> {
        let mut pattern_compiler = PatternCompiler::new(&mut self.codegen, &mut self.type_context, self.vm);
        pattern_compiler.compile_pattern_match(pattern, value_type, fail_addr)
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
                self.scopes.last_mut().unwrap().clear();
            }

            let condition_type =
                self.compile_expression(branch.condition.clone(), parameter_type.clone())?;

            // If condition is compile-time NIL (won't match), skip this branch entirely
            if condition_type == Type::Resolved(types::Type::Tuple(TypeId::NIL)) {
                // Don't add to branch_types, continue to next branch
                continue;
            }

            if branch.consequence.is_some() {
                // Branch has a consequence - compile it
                self.codegen.add_instruction(Instruction::Duplicate);
                let next_branch_jump = self.codegen.emit_jump_if_nil_placeholder();
                next_branch_jumps.push((next_branch_jump, i + 1));
                self.codegen.add_instruction(Instruction::Pop);

                let consequence_type = self.compile_expression(
                    branch.consequence.clone().unwrap(),
                    parameter_type.clone(),
                )?;
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
                    let success_jump = self.codegen.emit_jump_if_not_nil_placeholder();
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
        parameter_type: Type,
    ) -> Result<Type, Error> {
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
            if let Some(Type::Resolved(types::Type::Tuple(TypeId::NIL))) = last_type {
                break;
            }

            if i < expression.terms.len() - 1 {
                self.codegen.add_instruction(Instruction::Duplicate);
                let end_jump = self.codegen.emit_jump_if_nil_placeholder();
                end_jumps.push(end_jump);
                self.codegen.add_instruction(Instruction::Pop);
            }
        }

        let end_addr = self.codegen.instructions.len();
        for jump_addr in end_jumps {
            self.codegen.patch_jump_to_addr(jump_addr, end_addr);
        }

        Ok(last_type.unwrap())
    }

    fn compile_parameter(
        &mut self,
        parameter: ast::Parameter,
        parameter_type: Type,
    ) -> Result<Type, Error> {
        match parameter {
            ast::Parameter::Self_ => {
                self.codegen.add_instruction(Instruction::Parameter);
                Ok(parameter_type)
            }
            ast::Parameter::Indexed(index) => {
                self.codegen.add_instruction(Instruction::Parameter);
                self.codegen.add_instruction(Instruction::Get(index));

                match parameter_type {
                    Type::Resolved(types::Type::Tuple(type_id)) => {
                        if let Some(type_info) = self.type_context.type_registry.lookup_type(&type_id) {
                            if let Some(field) = type_info.1.get(index) {
                                Ok(Type::Resolved(field.1.clone()))
                            } else {
                                Err(Error::ParameterIndexOutOfBounds { index })
                            }
                        } else {
                            Err(Error::ParameterTypeNotInRegistry {
                                type_id: format!("{:?}", type_id),
                            })
                        }
                    }
                    _ => Err(Error::ParameterAccessOnNonTuple { index }),
                }
            }
        }
    }

    fn compile_chain(&mut self, chain: ast::Chain, parameter_type: Type) -> Result<Type, Error> {
        let mut value_type = match chain.value {
            ast::Value::Literal(literal) => self.compile_literal(literal),
            ast::Value::Tuple(tuple) => {
                self.compile_value_tuple(tuple.name, tuple.fields, parameter_type.clone())
            }
            ast::Value::FunctionDefinition(function_definition) => {
                self.compile_function_definition(function_definition)
            }
            ast::Value::Block(block) => {
                self.codegen.add_instruction(Instruction::Tuple(TypeId::NIL, 0));
                self.compile_block(block, Type::Resolved(types::Type::Tuple(TypeId::NIL)))
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

    fn value_to_instructions(&mut self, value: &vm::Value) -> Result<Type, Error> {
        match value {
            vm::Value::Integer(int_value) => {
                let index = self.vm.register_constant(Constant::Integer(*int_value));
                self.codegen.add_instruction(Instruction::Constant(index));
                Ok(Type::Resolved(types::Type::Integer))
            }
            vm::Value::Binary(const_index) => {
                self.codegen.add_instruction(Instruction::Constant(*const_index));
                Ok(Type::Resolved(types::Type::Binary))
            }
            vm::Value::Tuple(type_id, fields) => {
                for field in fields {
                    self.value_to_instructions(field)?;
                }
                self.codegen.add_instruction(Instruction::Tuple(*type_id, fields.len()));
                Ok(Type::Resolved(types::Type::Tuple(*type_id)))
            }
            vm::Value::Function { function, captures } => {
                for capture in captures {
                    self.value_to_instructions(capture)?;
                }
                if let Some(func_def) = self.vm.get_functions().get(*function).cloned() {
                    let func_index = self.vm.register_function(func_def);
                    self.codegen.add_instruction(Instruction::Function(func_index));
                    Ok(Type::Resolved(self.function_to_type(func_index)))
                } else {
                    Err(Error::FeatureUnsupported(
                        "Invalid function reference".to_string(),
                    ))
                }
            }
        }
    }

    fn compile_value_import(&mut self, module_path: &str) -> Result<Type, Error> {
        // For now, just return a simple tuple value. This will need proper module import handling later.
        self.codegen.add_instruction(Instruction::Tuple(TypeId::NIL, 0));
        Ok(Type::Resolved(types::Type::Tuple(TypeId::NIL)))
    }


    fn compile_operation(
        &mut self,
        operation: ast::Operation,
        value_type: Type,
        parameter_type: Type,
    ) -> Result<Type, Error> {
        match operation {
            ast::Operation::Operator(operator) => self.compile_operator(operator, value_type),
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
        }
    }

    fn compile_tuple_element_access(
        &mut self,
        accessor: TupleAccessor,
        value_type: Type,
    ) -> Result<Type, Error> {
        match value_type {
            Type::Resolved(types::Type::Tuple(type_id)) => {
                let (index, result_type) = match accessor {
                    TupleAccessor::Field(field_name) => {
                        self.type_context.get_tuple_field_type_by_name(&type_id, &field_name)?
                    }
                    TupleAccessor::Position(position) => {
                        let field_type =
                            self.type_context.get_tuple_field_type_by_position(&type_id, position)?;
                        (position, field_type)
                    }
                };
                self.codegen.add_instruction(Instruction::Get(index));
                Ok(Type::Resolved(result_type))
            }
            Type::Resolved(_) => match accessor {
                TupleAccessor::Field(field_name) => {
                    Err(Error::FieldAccessOnNonTuple { field_name })
                }
                TupleAccessor::Position(index) => Err(Error::PositionalAccessOnNonTuple { index }),
            },
            Type::Unresolved(_) => match accessor {
                TupleAccessor::Field(field_name) => {
                    Err(Error::FieldAccessOnNonTuple { field_name })
                }
                TupleAccessor::Position(index) => Err(Error::PositionalAccessOnNonTuple { index }),
            },
        }
    }

    fn compile_operation_tail_call(&mut self, identifier: &str) -> Result<Type, Error> {
        if identifier.is_empty() {
            self.codegen.add_instruction(Instruction::TailCall(true));
            Ok(Type::Unresolved(vec![]))
        } else {
            self.codegen.add_instruction(Instruction::Load(identifier.to_string()));
            self.codegen.add_instruction(Instruction::TailCall(false));

            match self.lookup_variable(identifier) {
                Some(function_type) => Ok(function_type),
                None => Err(Error::VariableUndefined(identifier.to_string())),
            }
        }
    }

    fn compile_operation_parameter(
        &mut self,
        parameter: ast::Parameter,
        value_type: Type,
        parameter_type: Type,
    ) -> Result<Type, Error> {
        // Get the operation type from the parameter (could be function or other callable)
        let function_type = match parameter {
            ast::Parameter::Self_ => {
                self.codegen.add_instruction(Instruction::Parameter);
                parameter_type
            }
            ast::Parameter::Indexed(index) => {
                self.codegen.add_instruction(Instruction::Parameter);
                self.codegen.add_instruction(Instruction::Get(index));

                let param_types = parameter_type.to_type_vec();
                let mut field_type_results = Vec::new();

                for param_type in param_types {
                    match param_type {
                        types::Type::Tuple(type_id) => {
                            if let Some(type_info) = self.type_context.type_registry.lookup_type(&type_id) {
                                if let Some(field) = type_info.1.get(index) {
                                    field_type_results.push(Type::Resolved(field.1.clone()));
                                } else {
                                    return Err(Error::ParameterIndexOutOfBounds { index });
                                }
                            } else {
                                return Err(Error::ParameterTypeNotInRegistry {
                                    type_id: format!("{:?}", type_id),
                                });
                            }
                        }
                        _ => return Err(Error::ParameterAccessOnNonTuple { index }),
                    }
                }

                if field_type_results.is_empty() {
                    return Err(Error::ParameterIndexOutOfBounds { index });
                }

                // Use narrow_types to properly combine the field types
                narrow_types(field_type_results)?
            }
        };

        // Use the unified function call handler
        self.compile_function_call(function_type, value_type)
    }

    fn compile_operator(
        &mut self,
        operator: ast::Operator,
        value_type: Type,
    ) -> Result<Type, Error> {
        match value_type {
            Type::Resolved(types::Type::Tuple(type_id)) => {
                let type_registry = &self.type_context.type_registry;
                let tuple_type = type_registry.lookup_type(&type_id).ok_or(
                    Error::OperatorTypeNotInRegistry {
                        type_id: format!("{:?}", type_id),
                    },
                )?;
                let tuple_size = tuple_type.1.len();

                // TODO: check all tuple items are integers

                for i in 0..tuple_size {
                    if i < tuple_size - 1 {
                        self.codegen.add_instruction(Instruction::Duplicate);
                    }
                    self.codegen.add_instruction(Instruction::Get(i));
                    if i < tuple_size - 1 {
                        self.codegen.add_instruction(Instruction::Swap);
                    }
                }

                match operator {
                    crate::ast::Operator::Add => {
                        self.codegen.add_instruction(Instruction::Add(tuple_size));
                        Ok(Type::Resolved(types::Type::Integer))
                    }
                    crate::ast::Operator::Subtract => {
                        self.codegen.add_instruction(Instruction::Subtract(tuple_size));
                        Ok(Type::Resolved(types::Type::Integer))
                    }
                    crate::ast::Operator::Multiply => {
                        self.codegen.add_instruction(Instruction::Multiply(tuple_size));
                        Ok(Type::Resolved(types::Type::Integer))
                    }
                    crate::ast::Operator::Divide => {
                        self.codegen.add_instruction(Instruction::Divide(tuple_size));
                        Ok(Type::Resolved(types::Type::Integer))
                    }
                    crate::ast::Operator::Modulo => {
                        self.codegen.add_instruction(Instruction::Modulo(tuple_size));
                        Ok(Type::Resolved(types::Type::Integer))
                    }
                    crate::ast::Operator::Equal => {
                        self.codegen.add_instruction(Instruction::Equal(tuple_size));
                        Ok(Type::Unresolved(vec![
                            types::Type::Tuple(TypeId::NIL),
                            types::Type::Integer,
                        ]))
                    }
                    crate::ast::Operator::NotEqual => {
                        self.codegen.add_instruction(Instruction::NotEqual(tuple_size));
                        Ok(Type::Unresolved(vec![
                            types::Type::Tuple(TypeId::NIL),
                            types::Type::Integer,
                        ]))
                    }
                    crate::ast::Operator::LessThan => {
                        self.codegen.add_instruction(Instruction::Less(tuple_size));
                        Ok(Type::Unresolved(vec![
                            types::Type::Tuple(TypeId::NIL),
                            types::Type::Integer,
                        ]))
                    }
                    crate::ast::Operator::LessThanOrEqual => {
                        self.codegen.add_instruction(Instruction::LessEqual(tuple_size));
                        Ok(Type::Unresolved(vec![
                            types::Type::Tuple(TypeId::NIL),
                            types::Type::Integer,
                        ]))
                    }
                    crate::ast::Operator::GreaterThan => {
                        self.codegen.add_instruction(Instruction::Greater(tuple_size));
                        Ok(Type::Unresolved(vec![
                            types::Type::Tuple(TypeId::NIL),
                            types::Type::Integer,
                        ]))
                    }
                    crate::ast::Operator::GreaterThanOrEqual => {
                        self.codegen.add_instruction(Instruction::GreaterEqual(tuple_size));
                        Ok(Type::Unresolved(vec![
                            types::Type::Tuple(TypeId::NIL),
                            types::Type::Integer,
                        ]))
                    }
                }
            }
            _ => Err(Error::OperatorOnNonTuple {
                operator: format!("{:?}", operator),
            }),
        }
    }

    fn compile_target_access(&mut self, target: &str) -> Result<Type, Error> {
        if let Some(var_type) = self.lookup_variable(target) {
            self.codegen.add_instruction(Instruction::Load(target.to_string()));
            return Ok(var_type);
        }

        if let Some(parameter_fields) = self.parameter_fields_stack.last() {
            if let Some(&(field_index, ref field_type)) = parameter_fields.get(target) {
                let field_index_copy = field_index;
                let field_type_copy = field_type.clone();
                self.codegen.add_instruction(Instruction::Parameter);
                self.codegen.add_instruction(Instruction::Get(field_index_copy));
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
    ) -> Result<Type, Error> {
        let mut last_type = if emit_load {
            self.codegen.add_instruction(Instruction::Load(target.to_string()));
            self.lookup_variable(target)
                .ok_or(Error::VariableUndefined(target.to_string()))?
        } else {
            self.compile_target_access(target)?
        };

        for accessor in accessors {
            match last_type {
                Type::Resolved(types::Type::Tuple(type_id)) => {
                    let (index, result_type) = match accessor {
                        ast::AccessPath::Field(field_name) => self
                            .type_context.get_tuple_field_type_by_name(&type_id, &field_name)
                            .map_err(|_| Error::MemberFieldNotFound {
                                field_name: field_name.clone(),
                                target: target.to_string(),
                            })?,
                        ast::AccessPath::Index(index) => {
                            let field_type = self
                                .type_context.get_tuple_field_type_by_position(&type_id, index)
                                .map_err(|_| Error::MemberAccessOnNonTuple {
                                    target: target.to_string(),
                                })?;
                            (index, field_type)
                        }
                    };
                    self.codegen.add_instruction(Instruction::Get(index));
                    last_type = Type::Resolved(result_type);
                }
                _ => {
                    return Err(Error::MemberAccessOnNonTuple {
                        target: target.to_string(),
                    });
                }
            }
        }

        Ok(last_type)
    }

    fn compile_value_member_access(
        &mut self,
        target: &str,
        accessors: Vec<ast::AccessPath>,
    ) -> Result<Type, Error> {
        self.compile_member_access_chain(target, accessors, false)
    }

    fn compile_operation_member_access(
        &mut self,
        target: &str,
        accessors: Vec<ast::AccessPath>,
        value_type: Type,
    ) -> Result<Type, Error> {
        let function_type = self.compile_member_access_chain(target, accessors, true)?;
        // Use the unified function call handler
        self.compile_function_call(function_type, value_type)
    }

    fn compile_function_call(
        &mut self,
        operation_type: Type,
        value_type: Type,
    ) -> Result<Type, Error> {
        let operation_types = operation_type.to_type_vec();
        let value_types = value_type.to_type_vec();

        // Separate function types from non-function types
        let mut function_variants = Vec::new();
        let mut non_function_types = Vec::new();

        for op_type in &operation_types {
            match op_type {
                types::Type::Function(func_type) => function_variants.push(func_type),
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
                value_types
                    .iter()
                    .any(|value_type| func_param_type == value_type)
            });

            if !is_compatible {
                return Err(Error::TypeMismatch {
                    expected: format!("function parameter compatible with {:?}", value_types),
                    found: format!("function parameter {:?}", func_type.parameter),
                });
            }

            // Collect return types from this compatible function
            all_return_types.extend(func_type.result.iter().map(|t| Type::Resolved(t.clone())));
        }

        // Generate the call instruction
        self.codegen.add_instruction(Instruction::Call);

        // Combine return types using narrow_types
        narrow_types(all_return_types)
    }

    fn cartesian_product_tuple_types(
        &mut self,
        tuple_name: &Option<String>,
        field_variants: Vec<(Option<String>, Vec<types::Type>)>,
    ) -> Vec<types::Type> {
        // Handle empty fields case
        if field_variants.is_empty() {
            let type_id = self.type_context.type_registry.register_type(tuple_name.clone(), vec![]);
            return vec![types::Type::Tuple(type_id)];
        }

        // Generate all combinations using recursive cartesian product
        let combinations = self.cartesian_product_recursive(&field_variants, 0, vec![]);

        // Register each combination as a tuple type
        combinations
            .into_iter()
            .map(|field_types| {
                let type_id = self
                    .type_context.type_registry
                    .register_type(tuple_name.clone(), field_types);
                types::Type::Tuple(type_id)
            })
            .collect()
    }

    fn cartesian_product_recursive(
        &self,
        field_variants: &[(Option<String>, Vec<types::Type>)],
        index: usize,
        current: Vec<(Option<String>, types::Type)>,
    ) -> Vec<Vec<(Option<String>, types::Type)>> {
        if index >= field_variants.len() {
            return vec![current];
        }

        let (field_name, type_options) = &field_variants[index];
        let mut results = Vec::new();

        for type_option in type_options {
            let mut new_current = current.clone();
            new_current.push((field_name.clone(), type_option.clone()));

            let sub_results =
                self.cartesian_product_recursive(field_variants, index + 1, new_current);
            results.extend(sub_results);
        }

        results
    }

    fn add_instruction(&mut self, instruction: Instruction) {
        self.codegen.instructions.push(instruction)
    }

    /// Extracts all fields from a tuple on the stack, placing field values on top.
    /// Assumes tuple is on top of stack. After execution:
    /// Stack: [tuple, valueN-1, ..., value1, value0] (value0 on top)
    fn emit_extract_tuple_fields(&mut self, num_fields: usize) {
        for i in (0..num_fields).rev() {
            // Copy tuple from depth (num_fields - 1 - i) to top of stack
            self.codegen.add_instruction(Instruction::Copy(num_fields - 1 - i));
            // Extract field i from the copied tuple
            self.codegen.add_instruction(Instruction::Get(i));
        }
    }

    /// Extracts specific fields from a tuple by field indices.
    /// Assumes tuple is on top of stack. After execution:
    /// Stack: [tuple, fieldN-1, ..., field1, field0] (field0 on top)
    fn emit_extract_specific_fields(&mut self, field_indices: &[usize]) {
        for (i, &field_index) in field_indices.iter().rev().enumerate() {
            // Copy tuple from depth i to top of stack
            self.codegen.add_instruction(Instruction::Copy(i));
            // Extract field from the copied tuple
            self.codegen.add_instruction(Instruction::Get(field_index));
        }
    }

    /// Emits a jump placeholder and returns the address to patch later
    fn emit_jump_placeholder(&mut self) -> usize {
        let addr = self.codegen.instructions.len();
        self.codegen.add_instruction(Instruction::Jump(0));
        addr
    }

    /// Emits a conditional jump placeholder and returns the address to patch later
    fn emit_jump_if_nil_placeholder(&mut self) -> usize {
        let addr = self.codegen.instructions.len();
        self.codegen.add_instruction(Instruction::JumpIfNil(0));
        addr
    }

    /// Emits a conditional jump placeholder and returns the address to patch later
    fn emit_jump_if_not_nil_placeholder(&mut self) -> usize {
        let addr = self.codegen.instructions.len();
        self.codegen.add_instruction(Instruction::JumpIfNotNil(0));
        addr
    }

    /// Patches a jump instruction to target the current instruction address
    fn patch_jump_to_here(&mut self, jump_addr: usize) {
        let target_addr = self.codegen.instructions.len();
        let offset = (target_addr as isize) - (jump_addr as isize) - 1;
        self.codegen.instructions[jump_addr] = match &self.codegen.instructions[jump_addr] {
            Instruction::Jump(_) => Instruction::Jump(offset),
            Instruction::JumpIfNil(_) => Instruction::JumpIfNil(offset),
            Instruction::JumpIfNotNil(_) => Instruction::JumpIfNotNil(offset),
            _ => panic!("Cannot patch non-jump instruction"),
        };
    }

    /// Patches a jump instruction to target a specific address
    fn patch_jump_to_addr(&mut self, jump_addr: usize, target_addr: usize) {
        let offset = (target_addr as isize) - (jump_addr as isize) - 1;
        self.codegen.instructions[jump_addr] = match &self.codegen.instructions[jump_addr] {
            Instruction::Jump(_) => Instruction::Jump(offset),
            Instruction::JumpIfNil(_) => Instruction::JumpIfNil(offset),
            Instruction::JumpIfNotNil(_) => Instruction::JumpIfNotNil(offset),
            _ => panic!("Cannot patch non-jump instruction"),
        };
    }

    /// Emits a conditional jump that immediately targets the fail address
    fn emit_jump_if_nil_to_addr(&mut self, fail_addr: usize) {
        let jump_addr = self.codegen.instructions.len();
        self.codegen.add_instruction(Instruction::JumpIfNil(0));
        let offset = (fail_addr as isize) - (jump_addr as isize) - 1;
        self.codegen.instructions[jump_addr] = Instruction::JumpIfNil(offset);
    }

    /// Gets field type from a tuple type by field name
    fn get_tuple_field_type_by_name(
        &self,
        type_id: &TypeId,
        field_name: &str,
    ) -> Result<(usize, types::Type), Error> {
        let tuple_type = self.type_context.type_registry.lookup_type(type_id).unwrap();
        let (index, (_, field_type)) = tuple_type
            .1
            .iter()
            .enumerate()
            .find(|(_, field)| field.0.as_deref() == Some(field_name))
            .ok_or(Error::FieldNotFound {
                field_name: field_name.to_string(),
                type_name: format!("{:?}", type_id),
            })?;
        Ok((index, field_type.clone()))
    }

    /// Gets field type from a tuple type by position
    fn get_tuple_field_type_by_position(
        &self,
        type_id: &TypeId,
        position: usize,
    ) -> Result<types::Type, Error> {
        let tuple_type = self.type_context.type_registry.lookup_type(type_id).unwrap();
        if position >= tuple_type.1.len() {
            return Err(Error::PositionalAccessOnNonTuple { index: position });
        }
        Ok(tuple_type.1[position].1.clone())
    }

    /// Emits runtime type check for tuple type
    fn emit_runtime_tuple_type_check(&mut self, type_id: TypeId, fail_addr: usize) {
        self.codegen.add_instruction(Instruction::Duplicate);
        self.codegen.add_instruction(Instruction::IsTuple(type_id));
        self.emit_jump_if_nil_to_addr(fail_addr);
        self.codegen.add_instruction(Instruction::Pop);
    }

    /// Emits a sequence of instructions for cleanup pattern (pop values and return NIL)
    fn emit_pattern_match_cleanup(&mut self, num_values_to_pop: usize) {
        for _ in 0..num_values_to_pop {
            self.codegen.add_instruction(Instruction::Pop);
        }
        self.codegen.add_instruction(Instruction::Pop); // Remove duplicated value
        self.codegen.add_instruction(Instruction::Tuple(TypeId::NIL, 0));
    }

    /// Emits pattern match success sequence (store variables and return OK)
    fn emit_pattern_match_success(&mut self, assignments: &[(String, Type)]) {
        for (variable_name, variable_type) in assignments {
            self.codegen.add_instruction(Instruction::Store(variable_name.clone()));
            self.define_variable(variable_name, variable_type.clone());
        }
        self.codegen.add_instruction(Instruction::Pop); // Remove duplicated value
        self.codegen.add_instruction(Instruction::Tuple(TypeId::OK, 0));
    }

    fn define_variable(&mut self, name: &str, var_type: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), var_type);
        }
    }

    fn function_to_type(&self, function_index: usize) -> types::Type {
        if let Some(func_def) = self.vm.get_functions().get(function_index) {
            if let Some(func_type) = &func_def.function_type {
                types::Type::Function(Box::new(func_type.clone()))
            } else {
                // Fallback if no type information is available
                types::Type::Function(Box::new(types::FunctionType {
                    parameter: vec![types::Type::Tuple(TypeId::NIL)],
                    result: vec![types::Type::Tuple(TypeId::NIL)],
                }))
            }
        } else {
            types::Type::Tuple(TypeId::NIL)
        }
    }

    fn value_to_type(&self, value: &vm::Value) -> Type {
        match value {
            vm::Value::Integer(_) => Type::Resolved(types::Type::Integer),
            vm::Value::Binary(_) => Type::Resolved(types::Type::Binary),
            vm::Value::Tuple(type_id, _) => Type::Resolved(types::Type::Tuple(*type_id)),
            vm::Value::Function { function, .. } => {
                Type::Resolved(self.function_to_type(*function))
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

    // Extract bindings from patterns recursively, with type information when available
    fn extract_bindings_from_pattern(
        &self,
        pattern: &ast::Pattern,
        expected_type: Option<&Type>,
    ) -> Vec<(String, Type)> {
        let mut bindings = Vec::new();
        match pattern {
            ast::Pattern::Identifier(name) => {
                let var_type = expected_type.cloned().unwrap_or(Type::Unresolved(vec![]));
                bindings.push((name.clone(), var_type));
            }
            ast::Pattern::Literal(_) => {}  // No variables
            ast::Pattern::Placeholder => {} // No variables
            ast::Pattern::Tuple(tuple_pattern) => {
                // Try to get field types from expected tuple type
                let field_types =
                    if let Some(Type::Resolved(types::Type::Tuple(type_id))) = expected_type {
                        self.type_context.type_registry
                            .lookup_type(type_id)
                            .map(|info| {
                                info.1
                                    .iter()
                                    .map(|(_, t)| Type::Resolved(t.clone()))
                                    .collect::<Vec<_>>()
                            })
                            .unwrap_or_default()
                    } else {
                        vec![]
                    };

                for (i, field) in tuple_pattern.fields.iter().enumerate() {
                    let field_type = field_types.get(i);
                    bindings.extend(self.extract_bindings_from_pattern(&field.pattern, field_type));
                }
            }
            ast::Pattern::Partial(field_names) => {
                for name in field_names {
                    bindings.push((name.clone(), Type::Unresolved(vec![])));
                }
            }
            ast::Pattern::Star => {} // No variables in star pattern itself
        }
        bindings
    }
}
