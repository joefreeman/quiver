use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    path::PathBuf,
    rc::Rc,
};

mod free_variables;

use crate::{
    ast,
    bytecode::{Constant, Function, Instruction, TypeId},
    modules::{ModuleError, ModuleLoader},
    parser, types, vm,
};

#[derive(Debug)]
pub enum Error {
    UndefinedVariable(String),
    DuplicatedFieldName(String),
    NotSupported(String),
    NoChainedValue,
    UnusedChainedValue,
    TypeMismatch { expected: String, found: String },
    ModuleError(ModuleError),

    Generic(String), // TODO: remove
}

#[derive(Debug, Clone, PartialEq)]
enum Type {
    Resolved(types::Type),
    Unresolved(Vec<types::Type>),
}

fn narrow_types(types: Vec<Type>) -> Type {
    let mut flattened = Vec::new();
    for t in types {
        match t {
            Type::Unresolved(ts) => flattened.extend(ts),
            Type::Resolved(t) => flattened.push(t),
        }
    }

    flattened.dedup();

    match flattened.len() {
        0 => Type::Unresolved(vec![]), // Handle empty case gracefully
        1 => Type::Resolved(flattened.get(0).unwrap().clone()),
        _ => Type::Unresolved(flattened),
    }
}

pub struct Compiler<'a> {
    instructions: Vec<Instruction>,
    scopes: Vec<HashMap<String, Type>>,
    type_aliases: HashMap<String, Type>,
    type_registry: Rc<RefCell<types::TypeRegistry>>,
    vm: Rc<RefCell<vm::VM>>,
    module_loader: &'a dyn ModuleLoader,
    module_path: Option<PathBuf>,
}

impl<'a> Compiler<'a> {
    pub fn compile(
        program: ast::Program,
        type_registry: &mut types::TypeRegistry,
        module_loader: &'a dyn ModuleLoader,
        vm: &mut vm::VM,
        module_path: Option<PathBuf>,
    ) -> Result<Vec<Instruction>, Error> {
        let type_registry_rc = Rc::new(RefCell::new(std::mem::replace(
            type_registry,
            types::TypeRegistry::new(),
        )));
        let vm_rc = Rc::new(RefCell::new(std::mem::replace(vm, vm::VM::new())));

        let mut compiler = Self {
            instructions: Vec::new(),
            scopes: vec![HashMap::new()],
            type_aliases: HashMap::new(),
            type_registry: type_registry_rc.clone(),
            vm: vm_rc.clone(),
            module_loader,
            module_path,
        };

        // Initialize compiler scope with existing VM variables
        let existing_variables = vm_rc.borrow().list_variables();
        for (name, value) in existing_variables {
            let var_type = compiler.value_to_type(&value);
            compiler.define_variable(&name, var_type);
        }

        let result = (|| -> Result<Vec<Instruction>, Error> {
            for statement in program.statements {
                compiler.compile_statement(statement)?;
            }
            Ok(compiler.instructions)
        })();

        // Always restore state, regardless of success or failure
        *type_registry = Rc::try_unwrap(type_registry_rc)
            .map(|cell| cell.into_inner())
            .unwrap_or_else(|rc| rc.borrow().clone());
        *vm = Rc::try_unwrap(vm_rc)
            .map(|cell| cell.into_inner())
            .unwrap_or_else(|rc| rc.borrow().clone());

        result
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
        let resolved_type = self.resolve_ast_type(type_definition);
        self.type_aliases.insert(name.to_string(), resolved_type);
        Ok(())
    }

    fn compile_type_import(
        &mut self,
        pattern: ast::TypeImportPattern,
        module_path: &str,
    ) -> Result<(), Error> {
        let content = self
            .module_loader
            .load(module_path, self.module_path.as_deref())
            .map_err(Error::ModuleError)?;

        let parsed = parser::parse(&content)
            .map_err(|_e| Error::Generic("Failed to parse imported module".to_string()))?;

        let type_aliases: Vec<_> = parsed
            .statements
            .iter()
            .filter_map(|stmt| match stmt {
                ast::Statement::TypeAlias {
                    name,
                    type_definition,
                } => Some((name, type_definition)),
                _ => None,
            })
            .collect();

        match &pattern {
            ast::TypeImportPattern::Star => {
                for (name, type_definition) in type_aliases {
                    self.compile_type_alias(name, type_definition.clone())?;
                }
            }
            ast::TypeImportPattern::Partial(requested_names) => {
                for requested_name in requested_names {
                    if let Some((name, type_definition)) = type_aliases
                        .iter()
                        .find(|alias| alias.0.as_str() == *requested_name)
                    {
                        self.compile_type_alias(name, (*type_definition).clone())?;
                    } else {
                        return Err(Error::Generic(format!(
                            "Type '{}' not found in module '{}'",
                            requested_name, module_path
                        )));
                    }
                }
            }
        }

        Ok(())
    }

    fn compile_literal(&mut self, literal: ast::Literal) -> Result<Type, Error> {
        match literal {
            ast::Literal::Integer(integer) => {
                let index = self
                    .vm
                    .borrow_mut()
                    .register_constant(Constant::Integer(integer));
                self.add_instruction(Instruction::Constant(index));
                Ok(Type::Resolved(types::Type::Integer))
            }
            ast::Literal::Binary(bytes) => {
                // TODO: is clone bad?
                let index = self
                    .vm
                    .borrow_mut()
                    .register_constant(Constant::Binary(bytes.clone()));
                self.add_instruction(Instruction::Constant(index));
                Ok(Type::Resolved(types::Type::Binary))
            }
            ast::Literal::String(string) => {
                let bytes = string.as_bytes().to_vec();
                let index = self
                    .vm
                    .borrow_mut()
                    .register_constant(Constant::Binary(bytes));
                self.add_instruction(Instruction::Constant(index));
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
                    return Err(Error::DuplicatedFieldName(field_name.clone()));
                }
            }
            let field_type = self.compile_chain(field.value.clone(), parameter_type.clone())?;
            if let Type::Resolved(resolved_type) = field_type {
                field_types.push((field.name.clone(), resolved_type));
            } else {
                return Err(Error::Generic(format!("Tuple field type unresolved")));
            }
        }

        let type_id = self
            .type_registry
            .borrow_mut()
            .register_type(tuple_name, field_types);
        self.add_instruction(Instruction::Tuple(type_id, fields.len()));
        Ok(Type::Resolved(types::Type::Tuple(type_id)))
    }

    fn compile_operation_tuple(
        &mut self,
        name: Option<String>,
        fields: Vec<ast::OperationTupleField>,
        value_type: Type,
        parameter_type: Type,
    ) -> Result<Type, Error> {
        // TODO: check that ripple is used (otherwise error)

        let mut field_types = Vec::new();
        let mut seen_names = HashSet::new();
        for field in &fields {
            if let Some(ref name) = field.name {
                if !seen_names.insert(name.clone()) {
                    return Err(Error::DuplicatedFieldName(name.clone()));
                }
            }
            let field_type = match &field.value {
                ast::OperationTupleFieldValue::Ripple => {
                    // TODO: avoid using variable?
                    self.add_instruction(Instruction::Load("~".to_string()));
                    value_type.clone()
                }
                ast::OperationTupleFieldValue::Chain(chain) => {
                    self.compile_chain(chain.clone(), parameter_type.clone())?
                }
            };

            if let Type::Resolved(resolved_type) = field_type {
                field_types.push((field.name.clone(), resolved_type));
            } else {
                return Err(Error::Generic(format!("Tuple field type unresolved")));
            }
        }

        let type_id = self
            .type_registry
            .borrow_mut()
            .register_type(name, field_types);
        self.add_instruction(Instruction::Tuple(type_id, fields.len()));
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
            &function_definition.body.expression,
            &function_params,
            &|name| self.lookup_variable(name).is_some(),
        );

        let parameter_type = match &function_definition.parameter_type {
            Some(t) => self.resolve_ast_type(t.clone()),
            None => Type::Resolved(types::Type::Tuple(TypeId::NIL)),
        };

        let mut compiler = Self {
            instructions: Vec::new(),
            scopes: vec![HashMap::new()],
            type_aliases: self.type_aliases.clone(),
            type_registry: self.type_registry.clone(),
            vm: self.vm.clone(),
            module_loader: self.module_loader,
            module_path: self.module_path.clone(),
        };

        for capture_name in &captures {
            if let Some(var_type) = self.lookup_variable(&capture_name) {
                compiler.define_variable(&capture_name, var_type);
            }
        }

        if let Some(ast::Type::Tuple(tuple_type)) = &function_definition.parameter_type {
            for (field_index, field) in tuple_type.fields.iter().enumerate() {
                // TODO: determine whether field is used
                if let Some(field_name) = &field.name {
                    compiler.add_instruction(Instruction::Parameter);
                    compiler.add_instruction(Instruction::Get(field_index));
                    compiler.add_instruction(Instruction::Store(field_name.clone()));
                    let field_type = compiler.resolve_ast_type(field.type_def.clone());
                    compiler.define_variable(field_name, field_type);
                }
            }
        }

        let body_type = compiler
            .compile_expression(function_definition.body.expression, parameter_type.clone())?;
        compiler.add_instruction(Instruction::Return);

        let function_index = self.vm.borrow_mut().register_function(Function {
            instructions: compiler.instructions,
            captures: captures.into_iter().collect(),
        });

        self.add_instruction(Instruction::Function(function_index));

        // For unresolved parameter or body types, we return the whole function type as unresolved
        match (&parameter_type, &body_type) {
            (Type::Resolved(param_t), Type::Resolved(body_t)) => Ok(Type::Resolved(
                types::Type::Function(Box::new(param_t.clone()), Box::new(body_t.clone())),
            )),
            _ => {
                // If either parameter or body type is unresolved, return an unresolved function type
                // For now, we'll represent this as an unresolved type with an empty vector
                // In a more complete implementation, we'd preserve the unresolved nature
                Ok(Type::Unresolved(vec![]))
            }
        }
    }

    fn resolve_ast_type(&mut self, ast_type: ast::Type) -> Type {
        match ast_type {
            ast::Type::Primitive(ast::PrimitiveType::Int) => Type::Resolved(types::Type::Integer),
            ast::Type::Primitive(ast::PrimitiveType::Bin) => Type::Resolved(types::Type::Binary),
            ast::Type::Tuple(tuple) => {
                // Collect all possible types for each field
                let mut field_variants: Vec<(Option<String>, Vec<types::Type>)> = Vec::new();

                for field in tuple.fields {
                    let field_possibilities = match self.resolve_ast_type(field.type_def) {
                        Type::Resolved(t) => vec![t],
                        Type::Unresolved(ts) => ts,
                    };
                    field_variants.push((field.name, field_possibilities));
                }

                // Generate cartesian product of all field type combinations
                let tuple_variants =
                    self.cartesian_product_tuple_types(&tuple.name, field_variants);

                match tuple_variants.len() {
                    0 => Type::Unresolved(vec![]),
                    1 => Type::Resolved(tuple_variants.into_iter().next().unwrap()),
                    _ => Type::Unresolved(tuple_variants),
                }
            }
            ast::Type::Function(function) => {
                let input_possibilities = match self.resolve_ast_type(*function.input) {
                    Type::Resolved(t) => vec![t],
                    Type::Unresolved(ts) => ts,
                };
                let output_possibilities = match self.resolve_ast_type(*function.output) {
                    Type::Resolved(t) => vec![t],
                    Type::Unresolved(ts) => ts,
                };

                // Generate cartesian product of input × output types
                let function_variants: Vec<types::Type> = input_possibilities
                    .into_iter()
                    .flat_map(|input_type| {
                        output_possibilities.iter().map(move |output_type| {
                            types::Type::Function(
                                Box::new(input_type.clone()),
                                Box::new(output_type.clone()),
                            )
                        })
                    })
                    .collect();

                match function_variants.len() {
                    0 => Type::Unresolved(vec![]),
                    1 => Type::Resolved(function_variants.into_iter().next().unwrap()),
                    _ => Type::Unresolved(function_variants),
                }
            }
            ast::Type::Union(union) => {
                // Resolve all union member types
                let mut resolved_types = Vec::new();
                for member_type in union.types {
                    match self.resolve_ast_type(member_type) {
                        Type::Resolved(t) => resolved_types.push(t),
                        Type::Unresolved(ts) => resolved_types.extend(ts),
                    }
                }

                // Return as unresolved union type
                Type::Unresolved(resolved_types)
            }
            ast::Type::Identifier(alias) => {
                // Look up type alias
                if let Some(aliased_type) = self.type_aliases.get(&alias) {
                    aliased_type.clone()
                } else {
                    // Unknown type alias - for now return an unresolved type
                    // In a more complete implementation, this should be an error
                    Type::Unresolved(vec![])
                }
            }
        }
    }

    fn compile_assigment(
        &mut self,
        pattern: ast::Pattern,
        value: ast::Chain,
        parameter_type: Type,
    ) -> Result<Type, Error> {
        let expression_type = self.compile_chain(value, parameter_type)?;

        match pattern {
            ast::Pattern::Identifier(name) => {
                self.add_instruction(Instruction::Store(name.clone()));
                self.define_variable(&name, expression_type);
                self.add_instruction(Instruction::Tuple(TypeId::OK, 0));
            }
            ast::Pattern::Tuple(tuple_pattern) => {
                self.compile_tuple_destructuring(tuple_pattern, &expression_type)
            }
            ast::Pattern::Partial(field_names) => {
                self.compile_partial_destructuring(field_names, &expression_type)
            }
            ast::Pattern::Star => self.compile_star_destructuring(&expression_type),
            ast::Pattern::Literal(literal_value) => {
                match literal_value {
                    ast::Literal::Integer(integer) => {
                        let index = self
                            .vm
                            .borrow_mut()
                            .register_constant(Constant::Integer(integer));
                        self.add_instruction(Instruction::Constant(index));
                    }
                    ast::Literal::Binary(_) => {
                        Err(Error::NotSupported("binary matching".to_string()))?;
                    }
                    ast::Literal::String(_) => {
                        Err(Error::NotSupported("string matching".to_string()))?;
                    }
                }
                self.add_instruction(Instruction::Equal(2));
            }
            ast::Pattern::Wildcard => {
                self.add_instruction(Instruction::Pop);
                self.add_instruction(Instruction::Tuple(TypeId::OK, 0));
            }
        }
        Ok(Type::Resolved(types::Type::Tuple(TypeId::OK)))
    }

    fn compile_tuple_destructuring(
        &mut self,
        tuple_pattern: ast::TuplePattern,
        expression_type: &Type,
    ) {
        for (field_index, field) in tuple_pattern.fields.iter().enumerate() {
            self.add_instruction(Instruction::Duplicate);
            self.add_instruction(Instruction::Get(field_index));

            let field_type = match expression_type {
                Type::Resolved(types::Type::Tuple(type_id)) => {
                    if let Some(type_info) = self.type_registry.borrow().lookup_type(type_id) {
                        if let Some(field_info) = type_info.1.get(field_index) {
                            Type::Resolved(field_info.1.clone())
                        } else {
                            Type::Unresolved(vec![]) // Field index out of bounds
                        }
                    } else {
                        Type::Unresolved(vec![]) // Type not found
                    }
                }
                _ => Type::Unresolved(vec![]), // Non-tuple type
            };

            match &field.pattern {
                ast::Pattern::Identifier(name) => {
                    self.add_instruction(Instruction::Store(name.clone()));
                    self.define_variable(name, field_type);
                }
                ast::Pattern::Wildcard => {
                    self.add_instruction(Instruction::Pop);
                }
                _ => {
                    self.add_instruction(Instruction::Pop);
                }
            }
        }

        self.add_instruction(Instruction::Pop);
        self.add_instruction(Instruction::Tuple(TypeId::OK, 0));
    }

    fn compile_partial_destructuring(&mut self, field_names: Vec<String>, expression_type: &Type) {
        for field_name in field_names {
            self.add_instruction(Instruction::Duplicate);

            let (field_index, field_type) = match expression_type {
                Type::Resolved(types::Type::Tuple(type_id)) => {
                    if let Some(type_info) = self.type_registry.borrow().lookup_type(type_id) {
                        if let Some((index, (_, field_type))) = type_info
                            .1
                            .iter()
                            .enumerate()
                            .find(|(_, (name, _))| name.as_ref() == Some(&field_name))
                        {
                            (index, Type::Resolved(field_type.clone()))
                        } else {
                            // Field not found - use index 0 as fallback
                            (0, Type::Unresolved(vec![]))
                        }
                    } else {
                        (0, Type::Unresolved(vec![]))
                    }
                }
                _ => (0, Type::Unresolved(vec![])), // Non-tuple type
            };

            self.add_instruction(Instruction::Get(field_index));
            self.add_instruction(Instruction::Store(field_name.clone()));
            self.define_variable(&field_name, field_type);
        }

        self.add_instruction(Instruction::Pop);
        self.add_instruction(Instruction::Tuple(TypeId::OK, 0));
    }

    fn compile_star_destructuring(&mut self, expression_type: &Type) {
        match expression_type {
            Type::Resolved(types::Type::Tuple(type_id)) => {
                let named_fields: Vec<(usize, String, types::Type)> = {
                    if let Some(type_info) = self.type_registry.borrow().lookup_type(type_id) {
                        type_info
                            .1
                            .iter()
                            .enumerate()
                            .filter_map(|(index, (name, field_type))| {
                                name.as_ref()
                                    .map(|n| (index, n.clone(), field_type.clone()))
                            })
                            .collect()
                    } else {
                        Vec::new()
                    }
                };

                for (field_index, name, field_type) in named_fields {
                    self.add_instruction(Instruction::Duplicate);
                    self.add_instruction(Instruction::Get(field_index));
                    self.add_instruction(Instruction::Store(name.clone()));
                    self.define_variable(&name, Type::Resolved(field_type));
                }
            }
            _ => {
                // Non-tuple type or unresolved type - can't extract fields
            }
        }

        self.add_instruction(Instruction::Pop); // Pop the original tuple
        self.add_instruction(Instruction::Tuple(TypeId::OK, 0)); // Push OK
    }

    fn compile_expression(
        &mut self,
        expression: ast::Expression,
        parameter_type: Type,
    ) -> Result<Type, Error> {
        let mut next_branch_jumps = Vec::new();
        let mut end_jumps = Vec::new();
        let mut branch_types = Vec::new();
        let mut branch_starts = Vec::new();

        for (i, branch) in expression.branches.iter().enumerate() {
            let is_last_branch = i == expression.branches.len() - 1;

            branch_starts.push(self.instructions.len());

            if i > 0 {
                // Pop the nil result from previous branch
                // TODO: can this be removed?
                self.add_instruction(Instruction::Pop);
            }

            let condition_type =
                self.compile_sequence(branch.condition.clone(), parameter_type.clone())?;

            if branch.consequence.is_some() {
                self.add_instruction(Instruction::Duplicate);
                let next_branch_jump = self.instructions.len();
                self.add_instruction(Instruction::JumpIfNil(0));
                next_branch_jumps.push((next_branch_jump, i + 1));
                self.add_instruction(Instruction::Pop);

                let consequence_type = self.compile_sequence(
                    branch.consequence.clone().unwrap(),
                    parameter_type.clone(),
                )?;
                branch_types.push(consequence_type);

                if !is_last_branch {
                    let end_jump = self.instructions.len();
                    self.add_instruction(Instruction::Jump(0));
                    end_jumps.push(end_jump);
                }
            } else {
                if !is_last_branch {
                    self.add_instruction(Instruction::Duplicate);
                    let success_jump = self.instructions.len();
                    self.add_instruction(Instruction::JumpIfNotNil(0));
                    end_jumps.push(success_jump);
                }
                branch_types.push(condition_type);
            }
        }

        let end_addr = self.instructions.len();

        for (jump_addr, next_branch_idx) in next_branch_jumps {
            let target_addr = if next_branch_idx >= branch_starts.len() {
                end_addr
            } else {
                branch_starts[next_branch_idx]
            };
            let offset = target_addr as isize - jump_addr as isize - 1;
            self.instructions[jump_addr] = Instruction::JumpIfNil(offset);
        }

        for jump_addr in end_jumps {
            let offset = end_addr as isize - jump_addr as isize - 1;
            if let Some(instruction) = self.instructions.get_mut(jump_addr) {
                *instruction = match instruction {
                    Instruction::Jump(_) => Instruction::Jump(offset),
                    Instruction::JumpIfNotNil(_) => Instruction::JumpIfNotNil(offset),
                    _ => instruction.clone(),
                };
            }
        }

        Ok(narrow_types(branch_types))
    }

    fn compile_sequence(
        &mut self,
        sequence: ast::Sequence,
        parameter_type: Type,
    ) -> Result<Type, Error> {
        let mut last_type = None;
        let mut end_jumps = Vec::new();

        for (i, term) in sequence.terms.iter().enumerate() {
            last_type = Some(match term {
                ast::Term::Assignment { pattern, value } => {
                    self.compile_assigment(pattern.clone(), value.clone(), parameter_type.clone())
                }
                ast::Term::Chain(chain) => {
                    self.compile_chain(chain.clone(), parameter_type.clone())
                }
            }?);

            if i < sequence.terms.len() - 1 {
                self.add_instruction(Instruction::Duplicate);
                let end_jump = self.instructions.len();
                self.add_instruction(Instruction::JumpIfNil(0));
                end_jumps.push(end_jump);
                self.add_instruction(Instruction::Pop);
            }
        }

        let end_addr = self.instructions.len();
        for jump_addr in end_jumps {
            let offset = (end_addr - jump_addr - 1) as isize;
            self.instructions[jump_addr] = Instruction::JumpIfNil(offset);
        }

        Ok(last_type.unwrap())
    }

    fn compile_block(&mut self, block: ast::Block, value_type: Type) -> Result<Type, Error> {
        self.add_instruction(Instruction::Enter);
        self.scopes.push(HashMap::new());
        let result_type = self.compile_expression(block.expression, value_type)?;
        self.scopes.pop();
        self.add_instruction(Instruction::Exit);
        Ok(result_type)
    }

    fn compile_parameter(
        &mut self,
        parameter: ast::Parameter,
        parameter_type: Type,
    ) -> Result<Type, Error> {
        match parameter {
            ast::Parameter::Self_ => {
                self.add_instruction(Instruction::Parameter);
                Ok(parameter_type)
            }
            ast::Parameter::Indexed(index) => {
                self.add_instruction(Instruction::Parameter);
                self.add_instruction(Instruction::Get(index));

                match parameter_type {
                    Type::Resolved(types::Type::Tuple(type_id)) => {
                        if let Some(type_info) = self.type_registry.borrow().lookup_type(&type_id) {
                            if let Some(field) = type_info.1.get(index) {
                                Ok(Type::Resolved(field.1.clone()))
                            } else {
                                Err(Error::Generic("".to_string()))
                            }
                        } else {
                            Err(Error::Generic("".to_string()))
                        }
                    }
                    _ => Err(Error::Generic("".to_string())),
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
                self.compile_block(block, Type::Resolved(types::Type::Tuple(TypeId::NIL)))
            }
            ast::Value::Parameter(parameter) => {
                self.compile_parameter(parameter, parameter_type.clone())
            }
            ast::Value::MemberAccess(member_access) => {
                self.compile_value_member_access(&member_access.target, member_access.accessors)
            }
            ast::Value::TailCall(identifier) => self.compile_value_tail_call(&identifier),
            ast::Value::Import(path) => self.compile_value_import(&path),
            ast::Value::Parenthesized(expression) => {
                self.compile_expression(*expression, parameter_type.clone())
            }
        }?;

        for operation in chain.operations {
            value_type = self.compile_operation(operation, value_type, parameter_type.clone())?;
        }

        Ok(value_type)
    }

    fn compile_value_tail_call(&self, _identifier: &str) -> Result<Type, Error> {
        todo!()
    }

    fn compile_value_import(&mut self, module_path: &str) -> Result<Type, Error> {
        // TODO: cache
        // TODO: check for circular imports

        let content = self
            .module_loader
            .load(module_path, self.module_path.as_deref())
            .map_err(Error::ModuleError)?;

        let parsed = parser::parse(&content)
            .map_err(|_e| Error::Generic("Failed to parse imported module".to_string()))?;

        // TODO: update module path (to path of resolved module)
        let instructions = Compiler::compile(
            parsed,
            &mut *self.type_registry.borrow_mut(),
            self.module_loader,
            &mut *self.vm.borrow_mut(),
            self.module_path.clone(),
        )?;
        let _result = self
            .vm
            .borrow_mut()
            .execute_instructions(instructions, true);

        // TODO: 'emit_value_reconstruction'
        todo!()
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
                self.compile_operation_field_access(field, value_type)
            }
            ast::Operation::PositionalAccess(position) => {
                self.compile_operation_positional_access(position, value_type)
            }
            ast::Operation::TailCall(identifier) => self.compile_operation_tail_call(&identifier),
        }
    }

    fn compile_operation_field_access(
        &mut self,
        field_name: String,
        value_type: Type,
    ) -> Result<Type, Error> {
        match value_type {
            Type::Resolved(types::Type::Tuple(type_id)) => {
                let type_registry = self.type_registry.borrow();
                let tuple_type = type_registry.lookup_type(&type_id).unwrap();
                let index = tuple_type
                    .1
                    .iter()
                    .position(|f| f.0 == Some(field_name.clone()))
                    .ok_or(Error::Generic("".to_string()))?;
                let result_type = tuple_type.1[index].1.clone();
                drop(type_registry); // Drop the borrow before calling add_instruction
                self.add_instruction(Instruction::Get(index));
                Ok(Type::Resolved(result_type))
            }
            _ => Err(Error::Generic(format!(
                "Cannot access field '{}' on non-tuple type",
                field_name
            ))),
        }
    }

    fn compile_operation_positional_access(
        &mut self,
        index: usize,
        value_type: Type,
    ) -> Result<Type, Error> {
        match value_type {
            Type::Resolved(types::Type::Tuple(type_id)) => {
                let type_registry = self.type_registry.borrow();
                let tuple_type = type_registry.lookup_type(&type_id).unwrap();
                let result_type = tuple_type.1[index].1.clone();
                drop(type_registry); // Drop the borrow before calling add_instruction
                self.add_instruction(Instruction::Get(index));
                Ok(Type::Resolved(result_type))
            }
            _ => Err(Error::Generic(format!(
                "Cannot access field at position {} on non-tuple type",
                index
            ))),
        }
    }

    fn compile_operation_tail_call(&self, _identifier: &str) -> Result<Type, Error> {
        todo!()
    }

    fn compile_operator(
        &mut self,
        operator: ast::Operator,
        value_type: Type,
    ) -> Result<Type, Error> {
        match value_type {
            Type::Resolved(types::Type::Tuple(type_id)) => {
                let type_registry = self.type_registry.borrow();
                let tuple_type = type_registry
                    .lookup_type(&type_id)
                    .ok_or(Error::Generic("".to_string()))?;
                let tuple_size = tuple_type.1.len();
                drop(type_registry); // Drop the borrow before calling add_instruction

                // TODO: check all tuple items are integers

                for i in 0..tuple_size {
                    if i < tuple_size - 1 {
                        self.add_instruction(Instruction::Duplicate);
                    }
                    self.add_instruction(Instruction::Get(i));
                    if i < tuple_size - 1 {
                        self.add_instruction(Instruction::Swap);
                    }
                }

                match operator {
                    crate::ast::Operator::Add => {
                        self.add_instruction(Instruction::Add(tuple_size));
                        Ok(Type::Resolved(types::Type::Integer))
                    }
                    crate::ast::Operator::Subtract => {
                        self.add_instruction(Instruction::Subtract(tuple_size));
                        Ok(Type::Resolved(types::Type::Integer))
                    }
                    crate::ast::Operator::Multiply => {
                        self.add_instruction(Instruction::Multiply(tuple_size));
                        Ok(Type::Resolved(types::Type::Integer))
                    }
                    crate::ast::Operator::Divide => {
                        self.add_instruction(Instruction::Divide(tuple_size));
                        Ok(Type::Resolved(types::Type::Integer))
                    }
                    crate::ast::Operator::Modulo => {
                        self.add_instruction(Instruction::Modulo(tuple_size));
                        Ok(Type::Resolved(types::Type::Integer))
                    }
                    crate::ast::Operator::Equal => {
                        self.add_instruction(Instruction::Equal(tuple_size));
                        Ok(Type::Unresolved(vec![
                            types::Type::Tuple(TypeId::NIL),
                            types::Type::Integer,
                        ]))
                    }
                    crate::ast::Operator::NotEqual => {
                        self.add_instruction(Instruction::NotEqual(tuple_size));
                        Ok(Type::Unresolved(vec![
                            types::Type::Tuple(TypeId::NIL),
                            types::Type::Integer,
                        ]))
                    }
                    crate::ast::Operator::LessThan => {
                        self.add_instruction(Instruction::Less(tuple_size));
                        Ok(Type::Unresolved(vec![
                            types::Type::Tuple(TypeId::NIL),
                            types::Type::Integer,
                        ]))
                    }
                    crate::ast::Operator::LessThanOrEqual => {
                        self.add_instruction(Instruction::LessEqual(tuple_size));
                        Ok(Type::Unresolved(vec![
                            types::Type::Tuple(TypeId::NIL),
                            types::Type::Integer,
                        ]))
                    }
                    crate::ast::Operator::GreaterThan => {
                        self.add_instruction(Instruction::Greater(tuple_size));
                        Ok(Type::Unresolved(vec![
                            types::Type::Tuple(TypeId::NIL),
                            types::Type::Integer,
                        ]))
                    }
                    crate::ast::Operator::GreaterThanOrEqual => {
                        self.add_instruction(Instruction::GreaterEqual(tuple_size));
                        Ok(Type::Unresolved(vec![
                            types::Type::Tuple(TypeId::NIL),
                            types::Type::Integer,
                        ]))
                    }
                }
            }
            _ => Err(Error::Generic("".to_string())),
        }
    }

    fn compile_value_member_access(
        &mut self,
        target: &str,
        accessors: Vec<ast::AccessPath>,
    ) -> Result<Type, Error> {
        self.add_instruction(Instruction::Load(target.to_string()));

        let mut last_type = self
            .lookup_variable(target)
            .ok_or(Error::UndefinedVariable(target.to_string()))?;

        for accessor in accessors {
            match last_type {
                Type::Resolved(types::Type::Tuple(type_id)) => {
                    let type_registry = self.type_registry.borrow();
                    let tuple_type = type_registry.lookup_type(&type_id).unwrap();
                    let index = match accessor {
                        ast::AccessPath::Field(field_name) => tuple_type
                            .1
                            .iter()
                            .position(|f| f.0 == Some(field_name.clone()))
                            .ok_or(Error::Generic("".to_string()))?,
                        ast::AccessPath::Index(index) => index,
                    };
                    let new_type = tuple_type.1[index].1.clone();
                    drop(type_registry); // Drop the borrow before calling add_instruction
                    self.add_instruction(Instruction::Get(index));
                    last_type = Type::Resolved(new_type);
                }
                _ => return Err(Error::Generic("".to_string())),
            }
        }

        Ok(last_type)
    }

    fn compile_operation_member_access(
        &mut self,
        target: &str,
        accessors: Vec<ast::AccessPath>,
        value_type: Type,
    ) -> Result<Type, Error> {
        self.add_instruction(Instruction::Load(target.to_string()));

        let mut last_type = self
            .lookup_variable(target)
            .ok_or(Error::UndefinedVariable(target.to_string()))?;

        for accessor in accessors {
            match last_type {
                Type::Resolved(types::Type::Tuple(type_id)) => {
                    let type_registry = self.type_registry.borrow();
                    let tuple_type = type_registry.lookup_type(&type_id).unwrap();
                    let index = match accessor {
                        ast::AccessPath::Field(field_name) => tuple_type
                            .1
                            .iter()
                            .position(|f| f.0 == Some(field_name.clone()))
                            .ok_or(Error::Generic("".to_string()))?,
                        ast::AccessPath::Index(index) => index,
                    };
                    let new_type = tuple_type.1[index].1.clone();
                    drop(type_registry); // Drop the borrow before calling add_instruction
                    self.add_instruction(Instruction::Get(index));
                    last_type = Type::Resolved(new_type);
                }
                _ => return Err(Error::Generic("".to_string())),
            }
        }

        match &last_type {
            Type::Resolved(types::Type::Function(parameter_type, return_type)) => {
                // Extract the inner type from value_type for comparison
                let value_inner_type = match &value_type {
                    Type::Resolved(inner_type) => inner_type,
                    Type::Unresolved(_) => {
                        return Err(Error::TypeMismatch {
                            expected: format!("{:?}", parameter_type),
                            found: "unresolved type".to_string(),
                        });
                    }
                };

                if value_inner_type != parameter_type.as_ref() {
                    return Err(Error::TypeMismatch {
                        expected: format!("{:?}", parameter_type),
                        found: format!("{:?}", value_inner_type),
                    });
                }

                self.add_instruction(Instruction::Call);
                Ok(Type::Resolved((**return_type).clone()))
            }
            Type::Unresolved(_) => {
                // For unresolved function types, we'll allow the call but return an unresolved result
                // This is a more permissive approach for cases where types can't be fully resolved
                self.add_instruction(Instruction::Call);
                Ok(Type::Unresolved(vec![]))
            }
            _ => Err(Error::UnusedChainedValue),
        }
    }

    fn cartesian_product_tuple_types(
        &mut self,
        tuple_name: &Option<String>,
        field_variants: Vec<(Option<String>, Vec<types::Type>)>,
    ) -> Vec<types::Type> {
        // Handle empty fields case
        if field_variants.is_empty() {
            let type_id = self
                .type_registry
                .borrow_mut()
                .register_type(tuple_name.clone(), vec![]);
            return vec![types::Type::Tuple(type_id)];
        }

        // Generate all combinations using recursive cartesian product
        let combinations = self.cartesian_product_recursive(&field_variants, 0, vec![]);

        // Register each combination as a tuple type
        combinations
            .into_iter()
            .map(|field_types| {
                let type_id = self
                    .type_registry
                    .borrow_mut()
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
        self.instructions.push(instruction)
    }

    fn define_variable(&mut self, name: &str, var_type: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), var_type);
        }
    }

    fn value_to_type(&self, value: &vm::Value) -> Type {
        match value {
            vm::Value::Integer(_) => Type::Resolved(types::Type::Integer),
            vm::Value::Binary(_) => Type::Resolved(types::Type::Binary),
            vm::Value::Tuple(type_id, _) => Type::Resolved(types::Type::Tuple(*type_id)),
            vm::Value::Function { .. } => {
                // For functions, we could try to infer the type, but for now use a generic function type
                // This is a simplified approach - in practice we'd need more sophisticated type inference
                Type::Resolved(types::Type::Tuple(TypeId::NIL)) // Placeholder
            }
        }
    }

    fn lookup_variable(&self, name: &str) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(variable_type) = scope.get(name) {
                return Some(variable_type.clone());
            }
        }
        None
    }
}
