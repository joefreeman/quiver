use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
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

#[derive(Debug, Clone, PartialEq)]
enum Type {
    Resolved(types::Type),
    Unresolved(Vec<types::Type>),
}

impl Type {
    fn to_type_vec(&self) -> Vec<types::Type> {
        match self {
            Type::Resolved(t) => vec![t.clone()],
            Type::Unresolved(types) => types.clone(),
        }
    }
}

fn narrow_types(types: Vec<Type>) -> Result<Type, Error> {
    let mut flattened = Vec::new();
    for t in types {
        match t {
            Type::Unresolved(ts) => flattened.extend(ts),
            Type::Resolved(t) => flattened.push(t),
        }
    }

    flattened.dedup();

    match flattened.len() {
        0 => Err(Error::TypeUnresolved(
            "No valid types found in narrowing".to_string(),
        )),
        1 => Ok(Type::Resolved(flattened.get(0).unwrap().clone())),
        _ => Ok(Type::Unresolved(flattened)),
    }
}

pub struct Compiler<'a> {
    instructions: Vec<Instruction>,
    scopes: Vec<HashMap<String, Type>>,
    type_aliases: HashMap<String, Type>,
    type_registry: &'a mut types::TypeRegistry,
    vm: &'a mut vm::VM,
    module_loader: &'a dyn ModuleLoader,
    module_path: Option<PathBuf>,
    // Module caches
    ast_cache: HashMap<String, ast::Program>,
    evaluation_cache: HashMap<String, vm::Value>,
    // Circular import detection
    import_stack: Vec<String>,
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
            instructions: Vec::new(),
            scopes: vec![HashMap::new()],
            type_aliases: HashMap::new(),
            type_registry,
            vm,
            module_loader,
            module_path,
            ast_cache: HashMap::new(),
            evaluation_cache: HashMap::new(),
            import_stack: Vec::new(),
        };

        for (name, value) in existing_variables {
            let var_type = compiler.value_to_type(&value);
            compiler.define_variable(&name, var_type);
        }

        for statement in program.statements {
            compiler.compile_statement(statement)?;
        }

        Ok(compiler.instructions)
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
        let resolved_type = self.resolve_ast_type(type_definition)?;
        self.type_aliases.insert(name.to_string(), resolved_type);
        Ok(())
    }

    fn compile_type_import(
        &mut self,
        pattern: ast::TypeImportPattern,
        module_path: &str,
    ) -> Result<(), Error> {
        let parsed = self.load_and_cache_ast(module_path)?;

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
                        return Err(Error::ModuleTypeMissing {
                            type_name: requested_name.clone(),
                            module_path: module_path.to_string(),
                        });
                    }
                }
            }
        }

        Ok(())
    }

    fn compile_literal(&mut self, literal: ast::Literal) -> Result<Type, Error> {
        match literal {
            ast::Literal::Integer(integer) => {
                let index = self.vm.register_constant(Constant::Integer(integer));
                self.add_instruction(Instruction::Constant(index));
                Ok(Type::Resolved(types::Type::Integer))
            }
            ast::Literal::Binary(bytes) => {
                // TODO: is clone bad?
                let index = self.vm.register_constant(Constant::Binary(bytes.clone()));
                self.add_instruction(Instruction::Constant(index));
                Ok(Type::Resolved(types::Type::Binary))
            }
            ast::Literal::String(string) => {
                let bytes = string.as_bytes().to_vec();
                let index = self.vm.register_constant(Constant::Binary(bytes));
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

        let type_id = self.type_registry.register_type(tuple_name, field_types);
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
                    return Err(Error::FieldDuplicated(name.clone()));
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
                return Err(Error::TupleFieldTypeUnresolved {
                    field_index: field_types.len(),
                });
            }
        }

        let type_id = self.type_registry.register_type(name, field_types);
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
            Some(t) => self.resolve_ast_type(t.clone())?,
            None => Type::Resolved(types::Type::Tuple(TypeId::NIL)),
        };

        let saved_instructions = std::mem::take(&mut self.instructions);
        let saved_scopes = std::mem::take(&mut self.scopes);

        self.scopes = vec![HashMap::new()];
        self.instructions = Vec::new();

        for capture_name in &captures {
            if let Some(var_type) = self.lookup_variable_in_scopes(&saved_scopes, &capture_name) {
                self.define_variable(&capture_name, var_type);
            }
        }

        if let Some(ast::Type::Tuple(tuple_type)) = &function_definition.parameter_type {
            for (field_index, field) in tuple_type.fields.iter().enumerate() {
                if let Some(field_name) = &field.name {
                    self.add_instruction(Instruction::Parameter);
                    self.add_instruction(Instruction::Get(field_index));
                    self.add_instruction(Instruction::Store(field_name.clone()));
                    let field_type = self.resolve_ast_type(field.type_def.clone())?;
                    self.define_variable(field_name, field_type);
                }
            }
        }

        let body_type =
            self.compile_expression(function_definition.body.expression, parameter_type.clone())?;
        self.add_instruction(Instruction::Return);

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

        let function_instructions = std::mem::take(&mut self.instructions);
        let function_index = self.vm.register_function(Function {
            instructions: function_instructions,
            captures: captures.into_iter().collect(),
            function_type: Some(func_type.clone()),
        });

        self.instructions = saved_instructions;
        self.scopes = saved_scopes;

        self.add_instruction(Instruction::Function(function_index));

        let function_type = types::Type::Function(Box::new(func_type));

        Ok(Type::Resolved(function_type))
    }

    fn resolve_ast_type(&mut self, ast_type: ast::Type) -> Result<Type, Error> {
        match ast_type {
            ast::Type::Primitive(ast::PrimitiveType::Int) => {
                Ok(Type::Resolved(types::Type::Integer))
            }
            ast::Type::Primitive(ast::PrimitiveType::Bin) => {
                Ok(Type::Resolved(types::Type::Binary))
            }
            ast::Type::Tuple(tuple) => {
                // Collect all possible types for each field
                let mut field_variants: Vec<(Option<String>, Vec<types::Type>)> = Vec::new();

                for field in tuple.fields {
                    let field_possibilities = match self.resolve_ast_type(field.type_def)? {
                        Type::Resolved(t) => vec![t],
                        Type::Unresolved(ts) => {
                            if ts.is_empty() {
                                return Err(Error::TypeUnresolved(
                                    "Empty field type possibilities".to_string(),
                                ));
                            }
                            ts
                        }
                    };
                    field_variants.push((field.name, field_possibilities));
                }

                // Generate cartesian product of all field type combinations
                let tuple_variants =
                    self.cartesian_product_tuple_types(&tuple.name, field_variants);

                Ok(match tuple_variants.len() {
                    0 => {
                        return Err(Error::TypeUnresolved(
                            "No valid tuple type variants found".to_string(),
                        ));
                    }
                    1 => Type::Resolved(tuple_variants.into_iter().next().unwrap()),
                    _ => Type::Unresolved(tuple_variants),
                })
            }
            ast::Type::Function(function) => {
                let input_possibilities = match self.resolve_ast_type(*function.input)? {
                    Type::Resolved(t) => vec![t],
                    Type::Unresolved(ts) => {
                        if ts.is_empty() {
                            return Err(Error::TypeUnresolved(
                                "Empty function input type possibilities".to_string(),
                            ));
                        }
                        ts
                    }
                };
                let output_possibilities = match self.resolve_ast_type(*function.output)? {
                    Type::Resolved(t) => vec![t],
                    Type::Unresolved(ts) => {
                        if ts.is_empty() {
                            return Err(Error::TypeUnresolved(
                                "Empty function output type possibilities".to_string(),
                            ));
                        }
                        ts
                    }
                };

                // Generate cartesian product of input × output types
                let function_variants: Vec<types::Type> = input_possibilities
                    .into_iter()
                    .flat_map(|input_type| {
                        output_possibilities.iter().map(move |output_type| {
                            types::Type::Function(Box::new(types::FunctionType {
                                parameter: vec![input_type.clone()],
                                result: vec![output_type.clone()],
                            }))
                        })
                    })
                    .collect();

                Ok(match function_variants.len() {
                    0 => {
                        return Err(Error::TypeUnresolved(
                            "No valid function type variants found".to_string(),
                        ));
                    }
                    1 => Type::Resolved(function_variants.into_iter().next().unwrap()),
                    _ => Type::Unresolved(function_variants),
                })
            }
            ast::Type::Union(union) => {
                // Resolve all union member types
                let mut resolved_types = Vec::new();
                for member_type in union.types {
                    match self.resolve_ast_type(member_type)? {
                        Type::Resolved(t) => resolved_types.push(t),
                        Type::Unresolved(ts) => {
                            if ts.is_empty() {
                                return Err(Error::TypeUnresolved(
                                    "Empty union member type".to_string(),
                                ));
                            }
                            resolved_types.extend(ts);
                        }
                    }
                }

                // Return as unresolved union type
                if resolved_types.is_empty() {
                    return Err(Error::TypeUnresolved("Empty union type".to_string()));
                }
                Ok(Type::Unresolved(resolved_types))
            }
            ast::Type::Identifier(alias) => {
                // Look up type alias
                if let Some(aliased_type) = self.type_aliases.get(&alias) {
                    Ok(aliased_type.clone())
                } else {
                    Err(Error::TypeAliasMissing(alias))
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
                self.compile_tuple_destructuring(tuple_pattern, &expression_type)?;
            }
            ast::Pattern::Partial(field_names) => {
                self.compile_partial_destructuring(field_names, &expression_type)?;
            }
            ast::Pattern::Star => {
                self.compile_star_destructuring(&expression_type)?;
            }
            ast::Pattern::Literal(literal_value) => {
                match literal_value {
                    ast::Literal::Integer(integer) => {
                        let index = self.vm.register_constant(Constant::Integer(integer));
                        self.add_instruction(Instruction::Constant(index));
                    }
                    ast::Literal::Binary(_) => {
                        Err(Error::FeatureUnsupported("binary matching".to_string()))?;
                    }
                    ast::Literal::String(_) => {
                        Err(Error::FeatureUnsupported("string matching".to_string()))?;
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
    ) -> Result<(), Error> {
        if self.compile_tuple_destructuring_impl(tuple_pattern, expression_type)? {
            self.add_instruction(Instruction::Tuple(TypeId::OK, 0));
        } else {
            self.add_instruction(Instruction::Tuple(TypeId::NIL, 0));
        }
        Ok(())
    }

    fn compile_tuple_destructuring_impl(
        &mut self,
        tuple_pattern: ast::TuplePattern,
        expression_type: &Type,
    ) -> Result<bool, Error> {
        match expression_type {
            Type::Resolved(types::Type::Tuple(expression_type_id)) => {
                let expression_type_info = self
                    .type_registry
                    .lookup_type(expression_type_id)
                    .ok_or_else(|| {
                        Error::TypeUnresolved("Expression type not found".to_string())
                    })?;

                let mut pattern_field_types = Vec::new();
                for (field_index, field) in tuple_pattern.fields.iter().enumerate() {
                    if let Some(expr_field_info) = expression_type_info.1.get(field_index) {
                        pattern_field_types.push((field.name.clone(), expr_field_info.1.clone()));
                    } else {
                        self.add_instruction(Instruction::Pop);
                        return Ok(false);
                    }
                }

                let pattern_type_id = self
                    .type_registry
                    .find_type(tuple_pattern.name.clone(), &pattern_field_types);

                if pattern_type_id != Some(*expression_type_id) {
                    self.add_instruction(Instruction::Pop);
                    return Ok(false);
                }

                for (field_index, field) in tuple_pattern.fields.iter().enumerate() {
                    self.add_instruction(Instruction::Duplicate);
                    self.add_instruction(Instruction::Get(field_index));

                    let field_type = Type::Resolved(pattern_field_types[field_index].1.clone());

                    if !self.compile_pattern_destructuring(field.pattern.clone(), &field_type)? {
                        self.add_instruction(Instruction::Pop);
                        return Ok(false);
                    }
                }

                self.add_instruction(Instruction::Pop);
                Ok(true)
            }
            _ => {
                self.add_instruction(Instruction::Pop);
                Ok(false)
            }
        }
    }

    fn compile_pattern_destructuring(
        &mut self,
        pattern: ast::Pattern,
        expression_type: &Type,
    ) -> Result<bool, Error> {
        match pattern {
            ast::Pattern::Identifier(name) => {
                self.add_instruction(Instruction::Store(name.clone()));
                self.define_variable(&name, expression_type.clone());
                Ok(true)
            }
            ast::Pattern::Wildcard => {
                self.add_instruction(Instruction::Pop);
                Ok(true)
            }
            ast::Pattern::Tuple(tuple_pattern) => {
                self.compile_tuple_destructuring_impl(tuple_pattern, expression_type)
            }
            ast::Pattern::Literal(literal_value) => {
                match literal_value {
                    ast::Literal::Integer(integer) => {
                        let index = self.vm.register_constant(Constant::Integer(integer));
                        self.add_instruction(Instruction::Constant(index));
                    }
                    ast::Literal::Binary(bytes) => {
                        let index = self.vm.register_constant(Constant::Binary(bytes));
                        self.add_instruction(Instruction::Constant(index));
                    }
                    ast::Literal::String(string) => {
                        let bytes = string.as_bytes().to_vec();
                        let index = self.vm.register_constant(Constant::Binary(bytes));
                        self.add_instruction(Instruction::Constant(index));
                    }
                }
                self.add_instruction(Instruction::Equal(2));
                Ok(true)
            }
            ast::Pattern::Partial(field_names) => {
                self.compile_partial_destructuring_impl(field_names, expression_type)
            }
            ast::Pattern::Star => self.compile_star_destructuring_impl(expression_type),
        }
    }

    fn compile_partial_destructuring(
        &mut self,
        field_names: Vec<String>,
        expression_type: &Type,
    ) -> Result<(), Error> {
        if self.compile_partial_destructuring_impl(field_names, expression_type)? {
            self.add_instruction(Instruction::Tuple(TypeId::OK, 0));
        } else {
            self.add_instruction(Instruction::Tuple(TypeId::NIL, 0));
        }
        Ok(())
    }

    fn compile_partial_destructuring_impl(
        &mut self,
        field_names: Vec<String>,
        expression_type: &Type,
    ) -> Result<bool, Error> {
        for field_name in field_names {
            self.add_instruction(Instruction::Duplicate);

            let (field_index, field_type) = match expression_type {
                Type::Resolved(types::Type::Tuple(type_id)) => {
                    if let Some(type_info) = self.type_registry.lookup_type(type_id) {
                        if let Some((index, (_, field_type))) = type_info
                            .1
                            .iter()
                            .enumerate()
                            .find(|(_, (name, _))| name.as_ref() == Some(&field_name))
                        {
                            (index, Type::Resolved(field_type.clone()))
                        } else {
                            self.add_instruction(Instruction::Pop);
                            return Ok(false);
                        }
                    } else {
                        self.add_instruction(Instruction::Pop);
                        return Ok(false);
                    }
                }
                _ => {
                    self.add_instruction(Instruction::Pop);
                    return Ok(false);
                }
            };

            self.add_instruction(Instruction::Get(field_index));
            self.add_instruction(Instruction::Store(field_name.clone()));
            self.define_variable(&field_name, field_type);
        }

        self.add_instruction(Instruction::Pop);
        Ok(true)
    }

    fn compile_star_destructuring(&mut self, expression_type: &Type) -> Result<(), Error> {
        if self.compile_star_destructuring_impl(expression_type)? {
            self.add_instruction(Instruction::Tuple(TypeId::OK, 0));
        } else {
            self.add_instruction(Instruction::Tuple(TypeId::NIL, 0));
        }
        Ok(())
    }

    fn compile_star_destructuring_impl(&mut self, expression_type: &Type) -> Result<bool, Error> {
        match expression_type {
            Type::Resolved(types::Type::Tuple(type_id)) => {
                let named_fields: Vec<(usize, String, types::Type)> = {
                    if let Some(type_info) = self.type_registry.lookup_type(type_id) {
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

                self.add_instruction(Instruction::Pop);
                Ok(true)
            }
            _ => {
                self.add_instruction(Instruction::Pop);
                Ok(false)
            }
        }
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

        narrow_types(branch_types)
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
                        if let Some(type_info) = self.type_registry.lookup_type(&type_id) {
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
                self.compile_block(block, Type::Resolved(types::Type::Tuple(TypeId::NIL)))
            }
            ast::Value::Parameter(parameter) => {
                self.compile_parameter(parameter, parameter_type.clone())
            }
            ast::Value::MemberAccess(member_access) => {
                self.compile_value_member_access(&member_access.target, member_access.accessors)
            }
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

    fn value_to_instructions(&mut self, value: &vm::Value) -> Result<Type, Error> {
        match value {
            vm::Value::Integer(int_value) => {
                let index = self.vm.register_constant(Constant::Integer(*int_value));
                self.add_instruction(Instruction::Constant(index));
                Ok(Type::Resolved(types::Type::Integer))
            }
            vm::Value::Binary(const_index) => {
                self.add_instruction(Instruction::Constant(*const_index));
                Ok(Type::Resolved(types::Type::Binary))
            }
            vm::Value::Tuple(type_id, fields) => {
                for field in fields {
                    self.value_to_instructions(field)?;
                }
                self.add_instruction(Instruction::Tuple(*type_id, fields.len()));
                Ok(Type::Resolved(types::Type::Tuple(*type_id)))
            }
            vm::Value::Function { function, captures } => {
                for capture in captures {
                    self.value_to_instructions(capture)?;
                }
                if let Some(func_def) = self.vm.get_functions().get(*function).cloned() {
                    let func_index = self.vm.register_function(func_def);
                    self.add_instruction(Instruction::Function(func_index));
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
        if self.import_stack.contains(&module_path.to_string()) {
            return Err(Error::FeatureUnsupported(
                "Circular import detected".to_string(),
            ));
        }

        if let Some(cached_value) = self.evaluation_cache.get(module_path).cloned() {
            return self.value_to_instructions(&cached_value);
        }

        self.import_stack.push(module_path.to_string());
        let result = self.import_module_internal(module_path);
        self.import_stack.pop();

        result
    }

    fn load_and_cache_ast(&mut self, module_path: &str) -> Result<ast::Program, Error> {
        if let Some(cached_ast) = self.ast_cache.get(module_path).cloned() {
            return Ok(cached_ast);
        }

        let content = self
            .module_loader
            .load(module_path, self.module_path.as_deref())
            .map_err(Error::ModuleLoad)?;

        let parsed = parser::parse(&content).map_err(|_e| {
            Error::ModuleParse(format!("Failed to parse imported module: {}", module_path))
        })?;

        self.ast_cache
            .insert(module_path.to_string(), parsed.clone());

        Ok(parsed)
    }

    fn import_module_internal(&mut self, module_path: &str) -> Result<Type, Error> {
        let parsed = self.load_and_cache_ast(module_path)?;

        let saved_instructions = std::mem::take(&mut self.instructions);
        let saved_scopes = std::mem::take(&mut self.scopes);
        let saved_type_aliases = std::mem::take(&mut self.type_aliases);

        self.scopes = vec![HashMap::new()];
        self.type_aliases = HashMap::new();

        for statement in parsed.statements {
            self.compile_statement(statement)?;
        }

        let module_instructions = std::mem::take(&mut self.instructions);

        self.instructions = saved_instructions;
        self.scopes = saved_scopes;
        self.type_aliases = saved_type_aliases;

        let value = self
            .vm
            .execute_instructions(module_instructions, true)
            .map_err(|_e| Error::FeatureUnsupported("Module execution failed".to_string()))?
            .unwrap_or(vm::Value::Tuple(TypeId::NIL, vec![]));

        self.evaluation_cache
            .insert(module_path.to_string(), value.clone());
        self.value_to_instructions(&value)
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
                let type_registry = &self.type_registry;
                let tuple_type = type_registry.lookup_type(&type_id).unwrap();
                let index = tuple_type
                    .1
                    .iter()
                    .position(|f| f.0 == Some(field_name.clone()))
                    .ok_or(Error::FieldNotFound {
                        field_name: field_name.clone(),
                        type_name: format!("{:?}", type_id),
                    })?;
                let result_type = tuple_type.1[index].1.clone();
                self.add_instruction(Instruction::Get(index));
                Ok(Type::Resolved(result_type))
            }
            _ => Err(Error::FieldAccessOnNonTuple { field_name }),
        }
    }

    fn compile_operation_positional_access(
        &mut self,
        index: usize,
        value_type: Type,
    ) -> Result<Type, Error> {
        match value_type {
            Type::Resolved(types::Type::Tuple(type_id)) => {
                let type_registry = &self.type_registry;
                let tuple_type = type_registry.lookup_type(&type_id).unwrap();
                let result_type = tuple_type.1[index].1.clone();
                self.add_instruction(Instruction::Get(index));
                Ok(Type::Resolved(result_type))
            }
            _ => Err(Error::PositionalAccessOnNonTuple { index }),
        }
    }

    fn compile_operation_tail_call(&mut self, identifier: &str) -> Result<Type, Error> {
        if identifier.is_empty() {
            self.add_instruction(Instruction::TailCall(true));
            Ok(Type::Unresolved(vec![]))
        } else {
            self.add_instruction(Instruction::Load(identifier.to_string()));
            self.add_instruction(Instruction::TailCall(false));

            match self.lookup_variable(identifier) {
                Some(function_type) => Ok(function_type),
                None => Err(Error::VariableUndefined(identifier.to_string())),
            }
        }
    }

    fn compile_operator(
        &mut self,
        operator: ast::Operator,
        value_type: Type,
    ) -> Result<Type, Error> {
        match value_type {
            Type::Resolved(types::Type::Tuple(type_id)) => {
                let type_registry = &self.type_registry;
                let tuple_type = type_registry.lookup_type(&type_id).ok_or(
                    Error::OperatorTypeNotInRegistry {
                        type_id: format!("{:?}", type_id),
                    },
                )?;
                let tuple_size = tuple_type.1.len();

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
            _ => Err(Error::OperatorOnNonTuple {
                operator: format!("{:?}", operator),
            }),
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
            .ok_or(Error::VariableUndefined(target.to_string()))?;

        for accessor in accessors {
            match last_type {
                Type::Resolved(types::Type::Tuple(type_id)) => {
                    let type_registry = &self.type_registry;
                    let tuple_type = type_registry.lookup_type(&type_id).unwrap();
                    let index = match accessor {
                        ast::AccessPath::Field(field_name) => tuple_type
                            .1
                            .iter()
                            .position(|f| f.0 == Some(field_name.clone()))
                            .ok_or(Error::MemberFieldNotFound {
                                field_name: field_name.clone(),
                                target: target.to_string(),
                            })?,
                        ast::AccessPath::Index(index) => index,
                    };
                    let new_type = tuple_type.1[index].1.clone();
                    self.add_instruction(Instruction::Get(index));
                    last_type = Type::Resolved(new_type);
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

    fn compile_operation_member_access(
        &mut self,
        target: &str,
        accessors: Vec<ast::AccessPath>,
        value_type: Type,
    ) -> Result<Type, Error> {
        self.add_instruction(Instruction::Load(target.to_string()));

        let mut last_type = self
            .lookup_variable(target)
            .ok_or(Error::VariableUndefined(target.to_string()))?;

        for accessor in accessors {
            match last_type {
                Type::Resolved(types::Type::Tuple(type_id)) => {
                    let type_registry = &self.type_registry;
                    let tuple_type = type_registry.lookup_type(&type_id).unwrap();
                    let index = match accessor {
                        ast::AccessPath::Field(field_name) => tuple_type
                            .1
                            .iter()
                            .position(|f| f.0 == Some(field_name.clone()))
                            .ok_or(Error::MemberFieldNotFound {
                                field_name: field_name.clone(),
                                target: target.to_string(),
                            })?,
                        ast::AccessPath::Index(index) => index,
                    };
                    let new_type = tuple_type.1[index].1.clone();
                    self.add_instruction(Instruction::Get(index));
                    last_type = Type::Resolved(new_type);
                }
                _ => {
                    return Err(Error::MemberAccessOnNonTuple {
                        target: target.to_string(),
                    });
                }
            }
        }

        match &last_type {
            Type::Resolved(types::Type::Function(func_type)) => {
                let parameter_type = &func_type.parameter[0]; // For now, take first parameter type
                let return_type = &func_type.result[0]; // For now, take first result type
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

                if value_inner_type != parameter_type {
                    return Err(Error::TypeMismatch {
                        expected: format!("{:?}", parameter_type),
                        found: format!("{:?}", value_inner_type),
                    });
                }

                self.add_instruction(Instruction::Call);
                Ok(Type::Resolved(return_type.clone()))
            }
            Type::Unresolved(function_types) => {
                if function_types.is_empty() {
                    Err(Error::TypeUnresolved(
                        "Cannot call function with unresolved type".to_string(),
                    ))
                } else {
                    // For union function types, try to find a matching variant
                    // For now, we'll be permissive and allow the call
                    self.add_instruction(Instruction::Call);
                    // Extract all possible return types from function variants
                    let mut return_types = Vec::new();
                    for func_type in function_types {
                        if let types::Type::Function(func_type_inner) = func_type {
                            for return_type in &func_type_inner.result {
                                return_types.push(return_type.clone());
                            }
                        }
                    }
                    if return_types.is_empty() {
                        Err(Error::TypeUnresolved(
                            "No function variants found in union".to_string(),
                        ))
                    } else {
                        Ok(Type::Unresolved(return_types))
                    }
                }
            }
            _ => Err(Error::ChainValueUnused),
        }
    }

    fn cartesian_product_tuple_types(
        &mut self,
        tuple_name: &Option<String>,
        field_variants: Vec<(Option<String>, Vec<types::Type>)>,
    ) -> Vec<types::Type> {
        // Handle empty fields case
        if field_variants.is_empty() {
            let type_id = self.type_registry.register_type(tuple_name.clone(), vec![]);
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
}
