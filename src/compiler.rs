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
pub use pattern::MatchCertainty;
pub use typing::{TupleAccessor, union_types};

use crate::{
    ast,
    builtins::BUILTIN_REGISTRY,
    bytecode::{Constant, Function, Instruction, TypeId},
    modules::{ModuleError, ModuleLoader},
    program::Program,
    types::{CallableType, ProcessType, Type, TypeLookup},
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

    // Positional access
    PositionalIndexOutOfBounds {
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

    // Process messaging errors
    ReceiveTypeMismatch {
        first: String,
        second: String,
    },

    // Internal consistency errors
    InternalError {
        message: String,
    },
}

struct Scope {
    variables: HashMap<String, (Type, usize)>,
    parameter: Option<(Type, usize)>,
}

impl Scope {
    fn new(variables: HashMap<String, (Type, usize)>, parameter: Option<(Type, usize)>) -> Self {
        Self {
            variables,
            parameter,
        }
    }
}

pub struct Compiler<'a> {
    // Core components
    codegen: InstructionBuilder,
    type_context: TypeContext<'a>,
    module_cache: ModuleCache,

    // State management
    scopes: Vec<Scope>,
    local_count: usize,
    program: &'a mut Program,
    module_loader: &'a dyn ModuleLoader,
    module_path: Option<PathBuf>,

    // Counter for nested ripple contexts in tuple construction
    ripple_depth: usize,

    // Track the receive type of the function currently being compiled
    current_receive_type: Type,
}

impl<'a> Compiler<'a> {
    pub fn compile(
        program: ast::Program,
        type_aliases: &'a mut HashMap<String, Type>,
        module_loader: &'a dyn ModuleLoader,
        compiled_program: &'a mut Program,
        module_path: Option<PathBuf>,
        existing_variables: Option<&HashMap<String, (Type, usize)>>,
        parameter_type: Type,
    ) -> Result<(Vec<Instruction>, Type, HashMap<String, (Type, usize)>), Error> {
        let mut compiler = Self {
            codegen: InstructionBuilder::new(),
            type_context: TypeContext::new(type_aliases),
            module_cache: ModuleCache::new(),
            scopes: vec![],
            local_count: 0,
            program: compiled_program,
            module_loader,
            module_path,
            ripple_depth: 0,
            current_receive_type: Type::never(),
        };

        // Prepare scope variables from existing variables
        let mut scope_variables = HashMap::new();
        if let Some(variables) = existing_variables {
            for (name, (var_type, index)) in variables {
                scope_variables.insert(name.clone(), (var_type.clone(), *index));
            }

            compiler.local_count = variables
                .values()
                .map(|(_, index)| index)
                .copied()
                .max()
                .map(|max_index| max_index + 1)
                .unwrap_or(0);
        }

        // Prepare parameter (always allocate slot)
        let param_local = compiler.local_count;
        compiler.local_count += 1;
        let scope_parameter = Some((parameter_type, param_local));

        // Initialize scope with variables and parameter
        compiler.scopes = vec![Scope::new(scope_variables, scope_parameter.clone())];

        // Always emit instructions for parameter slot
        compiler.codegen.add_instruction(Instruction::Allocate(1));
        compiler
            .codegen
            .add_instruction(Instruction::Store(param_local));

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
        let mut result_type = Type::nil();
        for (i, statement) in program.statements.into_iter().enumerate() {
            let is_last = i == num_statements - 1;
            let is_expression = matches!(&statement, ast::Statement::Expression(_));

            let statement_type = compiler.compile_statement(statement)?;

            // Track the type of the last statement
            if is_last {
                result_type = statement_type;
            }

            // Pop intermediate expression results, keeping only the last one
            if !is_last && is_expression {
                compiler.codegen.add_instruction(Instruction::Pop);
            }
        }

        // Extract variables with types, excluding internal ones (like ripple variables ~0, ~1, etc.)
        let variables = compiler.scopes[0]
            .variables
            .iter()
            .filter(|(name, _)| !name.starts_with('~'))
            .map(|(name, (var_type, index))| (name.clone(), (var_type.clone(), *index)))
            .collect();

        Ok((compiler.codegen.instructions, result_type, variables))
    }

    fn compile_statement(&mut self, statement: ast::Statement) -> Result<Type, Error> {
        match statement {
            ast::Statement::TypeAlias {
                name,
                type_definition,
            } => {
                self.compile_type_alias(&name, type_definition)?;
                Ok(Type::nil())
            }
            ast::Statement::TypeImport {
                pattern,
                module_path,
            } => {
                self.compile_type_import(pattern, &module_path)?;
                Ok(Type::nil())
            }
            ast::Statement::Expression(expression) => self.compile_expression(expression, None),
        }
    }

    fn compile_type_alias(&mut self, name: &str, type_definition: ast::Type) -> Result<(), Error> {
        let resolved_type = self
            .type_context
            .resolve_ast_type(type_definition, self.program)?;

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
            self.program,
        )
    }

    fn compile_literal(&mut self, literal: ast::Literal) -> Result<Type, Error> {
        match literal {
            ast::Literal::Integer(integer) => {
                let index = self.program.register_constant(Constant::Integer(integer));
                self.codegen.add_instruction(Instruction::Constant(index));
                Ok(Type::Integer)
            }
            ast::Literal::Binary(bytes) => {
                // TODO: is clone bad?
                let index = self
                    .program
                    .register_constant(Constant::Binary(bytes.clone()));
                self.codegen.add_instruction(Instruction::Constant(index));
                Ok(Type::Binary)
            }
        }
    }

    fn compile_create_tuple(
        &mut self,
        tuple_name: Option<String>,
        fields: Vec<ast::TupleField>,
    ) -> Result<Type, Error> {
        Self::check_field_name_duplicates(&fields, |f| f.name.as_ref())?;

        // Compile field values and collect their types
        let mut field_types = Vec::new();
        for field in &fields {
            let field_type = match &field.value {
                ast::FieldValue::Chain(chain) => {
                    // Value tuples don't have a value context
                    self.compile_chain(chain.clone(), None)?
                }
                ast::FieldValue::Ripple => {
                    // Ripple is only valid if we're inside an operation context
                    if self.ripple_depth == 0 {
                        return Err(Error::FeatureUnsupported(
                            "Ripple cannot be used outside of an operation context".to_string(),
                        ));
                    }
                    // Use the stored ripple value from the enclosing operation
                    let ripple_var_name = format!("~{}", self.ripple_depth - 1);
                    // We need to get the type and index of the ripple value
                    let (ripple_type, ripple_index) = self
                        .lookup_variable(&self.scopes, &ripple_var_name, &[])
                        .ok_or_else(|| Error::InternalError {
                            message: "Ripple value type not found in scope".to_string(),
                        })?;
                    self.codegen
                        .add_instruction(Instruction::Load(ripple_index));
                    ripple_type
                }
            };
            field_types.push((field.name.clone(), field_type));
        }

        // Register the tuple type with potentially union field types
        let type_id = self.program.register_type(tuple_name, field_types);
        self.codegen.add_instruction(Instruction::Tuple(type_id));

        Ok(Type::Tuple(type_id))
    }

    // Helper function to check if a tuple contains ripple operations
    fn tuple_contains_ripple(tuple: &ast::Tuple) -> bool {
        tuple.fields.iter().any(|field| {
            match &field.value {
                ast::FieldValue::Ripple => true,
                ast::FieldValue::Chain(chain) => {
                    // Check for nested tuples that contain ripples
                    chain.terms.iter().any(|term| {
                        if let ast::Term::Tuple(nested_tuple) = term {
                            Self::tuple_contains_ripple(nested_tuple)
                        } else {
                            false
                        }
                    })
                }
            }
        })
    }

    fn compile_wrap_tupple(
        &mut self,
        name: Option<String>,
        fields: Vec<ast::TupleField>,
        value_type: Type,
    ) -> Result<Type, Error> {
        // The parser ensures only tuples with ripples are parsed as Operation::Tuple
        // so we don't need to check for ripples here

        // Use a unique variable name for each ripple context to avoid conflicts
        let ripple_var_name = format!("~{}", self.ripple_depth);
        self.ripple_depth += 1;
        let ripple_index = self.define_variable(&ripple_var_name, &[], value_type.clone());
        self.codegen.add_instruction(Instruction::Allocate(1));
        self.codegen
            .add_instruction(Instruction::Store(ripple_index));

        Self::check_field_name_duplicates(&fields, |f| f.name.as_ref())?;

        // Compile field values and collect their types
        let mut field_types = Vec::new();
        for field in &fields {
            let field_type = match &field.value {
                ast::FieldValue::Ripple => {
                    // Use the stored ripple value
                    let ripple_var_name = format!("~{}", self.ripple_depth - 1);
                    let (ripple_type, ripple_index) = self
                        .lookup_variable(&self.scopes, &ripple_var_name, &[])
                        .ok_or_else(|| Error::InternalError {
                            message: "Ripple value not found in scope".to_string(),
                        })?;
                    self.codegen
                        .add_instruction(Instruction::Load(ripple_index));
                    ripple_type.clone()
                }
                ast::FieldValue::Chain(chain) => {
                    // Regular chain compilation
                    self.compile_chain(chain.clone(), None)?
                }
            };

            field_types.push((field.name.clone(), field_type));
        }

        // Pop the ripple context
        self.ripple_depth -= 1;

        // Register the tuple type with potentially union field types
        let type_id = self.program.register_type(name, field_types);
        self.codegen.add_instruction(Instruction::Tuple(type_id));

        // Clear the ripple local now that we're done with it
        self.codegen.add_instruction(Instruction::Clear(1));
        self.local_count -= 1;

        Ok(Type::Tuple(type_id))
    }

    fn extract_receive_type(&mut self, block: &ast::Block) -> Result<Type, Error> {
        let mut receive_types = Vec::new();
        self.collect_receive_types(block, &mut receive_types)?;

        if receive_types.is_empty() {
            return Ok(Type::never());
        }

        // Check all receives have the same type
        let first_type = &receive_types[0];
        for ty in &receive_types[1..] {
            if ty != first_type {
                return Err(Error::ReceiveTypeMismatch {
                    first: format!("{:?}", first_type),
                    second: format!("{:?}", ty),
                });
            }
        }

        Ok(first_type.clone())
    }

    fn collect_receive_types(
        &mut self,
        block: &ast::Block,
        receive_types: &mut Vec<Type>,
    ) -> Result<(), Error> {
        for branch in &block.branches {
            self.collect_receive_types_from_expression(&branch.condition, receive_types)?;
            if let Some(consequence) = &branch.consequence {
                self.collect_receive_types_from_expression(consequence, receive_types)?;
            }
        }
        Ok(())
    }

    fn collect_receive_types_from_expression(
        &mut self,
        expression: &ast::Expression,
        receive_types: &mut Vec<Type>,
    ) -> Result<(), Error> {
        for chain in &expression.chains {
            self.collect_receive_types_from_chain(chain, receive_types)?;
        }
        Ok(())
    }

    fn collect_receive_types_from_chain(
        &mut self,
        chain: &ast::Chain,
        receive_types: &mut Vec<Type>,
    ) -> Result<(), Error> {
        for term in &chain.terms {
            self.collect_receive_types_from_term(term, receive_types)?;
        }
        Ok(())
    }

    fn collect_receive_types_from_term(
        &mut self,
        term: &ast::Term,
        receive_types: &mut Vec<Type>,
    ) -> Result<(), Error> {
        match term {
            ast::Term::Receive(receive) => {
                let message_type = self
                    .type_context
                    .resolve_ast_type(receive.type_def.clone(), self.program)?;
                receive_types.push(message_type);
                // Also check nested receives in the receive block
                if let Some(block) = &receive.block {
                    self.collect_receive_types(block, receive_types)?;
                }
            }
            ast::Term::Block(block) => {
                self.collect_receive_types(block, receive_types)?;
            }
            ast::Term::FunctionDefinition(_) => {
                // Don't recurse into nested function definitions - they have their own receive types
            }
            ast::Term::Tuple(tuple) => {
                // Check tuple fields
                for field in &tuple.fields {
                    if let ast::FieldValue::Chain(chain) = &field.value {
                        self.collect_receive_types_from_chain(chain, receive_types)?;
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn compile_function(
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
            &|name, accessors| {
                // Check if the full path (base + accessors) is defined, or just the base
                self.lookup_variable(&self.scopes, name, accessors)
                    .is_some()
                    || self.lookup_variable(&self.scopes, name, &[]).is_some()
            },
        );

        // Deduplicate captures (same base + accessors may appear multiple times)
        let mut unique_captures: Vec<variables::Capture> = Vec::new();
        for capture in captures {
            if !unique_captures.contains(&capture) {
                unique_captures.push(capture);
            }
        }

        // Resolve capture values in current scope
        // For captures with accessors, evaluate the access and store in temporary locals
        let mut capture_locals = Vec::new();
        let mut capture_temps = Vec::new(); // Track which locals we allocated for captures

        for capture in &unique_captures {
            // First check if the full path is available (for nested captures)
            if let Some((_full_type, full_index)) =
                self.lookup_variable(&self.scopes, &capture.base, &capture.accessors)
            {
                // The full path is already captured, just reuse it
                capture_locals.push(full_index);
            } else if let Some((var_type, base_index)) =
                self.lookup_variable(&self.scopes, &capture.base, &[])
            {
                if capture.accessors.is_empty() {
                    // Simple capture - just reference the existing local
                    capture_locals.push(base_index);
                } else {
                    // Capture with accessors - evaluate the access
                    // Load the base variable
                    self.codegen.add_instruction(Instruction::Load(base_index));

                    // Apply accessors to get the final value
                    let _accessed_type =
                        self.compile_accessor(var_type, capture.accessors.clone(), &capture.base)?;

                    // Store in temporary local
                    let temp_local = self.local_count;
                    self.local_count += 1;
                    self.codegen.add_instruction(Instruction::Allocate(1));
                    self.codegen.add_instruction(Instruction::Store(temp_local));

                    capture_locals.push(temp_local);
                    capture_temps.push(temp_local);
                }
            }
        }

        let parameter_type = match &function_definition.parameter_type {
            Some(t) => self
                .type_context
                .resolve_ast_type(t.clone(), self.program)?,
            None => Type::nil(),
        };

        // Extract receive type from function body
        let receive_type = self.extract_receive_type(&function_definition.body)?;

        let saved_instructions = std::mem::take(&mut self.codegen.instructions);
        let saved_scopes = std::mem::take(&mut self.scopes);
        let saved_local_count = self.local_count;
        let saved_receive_type = self.current_receive_type.clone();

        self.scopes = vec![Scope::new(HashMap::new(), None)];
        self.codegen.instructions = Vec::new();
        self.local_count = 0;
        self.current_receive_type = receive_type.clone();

        // Define captures as first locals in function body scope
        for capture in &unique_captures {
            // Determine the type of the captured value
            // First check if the full path is already available (for nested captures)
            let capture_type = if let Some((full_type, _)) =
                self.lookup_variable(&saved_scopes, &capture.base, &capture.accessors)
            {
                // The full path is already available from parent, use its type
                full_type
            } else if capture.accessors.is_empty() {
                // Simple capture - use the base variable's type
                if let Some((var_type, _)) = self.lookup_variable(&saved_scopes, &capture.base, &[])
                {
                    var_type
                } else {
                    continue;
                }
            } else {
                // Capture with accessors - need to compute the accessed type
                if let Some((var_type, _)) = self.lookup_variable(&saved_scopes, &capture.base, &[])
                {
                    // Use compile_accessor logic to determine type
                    // We need to compute this without generating bytecode
                    let mut last_type = var_type;

                    for accessor in &capture.accessors {
                        let tuple_types = last_type.extract_tuple_types();
                        let field_types = match accessor {
                            ast::AccessPath::Field(field_name) => {
                                match self.get_field_types_by_name(&tuple_types, field_name) {
                                    Ok(results) if !results.is_empty() => {
                                        results.into_iter().map(|(_, t)| t).collect()
                                    }
                                    _ => continue,
                                }
                            }
                            ast::AccessPath::Index(index) => {
                                match self.get_field_types_at_position(&tuple_types, *index) {
                                    Ok(types) => types,
                                    _ => continue,
                                }
                            }
                        };
                        last_type = Type::from_types(field_types);
                    }
                    last_type
                } else {
                    continue;
                }
            };

            self.define_variable(&capture.base, &capture.accessors, capture_type);
        }

        let mut parameter_fields = HashMap::new();
        if let Some(ast::Type::Tuple(tuple_type)) = &function_definition.parameter_type {
            for (field_index, field) in tuple_type.fields.iter().enumerate() {
                if let Some(field_name) = &field.name {
                    let field_type = self
                        .type_context
                        .resolve_ast_type(field.type_def.clone(), self.program)?;
                    parameter_fields.insert(field_name.clone(), (field_index, field_type));
                }
            }
        }
        let body_type =
            self.compile_block(function_definition.body, parameter_type.clone(), None)?;
        self.codegen.add_instruction(Instruction::Return);

        let func_type = CallableType {
            parameter: parameter_type,
            result: body_type,
            receive: receive_type.clone(),
        };

        let function_instructions = std::mem::take(&mut self.codegen.instructions);
        let function_index = self.program.register_function(Function {
            instructions: function_instructions,
            function_type: Some(func_type.clone()),
            captures: capture_locals,
        });

        self.codegen.instructions = saved_instructions;
        self.scopes = saved_scopes;
        self.local_count = saved_local_count;
        self.current_receive_type = saved_receive_type;

        self.codegen
            .add_instruction(Instruction::Function(function_index));

        // Clean up temporary locals we allocated for captures with accessors
        if !capture_temps.is_empty() {
            self.codegen
                .add_instruction(Instruction::Clear(capture_temps.len()));
            self.local_count -= capture_temps.len();
        }

        let function_type = Type::Callable(Box::new(func_type));

        Ok(function_type)
    }

    fn compile_destructure(
        &mut self,
        term: ast::Term,
        value_type: Type,
        on_no_match: Option<usize>,
    ) -> Result<Type, Error> {
        let start_jump_addr = self.codegen.emit_jump_placeholder();
        let fail_jump_addr = self.codegen.emit_jump_placeholder();

        self.codegen.patch_jump_to_here(start_jump_addr);

        // Analyze pattern to get bindings without generating code yet
        let (certainty, bindings, binding_sets) =
            pattern::analyze_pattern(&self.type_context, self.program, &term, &value_type)?;

        match certainty {
            MatchCertainty::WontMatch => {
                self.codegen.add_instruction(Instruction::Pop);
                self.codegen
                    .add_instruction(Instruction::Tuple(TypeId::NIL));
                Ok(Type::nil())
            }
            MatchCertainty::WillMatch | MatchCertainty::MightMatch => {
                // Pre-allocate locals for all bindings
                let mut bindings_map = std::collections::HashMap::new();

                for (variable_name, variable_type) in &bindings {
                    let local_index = self.local_count;
                    self.local_count += 1;
                    bindings_map.insert(variable_name.clone(), local_index);

                    // Register in scope
                    if let Some(scope) = self.scopes.last_mut() {
                        scope
                            .variables
                            .insert(variable_name.clone(), (variable_type.clone(), local_index));
                    }
                }

                // Allocate locals in VM
                let num_bindings = bindings.len();
                if num_bindings > 0 {
                    self.codegen
                        .add_instruction(Instruction::Allocate(num_bindings));
                }

                // Now generate pattern matching code with pre-allocated locals
                // Use on_no_match if provided (for receive blocks), otherwise use fail_jump_addr
                let fail_target = on_no_match.unwrap_or(fail_jump_addr);
                pattern::generate_pattern_code(
                    &mut self.codegen,
                    self.program,
                    &mut self.local_count,
                    Some(&bindings_map),
                    &binding_sets,
                    fail_target,
                )?;

                self.codegen.add_instruction(Instruction::Pop);
                self.codegen.add_instruction(Instruction::Tuple(TypeId::OK));
                let success_jump_addr = self.codegen.emit_jump_placeholder();

                // Only patch fail_jump_addr if we didn't use on_no_match
                if on_no_match.is_none() {
                    self.codegen.patch_jump_to_here(fail_jump_addr);
                }
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

    fn compile_block(
        &mut self,
        block: ast::Block,
        parameter_type: Type,
        on_no_match: Option<usize>,
    ) -> Result<Type, Error> {
        let mut next_branch_jumps = Vec::new();
        let mut end_jumps = Vec::new();
        let mut branch_types = Vec::new();
        let mut branch_starts = Vec::new();

        // Record locals count before block
        let locals_before = self.local_count;

        // Allocate local for block parameter
        let param_local = self.local_count;
        self.local_count += 1;

        // Allocate and store parameter from stack
        self.codegen.add_instruction(Instruction::Allocate(1));
        self.codegen
            .add_instruction(Instruction::Store(param_local));

        // Push new scope with parameter
        self.scopes.push(Scope::new(
            HashMap::new(),
            Some((parameter_type.clone(), param_local)),
        ));

        for (i, branch) in block.branches.iter().enumerate() {
            let is_last_branch = i == block.branches.len() - 1;

            branch_starts.push(self.codegen.instructions.len());

            if i > 0 {
                self.codegen.add_instruction(Instruction::Pop);
                // Don't emit Clear here - branch variables are only allocated if the branch matches
                // So if we jump to this branch, the previous branch's variables were never allocated
                // Reset local count to after parameter (locals from previous branch are "forgotten")
                self.local_count = param_local + 1;
                // Clear scope so branch can reuse variable names
                self.scopes
                    .last_mut()
                    .ok_or_else(|| Error::InternalError {
                        message: "No scope available when compiling block branch".to_string(),
                    })?
                    .variables
                    .clear();
            }

            // Compile the condition expression - it can use ~> to access the parameter
            let condition_type = self.compile_expression(branch.condition.clone(), on_no_match)?;

            // If condition is compile-time NIL (won't match), skip this branch entirely
            if condition_type.as_concrete() == Some(&Type::nil()) {
                // Don't add to branch_types, continue to next branch
                continue;
            }

            if let Some(ref consequence) = branch.consequence {
                // Branch has a consequence - compile it
                let next_branch_jump = self.codegen.emit_duplicate_jump_if_nil_pop();
                next_branch_jumps.push((next_branch_jump, i + 1));

                // Compile the consequence - parameter not available (nil type)
                let consequence_type = self.compile_expression(consequence.clone(), None)?;
                branch_types.push(consequence_type);

                // Clear branch-specific locals, but keep the parameter
                let branch_specific_locals = self.local_count - (param_local + 1);
                if branch_specific_locals > 0 {
                    self.codegen
                        .add_instruction(Instruction::Clear(branch_specific_locals));
                }

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

                // Clear branch-specific locals, but keep the parameter
                let branch_specific_locals = self.local_count - (param_local + 1);
                if branch_specific_locals > 0 {
                    self.codegen
                        .add_instruction(Instruction::Clear(branch_specific_locals));
                }
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

        // Clear the parameter (branches have already cleared their specific locals)
        self.codegen.add_instruction(Instruction::Clear(1));

        // Reset local count so future variables reuse these indexes
        self.local_count = locals_before;

        // Pop scope
        self.scopes.pop();

        for (jump_addr, next_branch_idx) in next_branch_jumps {
            let target_addr = if next_branch_idx >= branch_starts.len() {
                // No more branches - jump to on_no_match if provided, otherwise end
                on_no_match.unwrap_or(end_addr)
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
        on_no_match: Option<usize>,
    ) -> Result<Type, Error> {
        let mut last_type = None;
        let mut end_jumps = Vec::new();

        for (i, chain) in expression.chains.iter().enumerate() {
            let chain_type = self.compile_chain(chain.clone(), on_no_match)?;

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

    fn compile_chain(
        &mut self,
        chain: ast::Chain,
        on_no_match: Option<usize>,
    ) -> Result<Type, Error> {
        // If this is a continuation chain (starts with ~>), load the parameter
        let mut current_type = if chain.continuation {
            // Continuation chains explicitly use the parameter from scope
            let (parameter_type, param_local) = self.get_parameter()?;
            self.codegen.add_instruction(Instruction::Load(param_local));
            Some(parameter_type)
        } else {
            None
        };

        for term in chain.terms {
            current_type = Some(self.compile_term(term, current_type, on_no_match)?);
        }

        current_type.ok_or_else(|| Error::InternalError {
            message: "Chain compiled with no terms and no continuation".to_string(),
        })
    }

    fn value_to_instructions(&mut self, value: &Value) -> Result<Type, Error> {
        match value {
            Value::Integer(int_value) => {
                let index = self
                    .program
                    .register_constant(Constant::Integer(*int_value));
                self.codegen.add_instruction(Instruction::Constant(index));
                Ok(Type::Integer)
            }
            Value::Binary(binary_ref) => {
                match binary_ref {
                    BinaryRef::Constant(const_index) => {
                        self.codegen
                            .add_instruction(Instruction::Constant(*const_index));
                    }
                    BinaryRef::Heap(arc_bytes) => {
                        // Create a constant from the heap binary
                        let constant = crate::bytecode::Constant::Binary(arc_bytes.to_vec());
                        let index = self.program.register_constant(constant);
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
            Value::Function(function, captures) => {
                // Get the function definition
                let func_def = self.program.get_function(*function).cloned().ok_or(
                    Error::FeatureUnsupported("Invalid function reference".to_string()),
                )?;

                // If we have captures, store them in locals
                let mut capture_locals = Vec::new();
                if !captures.is_empty() {
                    // Store each capture value in locals
                    for value in captures {
                        // Recursively convert the captured value to instructions
                        self.value_to_instructions(value)?;
                        // Allocate a local and store it
                        let local_index = self.local_count;
                        self.local_count += 1;
                        self.codegen.add_instruction(Instruction::Allocate(1));
                        self.codegen
                            .add_instruction(Instruction::Store(local_index));
                        capture_locals.push(local_index);
                    }
                }

                // Register function with new capture locals for this context
                let func_index = self.program.register_function(Function {
                    instructions: func_def.instructions,
                    function_type: func_def.function_type,
                    captures: capture_locals,
                });

                // Emit the function instruction
                self.codegen
                    .add_instruction(Instruction::Function(func_index));

                self.function_to_type(func_index)
            }
            Value::Builtin(name) => {
                let builtin_index = self.program.register_builtin(name.to_string());
                self.codegen
                    .add_instruction(Instruction::Builtin(builtin_index));

                if let Some((param_type, result_type)) =
                    BUILTIN_REGISTRY.resolve_signature(name, self.program)
                {
                    Ok(Type::Callable(Box::new(CallableType {
                        parameter: param_type,
                        result: result_type,
                        receive: Type::never(),
                    })))
                } else {
                    Err(Error::BuiltinUndefined(name.to_string()))
                }
            }
            Value::Pid(_) => Err(Error::FeatureUnsupported(
                "Cannot use Pid in constant context".to_string(),
            )),
        }
    }

    fn compile_import(&mut self, module_path: &str) -> Result<Type, Error> {
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
            let value = self.import_module(module_path)?;
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

    fn import_module(&mut self, module_path: &str) -> Result<Value, Error> {
        // Parse the module
        let parsed = self.module_cache.load_and_cache_ast(
            module_path,
            self.module_loader,
            self.module_path.as_ref(),
        )?;

        // Save current compiler state
        let saved_instructions = std::mem::take(&mut self.codegen.instructions);
        let saved_scopes = std::mem::take(&mut self.scopes);
        let saved_local_count = self.local_count;
        let saved_type_aliases = self.type_context.type_aliases.clone();

        // Reset to clean state for module compilation
        self.scopes = vec![Scope::new(HashMap::new(), None)];
        self.local_count = 0;
        self.type_context.type_aliases.clear();

        for statement in parsed.statements {
            self.compile_statement(statement)?;
        }

        // Get the compiled module instructions
        let module_instructions = std::mem::take(&mut self.codegen.instructions);

        // Restore original compiler state
        self.codegen.instructions = saved_instructions;
        self.scopes = saved_scopes;
        self.local_count = saved_local_count;
        *self.type_context.type_aliases = saved_type_aliases;

        // Execute the module instructions to get the runtime value
        // Create a temporary VM for execution
        let temp_vm = VM::new(self.program.clone());
        let value = temp_vm
            .execute_instructions(module_instructions, None, None)
            .map_err(|e| Error::ModuleExecution {
                module_path: module_path.to_string(),
                error: e,
            })?
            .unwrap_or(Value::nil());

        Ok(value)
    }

    fn compile_term(
        &mut self,
        term: ast::Term,
        value_type: Option<Type>,
        on_no_match: Option<usize>,
    ) -> Result<Type, Error> {
        match term {
            ast::Term::Literal(literal) => {
                if let Some(val_type) = value_type {
                    // Treat as pattern match when value is present
                    self.compile_destructure(ast::Term::Literal(literal), val_type, on_no_match)
                } else {
                    // Compile as value when no value present
                    self.compile_literal(literal)
                }
            }
            ast::Term::Identifier(name) => {
                if let Some(val_type) = value_type {
                    // Treat as pattern match/destructuring when value is present
                    self.compile_destructure(ast::Term::Identifier(name), val_type, on_no_match)
                } else {
                    // Load the variable when no value present
                    let (var_type, index) = self
                        .lookup_variable(&self.scopes, &name, &[])
                        .ok_or_else(|| Error::VariableUndefined(name.clone()))?;
                    self.codegen.add_instruction(Instruction::Load(index));
                    Ok(var_type)
                }
            }
            ast::Term::Tuple(tuple) => {
                if let Some(val_type) = value_type {
                    // If tuple contains ripples, it's an operation; otherwise pattern match
                    if Self::tuple_contains_ripple(&tuple) {
                        self.compile_wrap_tupple(tuple.name, tuple.fields, val_type)
                    } else {
                        // Treat as pattern match
                        self.compile_destructure(ast::Term::Tuple(tuple), val_type, on_no_match)
                    }
                } else {
                    // Compile as value tuple when no value present
                    self.compile_create_tuple(tuple.name, tuple.fields)
                }
            }
            ast::Term::Block(block) => {
                // Blocks take their input as a parameter
                let block_parameter = value_type.clone().unwrap_or_else(Type::nil);
                if value_type.is_none() {
                    // Blocks without a value need NIL on stack
                    self.codegen
                        .add_instruction(Instruction::Tuple(TypeId::NIL));
                }
                self.compile_block(block, block_parameter, None)
            }
            ast::Term::FunctionDefinition(func_def) => {
                if value_type.is_some() {
                    return Err(Error::FeatureUnsupported(
                        "Function definition cannot be applied to value".to_string(),
                    ));
                }
                self.compile_function(func_def)
            }
            ast::Term::FunctionCall(target) => self.compile_call(target, value_type),
            ast::Term::MemberAccess(member_access) => {
                match &member_access.identifier {
                    None => {
                        // Field/positional access (.x, .0) requires a value
                        if let Some(val_type) = value_type {
                            // Access on the piped value
                            self.compile_accessor(val_type, member_access.accessors, "value")
                        } else {
                            return Err(Error::FeatureUnsupported(
                                "Field/positional access requires a value".to_string(),
                            ));
                        }
                    }
                    Some(name) => {
                        if value_type.is_some() {
                            return Err(Error::FeatureUnsupported(
                                "Member access cannot be applied to value".to_string(),
                            ));
                        }
                        self.compile_member_access(name, member_access.accessors)
                    }
                }
            }
            ast::Term::Import(path) => {
                if value_type.is_some() {
                    return Err(Error::FeatureUnsupported(
                        "Import cannot be applied to value".to_string(),
                    ));
                }
                self.compile_import(&path)
            }
            ast::Term::Builtin(name) => {
                if value_type.is_some() {
                    return Err(Error::FeatureUnsupported(
                        "Builtin cannot be applied to value".to_string(),
                    ));
                }
                self.compile_builtin(&name)
            }
            ast::Term::TailCall(tail_call) => {
                if value_type.is_none() {
                    return Err(Error::FeatureUnsupported(
                        "Tail call requires a value".to_string(),
                    ));
                }
                self.compile_tail_call(tail_call.identifier.as_deref(), &tail_call.accessors)
            }
            ast::Term::Equality => {
                let val_type = value_type.ok_or_else(|| {
                    Error::FeatureUnsupported("Equality operator requires a value".to_string())
                })?;
                self.compile_equality(val_type)
            }
            ast::Term::Not => {
                let val_type = value_type.ok_or_else(|| {
                    Error::FeatureUnsupported("Not operator requires a value".to_string())
                })?;
                self.compile_not(val_type)
            }
            ast::Term::Partial(_) | ast::Term::Star | ast::Term::Placeholder => {
                // These are always patterns
                let val_type = value_type.ok_or_else(|| {
                    Error::FeatureUnsupported("Pattern matching requires a value".to_string())
                })?;
                self.compile_destructure(term, val_type, on_no_match)
            }
            ast::Term::Spawn(term) => {
                // Spawn should not receive a value - it spawns the term
                if value_type.is_some() {
                    return Err(Error::FeatureUnsupported(
                        "Spawn cannot be applied to a value".to_string(),
                    ));
                }
                // Compile the term to get the function value
                let term_type = self.compile_term(*term, None, None)?;

                // Extract the receive and return types from the function
                let (receive_type, return_type) = if let Type::Callable(callable) = &term_type {
                    (callable.receive.clone(), callable.result.clone())
                } else {
                    return Err(Error::FeatureUnsupported(
                        "Can only spawn functions".to_string(),
                    ));
                };

                // Emit spawn instruction (pops function, pushes Process)
                self.codegen.add_instruction(Instruction::Spawn);

                Ok(Type::Process(Box::new(ProcessType {
                    receive: Some(Box::new(receive_type)),
                    returns: Some(Box::new(return_type)),
                })))
            }
            ast::Term::SendCall(send_call) => {
                // Send expects a value on the stack (the message)
                let val_type = value_type.ok_or_else(|| {
                    Error::FeatureUnsupported("Send requires a value (message)".to_string())
                })?;

                // Load the pid from the identifier with accessors and get its type
                let pid_type = if send_call.accessors.is_empty() {
                    // Simple identifier lookup
                    let (pid_type, index) = self
                        .lookup_variable(&self.scopes, &send_call.name, &[])
                        .ok_or_else(|| Error::VariableUndefined(send_call.name.clone()))?;
                    self.codegen.add_instruction(Instruction::Load(index));
                    pid_type
                } else {
                    // Use member access compilation for accessors
                    self.compile_member_access(&send_call.name, send_call.accessors.clone())?
                };

                // Type check: ensure the pid is a Process and message type matches
                if let Type::Process(process_type) = &pid_type {
                    if let Some(expected_msg_type) = &process_type.receive {
                        // Check if it's the empty union (never receives)
                        if matches!(expected_msg_type.as_ref(), Type::Union(v) if v.is_empty()) {
                            return Err(Error::TypeMismatch {
                                expected: "process with receive type".to_string(),
                                found: "process without receive type (cannot send messages)"
                                    .to_string(),
                            });
                        }
                        if !val_type.is_compatible(expected_msg_type, self.program) {
                            return Err(Error::TypeMismatch {
                                expected: crate::format::format_type(
                                    self.program,
                                    expected_msg_type,
                                ),
                                found: crate::format::format_type(self.program, &val_type),
                            });
                        }
                    } else {
                        // None means unknown receive type
                        return Err(Error::TypeMismatch {
                            expected: "process with known receive type".to_string(),
                            found: "process with unknown receive type".to_string(),
                        });
                    }
                } else {
                    return Err(Error::TypeMismatch {
                        expected: "Process".to_string(),
                        found: format!("{:?}", pid_type),
                    });
                }

                // Emit send instruction (expects [message, pid] on stack)
                self.codegen.add_instruction(Instruction::Send);
                // Send returns Ok (like assignment)
                Ok(Type::ok())
            }
            ast::Term::SelfRef => {
                if value_type.is_some() {
                    return Err(Error::FeatureUnsupported(
                        "Self reference cannot be applied to value".to_string(),
                    ));
                }
                self.codegen.add_instruction(Instruction::Self_);
                // Return a process type with the current function's receive type
                // Return type is None since a process can't know its own return type
                let process_type = ProcessType {
                    receive: Some(Box::new(self.current_receive_type.clone())),
                    returns: None,
                };
                Ok(Type::Process(Box::new(process_type)))
            }
            ast::Term::Receive(receive) => {
                let message_type = self
                    .type_context
                    .resolve_ast_type(receive.type_def.clone(), self.program)?;

                if let Some(block) = receive.block.clone() {
                    // Receive with block - pattern matching logic
                    let loop_start = self.codegen.instructions.len();
                    self.codegen.add_instruction(Instruction::Receive);

                    // We'll create the retry cleanup code after the success path
                    // Use a placeholder jump to skip over it
                    let skip_retry = self.codegen.emit_jump_placeholder();

                    // Retry cleanup code: pop nil, clear parameter local, jump back
                    let retry_addr = self.codegen.instructions.len();
                    self.codegen.add_instruction(Instruction::Pop);
                    self.codegen.add_instruction(Instruction::Clear(1)); // Clear parameter local
                    let offset =
                        (loop_start as isize) - (self.codegen.instructions.len() as isize) - 1;
                    self.codegen.add_instruction(Instruction::Jump(offset));

                    // Patch the skip jump to land here (after retry code)
                    self.codegen.patch_jump_to_here(skip_retry);

                    // Compile block with on_no_match pointing to retry_addr
                    let result_type = self.compile_block(block, message_type, Some(retry_addr))?;

                    // If we get here, a pattern matched - acknowledge the message
                    self.codegen.add_instruction(Instruction::Acknowledge);

                    Ok(result_type)
                } else {
                    // Receive without block - just receive and acknowledge
                    self.codegen.add_instruction(Instruction::Receive);
                    self.codegen.add_instruction(Instruction::Acknowledge);
                    Ok(message_type)
                }
            }
        }
    }

    fn compile_call(
        &mut self,
        target: ast::FunctionCall,
        value_type: Option<Type>,
    ) -> Result<Type, Error> {
        // Use nil as value if value_type is None
        let val_type = value_type.unwrap_or_else(|| {
            self.codegen
                .add_instruction(Instruction::Tuple(TypeId::NIL));
            Type::nil()
        });

        // Get the function type based on the target
        let function_type = match target {
            ast::FunctionCall::Builtin(name) => self.compile_builtin(&name)?,
            ast::FunctionCall::Identifier { name, accessors } => {
                self.compile_member_access(&name, accessors)?
            }
        };

        // Extract callable type and verify it's a function or process
        match function_type {
            Type::Callable(func_type) => {
                // Check parameter compatibility
                if !val_type.is_compatible(&func_type.parameter, self.program) {
                    return Err(Error::TypeMismatch {
                        expected: format!(
                            "function parameter compatible with {}",
                            crate::format::format_type(self.program, &func_type.parameter)
                        ),
                        found: crate::format::format_type(self.program, &val_type),
                    });
                }

                // Check receive type compatibility
                let called_receive_type = &func_type.receive;

                // Check if called function has receives (not Type::never())
                if !matches!(called_receive_type, Type::Union(v) if v.is_empty()) {
                    // Called function has receives - verify current context matches
                    // Check if current context is never (empty union = can't receive)
                    if let Type::Union(variants) = &self.current_receive_type {
                        if variants.is_empty() {
                            // Current context can't receive - can't call a receiving function
                            return Err(Error::TypeMismatch {
                                expected: "function without receive type".to_string(),
                                found: format!(
                                    "function with receive type {}",
                                    crate::format::format_type(self.program, called_receive_type)
                                ),
                            });
                        }
                    }

                    // Check compatibility
                    if !called_receive_type.is_compatible(&self.current_receive_type, self.program)
                    {
                        return Err(Error::TypeMismatch {
                            expected: format!(
                                "function with receive type compatible with {}",
                                crate::format::format_type(
                                    self.program,
                                    &self.current_receive_type
                                )
                            ),
                            found: format!(
                                "function with receive type {}",
                                crate::format::format_type(self.program, called_receive_type)
                            ),
                        });
                    }
                }

                // Execute the call
                self.codegen.add_instruction(Instruction::Call);
                Ok(func_type.result)
            }
            Type::Process(process_type) => {
                // Await process - check it has a return type
                if let Some(return_type) = &process_type.returns {
                    self.codegen.add_instruction(Instruction::Call);
                    Ok((**return_type).clone())
                } else {
                    Err(Error::TypeMismatch {
                        expected: "process with return type (awaitable)".to_string(),
                        found: "process without return type (cannot await)".to_string(),
                    })
                }
            }
            _ => {
                return Err(Error::TypeMismatch {
                    expected: "function or process".to_string(),
                    found: crate::format::format_type(self.program, &function_type),
                });
            }
        }
    }

    fn compile_tail_call(
        &mut self,
        identifier: Option<&str>,
        accessors: &[ast::AccessPath],
    ) -> Result<Type, Error> {
        if identifier.is_none() && accessors.is_empty() {
            // Tail call to parameter (never returns)
            self.codegen.add_instruction(Instruction::TailCall(true));
            Ok(Type::never())
        } else {
            // Tail call to identifier with accessors
            let name = identifier.ok_or_else(|| {
                Error::FeatureUnsupported(
                    "Member access in tail call requires an identifier".to_string(),
                )
            })?;

            let func_type = if accessors.is_empty() {
                // Simple identifier lookup
                match self.lookup_variable(&self.scopes, name, &[]) {
                    Some((func_type, index)) => {
                        self.codegen.add_instruction(Instruction::Load(index));
                        func_type
                    }
                    None => return Err(Error::VariableUndefined(name.to_string())),
                }
            } else {
                // Use member access compilation for accessors
                self.compile_member_access(name, accessors.to_vec())?
            };

            // Verify it's a function
            match func_type {
                Type::Callable(func_type) => {
                    self.codegen.add_instruction(Instruction::TailCall(false));
                    Ok(func_type.result)
                }
                other_type => Err(Error::TypeMismatch {
                    expected: "function".to_string(),
                    found: crate::format::format_type(self.program, &other_type),
                }),
            }
        }
    }

    fn compile_builtin(&mut self, name: &str) -> Result<Type, Error> {
        let (param_type, result_type) = BUILTIN_REGISTRY
            .resolve_signature(name, self.program)
            .ok_or_else(|| Error::BuiltinUndefined(name.to_string()))?;

        let builtin_index = self.program.register_builtin(name.to_string());

        self.codegen
            .add_instruction(Instruction::Builtin(builtin_index));

        Ok(Type::Callable(Box::new(crate::types::CallableType {
            parameter: param_type,
            result: result_type,
            receive: Type::never(),
        })))
    }

    fn compile_equality(&mut self, value_type: Type) -> Result<Type, Error> {
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
                .program
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

    fn compile_not(&mut self, _value_type: Type) -> Result<Type, Error> {
        // The ! operator works with any value on the stack
        // It converts [] to Ok and everything else to []
        self.codegen.add_instruction(Instruction::Not);

        // The Not instruction returns either Ok or NIL
        Ok(Type::from_types(vec![Type::ok(), Type::nil()]))
    }

    fn compile_accessor(
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

    fn compile_member_access(
        &mut self,
        target: &str,
        accessors: Vec<ast::AccessPath>,
    ) -> Result<Type, Error> {
        // Check if we have a capture for the full path (base + accessors)
        if !accessors.is_empty() {
            if let Some((capture_type, capture_index)) =
                self.lookup_variable(&self.scopes, target, &accessors)
            {
                // We have a pre-evaluated capture for this exact path
                self.codegen
                    .add_instruction(Instruction::Load(capture_index));
                return Ok(capture_type);
            }
        }

        // No pre-evaluated capture, use standard member access
        let (last_type, index) = self
            .lookup_variable(&self.scopes, target, &[])
            .ok_or(Error::VariableUndefined(target.to_string()))?;
        self.codegen.add_instruction(Instruction::Load(index));

        self.compile_accessor(last_type, accessors, target)
    }

    fn make_capture_name(&self, base: &str, accessors: &[ast::AccessPath]) -> String {
        if accessors.is_empty() {
            base.to_string()
        } else {
            let accessor_suffix = accessors
                .iter()
                .map(|acc| match acc {
                    ast::AccessPath::Field(name) => name.clone(),
                    ast::AccessPath::Index(idx) => idx.to_string(),
                })
                .collect::<Vec<_>>()
                .join(".");
            format!("{}.{}", base, accessor_suffix)
        }
    }

    fn define_variable(
        &mut self,
        name: &str,
        accessors: &[ast::AccessPath],
        var_type: Type,
    ) -> usize {
        let full_name = self.make_capture_name(name, accessors);
        let index = self.local_count;
        self.local_count += 1;
        if let Some(scope) = self.scopes.last_mut() {
            scope.variables.insert(full_name, (var_type, index));
        }
        index
    }

    fn function_to_type(&self, function_index: usize) -> Result<Type, Error> {
        if let Some(func_def) = self.program.get_function(function_index) {
            if let Some(func_type) = &func_def.function_type {
                Ok(Type::Callable(Box::new(func_type.clone())))
            } else {
                Err(Error::FunctionUndefined(function_index))
            }
        } else {
            Err(Error::FunctionUndefined(function_index))
        }
    }

    fn lookup_variable(
        &self,
        scopes: &[Scope],
        name: &str,
        accessors: &[ast::AccessPath],
    ) -> Option<(Type, usize)> {
        let full_name = self.make_capture_name(name, accessors);
        for scope in scopes.iter().rev() {
            if let Some(&(ref variable_type, index)) = scope.variables.get(&full_name) {
                return Some((variable_type.clone(), index));
            }
        }
        None
    }

    fn get_parameter(&self) -> Result<(Type, usize), Error> {
        self.scopes
            .last()
            .and_then(|scope| scope.parameter.clone())
            .ok_or_else(|| Error::InternalError {
                message: "No parameter in current scope".to_string(),
            })
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
                .program
                .lookup_type(type_id)
                .ok_or_else(|| Error::TypeNotInRegistry { type_id: *type_id })?;

            if let Some((_, field_type)) = fields.get(position) {
                field_types.push(field_type.clone());
            } else {
                return Err(Error::PositionalIndexOutOfBounds { index: position });
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
            let (index, field_type) = self.type_context.get_tuple_field_type_by_name(
                type_id,
                field_name,
                self.program,
            )?;

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
