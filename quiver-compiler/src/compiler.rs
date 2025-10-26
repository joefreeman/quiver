use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

mod codegen;
mod modules;
mod pattern;
mod spread;
mod typing;
mod variables;

pub use codegen::InstructionBuilder;
pub use modules::{ModuleCache, compile_type_import};
pub use pattern::MatchCertainty;
pub use typing::{TupleAccessor, TypeAliasDef, resolve_type_alias_for_display, union_types};

use crate::{
    ast,
    modules::{ModuleError, ModuleLoader},
};

use quiver_core::{
    builtins::BUILTIN_REGISTRY,
    bytecode::{Constant, Function, Instruction, TypeId},
    executor::Executor,
    program::Program,
    types::{CallableType, ProcessType, Type, TypeLookup},
    value::Value,
};

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
        error: quiver_core::error::Error,
    },
    ModuleTypeMissing {
        type_name: String,
        module_path: String,
    },

    // Language feature errors
    FeatureUnsupported(String),

    // Compilation flow errors
    ChainValueUnused,
    ValueIgnored(String),

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

/// Result of compilation containing all outputs
pub struct CompilationResult {
    pub instructions: Vec<Instruction>,
    pub result_type: Type,
    pub variables: HashMap<String, (Type, usize)>,
    pub program: Program,
    pub type_aliases: HashMap<String, typing::TypeAliasDef>,
    pub module_cache: ModuleCache,
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

/// Context for ripple operator (~) usage
/// Tracks the value being rippled and where it is on the stack
struct RippleContext {
    /// Type of the value being rippled
    value_type: Type,
    /// Stack offset where the ripple value is located
    stack_offset: usize,
    /// Whether this context owns the value and must clean it up
    owns_value: bool,
}

pub struct Compiler<'a> {
    // Core components
    codegen: InstructionBuilder,
    type_aliases: HashMap<String, typing::TypeAliasDef>,
    module_cache: ModuleCache,

    // State management
    scopes: Vec<Scope>,
    local_count: usize,
    program: Program,
    module_loader: &'a dyn ModuleLoader,
    module_path: Option<PathBuf>,

    // Track the receive type of the function currently being compiled
    current_receive_type: Type,
}

impl<'a> Compiler<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn compile(
        program: ast::Program,
        type_aliases: HashMap<String, typing::TypeAliasDef>,
        module_cache: ModuleCache,
        module_loader: &'a dyn ModuleLoader,
        base_program: &Program,
        module_path: Option<PathBuf>,
        existing_variables: Option<&HashMap<String, (Type, usize)>>,
        parameter_type: Type,
    ) -> Result<CompilationResult, Error> {
        let mut compiler = Self {
            codegen: InstructionBuilder::new(),
            type_aliases,
            module_cache,
            scopes: vec![],
            local_count: 0,
            program: base_program.clone(),
            module_loader,
            module_path,
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

        // Only allocate parameter slot if we have expressions
        // (Type definitions and imports don't need parameters)
        let has_expressions = program
            .statements
            .iter()
            .any(|s| matches!(s, ast::Statement::Expression(_)));

        let scope_parameter = if has_expressions {
            let param_local = compiler.local_count;
            compiler.local_count += 1;

            compiler.codegen.add_instruction(Instruction::Allocate(1));
            compiler
                .codegen
                .add_instruction(Instruction::Store(param_local));

            Some((parameter_type, param_local))
        } else {
            None
        };

        // Initialize scope with variables and optional parameter
        compiler.scopes = vec![Scope::new(scope_variables, scope_parameter)];

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

        Ok(CompilationResult {
            instructions: compiler.codegen.instructions,
            result_type,
            variables,
            program: compiler.program,
            type_aliases: compiler.type_aliases,
            module_cache: compiler.module_cache,
        })
    }

    fn compile_statement(&mut self, statement: ast::Statement) -> Result<Type, Error> {
        match statement {
            ast::Statement::TypeAlias {
                name,
                type_parameters,
                type_definition,
            } => {
                self.compile_type_alias(&name, type_parameters, type_definition)?;
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

    fn compile_type_alias(
        &mut self,
        name: &str,
        type_parameters: Vec<String>,
        type_definition: ast::Type,
    ) -> Result<(), Error> {
        // Prevent shadowing primitive types
        if matches!(name, "int" | "bin") {
            return Err(Error::TypeUnresolved(format!(
                "Cannot redefine primitive type '{}'",
                name
            )));
        }

        // Validate the AST before storing (even though we won't resolve it yet)
        Self::validate_type_ast(&type_definition)?;

        // Store all type aliases (with 0+ parameters)
        self.type_aliases
            .insert(name.to_string(), (type_parameters, type_definition));
        Ok(())
    }

    fn validate_type_ast(ast_type: &ast::Type) -> Result<(), Error> {
        match ast_type {
            ast::Type::Tuple(tuple) => {
                // Validate partial types have all named fields
                if tuple.is_partial {
                    let has_unnamed = tuple
                        .fields
                        .iter()
                        .any(|f| matches!(f, ast::FieldType::Field { name: None, .. }));
                    let has_named = tuple
                        .fields
                        .iter()
                        .any(|f| matches!(f, ast::FieldType::Field { name: Some(_), .. }));

                    if has_unnamed && has_named {
                        return Err(Error::TypeUnresolved(
                            "All fields in a partial type must be named".to_string(),
                        ));
                    }
                }
                // Recursively validate field types
                for field in &tuple.fields {
                    if let ast::FieldType::Field { type_def, .. } = field {
                        Self::validate_type_ast(type_def)?;
                    }
                }
                Ok(())
            }
            ast::Type::Function(func) => {
                Self::validate_type_ast(&func.input)?;
                Self::validate_type_ast(&func.output)
            }
            ast::Type::Process(proc) => {
                if let Some(receive) = &proc.receive_type {
                    Self::validate_type_ast(receive)?;
                }
                if let Some(returns) = &proc.return_type {
                    Self::validate_type_ast(returns)?;
                }
                Ok(())
            }
            ast::Type::Union(union) => {
                for variant in &union.types {
                    Self::validate_type_ast(variant)?;
                }
                Ok(())
            }
            ast::Type::Identifier { arguments, .. } => {
                for arg in arguments {
                    Self::validate_type_ast(arg)?;
                }
                Ok(())
            }
            ast::Type::Primitive(_) | ast::Type::Cycle(_) => Ok(()),
        }
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
            &mut self.type_aliases,
            &mut self.program,
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

    fn compile_tuple(
        &mut self,
        tuple_name: ast::TupleName,
        fields: Vec<ast::TupleField>,
        ripple_context: Option<&RippleContext>,
    ) -> Result<Type, Error> {
        Self::check_field_name_duplicates(&fields, |f| f.name.as_ref())?;

        // Check if this tuple contains spreads
        let contains_spread = Self::tuple_contains_spread(&fields);

        if contains_spread {
            // Resolve tuple name based on the TupleName variant
            let resolved_tuple_name = match tuple_name {
                ast::TupleName::Literal(name) => Some(name),
                ast::TupleName::None => None,
                ast::TupleName::Ripple => {
                    // Ripple spread: ~[..., fields]
                    // Get the ripple type from ripple_context
                    ripple_context.and_then(|ctx| {
                        let ripple_type = &ctx.value_type;
                        if let Type::Tuple(type_id) = ripple_type {
                            self.program
                                .lookup_type(type_id)
                                .and_then(|type_info| type_info.name.clone())
                        } else {
                            None
                        }
                    })
                }
                ast::TupleName::Identifier(id) => {
                    // Identifier spread: identifier[..., fields]
                    // Look up the variable's type
                    self.lookup_variable(&self.scopes, &id, &[])
                        .and_then(|(var_type, _)| {
                            if let Type::Tuple(type_id) = var_type {
                                self.program
                                    .lookup_type(&type_id)
                                    .and_then(|type_info| type_info.name.clone())
                            } else {
                                None
                            }
                        })
                }
            };

            // Use specialized compilation for tuples with spreads
            return spread::compile_tuple_with_spread(
                self,
                resolved_tuple_name,
                fields,
                ripple_context,
            );
        }

        // Compile field values and collect their types
        let mut field_types = Vec::new();
        for (fields_compiled, field) in fields.iter().enumerate() {
            let field_type = match &field.value {
                ast::FieldValue::Chain(chain) => {
                    // Pass ripple_context to chains, incrementing offset for each field compiled
                    // Set owns_value=false since we're passing it through, not owning it
                    let ripple_context_value;
                    let ripple_context_param = if let Some(ctx) = ripple_context {
                        ripple_context_value = RippleContext {
                            value_type: ctx.value_type.clone(),
                            stack_offset: ctx.stack_offset + fields_compiled,
                            owns_value: false,
                        };
                        Some(&ripple_context_value)
                    } else {
                        None
                    };
                    self.compile_chain(chain.clone(), None, ripple_context_param)?
                }
                ast::FieldValue::Spread(_) => {
                    unreachable!("Spread should be handled by compile_tuple_with_spread")
                }
            };
            field_types.push((field.name.clone(), field_type));
        }

        // Register the tuple type and emit instruction
        let resolved_name = match tuple_name {
            ast::TupleName::Literal(name) => Some(name),
            ast::TupleName::None => None,
            ast::TupleName::Identifier(_) | ast::TupleName::Ripple => {
                // Parser enforces that identifier/ripple syntax requires spreads,
                // so this path only executes when there are spreads (handled above)
                unreachable!(
                    "TupleName::Identifier/Ripple without spreads should be rejected by parser"
                )
            }
        };
        let type_id = self.program.register_type(resolved_name, field_types);
        self.codegen.add_instruction(Instruction::Tuple(type_id));

        // Clean up ripple value if we own it
        if let Some(ctx) = ripple_context
            && ctx.owns_value
        {
            self.codegen.add_instruction(Instruction::Rotate(2));
            self.codegen.add_instruction(Instruction::Pop);
        }

        Ok(Type::Tuple(type_id))
    }

    fn tuple_contains_ripple(fields: &[ast::TupleField]) -> bool {
        fields.iter().any(|f| match &f.value {
            ast::FieldValue::Chain(chain) => Self::chain_contains_ripple(chain),
            ast::FieldValue::Spread(_) => false,
        })
    }

    fn select_contains_ripple(sources: &[ast::Chain]) -> bool {
        sources.iter().any(Self::chain_contains_ripple)
    }

    fn chain_contains_ripple(chain: &ast::Chain) -> bool {
        for term in &chain.terms {
            match term {
                ast::Term::Ripple => return true,
                ast::Term::BindMatch(_) | ast::Term::PinMatch(_) => continue,
                ast::Term::Tuple(tuple) => return Self::tuple_contains_ripple(&tuple.fields),
                ast::Term::Block(block) => return Self::block_contains_ripple(block),
                ast::Term::Function(func) => {
                    return func.body.as_ref().is_some_and(Self::block_contains_ripple);
                }
                ast::Term::Select(select) => {
                    return Self::select_contains_ripple(&select.sources);
                }
                _ => return false,
            }
        }
        false
    }

    fn block_contains_ripple(block: &ast::Block) -> bool {
        block.branches.iter().any(|branch| {
            branch
                .condition
                .chains
                .iter()
                .any(Self::chain_contains_ripple)
                || branch
                    .consequence
                    .as_ref()
                    .is_some_and(|expr| expr.chains.iter().any(Self::chain_contains_ripple))
        })
    }

    // Helper function to check if tuple fields contain spread operations
    fn tuple_contains_spread(fields: &[ast::TupleField]) -> bool {
        fields
            .iter()
            .any(|field| matches!(field.value, ast::FieldValue::Spread(_)))
    }

    fn extract_receive_type(&mut self, block: Option<&ast::Block>) -> Result<Type, Error> {
        let mut receive_types = Vec::new();
        if let Some(block) = block {
            self.collect_receive_types(block, &mut receive_types)?;
        }

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
        // Track the chained type as we traverse the chain
        let mut chained_type: Option<Type> = None;

        for term in &chain.terms {
            chained_type =
                self.collect_receive_types_from_term(term, chained_type, receive_types)?;
        }
        Ok(())
    }

    fn collect_receive_types_from_term(
        &mut self,
        term: &ast::Term,
        chained_type: Option<Type>,
        receive_types: &mut Vec<Type>,
    ) -> Result<Option<Type>, Error> {
        match term {
            ast::Term::Select(select) => {
                // For a single select with multiple sources, collect all types
                // and create a union if they differ
                let mut select_sources = Vec::new();
                for source in &select.sources {
                    self.extract_receive_type_from_source(
                        source,
                        chained_type.as_ref(),
                        &mut select_sources,
                    )?;
                }

                // If this select has receive sources, add the unified type
                if !select_sources.is_empty() {
                    if select_sources.len() == 1 {
                        receive_types.push(select_sources[0].clone());
                    } else {
                        // Multiple sources in same select - create union
                        receive_types.push(Type::Union(select_sources));
                    }
                }
                // Select doesn't produce a chainable type for receive extraction
                Ok(None)
            }
            ast::Term::Access(access) => {
                // Check argument tuple for receive types (e.g., math.div[~, !#int])
                if let Some(arg_tuple) = &access.argument {
                    for field in &arg_tuple.fields {
                        if let ast::FieldValue::Chain(chain) = &field.value {
                            self.collect_receive_types_from_chain(chain, receive_types)?;
                        }
                    }
                }

                // Try to resolve the access to get its type
                if let Some(identifier) = &access.identifier {
                    let var_type = self
                        .lookup_variable(&self.scopes, identifier, &access.accessors)
                        .map(|(t, _)| t)
                        .or_else(|| {
                            let (base_type, _) =
                                self.lookup_variable(&self.scopes, identifier, &[])?;
                            self.resolve_accessor_type(base_type, &access.accessors, identifier)
                                .ok()
                        });
                    Ok(var_type)
                } else {
                    Ok(None)
                }
            }
            ast::Term::Block(block) => {
                self.collect_receive_types(block, receive_types)?;
                Ok(None)
            }
            ast::Term::Function(_) => {
                // Don't recurse into nested function definitions - they have their own receive types
                Ok(None)
            }
            ast::Term::Tuple(tuple) => {
                // Check tuple fields
                for field in &tuple.fields {
                    if let ast::FieldValue::Chain(chain) = &field.value {
                        self.collect_receive_types_from_chain(chain, receive_types)?;
                    }
                }
                Ok(None)
            }
            ast::Term::Builtin(builtin) => {
                // Check argument tuple for receive types (e.g., __add__[!#int, 5])
                if let Some(arg_tuple) = &builtin.argument {
                    for field in &arg_tuple.fields {
                        if let ast::FieldValue::Chain(chain) = &field.value {
                            self.collect_receive_types_from_chain(chain, receive_types)?;
                        }
                    }
                }
                Ok(None)
            }
            ast::Term::TailCall(tail_call) => {
                // Check argument tuple for receive types (e.g., &f[!#int, 5])
                if let Some(arg_tuple) = &tail_call.argument {
                    for field in &arg_tuple.fields {
                        if let ast::FieldValue::Chain(chain) = &field.value {
                            self.collect_receive_types_from_chain(chain, receive_types)?;
                        }
                    }
                }
                Ok(None)
            }
            _ => Ok(None),
        }
    }

    fn extract_receive_type_from_source(
        &mut self,
        source: &ast::Chain,
        chained_type: Option<&Type>,
        receive_types: &mut Vec<Type>,
    ) -> Result<(), Error> {
        // A source can be:
        // 1. A function literal: #int { ... } -> extract int
        // 2. A variable reference: r1 -> look up and extract parameter type
        // 3. A ripple: ~ -> use the chained type
        // 4. Something else (process, timeout) -> ignore

        for term in &source.terms {
            match term {
                ast::Term::Ripple => {
                    // Ripple refers to the chained value - extract its receive type
                    if let Some(Type::Callable(callable)) = chained_type {
                        receive_types.push(callable.parameter.clone());
                    }
                }
                ast::Term::Function(func) => {
                    // Inline function definition - extract parameter type
                    if let Some(param_type) = &func.parameter_type {
                        let resolved_type = typing::resolve_ast_type(
                            &self.type_aliases,
                            param_type.clone(),
                            &mut self.program,
                        )?;
                        receive_types.push(resolved_type);
                    }
                }
                ast::Term::Access(access) => {
                    // Variable reference - look up its type
                    // Skip function calls (only handle bare references)
                    if access.argument.is_some() {
                        continue;
                    }

                    let Some(identifier) = &access.identifier else {
                        continue;
                    };

                    // Resolve variable type: try full path first, then base + accessor resolution
                    let var_type = self
                        .lookup_variable(&self.scopes, identifier, &access.accessors)
                        .map(|(t, _)| t)
                        .or_else(|| {
                            // Full path not found - try resolving accessors through type system
                            let (base_type, _) =
                                self.lookup_variable(&self.scopes, identifier, &[])?;
                            self.resolve_accessor_type(base_type, &access.accessors, identifier)
                                .ok()
                        });

                    if let Some(Type::Callable(callable)) = var_type {
                        receive_types.push(callable.parameter.clone());
                    }
                }
                _ => {
                    // Recursively check nested structures
                    self.collect_receive_types_from_term(term, None, receive_types)?;
                }
            }
        }
        Ok(())
    }

    fn compile_function(&mut self, function: ast::Function) -> Result<Type, Error> {
        let mut function_params: HashSet<String> = HashSet::new();

        if let Some(ast::Type::Tuple(tuple_type)) = &function.parameter_type {
            for field in &tuple_type.fields {
                if let ast::FieldType::Field {
                    name: Some(field_name),
                    ..
                } = field
                {
                    function_params.insert(field_name.clone());
                }
            }
        }

        let captures = variables::collect_free_variables(
            function.body.as_ref(),
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

        // Resolve parameter type with declared type parameters
        let parameter_type = match &function.parameter_type {
            Some(t) => typing::resolve_function_parameter_type(
                &self.type_aliases,
                t.clone(),
                &function.type_parameters,
                &mut self.program,
            )?,
            None => Type::nil(),
        };

        // Extract receive type from function body
        let receive_type = self.extract_receive_type(function.body.as_ref())?;

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
        if let Some(ast::Type::Tuple(tuple_type)) = &function.parameter_type {
            for (field_index, field) in tuple_type.fields.iter().enumerate() {
                if let ast::FieldType::Field {
                    name: Some(field_name),
                    type_def,
                } = field
                {
                    let field_type = typing::resolve_ast_type(
                        &self.type_aliases,
                        type_def.clone(),
                        &mut self.program,
                    )?;
                    parameter_fields.insert(field_name.clone(), (field_index, field_type));
                }
            }
        }
        let body_type = match function.body {
            Some(body) => self.compile_block(body, parameter_type.clone(), None)?,
            None => {
                // Identity function: just return the parameter
                // The calling convention puts the parameter on the stack,
                // so we don't need any instructions - just leave it there
                parameter_type.clone()
            }
        };

        let func_type = CallableType {
            parameter: parameter_type,
            result: body_type,
            receive: receive_type.clone(),
        };

        let function_instructions = std::mem::take(&mut self.codegen.instructions);
        let function_index = self.program.register_function(Function {
            instructions: function_instructions,
            function_type: func_type.clone(),
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

    fn compile_match(
        &mut self,
        pattern: ast::Match,
        value_type: Type,
        on_no_match: Option<usize>,
        mode: pattern::PatternMode,
    ) -> Result<Type, Error> {
        let start_jump_addr = self.codegen.emit_jump_placeholder();
        let fail_jump_addr = self.codegen.emit_jump_placeholder();

        self.codegen.patch_jump_to_here(start_jump_addr);

        // Build available variables set from current scope for pin patterns
        let mut available_variables = HashSet::new();
        if let Some(scope) = self.scopes.last() {
            for (name, (_type, _index)) in &scope.variables {
                available_variables.insert(name.clone());
            }
        }

        // Analyze pattern to get bindings without generating code yet
        let (certainty, bindings, binding_sets) = pattern::analyze_pattern(
            &self.program,
            &pattern,
            &value_type,
            mode,
            &available_variables,
        )?;

        match certainty {
            MatchCertainty::WontMatch => {
                self.codegen.add_instruction(Instruction::Pop);
                self.codegen
                    .add_instruction(Instruction::Tuple(TypeId::NIL));
                Ok(Type::nil())
            }
            MatchCertainty::WillMatch | MatchCertainty::MightMatch => {
                // Pre-allocate locals for all bindings
                let mut bindings_map = HashMap::new();

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

                // Build variables map from current scope for pin patterns
                let mut variables = HashMap::new();
                if let Some(scope) = self.scopes.last() {
                    for (name, (_type, index)) in &scope.variables {
                        variables.insert(name.clone(), *index);
                    }
                }

                // Now generate pattern matching code with pre-allocated locals
                // Use on_no_match if provided (for receive blocks), otherwise use fail_jump_addr
                let fail_target = on_no_match.unwrap_or(fail_jump_addr);
                pattern::generate_pattern_code(
                    &mut self.codegen,
                    &mut self.program,
                    &mut self.local_count,
                    Some(&bindings_map),
                    &variables,
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
                        Type::Union(types) => types.iter().any(Type::is_nil),
                        t => t.is_nil(),
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
            let chain_type = self.compile_chain(chain.clone(), on_no_match, None)?;

            // Check if the previous type contained nil and we're not on the first chain
            let should_propagate_nil = if i > 0 {
                if let Some(ref prev_type) = last_type {
                    match prev_type {
                        Type::Union(types) => types.iter().any(Type::is_nil),
                        t => t.is_nil(),
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
            if let Some(ref last_type) = last_type
                && last_type.as_concrete() == Some(&Type::nil())
            {
                break;
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
        ripple_context: Option<&RippleContext>,
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
            current_type =
                Some(self.compile_term(term, current_type, on_no_match, ripple_context)?);
        }

        let result_type = current_type.ok_or_else(|| Error::InternalError {
            message: "Chain compiled with no terms and no continuation".to_string(),
        })?;

        // If there's a match pattern, apply it
        if let Some(pattern) = chain.match_pattern {
            self.compile_match(
                pattern,
                result_type,
                on_no_match,
                pattern::PatternMode::Bind,
            )
        } else {
            Ok(result_type)
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

        // Check if we already have the compiled module instructions cached
        let (instructions, result_type) = if let Some((cached_instructions, cached_type)) = self
            .module_cache
            .instruction_cache
            .get(module_path)
            .cloned()
        {
            // Return cached reconstruction instructions and type
            (cached_instructions, cached_type)
        } else {
            // Add to import stack to track circular imports
            self.module_cache.import_stack.push(module_path.to_string());
            let (instructions, result_type) = self.import_module(module_path)?;
            self.module_cache.import_stack.pop();

            // Cache the reconstruction instructions and type
            self.module_cache.instruction_cache.insert(
                module_path.to_string(),
                (instructions.clone(), result_type.clone()),
            );

            (instructions, result_type)
        };

        // Emit the cached instructions
        for instruction in instructions {
            self.codegen.add_instruction(instruction);
        }

        Ok(result_type)
    }

    fn import_module(&mut self, module_path: &str) -> Result<(Vec<Instruction>, Type), Error> {
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
        let saved_type_aliases = self.type_aliases.clone();

        // Reset to clean state for module compilation
        self.scopes = vec![Scope::new(HashMap::new(), None)];
        self.local_count = 0;
        self.type_aliases.clear();

        // Track the last statement's type for execute_instructions_sync
        let mut last_type = Type::nil();
        for statement in parsed.statements {
            last_type = self.compile_statement(statement)?;
        }

        // Get the compiled module instructions
        let module_instructions = std::mem::take(&mut self.codegen.instructions);

        // Restore original compiler state
        self.codegen.instructions = saved_instructions;
        self.scopes = saved_scopes;
        self.local_count = saved_local_count;
        self.type_aliases = saved_type_aliases;

        // Execute the module instructions to get the result value
        let (module_value, executor) =
            quiver_core::execute_instructions_sync(&self.program, module_instructions, last_type)
                .map_err(|e| Error::ModuleExecution {
                module_path: module_path.to_string(),
                error: e,
            })?;

        // Convert the runtime value back to instructions
        let (instructions, module_type) = self.value_to_instructions(&module_value, &executor)?;

        Ok((instructions, module_type))
    }

    /// Convert a runtime value back to instructions that reconstruct it
    fn value_to_instructions(
        &mut self,
        value: &Value,
        executor: &Executor,
    ) -> Result<(Vec<Instruction>, Type), Error> {
        match value {
            Value::Integer(int_value) => {
                let index = self
                    .program
                    .register_constant(Constant::Integer(*int_value));
                Ok((vec![Instruction::Constant(index)], Type::Integer))
            }
            Value::Binary(binary_ref) => {
                // Get the binary bytes (handles both Constant and Heap cases)
                let binary_data = executor.get_binary_bytes(binary_ref).map_err(|e| {
                    Error::FeatureUnsupported(format!("Failed to get binary bytes: {:?}", e))
                })?;

                // Create a constant from the binary data
                let constant = Constant::Binary(binary_data);
                let index = self.program.register_constant(constant);
                Ok((vec![Instruction::Constant(index)], Type::Binary))
            }
            Value::Tuple(type_id, fields) => {
                let mut instructions = Vec::new();
                for field in fields {
                    let (field_instructions, _) = self.value_to_instructions(field, executor)?;
                    instructions.extend(field_instructions);
                }
                instructions.push(Instruction::Tuple(*type_id));
                Ok((instructions, Type::Tuple(*type_id)))
            }
            Value::Function(function, captures) => {
                // Get the function definition
                let func_def = self.program.get_function(*function).cloned().ok_or(
                    Error::FeatureUnsupported("Invalid function reference".to_string()),
                )?;

                let mut instructions = Vec::new();
                let mut capture_locals = Vec::new();

                // Store each capture value in locals
                for value in captures {
                    // Recursively convert the captured value to instructions
                    let (capture_instructions, _) = self.value_to_instructions(value, executor)?;
                    instructions.extend(capture_instructions);

                    // Allocate a local and store it
                    let local_index = self.local_count;
                    self.local_count += 1;
                    instructions.push(Instruction::Allocate(1));
                    instructions.push(Instruction::Store(local_index));
                    capture_locals.push(local_index);
                }

                // Register function with new capture locals for this context
                let func_index = self.program.register_function(Function {
                    instructions: func_def.instructions,
                    function_type: func_def.function_type,
                    captures: capture_locals,
                });

                // Emit the function instruction
                instructions.push(Instruction::Function(func_index));

                // Return the function type
                let func = self
                    .program
                    .get_function(func_index)
                    .ok_or(Error::FunctionUndefined(func_index))?;
                let function_type = func.function_type.clone();
                Ok((instructions, Type::Callable(Box::new(function_type))))
            }
            Value::Builtin(name) => {
                let builtin_index = self.program.register_builtin(name.to_string());

                if let Some((param_type, result_type)) =
                    BUILTIN_REGISTRY.resolve_signature(name, &mut self.program)
                {
                    Ok((
                        vec![Instruction::Builtin(builtin_index)],
                        Type::Callable(Box::new(CallableType {
                            parameter: param_type,
                            result: result_type,
                            receive: Type::never(),
                        })),
                    ))
                } else {
                    Err(Error::BuiltinUndefined(name.to_string()))
                }
            }
            Value::Process(_, _) => Err(Error::FeatureUnsupported(
                "Cannot use process in constant context".to_string(),
            )),
        }
    }

    /// Validate that a receive function in a select has the correct return type
    fn validate_receive_function(
        &self,
        source: &ast::Chain,
        source_type: &Type,
    ) -> Result<(), Error> {
        // Only validate if this is a callable (receive function)
        let Type::Callable(callable) = source_type else {
            return Ok(());
        };

        // Identity functions (no body) and variable references are always allowed
        if !source
            .terms
            .iter()
            .any(|term| matches!(term, ast::Term::Function(func) if func.body.is_some()))
        {
            return Ok(());
        }

        // For functions with bodies, the result type must be nil, Ok, or a union of them
        let is_valid = match &callable.result {
            t if t.is_nil() || t.is_ok() => true,
            Type::Union(variants) => variants.iter().all(|v| v.is_nil() || v.is_ok()),
            _ => false,
        };

        if !is_valid {
            return Err(Error::TypeMismatch {
                expected: "receive function with body must return [] or Ok".to_string(),
                found: quiver_core::format::format_type(&self.program, &callable.result),
            });
        }

        Ok(())
    }

    /// Compile select sources and return their types
    fn compile_select_sources(
        &mut self,
        sources: &[ast::Chain],
        value_type: Option<&Type>,
    ) -> Result<Vec<Type>, Error> {
        let mut source_types = Vec::new();

        for (i, source) in sources.iter().enumerate() {
            // Pass ripple_context if there's a chained value
            // Chained value is at offset i (number of sources compiled so far)
            // Set owns_value=false since we'll clean it up manually after all sources
            let ripple_ctx;
            let ripple_param = if let Some(val_type) = value_type {
                ripple_ctx = RippleContext {
                    value_type: val_type.clone(),
                    stack_offset: i,
                    owns_value: false,
                };
                Some(&ripple_ctx)
            } else {
                None
            };

            let source_type = self.compile_chain(source.clone(), None, ripple_param)?;

            // Validate receive functions
            self.validate_receive_function(source, &source_type)?;

            source_types.push(source_type);
        }

        Ok(source_types)
    }

    /// Compile chained spawn: `value ~> @function`
    /// Creates a wrapper function that captures the argument and target, then spawns it
    fn compile_chained_spawn(&mut self, term: ast::Term, arg_type: Type) -> Result<Type, Error> {
        // Compile the target function
        let target_type = self.compile_term(term, None, None, None)?;

        let (target_param, target_receive, target_result) =
            if let Type::Callable(callable) = &target_type {
                (
                    callable.parameter.clone(),
                    callable.receive.clone(),
                    callable.result.clone(),
                )
            } else {
                return Err(Error::FeatureUnsupported(
                    "Can only spawn functions".to_string(),
                ));
            };

        // Type check: argument must be compatible with function parameter
        if !arg_type.is_compatible(&target_param, &self.program) {
            return Err(Error::TypeMismatch {
                expected: quiver_core::format::format_type(&self.program, &target_param),
                found: quiver_core::format::format_type(&self.program, &arg_type),
            });
        }

        // Stack is now: [argument, target_function]
        // We need to create a wrapper function that captures both and calls target with argument

        // Save both to locals (allocate 2 slots, store target then argument)
        let wrapper_locals_base = self.local_count;
        self.codegen.add_instruction(Instruction::Allocate(2));
        self.codegen
            .add_instruction(Instruction::Store(wrapper_locals_base + 1)); // target function
        self.codegen
            .add_instruction(Instruction::Store(wrapper_locals_base)); // argument

        // Create wrapper function that takes nil and calls target with captured argument
        let wrapper_instructions = vec![
            Instruction::Load(0), // Load captured argument (from wrapper's captures[0])
            Instruction::Load(1), // Load captured target function (from wrapper's captures[1])
            Instruction::Call,    // Call target with argument
        ];

        let wrapper_func_index = self.program.register_function(Function {
            instructions: wrapper_instructions,
            function_type: CallableType {
                parameter: Type::nil(),
                result: target_result.clone(),
                receive: target_receive.clone(),
            },
            captures: vec![wrapper_locals_base, wrapper_locals_base + 1],
        });

        // Emit function reference with captures
        self.codegen
            .add_instruction(Instruction::Function(wrapper_func_index));

        // Clean up locals (the function has captured them)
        self.codegen.add_instruction(Instruction::Clear(2));

        // Spawn the wrapper
        self.codegen.add_instruction(Instruction::Spawn);

        Ok(Type::Process(Box::new(ProcessType {
            receive: Some(Box::new(target_receive)),
            returns: Some(Box::new(target_result)),
        })))
    }

    /// Compute the return type of a select operation from source types
    fn compute_select_return_type(&self, source_types: &[Type]) -> Result<Type, Error> {
        let mut result_types = Vec::new();
        let mut has_timeout = false;

        for source_type in source_types {
            match source_type {
                Type::Process(process_type) => {
                    // Awaiting process - get its return type
                    if let Some(return_type) = &process_type.returns {
                        result_types.push((**return_type).clone());
                    } else {
                        return Err(Error::TypeMismatch {
                            expected: "process with return type (awaitable)".to_string(),
                            found: "process without return type (cannot await)".to_string(),
                        });
                    }
                }
                Type::Callable(callable) => {
                    // Receive function - use its result type
                    result_types.push(callable.result.clone());
                }
                Type::Integer => {
                    // Timeout source
                    has_timeout = true;
                }
                _ => {
                    return Err(Error::TypeMismatch {
                        expected: "process, function, or integer (timeout)".to_string(),
                        found: quiver_core::format::format_type(&self.program, source_type),
                    });
                }
            }
        }

        if result_types.is_empty() && !has_timeout {
            return Err(Error::FeatureUnsupported(
                "Select requires at least one source".to_string(),
            ));
        }

        // Determine final return type
        let base_type = if result_types.is_empty() {
            // Only timeout sources - returns nil
            Type::Tuple(TypeId::NIL)
        } else if result_types.len() == 1 {
            result_types[0].clone()
        } else {
            // Multiple different return types - create union
            Type::Union(result_types)
        };

        // If there's a timeout, union with nil
        if has_timeout {
            Ok(Type::Union(vec![base_type, Type::Tuple(TypeId::NIL)]))
        } else {
            Ok(base_type)
        }
    }

    fn compile_term(
        &mut self,
        term: ast::Term,
        value_type: Option<Type>,
        on_no_match: Option<usize>,
        ripple_context: Option<&RippleContext>,
    ) -> Result<Type, Error> {
        match term {
            ast::Term::Literal(literal) => {
                if value_type.is_some() {
                    return Err(Error::FeatureUnsupported(
                        "Literal cannot be used as pattern; use assignment pattern (e.g., =42)"
                            .to_string(),
                    ));
                }
                self.compile_literal(literal)
            }
            ast::Term::Tuple(tuple) => {
                // Tuples without ripples or spreads when a value is present should use assignment patterns
                if value_type.is_some()
                    && !Self::tuple_contains_ripple(&tuple.fields)
                    && !Self::tuple_contains_spread(&tuple.fields)
                {
                    return Err(Error::ValueIgnored(
                        "Tuple construction ignores piped value; use ripple (e.g., [~, 2]) or assignment pattern (e.g., =[x, y])"
                            .to_string(),
                    ));
                }

                // Convert value_type to ripple_context if present, otherwise use existing ripple_context
                // Set owns_value=true when converting from value_type (we own it and must clean up)
                let ripple_context_value;
                let ripple_context_param = if let Some(vt) = value_type.as_ref() {
                    ripple_context_value = RippleContext {
                        value_type: vt.clone(),
                        stack_offset: 0,
                        owns_value: true,
                    };
                    Some(&ripple_context_value)
                } else {
                    ripple_context
                };

                self.compile_tuple(tuple.name, tuple.fields, ripple_context_param)
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
            ast::Term::Function(func) => {
                // Compile the function itself
                let function_type = self.compile_function(func)?;

                // If a value is being piped to it, apply the value
                if let Some(val_type) = value_type {
                    self.apply_value_to_type(function_type, val_type)
                } else {
                    Ok(function_type)
                }
            }
            ast::Term::Access(access) => {
                match &access.identifier {
                    None => {
                        // Field/positional access (.x, .0) requires a value
                        if let Some(val_type) = value_type {
                            // Access on the piped value
                            let accessed_type =
                                self.compile_accessor(val_type, access.accessors, "value")?;

                            if let Some(args) = &access.argument {
                                // Compile argument - no ripples allowed (pass None as ripple_context)
                                let arg_type = self.compile_tuple(
                                    args.name.clone(),
                                    args.fields.clone(),
                                    None,
                                )?;

                                // Now we have: [function, tuple] on stack, but we need [tuple, function]
                                self.codegen.add_instruction(Instruction::Rotate(2));

                                // Apply argument to the accessed function
                                self.apply_value_to_type(accessed_type, arg_type)
                            } else {
                                Ok(accessed_type)
                            }
                        } else {
                            Err(Error::FeatureUnsupported(
                                "Field/positional access requires a value".to_string(),
                            ))
                        }
                    }
                    Some(name) => {
                        // If there's an argument, compile it first (before loading the function)
                        // so that ripples in the argument can access the piped value on the stack
                        let arg_type = if let Some(args) = &access.argument {
                            // Check if argument would silently drop piped value
                            if value_type.is_some()
                                && !Self::tuple_contains_ripple(&args.fields)
                                && !Self::tuple_contains_spread(&args.fields)
                            {
                                return Err(Error::ValueIgnored(
                                    "Function argument ignores piped value; use ripple (e.g., f[~, 2])"
                                        .to_string(),
                                ));
                            }

                            // Convert value_type to ripple_context if present
                            // Set owns_value=true when converting from value_type (we own it and must clean up)
                            let ripple_context_value;
                            let ripple_context_param = if let Some(vt) = value_type.as_ref() {
                                ripple_context_value = RippleContext {
                                    value_type: vt.clone(),
                                    stack_offset: 0,
                                    owns_value: true,
                                };
                                Some(&ripple_context_value)
                            } else {
                                ripple_context
                            };

                            Some(self.compile_tuple(
                                args.name.clone(),
                                args.fields.clone(),
                                ripple_context_param,
                            )?)
                        } else {
                            None
                        };

                        // Load the access
                        let accessed_type = self.compile_member_access(name, access.accessors)?;

                        // Apply argument if present, otherwise apply piped value if present
                        if let Some(arg_type) = arg_type {
                            self.apply_value_to_type(accessed_type, arg_type)
                        } else if let Some(val_type) = value_type {
                            self.apply_value_to_type(accessed_type, val_type)
                        } else {
                            Ok(accessed_type)
                        }
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
            ast::Term::Builtin(builtin) => {
                // If there's an argument, compile it first (before loading the builtin)
                // so that ripples in the argument can access the piped value on the stack
                let arg_type = if let Some(args) = &builtin.argument {
                    // Check if argument would silently drop piped value
                    if value_type.is_some()
                        && !Self::tuple_contains_ripple(&args.fields)
                        && !Self::tuple_contains_spread(&args.fields)
                    {
                        return Err(Error::ValueIgnored(
                            "Builtin argument ignores piped value; use ripple (e.g., __add__[~, 2])"
                                .to_string(),
                        ));
                    }

                    // Convert value_type to ripple_context if present
                    // Set owns_value=true when converting from value_type (we own it and must clean up)
                    let ripple_context_value;
                    let ripple_context_param = if let Some(vt) = value_type.as_ref() {
                        ripple_context_value = RippleContext {
                            value_type: vt.clone(),
                            stack_offset: 0,
                            owns_value: true,
                        };
                        Some(&ripple_context_value)
                    } else {
                        ripple_context
                    };

                    Some(self.compile_tuple(
                        args.name.clone(),
                        args.fields.clone(),
                        ripple_context_param,
                    )?)
                } else {
                    None
                };

                // Load the builtin
                let builtin_type = self.compile_builtin(&builtin.name)?;

                // Apply argument if present, otherwise apply piped value if present
                if let Some(arg_type) = arg_type {
                    self.apply_value_to_type(builtin_type, arg_type)
                } else if let Some(val_type) = value_type {
                    self.apply_value_to_type(builtin_type, val_type)
                } else {
                    Ok(builtin_type)
                }
            }
            ast::Term::TailCall(tail_call) => {
                // If there's an argument, compile it first (may contain ripples using value_type)
                let arg_type = if let Some(args) = &tail_call.argument {
                    // Convert value_type to ripple_context if present
                    // Set owns_value=true when converting from value_type (we own it and must clean up)
                    let ripple_context_value;
                    let ripple_context_param = if let Some(vt) = value_type.as_ref() {
                        ripple_context_value = RippleContext {
                            value_type: vt.clone(),
                            stack_offset: 0,
                            owns_value: true,
                        };
                        Some(&ripple_context_value)
                    } else {
                        ripple_context
                    };

                    Some(self.compile_tuple(
                        args.name.clone(),
                        args.fields.clone(),
                        ripple_context_param,
                    )?)
                } else {
                    value_type
                };

                self.compile_tail_call(
                    tail_call.identifier.as_deref(),
                    &tail_call.accessors,
                    arg_type,
                )
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
            ast::Term::BindMatch(pattern) => {
                // Bind matches create new bindings
                let val_type = value_type.ok_or_else(|| {
                    Error::FeatureUnsupported("Bind match requires a value".to_string())
                })?;
                self.compile_match(pattern, val_type, on_no_match, pattern::PatternMode::Bind)
            }
            ast::Term::PinMatch(pattern) => {
                // Pin matches check against existing variables
                let val_type = value_type.ok_or_else(|| {
                    Error::FeatureUnsupported("Pin match requires a value".to_string())
                })?;
                self.compile_match(pattern, val_type, on_no_match, pattern::PatternMode::Pin)
            }
            ast::Term::Spawn(term) => {
                // Spawn a function, optionally with an argument: @f or 42 ~> @f

                if let Some(arg_type) = value_type {
                    // Chained spawn: value ~> @function
                    self.compile_chained_spawn(*term, arg_type)
                } else {
                    // Direct spawn: @function (no argument)
                    // Compile the term to get the function value
                    let term_type = self.compile_term(*term, None, None, None)?;

                    // Extract the parameter, receive and return types from the function
                    let (param_type, receive_type, return_type) =
                        if let Type::Callable(callable) = &term_type {
                            (
                                callable.parameter.clone(),
                                callable.receive.clone(),
                                callable.result.clone(),
                            )
                        } else {
                            return Err(Error::FeatureUnsupported(
                                "Can only spawn functions".to_string(),
                            ));
                        };

                    // Type check: function must take nil as parameter
                    if param_type != Type::nil() {
                        return Err(Error::TypeMismatch {
                            expected: "function with nil parameter".to_string(),
                            found: format!(
                                "function with parameter {}",
                                quiver_core::format::format_type(&self.program, &param_type)
                            ),
                        });
                    }

                    // Emit spawn instruction (pops function, pushes Process)
                    self.codegen.add_instruction(Instruction::Spawn);

                    Ok(Type::Process(Box::new(ProcessType {
                        receive: Some(Box::new(receive_type)),
                        returns: Some(Box::new(return_type)),
                    })))
                }
            }
            ast::Term::Select(select) => {
                // Select operation: !(p1 | p2 | ...) or p ~> !(~) or p ~> !(sources)
                // When chained (p ~> !...), sources can reference the chained value via ripple (~)
                // Note: The parser transforms bare `!` into `!(~)` automatically

                let sources = select.sources.clone();

                if sources.is_empty() {
                    return Err(Error::FeatureUnsupported(
                        "Select requires at least one source".to_string(),
                    ));
                }

                // If there's a chained value, check that it's used (via ripple) in at least one source
                if value_type.is_some() && !Self::select_contains_ripple(&sources) {
                    return Err(Error::FeatureUnsupported(
                        "Chained value must be used in select sources (use ripple operator ~)"
                            .to_string(),
                    ));
                }

                // Compile all sources
                let source_types = self.compile_select_sources(&sources, value_type.as_ref())?;

                // If there was a chained value, remove it from the bottom of the stack
                // Stack is currently: [chained_value | src0 | src1 | ... | src(n-1)]
                // We want: [src0 | src1 | ... | src(n-1)]
                // Use Rotate to move chained_value to top, then Pop it
                if value_type.is_some() {
                    let n = sources.len();
                    // Rotate(n+1) moves the item at depth n (the chained value) to the top
                    self.codegen.add_instruction(Instruction::Rotate(n + 1));
                    self.codegen.add_instruction(Instruction::Pop);
                }

                // Emit Select(n) instruction
                self.codegen
                    .add_instruction(Instruction::Select(sources.len()));

                // Compute and return the select return type
                self.compute_select_return_type(&source_types)
            }
            ast::Term::Self_ => {
                self.codegen.add_instruction(Instruction::Self_);
                // Return a process type with the current function's receive type
                // Return type is None since a process can't know its own return type
                let process_type = ProcessType {
                    receive: Some(Box::new(self.current_receive_type.clone())),
                    returns: None,
                };
                let self_type = Type::Process(Box::new(process_type));

                // Apply value if present (for message sends like `10 ~> .`)
                if let Some(val_type) = value_type {
                    self.apply_value_to_type(self_type, val_type)
                } else {
                    Ok(self_type)
                }
            }
            ast::Term::Ripple => {
                // Ripple evaluates to the chained value
                // Prefer value_type (directly chained) over ripple_context (inherited from parent)
                if let Some(val_type) = value_type {
                    // Value was directly chained to this ripple - use it
                    // The value is already on the stack, no need to Pick
                    Ok(val_type)
                } else if let Some(ctx) = ripple_context {
                    // Inherit ripple context from parent tuple
                    self.codegen
                        .add_instruction(Instruction::Pick(ctx.stack_offset));
                    Ok(ctx.value_type.clone())
                } else {
                    // Neither value_type nor ripple_context available
                    Err(Error::FeatureUnsupported(
                        "Ripple placeholder (~) can only be used when a value is being chained"
                            .to_string(),
                    ))
                }
            }
        }
    }

    fn apply_value_to_type(&mut self, target_type: Type, value_type: Type) -> Result<Type, Error> {
        match target_type {
            Type::Callable(func_type) => {
                // Function call

                // Check if function has type variables - if so, perform unification
                let has_vars_param =
                    typing::contains_variables(&func_type.parameter, &self.program);
                let has_vars_result = typing::contains_variables(&func_type.result, &self.program);

                let result_type = if has_vars_param || has_vars_result {
                    // Perform unification to bind type variables
                    let mut bindings = HashMap::new();

                    typing::unify(
                        &mut bindings,
                        &func_type.parameter,
                        &value_type,
                        &self.program,
                    )?;

                    // Substitute bindings in the result type
                    typing::substitute(&func_type.result, &bindings, &mut self.program)
                } else {
                    // No type variables - just check compatibility
                    if !value_type.is_compatible(&func_type.parameter, &self.program) {
                        return Err(Error::TypeMismatch {
                            expected: format!(
                                "function parameter compatible with {}",
                                quiver_core::format::format_type(
                                    &self.program,
                                    &func_type.parameter
                                )
                            ),
                            found: quiver_core::format::format_type(&self.program, &value_type),
                        });
                    }
                    func_type.result.clone()
                };

                // Check receive type compatibility
                let called_receive_type = &func_type.receive;

                // Check if called function has receives (not Type::never())
                if !matches!(called_receive_type, Type::Union(v) if v.is_empty()) {
                    // Called function has receives - verify current context matches
                    // Check if current context is never (empty union = can't receive)
                    if let Type::Union(variants) = &self.current_receive_type
                        && variants.is_empty()
                    {
                        // Current context can't receive - can't call a receiving function
                        return Err(Error::TypeMismatch {
                            expected: "function without receive type".to_string(),
                            found: format!(
                                "function with receive type {}",
                                quiver_core::format::format_type(
                                    &self.program,
                                    called_receive_type
                                )
                            ),
                        });
                    }

                    // Check compatibility
                    if !called_receive_type.is_compatible(&self.current_receive_type, &self.program)
                    {
                        return Err(Error::TypeMismatch {
                            expected: format!(
                                "function with receive type compatible with {}",
                                quiver_core::format::format_type(
                                    &self.program,
                                    &self.current_receive_type
                                )
                            ),
                            found: format!(
                                "function with receive type {}",
                                quiver_core::format::format_type(
                                    &self.program,
                                    called_receive_type
                                )
                            ),
                        });
                    }
                }

                // Execute the call
                self.codegen.add_instruction(Instruction::Call);
                Ok(result_type)
            }
            Type::Process(process_type) => {
                // Message send
                // Type check: ensure the process has a receive type and message type matches
                if let Some(expected_msg_type) = &process_type.receive {
                    // Check if it's the empty union (never receives)
                    if matches!(expected_msg_type.as_ref(), Type::Union(v) if v.is_empty()) {
                        return Err(Error::TypeMismatch {
                            expected: "process with receive type".to_string(),
                            found: "process without receive type (cannot send messages)"
                                .to_string(),
                        });
                    }
                    if !value_type.is_compatible(expected_msg_type, &self.program) {
                        return Err(Error::TypeMismatch {
                            expected: quiver_core::format::format_type(
                                &self.program,
                                expected_msg_type,
                            ),
                            found: quiver_core::format::format_type(&self.program, &value_type),
                        });
                    }
                } else {
                    // None means unknown receive type
                    return Err(Error::TypeMismatch {
                        expected: "process with known receive type".to_string(),
                        found: "process with unknown receive type".to_string(),
                    });
                }

                // Emit send instruction (expects [message, pid] on stack)
                self.codegen.add_instruction(Instruction::Send);
                // Send returns Ok (like assignment)
                Ok(Type::ok())
            }
            _ => Err(Error::TypeMismatch {
                expected: "function or process".to_string(),
                found: quiver_core::format::format_type(&self.program, &target_type),
            }),
        }
    }

    fn compile_tail_call(
        &mut self,
        identifier: Option<&str>,
        accessors: &[ast::AccessPath],
        arg_type: Option<Type>,
    ) -> Result<Type, Error> {
        if identifier.is_none() && accessors.is_empty() {
            // Tail call to parameter
            if arg_type.is_none() {
                return Err(Error::FeatureUnsupported(
                    "Tail call requires a value".to_string(),
                ));
            }
            // Argument is already on stack, just emit tail call
            self.codegen.add_instruction(Instruction::TailCall(true));
            Ok(Type::never())
        } else {
            // Tail call to identifier with accessors
            let name = identifier.ok_or_else(|| {
                Error::FeatureUnsupported(
                    "Member access in tail call requires an identifier".to_string(),
                )
            })?;

            if arg_type.is_none() {
                return Err(Error::FeatureUnsupported(
                    "Tail call requires a value".to_string(),
                ));
            }

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
                    found: quiver_core::format::format_type(&self.program, &other_type),
                }),
            }
        }
    }

    fn compile_builtin(&mut self, name: &str) -> Result<Type, Error> {
        let (param_type, result_type) = BUILTIN_REGISTRY
            .resolve_signature(name, &mut self.program)
            .ok_or_else(|| Error::BuiltinUndefined(name.to_string()))?;

        let builtin_index = self.program.register_builtin(name.to_string());

        self.codegen
            .add_instruction(Instruction::Builtin(builtin_index));

        Ok(Type::Callable(Box::new(CallableType {
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
            let type_info = self
                .program
                .lookup_type(type_id)
                .ok_or(Error::TypeNotInRegistry { type_id: *type_id })?;
            let fields = &type_info.fields;

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
                if let Some((_, first_field_type)) = fields.first() {
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
                    self.codegen.add_instruction(Instruction::Rotate(2));
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

    fn resolve_accessor_type(
        &self,
        mut current_type: Type,
        accessors: &[ast::AccessPath],
        target_name: &str,
    ) -> Result<Type, Error> {
        for accessor in accessors {
            let tuple_types = current_type.extract_tuple_types();

            if tuple_types.is_empty() {
                return Err(Error::MemberAccessOnNonTuple {
                    target: target_name.to_string(),
                });
            }

            let field_types = match accessor {
                ast::AccessPath::Field(field_name) => {
                    let results = self
                        .get_field_types_by_name(&tuple_types, field_name)
                        .map_err(|_| Error::MemberFieldNotFound {
                            field_name: field_name.clone(),
                            target: target_name.to_string(),
                        })?;
                    if results.is_empty() {
                        return Err(Error::MemberFieldNotFound {
                            field_name: field_name.clone(),
                            target: target_name.to_string(),
                        });
                    }
                    results.into_iter().map(|(_, t)| t).collect()
                }
                ast::AccessPath::Index(index) => self
                    .get_field_types_at_position(&tuple_types, *index)
                    .map_err(|_| Error::MemberAccessOnNonTuple {
                        target: target_name.to_string(),
                    })?,
            };

            current_type = Type::from_types(field_types);
        }

        Ok(current_type)
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
        if !accessors.is_empty()
            && let Some((capture_type, capture_index)) =
                self.lookup_variable(&self.scopes, target, &accessors)
        {
            // We have a pre-evaluated capture for this exact path
            self.codegen
                .add_instruction(Instruction::Load(capture_index));
            return Ok(capture_type);
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
            if let Some(field_name) = get_name(field)
                && !seen_names.insert(field_name.clone())
            {
                return Err(Error::FieldDuplicated(field_name.clone()));
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
            let type_info = self
                .program
                .lookup_type(type_id)
                .ok_or(Error::TypeNotInRegistry { type_id: *type_id })?;
            let fields = &type_info.fields;

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
            // Look up the field in this tuple type
            let tuple_type = self
                .program
                .lookup_type(type_id)
                .ok_or(Error::TypeNotInRegistry { type_id: *type_id })?;
            let (index, (_, field_type)) = tuple_type
                .fields
                .iter()
                .enumerate()
                .find(|(_, field)| field.0.as_deref() == Some(field_name))
                .ok_or(Error::FieldNotFound {
                    field_name: field_name.to_string(),
                    type_name: format!("{:?}", type_id),
                })?;

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

            results.push((index, field_type.clone()));
        }

        Ok(results)
    }
}
