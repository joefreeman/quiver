use std::collections::{HashMap, HashSet};

mod codegen;
mod helpers;
mod modules;
mod narrowing;
use narrowing::{
    Narrowing, analyze_tuple_pattern_for_complement, apply_narrowing, compute_complement,
    get_field_narrowing, get_field_type, narrow_nil_from_new_bindings,
};
mod pattern;
mod provenance;
mod scopes;
mod spread;
mod type_queries;
mod typing;
mod variables;

pub use codegen::InstructionBuilder;
pub use modules::{ModuleCache, compile_type_import};
pub use provenance::{Narrowings, Provenance};
pub use scopes::{Binding, Parameter, Scope, ScopeKind};
pub use typing::{TupleAccessor, TypeAliasDef, resolve_type_alias_for_display, union_type_ids};

use crate::{
    ast,
    modules::{ModuleError, ModuleLoader},
};

use quiver_core::{
    bytecode::{Constant, Function, Instruction},
    program::Program,
    types::{NIL, OK, Type, TypeLookup},
    value::{Binary, Value},
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
    TupleNotInRegistry {
        tuple_id: usize,
    },

    // Structure errors
    FieldDuplicated(String),
    TupleFieldTypeUnresolved {
        field_index: usize,
    },

    // Access errors
    FieldNotFound {
        field_name: String,
        tuple_id: usize,
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
        tuple_id: String,
    },
    OperatorOnNonTuple {
        operator: String,
    },

    // Module errors
    ModuleLoad(ModuleError),
    ModuleParse {
        module: String,
        error: crate::parser::Error,
    },
    ModuleExecution {
        module: String,
        error: quiver_core::error::Error,
    },
    ModuleTypeMissing {
        type_name: String,
        module: String,
    },

    // Language feature errors
    FeatureUnsupported(String),

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

/// Result of compilation containing all outputs
pub struct CompilationResult {
    pub instructions: Vec<Instruction>,
    pub result_type: usize,
    pub receive_type: usize,
    pub bindings: HashMap<String, Binding>,
    pub program: Program,
    pub module_cache: ModuleCache,
}

/// Context for ripple operator (~) usage
/// Tracks the value being rippled and where it is on the stack
struct RippleContext {
    /// Type ID of the value being rippled
    value_type_id: usize,
    /// Stack offset where the ripple value is located
    stack_offset: usize,
    /// Whether this context owns the value and must clean it up
    owns_value: bool,
    /// Provenance of the rippled value for type narrowing
    provenance: Provenance,
}

pub struct Compiler<'a, E: quiver_core::effects::Effect> {
    // Core components
    codegen: InstructionBuilder,
    module_cache: ModuleCache,

    // State management
    scopes: Vec<Scope>,
    local_count: usize,
    program: Program,
    module_loader: &'a dyn ModuleLoader,
    builtins: &'a quiver_core::builtins::BuiltinRegistry<E>,

    process_types: &'a HashMap<usize, (usize, usize)>,

    // Track the receive type ID of the function currently being compiled
    current_receive_type_id: usize,

    _phantom: std::marker::PhantomData<E>,
}

impl<'a, E: quiver_core::effects::Effect> Compiler<'a, E> {
    #[allow(clippy::too_many_arguments)]
    pub fn compile(
        ast_program: ast::Program,
        existing_bindings: &HashMap<String, Binding>,
        module_cache: ModuleCache,
        module_loader: &'a dyn ModuleLoader,
        mut program: Program,
        parameter_type_id: usize,
        process_types: &'a HashMap<usize, (usize, usize)>,
        builtins: &'a quiver_core::builtins::BuiltinRegistry<E>,
    ) -> Result<CompilationResult, Error> {
        let never_id = program.never();

        let mut compiler = Self {
            codegen: InstructionBuilder::new(),
            module_cache,
            scopes: vec![],
            local_count: 0,
            program,
            module_loader,
            builtins,
            process_types,
            current_receive_type_id: never_id,
            _phantom: std::marker::PhantomData,
        };

        // Prepare scope bindings from existing bindings
        let mut scope_bindings = HashMap::new();

        // Clone all existing bindings
        for (name, binding) in existing_bindings {
            scope_bindings.insert(name.clone(), binding.clone());
        }

        // Calculate local_count from variables
        compiler.local_count = existing_bindings
            .values()
            .filter_map(|binding| {
                if let Binding::Variable { index, .. } = binding {
                    Some(*index)
                } else {
                    None
                }
            })
            .max()
            .map(|max_index| max_index + 1)
            .unwrap_or(0);

        // Only allocate parameter slot if we have expressions
        // (CType definitions and imports don't need parameters)
        let has_expressions = ast_program
            .statements
            .iter()
            .any(|s| matches!(s, ast::Statement::Expression(_)));

        let scope_parameter = if has_expressions {
            let param_local = compiler.local_count;
            compiler.local_count += 1;

            compiler.codegen.add_instruction(Instruction::Store);

            Some(scopes::Parameter {
                ty: parameter_type_id,
                index: param_local,
                provenance: Provenance::Parameter,
            })
        } else {
            None
        };

        // Initialize scope with bindings and optional parameter
        compiler.scopes = vec![Scope::new(scope_bindings, scope_parameter, ScopeKind::Root)];

        // Extract receive type from statements (like we do for function bodies)
        // This seeds the receive type from explicit selects in the code.
        // Additional receive types may be adopted during compilation when calling
        // functions that have receive types.
        compiler.current_receive_type_id =
            compiler.extract_receive_type_from_statements(&ast_program.statements)?;

        let num_statements = ast_program.statements.len();
        let mut result_type_id = compiler.program.register_type(Type::nil());
        for (i, statement) in ast_program.statements.into_iter().enumerate() {
            let is_last = i == num_statements - 1;
            let is_expression = matches!(&statement, ast::Statement::Expression(_));

            let statement_type_id = compiler.compile_statement(statement)?;

            // Track the type of the last statement
            if is_last {
                result_type_id = statement_type_id;
            }

            // Pop intermediate expression results, keeping only the last one
            if !is_last && is_expression {
                compiler.codegen.add_instruction(Instruction::Pop);
            }
        }

        // Extract bindings from the global scope
        let bindings: HashMap<String, Binding> = compiler.scopes[0]
            .bindings
            .iter()
            .filter(|(name, _)| !name.starts_with('~')) // Exclude internal variables
            .map(|(name, binding)| (name.clone(), binding.clone()))
            .collect();

        Ok(CompilationResult {
            instructions: compiler.codegen.instructions,
            result_type: result_type_id,
            // Use the final receive type, which may have been widened during compilation
            // when calling functions that have receive types
            receive_type: compiler.current_receive_type_id,
            bindings,
            program: compiler.program,
            module_cache: compiler.module_cache,
        })
    }

    fn compile_statement(&mut self, statement: ast::Statement) -> Result<usize, Error> {
        match statement {
            ast::Statement::TypeAlias {
                name,
                type_parameters,
                type_definition,
            } => {
                self.compile_type_alias(&name, type_parameters, type_definition)?;
                Ok(self.program.register_type(Type::nil()))
            }
            ast::Statement::TypeImport { pattern, module } => {
                self.compile_type_import(pattern, &module)?;
                Ok(self.program.register_type(Type::nil()))
            }
            ast::Statement::Expression(expression) => {
                let (result_type, _) = self.compile_expression(expression, None)?;
                Ok(result_type)
            }
        }
    }

    fn compile_type_alias(
        &mut self,
        name: &str,
        type_parameters: Vec<String>,
        type_definition: ast::Type,
    ) -> Result<(), Error> {
        // Prevent shadowing primitive types
        if helpers::is_reserved_name(name) {
            return Err(Error::TypeUnresolved(format!(
                "Cannot redefine primitive type '{}'",
                name
            )));
        }

        // Validate the AST structure
        Self::validate_type_ast(&type_definition)?;

        // Create Type::Variable bindings for type parameters
        let mut bindings = HashMap::new();
        for param in &type_parameters {
            let var_type_id = self.program.register_type(Type::Variable(param.clone()));
            bindings.insert(param.clone(), var_type_id);
        }

        // Resolve the type definition immediately
        let type_id = typing::resolve_ast_type_with_bindings(
            &self.scopes,
            type_definition,
            &mut self.program,
            &bindings,
        )?;

        // Store the resolved type alias
        scopes::define_type_alias(
            &mut self.scopes,
            name.to_string(),
            TypeAliasDef {
                parameters: type_parameters,
                type_id,
            },
        );
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
            ast::Type::Primitive(_) | ast::Type::Cycle(_) | ast::Type::Resource(_) => Ok(()),
        }
    }

    fn compile_type_import(
        &mut self,
        pattern: ast::TypeImportPattern,
        module: &[String],
    ) -> Result<(), Error> {
        compile_type_import(
            pattern,
            module,
            &mut self.module_cache,
            self.module_loader,
            &mut self.scopes,
            &mut self.program,
        )
    }

    // Helper methods for type operations on type IDs

    /// Check if a type ID represents the never type (empty union)
    fn is_never(&self, type_id: usize) -> bool {
        self.program
            .lookup_type(type_id)
            .map(|t| t.is_never())
            .unwrap_or(false)
    }

    /// Check if a type ID represents the nil type
    fn is_nil(&self, type_id: usize) -> bool {
        self.program
            .lookup_type(type_id)
            .map(|t| t.is_nil())
            .unwrap_or(false)
    }

    /// Check if a type ID is nil, Ok, or a union containing only nil/Ok
    fn is_nil_or_ok(&self, type_id: usize) -> bool {
        match self.program.lookup_type(type_id) {
            Some(Type::Tuple(id))
                if *id == quiver_core::types::NIL || *id == quiver_core::types::OK =>
            {
                true
            }
            Some(Type::Union(members)) => members.iter().all(|&member_id| {
                self.program
                    .lookup_type(member_id)
                    .map(|t| t.is_nil() || t.is_ok())
                    .unwrap_or(false)
            }),
            _ => false,
        }
    }

    /// Check if a type ID contains nil (either is nil or is a union containing nil)
    fn contains_nil(&self, type_id: usize) -> bool {
        self.program
            .lookup_type(type_id)
            .map(|t| t.contains_nil(&self.program))
            .unwrap_or(false)
    }

    /// Get type without nil variants
    fn without_nil(&mut self, type_id: usize) -> usize {
        if let Some(ty) = self.program.lookup_type(type_id) {
            let without_nil = ty.without_nil(&self.program);
            self.program.register_type(without_nil)
        } else {
            self.program.never()
        }
    }

    fn compile_literal(&mut self, literal: ast::Literal) -> Result<usize, Error> {
        match literal {
            ast::Literal::Integer(integer) => {
                let index = self.program.register_constant(Constant::Integer(integer));
                self.codegen.add_instruction(Instruction::Constant(index));
                Ok(self.program.register_type(Type::Integer))
            }
            ast::Literal::Binary(bytes) => {
                let index = self
                    .program
                    .register_constant(Constant::Binary(bytes.clone()));
                self.codegen.add_instruction(Instruction::Constant(index));
                Ok(self.program.register_type(Type::Binary))
            }
        }
    }

    fn compile_tuple(
        &mut self,
        tuple_name: Option<String>,
        fields: Vec<ast::TupleField>,
        ripple_context: Option<&RippleContext>,
    ) -> Result<(usize, Provenance), Error> {
        helpers::check_field_name_duplicates(&fields, |f| f.name.as_ref())?;

        // Check if this tuple contains spreads
        let contains_spread = helpers::tuple_contains_spread(&fields);

        if contains_spread {
            // Use specialized compilation for tuples with spreads
            return spread::compile_tuple_with_spread(self, tuple_name, fields, ripple_context);
        }

        // Compile field values and collect their types and provenances
        let mut field_types = Vec::new();
        let mut field_provenances = Vec::new();
        for (fields_compiled, field) in fields.iter().enumerate() {
            let (field_type, field_prov) = match &field.value {
                ast::FieldValue::Chain(chain) => {
                    // Each field chain receives a copy of the enclosing (piped) value as its
                    // input, so a leading callable field is called with it (and `&` is needed
                    // to pass a callable by value). The original value remains lower on the
                    // stack for nested `~` references and is cleaned up below if owned.
                    let input = ripple_context.map(|ctx| {
                        // Duplicate the piped value to the top of the stack as the input.
                        self.codegen
                            .add_instruction(Instruction::Pick(ctx.stack_offset + fields_compiled));
                        (ctx.value_type_id, ctx.provenance.clone())
                    });
                    // The input value carries the piped value (and its provenance); nested
                    // tuples re-derive their own ripple context from it, so we pass no parent
                    // ripple_context here (which would otherwise have a stale stack offset).
                    self.compile_chain_with_input(chain.clone(), None, None, input, None, false)?
                }
                ast::FieldValue::Spread(_) => {
                    unreachable!("Spread should be handled by compile_tuple_with_spread")
                }
            };
            field_types.push((field.name.clone(), field_type));
            field_provenances.push(field_prov);
        }

        // Register the tuple type and emit instruction
        let tuple_id = self.program.register_tuple(tuple_name, field_types);
        self.codegen.add_instruction(Instruction::Tuple(tuple_id));

        // Clean up ripple value if we own it
        if let Some(ctx) = ripple_context
            && ctx.owns_value
        {
            self.codegen.add_instruction(Instruction::Rotate(2));
            self.codegen.add_instruction(Instruction::Pop);
        }

        Ok((
            self.program.register_type(Type::Tuple(tuple_id)),
            Provenance::Tuple(field_provenances),
        ))
    }

    /// Unify multiple receive types into a single type.
    /// If there's one type, return it. If multiple, create a union.
    /// Deduplicates types to avoid redundant unions.
    fn unify_receive_types(&mut self, receive_types: Vec<usize>) -> usize {
        if receive_types.is_empty() {
            return self.program.never();
        }

        if receive_types.len() == 1 {
            return receive_types[0];
        }

        // Deduplicate types
        let mut unique_types: Vec<usize> = Vec::new();
        for ty in receive_types {
            if !unique_types.contains(&ty) {
                unique_types.push(ty);
            }
        }

        if unique_types.len() == 1 {
            return unique_types[0];
        }

        // Create union of all receive types
        self.program.register_type(Type::Union(unique_types))
    }

    fn extract_receive_type(&mut self, block: Option<&ast::Block>) -> Result<usize, Error> {
        let mut receive_types = Vec::new();
        if let Some(block) = block {
            self.collect_receive_types(block, &mut receive_types)?;
        }

        Ok(self.unify_receive_types(receive_types))
    }

    fn extract_receive_type_from_statements(
        &mut self,
        statements: &[ast::Statement],
    ) -> Result<usize, Error> {
        let mut receive_types = Vec::new();
        for statement in statements {
            if let ast::Statement::Expression(expression) = statement {
                self.collect_receive_types_from_expression(expression, &mut receive_types)?;
            }
        }

        Ok(self.unify_receive_types(receive_types))
    }

    fn collect_receive_types(
        &mut self,
        block: &ast::Block,
        receive_types: &mut Vec<usize>,
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
        receive_types: &mut Vec<usize>,
    ) -> Result<(), Error> {
        for chain in &expression.chains {
            self.collect_receive_types_from_chain(chain, receive_types)?;
        }
        Ok(())
    }

    fn collect_receive_types_from_chain(
        &mut self,
        chain: &ast::Chain,
        receive_types: &mut Vec<usize>,
    ) -> Result<(), Error> {
        // Track the chained type as we traverse the chain
        let mut chained_type: Option<usize> = None;

        for term in &chain.terms {
            chained_type =
                self.collect_receive_types_from_term(term, chained_type, receive_types)?;
        }
        Ok(())
    }

    fn collect_receive_types_from_term(
        &mut self,
        term: &ast::Term,
        chained_type: Option<usize>,
        receive_types: &mut Vec<usize>,
    ) -> Result<Option<usize>, Error> {
        match term {
            ast::Term::Select(sources) => {
                // Extract receive types from all sources
                let mut select_sources = Vec::new();

                match sources {
                    // Handle postfix form: `receiver ~> !` (bare !)
                    // In this case, extract receive type from the chained value
                    None => {
                        if let Some(chained) = chained_type
                            && let Some(Type::Callable { parameter, .. }) =
                                self.program.lookup_type(chained)
                        {
                            select_sources.push(*parameter);
                        }
                    }
                    // Explicit empty `![]` - no receive types
                    Some(sources) if sources.is_empty() => {}
                    // Explicit sources
                    Some(sources) => {
                        for source in sources {
                            self.extract_receive_type_from_source(
                                source,
                                chained_type.as_ref(),
                                &mut select_sources,
                            )?;
                        }
                    }
                }

                // If this select has receive sources, add the unified type
                if !select_sources.is_empty() {
                    if select_sources.len() == 1 {
                        receive_types.push(select_sources[0]);
                    } else {
                        // Multiple sources in same select - create union
                        receive_types.push(self.program.register_type(Type::Union(select_sources)));
                    }
                }
                // Select doesn't produce a chainable type for receive extraction
                Ok(None)
            }
            ast::Term::Access(access) => {
                // Check argument for receive types (e.g., math.div[~, !#int])
                if let Some(arg_fields) = &access.argument {
                    for field in arg_fields {
                        if let ast::FieldValue::Chain(chain) = &field.value {
                            self.collect_receive_types_from_chain(chain, receive_types)?;
                        }
                    }
                }

                // Try to resolve the access to get its type
                if let Some(ast::AccessSource::Identifier(identifier)) = &access.source {
                    let var_type =
                        scopes::lookup_variable(&self.scopes, identifier, &access.accessors)
                            .map(|(t, _)| t)
                            .or_else(|| {
                                let (base_type, _) =
                                    scopes::lookup_variable(&self.scopes, identifier, &[])?;
                                type_queries::resolve_accessor_type(
                                    &mut self.program,
                                    base_type,
                                    &access.accessors,
                                    identifier,
                                )
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
                if let Some(arg_fields) = &builtin.argument {
                    for field in arg_fields {
                        if let ast::FieldValue::Chain(chain) = &field.value {
                            self.collect_receive_types_from_chain(chain, receive_types)?;
                        }
                    }
                }
                Ok(None)
            }
            ast::Term::TailCall(tail_call) => {
                // Check argument tuple for receive types (e.g., &f[!#int, 5])
                if let Some(arg_fields) = &tail_call.argument {
                    for field in arg_fields {
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
        chained_type: Option<&usize>,
        receive_types: &mut Vec<usize>,
    ) -> Result<(), Error> {
        // A source can be:
        // 1. A function literal: #int { ... } -> extract int
        // 2. A variable reference: r1 -> look up and extract parameter type
        // 3. A ripple: ~ -> use the chained type
        // 4. Something else (process, timeout) -> ignore

        for term in &source.terms {
            match term {
                term if term.is_bare_ripple() => {
                    // Ripple refers to the chained value - extract its receive type
                    if let Some(chained_type_id) = chained_type
                        && let Some(Type::Callable { parameter, .. }) =
                            self.program.lookup_type(*chained_type_id)
                    {
                        receive_types.push(*parameter);
                    }
                }
                ast::Term::Function(func) => {
                    // Inline function definition - extract parameter type
                    if let Some(param_type) = &func.parameter_type {
                        let resolved_type = typing::resolve_ast_type(
                            &self.scopes,
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

                    let Some(ast::AccessSource::Identifier(identifier)) = &access.source else {
                        continue;
                    };

                    // Resolve variable type: try full path first, then base + accessor resolution
                    let var_type =
                        scopes::lookup_variable(&self.scopes, identifier, &access.accessors)
                            .map(|(t, _)| t)
                            .or_else(|| {
                                // Full path not found - try resolving accessors through type system
                                let (base_type, _) =
                                    scopes::lookup_variable(&self.scopes, identifier, &[])?;
                                type_queries::resolve_accessor_type(
                                    &mut self.program,
                                    base_type,
                                    &access.accessors,
                                    identifier,
                                )
                                .ok()
                            });

                    if let Some(var_type_id) = var_type
                        && let Some(Type::Callable { parameter, .. }) =
                            self.program.lookup_type(var_type_id)
                    {
                        receive_types.push(*parameter);
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

    fn compile_function(&mut self, function: ast::Function) -> Result<usize, Error> {
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

        let unique_captures = variables::collect_free_variables(
            function.body.as_ref(),
            &function_params,
            &|name, accessors| {
                // Check if the full path (base + accessors) is defined, or just the base
                scopes::lookup_variable(&self.scopes, name, accessors).is_some()
                    || scopes::lookup_variable(&self.scopes, name, &[]).is_some()
            },
        );

        // Resolve parameter type with declared type parameters
        let parameter_type = match &function.parameter_type {
            Some(t) => typing::resolve_function_parameter_type(
                &self.scopes,
                t.clone(),
                &function.type_parameters,
                &mut self.program,
            )?,
            None => self.program.register_type(Type::nil()),
        };

        // Extract receive type from function body
        let receive_type = self.extract_receive_type(function.body.as_ref())?;

        let saved_instructions = std::mem::take(&mut self.codegen.instructions);
        let saved_scopes = std::mem::take(&mut self.scopes);
        let saved_local_count = self.local_count;
        let saved_receive_type = self.current_receive_type_id;

        // Extract type aliases from parent scopes to preserve in function scope
        let mut function_scope_bindings = HashMap::new();
        for scope in &saved_scopes {
            for (name, binding) in &scope.bindings {
                if let Binding::TypeAlias(_) = binding {
                    function_scope_bindings.insert(name.clone(), binding.clone());
                }
            }
        }

        self.scopes = vec![Scope::new(function_scope_bindings, None, ScopeKind::Root)];
        self.codegen.instructions = Vec::new();
        self.local_count = 0;
        self.current_receive_type_id = receive_type;

        // Define captures as first locals in function body scope
        for capture in &unique_captures {
            // Determine the type of the captured value
            // First check if the full path is already available (for nested captures)
            let capture_type = if let Some((full_type, _)) =
                scopes::lookup_variable(&saved_scopes, &capture.base, &capture.accessors)
            {
                // The full path is already available from parent, use its type
                full_type
            } else if capture.accessors.is_empty() {
                // Simple capture - use the base variable's type
                if let Some((var_type, _)) =
                    scopes::lookup_variable(&saved_scopes, &capture.base, &[])
                {
                    var_type
                } else {
                    continue;
                }
            } else {
                // Capture with accessors - need to compute the accessed type
                if let Some((var_type, _)) =
                    scopes::lookup_variable(&saved_scopes, &capture.base, &[])
                {
                    // Use compile_accessor logic to determine type
                    // We need to compute this without generating bytecode
                    let mut last_type = var_type;

                    for accessor in &capture.accessors {
                        let field_types = match accessor {
                            ast::AccessPath::Field(field_name) => {
                                match type_queries::get_field_by_name(
                                    &self.program,
                                    last_type,
                                    field_name,
                                    &capture.base,
                                ) {
                                    Ok((_, types)) => types,
                                    _ => continue,
                                }
                            }
                            ast::AccessPath::Index(index) => {
                                match type_queries::get_field_at_index(
                                    &self.program,
                                    last_type,
                                    *index,
                                    &capture.base,
                                ) {
                                    Ok(types) => types,
                                    _ => continue,
                                }
                            }
                        };
                        last_type = typing::union_type_ids(&mut self.program, field_types);
                    }
                    last_type
                } else {
                    continue;
                }
            };

            scopes::define_variable(
                &mut self.scopes,
                &mut self.local_count,
                &capture.base,
                &capture.accessors,
                capture_type,
                Provenance::Unknown, // Captures don't track provenance
            )?;
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
                        &self.scopes,
                        type_def.clone(),
                        &mut self.program,
                    )?;
                    parameter_fields.insert(field_name.clone(), (field_index, field_type));
                }
            }
        }
        let body_type = match function.body {
            Some(body) => {
                // Function parameters have Provenance::Parameter since they come from callers
                self.compile_block(
                    body,
                    parameter_type,
                    Provenance::Parameter,
                    None,
                    ScopeKind::Function,
                )?
            }
            None => {
                // Identity function: just return the parameter
                // The calling convention puts the parameter on the stack,
                // so we don't need any instructions - just leave it there
                parameter_type
            }
        };

        // Validate return type if specified
        if let Some(return_type_ast) = &function.return_type {
            let expected_return_type = typing::resolve_function_parameter_type(
                &self.scopes,
                return_type_ast.clone(),
                &function.type_parameters,
                &mut self.program,
            )?;

            // For generic functions, we need strict type equality (not just compatibility)
            // because type variables should match exactly, not be compatible with concrete types
            let types_match = if !function.type_parameters.is_empty() {
                // For generic functions: require exact type equality
                body_type == expected_return_type
            } else {
                // For non-generic functions: use compatibility check
                quiver_core::types::is_compatible(body_type, expected_return_type, &self.program)
            };

            if !types_match {
                // Get types for error message
                let expected_type = self.program.lookup_type(expected_return_type).unwrap();
                let found_type = self.program.lookup_type(body_type).unwrap();
                return Err(Error::TypeMismatch {
                    expected: quiver_core::format::format_type(&self.program, expected_type),
                    found: quiver_core::format::format_type(&self.program, found_type),
                });
            }
        }

        let function_instructions = std::mem::take(&mut self.codegen.instructions);

        // Create type information for the function
        let callable_type_id = self.program.register_type(Type::Callable {
            parameter: parameter_type,
            result: body_type,
            receive: receive_type,
        });

        let function_index = self.program.register_function(Function {
            instructions: function_instructions,
            captures: unique_captures.len(),
            type_id: callable_type_id,
        });

        self.codegen.instructions = saved_instructions;
        self.scopes = saved_scopes;
        self.local_count = saved_local_count;
        self.current_receive_type_id = saved_receive_type;

        // Emit instructions to push capture values onto the stack
        // These will be popped by the Function instruction
        for capture in &unique_captures {
            // First check if the full path is available (for nested captures)
            if let Some((_, full_index)) =
                scopes::lookup_variable(&self.scopes, &capture.base, &capture.accessors)
            {
                // The full path is already captured, just load it
                self.codegen.add_instruction(Instruction::Load(full_index));
            } else if let Some((var_type, base_index)) =
                scopes::lookup_variable(&self.scopes, &capture.base, &[])
            {
                // Load the base variable
                self.codegen.add_instruction(Instruction::Load(base_index));

                if !capture.accessors.is_empty() {
                    // Apply accessors to get the final value
                    self.compile_accessor(
                        var_type,
                        capture.accessors.clone(),
                        &capture.base,
                        Provenance::Unknown,
                    )?;
                }
            }
        }

        self.codegen
            .add_instruction(Instruction::Function(function_index));

        Ok(callable_type_id)
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_match(
        &mut self,
        pattern: ast::Match,
        value_type: usize,
        value_provenance: Provenance,
        on_no_match: Option<usize>,
        return_ok: bool,
        mut narrowing: Option<&mut Narrowing>,
    ) -> Result<usize, Error> {
        let start_jump_addr = self.codegen.emit_jump_placeholder();
        let fail_jump_addr = self.codegen.emit_jump_placeholder();

        self.codegen.patch_jump_to_here(start_jump_addr);

        // Analyze pattern to get bindings without generating code yet
        let (bindings, binding_sets, result_type) = pattern::analyze_pattern(
            &mut self.program,
            &pattern,
            value_type,
            &self.scopes,
            &value_provenance,
        )?;

        // Check if pattern has non-type requirements (literals, variable pins, path equality).
        // Patterns with non-type requirements cannot use complement narrowing.
        // Exception: tuple patterns with a single type-constraining field CAN use
        // field-specific complement narrowing, so don't disable in that case.
        let has_tuple_complement =
            analyze_tuple_pattern_for_complement(&pattern, value_type, &mut self.program).is_some();

        if pattern::prevents_complement_narrowing(&binding_sets, &self.program)
            && !has_tuple_complement
            && let Some(n) = narrowing.as_mut()
        {
            n.disable();
        }

        // If result type is never (empty union), pattern won't match - skip pattern matching code
        if self.is_never(result_type) {
            self.codegen.add_instruction(Instruction::Pop);
            self.codegen.add_instruction(Instruction::Tuple(NIL));
            return Ok(self.program.register_type(Type::nil()));
        }

        // Register locals for all bindings (indices needed for Load)
        for (variable_name, variable_type) in &bindings {
            // Check for reserved primitive type names
            if helpers::is_reserved_name(variable_name) {
                return Err(Error::TypeUnresolved(format!(
                    "Cannot use reserved primitive type '{}' as a variable name",
                    variable_name
                )));
            }

            let local_index = self.local_count;
            self.local_count += 1;

            // Register in scope
            // For simple identifier bindings (single binding), preserve the value's provenance
            // so tuple field provenance is preserved. For complex patterns (destructuring),
            // use Unknown since path resolution is complex.
            let var_provenance = if bindings.len() == 1 {
                value_provenance.clone()
            } else {
                Provenance::Unknown
            };

            if let Some(scope) = self.scopes.last_mut() {
                scope.bindings.insert(
                    variable_name.clone(),
                    Binding::Variable {
                        ty: *variable_type,
                        index: local_index,
                        provenance: var_provenance,
                    },
                );
            }
        }

        // Generate pattern matching code (Store instructions push locals)
        // Use on_no_match if provided (for receive blocks), otherwise use fail_jump_addr
        let fail_target = on_no_match.unwrap_or(fail_jump_addr);
        pattern::generate_pattern_code(
            &mut self.codegen,
            &mut self.program,
            &self.scopes,
            &binding_sets,
            fail_target,
        )?;

        // Apply narrowing to the matched value's provenance if the pattern narrows the type.
        // This is done here on the success path - the type has been narrowed by the pattern.
        // Note: result_type is the narrowed type from analyze_pattern.
        if !self.is_never(result_type) && !self.is_nil(result_type) {
            apply_narrowing(
                &mut self.scopes,
                &value_provenance,
                result_type,
                &mut self.program,
            );
        }

        // Record the narrowing for complement narrowing in blocks.
        // This must happen even when result_type is nil, so that subsequent branches
        // know the value is NOT nil (complement narrowing).
        if !self.is_never(result_type)
            && let Some(n) = narrowing
        {
            // For tuple patterns, check if we should record field-specific narrowing
            // instead of whole-value narrowing.
            // This enables patterns like `=[Nil, ys]` to narrow the first field
            // so subsequent branches know the first field is NOT Nil.
            let field_complement_info =
                analyze_tuple_pattern_for_complement(&pattern, value_type, &mut self.program)
                    .and_then(|(field_idx, constrained_type)| {
                        // Use narrowed field type if available (from complement of previous branch),
                        // otherwise fall back to static field type from tuple definition.
                        // This ensures that for subsequent branches, the "original" type
                        // is the narrowed type, so complement calculation is correct.
                        let original =
                            get_field_narrowing(&self.scopes, &value_provenance, field_idx)
                                .or_else(|| get_field_type(value_type, field_idx, &self.program))?;
                        Some((field_idx, original, constrained_type))
                    });

            if let Some((field_idx, original_field_type, constrained_type)) = field_complement_info
            {
                // Record field-specific narrowing for complement
                let field_provenance =
                    Provenance::Field(Box::new(value_provenance.clone()), field_idx);
                n.record(
                    &field_provenance,
                    original_field_type,
                    constrained_type,
                    &mut self.program,
                );
            } else {
                // Standard whole-value narrowing
                n.record(
                    &value_provenance,
                    value_type,
                    result_type,
                    &mut self.program,
                );
            }
        }

        // Success path: leave the value on the stack, or replace with Ok
        if return_ok {
            self.codegen.add_instruction(Instruction::Pop);
            self.codegen.add_instruction(Instruction::Tuple(OK));
        }
        let success_jump_addr = self.codegen.emit_jump_placeholder();

        // Only patch fail_jump_addr if we didn't use on_no_match
        if on_no_match.is_none() {
            self.codegen.patch_jump_to_here(fail_jump_addr);
        }
        // Failure path: pop the value and push nil
        self.codegen.add_instruction(Instruction::Pop);
        self.codegen.add_instruction(Instruction::Tuple(NIL));

        self.codegen.patch_jump_to_here(success_jump_addr);

        // Compute final type - if return_ok, replace value type with Ok
        let final_type = if return_ok {
            if self.contains_nil(result_type) {
                let ok_type_id = self.program.register_type(Type::ok());
                let nil_type_id = self.program.register_type(Type::nil());
                typing::union_type_ids(&mut self.program, vec![ok_type_id, nil_type_id])
            } else {
                self.program.register_type(Type::ok())
            }
        } else {
            result_type
        };

        Ok(final_type)
    }

    fn compile_block(
        &mut self,
        block: ast::Block,
        parameter_type: usize,
        parameter_provenance: Provenance,
        on_no_match: Option<usize>,
        scope_kind: ScopeKind,
    ) -> Result<usize, Error> {
        let mut next_branch_jumps = Vec::new();
        let mut end_jumps = Vec::new();
        let mut branch_types = Vec::new();
        let mut branch_starts = Vec::new();

        // Record locals count before block
        let locals_before = self.local_count;

        // Allocate local for block parameter
        let param_local = self.local_count;
        self.local_count += 1;

        // Store parameter from stack
        self.codegen.add_instruction(Instruction::Store);

        // Push new scope with parameter
        self.scopes.push(Scope::new(
            HashMap::new(),
            Some(scopes::Parameter {
                ty: parameter_type,
                index: param_local,
                provenance: parameter_provenance,
            }),
            scope_kind,
        ));

        // Track pending complement narrowing from previous branch
        let mut pending_complement: Option<(Provenance, usize)> = None;

        // Track whether the block exhaustively covers all type variants.
        // Assume not exhaustive until proven otherwise by complement narrowing.
        let mut is_exhaustive = false;

        for (i, branch) in block.branches.iter().enumerate() {
            let is_last_branch = i == block.branches.len() - 1;

            branch_starts.push(self.codegen.instructions.len());

            // Track complement from previous branch for potential propagation
            let mut applied_complement: Option<(Provenance, usize)> = None;

            if i > 0 {
                self.codegen.add_instruction(Instruction::Pop);
                // Don't emit Clear here - branch variables are only allocated if the branch matches
                // So if we jump to this branch, the previous branch's variables were never allocated
                // Reset local count to after parameter (locals from previous branch are "forgotten")
                self.local_count = param_local + 1;
                // Clear scope bindings and narrowings from previous branch
                let scope = self.scopes.last_mut().ok_or_else(|| Error::InternalError {
                    message: "No scope available when compiling block branch".to_string(),
                })?;
                scope.bindings.clear();
                scope.narrowings = provenance::Narrowings::default();

                // Apply complement narrowing from previous branch (if available)
                // Keep a copy to propagate if this branch doesn't record its own narrowing
                applied_complement = pending_complement.take();
                if let Some((ref prov, complement)) = applied_complement {
                    apply_narrowing(&mut self.scopes, prov, complement, &mut self.program);
                }
            }

            // Create a narrowing instance for this branch's condition
            let mut narrowing = Narrowing::new();
            // Track if we had a complement from previous branch (to propagate if needed)
            let had_previous_complement = applied_complement.is_some();

            // Record existing bindings before compiling condition - we'll narrow new ones later
            let bindings_before_condition: std::collections::HashSet<String> = self
                .scopes
                .last()
                .map(|s| s.bindings.keys().cloned().collect())
                .unwrap_or_default();

            // Compile the condition expression - it can use ~> to access the parameter
            // We need both the type and provenance for forward narrowing
            let (condition_type, condition_prov) = self.compile_expression_with_input(
                branch.condition.clone(),
                on_no_match,
                None,
                Some(&mut narrowing),
            )?;

            // If complement narrowing is valid, compute complement for exhaustiveness check
            // and (for non-last branches) to narrow subsequent branches
            if let Some((prov, original, narrowed)) = narrowing.take() {
                let complement = compute_complement(original, narrowed, &mut self.program);
                if self.is_never(complement) {
                    // All variants covered - block is exhaustive
                    is_exhaustive = true;
                } else if !is_last_branch {
                    // Store complement for narrowing subsequent branches
                    pending_complement = Some((prov, complement));
                }
            } else if had_previous_complement && !is_last_branch {
                // This branch didn't record its own narrowing, but we had a complement
                // from a previous branch. Propagate it to subsequent branches.
                // This handles cases like { a ~> =None | f[a] | g[a] } where the
                // narrowing from branch 1 should apply to branches 2 and 3.
                pending_complement = applied_complement;
            }

            // If condition is compile-time NIL (won't match), skip this branch entirely
            if self.is_nil(condition_type) {
                // Only include nil in result type if this is the last branch
                // Otherwise, nil causes fallthrough to the next branch
                if is_last_branch {
                    branch_types.push(condition_type);
                }
                continue;
            }

            if let Some(ref consequence) = branch.consequence {
                // Branch has a consequence - compile it
                // Use emit_duplicate_jump_if_nil (without pop) to keep the value on stack for consequence
                let next_branch_jump = self.codegen.emit_duplicate_jump_if_nil();

                // Track: jump address, target branch index, whether cleanup needed
                // Cleanup resets locals to branch start (param_local + 1)
                let needs_cleanup = self.local_count > param_local + 1;
                next_branch_jumps.push((next_branch_jump, i + 1, needs_cleanup));

                // Apply forward narrowing: if condition succeeded (non-nil), narrow to exclude nil.
                // This enables type refinement like { a => %math.add[a, 1] } when a: [] | int
                if self.contains_nil(condition_type) {
                    let truthy_type = self.without_nil(condition_type);
                    // Narrow the source variable if it has trackable provenance
                    if !matches!(condition_prov, Provenance::Unknown) {
                        apply_narrowing(
                            &mut self.scopes,
                            &condition_prov,
                            truthy_type,
                            &mut self.program,
                        );
                    }
                    // Narrow all bindings created during the condition that contain nil.
                    // This handles cases like { 0 ~> f ~> =x => ... } where x has Unknown
                    // provenance but should still be narrowed when the condition succeeds.
                    narrow_nil_from_new_bindings(
                        &mut self.scopes,
                        &bindings_before_condition,
                        &mut self.program,
                    );
                }

                // Pop the condition result - consequence starts fresh with block parameter
                self.codegen.add_instruction(Instruction::Pop);

                // Consequence is a new chain that starts with the block's parameter value
                // (not the condition's result). Every chain implicitly starts with the
                // surrounding block's parameter. Pass None for input_type so the consequence
                // loads the parameter via implicit_continuation.
                let (consequence_type, _) = self.compile_expression_with_input(
                    consequence.clone(),
                    None,
                    None, // No input - consequence loads parameter via implicit_continuation
                    None, // No narrowing for consequence
                )?;
                branch_types.push(consequence_type);

                // If this is the last branch and condition always succeeds (non-nil), block is exhaustive
                if is_last_branch && !self.contains_nil(condition_type) {
                    is_exhaustive = true;
                }

                // Reset to branch start (param_local + 1), keeping just the parameter
                // This is on the success path - bindings have been stored and should be cleared
                if self.local_count > param_local + 1 {
                    self.codegen
                        .add_instruction(Instruction::Reset(param_local + 1));
                }
            } else {
                // No consequence - use condition type
                // For non-last branches, filter out nil since it causes fallthrough to next branch
                if is_last_branch {
                    branch_types.push(condition_type);
                    // If the last branch's condition never returns nil, block is exhaustive
                    if !self.contains_nil(condition_type) {
                        is_exhaustive = true;
                    }
                } else {
                    // Only include non-nil types - nil will be handled by subsequent branches
                    if let Some(ty) = self.program.lookup_type(condition_type) {
                        match ty {
                            Type::Union(types) => {
                                let non_nil_types: Vec<usize> = types
                                    .iter()
                                    .filter(|&type_id| !self.is_nil(*type_id))
                                    .copied()
                                    .collect();
                                if !non_nil_types.is_empty() {
                                    branch_types.push(typing::union_type_ids(
                                        &mut self.program,
                                        non_nil_types,
                                    ));
                                }
                            }
                            _ if !self.is_nil(condition_type) => {
                                branch_types.push(condition_type);
                            }
                            _ => {
                                // Condition is purely nil - will always fall through
                            }
                        }
                    }
                }

                // Reset to branch start (param_local + 1), keeping just the parameter
                // Reset is safe regardless of whether condition succeeded: if pattern matching
                // failed (returned nil), no bindings were stored so Reset is a no-op.
                if self.local_count > param_local + 1 {
                    self.codegen
                        .add_instruction(Instruction::Reset(param_local + 1));
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

        // Reset to clear the parameter (branches have already reset their specific locals)
        // Save address for end_jumps patching
        let param_clear_addr = self.codegen.instructions.len();
        self.codegen
            .add_instruction(Instruction::Reset(locals_before));

        // Emit cleanup blocks for branches that need to reset locals before jumping
        // Track jumps that need to be patched to the final end address
        let mut final_end_jumps = Vec::new();

        // Check if we need cleanup blocks (any branch needs cleanup)
        let has_cleanup_blocks = next_branch_jumps
            .iter()
            .any(|(_, _, needs_cleanup)| *needs_cleanup);

        // If there are cleanup blocks, emit a jump to skip them on the success path
        let skip_cleanup_jump = if has_cleanup_blocks {
            Some(self.codegen.emit_jump_placeholder())
        } else {
            None
        };

        // Process each branch jump
        for (jump_addr, next_branch_idx, needs_cleanup) in next_branch_jumps {
            // Determine target: next branch start, on_no_match handler, or final end
            let target_addr = if next_branch_idx < branch_starts.len() {
                Some(branch_starts[next_branch_idx])
            } else {
                on_no_match
            };

            if needs_cleanup {
                // Emit cleanup block: Reset locals to branch start, then jump to target
                let cleanup_addr = self.codegen.instructions.len();
                self.codegen
                    .add_instruction(Instruction::Reset(param_local + 1));

                if let Some(addr) = target_addr {
                    self.codegen.emit_jump_to_addr(addr);
                } else {
                    // Target is final end - will patch later
                    final_end_jumps.push(self.codegen.emit_jump_placeholder());
                }

                // Patch original jump to point to cleanup block
                self.codegen.patch_jump_to_addr(jump_addr, cleanup_addr);
            } else {
                // No cleanup needed - patch directly to target
                if let Some(addr) = target_addr {
                    self.codegen.patch_jump_to_addr(jump_addr, addr);
                } else {
                    // Target is final end - will patch later
                    final_end_jumps.push(jump_addr);
                }
            }
        }

        // Patch the skip-cleanup jump and all final-end jumps to current position
        if let Some(skip_jump) = skip_cleanup_jump {
            self.codegen.patch_jump_to_here(skip_jump);
        }
        for jump_addr in final_end_jumps {
            self.codegen.patch_jump_to_here(jump_addr);
        }

        // Patch end_jumps to go to param clear
        for jump_addr in end_jumps {
            self.codegen.patch_jump_to_addr(jump_addr, param_clear_addr);
        }

        // Reset local count so future variables reuse these indexes
        self.local_count = locals_before;

        // Pop scope
        self.scopes.pop();

        // If the block is not exhaustive, add nil to the result type
        // (since some inputs might not match any branch)
        if !is_exhaustive {
            let nil_type = self.program.register_type(Type::nil());
            branch_types.push(nil_type);
        }

        Ok(typing::union_type_ids(&mut self.program, branch_types))
    }

    fn compile_expression(
        &mut self,
        expression: ast::Expression,
        on_no_match: Option<usize>,
    ) -> Result<(usize, Provenance), Error> {
        self.compile_expression_with_input(expression, on_no_match, None, None)
    }

    /// Compile an expression and return both the result type and provenance.
    fn compile_expression_with_input(
        &mut self,
        expression: ast::Expression,
        on_no_match: Option<usize>,
        input_type: Option<usize>,
        mut narrowing: Option<&mut Narrowing>,
    ) -> Result<(usize, Provenance), Error> {
        let has_input = input_type.is_some();
        let mut last_type = input_type;
        let mut last_prov = Provenance::Unknown;
        let mut end_jumps = Vec::new();

        for (i, chain) in expression.chains.iter().enumerate() {
            // Track bindings before this chain for inter-chain narrowing
            let bindings_before_chain: std::collections::HashSet<String> = self
                .scopes
                .last()
                .map(|s| s.bindings.keys().cloned().collect())
                .unwrap_or_default();

            let (chain_type, chain_prov) = self.compile_chain_with_input(
                chain.clone(),
                on_no_match,
                None,
                if i == 0 {
                    last_type.map(|t| (t, Provenance::Unknown))
                } else {
                    None
                },
                narrowing.as_deref_mut(),
                true, // implicit_continuation: block-level chains load parameter
            )?;

            // Check if the previous type contained nil and we're not on the first chain
            let should_propagate_nil =
                i > 0 && last_type.as_ref().is_some_and(|&t| self.contains_nil(t));

            // Special handling when input_type was provided (consequence with multiple chains)
            // In this case, chains are alternatives and we should combine their success types
            if has_input && i > 0 {
                // Strip nil from previous chains since it causes jump to next chain
                let prev_success_types: Vec<usize> = if let Some(prev) = last_type {
                    if let Some(ty) = self.program.lookup_type(prev) {
                        match ty {
                            Type::Union(types) => types
                                .iter()
                                .filter(|&type_id| !self.is_nil(*type_id))
                                .copied()
                                .collect(),
                            _ if !self.is_nil(prev) => vec![prev],
                            _ => vec![],
                        }
                    } else {
                        vec![]
                    }
                } else {
                    vec![]
                };

                let mut combined = prev_success_types;
                combined.push(chain_type);
                last_type = Some(typing::union_type_ids(&mut self.program, combined));
                // Multiple chains = unknown provenance
                last_prov = Provenance::Unknown;
            } else {
                // Normal case: propagate nil if previous chain could be nil
                last_type = Some(if should_propagate_nil {
                    let nil_type_id = self.program.register_type(Type::nil());
                    typing::union_type_ids(&mut self.program, vec![chain_type, nil_type_id])
                } else {
                    chain_type
                });
                last_prov = chain_prov;
            }

            // If last_type is NIL, subsequent chains are unreachable - break early
            if let Some(last_type_id) = last_type
                && self.is_nil(last_type_id)
            {
                break;
            }

            if i < expression.chains.len() - 1 {
                let end_jump = self.codegen.emit_duplicate_jump_if_nil_pop();
                end_jumps.push(end_jump);

                // After the jump, if chain could be nil, narrow bindings created in this chain.
                // This handles cases like `a ~> =x, %math.add[x, 1]` where x needs to be
                // narrowed before the next chain uses it.
                if self.contains_nil(chain_type) {
                    narrow_nil_from_new_bindings(
                        &mut self.scopes,
                        &bindings_before_chain,
                        &mut self.program,
                    );
                }
            }
        }

        let end_addr = self.codegen.instructions.len();
        for jump_addr in end_jumps {
            self.codegen.patch_jump_to_addr(jump_addr, end_addr);
        }

        let result_type = last_type.ok_or_else(|| Error::InternalError {
            message: "Expression compiled with no chains".to_string(),
        })?;
        Ok((result_type, last_prov))
    }

    fn compile_chain(
        &mut self,
        chain: ast::Chain,
        on_no_match: Option<usize>,
        ripple_context: Option<&RippleContext>,
    ) -> Result<usize, Error> {
        let (ty, _prov) = self.compile_chain_with_provenance(chain, on_no_match, ripple_context)?;
        Ok(ty)
    }

    /// Compile a chain within a tuple field (no implicit continuation).
    /// The field chain has no initial input - it uses ripple_context for `~`.
    fn compile_chain_with_provenance(
        &mut self,
        chain: ast::Chain,
        on_no_match: Option<usize>,
        ripple_context: Option<&RippleContext>,
    ) -> Result<(usize, Provenance), Error> {
        // Tuple fields don't have implicit continuation - they start with no input
        self.compile_chain_with_input(chain, on_no_match, ripple_context, None, None, false)
    }

    fn compile_chain_with_input(
        &mut self,
        chain: ast::Chain,
        on_no_match: Option<usize>,
        ripple_context: Option<&RippleContext>,
        input: Option<(usize, Provenance)>,
        mut narrowing: Option<&mut Narrowing>,
        implicit_continuation: bool,
    ) -> Result<(usize, Provenance), Error> {
        // Determine initial value:
        // - If input_type is provided, use it (value already on stack)
        // - If implicit_continuation is true, load the parameter from scope
        // - Otherwise, no initial value (for tuple field chains)
        let (mut current_type, mut current_prov) = if let Some((input_type, input_prov)) = input {
            // Input provided (e.g., from a tuple field, consequence, or previous expression term)
            (Some(input_type), input_prov)
        } else if implicit_continuation {
            // Load the parameter from scope (implicit continuation)
            let (parameter_type, param_local) = scopes::get_parameter(&self.scopes)?;
            self.codegen.add_instruction(Instruction::Load(param_local));
            (Some(parameter_type), Provenance::Parameter)
        } else {
            // No initial input (tuple field chains use ripple_context for ~)
            (None, Provenance::Unknown)
        };

        let terms: Vec<_> = chain.terms.into_iter().collect();
        let num_terms = terms.len();
        for (i, term) in terms.iter().enumerate() {
            let is_last_term = i == num_terms - 1;
            // Check if the NEXT term needs the full type including nil:
            // - Blocks need it for proper pattern matching in branches
            // - Match needs it for proper type checking
            let next_needs_full_type = !is_last_term
                && matches!(
                    terms.get(i + 1),
                    Some(ast::Term::Block(_))
                        | Some(ast::Term::Select(_))
                        | Some(ast::Term::Match(_))
                );

            let (term_type, term_prov) = self.compile_term(
                term.clone(),
                current_type,
                current_prov,
                on_no_match,
                ripple_context,
                narrowing.as_deref_mut(),
            )?;
            // For non-last terms that aren't followed by pattern matching terms: if the type
            // contains nil as part of a union (not nil alone), strip nil for subsequent terms.
            // At runtime, nil causes the chain to short-circuit (or fail), so subsequent
            // non-pattern terms only see non-nil values.
            // Pattern matching terms need the full type for proper type checking.
            // Note: Don't strip if the type IS nil (not a union containing nil), as that
            // would result in the never type which breaks pattern matching.
            let should_strip_nil = !is_last_term
                && !next_needs_full_type
                && self.contains_nil(term_type)
                && !self.is_nil(term_type); // Don't strip if type IS nil
            current_type = Some(if should_strip_nil {
                self.without_nil(term_type)
            } else {
                term_type
            });
            current_prov = term_prov;
        }

        let result_type = current_type.ok_or_else(|| Error::InternalError {
            message: "Chain compiled with no terms and no continuation".to_string(),
        })?;

        // If there's a match pattern, apply it
        if let Some(pattern) = chain.match_pattern {
            let ty = self.compile_match(
                pattern,
                result_type,
                current_prov.clone(),
                on_no_match,
                true, // Direct assignment returns Ok
                narrowing,
            )?;
            Ok((ty, current_prov))
        } else {
            Ok((result_type, current_prov))
        }
    }

    /// Resolve an import with optional accessor chain, returning the cached module,
    /// resolved value, and type. Does not emit any instructions.
    fn resolve_import(
        &mut self,
        module: &[String],
        accessors: &[ast::AccessPath],
    ) -> Result<(modules::CachedModule, Value, usize), Error> {
        let module_name = module.join("/");

        // Check for circular imports
        if self.module_cache.import_stack.contains(&module.to_vec()) {
            return Err(Error::FeatureUnsupported(
                "Circular import detected".to_string(),
            ));
        }

        // Get or compute cached module value
        let cached = if let Some(cached) = self.module_cache.get_cached_module(module) {
            cached.clone()
        } else {
            self.module_cache.import_stack.push(module.to_vec());
            let cached = self.import_and_cache_module(module)?;
            self.module_cache.import_stack.pop();
            cached
        };

        // Resolve accessor chain on the cached value
        let module_type_id = self.program.register_type(cached.module_type.clone());
        let (resolved_value, resolved_type) =
            self.resolve_accessors(&cached.value, &module_type_id, accessors, &module_name)?;

        Ok((cached, resolved_value, resolved_type))
    }

    /// Compile an import with optional accessor chain.
    /// Resolves accessors statically on the cached module value, emitting only
    /// instructions needed for the resolved value.
    fn compile_import(
        &mut self,
        module: &[String],
        accessors: &[ast::AccessPath],
    ) -> Result<usize, Error> {
        let (cached, resolved_value, resolved_type) = self.resolve_import(module, accessors)?;

        // Emit instructions for just the resolved value
        let (instructions, _) =
            self.value_to_instructions_from_cache(&resolved_value, &cached.binary_data)?;

        for instruction in instructions {
            self.codegen.add_instruction(instruction);
        }

        Ok(resolved_type)
    }

    /// Import a module, execute it, and cache the result.
    fn import_and_cache_module(
        &mut self,
        module: &[String],
    ) -> Result<modules::CachedModule, Error> {
        // Parse the module
        let parsed = self
            .module_cache
            .load_and_cache_ast(module, self.module_loader)?;

        // Save current compiler state
        let saved_instructions = std::mem::take(&mut self.codegen.instructions);
        let saved_scopes = std::mem::take(&mut self.scopes);
        let saved_local_count = self.local_count;

        // Reset to clean state for module compilation
        self.local_count = 0;

        // Check if module has expressions (needs a parameter scope for implicit continuation)
        let has_expressions = parsed
            .statements
            .iter()
            .any(|s| matches!(s, ast::Statement::Expression(_)));

        let scope_parameter = if has_expressions {
            let param_local = self.local_count;
            self.local_count += 1;
            self.codegen.add_instruction(Instruction::Store);
            let nil_type_id = self.program.register_type(Type::nil());
            Some(scopes::Parameter {
                ty: nil_type_id,
                index: param_local,
                provenance: Provenance::Parameter,
            })
        } else {
            None
        };

        self.scopes = vec![Scope::new(HashMap::new(), scope_parameter, ScopeKind::Root)];

        // Compile statements and track the result type of the last statement
        let num_statements = parsed.statements.len();
        let mut result_type_id = self.program.register_type(Type::nil());
        for (i, statement) in parsed.statements.into_iter().enumerate() {
            let is_last = i == num_statements - 1;
            let is_expression = matches!(&statement, ast::Statement::Expression(_));

            result_type_id = self.compile_statement(statement)?;

            // Pop intermediate expression results, keeping only the last one
            if !is_last && is_expression {
                self.codegen.add_instruction(Instruction::Pop);
            }
        }
        let module_type = self
            .program
            .lookup_type(result_type_id)
            .cloned()
            .unwrap_or_else(Type::nil);

        // Get the compiled module instructions
        let module_instructions = std::mem::take(&mut self.codegen.instructions);

        // Restore original compiler state
        self.codegen.instructions = saved_instructions;
        self.scopes = saved_scopes;
        self.local_count = saved_local_count;

        // Register the callable type for this module wrapper function
        // (modules take no arguments and return the module value)
        let nil_type_id = self.program.register_type(Type::nil());
        let never_id = self.program.never();
        let callable_type_id = self.program.register_type(Type::Callable {
            parameter: nil_type_id,
            result: result_type_id,
            receive: never_id,
        });

        // Generate bytecode, then add the module function
        let mut bytecode = self.program.to_bytecode(None);
        bytecode.functions.push(quiver_core::bytecode::Function {
            instructions: module_instructions,
            captures: 0,
            type_id: callable_type_id,
        });
        bytecode.entry = Some(bytecode.functions.len() - 1);

        // Execute the module to get the result value
        let (module_value, executor) =
            // Modules are executed at compile time only to produce their value; they don't
            // receive messages, so skip the (expensive) parameter-compatibility tables.
            quiver_core::execute_bytecode_sync_with(bytecode, self.builtins, false, false).map_err(
                |e| Error::ModuleExecution {
                    module: module.join("/"),
                    error: e,
                },
            )?;

        // Extract binary data from the executor
        let mut binary_data = HashMap::new();
        modules::extract_binary_data(&module_value, &executor, &mut binary_data);

        let cached = modules::CachedModule {
            value: module_value,
            module_type,
            binary_data,
        };

        // Cache the module
        self.module_cache
            .cache_module(module.to_vec(), cached.clone());

        Ok(cached)
    }

    /// Convert a cached runtime value back to instructions that reconstruct it.
    /// Uses pre-extracted binary data instead of an executor.
    fn value_to_instructions_from_cache(
        &mut self,
        value: &Value,
        binary_data: &HashMap<usize, Vec<u8>>,
    ) -> Result<(Vec<Instruction>, usize), Error> {
        match value {
            Value::Integer(int_value) => {
                let index = self
                    .program
                    .register_constant(Constant::Integer(*int_value));
                Ok((
                    vec![Instruction::Constant(index)],
                    self.program.register_type(Type::Integer),
                ))
            }
            Value::Binary(binary) => match binary {
                Binary::Constant(const_idx) => {
                    // Just use the existing constant
                    Ok((
                        vec![Instruction::Constant(*const_idx)],
                        self.program.register_type(Type::Binary),
                    ))
                }
                Binary::Heap(heap_idx) => {
                    // Use pre-extracted binary data from cache
                    let bytes = binary_data
                        .get(heap_idx)
                        .ok_or_else(|| {
                            Error::FeatureUnsupported(format!(
                                "Missing cached binary data for heap index {}",
                                heap_idx
                            ))
                        })?
                        .clone();
                    let constant = Constant::Binary(bytes);
                    let index = self.program.register_constant(constant);
                    Ok((
                        vec![Instruction::Constant(index)],
                        self.program.register_type(Type::Binary),
                    ))
                }
            },
            Value::Tuple(tuple_id, fields) => {
                let mut instructions = Vec::new();
                for field in fields.iter() {
                    let (field_instructions, _) =
                        self.value_to_instructions_from_cache(field, binary_data)?;
                    instructions.extend(field_instructions);
                }
                instructions.push(Instruction::Tuple(*tuple_id));
                Ok((
                    instructions,
                    self.program.register_type(Type::Tuple(*tuple_id)),
                ))
            }
            Value::Function(function, captures) => {
                // Get the function's callable type directly from the function's type_id
                let callable_type_id = self
                    .program
                    .get_function(*function)
                    .ok_or(Error::FunctionUndefined(*function))?
                    .type_id;

                let mut instructions = Vec::new();

                // Push capture values to stack (will be popped by Function instruction)
                for capture_value in captures.iter() {
                    let (capture_instructions, _) =
                        self.value_to_instructions_from_cache(capture_value, binary_data)?;
                    instructions.extend(capture_instructions);
                }

                // Reuse the same function index - no re-registration needed!
                instructions.push(Instruction::Function(*function));

                Ok((instructions, callable_type_id))
            }
            Value::Builtin(builtin_id) => {
                // Get the builtin info to retrieve its type signature
                let builtin_info = self
                    .program
                    .get_builtins()
                    .get(*builtin_id)
                    .ok_or_else(|| Error::BuiltinUndefined(format!("builtin_id {}", builtin_id)))?;
                let param_type = builtin_info.param_type;
                let result_type = builtin_info.result_type;

                let never_id = self.program.never();
                let callable_type_id = self.program.register_type(Type::Callable {
                    parameter: param_type,
                    result: result_type,
                    receive: never_id,
                });

                Ok((vec![Instruction::Builtin(*builtin_id)], callable_type_id))
            }
            Value::Process(_, _) => Err(Error::FeatureUnsupported(
                "Cannot use process in constant context".to_string(),
            )),
            Value::Resource(..) => Err(Error::FeatureUnsupported(
                "Cannot use resource in constant context".to_string(),
            )),
            Value::Reference(_) => Err(Error::FeatureUnsupported(
                "Cannot use ref in constant context".to_string(),
            )),
        }
    }

    /// Resolve an accessor chain on a compile-time known value.
    /// Returns the resolved value and its type.
    fn resolve_accessors(
        &mut self,
        value: &Value,
        value_type: &usize,
        accessors: &[ast::AccessPath],
        module_name: &str,
    ) -> Result<(Value, usize), Error> {
        let mut current_value = value.clone();
        let mut current_type = *value_type;

        for accessor in accessors {
            let Value::Tuple(_, fields) = &current_value else {
                return Err(Error::MemberAccessOnNonTuple {
                    target: module_name.to_string(),
                });
            };

            let (index, field_type) = match accessor {
                ast::AccessPath::Field(name) => {
                    let (index, field_types) = type_queries::get_field_by_name(
                        &self.program,
                        current_type,
                        name,
                        module_name,
                    )?;
                    (
                        index,
                        typing::union_type_ids(&mut self.program, field_types),
                    )
                }
                ast::AccessPath::Index(index) => {
                    let field_types = type_queries::get_field_at_index(
                        &self.program,
                        current_type,
                        *index,
                        module_name,
                    )?;
                    (
                        *index,
                        typing::union_type_ids(&mut self.program, field_types),
                    )
                }
            };

            current_value =
                fields
                    .get(index)
                    .cloned()
                    .ok_or_else(|| Error::MemberAccessOnNonTuple {
                        target: module_name.to_string(),
                    })?;
            current_type = field_type;
        }

        Ok((current_value, current_type))
    }

    /// Validate that a receive function in a select has the correct return type
    fn validate_receive_function(
        &mut self,
        source: &ast::Chain,
        source_type_id: usize,
    ) -> Result<(), Error> {
        // Only validate if this is a callable (receive function)
        let Some(Type::Callable { result, .. }) = self.program.lookup_type(source_type_id) else {
            return Ok(());
        };
        let result_id = *result;

        // Identity functions (no body) and variable references are always allowed
        if !source
            .terms
            .iter()
            .any(|term| matches!(term, ast::Term::Function(func) if func.body.is_some()))
        {
            return Ok(());
        }

        // For functions with bodies, the result type must be nil, Ok, or a union of them
        let is_valid = self.is_nil_or_ok(result_id);

        if !is_valid {
            return Err(Error::TypeMismatch {
                expected: "receive function with body must return [] or Ok".to_string(),
                found: quiver_core::format::format_type_by_id(&self.program, result_id),
            });
        }

        Ok(())
    }

    /// Compile select sources and return their types
    fn compile_select_sources(
        &mut self,
        sources: &[ast::Chain],
        value_type: Option<&usize>,
    ) -> Result<Vec<usize>, Error> {
        let mut source_types = Vec::new();

        for (i, source) in sources.iter().enumerate() {
            // Pass ripple_context if there's a chained value
            // Chained value is at offset i (number of sources compiled so far)
            // Set owns_value=false since we'll clean it up manually after all sources
            let ripple_ctx;
            let ripple_param = if let Some(val_type) = value_type {
                ripple_ctx = RippleContext {
                    value_type_id: *val_type,
                    stack_offset: i,
                    owns_value: false,
                    provenance: Provenance::Unknown, // Spawn context doesn't need narrowing
                };
                Some(&ripple_ctx)
            } else {
                None
            };

            let source_type = self.compile_chain(source.clone(), None, ripple_param)?;

            // Validate receive functions
            self.validate_receive_function(source, source_type)?;

            source_types.push(source_type);
        }

        Ok(source_types)
    }

    /// Compile spawn operation: @f, f ~> @, or arg ~> @f
    fn compile_spawn(&mut self, term: ast::Term, arg_type: Option<usize>) -> Result<usize, Error> {
        // Case 1: f ~> @ (function is the piped value, spawn with nil)
        if term.is_bare_ripple() {
            let fn_type = arg_type.ok_or_else(|| {
                Error::FeatureUnsupported("Ripple spawn requires piped value".to_string())
            })?;
            return self.emit_nil_param_spawn(fn_type);
        }

        // For nil-parameter spawn sugar (@{ ... }), the inline function definition
        // ignores the incoming value - it's not passed as an argument.
        // But for typed spawn sugar (@int { ... }), the incoming value IS the argument.
        let is_nil_param_inline =
            matches!(&term, ast::Term::Function(f) if f.parameter_type.is_none());
        let effective_arg_type = if is_nil_param_inline {
            // Pop the incoming value if present (nil-parameter spawn ignores it)
            if arg_type.is_some() {
                self.codegen.add_instruction(Instruction::Pop);
            }
            None
        } else {
            arg_type
        };

        // Compile the function term
        let (fn_type, _prov) =
            self.compile_term(term, None, Provenance::Unknown, None, None, None)?;

        if let Some(arg_type) = effective_arg_type {
            // Case 2: arg ~> @f (spawn with argument)
            self.emit_arg_spawn(fn_type, arg_type)
        } else {
            // Case 3: @f (spawn with nil) or @{ ... } (spawn sugar)
            self.emit_nil_param_spawn(fn_type)
        }
    }

    /// Emit spawn for nil-parameter function (cases 1 and 3)
    /// Stack before: [function]
    fn emit_nil_param_spawn(&mut self, fn_type_id: usize) -> Result<usize, Error> {
        let Some(Type::Callable {
            parameter,
            result,
            receive,
        }) = self.program.lookup_type(fn_type_id)
        else {
            return Err(Error::FeatureUnsupported(
                "Can only spawn functions".to_string(),
            ));
        };
        let (parameter, result, receive) = (*parameter, *result, *receive);

        let nil_type_id = self.program.register_type(Type::nil());
        if parameter != nil_type_id {
            return Err(Error::TypeMismatch {
                expected: "function with nil parameter".to_string(),
                found: format!(
                    "function with parameter {}",
                    quiver_core::format::format_type_by_id(&self.program, parameter)
                ),
            });
        }

        // Stack: [function] -> [nil, function] -> spawn
        self.codegen.add_instruction(Instruction::Tuple(NIL));
        self.codegen.add_instruction(Instruction::Rotate(2));
        self.codegen.add_instruction(Instruction::Spawn);

        Ok(self.program.register_type(Type::Process {
            send: Some(receive),
            receive: Some(result),
        }))
    }

    /// Emit spawn with argument (case 2)
    /// Stack before: [argument, function]
    fn emit_arg_spawn(&mut self, fn_type_id: usize, arg_type: usize) -> Result<usize, Error> {
        let Some(Type::Callable {
            parameter,
            result,
            receive,
        }) = self.program.lookup_type(fn_type_id)
        else {
            return Err(Error::FeatureUnsupported(
                "Can only spawn functions".to_string(),
            ));
        };
        let (parameter, result, receive) = (*parameter, *result, *receive);

        if !quiver_core::types::is_compatible(arg_type, parameter, &self.program) {
            return Err(Error::TypeMismatch {
                expected: quiver_core::format::format_type_by_id(&self.program, parameter),
                found: quiver_core::format::format_type_by_id(&self.program, arg_type),
            });
        }

        self.codegen.add_instruction(Instruction::Spawn);

        Ok(self.program.register_type(Type::Process {
            send: Some(receive),
            receive: Some(result),
        }))
    }

    /// Compile an argument tuple, handling ripple context conversion
    /// Returns None if no argument provided
    fn compile_argument(
        &mut self,
        argument: Option<&Vec<ast::TupleField>>,
        value_type: Option<&usize>,
        value_provenance: Provenance,
        ripple_context: Option<&RippleContext>,
        _context_name: &str,
    ) -> Result<Option<usize>, Error> {
        let Some(fields) = argument else {
            return Ok(None);
        };

        // Always flow the piped value into the argument tuple's fields (each field gets a
        // copy as input; callable fields are called with it, non-callables drop it). The
        // original is owned and cleaned up by compile_tuple.
        let ripple_context_value;
        let ripple_ctx = if let Some(vt) = value_type {
            ripple_context_value = RippleContext {
                value_type_id: *vt,
                stack_offset: 0,
                owns_value: true,
                provenance: value_provenance,
            };
            Some(&ripple_context_value)
        } else {
            ripple_context
        };

        let (ty, _prov) = self.compile_tuple(None, fields.clone(), ripple_ctx)?;
        Ok(Some(ty))
    }

    /// Compile access expression: .x, $.x, foo.x, etc.
    fn compile_access(
        &mut self,
        access: ast::Access,
        value_type: Option<usize>,
        value_provenance: Provenance,
        ripple_context: Option<&RippleContext>,
    ) -> Result<(usize, Provenance), Error> {
        match access.source {
            None => {
                // Field/positional access (.x, .0) requires a value
                let val_type = value_type.ok_or_else(|| {
                    Error::FeatureUnsupported(
                        "Field/positional access requires a value".to_string(),
                    )
                })?;

                // Track field provenance as we access fields
                let (accessed_type, accessed_prov) =
                    self.compile_accessor(val_type, access.accessors, "value", value_provenance)?;

                if let Some(arg_fields) = &access.argument {
                    // Compile argument - no ripples allowed for bare accessor
                    let (arg_type, _) = self.compile_tuple(None, arg_fields.clone(), None)?;
                    self.codegen.add_instruction(Instruction::Rotate(2));
                    // Function call result has unknown provenance
                    let ty = self.apply_value_to_type(accessed_type, arg_type)?;
                    Ok((ty, Provenance::Unknown))
                } else {
                    Ok((accessed_type, accessed_prov))
                }
            }
            Some(ast::AccessSource::Parameter) => {
                // $ accesses the function parameter
                let (param_type, param_local) = scopes::get_function_parameter(&self.scopes)?;

                // Check if there's an explicit argument
                let has_argument = access.argument.is_some();

                // Peek at the accessed type to determine if callable (without emitting code)
                let peeked_type = type_queries::resolve_accessor_type(
                    &mut self.program,
                    param_type,
                    &access.accessors,
                    "$",
                );
                let is_callable = peeked_type.is_ok_and(|ty| {
                    matches!(
                        self.program.lookup_type(ty),
                        Some(Type::Callable { .. }) | Some(Type::Process { .. })
                    )
                });

                // If not callable and no explicit argument, drop incoming value first
                if !is_callable && !has_argument && value_type.is_some() {
                    self.codegen.add_instruction(Instruction::Pop);
                }

                // Now load and compile
                self.codegen.add_instruction(Instruction::Load(param_local));

                // Parameter access has Parameter provenance
                let (accessed_type, accessed_prov) = self.compile_accessor(
                    param_type,
                    access.accessors,
                    "$",
                    Provenance::Parameter,
                )?;
                let arg_type = self.compile_argument(
                    access.argument.as_ref(),
                    value_type.as_ref(),
                    value_provenance.clone(),
                    ripple_context,
                    "$",
                )?;

                if let Some(arg_type) = arg_type {
                    // Explicit argument → call
                    let ty = self.apply_value_to_type(accessed_type, arg_type)?;
                    Ok((ty, Provenance::Unknown))
                } else if let (true, Some(val_type)) = (is_callable, value_type) {
                    // Callable with implicit input → call
                    let ty = self.apply_value_to_type(accessed_type, val_type)?;
                    Ok((ty, Provenance::Unknown))
                } else {
                    // Non-callable → already popped incoming value, just return reference
                    Ok((accessed_type, accessed_prov))
                }
            }
            Some(ast::AccessSource::Identifier(name)) => {
                // Check if this is a tuple variable being spread (a[..., y] syntax)
                let has_spread = access
                    .argument
                    .as_ref()
                    .is_some_and(|args| helpers::tuple_contains_spread(args));

                if has_spread
                    && access.accessors.is_empty()
                    && let Some((var_type_id, _var_index)) =
                        scopes::lookup_variable(&self.scopes, &name, &[])
                    && let Some(Type::Tuple(tuple_id)) = self.program.lookup_type(var_type_id)
                {
                    let tuple_id = *tuple_id;
                    // This is tuple spread: a[..., y] where a is a tuple
                    // Get the tuple name for inheritance
                    let tuple_name = self
                        .program
                        .lookup_tuple(tuple_id)
                        .and_then(|t| t.name.clone());

                    // Transform bare spreads (...) into named spreads (...a)
                    // so they load from the prefix variable
                    let mut arg_fields = access.argument.unwrap();
                    for field in &mut arg_fields {
                        if matches!(&field.value, ast::FieldValue::Spread(None)) {
                            field.value = ast::FieldValue::Spread(Some(name.clone()));
                        }
                    }

                    // Determine ripple context for field values:
                    // - If there's a chained value, ripples (~) should resolve to it
                    // - Otherwise use outer ripple_context if available
                    let chained_ripple_ctx;
                    let effective_ripple_ctx = if let Some(val_type) = value_type {
                        // Value is chained directly to this spread - create context for it
                        chained_ripple_ctx = RippleContext {
                            value_type_id: val_type,
                            stack_offset: 0,
                            owns_value: true,
                            provenance: value_provenance.clone(),
                        };
                        Some(&chained_ripple_ctx)
                    } else {
                        ripple_context
                    };

                    // Compile tuple with spread
                    let (ty, _) = spread::compile_tuple_with_spread(
                        self,
                        tuple_name,
                        arg_fields,
                        effective_ripple_ctx,
                    )?;
                    // Spread result has unknown provenance
                    return Ok((ty, Provenance::Unknown));
                }

                // Normal function call path
                // Compile argument first so ripples can access the piped value
                let arg_type = self.compile_argument(
                    access.argument.as_ref(),
                    value_type.as_ref(),
                    value_provenance.clone(),
                    ripple_context,
                    &name,
                )?;

                // For non-callable lookups, we need to know before loading whether to pop
                // the incoming value. Peek at the type first.
                // First try the full path as a variable (for captures), then resolve through type system.
                let peeked_type = scopes::lookup_variable(&self.scopes, &name, &access.accessors)
                    .map(|(ty, _)| ty)
                    .or_else(|| {
                        // Full path not found - resolve base variable + accessors through type system
                        let (base_type, _) = scopes::lookup_variable(&self.scopes, &name, &[])?;
                        type_queries::resolve_accessor_type(
                            &mut self.program,
                            base_type,
                            &access.accessors,
                            &name,
                        )
                        .ok()
                    });

                // Check if the final accessed type will be callable
                let is_callable = peeked_type.is_some_and(|ty| {
                    matches!(
                        self.program.lookup_type(ty),
                        Some(Type::Callable { .. }) | Some(Type::Process { .. })
                    )
                });

                // If not callable and no explicit argument, drop incoming value first
                if !is_callable && arg_type.is_none() && value_type.is_some() {
                    self.codegen.add_instruction(Instruction::Pop);
                }

                // compile_member_access loads the variable and handles accessors
                let (accessed_type, accessed_prov) =
                    self.compile_member_access(&name, access.accessors)?;

                if let Some(arg_type) = arg_type {
                    // Explicit argument → always try to call
                    let ty = self.apply_value_to_type(accessed_type, arg_type)?;
                    Ok((ty, Provenance::Unknown))
                } else if let (true, Some(val_type)) = (is_callable, value_type) {
                    // Callable with implicit input → call
                    let ty = self.apply_value_to_type(accessed_type, val_type)?;
                    Ok((ty, Provenance::Unknown))
                } else {
                    // Non-callable → already popped incoming value, just return reference
                    Ok((accessed_type, accessed_prov))
                }
            }
            Some(ast::AccessSource::Ripple) => {
                // Bare ~ - ripple placeholder that evaluates to the chained value
                if access.accessors.is_empty() && access.argument.is_none() {
                    // Prefer value_type (directly chained) over ripple_context (inherited from parent)
                    if let Some(val_type) = value_type {
                        // Value was directly chained to this ripple - use it
                        // The value is already on the stack, no need to Pick
                        return Ok((val_type, value_provenance));
                    } else if let Some(ctx) = ripple_context {
                        // Inherit ripple context from parent tuple
                        self.codegen
                            .add_instruction(Instruction::Pick(ctx.stack_offset));
                        return Ok((ctx.value_type_id, ctx.provenance.clone()));
                    } else {
                        return Err(Error::FeatureUnsupported(
                            "Ripple placeholder (~) can only be used when a value is being chained"
                                .to_string(),
                        ));
                    }
                }

                // ~[args] or ~.field[args] - use piped value as source
                let piped_type = value_type.ok_or_else(|| {
                    Error::FeatureUnsupported(
                        "Ripple access (~[...]) requires a piped value".to_string(),
                    )
                })?;

                let has_spread = access
                    .argument
                    .as_ref()
                    .is_some_and(|args| helpers::tuple_contains_spread(args));

                if access.accessors.is_empty() && has_spread {
                    // ~[..., y] - tuple spread from piped value (must be tuple)
                    if let Some(Type::Tuple(tuple_id)) = self.program.lookup_type(piped_type) {
                        let tuple_id = *tuple_id;
                        // Get tuple name for inheritance
                        let tuple_name = self
                            .program
                            .lookup_tuple(tuple_id)
                            .and_then(|t| t.name.clone());

                        // Piped value is already on stack, create ripple context
                        let ripple_ctx = RippleContext {
                            value_type_id: piped_type,
                            stack_offset: 0,
                            owns_value: true,
                            provenance: value_provenance.clone(),
                        };

                        let arg_fields = access.argument.unwrap();
                        let (ty, _) = spread::compile_tuple_with_spread(
                            self,
                            tuple_name,
                            arg_fields,
                            Some(&ripple_ctx),
                        )?;
                        return Ok((ty, Provenance::Unknown));
                    } else {
                        return Err(Error::TypeMismatch {
                            expected: "tuple".to_string(),
                            found: quiver_core::format::format_type_by_id(
                                &self.program,
                                piped_type,
                            ),
                        });
                    }
                }

                if access.accessors.is_empty() {
                    // ~[args] without spread - call piped value (must be callable)
                    // argument must be Some here since bare ~ was handled above
                    let arg_fields = access.argument.unwrap();

                    // Compile argument tuple (no ripple context needed, piped value IS the function)
                    let (arg_type, _) = self.compile_tuple(None, arg_fields, None)?;

                    // Rotate so piped value (function) is on top
                    self.codegen.add_instruction(Instruction::Rotate(2));

                    let ty = self.apply_value_to_type(piped_type, arg_type)?;
                    Ok((ty, Provenance::Unknown))
                } else {
                    // ~.field or ~.field[args] - access field on piped value
                    let (accessed_type, accessed_prov) =
                        self.compile_accessor(piped_type, access.accessors, "~", value_provenance)?;

                    if let Some(arg_fields) = access.argument {
                        // ~.field[args] - apply args to accessed value
                        let (arg_type, _) = self.compile_tuple(None, arg_fields, None)?;
                        self.codegen.add_instruction(Instruction::Rotate(2));
                        let ty = self.apply_value_to_type(accessed_type, arg_type)?;
                        Ok((ty, Provenance::Unknown))
                    } else {
                        Ok((accessed_type, accessed_prov))
                    }
                }
            }
            Some(ast::AccessSource::Import(module)) => {
                // %module or %module/submodule with optional .field accessors and [args]
                let module_name = format!("%{}", module.join("/"));

                // Compile argument first so ripples can access the piped value
                let arg_type = self.compile_argument(
                    access.argument.as_ref(),
                    value_type.as_ref(),
                    value_provenance.clone(),
                    ripple_context,
                    &module_name,
                )?;

                // Resolve import type first (no code emission yet) to check if callable
                let (cached, resolved_value, accessed_type) =
                    self.resolve_import(&module, &access.accessors)?;

                let is_callable = matches!(
                    self.program.lookup_type(accessed_type),
                    Some(Type::Callable { .. }) | Some(Type::Process { .. })
                );

                // If not callable and no explicit argument, drop incoming value BEFORE loading
                if !is_callable && arg_type.is_none() && value_type.is_some() {
                    self.codegen.add_instruction(Instruction::Pop);
                }

                // Now emit instructions for the resolved import value
                let (instructions, _) =
                    self.value_to_instructions_from_cache(&resolved_value, &cached.binary_data)?;
                for instruction in instructions {
                    self.codegen.add_instruction(instruction);
                }

                if let Some(arg_type) = arg_type {
                    // Explicit argument → always try to call
                    let ty = self.apply_value_to_type(accessed_type, arg_type)?;
                    Ok((ty, Provenance::Unknown))
                } else if let (true, Some(val_type)) = (is_callable, value_type) {
                    // Callable with implicit input → call
                    let ty = self.apply_value_to_type(accessed_type, val_type)?;
                    Ok((ty, Provenance::Unknown))
                } else {
                    // Non-callable → already popped incoming value, just return reference
                    Ok((accessed_type, Provenance::Unknown))
                }
            }
            Some(ast::AccessSource::Self_) => {
                // Self_ should only appear in Term::Reference, not Term::Access
                Err(Error::InternalError {
                    message: "Self_ source in Access (should use Term::Self_ or Term::Reference)"
                        .to_string(),
                })
            }
        }
    }

    /// Compile select expression: ![sources...] or postfix form (empty sources with piped value)
    ///
    /// Select expects either a single value or a tuple on the stack:
    /// - Single value (process, function, or int): used as the sole source
    /// - Tuple: each element is used as a source
    fn compile_select(
        &mut self,
        sources: Option<Vec<ast::Chain>>,
        value_type: Option<usize>,
    ) -> Result<usize, Error> {
        // Handle the different select forms:
        // - None (bare `!`): postfix form, use chained value as source
        // - Some(vec![]): explicit empty `![]`, discard chained value, return nil
        // - Some(sources): explicit sources, discard chained value
        let sources = match sources {
            None => {
                // Bare `!` - postfix form using chained value
                let val_type = value_type.ok_or_else(|| {
                    Error::FeatureUnsupported("Bare ! requires a piped value".to_string())
                })?;
                // Value is already on stack
                self.codegen.add_instruction(Instruction::Select);
                return self.compute_select_return_type(&[val_type]);
            }
            Some(sources) if sources.is_empty() => {
                // Explicit `![]` - discard chained value, return nil
                if value_type.is_some() {
                    self.codegen.add_instruction(Instruction::Pop);
                }
                self.codegen.add_instruction(Instruction::Tuple(NIL));
                return Ok(self.program.register_type(Type::nil()));
            }
            Some(sources) => sources,
        };

        // Compile all sources (explicit sources discard chained value)
        let source_types = self.compile_select_sources(&sources, value_type.as_ref())?;

        // If there was a chained value, remove it from the bottom of the stack
        if value_type.is_some() {
            self.codegen.emit_rotate_pop(sources.len() + 1);
        }

        // For multiple sources, create a tuple; for single source, leave as-is
        if sources.len() > 1 {
            let fields: Vec<(Option<String>, usize)> =
                source_types.iter().map(|&t| (None, t)).collect();
            let tuple_id = self.program.register_tuple(None, fields);
            self.codegen.add_instruction(Instruction::Tuple(tuple_id));
        }

        // Emit Select instruction (handles both single value and tuple)
        self.codegen.add_instruction(Instruction::Select);

        self.compute_select_return_type(&source_types)
    }

    /// Compute the return type of a select operation from source types
    fn compute_select_return_type(&mut self, source_types: &[usize]) -> Result<usize, Error> {
        let mut result_types = Vec::new();

        for &source_type_id in source_types {
            let source_type =
                self.program
                    .lookup_type(source_type_id)
                    .ok_or_else(|| Error::InternalError {
                        message: format!("Type ID {} not found", source_type_id),
                    })?;

            match source_type {
                Type::Process {
                    receive: Some(recv_type),
                    ..
                } => {
                    // Process (process or resource) - get its receive type
                    result_types.push(*recv_type);
                }
                Type::Process { receive: None, .. } => {
                    return Err(Error::TypeMismatch {
                        expected: "process with receive type (awaitable/readable)".to_string(),
                        found: "process without receive type (cannot select)".to_string(),
                    });
                }
                Type::Callable { parameter, .. } => {
                    // Receive function - use its parameter type (the message type being received)
                    result_types.push(*parameter);
                }
                Type::Integer => {
                    // Timeout source
                    result_types.push(self.program.register_type(Type::nil()));
                }
                _ => {
                    return Err(Error::TypeMismatch {
                        expected: "process, function, resource, or integer (timeout)".to_string(),
                        found: quiver_core::format::format_type_by_id(
                            &self.program,
                            source_type_id,
                        ),
                    });
                }
            }
        }

        if result_types.is_empty() {
            return Err(Error::FeatureUnsupported(
                "Select requires at least one source".to_string(),
            ));
        }

        Ok(typing::union_type_ids(&mut self.program, result_types))
    }

    fn compile_term(
        &mut self,
        term: ast::Term,
        value_type: Option<usize>,
        value_provenance: Provenance,
        on_no_match: Option<usize>,
        ripple_context: Option<&RippleContext>,
        mut narrowing: Option<&mut Narrowing>,
    ) -> Result<(usize, Provenance), Error> {
        match term {
            ast::Term::Literal(literal) => {
                // Literals don't use the piped value, drop it
                if value_type.is_some() {
                    self.codegen.add_instruction(Instruction::Pop);
                }
                let ty = self.compile_literal(literal)?;
                Ok((ty, Provenance::Unknown))
            }
            ast::Term::Tuple(tuple) => {
                // Always flow the piped value into the tuple's fields: each field receives a
                // copy as its input, so a callable field is called with it (and `&` is needed
                // to pass a callable by value), while non-callable fields drop it. The original
                // is owned and cleaned up by compile_tuple.
                let ripple_context_value;
                let ripple_context_param = if let Some(vt) = value_type.as_ref() {
                    ripple_context_value = RippleContext {
                        value_type_id: *vt,
                        stack_offset: 0,
                        owns_value: true,
                        provenance: value_provenance,
                    };
                    Some(&ripple_context_value)
                } else {
                    ripple_context
                };

                let (ty, tuple_prov) =
                    self.compile_tuple(tuple.name, tuple.fields, ripple_context_param)?;
                // Return tuple provenance for field access tracking
                Ok((ty, tuple_prov))
            }
            ast::Term::Block(block) => {
                // Blocks can fail for non-type reasons (pattern matches, literal comparisons),
                // so disable complement narrowing
                if let Some(n) = narrowing.as_deref_mut() {
                    n.disable();
                }

                // Blocks take their input as a parameter
                let block_parameter =
                    value_type.unwrap_or_else(|| self.program.register_type(Type::nil()));
                // Pass the provenance from the chain to the block
                let block_provenance = value_provenance.clone();
                if value_type.is_none() {
                    // Blocks without a value need NIL on stack
                    self.codegen.add_instruction(Instruction::Tuple(NIL));
                }
                let ty = self.compile_block(
                    block,
                    block_parameter,
                    block_provenance,
                    None,
                    ScopeKind::Block,
                )?;
                // Block results have unknown provenance
                Ok((ty, Provenance::Unknown))
            }
            ast::Term::Function(func) => {
                // Function literals always produce functions - they don't auto-call.
                // To call an inline function, bind it first: f = #int {...}, 5 ~> f
                if value_type.is_some() {
                    self.codegen.add_instruction(Instruction::Pop);
                }

                let function_type = self.compile_function(func)?;
                Ok((function_type, Provenance::Unknown))
            }
            ast::Term::Access(access) => {
                self.compile_access(access, value_type, value_provenance.clone(), ripple_context)
            }
            ast::Term::Builtin(builtin) => {
                // Compile argument first so ripples can access the piped value
                let arg_type = self.compile_argument(
                    builtin.argument.as_ref(),
                    value_type.as_ref(),
                    value_provenance,
                    ripple_context,
                    "__builtin__",
                )?;

                let builtin_type = self.compile_builtin(&builtin.name)?;

                // Builtin calls can fail for non-type reasons, disable complement narrowing
                if let Some(n) = narrowing.as_deref_mut() {
                    n.disable();
                }

                let result_type = if let Some(arg_type) = arg_type {
                    self.apply_value_to_type(builtin_type, arg_type)?
                } else if let Some(val_type) = value_type {
                    self.apply_value_to_type(builtin_type, val_type)?
                } else {
                    builtin_type
                };
                // Builtin results have unknown provenance
                Ok((result_type, Provenance::Unknown))
            }
            ast::Term::TailCall(tail_call) => {
                // Tail calls can fail for non-type reasons, disable complement narrowing
                if let Some(n) = narrowing.as_deref_mut() {
                    n.disable();
                }

                // Compile argument if present (may contain ripples using value_type)
                // TailCall doesn't check for ignored values - argument or piped value is used
                let arg_type = if let Some(arg_fields) = &tail_call.argument {
                    let ripple_context_value;
                    let ripple_ctx = if let Some(vt) = value_type.as_ref() {
                        ripple_context_value = RippleContext {
                            value_type_id: *vt,
                            stack_offset: 0,
                            owns_value: true,
                            provenance: value_provenance.clone(),
                        };
                        Some(&ripple_context_value)
                    } else {
                        ripple_context
                    };
                    let (ty, _) = self.compile_tuple(None, arg_fields.clone(), ripple_ctx)?;
                    Some(ty)
                } else {
                    value_type
                };

                let ty = self.compile_tail_call(
                    tail_call.identifier.as_deref(),
                    &tail_call.accessors,
                    arg_type,
                )?;
                // Tail call results have unknown provenance
                Ok((ty, Provenance::Unknown))
            }
            ast::Term::Equality => {
                let val_type = value_type.ok_or_else(|| {
                    Error::FeatureUnsupported("Equality operator requires a value".to_string())
                })?;
                let ty = self.compile_equality(val_type)?;
                Ok((ty, Provenance::Unknown))
            }
            ast::Term::Not => {
                let val_type = value_type.ok_or_else(|| {
                    Error::FeatureUnsupported("Not operator requires a value".to_string())
                })?;
                // The not operator narrows the type to [] (nil) when it succeeds
                // Record this narrowing so subsequent branches can use the complement
                if let Some(n) = narrowing.as_deref_mut() {
                    let nil_type_id = self.program.register_type(Type::nil());
                    n.record(&value_provenance, val_type, nil_type_id, &mut self.program);
                }
                let ty = self.compile_not(val_type)?;
                Ok((ty, Provenance::Unknown))
            }
            ast::Term::Match(pattern) => {
                // Match patterns can create new bindings or check against existing values/types
                let val_type = value_type.ok_or_else(|| {
                    Error::FeatureUnsupported("Match requires a value".to_string())
                })?;
                let ty = self.compile_match(
                    pattern,
                    val_type,
                    value_provenance.clone(),
                    on_no_match,
                    false,
                    narrowing,
                )?;
                // Match result preserves provenance of matched value
                Ok((ty, value_provenance))
            }
            ast::Term::Spawn(term) => {
                let ty = self.compile_spawn(*term, value_type)?;
                Ok((ty, Provenance::Unknown))
            }
            ast::Term::Select(select) => {
                let ty = self.compile_select(select, value_type)?;
                Ok((ty, Provenance::Unknown))
            }
            ast::Term::Self_ => {
                self.codegen.add_instruction(Instruction::Self_);
                // Return a process type with the current function's receive type
                // Return type is None since a process can't know its own return type
                let self_type = self.program.register_type(Type::Process {
                    send: Some(self.current_receive_type_id),
                    receive: None,
                });

                // Apply value if present (for message sends like `10 ~> .`)
                let result_type = if let Some(val_type) = value_type {
                    self.apply_value_to_type(self_type, val_type)?
                } else {
                    self_type
                };
                Ok((result_type, Provenance::Unknown))
            }
            ast::Term::Process(process_id) => {
                // Look up process info from the map (REPL-only feature)
                let (process_type, function_index) = self
                    .process_types
                    .get(&process_id)
                    .cloned()
                    .ok_or_else(|| Error::InternalError {
                    message: format!("Process {} not found", process_id),
                })?;

                // Generate Process instruction
                self.codegen
                    .add_instruction(Instruction::Process(process_id, function_index));

                // Apply value if present (for message sends like `10 ~> @1`)
                let result_type = if let Some(val_type) = value_type {
                    self.apply_value_to_type(process_type, val_type)?
                } else {
                    process_type
                };
                Ok((result_type, Provenance::Unknown))
            }
            ast::Term::Reference(Some(access)) => {
                // Explicit reference: drop incoming value and load the referenced value without calling
                if value_type.is_some() {
                    self.codegen.add_instruction(Instruction::Pop);
                }

                // Load the referenced value
                match access.source {
                    Some(ast::AccessSource::Identifier(ref name)) => {
                        let (accessed_type, accessed_prov) =
                            self.compile_member_access(name, access.accessors)?;
                        Ok((accessed_type, accessed_prov))
                    }
                    Some(ast::AccessSource::Parameter) => {
                        // &$ - reference to function parameter
                        let (param_type, param_local) =
                            scopes::get_function_parameter(&self.scopes)?;
                        self.codegen.add_instruction(Instruction::Load(param_local));
                        let (accessed_type, accessed_prov) = self.compile_accessor(
                            param_type,
                            access.accessors,
                            "$",
                            Provenance::Parameter,
                        )?;
                        Ok((accessed_type, accessed_prov))
                    }
                    Some(ast::AccessSource::Import(ref module)) => {
                        let ty = self.compile_import(module, &access.accessors)?;
                        Ok((ty, Provenance::Unknown))
                    }
                    Some(ast::AccessSource::Self_) => {
                        // &. - reference to self (current process)
                        self.codegen.add_instruction(Instruction::Self_);
                        let self_type = self.program.register_type(Type::Process {
                            send: Some(self.current_receive_type_id),
                            receive: None,
                        });
                        Ok((self_type, Provenance::Unknown))
                    }
                    Some(ast::AccessSource::Ripple) => Err(Error::FeatureUnsupported(
                        "Cannot reference ripple (~) - use it directly".to_string(),
                    )),
                    None => Err(Error::FeatureUnsupported(
                        "Reference requires an identifier (e.g., &f)".to_string(),
                    )),
                }
            }
            ast::Term::BuiltinReference(builtin) => {
                // &__builtin__ - the builtin function value, without applying it.
                if value_type.is_some() {
                    self.codegen.add_instruction(Instruction::Pop);
                }
                let builtin_type = self.compile_builtin(&builtin.name)?;
                Ok((builtin_type, Provenance::Unknown))
            }
            ast::Term::Apply(head, arg) => {
                // `f x` - compile the argument as a flow position (it receives the piped
                // value, so a callable argument is invoked with it; use `&` to pass one by
                // value), then feed its result into the head, which invokes the callable.
                let (arg_type, arg_prov) = self.compile_term(
                    *arg,
                    value_type,
                    value_provenance,
                    on_no_match,
                    ripple_context,
                    None,
                )?;
                self.compile_term(
                    *head,
                    Some(arg_type),
                    arg_prov,
                    on_no_match,
                    None,
                    narrowing,
                )
            }
            ast::Term::Reference(None) => {
                // Standalone & - create a new unique ref
                // Drop incoming value if present
                if value_type.is_some() {
                    self.codegen.add_instruction(Instruction::Pop);
                }
                self.codegen.add_instruction(Instruction::Reference);
                let ref_type = self.program.register_type(Type::Reference);
                Ok((ref_type, Provenance::Unknown))
            }
        }
    }

    /// Resolve cycles in a function's result type after a call
    ///
    /// When a function is called, its result may contain Cycle(n) references.
    /// A Cycle(n) means "go up n boundaries from here".
    ///
    /// This function traverses the result type, tracking depth (boundaries crossed).
    /// When it encounters Cycle(n) at depth D from the function:
    /// - To reach the function boundary from depth D requires going up (D+1) boundaries
    /// - If n == D+1, the cycle points to the function itself → resolve it
    /// - Otherwise, keep the cycle as-is
    fn resolve_function_cycles(
        &mut self,
        type_id: usize,
        function_type_id: usize,
        depth_from_function: usize,
    ) -> usize {
        let typ = match self.program.lookup_type(type_id) {
            Some(t) => t,
            None => return type_id,
        };

        match typ {
            Type::Cycle(n) => {
                // Cycle(n) means "n boundaries upward from here"
                // We're at depth_from_function boundaries inside the result
                // To reach the function: need to go up (depth_from_function + 1) boundaries
                //   - depth_from_function to exit the result's boundaries
                //   - +1 to exit the function boundary itself
                if *n == depth_from_function + 1 {
                    function_type_id
                } else {
                    // Cycle points elsewhere (could be to a boundary inside the result,
                    // or to something even further out)
                    type_id
                }
            }
            Type::Union(variants) => {
                // Union is a boundary - increment depth
                let variants_clone = variants.clone();
                let mut resolved_variants = Vec::new();
                for v in variants_clone {
                    let resolved =
                        self.resolve_function_cycles(v, function_type_id, depth_from_function + 1);
                    resolved_variants.push(resolved);
                }
                typing::union_type_ids(&mut self.program, resolved_variants)
            }
            Type::Callable {
                parameter,
                result,
                receive,
            } => {
                // Nested function is a boundary - increment depth
                let param_id = *parameter;
                let result_id = *result;
                let receive_id = *receive;
                let resolved_param = self.resolve_function_cycles(
                    param_id,
                    function_type_id,
                    depth_from_function + 1,
                );
                let resolved_result = self.resolve_function_cycles(
                    result_id,
                    function_type_id,
                    depth_from_function + 1,
                );
                let resolved_receive = self.resolve_function_cycles(
                    receive_id,
                    function_type_id,
                    depth_from_function + 1,
                );
                self.program.register_type(Type::Callable {
                    parameter: resolved_param,
                    result: resolved_result,
                    receive: resolved_receive,
                })
            }
            Type::Tuple(tuple_id) => {
                // Tuples are not boundaries - maintain depth
                if let Some(type_info) = self.program.lookup_tuple(*tuple_id).cloned() {
                    let new_fields: Vec<_> = type_info
                        .fields
                        .into_iter()
                        .map(|(name, field_type_id)| {
                            let resolved_field_type = self.resolve_function_cycles(
                                field_type_id,
                                function_type_id,
                                depth_from_function,
                            );
                            (name, resolved_field_type)
                        })
                        .collect();
                    let new_tuple_id = self.program.register_tuple(type_info.name, new_fields);
                    self.program.register_type(Type::Tuple(new_tuple_id))
                } else {
                    type_id
                }
            }
            Type::Partial { name, fields } => {
                // Partials are not boundaries - maintain depth
                // Clone upfront to avoid borrow issues with self.resolve_function_cycles
                let partial_name = name.clone();
                let partial_fields = fields.clone();
                let new_fields: Vec<_> = partial_fields
                    .into_iter()
                    .map(|(fname, field_type_id)| {
                        let resolved_field_type = self.resolve_function_cycles(
                            field_type_id,
                            function_type_id,
                            depth_from_function,
                        );
                        (fname, resolved_field_type)
                    })
                    .collect();
                self.program.register_type(Type::Partial {
                    name: partial_name,
                    fields: new_fields,
                })
            }
            Type::Process { send, receive } => {
                // Process types don't create boundaries but may contain types with cycles
                let send_id = *send;
                let receive_id = *receive;
                let resolved_send = send_id.map(|t| {
                    self.resolve_function_cycles(t, function_type_id, depth_from_function)
                });
                let resolved_receive = receive_id.map(|t| {
                    self.resolve_function_cycles(t, function_type_id, depth_from_function)
                });
                self.program.register_type(Type::Process {
                    send: resolved_send,
                    receive: resolved_receive,
                })
            }
            _ => type_id, // Integer, Binary, Variable, Resource don't contain nested types
        }
    }

    fn apply_value_to_type(
        &mut self,
        target_type_id: usize,
        value_type: usize,
    ) -> Result<usize, Error> {
        let target_type =
            self.program
                .lookup_type(target_type_id)
                .ok_or_else(|| Error::InternalError {
                    message: format!("Type ID {} not found", target_type_id),
                })?;

        if let Type::Callable {
            parameter,
            result,
            receive,
        } = target_type
        {
            // Function call
            let param_id = *parameter;
            let result_id = *result;
            let receive_id = *receive;

            // Check if function has type variables - if so, perform unification
            let has_vars_param = typing::contains_variables(param_id, &self.program);
            let has_vars_result = typing::contains_variables(result_id, &self.program);

            let result_type = if has_vars_param || has_vars_result {
                // Perform unification to bind type variables
                let mut bindings = HashMap::new();

                typing::unify(&mut bindings, param_id, value_type, &mut self.program)?;

                // Substitute bindings in the result type
                typing::substitute(result_id, &bindings, &mut self.program)
            } else {
                // No type variables - just check compatibility
                if !quiver_core::types::is_compatible(value_type, param_id, &self.program) {
                    return Err(Error::TypeMismatch {
                        expected: format!(
                            "function parameter compatible with {}",
                            quiver_core::format::format_type_by_id(&self.program, param_id)
                        ),
                        found: quiver_core::format::format_type_by_id(&self.program, value_type),
                    });
                }
                result_id
            };

            // Resolve cycles in result type that refer to the function itself
            // Start at depth 0 since we haven't descended into any boundaries yet
            let result_type = self.resolve_function_cycles(result_type, target_type_id, 0);

            // Check receive type compatibility
            let called_receive_type = receive_id;

            // Check if called function has receives (not NEVER)
            if !self.is_never(called_receive_type) {
                // Called function has receives - widen current context's receive type
                if self.is_never(self.current_receive_type_id) {
                    // Current context has no receive type yet - adopt the called function's receive type
                    self.current_receive_type_id = called_receive_type;
                } else if !quiver_core::types::is_compatible(
                    called_receive_type,
                    self.current_receive_type_id,
                    &self.program,
                ) {
                    // Current context has a receive type but it's incompatible - widen to union
                    self.current_receive_type_id = self.unify_receive_types(vec![
                        self.current_receive_type_id,
                        called_receive_type,
                    ]);
                }
                // If compatible, no change needed - current type already includes called type
            }

            // Execute the call
            self.codegen.add_instruction(Instruction::Call);
            Ok(result_type)
        } else if let Type::Process {
            send: send_type, ..
        } = target_type
        {
            // Send to process
            // Type check: ensure it has a send type and value type matches
            let send_id = *send_type;
            if let Some(expected_send_type_id) = send_id {
                // Check if it's the empty union (never accepts sends)
                if self.is_never(expected_send_type_id) {
                    return Err(Error::TypeMismatch {
                        expected: "process with send type".to_string(),
                        found: "process without send type (cannot send to it)".to_string(),
                    });
                }
                if !quiver_core::types::is_compatible(
                    value_type,
                    expected_send_type_id,
                    &self.program,
                ) {
                    return Err(Error::TypeMismatch {
                        expected: quiver_core::format::format_type_by_id(
                            &self.program,
                            expected_send_type_id,
                        ),
                        found: quiver_core::format::format_type_by_id(&self.program, value_type),
                    });
                }
            } else {
                // None means unknown send type
                return Err(Error::TypeMismatch {
                    expected: "process with known send type".to_string(),
                    found: "process with unknown send type".to_string(),
                });
            }

            // Emit send instruction (expects [value, process] on stack)
            self.codegen.add_instruction(Instruction::Send);

            Ok(target_type_id)
        } else {
            Err(Error::TypeMismatch {
                expected: "function, process, or resource".to_string(),
                found: quiver_core::format::format_type_by_id(&self.program, target_type_id),
            })
        }
    }

    fn compile_tail_call(
        &mut self,
        identifier: Option<&str>,
        accessors: &[ast::AccessPath],
        arg_type: Option<usize>,
    ) -> Result<usize, Error> {
        // Handle argument - if none provided, check if function parameter is nil and use that
        let _arg_type = if let Some(arg_t) = arg_type {
            arg_t
        } else {
            let (func_param_type, _) = scopes::get_function_parameter(&self.scopes)?;
            if func_param_type == self.program.register_type(Type::nil()) {
                // Push nil onto stack for tail call
                let nil_tuple_id = self.program.register_tuple(None, vec![]);
                self.codegen
                    .add_instruction(Instruction::Tuple(nil_tuple_id));
                self.program.register_type(Type::nil())
            } else {
                return Err(Error::FeatureUnsupported(
                    "Tail call requires a value".to_string(),
                ));
            }
        };

        if identifier.is_none() && accessors.is_empty() {
            // Tail call to parameter - argument is already on stack, just emit tail call
            self.codegen.add_instruction(Instruction::TailCall(true));
            Ok(self.program.never())
        } else {
            // Tail call to identifier with accessors
            let name = identifier.ok_or_else(|| {
                Error::FeatureUnsupported(
                    "Member access in tail call requires an identifier".to_string(),
                )
            })?;

            let func_type = if accessors.is_empty() {
                // Simple identifier lookup
                match scopes::lookup_variable(&self.scopes, name, &[]) {
                    Some((func_type, index)) => {
                        self.codegen.add_instruction(Instruction::Load(index));
                        func_type
                    }
                    None => return Err(Error::VariableUndefined(name.to_string())),
                }
            } else {
                // Use member access compilation for accessors
                self.compile_member_access(name, accessors.to_vec())?.0
            };

            // Verify it's a function
            match self.program.lookup_type(func_type) {
                Some(Type::Callable { result, .. }) => {
                    self.codegen.add_instruction(Instruction::TailCall(false));
                    Ok(*result)
                }
                _ => Err(Error::TypeMismatch {
                    expected: "function".to_string(),
                    found: quiver_core::format::format_type_by_id(&self.program, func_type),
                }),
            }
        }
    }

    fn compile_builtin(&mut self, name: &str) -> Result<usize, Error> {
        let (param_type, result_type) = self
            .builtins
            .resolve_signature(name, &mut self.program)
            .ok_or_else(|| Error::BuiltinUndefined(name.to_string()))?;

        // Register the types
        let param_type_id = self.program.register_type(param_type);
        let result_type_id = self.program.register_type(result_type);

        let builtin_index = self
            .program
            .register_builtin(name.to_string(), self.builtins);

        self.codegen
            .add_instruction(Instruction::Builtin(builtin_index));

        let never_id = self.program.never();
        Ok(self.program.register_type(Type::Callable {
            parameter: param_type_id,
            result: result_type_id,
            receive: never_id,
        }))
    }

    fn compile_equality(&mut self, value_type: usize) -> Result<usize, Error> {
        // The == operator works with a tuple on the stack
        // We need to extract the tuple elements and call Equal(count)

        // Get field count and first field types (handles both tuples and partials)
        let (field_count, first_field_types) =
            type_queries::get_field_count_and_first_types(&self.program, value_type)?;

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
        result_types.push(self.program.register_type(Type::nil()));
        Ok(typing::union_type_ids(&mut self.program, result_types))
    }

    fn compile_not(&mut self, _value_type: usize) -> Result<usize, Error> {
        // The ! operator works with any value on the stack
        // It converts [] to Ok and everything else to []
        self.codegen.add_instruction(Instruction::Not);

        // The Not instruction returns either Ok or NIL
        let ok_type = self.program.register_type(Type::ok());
        let nil_type = self.program.register_type(Type::nil());
        let result_type = typing::union_type_ids(&mut self.program, vec![ok_type, nil_type]);
        Ok(result_type)
    }

    /// Compiles accessor chain and tracks field provenance.
    fn compile_accessor(
        &mut self,
        mut last_type: usize,
        accessors: Vec<ast::AccessPath>,
        target_name: &str,
        base_provenance: Provenance,
    ) -> Result<(usize, Provenance), Error> {
        let mut current_prov = base_provenance;

        for accessor in accessors {
            let (index, field_types) = match accessor {
                ast::AccessPath::Field(field_name) => type_queries::get_field_by_name(
                    &self.program,
                    last_type,
                    &field_name,
                    target_name,
                )?,
                ast::AccessPath::Index(index) => {
                    let field_types = type_queries::get_field_at_index(
                        &self.program,
                        last_type,
                        index,
                        target_name,
                    )?;
                    (index, field_types)
                }
            };

            self.codegen.add_instruction(Instruction::Get(index));
            last_type = typing::union_type_ids(&mut self.program, field_types);
            // Update provenance to track the field access
            current_prov = current_prov.field(index);

            // If the field provenance resolved to a Variable and that variable stores
            // a Tuple provenance, use the Tuple provenance for nested tuple tracking.
            // Keep Variable provenance for non-tuple variables so narrowing works correctly.
            if let Provenance::Variable(ref var_name) = current_prov
                && let Some(stored_prov @ Provenance::Tuple(_)) =
                    scopes::lookup_variable_provenance(&self.scopes, var_name)
            {
                current_prov = stored_prov;
            }
        }

        Ok((last_type, current_prov))
    }

    /// Compiles member access and tracks provenance.
    fn compile_member_access(
        &mut self,
        target: &str,
        accessors: Vec<ast::AccessPath>,
    ) -> Result<(usize, Provenance), Error> {
        // Check if we have a capture for the full path (base + accessors)
        if !accessors.is_empty()
            && let Some((capture_type, capture_index)) =
                scopes::lookup_variable(&self.scopes, target, &accessors)
        {
            // We have a pre-evaluated capture for this exact path
            self.codegen
                .add_instruction(Instruction::Load(capture_index));
            // Captures lose provenance tracking
            return Ok((capture_type, Provenance::Unknown));
        }

        // No pre-evaluated capture, use standard member access
        let (last_type, index) = scopes::lookup_variable(&self.scopes, target, &[])
            .ok_or(Error::VariableUndefined(target.to_string()))?;
        self.codegen.add_instruction(Instruction::Load(index));

        // Determine the base provenance for this access:
        // - If no field access (empty accessors), use Variable provenance so narrowing affects
        //   this variable directly
        // - If field access and the stored provenance is a Tuple, use it so field access can
        //   resolve through to original source provenances
        // - Otherwise use Variable provenance for proper field narrowing
        let base_prov = if accessors.is_empty() {
            Provenance::Variable(target.to_string())
        } else {
            match scopes::lookup_variable_provenance(&self.scopes, target) {
                Some(prov @ Provenance::Tuple(_)) => prov,
                _ => Provenance::Variable(target.to_string()),
            }
        };
        self.compile_accessor(last_type, accessors, target, base_prov)
    }
}
