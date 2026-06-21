use std::collections::HashMap;

use crate::{
    ast, parser,
    resolver::{ModuleId, ModuleResolver, PackageId},
};
use quiver_core::effects::Effect;
use quiver_core::program::Program;
use quiver_core::types::Type;
use quiver_core::value::{Binary, Value};

use super::{Error, Scope, ScopeKind, scopes, typing, typing::TypeAliasDef};

/// Cached module value with extracted binary data
#[derive(Clone)]
pub struct CachedModule {
    pub value: Value,
    pub module_type: Type,
    /// Binary data extracted from executor heap, keyed by heap index
    pub binary_data: HashMap<usize, Vec<u8>>,
}

#[derive(Clone)]
pub struct ModuleCache {
    pub ast_cache: HashMap<ModuleId, ast::Program>,
    pub import_stack: Vec<ModuleId>,
    /// Cache for module values with their types and extracted binary data.
    /// With capture-by-value, function indices can be reused, making this cache valid.
    pub value_cache: HashMap<ModuleId, CachedModule>,
    /// Cache of resolved type namespaces (default + named types) per module. The type IDs
    /// are valid for the lifetime of the `Program` being compiled.
    pub type_namespace_cache: HashMap<ModuleId, ModuleTypeNamespace>,
    /// Modules whose type namespaces are currently being built, for cyclic-reference detection.
    pub type_namespace_stack: Vec<ModuleId>,
}

impl Default for ModuleCache {
    fn default() -> Self {
        Self::new()
    }
}

impl ModuleCache {
    pub fn new() -> Self {
        Self {
            ast_cache: HashMap::new(),
            import_stack: Vec::new(),
            value_cache: HashMap::new(),
            type_namespace_cache: HashMap::new(),
            type_namespace_stack: Vec::new(),
        }
    }

    /// Get cached module value
    pub fn get_cached_module(&self, id: &ModuleId) -> Option<&CachedModule> {
        self.value_cache.get(id)
    }

    /// Cache a module value with its type and extracted binary data
    pub fn cache_module(&mut self, id: ModuleId, cached: CachedModule) {
        self.value_cache.insert(id, cached);
    }

    pub fn load_and_cache_ast(
        &mut self,
        id: &ModuleId,
        source: &str,
    ) -> Result<ast::Program, Error> {
        if let Some(cached_ast) = self.ast_cache.get(id).cloned() {
            return Ok(cached_ast);
        }

        let parsed = parser::parse(source).map_err(|e| Error::ModuleParse {
            module: id.display(),
            error: Box::new(e),
        })?;

        self.ast_cache.insert(id.clone(), parsed.clone());

        Ok(parsed)
    }
}

/// Extract all binary data from a Value tree that references the executor heap.
/// Populates the provided map from heap index to the actual bytes.
pub fn extract_binary_data<E: Effect>(
    value: &Value,
    executor: &quiver_core::executor::Executor<E>,
    binary_data: &mut HashMap<usize, Vec<u8>>,
) {
    match value {
        Value::Binary(Binary::Heap(heap_idx)) => {
            if !binary_data.contains_key(heap_idx)
                && let Some(data) = executor.get_heap_binary(*heap_idx)
            {
                binary_data.insert(*heap_idx, data.to_vec());
            }
        }
        Value::Binary(Binary::Constant(_)) => {
            // Constant binaries reference Program.constants which persists - no extraction needed
        }
        Value::Tuple(_, fields) => {
            for field in fields.iter() {
                extract_binary_data(field, executor, binary_data);
            }
        }
        Value::Function(_, captures) => {
            for capture in captures.iter() {
                extract_binary_data(capture, executor, binary_data);
            }
        }
        Value::Integer(_)
        | Value::Builtin(_)
        | Value::Process(..)
        | Value::Resource(..)
        | Value::Reference(_) => {}
    }
}

/// A module's type namespace: its nameless default type (`' = ...`), if any, plus its
/// named type aliases, each resolved to a `TypeAliasDef`. Referenced from other modules
/// as `'%mod` (default) and `'%mod.name` (named).
#[derive(Clone)]
pub struct ModuleTypeNamespace {
    pub default: Option<TypeAliasDef>,
    pub named: HashMap<String, TypeAliasDef>,
}

/// Build (or fetch from cache) the type namespace of a module. Type definitions in the
/// module are resolved against the module's own package (hermetic resolution); references
/// to further modules (`'%other`) inside them resolve recursively. Cyclic references are
/// rejected.
pub fn module_type_namespace(
    module: &[String],
    resolver: &dyn ModuleResolver,
    module_cache: &mut ModuleCache,
    from_package: &PackageId,
    program: &mut Program,
) -> Result<ModuleTypeNamespace, Error> {
    let resolved = resolver
        .resolve(from_package, module)
        .map_err(Error::ModuleLoad)?;
    let id = resolved.id.clone();

    if let Some(namespace) = module_cache.type_namespace_cache.get(&id) {
        return Ok(namespace.clone());
    }
    if module_cache.type_namespace_stack.contains(&id) {
        return Err(Error::ModuleTypeCycle(module.join("/")));
    }

    let parsed = module_cache.load_and_cache_ast(&id, &resolved.source)?;

    module_cache.type_namespace_stack.push(id.clone());
    let result = build_type_namespace(&parsed, &resolved.package, resolver, module_cache, program);
    module_cache.type_namespace_stack.pop();
    let namespace = result?;

    module_cache
        .type_namespace_cache
        .insert(id, namespace.clone());
    Ok(namespace)
}

/// Resolve every type alias declared in a module into a `ModuleTypeNamespace`. Aliases are
/// resolved in order so that later definitions can reference earlier ones.
fn build_type_namespace(
    parsed: &ast::Program,
    package: &PackageId,
    resolver: &dyn ModuleResolver,
    module_cache: &mut ModuleCache,
    program: &mut Program,
) -> Result<ModuleTypeNamespace, Error> {
    // A module-local scope holding the aliases resolved so far, so they can reference
    // each other during resolution.
    let mut module_scope = vec![Scope::new(HashMap::new(), None, ScopeKind::Root)];
    let mut default: Option<TypeAliasDef> = None;
    let mut named: HashMap<String, TypeAliasDef> = HashMap::new();

    for statement in &parsed.statements {
        let ast::Statement::TypeAlias {
            name,
            type_parameters,
            type_definition,
            ..
        } = statement
        else {
            continue;
        };

        // Create Type::Variable bindings for type parameters
        let mut bindings = HashMap::new();
        for param in type_parameters {
            let var_type_id = program.register_type(Type::Variable(param.clone()));
            bindings.insert(param.clone(), var_type_id);
        }

        // Resolve the type definition using the module scope built so far
        let mut env = typing::TypeEnv {
            resolver,
            module_cache: &mut *module_cache,
            package,
        };
        let type_id = typing::resolve_ast_type_with_bindings(
            &mut env,
            &module_scope,
            type_definition.clone(),
            program,
            &bindings,
        )?;

        let def = TypeAliasDef {
            parameters: type_parameters.clone(),
            type_id,
        };
        match name {
            Some(name) => {
                named.insert(name.clone(), def.clone());
                scopes::define_type_alias(&mut module_scope, name.clone(), def);
            }
            None => {
                // Bind under the reserved key too, so a bare `'` later in the module resolves.
                default = Some(def.clone());
                scopes::define_type_alias(
                    &mut module_scope,
                    typing::SELF_DEFAULT_KEY.to_string(),
                    def,
                );
            }
        }
    }

    Ok(ModuleTypeNamespace { default, named })
}
