use std::collections::HashMap;

use crate::{ast, modules::ModuleLoader, parser};
use quiver_core::effects::Effect;
use quiver_core::types::Type;
use quiver_core::value::{Binary, Value};

use super::{Error, Scope, scopes};

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
    pub ast_cache: HashMap<Vec<String>, ast::Program>,
    pub import_stack: Vec<Vec<String>>,
    /// Cache for module values with their types and extracted binary data.
    /// With capture-by-value, function indices can be reused, making this cache valid.
    pub value_cache: HashMap<Vec<String>, CachedModule>,
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
        }
    }

    /// Get cached module value
    pub fn get_cached_module(&self, module: &[String]) -> Option<&CachedModule> {
        self.value_cache.get(module)
    }

    /// Cache a module value with its type and extracted binary data
    pub fn cache_module(&mut self, module: Vec<String>, cached: CachedModule) {
        self.value_cache.insert(module, cached);
    }

    pub fn load_and_cache_ast(
        &mut self,
        module: &[String],
        module_loader: &dyn ModuleLoader,
    ) -> Result<ast::Program, Error> {
        if let Some(cached_ast) = self.ast_cache.get(module).cloned() {
            return Ok(cached_ast);
        }

        let content = module_loader.load(module).map_err(Error::ModuleLoad)?;

        let parsed = parser::parse(&content).map_err(|e| Error::ModuleParse {
            module: module.join("/"),
            error: e,
        })?;

        self.ast_cache.insert(module.to_vec(), parsed.clone());

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
            for field in fields {
                extract_binary_data(field, executor, binary_data);
            }
        }
        Value::Function(_, captures) => {
            for capture in captures {
                extract_binary_data(capture, executor, binary_data);
            }
        }
        Value::Integer(_) | Value::Builtin(_) | Value::Process(..) | Value::Resource(..) => {}
    }
}

pub fn compile_type_import(
    pattern: ast::TypeImportPattern,
    module: &[String],
    module_cache: &mut ModuleCache,
    module_loader: &dyn ModuleLoader,
    scopes_mut: &mut [Scope],
) -> Result<(), Error> {
    let parsed = module_cache.load_and_cache_ast(module, module_loader)?;

    let found_aliases: Vec<_> = parsed
        .statements
        .iter()
        .filter_map(|stmt| match stmt {
            ast::Statement::TypeAlias {
                name,
                type_parameters,
                type_definition,
            } => Some((name, type_parameters, type_definition)),
            _ => None,
        })
        .collect();

    let module_str = module.join("/");

    match &pattern {
        ast::TypeImportPattern::Star => {
            for (name, type_parameters, type_definition) in found_aliases {
                // Prevent shadowing primitive types
                if matches!(name.as_str(), "int" | "bin") {
                    return Err(Error::TypeUnresolved(format!(
                        "Cannot redefine primitive type '{}'",
                        name
                    )));
                }

                // Store all types as type aliases in current scope
                scopes::define_type_alias(
                    scopes_mut,
                    name.to_string(),
                    (type_parameters.clone(), type_definition.clone()),
                );
            }
        }
        ast::TypeImportPattern::Partial(requested_names) => {
            for requested_name in requested_names {
                // Prevent shadowing primitive types
                if matches!(requested_name.as_str(), "int" | "bin") {
                    return Err(Error::TypeUnresolved(format!(
                        "Cannot redefine primitive type '{}'",
                        requested_name
                    )));
                }

                if let Some((name, type_parameters, type_definition)) = found_aliases
                    .iter()
                    .find(|alias| alias.0.as_str() == *requested_name)
                {
                    // Store all types as type aliases in current scope
                    scopes::define_type_alias(
                        scopes_mut,
                        name.to_string(),
                        ((*type_parameters).clone(), (*type_definition).clone()),
                    );
                } else {
                    return Err(Error::ModuleTypeMissing {
                        type_name: requested_name.clone(),
                        module: module_str.clone(),
                    });
                }
            }
        }
    }

    Ok(())
}
