use std::collections::HashMap;

use crate::{ast, modules::ModuleLoader, parser};
use quiver_core::program::Program;

use super::{Error, Scope, scopes};

#[derive(Clone)]
pub struct ModuleCache {
    pub ast_cache: HashMap<Vec<String>, ast::Program>,
    pub import_stack: Vec<Vec<String>>,
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
        }
    }

    pub fn load_and_cache_ast(
        &mut self,
        module: &[String],
        module_loader: &dyn ModuleLoader,
    ) -> Result<ast::Program, Error> {
        if let Some(cached_ast) = self.ast_cache.get(module).cloned() {
            return Ok(cached_ast);
        }

        let content = module_loader
            .load(module)
            .map_err(Error::ModuleLoad)?;

        let parsed = parser::parse(&content).map_err(|e| Error::ModuleParse {
            module: module.join("/"),
            error: e,
        })?;

        self.ast_cache.insert(module.to_vec(), parsed.clone());

        Ok(parsed)
    }
}

pub fn compile_type_import(
    pattern: ast::TypeImportPattern,
    module: &[String],
    module_cache: &mut ModuleCache,
    module_loader: &dyn ModuleLoader,
    scopes_mut: &mut [Scope],
    _program: &mut Program,
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
