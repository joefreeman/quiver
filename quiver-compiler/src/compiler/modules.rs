use std::{collections::HashMap, path::PathBuf};

use crate::{ast, modules::ModuleLoader, parser};
use quiver_core::{bytecode::Instruction, program::Program};

use super::{Error, typing::TypeAliasDef};

#[derive(Clone)]
pub struct ModuleCache {
    pub ast_cache: HashMap<String, ast::Program>,
    pub instruction_cache: HashMap<String, (Vec<Instruction>, quiver_core::types::Type)>,
    pub import_stack: Vec<String>,
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
            instruction_cache: HashMap::new(),
            import_stack: Vec::new(),
        }
    }

    pub fn load_and_cache_ast(
        &mut self,
        module_path: &str,
        module_loader: &dyn ModuleLoader,
        current_module_path: Option<&PathBuf>,
    ) -> Result<ast::Program, Error> {
        if let Some(cached_ast) = self.ast_cache.get(module_path).cloned() {
            return Ok(cached_ast);
        }

        let content = module_loader
            .load(module_path, current_module_path.map(|p| p.as_path()))
            .map_err(Error::ModuleLoad)?;

        let parsed = parser::parse(&content).map_err(|e| Error::ModuleParse {
            module_path: module_path.to_string(),
            error: e,
        })?;

        self.ast_cache
            .insert(module_path.to_string(), parsed.clone());

        Ok(parsed)
    }
}

pub fn compile_type_import(
    pattern: ast::TypeImportPattern,
    module_path: &str,
    module_cache: &mut ModuleCache,
    module_loader: &dyn ModuleLoader,
    current_module_path: Option<&PathBuf>,
    type_aliases: &mut HashMap<String, TypeAliasDef>,
    _program: &mut Program,
) -> Result<(), Error> {
    let parsed =
        module_cache.load_and_cache_ast(module_path, module_loader, current_module_path)?;

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

                // Store all types as type aliases (with 0+ parameters)
                type_aliases.insert(
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
                    // Store all types as type aliases (with 0+ parameters)
                    type_aliases.insert(
                        name.to_string(),
                        ((*type_parameters).clone(), (*type_definition).clone()),
                    );
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
