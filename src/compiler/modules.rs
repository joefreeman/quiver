use std::{collections::HashMap, path::PathBuf};

use crate::{ast, modules::ModuleLoader, parser, vm::Value};

use super::{Error, typing::TypeContext};

pub struct ModuleCache {
    pub ast_cache: HashMap<String, ast::Program>,
    pub evaluation_cache: HashMap<String, Value>,
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
            evaluation_cache: HashMap::new(),
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

        let parsed = parser::parse(&content).map_err(|_e| {
            Error::ModuleParse(format!("Failed to parse imported module: {}", module_path))
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
    type_context: &mut TypeContext,
) -> Result<(), Error> {
    let parsed =
        module_cache.load_and_cache_ast(module_path, module_loader, current_module_path)?;

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
                let resolved_type = type_context.resolve_ast_type(type_definition.clone())?;
                type_context
                    .type_aliases
                    .insert(name.to_string(), resolved_type);
            }
        }
        ast::TypeImportPattern::Partial(requested_names) => {
            for requested_name in requested_names {
                if let Some((name, type_definition)) = type_aliases
                    .iter()
                    .find(|alias| alias.0.as_str() == *requested_name)
                {
                    let resolved_type =
                        type_context.resolve_ast_type((*type_definition).clone())?;
                    type_context
                        .type_aliases
                        .insert(name.to_string(), resolved_type);
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
