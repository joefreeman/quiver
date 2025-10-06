use std::collections::HashMap;
use std::path::{Path, PathBuf};

pub trait ModuleLoader {
    fn load(&self, path: &str, from_dir: Option<&Path>) -> Result<String, ModuleError>;
}

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleError {
    NotFound(String),
    IoError(String),
}

#[derive(Clone)]
pub struct InMemoryModuleLoader {
    modules: HashMap<String, String>,
}

impl InMemoryModuleLoader {
    pub fn new(modules: HashMap<String, String>) -> Self {
        Self { modules }
    }
}

impl ModuleLoader for InMemoryModuleLoader {
    fn load(&self, path: &str, from_dir: Option<&Path>) -> Result<String, ModuleError> {
        if path.starts_with("./") || path.starts_with("../") {
            // Relative import - use in-memory modules
            let normalised = if let Some(base_dir) = from_dir {
                base_dir
                    .join(path)
                    .components()
                    .collect::<PathBuf>()
                    .to_string_lossy()
                    .to_string()
            } else {
                path.to_string()
            };

            self.modules
                .get(&normalised)
                .cloned()
                .ok_or_else(|| ModuleError::NotFound(path.to_string()))
        } else {
            // Standard library import - fall back to filesystem
            let filesystem_loader = FileSystemModuleLoader::new();
            filesystem_loader.load(path, from_dir)
        }
    }
}

pub struct FileSystemModuleLoader {}

impl FileSystemModuleLoader {
    pub fn new() -> Self {
        Self {}
    }
}

impl ModuleLoader for FileSystemModuleLoader {
    fn load(&self, path: &str, from_dir: Option<&Path>) -> Result<String, ModuleError> {
        let resolved = if path.starts_with("./") || path.starts_with("../") {
            // Relative import - resolve relative to from_dir
            let full_path = from_dir.unwrap_or(Path::new(".")).join(path);
            if full_path.exists() {
                full_path
            } else {
                return Err(ModuleError::NotFound(path.to_string()));
            }
        } else {
            // Standard library import - look in std/ directory
            let mut stdlib_path = PathBuf::from("std").join(path);
            // Add .qv extension if not present
            if stdlib_path.extension().is_none() {
                stdlib_path.set_extension("qv");
            }
            if stdlib_path.exists() {
                stdlib_path
            } else {
                return Err(ModuleError::NotFound(format!(
                    "Standard library module not found: {}",
                    path
                )));
            }
        };

        std::fs::read_to_string(&resolved).map_err(|e| ModuleError::IoError(e.to_string()))
    }
}
