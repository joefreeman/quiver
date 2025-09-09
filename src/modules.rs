use std::collections::HashMap;
use std::path::{Path, PathBuf};

pub trait ModuleLoader {
    fn load(&self, path: &str, from_dir: Option<&Path>) -> Result<String, ModuleError>;
}

#[derive(Debug, Clone)]
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
        let normalised = if path.starts_with("./") || path.starts_with("../") {
            if let Some(base_dir) = from_dir {
                base_dir
                    .join(path)
                    .components()
                    .collect::<PathBuf>()
                    .to_string_lossy()
                    .to_string()
            } else {
                path.to_string()
            }
        } else {
            return Err(ModuleError::NotFound(format!(
                "Module path must be relative (start with ./ or ../): {}",
                path
            )));
        };

        self.modules
            .get(&normalised)
            .cloned()
            .ok_or_else(|| ModuleError::NotFound(path.to_string()))
    }
}

pub struct FileSystemModuleLoader {}

impl Default for FileSystemModuleLoader {
    fn default() -> Self {
        Self::new()
    }
}

impl FileSystemModuleLoader {
    pub fn new() -> Self {
        Self {}
    }
}

impl ModuleLoader for FileSystemModuleLoader {
    fn load(&self, path: &str, from_dir: Option<&Path>) -> Result<String, ModuleError> {
        let resolved = if path.starts_with("./") || path.starts_with("../") {
            let full_path = from_dir.unwrap_or(Path::new(".")).join(path);
            if full_path.exists() {
                full_path
            } else {
                return Err(ModuleError::NotFound(path.to_string()));
            }
        } else {
            return Err(ModuleError::NotFound(format!(
                "Module path must be relative (start with ./ or ../): {}",
                path
            )));
        };

        std::fs::read_to_string(&resolved).map_err(|e| ModuleError::IoError(e.to_string()))
    }
}
