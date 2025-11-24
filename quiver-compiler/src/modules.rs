use std::collections::HashMap;
use std::path::PathBuf;

pub trait ModuleLoader {
    fn load(&self, module: &[String]) -> Result<String, ModuleError>;
}

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleError {
    NotFound(String),
    IoError(String),
}

#[derive(Clone)]
pub struct InMemoryModuleLoader {
    modules: HashMap<Vec<String>, String>,
}

impl InMemoryModuleLoader {
    pub fn new(modules: HashMap<Vec<String>, String>) -> Self {
        Self { modules }
    }
}

impl ModuleLoader for InMemoryModuleLoader {
    fn load(&self, module: &[String]) -> Result<String, ModuleError> {
        self.modules
            .get(module)
            .cloned()
            .ok_or_else(|| ModuleError::NotFound(module.join("/")))
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
    fn load(&self, module: &[String]) -> Result<String, ModuleError> {
        // Build file path: std/segment1/segment2/.../segmentN.qv
        let mut file_path = PathBuf::from("std");
        for segment in module {
            file_path.push(segment);
        }
        file_path.set_extension("qv");

        if file_path.exists() {
            std::fs::read_to_string(&file_path).map_err(|e| ModuleError::IoError(e.to_string()))
        } else {
            Err(ModuleError::NotFound(format!(
                "Module not found: {}",
                module.join("/")
            )))
        }
    }
}
