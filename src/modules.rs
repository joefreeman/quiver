use std::collections::HashMap;
use std::path::{Path, PathBuf};

pub trait ModuleLoader {
    fn load(&self, path: &str, from: Option<&Path>) -> Result<String, ModuleError>;
    fn resolve(&self, path: &str, from: Option<&Path>) -> Result<PathBuf, ModuleError>;
    fn clone_box(&self) -> Box<dyn ModuleLoader>;
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
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }

    pub fn add_module(&mut self, path: impl Into<String>, content: impl Into<String>) {
        self.modules.insert(path.into(), content.into());
    }

    pub fn with_modules(modules: HashMap<String, String>) -> Self {
        Self { modules }
    }
}

impl ModuleLoader for InMemoryModuleLoader {
    fn load(&self, path: &str, from: Option<&Path>) -> Result<String, ModuleError> {
        // Use the same normalization logic as resolve
        let normalized = if path.starts_with("./") || path.starts_with("../") {
            if let Some(base) = from {
                // Get the parent directory and join with the relative path
                let parent = base.parent().unwrap_or(base);
                let joined = parent.join(path);
                // Normalize the path by collecting components to remove redundant "./" parts
                joined.components().collect::<PathBuf>()
                    .to_string_lossy()
                    .to_string()
            } else {
                path.to_string()
            }
        } else {
            path.to_string()
        };
        
        self.modules
            .get(&normalized)
            .cloned()
            .ok_or_else(|| ModuleError::NotFound(path.to_string()))
    }

    fn resolve(&self, path: &str, from: Option<&Path>) -> Result<PathBuf, ModuleError> {
        // For in-memory loader, just normalize the path
        let normalized = if path.starts_with("./") || path.starts_with("../") {
            if let Some(base) = from {
                // Get the parent directory and join with the relative path
                let parent = base.parent().unwrap_or(base);
                let joined = parent.join(path);
                // Normalize the path by collecting components to remove redundant "./" parts
                joined.components().collect::<PathBuf>()
                    .to_string_lossy()
                    .to_string()
            } else {
                path.to_string()
            }
        } else {
            path.to_string()
        };
        
        
        if self.modules.contains_key(&normalized) {
            Ok(PathBuf::from(normalized))
        } else {
            Err(ModuleError::NotFound(path.to_string()))
        }
    }
    
    fn clone_box(&self) -> Box<dyn ModuleLoader> {
        Box::new(self.clone())
    }
}

pub struct FileSystemModuleLoader {
    search_paths: Vec<PathBuf>,
}

impl FileSystemModuleLoader {
    pub fn new() -> Self {
        Self {
            search_paths: vec![PathBuf::from(".")],
        }
    }

    pub fn with_search_paths(paths: Vec<PathBuf>) -> Self {
        Self {
            search_paths: paths,
        }
    }
}

impl ModuleLoader for FileSystemModuleLoader {
    fn load(&self, path: &str, from: Option<&Path>) -> Result<String, ModuleError> {
        let resolved = self.resolve(path, from)?;
        std::fs::read_to_string(&resolved)
            .map_err(|e| ModuleError::IoError(e.to_string()))
    }

    fn resolve(&self, path: &str, from: Option<&Path>) -> Result<PathBuf, ModuleError> {
        // Handle relative paths
        if path.starts_with("./") || path.starts_with("../") {
            let base = from.and_then(|p| p.parent()).unwrap_or(Path::new("."));
            let full_path = base.join(path);
            
            // Try with .qv extension
            let with_ext = full_path.with_extension("qv");
            if with_ext.exists() {
                return Ok(with_ext);
            }
            
            if full_path.exists() {
                return Ok(full_path);
            }
            
            return Err(ModuleError::NotFound(path.to_string()));
        }
        
        // Handle standard library (std.*)
        if path.starts_with("std.") {
            // Look in standard library paths
            for search_path in &self.search_paths {
                let std_path = search_path.join("std").join(&path[4..]).with_extension("qv");
                if std_path.exists() {
                    return Ok(std_path);
                }
            }
        }
        
        // Handle third-party modules
        for search_path in &self.search_paths {
            let module_path = search_path.join(path).with_extension("qv");
            if module_path.exists() {
                return Ok(module_path);
            }
        }
        
        Err(ModuleError::NotFound(path.to_string()))
    }
    
    fn clone_box(&self) -> Box<dyn ModuleLoader> {
        Box::new(FileSystemModuleLoader::with_search_paths(self.search_paths.clone()))
    }
}