pub mod ast;
pub mod compiler;
pub mod format;
pub mod manifest;
pub mod parser;
pub mod pretty;
pub mod recorder;
pub mod resolver;
pub mod simplify;

pub use compiler::Compiler;
pub use format::format_program;
pub use manifest::{Manifest, ManifestError};
pub use parser::parse;
pub use resolver::{
    ModuleError, ModuleId, ModuleOrigin, ModuleResolver, Overlay, PackageId, PackageResolver,
    ResolvedModule, find_project_root,
};
