//! Module resolution.
//!
//! Resolution is **per package**: a module resolves its imports against its own package's
//! `modules` routing table, never the importer's. A package boundary exists exactly where a
//! `quiver.toml` exists; the standard library is a built-in package embedded in the binary.
//! See `docs/module-resolution.md`.

use std::collections::HashMap;
use std::path::{Component, Path, PathBuf};
use std::sync::{Arc, Mutex};

use include_dir::{Dir, include_dir};

use crate::manifest::{Manifest, Provider};

/// The standard library, compiled into the binary so it resolves regardless of where (or how)
/// the host runs. This is the source of the built-in `std` package.
static STD_DIR: Dir = include_dir!("$CARGO_MANIFEST_DIR/../std");

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleError {
    NotFound(String),
    Io(String),
    Manifest(String),
}

impl std::fmt::Display for ModuleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModuleError::NotFound(m) => write!(f, "module not found: {m}"),
            ModuleError::Io(e) => write!(f, "module I/O error: {e}"),
            ModuleError::Manifest(e) => write!(f, "{e}"),
        }
    }
}

/// Identity of a package. Used (with a module name) as the canonical key for caching.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PackageId {
    /// The built-in standard library.
    Std,
    /// An in-memory package (REPL, web, tests).
    Memory,
    /// A filesystem package, identified by its canonical root directory (the directory of the
    /// `quiver.toml`, or — for a loose script with no manifest — the script's directory).
    Path(PathBuf),
}

/// Canonical identity of a module: its package plus its name within that package.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleId {
    pub package: PackageId,
    pub name: Vec<String>,
}

impl ModuleId {
    pub fn display(&self) -> String {
        self.name.join("/")
    }
}

/// Where a resolved module's source lives. `Path` modules can be opened by tooling (the
/// language server uses this for cross-file go-to-definition); `Virtual` ones (embedded
/// standard library, in-memory packages) have no openable location.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleOrigin {
    Path(PathBuf),
    Virtual,
}

/// A resolved module: its canonical id, the package context its own imports resolve against,
/// where its source lives, and its source.
pub struct ResolvedModule {
    pub id: ModuleId,
    pub package: PackageId,
    pub origin: ModuleOrigin,
    pub source: String,
}

/// Resolve an import path, in the context of the package it is imported from.
pub trait ModuleResolver {
    /// The package context for the entry (top-level) program.
    fn entry_package(&self) -> PackageId;
    /// Resolve `path` (e.g. `["mathx", "vec"]`) as imported from within `from`.
    fn resolve(&self, from: &PackageId, path: &[String]) -> Result<ResolvedModule, ModuleError>;
}

/// A read-only view of source files. Backs filesystem packages (`OverlayVfs`), the embedded
/// standard library (`EmbeddedVfs`), and in-memory packages (`MemoryVfs`).
pub trait Vfs: Send + Sync {
    fn read(&self, path: &Path) -> Option<String>;
    fn exists(&self, path: &Path) -> bool;
}

/// Unsaved editor buffers, overlaid on the real filesystem. The language server keeps this in
/// sync with open documents so imports resolve to live (unsaved) content rather than stale
/// on-disk source. Keyed by canonical path. Empty for non-editor hosts (CLI, tests), where
/// `OverlayVfs` is just the filesystem.
#[derive(Default)]
pub struct Overlay {
    files: Mutex<HashMap<PathBuf, String>>,
}

impl Overlay {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add or replace the buffer for `path`.
    pub fn set(&self, path: &Path, content: String) {
        self.files
            .lock()
            .unwrap()
            .insert(overlay_key(path), content);
    }

    /// Drop the buffer for `path` (e.g. the document was closed); reads fall back to disk.
    pub fn remove(&self, path: &Path) {
        self.files.lock().unwrap().remove(&overlay_key(path));
    }

    fn get(&self, path: &Path) -> Option<String> {
        self.files.lock().unwrap().get(&overlay_key(path)).cloned()
    }

    fn contains(&self, path: &Path) -> bool {
        self.files.lock().unwrap().contains_key(&overlay_key(path))
    }
}

/// Normalize a path for use as an overlay key, so a buffer set by absolute editor URI and a
/// lookup of a resolver-computed path land on the same entry.
fn overlay_key(path: &Path) -> PathBuf {
    canonicalize(path)
}

/// The filesystem, with open editor buffers (if any) taking precedence.
struct OverlayVfs {
    overlay: Arc<Overlay>,
}
impl Vfs for OverlayVfs {
    fn read(&self, path: &Path) -> Option<String> {
        self.overlay
            .get(path)
            .or_else(|| std::fs::read_to_string(path).ok())
    }
    fn exists(&self, path: &Path) -> bool {
        self.overlay.contains(path) || path.exists()
    }
}

struct EmbeddedVfs(&'static Dir<'static>);
impl Vfs for EmbeddedVfs {
    fn read(&self, path: &Path) -> Option<String> {
        self.0
            .get_file(path.to_string_lossy().as_ref())
            .and_then(|f| f.contents_utf8())
            .map(str::to_string)
    }
    fn exists(&self, path: &Path) -> bool {
        self.0.get_file(path.to_string_lossy().as_ref()).is_some()
    }
}

struct MemoryVfs(HashMap<String, String>);
impl Vfs for MemoryVfs {
    fn read(&self, path: &Path) -> Option<String> {
        self.0.get(path.to_string_lossy().as_ref()).cloned()
    }
    fn exists(&self, path: &Path) -> bool {
        self.0.contains_key(path.to_string_lossy().as_ref())
    }
}

/// One package: its identity, manifest, the root that its `path`/`file` providers are relative
/// to, and the file view they read through.
struct Package {
    id: PackageId,
    manifest: Manifest,
    root: PathBuf,
    vfs: Arc<dyn Vfs>,
    /// Whether this package's files live on the real filesystem (so cross-package boundaries —
    /// nested `quiver.toml` — are discovered by walking up). Embedded/in-memory packages have
    /// no nested manifests, so a file they provide always belongs to themselves.
    on_disk: bool,
}

/// The single concrete resolver. Holds the built-in std package and any discovered/registered
/// packages, and walks the filesystem to discover package boundaries on demand.
pub struct PackageResolver {
    entry: PackageId,
    packages: Mutex<HashMap<PackageId, Arc<Package>>>,
    /// Open editor buffers overlaid on disk for filesystem packages. Empty for CLI/tests.
    overlay: Arc<Overlay>,
}

impl PackageResolver {
    fn std_package() -> Arc<Package> {
        Arc::new(Package {
            id: PackageId::Std,
            // The standard library is just a directory of modules.
            manifest: Manifest {
                modules: vec![crate::manifest::ModuleRule {
                    name: None,
                    provider: Provider::Path(".".to_string()),
                }],
            },
            root: PathBuf::new(),
            vfs: Arc::new(EmbeddedVfs(&STD_DIR)),
            on_disk: false,
        })
    }

    fn base(entry: PackageId, extra: Vec<Arc<Package>>, overlay: Arc<Overlay>) -> Self {
        let mut packages = HashMap::new();
        let std = Self::std_package();
        packages.insert(PackageId::Std, std);
        for p in extra {
            packages.insert(p.id.clone(), p);
        }
        Self {
            entry,
            packages: Mutex::new(packages),
            overlay,
        }
    }

    /// A resolver for an in-memory package (REPL, web, tests). `modules` maps module names to
    /// source; the package can shadow the standard library, and bare names fall through to it.
    pub fn memory(modules: HashMap<Vec<String>, String>) -> Self {
        let files = modules
            .into_iter()
            .map(|(name, src)| (module_to_path(&name).to_string_lossy().into_owned(), src))
            .collect();
        let package = Arc::new(Package {
            id: PackageId::Memory,
            manifest: Manifest {
                modules: vec![
                    crate::manifest::ModuleRule {
                        name: None,
                        provider: Provider::Std,
                    },
                    crate::manifest::ModuleRule {
                        name: None,
                        provider: Provider::Path(".".to_string()),
                    },
                ],
            },
            root: PathBuf::new(),
            vfs: Arc::new(MemoryVfs(files)),
            on_disk: false,
        });
        Self::base(PackageId::Memory, vec![package], Arc::new(Overlay::new()))
    }

    /// A resolver whose entry program has no project — inline `--eval`, the MCP tools, a bare
    /// REPL. The default package is the standard library only.
    pub fn inline() -> Self {
        Self::memory(HashMap::new())
    }

    /// A resolver for an entry file on disk: discovers the entry's package by walking up for a
    /// `quiver.toml`, and discovers dependency packages the same way during resolution.
    pub fn for_entry_file(path: &Path) -> Self {
        Self::for_entry_file_with_overlay(path, Arc::new(Overlay::new()))
    }

    /// Like [`for_entry_file`](Self::for_entry_file), but reads filesystem packages through an
    /// editor `overlay` so unsaved buffers resolve to their live content. Used by the language
    /// server; the overlay is shared so the host can keep it in sync with open documents.
    pub fn for_entry_file_with_overlay(path: &Path, overlay: Arc<Overlay>) -> Self {
        Self::for_dir_with_overlay(path.parent().unwrap_or(Path::new(".")), overlay)
    }

    /// A resolver rooted at a directory — e.g. a REPL launched in `dir`. The entry package is
    /// the project containing `dir` (nearest `quiver.toml`), or a stdlib-only default if none.
    pub fn for_dir(dir: &Path) -> Self {
        Self::for_dir_with_overlay(dir, Arc::new(Overlay::new()))
    }

    fn for_dir_with_overlay(dir: &Path, overlay: Arc<Overlay>) -> Self {
        let resolver = Self::base(PackageId::Std, vec![], overlay);
        let entry = resolver.os_package_for_dir(dir).id.clone();
        // Re-home the entry now that we know its real package id.
        Self {
            entry,
            packages: resolver.packages,
            overlay: resolver.overlay,
        }
    }

    fn package(&self, id: &PackageId) -> Option<Arc<Package>> {
        self.packages.lock().unwrap().get(id).cloned()
    }

    /// Find (or build and cache) the filesystem package that owns `dir`: the nearest ancestor
    /// containing a `quiver.toml`, or — if none — a default (stdlib-only) package rooted at
    /// `dir` itself.
    fn os_package_for_dir(&self, dir: &Path) -> Arc<Package> {
        let mut current = canonicalize(dir);
        loop {
            if current.join("quiver.toml").exists() {
                return self.os_package(&current, true);
            }
            match current.parent() {
                Some(parent) => current = parent.to_path_buf(),
                None => break,
            }
        }
        self.os_package(&canonicalize(dir), false)
    }

    fn os_package(&self, root: &Path, has_manifest: bool) -> Arc<Package> {
        let root = canonicalize(root);
        let id = PackageId::Path(root.clone());
        if let Some(existing) = self.package(&id) {
            return existing;
        }
        let manifest = if has_manifest {
            let source = std::fs::read_to_string(root.join("quiver.toml")).unwrap_or_default();
            Manifest::parse(&source).unwrap_or_default()
        } else {
            Manifest::default()
        };
        let package = Arc::new(Package {
            id: id.clone(),
            manifest,
            root,
            vfs: Arc::new(OverlayVfs {
                overlay: self.overlay.clone(),
            }),
            on_disk: true,
        });
        self.packages.lock().unwrap().insert(id, package.clone());
        package
    }

    /// Determine which package a resolved file belongs to. For on-disk packages this walks up
    /// for the nearest `quiver.toml` (so vendored dependencies are hermetic); for embedded and
    /// in-memory packages the file always belongs to the providing package.
    fn owner_of(&self, provider_pkg: &Package, file: &Path) -> Arc<Package> {
        if provider_pkg.on_disk {
            let dir = file.parent().unwrap_or(Path::new("."));
            self.os_package_for_dir(dir)
        } else {
            self.package(&provider_pkg.id)
                .expect("provider package present")
        }
    }

    fn try_provider(
        &self,
        pkg: &Package,
        provider: &Provider,
        rem: &[String],
    ) -> Option<ResolvedModule> {
        match provider {
            Provider::Std => {
                let std = self.package(&PackageId::Std).unwrap();
                let file = module_to_path(rem);
                let source = std.vfs.read(&file)?;
                Some(ResolvedModule {
                    id: ModuleId {
                        package: PackageId::Std,
                        name: rem.to_vec(),
                    },
                    package: PackageId::Std,
                    origin: ModuleOrigin::Virtual,
                    source,
                })
            }
            Provider::Path(dir) => {
                // A bare namespace import (no remaining segments) has no file name.
                if rem.is_empty() {
                    return None;
                }
                let file = clean_path(&pkg.root.join(dir).join(module_to_path(rem)));
                let source = pkg.vfs.read(&file)?;
                let owner = self.owner_of(pkg, &file);
                Some(ResolvedModule {
                    id: module_id(&owner, &file),
                    package: owner.id.clone(),
                    origin: origin_of(pkg, &file),
                    source,
                })
            }
            Provider::File(path) => {
                if !rem.is_empty() {
                    return None;
                }
                let file = clean_path(&pkg.root.join(path));
                let source = pkg.vfs.read(&file)?;
                let owner = self.owner_of(pkg, &file);
                Some(ResolvedModule {
                    id: module_id(&owner, &file),
                    package: owner.id.clone(),
                    origin: origin_of(pkg, &file),
                    source,
                })
            }
        }
    }
}

impl ModuleResolver for PackageResolver {
    fn entry_package(&self) -> PackageId {
        self.entry.clone()
    }

    fn resolve(&self, from: &PackageId, path: &[String]) -> Result<ResolvedModule, ModuleError> {
        let pkg = self
            .package(from)
            .ok_or_else(|| ModuleError::NotFound(path.join("/")))?;

        // Later rules take precedence; a rule whose provider misses falls through.
        for rule in pkg.manifest.modules.iter().rev() {
            let Some(rem) = strip_name(rule.name.as_deref(), path) else {
                continue;
            };
            if let Some(resolved) = self.try_provider(&pkg, &rule.provider, &rem) {
                return Ok(resolved);
            }
        }
        Err(ModuleError::NotFound(path.join("/")))
    }
}

/// Strip a rule's `name` (a `/`-separated prefix) from an import path, returning the remaining
/// segments. `None` name matches at the root (returns the whole path).
fn strip_name(name: Option<&str>, path: &[String]) -> Option<Vec<String>> {
    match name {
        None => Some(path.to_vec()),
        Some(name) => {
            let prefix: Vec<&str> = name.split('/').filter(|s| !s.is_empty()).collect();
            if path.len() >= prefix.len() && path.iter().zip(&prefix).all(|(a, b)| a == b) {
                Some(path[prefix.len()..].to_vec())
            } else {
                None
            }
        }
    }
}

/// The openable location of a file provided by `pkg`: a real (canonical) path for on-disk
/// packages, `Virtual` for embedded/in-memory ones.
fn origin_of(pkg: &Package, file: &Path) -> ModuleOrigin {
    if pkg.on_disk {
        ModuleOrigin::Path(canonicalize(file))
    } else {
        ModuleOrigin::Virtual
    }
}

/// `["a", "b"]` → `a/b.qv`.
fn module_to_path(name: &[String]) -> PathBuf {
    let mut p = PathBuf::new();
    for segment in name {
        p.push(segment);
    }
    p.set_extension("qv");
    p
}

/// The canonical module name for a file within its owning package: the file's path relative to
/// the package root, without the `.qv` extension, as segments.
fn module_id(owner: &Package, file: &Path) -> ModuleId {
    let relative = file.strip_prefix(&owner.root).unwrap_or(file);
    let mut name: Vec<String> = relative
        .components()
        .filter_map(|c| match c {
            Component::Normal(s) => Some(s.to_string_lossy().into_owned()),
            _ => None,
        })
        .collect();
    if let Some(last) = name.last_mut() {
        *last = last.trim_end_matches(".qv").to_string();
    }
    ModuleId {
        package: owner.id.clone(),
        name,
    }
}

/// Remove `.` and empty components so paths read through `MemoryVfs`/`EmbeddedVfs` match their
/// keys (e.g. `./mymath.qv` → `mymath.qv`). `..` components are preserved.
fn clean_path(path: &Path) -> PathBuf {
    let mut out = PathBuf::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::Normal(s) if s.is_empty() => {}
            other => out.push(other.as_os_str()),
        }
    }
    out
}

fn canonicalize(path: &Path) -> PathBuf {
    std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf())
}

/// Walk up from `dir` for the nearest `quiver.toml`, returning the directory that contains it
/// (the project root), or `None` if there is no manifest at or above `dir`.
pub fn find_project_root(dir: &Path) -> Option<PathBuf> {
    let mut current = canonicalize(dir);
    loop {
        if current.join("quiver.toml").exists() {
            return Some(current);
        }
        current = current.parent()?.to_path_buf();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A throwaway on-disk project: a manifest exposing `./src` and a `util` module.
    fn temp_project(tag: &str) -> PathBuf {
        let dir =
            std::env::temp_dir().join(format!("quiver-resolver-{}-{}", tag, std::process::id()));
        let src = dir.join("src");
        std::fs::create_dir_all(&src).unwrap();
        std::fs::write(
            dir.join("quiver.toml"),
            "modules = [ { std = true }, { path = \"./src\" } ]",
        )
        .unwrap();
        std::fs::write(src.join("util.qv"), "DISK").unwrap();
        dir
    }

    #[test]
    fn resolves_project_module_and_std_with_origins() {
        let dir = temp_project("origins");
        let resolver = PackageResolver::for_entry_file(&dir.join("src/main.qv"));
        let entry = resolver.entry_package();

        let util = resolver.resolve(&entry, &["util".to_string()]).unwrap();
        assert_eq!(util.source, "DISK");
        assert_eq!(
            util.origin,
            ModuleOrigin::Path(std::fs::canonicalize(dir.join("src/util.qv")).unwrap())
        );

        // The standard library still resolves, with no openable origin.
        let math = resolver.resolve(&entry, &["math".to_string()]).unwrap();
        assert_eq!(math.id.package, PackageId::Std);
        assert_eq!(math.origin, ModuleOrigin::Virtual);

        std::fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn overlay_buffer_takes_precedence_over_disk() {
        let dir = temp_project("overlay");
        let overlay = Arc::new(Overlay::new());
        overlay.set(&dir.join("src/util.qv"), "BUFFER".to_string());

        let resolver =
            PackageResolver::for_entry_file_with_overlay(&dir.join("src/main.qv"), overlay.clone());
        let entry = resolver.entry_package();

        // The unsaved buffer wins over the on-disk "DISK".
        let util = resolver.resolve(&entry, &["util".to_string()]).unwrap();
        assert_eq!(util.source, "BUFFER");

        // Dropping the buffer falls back to disk.
        overlay.remove(&dir.join("src/util.qv"));
        let util = resolver.resolve(&entry, &["util".to_string()]).unwrap();
        assert_eq!(util.source, "DISK");

        std::fs::remove_dir_all(&dir).ok();
    }
}
