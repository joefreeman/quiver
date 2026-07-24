//! Emits `QUIVER_COMPILER_FINGERPRINT`: a hash of the sources that determine the
//! compiler's behaviour — this crate and quiver-core. It is one component of every
//! artifact key (see `src/artifact.rs`), so a compiler change invalidates all cached
//! artifacts. Standard-library sources are deliberately *not* hashed: std modules
//! are content-addressed individually through their own sources, so editing one
//! invalidates only its artifact and its dependents' — not every cache on the
//! machine. The rerun directive for `../std` keeps the embedded sources fresh.

use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::path::Path;

fn main() {
    let mut hasher = DefaultHasher::new();
    for dir in ["src", "../quiver-core/src"] {
        hash_dir(Path::new(dir), Path::new(dir), &mut hasher);
        println!("cargo:rerun-if-changed={dir}");
    }
    println!("cargo:rerun-if-changed=../std");
    println!(
        "cargo:rustc-env=QUIVER_COMPILER_FINGERPRINT={:016x}",
        hasher.finish()
    );
}

fn hash_dir(root: &Path, dir: &Path, hasher: &mut DefaultHasher) {
    let mut entries: Vec<_> = std::fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("cannot read {}: {e}", dir.display()))
        .map(|entry| entry.expect("cannot read dir entry").path())
        .collect();
    entries.sort();
    for path in entries {
        if path.is_dir() {
            hash_dir(root, &path, hasher);
        } else {
            path.strip_prefix(root)
                .unwrap()
                .to_string_lossy()
                .hash(hasher);
            std::fs::read(&path)
                .unwrap_or_else(|e| panic!("cannot read {}: {e}", path.display()))
                .hash(hasher);
        }
    }
}
