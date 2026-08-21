//! Every suite, in one test binary.
//!
//! Cargo builds one binary per file in `tests/`, and each links the whole compiler and
//! runtime — 100MB apiece, three seconds apiece, relinked in full whenever anything beneath
//! them changes. Eighty-odd of those is minutes of linking and tens of gigabytes for a suite
//! whose actual execution is measured in seconds.
//!
//! Merging also compounds the environment pool in `common`: the pool is thread-local, so its
//! saving is bounded by how many tests share a process. One binary is one process, and the
//! standard library links into a pooled environment once for all of them rather than once per
//! suite.

#[path = "common/mod.rs"]
mod common;
mod suites;
