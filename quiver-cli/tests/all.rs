//! The CLI integration suites, in one test binary.
//!
//! Each of these links the `quiv` binary's whole dependency tree — 155MB a piece — so they
//! are merged for the same reason `quiver-tests` is: one link instead of four, and one
//! artifact instead of four.

mod suites;
