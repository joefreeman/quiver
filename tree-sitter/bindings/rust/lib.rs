//! This crate provides Quiver language support for the [tree-sitter] parsing library.
//!
//! Typically, you will use the [`LANGUAGE`] constant to add this language to a
//! tree-sitter [`Parser`], and then use the parser to parse some code:
//!
//! ```
//! let code = r#"
//!     double = #'int { math.mul [~, 2] }
//! "#;
//! let mut parser = tree_sitter::Parser::new();
//! let language = tree_sitter_quiver::LANGUAGE;
//! parser
//!     .set_language(&language.into())
//!     .expect("Error loading Quiver parser");
//! let tree = parser.parse(code, None).unwrap();
//! assert!(!tree.root_node().has_error());
//! ```
//!
//! [`Parser`]: https://docs.rs/tree-sitter/*/tree_sitter/struct.Parser.html
//! [tree-sitter]: https://tree-sitter.github.io/

use tree_sitter_language::LanguageFn;

extern "C" {
    fn tree_sitter_quiver() -> *const ();
}

/// The tree-sitter [`LanguageFn`] for this grammar.
pub const LANGUAGE: LanguageFn = unsafe { LanguageFn::from_raw(tree_sitter_quiver) };

/// The syntax-highlighting query for this grammar.
pub const HIGHLIGHTS_QUERY: &str = include_str!("../../queries/highlights.scm");

/// The indentation query for this grammar, in the Helix/Aether `@indent`/`@outdent`
/// dialect (not the nvim-treesitter indent dialect).
pub const INDENTS_QUERY: &str = include_str!("../../queries/indents.scm");

#[cfg(test)]
mod tests {
    #[test]
    fn test_can_load_grammar() {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&super::LANGUAGE.into())
            .expect("Error loading Quiver parser");
    }

    #[test]
    fn test_highlights_query_compiles() {
        let language = super::LANGUAGE.into();
        tree_sitter::Query::new(&language, super::HIGHLIGHTS_QUERY)
            .expect("Error compiling Quiver highlights query");
    }

    #[test]
    fn test_indents_query_compiles() {
        let language = super::LANGUAGE.into();
        tree_sitter::Query::new(&language, super::INDENTS_QUERY)
            .expect("Error compiling Quiver indents query");
    }
}
