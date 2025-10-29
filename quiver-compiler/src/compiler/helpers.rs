use crate::ast;
use std::collections::HashSet;

use super::Error;

/// Reserved primitive type names that cannot be used as variable or type alias names
pub const RESERVED_NAMES: &[&str] = &["int", "bin"];

/// Check if a name is reserved and cannot be used as a variable or type alias
pub fn is_reserved_name(name: &str) -> bool {
    RESERVED_NAMES.contains(&name)
}

/// Check if a tuple contains ripple operations (~) in any of its field values
/// Only checks if fields would consume an outer piped value, not ripples in nested chains
pub fn tuple_contains_ripple(fields: &[ast::TupleField]) -> bool {
    fields.iter().any(|f| match &f.value {
        ast::FieldValue::Chain(chain) => chain_starts_with_ripple(chain),
        ast::FieldValue::Spread(_) => false,
    })
}

/// Check if a chain starts with (or immediately contains) a ripple operation
/// This determines if the chain would consume an outer piped value.
/// Returns false if the chain starts with a self-contained expression (literal, identifier, etc.)
/// as any ripples in that case would consume the expression's value, not the outer piped value.
fn chain_starts_with_ripple(chain: &ast::Chain) -> bool {
    // Only check the first term - if it starts with a value-producing expression,
    // any later ripples are part of a nested chain
    if let Some(first_term) = chain.terms.first() {
        match first_term {
            // Direct ripple - consumes outer piped value
            ast::Term::Ripple => true,

            // Tuples/blocks might contain ripple that consumes outer value
            ast::Term::Tuple(tuple) => tuple_contains_ripple(&tuple.fields),
            ast::Term::Block(block) => block_contains_ripple(block),

            // Anything else (literal, identifier, function, etc.) is self-contained
            // Any ripples after this consume THIS value, not the outer piped value
            _ => false,
        }
    } else {
        false
    }
}

/// Check if any source in a select operation uses the outer piped value via ripple
pub fn select_contains_ripple(sources: &[ast::Chain]) -> bool {
    sources.iter().any(chain_starts_with_ripple)
}

/// Check if a block contains ripple operations that would consume an outer piped value
pub fn block_contains_ripple(block: &ast::Block) -> bool {
    block.branches.iter().any(|branch| {
        branch.condition.chains.iter().any(chain_starts_with_ripple)
            || branch
                .consequence
                .as_ref()
                .is_some_and(|expr| expr.chains.iter().any(chain_starts_with_ripple))
    })
}

/// Check if tuple fields contain spread operations (...)
pub fn tuple_contains_spread(fields: &[ast::TupleField]) -> bool {
    fields
        .iter()
        .any(|field| matches!(field.value, ast::FieldValue::Spread(_)))
}

/// Create a capture variable name from a base name and accessor path
/// Example: "foo" with [Field("bar"), Index(0)] becomes "foo.bar.0"
pub fn make_capture_name(base: &str, accessors: &[ast::AccessPath]) -> String {
    if accessors.is_empty() {
        base.to_string()
    } else {
        let accessor_suffix = accessors
            .iter()
            .map(|acc| match acc {
                ast::AccessPath::Field(name) => name.clone(),
                ast::AccessPath::Index(idx) => idx.to_string(),
            })
            .collect::<Vec<_>>()
            .join(".");
        format!("{}.{}", base, accessor_suffix)
    }
}

/// Check for duplicate field names in a collection
/// Returns an error if any field name appears more than once
pub fn check_field_name_duplicates<T>(
    fields: &[T],
    get_name: impl Fn(&T) -> Option<&String>,
) -> Result<(), Error> {
    let mut seen_names = HashSet::new();
    for field in fields {
        if let Some(field_name) = get_name(field)
            && !seen_names.insert(field_name.clone())
        {
            return Err(Error::FieldDuplicated(field_name.clone()));
        }
    }
    Ok(())
}
