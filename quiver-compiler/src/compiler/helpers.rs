use crate::ast;
use std::collections::HashSet;

use super::Error;

/// Check if a tuple contains ripple operations (~) in any of its field values
pub fn tuple_contains_ripple(fields: &[ast::TupleField]) -> bool {
    fields.iter().any(|f| match &f.value {
        ast::FieldValue::Chain(chain) => chain_contains_ripple(chain),
        ast::FieldValue::Spread(_) => false,
    })
}

/// Check if any source in a select operation contains ripple
pub fn select_contains_ripple(sources: &[ast::Chain]) -> bool {
    sources.iter().any(chain_contains_ripple)
}

/// Check if a chain contains ripple operations
pub fn chain_contains_ripple(chain: &ast::Chain) -> bool {
    for term in &chain.terms {
        match term {
            ast::Term::Ripple => return true,
            ast::Term::BindMatch(_) | ast::Term::PinMatch(_) => continue,
            ast::Term::Tuple(tuple) => return tuple_contains_ripple(&tuple.fields),
            ast::Term::Block(block) => return block_contains_ripple(block),
            ast::Term::Function(func) => {
                return func.body.as_ref().is_some_and(block_contains_ripple);
            }
            ast::Term::Select(select) => {
                return select_contains_ripple(&select.sources);
            }
            _ => return false,
        }
    }
    false
}

/// Check if a block contains ripple operations
pub fn block_contains_ripple(block: &ast::Block) -> bool {
    block.branches.iter().any(|branch| {
        branch.condition.chains.iter().any(chain_contains_ripple)
            || branch
                .consequence
                .as_ref()
                .is_some_and(|expr| expr.chains.iter().any(chain_contains_ripple))
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
