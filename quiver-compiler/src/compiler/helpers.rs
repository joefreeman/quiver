use crate::ast;
use std::collections::HashSet;

use super::Error;

/// Reserved primitive type names that cannot be used as variable or type alias names
pub const RESERVED_NAMES: &[&str] = &["int", "bin"];

/// Check if a name is reserved and cannot be used as a variable or type alias
pub fn is_reserved_name(name: &str) -> bool {
    RESERVED_NAMES.contains(&name)
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
