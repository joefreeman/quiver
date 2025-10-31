mod common;
use common::*;

#[test]
fn test_equal_integers() {
    quiver().evaluate("[42] ~> ==").expect("42");
    quiver().evaluate("[42, 42, 42] ~> ==").expect("42");
}

#[test]
fn test_unequal_integers() {
    quiver().evaluate("[1, 1, 1, 2] ~> ==").expect("[]");
}

#[test]
fn test_binary_equality_content() {
    // Test that binaries with same content are equal even if different references
    quiver()
        .evaluate("['68656c6c6f', '68656c6c6f'] ~> ==")
        .expect("'68656c6c6f'");
    quiver().evaluate("['abcd', 'abcd'] ~> ==").expect("'abcd'");
}

#[test]
fn test_binary_inequality_content() {
    // Test that binaries with different content are not equal
    quiver()
        .evaluate("['68656c6c6f', '776f726c64'] ~> ==")
        .expect("[]");
    quiver()
        .evaluate("['61626364', '65666768'] ~> ==")
        .expect("[]");
}

#[test]
fn test_equal_strings() {
    quiver()
        .evaluate("[\"abc\", \"abc\"] ~> ==")
        .expect("\"abc\"");
}
