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
        .evaluate("[0x68656c6c6f, 0x68656c6c6f] ~> ==")
        .expect("0x68656c6c6f");
    quiver().evaluate("[0xabcd, 0xabcd] ~> ==").expect("0xabcd");
}

#[test]
fn test_binary_inequality_content() {
    // Test that binaries with different content are not equal
    quiver()
        .evaluate("[0x68656c6c6f, 0x776f726c64] ~> ==")
        .expect("[]");
    quiver()
        .evaluate("[0x61626364, 0x65666768] ~> ==")
        .expect("[]");
}

#[test]
fn test_equal_strings() {
    quiver()
        .evaluate("[\"abc\", \"abc\"] ~> ==")
        .expect("\"abc\"");
}

#[test]
fn test_structural_equality_across_construction() {
    // Value equality is structural: two tuples with the same name, field labels, and elements are
    // equal even when built via paths that inferred different field types (so they carry different
    // internal tuple type-ids). Here a literal list vs. one built by a generic function.
    quiver()
        .evaluate(
            "mk = #<'t>['t, 't] { =[x, y], Cons[x, Cons[y, Nil]] }, \
             [Cons[1, Cons[2, Nil]], mk [1, 2]] ~> ==",
        )
        .expect("Cons[1, Cons[2, Nil]]");
}

#[test]
fn test_structural_equality_distinct_shapes_differ() {
    // Same arity/elements but different field labels are NOT equal...
    quiver().evaluate("[[x: 1], [y: 1]] ~> ==").expect("[]");
    // ...nor are different tuple names.
    quiver().evaluate("[A[1], B[1]] ~> ==").expect("[]");
}
