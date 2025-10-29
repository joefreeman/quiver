mod common;
use common::*;

#[test]
fn test_pin_simple() {
    quiver().evaluate("y = 2, 2 ~> ^y").expect("2");
    quiver().evaluate("y = 2, 3 ~> ^y").expect("[]");
}

#[test]
fn test_pin_in_tuple() {
    quiver()
        .evaluate("y = 2, Point[x, ^y] = Point[1, 2], x")
        .expect("1");
    quiver()
        .evaluate("y = 2, Point[x, ^y] = Point[1, 3]")
        .expect("[]");
}

#[test]
fn test_pin_with_term_syntax() {
    quiver()
        .evaluate("y = 2, Point[1, 2] ~> ^Point[=x, y], x")
        .expect("1");
    quiver()
        .evaluate("y = 2, Point[1, 3] ~> ^Point[=x, y]")
        .expect("[]");
}

#[test]
fn test_mixed_pin_and_bind() {
    quiver()
        .evaluate("y = 2, Point[1, 2] ~> ^Point[=x, y], x")
        .expect("1");
    quiver()
        .evaluate("y = 2, Point[x, ^y] = Point[1, 2], x")
        .expect("1");
}

#[test]
fn test_nested_pin_and_bind() {
    quiver()
        .evaluate("y = 2, A[1, B[2, C[3]]] ~> =A[x, ^B[y, =C[z]]], [x, z]")
        .expect("[1, 3]");
}

#[test]
fn test_pin_multiple_variables() {
    quiver()
        .evaluate("x = 1, y = 2, [1, 2] ~> ^[x, y]")
        .expect("[1, 2]");
    quiver()
        .evaluate("x = 1, y = 2, [1, 3] ~> ^[x, y]")
        .expect("[]");
}

#[test]
fn test_repeated_identifier_pin() {
    // Repeated identifier in pin mode - checks equality
    quiver().evaluate("[5, 5] ~> ^[x, x]").expect("[5, 5]");
    quiver().evaluate("[5, 6] ~> ^[x, x]").expect("[]");
}

#[test]
fn test_pin_against_partial() {
    quiver()
        .evaluate("x = 1, A[x: 1] ~> ^(x)")
        .expect("A[x: 1]");
    quiver().evaluate("x = 2, A[x: 1] ~> ^(x)").expect("[]");
    quiver().evaluate("A[x: 1] ~> ^(x)").expect_compile_error(
        quiver_compiler::compiler::Error::InternalError {
            message: "Pin variable 'x' not found in scope".to_string(),
        },
    );
}

#[test]
fn test_pin_with_variable_and_repetition() {
    // When variable exists and identifier is repeated, check both Variable and FieldEquality
    quiver()
        .evaluate("x = 5, [5, 5] ~> ^[x, x]")
        .expect("[5, 5]");
    quiver().evaluate("x = 5, [5, 6] ~> ^[x, x]").expect("[]"); // Fails field equality
    quiver().evaluate("x = 5, [4, 4] ~> ^[x, x]").expect("[]"); // Fails variable check
}

#[test]
fn test_pin_without_variable_single_occurrence() {
    // Pin with single occurrence and no variable should error
    quiver().evaluate("5 ~> ^x").expect_compile_error(
        quiver_compiler::compiler::Error::InternalError {
            message: "Pin variable 'x' not found in scope".to_string(),
        },
    );
}

#[test]
fn test_pin_from_outer_scope() {
    // Pin pattern should be able to reference variables from outer scopes
    quiver()
        .evaluate("x = 5, [] ~> #{ A[5] ~> ^A[x] }")
        .expect("A[5]");
    quiver()
        .evaluate("x = 5, [] ~> #{ A[6] ~> ^A[x] }")
        .expect("[]");
}

#[test]
fn test_pin_mixed_repeated_and_single() {
    quiver()
        .evaluate("[1, 1, 2] ~> ^[x, x, y]")
        .expect_compile_error(quiver_compiler::compiler::Error::InternalError {
            message: "Pin variable 'y' not found in scope".to_string(),
        });
}

// Type narrowing tests

#[test]
fn test_pin_int_type() {
    quiver().evaluate("42 ~> ^int").expect("42");
    quiver().evaluate("'ff' ~> ^int").expect("[]");
}

#[test]
fn test_pin_bin_type() {
    quiver().evaluate("'abcd' ~> ^bin").expect("'abcd'");
    quiver().evaluate("42 ~> ^bin").expect("[]");
}

#[test]
fn test_type_narrowing_in_blocks() {
    // Test type narrowing for int
    quiver()
        .evaluate("value = 42, value ~> { ~> ^int => \"is_int\" | \"is_bin\" }")
        .expect("\"is_int\"");

    // Test type narrowing for bin
    quiver()
        .evaluate("value = 'abcd', value ~> { ~> ^bin => \"is_bin\" | \"is_int\" }")
        .expect("\"is_bin\"");

    // Test type narrowing failure falls through
    quiver()
        .evaluate("value = 42, value ~> { ~> ^bin => \"is_bin\" | \"not_bin\" }")
        .expect("\"not_bin\"");
}

#[test]
fn test_type_narrowing_in_function() {
    quiver()
        .evaluate("1 ~> #(int | bin) { ~> ^int }")
        .expect_type("[] | int");
}

#[test]
fn test_narrowing_multiple_matching_variants() {
    // Multiple variants match the pattern structurally
    quiver()
        .evaluate("A[1] ~> #(A[int] | A[bin] | B[int]) { ~> =A[x] => x }")
        .expect_type("[] | bin | int");
}

#[test]
fn test_narrowing_no_matching_variants() {
    // No variants match - should return just []
    quiver()
        .evaluate("A[1] ~> #(A[int] | B[int]) { ~> =C[x] }")
        .expect_type("[]");
}

#[test]
fn test_narrowing_all_variants_match() {
    // All variants match structurally, but runtime checks can still fail
    quiver()
        .evaluate("A[1] ~> #(A[int] | A[bin]) { ~> =A[x] => x }")
        .expect_type("[] | bin | int");
}

#[test]
fn test_narrowing_nested_field() {
    // Pattern narrows based on nested tuple types
    quiver()
        .evaluate("X[A[1]] ~> #(X[A[int]] | X[B[int]]) { ~> ^X[A[int]] }")
        .expect_type("[] | X[A[int]]");
}

#[test]
fn test_narrowing_with_wildcard() {
    // Wildcards match anything - only outer structure matters
    quiver()
        .evaluate("A[1, 'ff'] ~> #(A[int, bin] | B[int, bin]) { ~> ^A[_, _] }")
        .expect_type("[] | A[int, bin]");
}

#[test]
fn test_narrowing_with_literal() {
    // Literal pattern should narrow to only matching variant
    quiver()
        .evaluate("A[1] ~> #(A[int] | B[int]) { ~> ^A[1] }")
        .expect_type("[] | A[int]");
}

#[test]
fn test_narrowing_repeated_identifiers() {
    // Repeated identifier requires both structural match and runtime equality
    quiver()
        .evaluate(
            r#"
            A[1, 1] ~> #(A[int, int] | A[int, bin] | B[int, int]) {
              ~> =A[x, x] => x
            }
            "#,
        )
        .expect_type("[] | int");
}

#[test]
fn test_narrowing_partial_types() {
    // Should work with partial types
    quiver()
        .evaluate("A[x: 1] ~> #(A[x: int] | B[x: int]) { ~> ^A[x: int] }")
        .expect_type("[] | A[x: int]");
}

#[test]
fn test_narrowing_type_and_variable_pin() {
    // Combines structural narrowing with runtime variable check
    quiver()
        .evaluate("y = 2, A[2] ~> #(A[int] | B[int]) { ~> ^A[y] }")
        .expect_type("[] | A[int]");
}

#[test]
fn test_narrowing_nested_union_in_field() {
    quiver()
        .evaluate("A[1] ~> #(A[int | bin] | B[int]) { ~> ^A[(int | bin)] }")
        .expect_type("[] | A[(bin | int)]");
}

#[test]
fn test_narrowing_with_branches() {
    quiver()
        .evaluate("A ~> #(A | B | C) { ~> ^(A | B) => ~> ^A }")
        .expect_type("[] | A");

    quiver()
        .evaluate("A ~> #(A | B | C) { ~> ^(A | B) => ~> ^A | X }")
        .expect_type("[] | A | X");

    quiver()
        .evaluate("A ~> #(A | B | C) { ~> ^(A | B) => 1 | X }")
        .expect_type("int | X");
}

#[test]
fn test_narrowing_star_pattern() {
    // Star matches everything - no narrowing, no failure possible
    quiver()
        .evaluate("A[1] ~> #(A[int] | B[int]) { ~> ^_ => 1 }")
        .expect_type("int");
}

#[test]
fn test_narrowing_with_type_alias() {
    // Type alias should work for narrowing
    quiver()
        .evaluate(
            r#"
            a : A[int, int];
            A[1, 2] ~> #(A[int, int] | B[int, int]) { ~> ^a }
            "#,
        )
        .expect_type("[] | A[int, int]");
}

#[test]
fn test_narrowing_generic_type() {
    // Parameterized types should narrow correctly
    quiver()
        .evaluate(
            r#"
            box<t> : Box[t];
            Box[A[1]] ~> #(Box[A[int]] | Box[B[int]]) { ~> ^box<A[int]> }
            "#,
        )
        .expect_type("[] | Box[A[int]]");
}

#[test]
fn test_narrowing_preserves_field_types() {
    // Narrowing should preserve the exact field types from matching variants
    quiver()
        .evaluate("A[1] ~> #(A[int] | B[int]) { ~> ^A[1] => 1 }")
        .expect_type("[] | int");
}

#[test]
fn test_narrowing_complex_nested_pattern() {
    // Complex nested pattern with multiple levels
    quiver()
        .evaluate(
            r#"
            X[Y[A[1]]] ~> #(X[Y[A[int]]] | X[Y[B[int]]] | X[Z[A[int]]]) {
                ~> ^X[Y[A[int]]]
            }
            "#,
        )
        .expect_type("[] | X[Y[A[int]]]");
}

#[test]
fn test_narrowing_in_block_branches() {
    // Type narrowing in block branches
    quiver()
        .evaluate(
            r#"
            value = A[1],
            value ~> #(A[int] | B[int]) {
              | ~> =A[x] => x
              | ~> =B[x] => x
              | 0
            }
            "#,
        )
        .expect_type("int");
}
