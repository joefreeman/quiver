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
fn test_type_narrowing_in_tuples() {
    // Type narrowing on tuple field
    quiver()
        .evaluate("data = Data[42], data ~> =Data[^int], Ok")
        .expect("Ok");

    quiver()
        .evaluate("data = Data['abcd'], data ~> =Data[^int]")
        .expect("[]");

    quiver()
        .evaluate("data = Data['abcd'], data ~> =Data[^bin], Ok")
        .expect("Ok");
}

#[test]
fn test_type_narrowing_mixed_list() {
    // Test type narrowing on mixed int/bin values
    quiver()
        .evaluate("value = 42, value ~> { ~> ^int => value | 0 }")
        .expect("42");

    quiver()
        .evaluate("value = 'abcd', value ~> { ~> ^int => value | 0 }")
        .expect("0");

    quiver()
        .evaluate("value = 'abcd', value ~> { ~> ^bin => value | 'ff' }")
        .expect("'abcd'");

    quiver()
        .evaluate("value = 42, value ~> { ~> ^bin => value | 'ff' }")
        .expect("'ff'");
}

#[test]
fn test_reserved_names_still_reserved_in_bind_mode() {
    // int and bin should still be reserved as variable names
    quiver().evaluate("int = 42").expect_compile_error(
        quiver_compiler::compiler::Error::TypeUnresolved(
            "Cannot use reserved primitive type 'int' as a variable name".to_string(),
        ),
    );

    quiver().evaluate("bin = 'abcd'").expect_compile_error(
        quiver_compiler::compiler::Error::TypeUnresolved(
            "Cannot use reserved primitive type 'bin' as a variable name".to_string(),
        ),
    );
}
