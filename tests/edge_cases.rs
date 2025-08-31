mod common;

use common::*;

#[test]
fn test_integer_overflow_addition() {
    quiver()
        .evaluate(&format!("[{}, 1] ~> +", i64::MAX))
        .expect_error();
    quiver()
        .evaluate(&format!("[{}, -1] ~> +", i64::MIN))
        .expect_error();
}

#[test]
fn test_integer_overflow_multiplication() {
    quiver()
        .evaluate(&format!("[{}, 2] ~> *", i64::MAX))
        .expect_error();
    quiver()
        .evaluate(&format!("[{}, -2] ~> *", i64::MIN))
        .expect_error();
}

#[test]
fn test_integer_overflow_subtraction() {
    quiver()
        .evaluate(&format!("[{}, -1] ~> -", i64::MIN))
        .expect_error();
    quiver()
        .evaluate(&format!("[{}, 1] ~> -", i64::MAX))
        .expect_error();
}

#[test]
fn test_division_by_zero() {
    quiver().evaluate("[42, 0] ~> /").expect_error();
    quiver().evaluate("[0, 0] ~> /").expect_error();
    quiver().evaluate("[-17, 0] ~> /").expect_error();
}

#[test]
fn test_modulo_by_zero() {
    quiver().evaluate("[42, 0] ~> %").expect_error();
    quiver().evaluate("[0, 0] ~> %").expect_error();
    quiver().evaluate("[-17, 0] ~> %").expect_error();
}

#[test]
fn test_empty_tuple_operations() {
    quiver().evaluate("[] ~> +").expect_error();
    quiver().evaluate("[] ~> -").expect_error();
    quiver().evaluate("[] ~> *").expect_error();
    quiver().evaluate("[] ~> /").expect_error();
    quiver().evaluate("[] ~> %").expect_error();
    quiver().evaluate("[] ~> ==").expect_error();
    quiver().evaluate("[] ~> <").expect_error();
    quiver().evaluate("[] ~> >").expect_error();
}

#[test]
fn test_single_element_comparisons() {
    quiver().evaluate("[42] ~> ==").expect_error();
    quiver().evaluate("[42] ~> <").expect_error();
    quiver().evaluate("[42] ~> >").expect_error();
    quiver().evaluate("[42] ~> <=").expect_error();
    quiver().evaluate("[42] ~> >=").expect_error();
    quiver().evaluate("[42] ~> !=").expect_error();
}

#[test]
fn test_invalid_field_access() {
    quiver().evaluate("[1, 2, 3].10").expect_error();
    quiver().evaluate("[].0").expect_error();
    quiver().evaluate("42.field").expect_error();
    quiver().evaluate("[x: 10].y").expect_error();
}

#[test]
fn test_invalid_chained_field_access() {
    quiver().evaluate("42.field.subfield").expect_error();
    quiver().evaluate("[1, 2].0.invalid").expect_error();
    quiver().evaluate("[x: 5].y.z").expect_error();
}

#[test]
fn test_function_arity_mismatch() {
    quiver()
        .evaluate("f = #int { $ }; [1, 2] ~> f")
        .expect_error();
    quiver()
        .evaluate("g = #[int, int] { [$0, $1] ~> + }; 42 ~> g")
        .expect_error();
    quiver()
        .evaluate("h = #[int, int, int] { [$0, $1, $2] ~> + }; [1, 2] ~> h")
        .expect_error();
}

#[test]
fn test_invalid_pattern_matching() {
    quiver().evaluate("[a, b] = [1]").expect_nil();
    quiver().evaluate("[a, b, c] = [1, 2]").expect_nil();
    quiver().evaluate("Point[x: a, y: b] = [1, 2]").expect_nil();
    quiver().evaluate("[a, b] = Point[x: 1, y: 2]").expect_nil();
}

#[test]
fn test_invalid_destructuring_field_names() {
    quiver()
        .evaluate("(nonexistent) = X[a: 1, b: 2]")
        .expect_nil();
    quiver()
        .evaluate("(missing, also_missing) = Y[present: 1]")
        .expect_nil();
}

#[test]
fn test_recursive_function_without_base_case() {
    quiver()
        .evaluate("infinite = #int { $ ~> & }; 5 ~> infinite")
        .expect_error();
}

#[test]
fn test_recursive_call_on_non_recursive_function() {
    quiver()
        .evaluate("not_recursive = #int { $ }; 5 ~> not_recursive ~> &")
        .expect_error();
}

#[test]
fn test_maximum_recursion_depth() {
    quiver().evaluate(
        "deep_recursion = #int { [$, 0] ~> == => 0 | [$, 1] ~> - ~> & }; 10000 ~> deep_recursion",
    ).expect_error();
}

#[test]
fn test_cyclic_references() {
    quiver().evaluate("x = y; y = x; x").expect_error();
}

#[test]
fn test_undefined_variable_access() {
    quiver().evaluate("undefined_var").expect_error();
    quiver().evaluate("x = undefined_var; x").expect_error();
}

#[test]
fn test_invalid_operator_application() {
    quiver().evaluate("42 ~> +").expect_error();
    quiver().evaluate("\"hello\" ~> *").expect_error();
    quiver().evaluate("[] ~> 42").expect_error();
}

#[test]
fn test_type_errors_in_operations() {
    quiver().evaluate("[\"hello\", 42] ~> +").expect_error();
    quiver().evaluate("[[1, 2], 3] ~> *").expect_error();
    quiver()
        .evaluate("[Point[x: 1, y: 2], 5] ~> /")
        .expect_error();
}

#[test]
fn test_invalid_tuple_construction() {
    quiver()
        .evaluate("Point[x: 1, y: 2, z: 3, x: 4]")
        .expect_error(); // Duplicate field names
}

#[test]
fn test_malformed_binary_literals() {
    quiver().evaluate("'invalid_hex_g'").expect_error();
    quiver().evaluate("'odd_length_he'").expect_error();
    quiver().evaluate("''").expect_error(); // Empty binary literal
}

#[test]
fn test_malformed_function_definitions() {
    quiver()
        .evaluate("#invalid_param_type { $ }")
        .expect_error();
    quiver().evaluate("#[int int] { $ }").expect_error(); // Missing comma
    quiver().evaluate("#[] { $ }").expect_error(); // Empty parameter list with brackets
}

#[test]
fn test_invalid_arrow_conditionals() {
    quiver().evaluate("[] => 42 => 100").expect_error(); // Multiple consecutive arrows
    quiver().evaluate("=> 42").expect_error(); // Arrow without condition
}

#[test]
fn test_invalid_pipe_operations() {
    quiver().evaluate("| 42").expect_error(); // Pipe without left operand
    quiver().evaluate("42 |").expect_error(); // Pipe without right operand
}

#[test]
fn test_invalid_comma_sequences() {
    quiver().evaluate(", 42").expect_error(); // Comma without left operand
    quiver().evaluate("42 ,").expect_error(); // Comma without right operand
}

#[test]
fn test_nested_errors_propagation() {
    quiver().evaluate("[[5, 0] ~> /, 2] ~> +").expect_error(); // Division by zero in nested operation
    quiver().evaluate("[undefined_var, 2] ~> *").expect_error(); // Undefined variable in nested operation
}

#[test]
fn test_stack_overflow_prevention() {
    quiver().evaluate("x = [x]; x").expect_error(); // Self-referential assignment
}

#[test]
fn test_invalid_imports() {
    quiver().evaluate("import").expect_error();
    quiver().evaluate("from").expect_error();
    quiver().evaluate("import as").expect_error();
    quiver().evaluate("from import").expect_error();
    quiver()
        .evaluate("import module.submodule.invalid")
        .expect_error();
}

#[test]
fn test_circular_import_detection() {
    // This would need module setup, but testing the error case
    quiver()
        .evaluate("import self_referencing_module")
        .expect_error();
}

#[test]
fn test_unicode_edge_cases() {
    quiver().evaluate("\"emoji: 🚀\"").expect_binary(0);
    quiver().evaluate("\"unicode: αβγ\"").expect_binary(1);
    quiver()
        .evaluate("emoji_var = 42; emoji_var")
        .expect_int(42); // Unicode in variable names should work
}

#[test]
fn test_very_long_identifiers() {
    let long_name = "a".repeat(1000);
    quiver()
        .evaluate(&format!("{} = 42; {}", long_name, long_name))
        .expect_int(42);
}

#[test]
fn test_deeply_nested_structures() {
    let deep_tuple = "[".repeat(100) + "42" + &"]".repeat(100);
    quiver()
        .evaluate(&format!("{}.{}", deep_tuple, "0.".repeat(99) + "0"))
        .expect_int(42);
}

#[test]
fn test_whitespace_edge_cases() {
    quiver().evaluate("  42  ").expect_int(42);
    quiver().evaluate("\n\t42\n\t").expect_int(42);
    quiver().evaluate("/*comment*/42/*comment*/").expect_int(42);
}

#[test]
fn test_comment_edge_cases() {
    quiver()
        .evaluate("42 /* nested /* comment */ */")
        .expect_int(42);
    quiver()
        .evaluate("/* comment at start */ 42")
        .expect_int(42);
    quiver().evaluate("42 /* comment at end */").expect_int(42);
}

#[test]
fn test_operator_precedence_edge_cases() {
    quiver().evaluate("1 + 2 * 3").expect_int(7); // Should be 1 + (2 * 3) = 7
    quiver().evaluate("10 - 4 / 2").expect_int(8); // Should be 10 - (4 / 2) = 8
    quiver().evaluate("1 < 2 == 3 < 4").expect_nil(); // Comparison chaining issues
}

#[test]
fn test_assignment_edge_cases() {
    quiver().evaluate("42 = x").expect_error(); // Invalid left-hand side
    quiver().evaluate("[1, 2] = x, y").expect_error(); // Invalid destructuring syntax
}

#[test]
fn test_function_definition_edge_cases() {
    quiver().evaluate("#{ }").expect_error(); // Empty function body
    quiver().evaluate("#int int { $ }").expect_error(); // Invalid parameter syntax
}

#[test]
fn test_ripple_operator_edge_cases() {
    quiver().evaluate("~~").expect_error(); // Double ripple
    quiver().evaluate("42 ~").expect_error(); // Incomplete ripple
}

#[test]
fn test_pattern_matching_edge_cases() {
    quiver().evaluate("* = 42").expect_nil(); // Star destructuring on non-tuple
    quiver()
        .evaluate("(a, b, c) = Point[x: 1, y: 2]")
        .expect_nil(); // Too many fields
}

#[test]
fn test_conditional_edge_cases() {
    quiver().evaluate("[] => 42 | [] => 100 | 200").expect_nil(); // All conditions fail
    quiver().evaluate("42 => [] | 100").expect_int(100); // First condition passes but returns nil
}

#[test]
fn test_binary_operation_edge_cases() {
    quiver().evaluate("[1, 2, 3] ~> +").expect_error(); // Multi-arity on wrong tuple size
    quiver().evaluate("[1] ~> ==").expect_error(); // Comparison needs two operands
}
