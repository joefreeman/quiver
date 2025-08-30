mod common;

use common::*;
use quiver::vm::Value;

#[test]
fn test_integer_overflow_addition() {
    expect_error(&format!("[{}, 1] ~> +", i64::MAX));
    expect_error(&format!("[{}, -1] ~> +", i64::MIN));
}

#[test]
fn test_integer_overflow_multiplication() {
    expect_error(&format!("[{}, 2] ~> *", i64::MAX));
    expect_error(&format!("[{}, -2] ~> *", i64::MIN));
}

#[test]
fn test_integer_overflow_subtraction() {
    expect_error(&format!("[{}, -1] ~> -", i64::MIN));
    expect_error(&format!("[{}, 1] ~> -", i64::MAX));
}

#[test]
fn test_division_by_zero() {
    expect_error("[42, 0] ~> /");
    expect_error("[0, 0] ~> /");
    expect_error("[-17, 0] ~> /");
}

#[test]
fn test_modulo_by_zero() {
    expect_error("[42, 0] ~> %");
    expect_error("[0, 0] ~> %");
    expect_error("[-17, 0] ~> %");
}

#[test]
fn test_empty_tuple_operations() {
    expect_error("[] ~> +");
    expect_error("[] ~> -");
    expect_error("[] ~> *");
    expect_error("[] ~> /");
    expect_error("[] ~> %");
    expect_error("[] ~> ==");
    expect_error("[] ~> <");
    expect_error("[] ~> >");
}

#[test]
fn test_single_element_comparisons() {
    expect_error("[42] ~> ==");
    expect_error("[42] ~> <");
    expect_error("[42] ~> >");
    expect_error("[42] ~> <=");
    expect_error("[42] ~> >=");
    expect_error("[42] ~> !=");
}

#[test]
fn test_invalid_field_access() {
    expect_error("[1, 2, 3].10");
    expect_error("[].0");
    expect_error("42.field");
    expect_error("[x: 10].y");
}

#[test]
fn test_invalid_chained_field_access() {
    expect_error("42.field.subfield");
    expect_error("[1, 2].0.invalid");
    expect_error("[x: 5].y.z");
}

#[test]
fn test_function_arity_mismatch() {
    expect_error("f = #int { $ }; [1, 2] ~> f");
    expect_error("g = #[int, int] { [$0, $1] ~> + }; 42 ~> g");
    expect_error("h = #[int, int, int] { [$0, $1, $2] ~> + }; [1, 2] ~> h");
}

#[test]
fn test_invalid_pattern_matching() {
    expect_nil("[a, b] = [1]");
    expect_nil("[a, b, c] = [1, 2]");
    expect_nil("Point[x: a, y: b] = [1, 2]");
    expect_nil("[a, b] = Point[x: 1, y: 2]");
}

#[test]
fn test_invalid_destructuring_field_names() {
    expect_nil("(nonexistent) = X[a: 1, b: 2]");
    expect_nil("(missing, also_missing) = Y[present: 1]");
}

#[test]
fn test_recursive_function_without_base_case() {
    expect_error("infinite = #int { $ ~> & }; 5 ~> infinite");
}

#[test]
fn test_recursive_call_on_non_recursive_function() {
    expect_error("not_recursive = #int { $ }; 5 ~> not_recursive ~> &");
}

#[test]
fn test_maximum_recursion_depth() {
    expect_error("deep_recursion = #int { [$, 0] ~> == => 0 | [$, 1] ~> - ~> & }; 10000 ~> deep_recursion");
}

#[test]
fn test_cyclic_references() {
    expect_error("x = y; y = x; x");
}

#[test]
fn test_undefined_variable_access() {
    expect_error("undefined_var");
    expect_error("x = undefined_var; x");
}

#[test]
fn test_invalid_operator_application() {
    expect_error("42 ~> +");
    expect_error("\"hello\" ~> *");
    expect_error("[] ~> 42");
}

#[test]
fn test_type_errors_in_operations() {
    expect_error("[\"hello\", 42] ~> +");
    expect_error("[[1, 2], 3] ~> *");
    expect_error("[Point[x: 1, y: 2], 5] ~> /");
}

#[test]
fn test_invalid_tuple_construction() {
    expect_error("Point[x: 1, y: 2, z: 3, x: 4]"); // Duplicate field names
}

#[test]
fn test_malformed_binary_literals() {
    expect_error("'invalid_hex_g'");
    expect_error("'odd_length_he'");
    expect_error("''"); // Empty binary literal
}

#[test]
fn test_malformed_function_definitions() {
    expect_error("#invalid_param_type { $ }");
    expect_error("#[int int] { $ }"); // Missing comma
    expect_error("#[] { $ }"); // Empty parameter list with brackets
}

#[test]
fn test_invalid_arrow_conditionals() {
    expect_error("[] => 42 => 100"); // Multiple consecutive arrows
    expect_error("=> 42"); // Arrow without condition
}

#[test]
fn test_invalid_pipe_operations() {
    expect_error("| 42"); // Pipe without left operand
    expect_error("42 |"); // Pipe without right operand
}

#[test]
fn test_invalid_comma_sequences() {
    expect_error(", 42"); // Comma without left operand
    expect_error("42 ,"); // Comma without right operand
}

#[test]
fn test_nested_errors_propagation() {
    expect_error("[[5, 0] ~> /, 2] ~> +"); // Division by zero in nested operation
    expect_error("[undefined_var, 2] ~> *"); // Undefined variable in nested operation
}

#[test]
fn test_stack_overflow_prevention() {
    expect_error("x = [x]; x"); // Self-referential assignment
}

#[test]
fn test_memory_exhaustion_protection() {
    expect_error("huge_tuple = " + &"[1, ".repeat(100000) + "2" + &"]".repeat(100000));
}

#[test]
fn test_invalid_imports() {
    expect_error("import");
    expect_error("from");
    expect_error("import as");
    expect_error("from import");
    expect_error("import module.submodule.invalid");
}

#[test]
fn test_circular_import_detection() {
    // This would need module setup, but testing the error case
    expect_error("import self_referencing_module");
}

#[test]
fn test_unicode_edge_cases() {
    expect_binary("\"emoji: 🚀\"", 0);
    expect_binary("\"unicode: αβγ\"", 1);
    expect_int("emoji_var = 42; emoji_var", 42); // Unicode in variable names should work
}

#[test]
fn test_very_long_identifiers() {
    let long_name = "a".repeat(1000);
    expect_int(&format!("{} = 42; {}", long_name, long_name), 42);
}

#[test]
fn test_deeply_nested_structures() {
    let deep_tuple = "[".repeat(100) + "42" + &"]".repeat(100);
    expect_int(&format!("{}.{}", deep_tuple, "0.".repeat(99) + "0"), 42);
}

#[test]
fn test_whitespace_edge_cases() {
    expect_int("  42  ", 42);
    expect_int("\n\t42\n\t", 42);
    expect_int("/*comment*/42/*comment*/", 42);
}

#[test]
fn test_comment_edge_cases() {
    expect_int("42 /* nested /* comment */ */", 42);
    expect_int("/* comment at start */ 42", 42);
    expect_int("42 /* comment at end */", 42);
}

#[test]
fn test_operator_precedence_edge_cases() {
    expect_int("1 + 2 * 3", 7); // Should be 1 + (2 * 3) = 7
    expect_int("10 - 4 / 2", 8); // Should be 10 - (4 / 2) = 8
    expect_nil("1 < 2 == 3 < 4"); // Comparison chaining issues
}

#[test]
fn test_assignment_edge_cases() {
    expect_error("42 = x"); // Invalid left-hand side
    expect_error("[1, 2] = x, y"); // Invalid destructuring syntax
}

#[test]
fn test_function_definition_edge_cases() {
    expect_error("#{ }"); // Empty function body
    expect_error("#int int { $ }"); // Invalid parameter syntax
}

#[test]
fn test_ripple_operator_edge_cases() {
    expect_error("~~"); // Double ripple
    expect_error("42 ~"); // Incomplete ripple
}

#[test]
fn test_pattern_matching_edge_cases() {
    expect_nil("* = 42"); // Star destructuring on non-tuple
    expect_nil("(a, b, c) = Point[x: 1, y: 2]"); // Too many fields
}

#[test]
fn test_conditional_edge_cases() {
    expect_nil("[] => 42 | [] => 100 | 200"); // All conditions fail
    expect_int("42 => [] | 100", 100); // First condition passes but returns nil
}

#[test]
fn test_binary_operation_edge_cases() {
    expect_error("[1, 2, 3] ~> +"); // Multi-arity on wrong tuple size
    expect_error("[1] ~> =="); // Comparison needs two operands
}