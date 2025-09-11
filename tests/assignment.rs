mod common;
use common::*;
use quiver::vm::Value;

#[test]
fn test_simple_assignment() {
    quiver()
        .evaluate("x = 1, y = 2, [x, y] ~> <add>")
        .expect_int(3);
}

#[test]
fn test_tuple_destructuring() {
    quiver()
        .evaluate("[a, b] = [1, 2], [a, b] ~> <add>")
        .expect_int(3);
}

#[test]
fn test_named_field_assignment() {
    quiver().evaluate("A[a: a] = A[a: 1], a").expect_int(1);
}

#[test]
fn test_nested_field_assignment() {
    quiver()
        .evaluate("A[a: B[b: b]] = A[a: B[b: 2]], b")
        .expect_int(2);
}

#[test]
fn test_partial_tuple_assignment() {
    quiver()
        .evaluate("(x, y) = [x: 1, y: 2, z: 3], [x, y] ~> <add>")
        .expect_int(3);
}

#[test]
fn test_star_assignment() {
    quiver()
        .evaluate("* = [a: 1, b: 2], [a, b] ~> <add>")
        .expect_int(3);
}

#[test]
fn test_ignore_placeholder() {
    quiver().evaluate("[a, _] = [1, 2], a").expect_int(1);
}

#[test]
fn test_failed_assignment_length() {
    quiver().evaluate("[a] = [1, 2]").expect_nil();
}

#[test]
fn test_failed_assignment_on_mismatched_literal() {
    quiver().evaluate("a = 4, 3 = a").expect_nil();
}

#[test]
fn test_failed_assignment_on_mismatched_literal_in_tuple() {
    quiver().evaluate("[a, 3] = [1, 2]").expect_nil();
}

#[test]
fn test_failed_assignment_with_unrecognised_partial_field() {
    // TODO: specify error
    quiver().evaluate("(c) = [a: 1, b: 2]").expect_nil();
}

#[test]
fn test_failed_assignment_type() {
    // TODO: specify error
    quiver().evaluate("A[a] = B[1]").expect_nil();
}

#[test]
fn test_union_match_with_literals() {
    quiver()
        .evaluate(
            r#"
            2 ~> {
              | 1 = $ => 100
              | 2 = $ => 200
              | 3 = $ => 300
            }
            "#,
        )
        .expect_int(200)
}
#[test]
fn test_union_match_with_tuples() {
    quiver()
        .evaluate(
            r#"
            A[3] ~> {
              | A[x] = $ => [x, 1] ~> <add>
              | B[x] = $ => [x, 2] ~> <add>
            }
            "#,
        )
        .expect_int(4)
}

#[test]
fn test_union_type_partial_destructuring() {
    quiver()
        .evaluate(
            r#"
            f = #[a: int, b: int] {
              (a, b) = $ => [a, b]
            },
            [a: 1, b: 2] ~> f
            "#,
        )
        .expect_tuple(vec![Value::Integer(1), Value::Integer(2)]);

    quiver()
        .evaluate(
            r#"
            type union = [a: int, b: int] | [x: int];
            f = #union {
              (a, b) = $ => [a, b]
            },
            [a: 1, b: 2] ~> f
            "#,
        )
        .expect_tuple(vec![Value::Integer(1), Value::Integer(2)]);
}

#[test]
fn test_match_union_in_nested_tuple() {
    quiver()
        .evaluate(
            r#"
        type option = Some[int] | None;
        f = #[option, int] {
          | [None, z] = $ => 0
          | [Some[x], z] = $ => [x, z] ~> <add>
        },
        [Some[5], 2] ~> f
        "#,
        )
        .expect_int(7)
}

#[test]
fn test_multiple_placeholders() {
    quiver()
        .evaluate("[_, x, _, y, _] = [1, 2, 3, 4, 5], [x, y] ~> <add>")
        .expect_int(6);
}

#[test]
fn test_mixed_pattern_literal_and_binding() {
    quiver().evaluate("[1, x, 3] = [1, 2, 3], x").expect_int(2);
    quiver().evaluate("[1, x, 3] = [1, 2, 4]").expect_nil(); // Literal 3 doesn't match 4
}

#[test]
fn test_deeply_nested_tuple_pattern() {
    quiver()
        .evaluate(
            r#"
            A[B[C[x]]] = A[B[C[42]]], x
            "#,
        )
        .expect_int(42);
}

#[test]
fn test_empty_tuple_pattern() {
    quiver().evaluate("[] = [], 1").expect_int(1);
    quiver().evaluate("[] = [1]").expect_nil();
}

#[test]
fn test_string_literal_pattern() {
    // String literal matching with integer extraction
    quiver()
        .evaluate(r#"["hello", x] = ["hello", 123], x"#)
        .expect_int(123);

    quiver()
        .evaluate(r#"["hello", x] = ["goodbye", 123]"#)
        .expect_nil(); // String literal doesn't match
}

#[test]
fn test_complex_union_pattern_matching() {
    quiver()
        .evaluate(
            r#"
            type result = Ok[int] | Err[int];
            type option = Some[result] | None;

            Some[Ok[42]] ~> {
              | None = $ => 0
              | Some[Err[_]] = $ => -1
              | Some[Ok[x]] = $ => x
            }
            "#,
        )
        .expect_int(42);
}

#[test]
fn test_simple_partial_pattern() {
    // Test a simple partial pattern without unions first
    quiver().evaluate("(x, y) = [x: 1, y: 2], x").expect_int(1);
}

#[test]
fn test_partial_pattern_order_for_union() {
    quiver()
        .evaluate(
            r#"
            type union = A[x: int, y: int] | B[y: int, x: int]
            f = #union { (x, y) = $ => [x, y] }
            B[y: 1, x: 2] ~> f
            "#,
        )
        .expect_tuple(vec![Value::Integer(2), Value::Integer(1)]);
}
