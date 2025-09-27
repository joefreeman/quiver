mod common;
use common::*;

#[test]
fn test_simple_assignment() {
    quiver()
        .evaluate("1 ~> x, 2 ~> y, [x, y] ~> <add>")
        .expect("3");
}

#[test]
fn test_tuple_destructuring() {
    quiver()
        .evaluate("[1, 2] ~> [a, b], [a, b] ~> <add>")
        .expect("3");
}

#[test]
fn test_named_field_assignment() {
    quiver().evaluate("A[a: 1] ~> A[a: a], a").expect("1");
}

#[test]
fn test_nested_field_assignment() {
    quiver()
        .evaluate("A[a: B[b: 2]] ~> A[a: B[b: b]], b")
        .expect("2");
}

#[test]
fn test_partial_tuple_assignment() {
    quiver()
        .evaluate("[x: 1, y: 2, z: 3] ~> (x, y), [x, y] ~> <add>")
        .expect("3");
}

#[test]
fn test_star_assignment() {
    quiver()
        .evaluate("[a: 1, b: 2] ~> *, [a, b] ~> <add>")
        .expect("3");
}

#[test]
fn test_ignore_placeholder() {
    quiver().evaluate("[1, 2] ~> [a, _], a").expect("1");
}

#[test]
fn test_failed_assignment_length() {
    quiver().evaluate("[1, 2] ~> [a]").expect("[]");
}

#[test]
fn test_failed_assignment_on_mismatched_literal() {
    quiver().evaluate("4 ~> a, a ~> 3").expect("[]");
}

#[test]
fn test_failed_assignment_on_mismatched_literal_in_tuple() {
    quiver().evaluate("[1, 2] ~> [a, 3]").expect("[]");
}

#[test]
fn test_failed_assignment_with_unrecognised_partial_field() {
    // TODO: specify error
    quiver().evaluate("[a: 1, b: 2] ~> (c)").expect("[]");
}

#[test]
fn test_failed_assignment_type() {
    // TODO: specify error
    quiver().evaluate("B[1] ~> A[a]").expect("[]");
}

#[test]
fn test_union_match_with_literals() {
    quiver()
        .evaluate(
            r#"
            2 ~> {
              | 1 => 100
              | 2 => 200
              | 3 => 300
            }
            "#,
        )
        .expect("200")
}
#[test]
fn test_union_match_with_tuples() {
    quiver()
        .evaluate(
            r#"
            A[3] ~> {
              | A[x] => [x, 1] ~> <add>
              | B[x] => [x, 2] ~> <add>
            }
            "#,
        )
        .expect("4")
}

#[test]
fn test_union_type_partial_destructuring() {
    quiver()
        .evaluate(
            r#"
            #[a: int, b: int] {
              (a, b) => [a, b]
            } ~> f,
            [a: 1, b: 2] ~> f!
            "#,
        )
        .expect("[1, 2]");

    quiver()
        .evaluate(
            r#"
            type union = [a: int, b: int] | [x: int];
            #union {
              (a, b) => [a, b]
            } ~> f,
            [a: 1, b: 2] ~> f!
            "#,
        )
        .expect("[1, 2]");
}

#[test]
fn test_match_union_in_nested_tuple() {
    quiver()
        .evaluate(
            r#"
            type option = Some[int] | None;
            #[option, int] {
              | [None, z] => 0
              | [Some[x], z] => [x, z] ~> <add>
            } ~> f,
            [Some[5], 2] ~> f!
            "#,
        )
        .expect("7")
}

#[test]
fn test_multiple_placeholders() {
    quiver()
        .evaluate("[1, 2, 3, 4, 5] ~> [_, x, _, y, _], [x, y] ~> <add>")
        .expect("6");
}

#[test]
fn test_mixed_pattern_literal_and_binding() {
    quiver().evaluate("[1, 2, 3] ~> [1, x, 3], x").expect("2");
    quiver().evaluate("[1, 2, 4] ~> [1, x, 3]").expect("[]"); // Literal 4 doesn't match 3
}

#[test]
fn test_deeply_nested_tuple_pattern() {
    quiver()
        .evaluate("A[B[C[42]]] ~> A[B[C[x]]], x")
        .expect("42");
}

#[test]
fn test_empty_tuple_pattern() {
    quiver().evaluate("[] ~> []").expect("Ok");
    quiver().evaluate("[1] ~> []").expect("[]");
}

#[test]
fn test_string_literal_pattern() {
    // String literal matching with integer extraction
    quiver()
        .evaluate(r#"["hello", 123] ~> ["hello", x], x"#)
        .expect("123");

    quiver()
        .evaluate(r#"["goodbye", 123] ~> ["hello", x]"#)
        .expect("[]"); // String literal doesn't match
}

#[test]
fn test_complex_union_pattern_matching() {
    quiver()
        .evaluate(
            r#"
            type result = Ok[int] | Err[int];
            type option = Some[result] | None;

            Some[Ok[42]] ~> {
              | None => 0
              | Some[Err[_]] => -1
              | Some[Ok[x]] => x
            }
            "#,
        )
        .expect("42");
}

#[test]
fn test_simple_partial_pattern() {
    // Test a simple partial pattern without unions first
    quiver().evaluate("[x: 1, y: 2] ~> (x, y), x").expect("1");
}

#[test]
fn test_partial_pattern_order_for_union() {
    quiver()
        .evaluate(
            r#"
            type union = A[x: int, y: int] | B[y: int, x: int]
            #Wrapper[union] { Wrapper[(x, y)] => [x, y] } ~> f
            Wrapper[B[y: 1, x: 2]] ~> f!
            "#,
        )
        .expect("[2, 1]");
}

#[test]
fn test_star_pattern_order_for_union() {
    quiver()
        .evaluate(
            r#"
            type union = A[x: int, y: int] | B[y: int, x: int]
            #union { * => [x, y] } ~> f
            B[y: 1, x: 2] ~> f!
            "#,
        )
        .expect("[2, 1]");
}
