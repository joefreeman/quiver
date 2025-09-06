mod common;
use common::*;

#[test]
fn test_simple_assignment() {
    quiver().evaluate("x = 1, y = 2, [x, y] ~> +").expect_int(3);
}

#[test]
fn test_tuple_destructuring() {
    quiver()
        .evaluate("[a, b] = [1, 2], [a, b] ~> +")
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
        .evaluate("(x, y) = [x: 1, y: 2, z: 3], [x, y] ~> +")
        .expect_int(3);
}

#[test]
fn test_star_assignment() {
    quiver()
        .evaluate("* = [a: 1, b: 2], [a, b] ~> +")
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
              | A[x] = $ => [x, 1] ~> +
              | B[x] = $ => [x, 2] ~> +
            }
            "#,
        )
        .expect_int(4)
}

// #[test]
// fn test_failed_union_match() {
//     quiver()
//         .evaluate(
//             r#"
//             C[3] ~> {
//               | A[x] = $ => [x, 1] ~> +
//               | B[x] = $ => [x, 2] ~> +
//             }
//             "#,
//         )
//         .expect_nil()
// }

// #[test]
// fn test_variable_not_assigned_if_no_match() {
//     quiver()
//         .evaluate("A[a: [x, 3]] = A[a: [1, 2]], x")
//         .expect_nil();
// }
