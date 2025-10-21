mod common;
use common::*;

#[test]
fn test_spread_simple() {
    quiver()
        .evaluate("a = A[x: 1, y: 2], [...a]")
        .expect("[x: 1, y: 2]");
}

#[test]
fn test_spread_with_field_replacement() {
    quiver()
        .evaluate("a = A[x: 1, y: 2], [...a, y: 3]")
        .expect("[x: 1, y: 3]");
}

#[test]
fn test_spread_with_field_addition() {
    quiver()
        .evaluate("a = A[x: 1, y: 2], [...a, z: 3]")
        .expect("[x: 1, y: 2, z: 3]");
}

#[test]
fn test_spread_with_unnamed_fields_appended() {
    quiver()
        .evaluate("a = A[x: 1, y: 2], [...a, 5, 6]")
        .expect("[x: 1, y: 2, 5, 6]");
}

#[test]
fn test_spread_with_unnamed_fields_prepended() {
    quiver()
        .evaluate("a = [x: 1, y: 2], [5, ...a]")
        .expect("[5, x: 1, y: 2]");
}

#[test]
fn test_spread_multiple_merges() {
    quiver()
        .evaluate("a = [y: 2], [x: 3] ~> [...a, ..., z: 4]")
        .expect("[y: 2, x: 3, z: 4]");
}

#[test]
fn test_spread_with_new_name() {
    quiver()
        .evaluate("a = A[x: 1, y: 2], B[...a, y: 3]")
        .expect("B[x: 1, y: 3]");
}

#[test]
fn test_spread_empty_tuple() {
    quiver().evaluate("a = [], [...a, x: 1]").expect("[x: 1]");
}

#[test]
fn test_spread_multiple_spreads() {
    quiver()
        .evaluate("a = [x: 1], b = [y: 2], [...a, ...b]")
        .expect("[x: 1, y: 2]");
}

#[test]
fn test_spread_with_overlapping_names() {
    quiver()
        .evaluate("a = [x: 1, y: 2], b = [y: 3, z: 4], [...a, ...b]")
        .expect("[x: 1, y: 3, z: 4]");
}

#[test]
fn test_spread_preserves_only_unnamed_fields() {
    quiver()
        .evaluate("a = [1, 2], b = [3, 4], [...a, ...b]")
        .expect("[1, 2, 3, 4]");
}

#[test]
fn test_spread_union_single_spread() {
    quiver()
        .evaluate(
            r#"
            type t = [x: int] | [y: int];
            f = #[v: t] { ~> =[v: v] => [z: 0, ...v] },
            f[v: [x: 1]]
            "#,
        )
        .expect("[z: 0, x: 1]");
}

#[test]
fn test_spread_union_cartesian_product() {
    quiver()
        .evaluate(
            r#"
            type ta = [x: int] | [y: int];
            type tb = [z: int] | [w: int];
            f = #[a: ta, b: tb] { ~> =[a: a, b: b] => [...a, ...b] },
            f[a: [x: 1], b: [z: 2]]
            "#,
        )
        .expect("[x: 1, z: 2]");
}

#[test]
fn test_spread_union_multiple_spreads() {
    quiver()
        .evaluate(
            r#"
            type ta = [x: int];
            type tb = [z: int] | [w: int];
            f = #[a: ta, b: tb] { ~> =[a: a, b: b] => [...a, ...b] },
            f[a: [x: 1], b: [w: 2]]
            "#,
        )
        .expect("[x: 1, w: 2]");
}

#[test]
fn test_spread_same_fields_different_sources() {
    quiver()
        .evaluate(
            r#"
            type ta = [x: int];
            type tb = [x: int, y: int] | [y: int];
            f = #[a: ta, b: tb] { ~> =[a: a, b: b] => [...a, ...b] },
            [f[a: [x: 1], b: [x: 2, y: 3]], f[a: [x: 4], b: [y: 5]]]
            "#,
        )
        .expect("[[x: 2, y: 3], [x: 4, y: 5]]");
}
