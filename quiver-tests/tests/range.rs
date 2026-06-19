mod common;
use common::*;

#[test]
fn test_range_to() {
    quiver()
        .evaluate(
            r#"
            5
            ~> %range.to
            ~> %range.iter
            ~> %list.collect
            "#,
        )
        .expect("Cons[0, Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]]]");
}

#[test]
fn test_range_from() {
    quiver()
        .evaluate(
            r#"
            10
            ~> %range.from
            ~> %range.iter
            ~> %iter.take [~, 5]
            ~> %list.collect
            "#,
        )
        .expect("Cons[10, Cons[11, Cons[12, Cons[13, Cons[14, Nil]]]]]");
}

#[test]
fn test_range_between() {
    quiver()
        .evaluate(
            r#"
            [3, 8]
            ~> %range.between
            ~> %range.iter
            ~> %list.collect
            "#,
        )
        .expect("Cons[3, Cons[4, Cons[5, Cons[6, Cons[7, Nil]]]]]");
}

#[test]
fn test_range_new_with_step() {
    quiver()
        .evaluate(
            r#"
            [0, 10, 2]
            ~> %range.new
            ~> %range.iter
            ~> %list.collect
            "#,
        )
        .expect("Cons[0, Cons[2, Cons[4, Cons[6, Cons[8, Nil]]]]]");
}

#[test]
fn test_range_new_negative_step() {
    quiver()
        .evaluate(
            r#"
            [10, 5, -1]
            ~> %range.new
            ~> %range.iter
            ~> %list.collect
            "#,
        )
        .expect("Cons[10, Cons[9, Cons[8, Cons[7, Cons[6, Nil]]]]]");
}

#[test]
fn test_range_empty() {
    quiver()
        .evaluate(
            r#"
            [5, 5, 1]
            ~> %range.new
            ~> %range.iter
            ~> %list.collect
            "#,
        )
        .expect("Nil");
}

#[test]
fn test_range_infinite() {
    quiver()
        .evaluate(
            r#"
            [0, None, 3]
            ~> %range.new
            ~> %range.iter
            ~> %iter.take [~, 4]
            ~> %list.collect
            "#,
        )
        .expect("Cons[0, Cons[3, Cons[6, Cons[9, Nil]]]]");
}
