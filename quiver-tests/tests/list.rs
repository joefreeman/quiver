mod common;
use common::*;

#[test]
fn test_new() {
    quiver().evaluate("%list.new").expect("Nil");
}

#[test]
fn test_prepend() {
    quiver()
        .evaluate(
            r#"
            %list.new [~, 10] %list.prepend [~, 20] %list.prepend
            "#,
        )
        .expect("Cons[20, Cons[10, Nil]]");
}

#[test]
fn test_head() {
    quiver().evaluate("%list.new %list.head").expect("[]");

    quiver()
        .evaluate(
            r#"
            %list.new [~, 10] %list.prepend [~, 20] %list.prepend %list.head
            "#,
        )
        .expect("20");
}

#[test]
fn test_tail() {
    quiver()
        .evaluate(
            r#"
            %list.new [~, 10] %list.prepend [~, 20] %list.prepend %list.tail
            "#,
        )
        .expect("Cons[10, Nil]");

    quiver()
        .evaluate(
            r#"
            %list.new [~, 10] %list.prepend %list.tail
            "#,
        )
        .expect("Nil");

    quiver().evaluate("%list.new %list.tail").expect("[]");
}

#[test]
fn test_is_empty() {
    quiver().evaluate("%list.new %list.empty?").expect("Ok");

    quiver()
        .evaluate("%list.new [~, 10] %list.prepend %list.empty?")
        .expect("[]");
}

#[test]
fn test_append() {
    quiver()
        .evaluate("%list.new [~, 10] %list.append")
        .expect("Cons[10, Nil]");

    quiver()
        .evaluate(
            r#"
            %list.new [~, 10] %list.append [~, 20] %list.append
            "#,
        )
        .expect("Cons[10, Cons[20, Nil]]");

    quiver()
        .evaluate(
            r#"
            %list.new [~, 10] %list.prepend [~, 20] %list.append
            "#,
        )
        .expect("Cons[10, Cons[20, Nil]]");
}

#[test]
fn test_reverse() {
    quiver().evaluate("%list.new %list.reverse").expect("Nil");

    quiver()
        .evaluate("%list.new [~, 10] %list.prepend %list.reverse")
        .expect("Cons[10, Nil]");

    quiver()
        .evaluate(
            r#"
            %list.new [~, 10] %list.prepend [~, 20] %list.prepend %list.reverse
            "#,
        )
        .expect("Cons[10, Cons[20, Nil]]");

    quiver()
        .evaluate(
            r#"
            %list.new [~, 10] %list.prepend [~, 20] %list.prepend [~, 30] %list.prepend %list.reverse
            "#,
        )
        .expect("Cons[10, Cons[20, Cons[30, Nil]]]");
}

#[test]
fn test_iter_collect() {
    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] %list.iter %list.collect")
        .expect("Cons[1, Cons[2, Cons[3, Nil]]]");
}
