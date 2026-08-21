use crate::common::*;

#[test]
fn test_new() {
    quiver().evaluate("%list.new []").expect("Nil");
}

#[test]
fn test_prepend() {
    quiver()
        .evaluate(
            r#"
            %list.new [] ~> [~, 10] ~> %list.prepend ~ ~> [~, 20] ~> %list.prepend ~
            "#,
        )
        .expect("Cons[20, Cons[10, Nil]]");
}

#[test]
fn test_head() {
    quiver()
        .evaluate("%list.new [] ~> %list.head ~")
        .expect("[]");

    quiver()
        .evaluate(
            r#"
            %list.new [] ~> [~, 10] ~> %list.prepend ~ ~> [~, 20] ~> %list.prepend ~ ~> %list.head ~
            "#,
        )
        .expect("20");
}

#[test]
fn test_tail() {
    quiver()
        .evaluate(
            r#"
            %list.new [] ~> [~, 10] ~> %list.prepend ~ ~> [~, 20] ~> %list.prepend ~ ~> %list.tail ~
            "#,
        )
        .expect("Cons[10, Nil]");

    quiver()
        .evaluate(
            r#"
            %list.new [] ~> [~, 10] ~> %list.prepend ~ ~> %list.tail ~
            "#,
        )
        .expect("Nil");

    quiver()
        .evaluate("%list.new [] ~> %list.tail ~")
        .expect("[]");
}

#[test]
fn test_is_empty() {
    quiver()
        .evaluate("%list.new [] ~> %list.empty? ~")
        .expect("Ok");

    quiver()
        .evaluate("%list.new [] ~> [~, 10] ~> %list.prepend ~ ~> %list.empty? ~")
        .expect("[]");
}

#[test]
fn test_append() {
    quiver()
        .evaluate("%list.new [] ~> [~, 10] ~> %list.append ~")
        .expect("Cons[10, Nil]");

    quiver()
        .evaluate(
            r#"
            %list.new [] ~> [~, 10] ~> %list.append ~ ~> [~, 20] ~> %list.append ~
            "#,
        )
        .expect("Cons[10, Cons[20, Nil]]");

    quiver()
        .evaluate(
            r#"
            %list.new [] ~> [~, 10] ~> %list.prepend ~ ~> [~, 20] ~> %list.append ~
            "#,
        )
        .expect("Cons[10, Cons[20, Nil]]");
}

#[test]
fn test_reverse() {
    quiver()
        .evaluate("%list.new [] ~> %list.reverse ~")
        .expect("Nil");

    quiver()
        .evaluate("%list.new [] ~> [~, 10] ~> %list.prepend ~ ~> %list.reverse ~")
        .expect("Cons[10, Nil]");

    quiver()
        .evaluate(
            r#"
            %list.new [] ~> [~, 10] ~> %list.prepend ~ ~> [~, 20] ~> %list.prepend ~ ~> %list.reverse ~
            "#,
        )
        .expect("Cons[10, Cons[20, Nil]]");

    quiver()
        .evaluate(
            r#"
            %list.new [] ~> [~, 10] ~> %list.prepend ~ ~> [~, 20] ~> %list.prepend ~ ~> [~, 30] ~> %list.prepend ~ ~> %list.reverse ~
            "#,
        )
        .expect("Cons[10, Cons[20, Cons[30, Nil]]]");
}

#[test]
fn test_iter_collect() {
    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.iter ~ ~> %list.collect ~")
        .expect("Cons[1, Cons[2, Cons[3, Nil]]]");
}

#[test]
fn test_map() {
    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.map [~, #{ %num.mul [$, 10] }]")
        .expect("Cons[10, Cons[20, Cons[30, Nil]]]");
}

#[test]
fn test_filter() {
    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.filter [~, #{ %num.gt? [$, 1]; $ }]")
        .expect("Cons[2, Cons[3, Nil]]");
}

#[test]
fn test_take() {
    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.take [~, 2]")
        .expect("Cons[1, Cons[2, Nil]]");

    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.take [~, 5]")
        .expect("Cons[1, Cons[2, Cons[3, Nil]]]");
}

#[test]
fn test_drop() {
    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.drop [~, 1]")
        .expect("Cons[2, Cons[3, Nil]]");

    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.drop [~, 5]")
        .expect("Nil");
}

#[test]
fn test_take_while() {
    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.take_while [~, #{ %num.lt? [$, 3]; $ }]")
        .expect("Cons[1, Cons[2, Nil]]");
}

#[test]
fn test_drop_while() {
    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.drop_while [~, #{ %num.lt? [$, 3]; $ }]")
        .expect("Cons[3, Nil]");
}

#[test]
fn test_flat_map() {
    quiver()
        .evaluate("Cons[1, Cons[2, Nil]] ~> %list.flat_map [~, #{ Cons[$, Cons[$, Nil]] }]")
        .expect("Cons[1, Cons[1, Cons[2, Cons[2, Nil]]]]");
}

#[test]
fn test_chain() {
    quiver()
        .evaluate("%list.chain [Cons[1, Cons[2, Nil]], Cons[3, Nil]]")
        .expect("Cons[1, Cons[2, Cons[3, Nil]]]");
}

#[test]
fn test_zip() {
    quiver()
        .evaluate("%list.zip [Cons[1, Cons[2, Nil]], Cons[10, Cons[20, Cons[30, Nil]]]]")
        .expect("Cons[[1, 10], Cons[[2, 20], Nil]]");
}

#[test]
fn test_enumerate() {
    quiver()
        .evaluate("Cons[10, Cons[20, Nil]] ~> %list.enumerate ~")
        .expect("Cons[[0, 10], Cons[[1, 20], Nil]]");
}

#[test]
fn test_intersperse() {
    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.intersperse [~, 0]")
        .expect("Cons[1, Cons[0, Cons[2, Cons[0, Cons[3, Nil]]]]]");
}

#[test]
fn test_fold() {
    quiver()
        .evaluate("%list.fold [Cons[1, Cons[2, Cons[3, Nil]]], 0, #{ %num.add [$0, $1] }]")
        .expect("6");
}

#[test]
fn test_count() {
    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.count ~")
        .expect("3");

    quiver()
        .evaluate("%list.new [] ~> %list.count ~")
        .expect("0");
}

#[test]
fn test_nth() {
    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.nth [~, 1]")
        .expect("2");

    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.nth [~, 5]")
        .expect("[]");
}

#[test]
fn test_find() {
    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.find [~, #{ %num.gt? [$, 1]; $ }]")
        .expect("2");

    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.find [~, #{ %num.gt? [$, 9]; $ }]")
        .expect("[]");
}

#[test]
fn test_find_index() {
    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.find_index [~, #{ %num.gt? [$, 1]; $ }]")
        .expect("1");

    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.find_index [~, #{ %num.gt? [$, 9]; $ }]")
        .expect("[]");
}

#[test]
fn test_any() {
    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.any? [~, #{ %num.gt? [$, 2]; $ }]")
        .expect("Ok");

    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.any? [~, #{ %num.gt? [$, 9]; $ }]")
        .expect("[]");
}

#[test]
fn test_all() {
    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.all? [~, #{ %num.gt? [$, 0]; $ }]")
        .expect("Ok");

    quiver()
        .evaluate("Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.all? [~, #{ %num.gt? [$, 1]; $ }]")
        .expect("[]");
}
