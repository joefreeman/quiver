mod common;
use common::*;

#[test]
fn test_map() {
    quiver()
        .evaluate(
            r#"
            inc = #'int { [~, 1] %num.add },
            Cons[1, Cons[2, Cons[3, Nil]]] %list.iter [~, &inc] %iter.map %list.collect
            "#,
        )
        .expect("Cons[2, Cons[3, Cons[4, Nil]]]");
}

#[test]
fn test_filter() {
    quiver()
        .evaluate(
            r#"
            even? = #'int { [~, 2] %int.mod =0 },
            Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]] %list.iter [~, &even?] %iter.filter %list.collect
            "#,
        )
        .expect("Cons[2, Cons[4, Nil]]");
}

#[test]
fn test_take() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]] %list.iter [~, 3] %iter.take %list.collect
            "#,
        )
        .expect("Cons[1, Cons[2, Cons[3, Nil]]]");
}

#[test]
fn test_drop() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]] %list.iter [~, 2] %iter.drop %list.collect
            "#,
        )
        .expect("Cons[3, Cons[4, Nil]]");
}

#[test]
fn test_take_while() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]] %list.iter [~, #'int { =3 <> }] %iter.take_while %list.collect
            "#,
        )
        .expect("Cons[1, Cons[2, Nil]]");
}

#[test]
fn test_drop_while() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]] %list.iter [~, #'int { =2 <> }] %iter.drop_while %list.collect
            "#,
        )
        .expect("Cons[2, Cons[3, Cons[4, Nil]]]");
}

#[test]
fn test_flat_map() {
    quiver()
        .evaluate(
            r#"
            f = #'int { Cons[~, Cons[~, Nil]] %list.iter },
            Cons[1, Cons[2, Cons[3, Nil]]] %list.iter [~, &f] %iter.flat_map %list.collect
            "#,
        )
        .expect("Cons[1, Cons[1, Cons[2, Cons[2, Cons[3, Cons[3, Nil]]]]]]");
}

#[test]
fn test_chain() {
    quiver()
        .evaluate(
            r#"
            xs = Cons[1, Cons[2, Cons[3, Nil]]] %list.iter,
            ys = Cons[4, Cons[5, Nil]] %list.iter,
            [&xs, &ys] %iter.chain %list.collect
            "#,
        )
        .expect("Cons[1, Cons[2, Cons[3, Cons[4, Cons[5, Nil]]]]]");
}

#[test]
fn test_zip() {
    quiver()
        .evaluate(
            r#"
            xs = Cons[1, Cons[2, Cons[3, Nil]]] %list.iter,
            ys = Cons[4, Cons[5, Nil]] %list.iter,
            [&xs, &ys] %iter.zip %list.collect
            "#,
        )
        .expect("Cons[[1, 4], Cons[[2, 5], Nil]]");
}

#[test]
fn test_enumerate() {
    quiver()
        .evaluate(
            r#"
            Cons[2, Cons[3, Cons[4, Nil]]] %list.iter %iter.enumerate %list.collect
            "#,
        )
        .expect("Cons[[0, 2], Cons[[1, 3], Cons[[2, 4], Nil]]]");
}

#[test]
fn test_cycle() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Nil]] %list.iter %iter.cycle [~, 5] %iter.take %list.collect
            "#,
        )
        .expect("Cons[1, Cons[2, Cons[1, Cons[2, Cons[1, Nil]]]]]");
}

#[test]
fn test_repeat() {
    quiver()
        .evaluate(
            r#"
            42 %iter.repeat [~, 3] %iter.take %list.collect
            "#,
        )
        .expect("Cons[42, Cons[42, Cons[42, Nil]]]");
}

#[test]
fn test_fold() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Nil]]] %list.iter [~, 0, &%num.add] %iter.fold
            "#,
        )
        .expect("6");
}

#[test]
fn test_find() {
    quiver()
        .evaluate(
            r#"
            Cons[10, Cons[20, Cons[30, Nil]]] %list.iter [~, #'int { [~, 15] %num.gt? }] %iter.find
            "#,
        )
        .expect("20");
}

#[test]
fn test_any() {
    quiver()
        .evaluate(
            r#"
            even? = #'int { [~, 2] %int.mod =0 },
            Cons[1, Cons[2, Cons[3, Nil]]] %list.iter [~, &even?] %iter.any?
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            even? = #'int { [~, 2] %int.mod =0 },
            Cons[1, Cons[3, Nil]] %list.iter [~, &even?] %iter.any?
            "#,
        )
        .expect("[]");

    quiver()
        .evaluate(
            r#"
            even? = #'int { [~, 2] %int.mod =0 },
            Nil %list.iter [~, &even?] %iter.any?
            "#,
        )
        .expect("[]");
}

#[test]
fn test_all() {
    quiver()
        .evaluate(
            r#"
            even? = #'int { [~, 2] %int.mod =0 },
            Cons[2, Cons[4, Cons[6, Nil]]] %list.iter [~, &even?] %iter.all?
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            even? = #'int { [~, 2] %int.mod =0 },
            Cons[1, Cons[3, Nil]] %list.iter [~, &even?] %iter.all?
            "#,
        )
        .expect("[]");

    quiver()
        .evaluate(
            r#"
            even? = #'int { [~, 2] %int.mod =0 },
            Nil %list.iter [~, &even?] %iter.all?
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_count() {
    quiver()
        .evaluate(
            r#"
            Cons[5, Cons[6, Cons[7, Nil]]] %list.iter %iter.count
            "#,
        )
        .expect("3");

    quiver()
        .evaluate(
            r#"
            Nil %list.iter %iter.count
            "#,
        )
        .expect("0");
}

#[test]
fn test_nth() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Nil]]] %list.iter [~, 1] %iter.nth
            "#,
        )
        .expect("2");

    quiver()
        .evaluate(
            r#"
            Cons[1, Nil] %list.iter [~, 1] %iter.nth
            "#,
        )
        .expect("[]");
}

#[test]
fn test_filter_long_skip_run_is_tail_recursive() {
    // Keeping only one late element forces ~50k consecutive skips through `filter_`'s advance
    // loop. That loop tail-calls the next iterator via `^~`, so it runs in constant stack; without
    // tail-call optimization this would overflow.
    quiver()
        .evaluate(
            r#"
            50000 %range.to %range.iter [~, #'int { =49999 }] %iter.filter [~, 0] %iter.nth
            "#,
        )
        .expect("49999");
}

#[test]
fn test_find_index() {
    quiver()
        .evaluate(
            r#"
            even? = #'int { [~, 2] %int.mod =0 },
            Cons[1, Cons[2, Cons[3, Nil]]] %list.iter [~, &even?] %iter.find_index
            "#,
        )
        .expect("1");

    quiver()
        .evaluate(
            r#"
            even? = #'int { [~, 2] %int.mod =0 },
            Cons[1, Nil] %list.iter [~, &even?] %iter.find_index
            "#,
        )
        .expect("[]");
}

#[test]
fn test_intersperse() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Nil]]] %list.iter [~, 0] %iter.intersperse %list.collect
            "#,
        )
        .expect("Cons[1, Cons[0, Cons[2, Cons[0, Cons[3, Nil]]]]]");

    quiver()
        .evaluate(
            r#"
            Cons[42, Nil] %list.iter [~, 0] %iter.intersperse %list.collect
            "#,
        )
        .expect("Cons[42, Nil]");

    quiver()
        .evaluate(
            r#"
            Nil %list.iter [~, 0] %iter.intersperse %list.collect
            "#,
        )
        .expect("Nil");
}
