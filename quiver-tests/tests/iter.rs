mod common;
use common::*;

#[test]
fn test_map() {
    quiver()
        .evaluate(
            r#"
            inc = #int { %math.add[~, 1] },
            Cons[1, Cons[2, Cons[3, Nil]]]
            ~> %list.iter
            ~> %iter.map[~, &inc]
            ~> %list.collect
            "#,
        )
        .expect("Cons[2, Cons[3, Cons[4, Nil]]]");
}

#[test]
fn test_filter() {
    quiver()
        .evaluate(
            r#"
            even? = #int { %math.mod[~, 2] ~> =0 },
            Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]]
            ~> %list.iter
            ~> %iter.filter[~, &even?]
            ~> %list.collect
            "#,
        )
        .expect("Cons[2, Cons[4, Nil]]");
}

#[test]
fn test_take() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]]
            ~> %list.iter
            ~> %iter.take[~, 3]
            ~> %list.collect
            "#,
        )
        .expect("Cons[1, Cons[2, Cons[3, Nil]]]");
}

#[test]
fn test_drop() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]]
            ~> %list.iter
            ~> %iter.drop[~, 2]
            ~> %list.collect
            "#,
        )
        .expect("Cons[3, Cons[4, Nil]]");
}

#[test]
fn test_take_while() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]]
            ~> %list.iter
            ~> %iter.take_while[~, #int { =3 ~> / }]
            ~> %list.collect
            "#,
        )
        .expect("Cons[1, Cons[2, Nil]]");
}

#[test]
fn test_drop_while() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]]
            ~> %list.iter
            ~> %iter.drop_while[~, #int { =2 ~> / }]
            ~> %list.collect
            "#,
        )
        .expect("Cons[2, Cons[3, Cons[4, Nil]]]");
}

#[test]
fn test_flat_map() {
    quiver()
        .evaluate(
            r#"
            f = #int { Cons[~, Cons[~, Nil]] ~> %list.iter },
            Cons[1, Cons[2, Cons[3, Nil]]]
            ~> %list.iter
            ~> %iter.flat_map[~, &f]
            ~> %list.collect
            "#,
        )
        .expect("Cons[1, Cons[1, Cons[2, Cons[2, Cons[3, Cons[3, Nil]]]]]]");
}

#[test]
fn test_chain() {
    quiver()
        .evaluate(
            r#"
            xs = Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.iter,
            ys = Cons[4, Cons[5, Nil]] ~> %list.iter,
            [&xs, &ys] ~> %iter.chain ~> %list.collect
            "#,
        )
        .expect("Cons[1, Cons[2, Cons[3, Cons[4, Cons[5, Nil]]]]]");
}

#[test]
fn test_zip() {
    quiver()
        .evaluate(
            r#"
            xs = Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.iter,
            ys = Cons[4, Cons[5, Nil]] ~> %list.iter,
            [&xs, &ys] ~> %iter.zip ~> %list.collect
            "#,
        )
        .expect("Cons[[1, 4], Cons[[2, 5], Nil]]");
}

#[test]
fn test_enumerate() {
    quiver()
        .evaluate(
            r#"
            Cons[2, Cons[3, Cons[4, Nil]]]
            ~> %list.iter
            ~> %iter.enumerate
            ~> %list.collect
            "#,
        )
        .expect("Cons[[0, 2], Cons[[1, 3], Cons[[2, 4], Nil]]]");
}

#[test]
fn test_cycle() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Nil]]
            ~> %list.iter
            ~> %iter.cycle
            ~> %iter.take[~, 5]
            ~> %list.collect
            "#,
        )
        .expect("Cons[1, Cons[2, Cons[1, Cons[2, Cons[1, Nil]]]]]");
}

#[test]
fn test_repeat() {
    quiver()
        .evaluate(
            r#"
            42
            ~> %iter.repeat
            ~> %iter.take[~, 3]
            ~> %list.collect
            "#,
        )
        .expect("Cons[42, Cons[42, Cons[42, Nil]]]");
}

#[test]
fn test_fold() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Nil]]]
            ~> %list.iter
            ~> %iter.fold[~, 0, &%math.add]
            "#,
        )
        .expect("6");
}

#[test]
fn test_find() {
    quiver()
        .evaluate(
            r#"
            Cons[10, Cons[20, Cons[30, Nil]]]
            ~> %list.iter
            ~> %iter.find[~, #int { %math.gt[~, 15] }]
            "#,
        )
        .expect("20");
}

#[test]
fn test_any() {
    quiver()
        .evaluate(
            r#"
            even? = #int { %math.mod[~, 2] ~> =0 },
            Cons[1, Cons[2, Cons[3, Nil]]]
            ~> %list.iter
            ~> %iter.any?[~, &even?]
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            even? = #int { %math.mod[~, 2] ~> =0 },
            Cons[1, Cons[3, Nil]]
            ~> %list.iter
            ~> %iter.any?[~, &even?]
            "#,
        )
        .expect("[]");

    quiver()
        .evaluate(
            r#"
            even? = #int { %math.mod[~, 2] ~> =0 },
            Nil
            ~> %list.iter
            ~> %iter.any?[~, &even?]
            "#,
        )
        .expect("[]");
}

#[test]
fn test_all() {
    quiver()
        .evaluate(
            r#"
            even? = #int { %math.mod[~, 2] ~> =0 },
            Cons[2, Cons[4, Cons[6, Nil]]]
            ~> %list.iter
            ~> %iter.all?[~, &even?]
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            even? = #int { %math.mod[~, 2] ~> =0 },
            Cons[1, Cons[3, Nil]]
            ~> %list.iter
            ~> %iter.all?[~, &even?]
            "#,
        )
        .expect("[]");

    quiver()
        .evaluate(
            r#"
            even? = #int { %math.mod[~, 2] ~> =0 },
            Nil
            ~> %list.iter
            ~> %iter.all?[~, &even?]
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_count() {
    quiver()
        .evaluate(
            r#"
            Cons[5, Cons[6, Cons[7, Nil]]]
            ~> %list.iter
            ~> %iter.count
            "#,
        )
        .expect("3");

    quiver()
        .evaluate(
            r#"
            Nil
            ~> %list.iter
            ~> %iter.count
            "#,
        )
        .expect("0");
}

#[test]
fn test_nth() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Nil]]]
            ~> %list.iter
            ~> %iter.nth[~, 1]
            "#,
        )
        .expect("2");

    quiver()
        .evaluate(
            r#"
            Cons[1, Nil]
            ~> %list.iter
            ~> %iter.nth[~, 1]
            "#,
        )
        .expect("[]");
}

#[test]
fn test_find_index() {
    quiver()
        .evaluate(
            r#"
            even? = #int { %math.mod[~, 2] ~> =0 },
            Cons[1, Cons[2, Cons[3, Nil]]]
            ~> %list.iter
            ~> %iter.find_index[~, &even?]
            "#,
        )
        .expect("1");

    quiver()
        .evaluate(
            r#"
            even? = #int { %math.mod[~, 2] ~> =0 },
            Cons[1, Nil]
            ~> %list.iter
            ~> %iter.find_index[~, &even?]
            "#,
        )
        .expect("[]");
}

#[test]
fn test_intersperse() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Nil]]]
            ~> %list.iter
            ~> %iter.intersperse[~, 0]
            ~> %list.collect
            "#,
        )
        .expect("Cons[1, Cons[0, Cons[2, Cons[0, Cons[3, Nil]]]]]");

    quiver()
        .evaluate(
            r#"
            Cons[42, Nil]
            ~> %list.iter
            ~> %iter.intersperse[~, 0]
            ~> %list.collect
            "#,
        )
        .expect("Cons[42, Nil]");

    quiver()
        .evaluate(
            r#"
            Nil
            ~> %list.iter
            ~> %iter.intersperse[~, 0]
            ~> %list.collect
            "#,
        )
        .expect("Nil");
}
