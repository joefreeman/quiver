use crate::common::*;

#[test]
fn test_map() {
    quiver()
        .evaluate(
            r#"
            inc = #'int { [~, 1] ~> %num.add ~ };
            Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.iter ~ ~> [~, inc] ~> %iter.map ~ ~> %list.collect ~
            "#,
        )
        .expect("Cons[2, Cons[3, Cons[4, Nil]]]");
}

#[test]
fn test_filter() {
    quiver()
        .evaluate(
            r#"
            even? = #'int { [~, 2] ~> %int.mod ~ ~> =0 };
            Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]] ~> %list.iter ~ ~> [~, even?] ~> %iter.filter ~ ~> %list.collect ~
            "#,
        )
        .expect("Cons[2, Cons[4, Nil]]");
}

#[test]
fn test_take() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]] ~> %list.iter ~ ~> [~, 3] ~> %iter.take ~ ~> %list.collect ~
            "#,
        )
        .expect("Cons[1, Cons[2, Cons[3, Nil]]]");
}

#[test]
fn test_drop() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]] ~> %list.iter ~ ~> [~, 2] ~> %iter.drop ~ ~> %list.collect ~
            "#,
        )
        .expect("Cons[3, Cons[4, Nil]]");
}

#[test]
fn test_take_while() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]] ~> %list.iter ~ ~> [~, #'int { | =3 => [] | Ok }] ~> %iter.take_while ~ ~> %list.collect ~
            "#,
        )
        .expect("Cons[1, Cons[2, Nil]]");
}

#[test]
fn test_drop_while() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]] ~> %list.iter ~ ~> [~, #'int { | =2 => [] | Ok }] ~> %iter.drop_while ~ ~> %list.collect ~
            "#,
        )
        .expect("Cons[2, Cons[3, Cons[4, Nil]]]");
}

#[test]
fn test_flat_map() {
    quiver()
        .evaluate(
            r#"
            f = #'int { Cons[~, Cons[~, Nil]] ~> %list.iter ~ };
            Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.iter ~ ~> [~, f] ~> %iter.flat_map ~ ~> %list.collect ~
            "#,
        )
        .expect("Cons[1, Cons[1, Cons[2, Cons[2, Cons[3, Cons[3, Nil]]]]]]");
}

#[test]
fn test_chain() {
    quiver()
        .evaluate(
            r#"
            xs = Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.iter ~;
            ys = Cons[4, Cons[5, Nil]] ~> %list.iter ~;
            [xs, ys] ~> %iter.chain ~ ~> %list.collect ~
            "#,
        )
        .expect("Cons[1, Cons[2, Cons[3, Cons[4, Cons[5, Nil]]]]]");
}

#[test]
fn test_zip() {
    quiver()
        .evaluate(
            r#"
            xs = Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.iter ~;
            ys = Cons[4, Cons[5, Nil]] ~> %list.iter ~;
            [xs, ys] ~> %iter.zip ~ ~> %list.collect ~
            "#,
        )
        .expect("Cons[[1, 4], Cons[[2, 5], Nil]]");
}

#[test]
fn test_enumerate() {
    quiver()
        .evaluate(
            r#"
            Cons[2, Cons[3, Cons[4, Nil]]] ~> %list.iter ~ ~> %iter.enumerate ~ ~> %list.collect ~
            "#,
        )
        .expect("Cons[[0, 2], Cons[[1, 3], Cons[[2, 4], Nil]]]");
}

#[test]
fn test_cycle() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Nil]] ~> %list.iter ~ ~> %iter.cycle ~ ~> [~, 5] ~> %iter.take ~ ~> %list.collect ~
            "#,
        )
        .expect("Cons[1, Cons[2, Cons[1, Cons[2, Cons[1, Nil]]]]]");
}

#[test]
fn test_repeat() {
    quiver()
        .evaluate(
            r#"
            42 ~> %iter.repeat ~ ~> [~, 3] ~> %iter.take ~ ~> %list.collect ~
            "#,
        )
        .expect("Cons[42, Cons[42, Cons[42, Nil]]]");
}

#[test]
fn test_fold() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.iter ~ ~> [~, 0, %num.add] ~> %iter.fold ~
            "#,
        )
        .expect("6");
}

#[test]
fn test_find() {
    quiver()
        .evaluate(
            r#"
            Cons[10, Cons[20, Cons[30, Nil]]] ~> %list.iter ~ ~> [~, #'int { [~, 15] ~> %num.gt? ~ }] ~> %iter.find ~
            "#,
        )
        .expect("20");
}

#[test]
fn test_any() {
    quiver()
        .evaluate(
            r#"
            even? = #'int { [~, 2] ~> %int.mod ~ ~> =0 };
            Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.iter ~ ~> [~, even?] ~> %iter.any? ~
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            even? = #'int { [~, 2] ~> %int.mod ~ ~> =0 };
            Cons[1, Cons[3, Nil]] ~> %list.iter ~ ~> [~, even?] ~> %iter.any? ~
            "#,
        )
        .expect("[]");

    quiver()
        .evaluate(
            r#"
            even? = #'int { [~, 2] ~> %int.mod ~ ~> =0 };
            Nil ~> %list.iter ~ ~> [~, even?] ~> %iter.any? ~
            "#,
        )
        .expect("[]");
}

#[test]
fn test_all() {
    quiver()
        .evaluate(
            r#"
            even? = #'int { [~, 2] ~> %int.mod ~ ~> =0 };
            Cons[2, Cons[4, Cons[6, Nil]]] ~> %list.iter ~ ~> [~, even?] ~> %iter.all? ~
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            even? = #'int { [~, 2] ~> %int.mod ~ ~> =0 };
            Cons[1, Cons[3, Nil]] ~> %list.iter ~ ~> [~, even?] ~> %iter.all? ~
            "#,
        )
        .expect("[]");

    quiver()
        .evaluate(
            r#"
            even? = #'int { [~, 2] ~> %int.mod ~ ~> =0 };
            Nil ~> %list.iter ~ ~> [~, even?] ~> %iter.all? ~
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_count() {
    quiver()
        .evaluate(
            r#"
            Cons[5, Cons[6, Cons[7, Nil]]] ~> %list.iter ~ ~> %iter.count ~
            "#,
        )
        .expect("3");

    quiver()
        .evaluate(
            r#"
            Nil ~> %list.iter ~ ~> %iter.count ~
            "#,
        )
        .expect("0");
}

#[test]
fn test_nth() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.iter ~ ~> [~, 1] ~> %iter.nth ~
            "#,
        )
        .expect("2");

    quiver()
        .evaluate(
            r#"
            Cons[1, Nil] ~> %list.iter ~ ~> [~, 1] ~> %iter.nth ~
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
        // ~3s of pure compute standalone: headroom for a loaded machine, where the
        // default 5s has flaked under a full parallel suite run.
        .with_timeout(std::time::Duration::from_secs(20))
        .evaluate(
            r#"
            50000 ~> %range.to ~ ~> %range.iter ~ ~> [~, #'int { =49999 }] ~> %iter.filter ~ ~> [~, 0] ~> %iter.nth ~
            "#,
        )
        .expect("49999");
}

#[test]
fn test_find_index() {
    quiver()
        .evaluate(
            r#"
            even? = #'int { [~, 2] ~> %int.mod ~ ~> =0 };
            Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.iter ~ ~> [~, even?] ~> %iter.find_index ~
            "#,
        )
        .expect("1");

    quiver()
        .evaluate(
            r#"
            even? = #'int { [~, 2] ~> %int.mod ~ ~> =0 };
            Cons[1, Nil] ~> %list.iter ~ ~> [~, even?] ~> %iter.find_index ~
            "#,
        )
        .expect("[]");
}

#[test]
fn test_intersperse() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.iter ~ ~> [~, 0] ~> %iter.intersperse ~ ~> %list.collect ~
            "#,
        )
        .expect("Cons[1, Cons[0, Cons[2, Cons[0, Cons[3, Nil]]]]]");

    quiver()
        .evaluate(
            r#"
            Cons[42, Nil] ~> %list.iter ~ ~> [~, 0] ~> %iter.intersperse ~ ~> %list.collect ~
            "#,
        )
        .expect("Cons[42, Nil]");

    quiver()
        .evaluate(
            r#"
            Nil ~> %list.iter ~ ~> [~, 0] ~> %iter.intersperse ~ ~> %list.collect ~
            "#,
        )
        .expect("Nil");
}

#[test]
fn test_reference_predicate_keeps_element_type() {
    // Regression: a referenced predicate with a declared union result `('t | [])` used
    // to widen the iterator's element type with `| []` — the unifier's bound-variable
    // arm absorbed the result's nil variant (by widening) before the expected union's
    // own `[]` member was tried. A consumer demanding the exact element type then
    // failed with "[] is not ...", and generic consumers silently carried the widened
    // element. The strict `#'%iter<'int>` consumer here only compiles if the filtered
    // iterator's element stays exactly 'int.
    quiver()
        .evaluate(
            r#"
            pred = #'int -> ('int | []) { %num.gt? [$, 1]; $ }
            strict = #'%iter<'int> { %iter.count ~ }
            Cons[1, Cons[2, Nil]] ~> %list.iter ~ ~> %iter.filter [~, pred] ~> strict ~
            "#,
        )
        .expect("1");

    // The same shape with a tuple element (where the nil variant used to be paired
    // against the tuple member), collected by a consumer that demands the pair exactly.
    quiver()
        .evaluate(
            r#"
            'lst<'t> = Nil | Cons['t, ^]
            keep? = #[Str['bin], 'int] -> ([Str['bin], 'int] | []) { $ }
            from_it = #'%iter<[Str['bin], 'int]> {
              %iter.fold [~, Nil, #['lst<[Str['bin], 'int]>, [Str['bin], 'int]] { Cons[$1, $0] }]
            }
            Cons[["a", 1], Nil] ~> %list.iter ~ ~> %iter.filter [~, keep?] ~> from_it ~
            "#,
        )
        .expect(r#"Cons[["a", 1], Nil]"#);
}
