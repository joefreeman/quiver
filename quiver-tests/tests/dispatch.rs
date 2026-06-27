mod common;
use common::*;

// A function that dispatches on its parameter records a per-branch case table, so a call
// with a concrete argument type infers the result type of just the reachable branch(es)
// rather than the whole union of branch results.

const ADD: &str = r#"
'n = 'int | Rational['int, 'int];
radd_ = #[Rational['int, 'int], Rational['int, 'int]] {
  =[Rational[a, b], Rational[c, d]],
  Rational[[[a, d] ~> __integer_multiply__, [c, b] ~> __integer_multiply__] ~> __integer_add__, [b, d] ~> __integer_multiply__]
};
tr_ = #'n { | =Rational[n, d] => Rational[n, d] | =n => Rational[n, 1] };
add = #['n, 'n] {
  | =[Rational[a, b], y] => [Rational[a, b], y ~> tr_] ~> radd_
  | =[x, Rational[c, d]] => [x ~> tr_, Rational[c, d]] ~> radd_
  | =[a, b] => [a, b] ~> __integer_add__
};
"#;

#[test]
fn test_int_int_infers_int() {
    quiver()
        .evaluate(&format!("{ADD} [2, 3] ~> add"))
        .expect("5")
        .expect_type("'int");
}

#[test]
fn test_mixed_infers_rational() {
    quiver()
        .evaluate(&format!("{ADD} [1/2, 3] ~> add"))
        .expect("7/2")
        .expect_type("Rational['int, 'int]");
}

#[test]
fn test_rational_rational_infers_rational() {
    // 1/2 + 1/3 = 5/6 (already lowest terms, so the minimal inlined `radd_` here, which does
    // not reduce, still gives the canonical value).
    quiver()
        .evaluate(&format!("{ADD} [1/2, 1/3] ~> add"))
        .expect("5/6")
        .expect_type("Rational['int, 'int]");
}

#[test]
fn test_unknown_kind_infers_union() {
    // When the argument kinds aren't statically known (here `m` is typed `'int | Rational`),
    // every branch is reachable, so the result widens back to the full union — graceful
    // degradation rather than a wrong narrow answer.
    quiver()
        .evaluate(&format!(
            "{ADD} mk = #'int {{ =0 => 1/2 | 7 }}, m = 0 ~> mk, [m, m] ~> add"
        ))
        .expect_type("'int | Rational['int, 'int]");
}

// The case table survives module compilation: `%num.add` (inlined as a parameter dispatch
// in std/num.qv) specializes its result at call sites in the importing program.

#[test]
fn test_num_add_module_boundary_int() {
    quiver()
        .evaluate("[2, 3] ~> %num.add")
        .expect("5")
        .expect_type("'int");
}

#[test]
fn test_num_add_module_boundary_mixed() {
    quiver()
        .evaluate("[1/2, 3] ~> %num.add")
        .expect("7/2")
        .expect_type("Rational['int, 'int]");
}

#[test]
fn test_num_sub_mul_dispatch() {
    quiver()
        .evaluate("[5, 2] ~> %num.sub")
        .expect("3")
        .expect_type("'int");
    quiver()
        .evaluate("[5, 1/2] ~> %num.sub")
        .expect("9/2")
        .expect_type("Rational['int, 'int]");
    quiver()
        .evaluate("[6, 7] ~> %num.mul")
        .expect("42")
        .expect_type("'int");
    quiver()
        .evaluate("[2/3, 3] ~> %num.mul")
        .expect("2/1")
        .expect_type("Rational['int, 'int]");
}

#[test]
fn test_num_neg_dispatch() {
    quiver()
        .evaluate("5 ~> %num.neg")
        .expect("-5")
        .expect_type("'int");
    quiver()
        .evaluate("3/4 ~> %num.neg")
        .expect("-3/4")
        .expect_type("Rational['int, 'int]");
}

#[test]
fn test_num_div_always_rational() {
    // div is not a kind-preserving dispatch — it always returns a rational.
    quiver()
        .evaluate("[6, 3] ~> %num.div")
        .expect("2/1")
        .expect_type("Rational['int, 'int] | []");
}

#[test]
fn test_nested_cross_field_dispatch() {
    // Dispatch narrows tuple fields *below the root*: reaching the final branch, both nested
    // elements are 'int, so the int-only `__integer_add__` type-checks. (Was a compile error before
    // structural narrowing.)
    quiver()
        .evaluate(
            r#"
            'n = 'int | Rational['int, 'int];
            f = #[p: ['n, 'n]] {
              | =[p: [Rational[a, b], y]] => Rat
              | =[p: [x, Rational[c, d]]] => Rat
              | =[p: [a, b]] => [a, b] ~> __integer_add__
            },
            [p: [2, 3]] ~> f
            "#,
        )
        .expect("5");
}
