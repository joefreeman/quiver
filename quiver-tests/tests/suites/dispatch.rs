use crate::common::*;

// A function that dispatches on its parameter records a per-branch case table, so a call
// with a concrete argument type infers the result type of just the reachable branch(es)
// rather than the whole union of branch results.

const ADD: &str = r#"
'n = 'int | Rational['int, 'int];
radd_ = #[Rational['int, 'int], Rational['int, 'int]] {
  =[Rational[a, b], Rational[c, d]];
  Rational[[[a, d] ~> __integer_multiply__ ~, [c, b] ~> __integer_multiply__ ~] ~> __integer_add__ ~, [b, d] ~> __integer_multiply__ ~]
};
tr_ = #'n { | =Rational[n, d] => Rational[n, d] | =n => Rational[n, 1] };
add = #['n, 'n] {
  | =[Rational[a, b], y] => [Rational[a, b], y ~> tr_ ~] ~> radd_ ~
  | =[x, Rational[c, d]] => [x ~> tr_ ~, Rational[c, d]] ~> radd_ ~
  | =[a, b] => [a, b] ~> __integer_add__ ~
};
"#;

#[test]
fn test_int_int_infers_int() {
    quiver()
        .evaluate(&format!("{ADD} [2, 3] ~> add ~"))
        .expect("5")
        .expect_type("'int");
}

#[test]
fn test_mixed_infers_rational() {
    quiver()
        .evaluate(&format!("{ADD} [1/2, 3] ~> add ~"))
        .expect("7/2")
        .expect_type("Rational['int, 'int]");
}

#[test]
fn test_rational_rational_infers_rational() {
    // 1/2 + 1/3 = 5/6 (already lowest terms, so the minimal inlined `radd_` here, which does
    // not reduce, still gives the canonical value).
    quiver()
        .evaluate(&format!("{ADD} [1/2, 1/3] ~> add ~"))
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
            "{ADD} mk = #'int {{ =0 => 1/2 | 7 }}; m = 0 ~> mk ~; [m, m] ~> add ~"
        ))
        .expect_type("'int | Rational['int, 'int]");
}

// The case table survives module compilation: `%num.add` (inlined as a parameter dispatch
// in std/num.qv) specializes its result at call sites in the importing program.

#[test]
fn test_num_add_module_boundary_int() {
    quiver()
        .evaluate("[2, 3] ~> %num.add ~")
        .expect("5")
        .expect_type("'int");
}

#[test]
fn test_num_add_module_boundary_mixed() {
    quiver()
        .evaluate("[1/2, 3] ~> %num.add ~")
        .expect("7/2")
        .expect_type("Rational['int, 'int]");
}

#[test]
fn test_num_sub_mul_dispatch() {
    quiver()
        .evaluate("[5, 2] ~> %num.sub ~")
        .expect("3")
        .expect_type("'int");
    quiver()
        .evaluate("[5, 1/2] ~> %num.sub ~")
        .expect("9/2")
        .expect_type("Rational['int, 'int]");
    quiver()
        .evaluate("[6, 7] ~> %num.mul ~")
        .expect("42")
        .expect_type("'int");
    quiver()
        .evaluate("[2/3, 3] ~> %num.mul ~")
        .expect("2/1")
        .expect_type("Rational['int, 'int]");
}

#[test]
fn test_num_neg_dispatch() {
    quiver()
        .evaluate("5 ~> %num.neg ~")
        .expect("-5")
        .expect_type("'int");
    quiver()
        .evaluate("3/4 ~> %num.neg ~")
        .expect("-3/4")
        .expect_type("Rational['int, 'int]");
}

#[test]
fn test_num_div_always_rational() {
    // div is not a kind-preserving dispatch — it always returns a rational, or nil carrying
    // the zero divisor as its `:error`.
    quiver()
        .evaluate("[6, 3] ~> %num.div ~")
        .expect("2/1")
        .expect_type("Rational['int, 'int] | [] { :error DivisionByZero }");
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
              | =[p: [a, b]] => [a, b] ~> __integer_add__ ~
            };
            [p: [2, 3]] ~> f ~
            "#,
        )
        .expect("5");
}

#[test]
fn test_dispatch_across_repl_entries() {
    // A dispatch function defined in an *earlier* REPL entry must specialise its result at the
    // call site exactly as one defined in the same entry does. The tables recording it are
    // per-compilation, and an entry is its own compilation — so without the session carrying
    // them, `f A` widened to the whole union and the `%num.add` below became a type error that
    // the identical program in a file compiles fine.
    quiver()
        .evaluate("f = #(A | B) { | =A => 1 | =B => <01> }; Ok")
        .then_evaluate("f A ~> %num.add [~, 1]")
        .expect("2");
}

#[test]
fn test_dispatch_to_tail_call_branch_takes_the_function_result() {
    // A branch whose value is a tail call types as `never` (inference gives up there), so a call
    // dispatching to it — here because `ts` is known non-empty — must answer the function's
    // result type instead, or binding the call reads as a match that can never succeed.
    quiver()
        .evaluate(
            r#"
            'l = Nil | Cons['int, ^]
            len = #['l, 'int] -> 'int { | =[Nil, n] => n | =[Cons[_, rest], n] => ^ [rest, __integer_add__ [n, 1]] }
            d = #'l { | =Nil => 0 | =ts => { n = len [ts, 0]; n } }
            d Cons[1, Cons[2, Nil]]
            "#,
        )
        .expect("2")
        .expect_type("'int");
}

#[test]
fn test_ascribed_fast_arm_guards_only_its_own_types() {
    // An arm ascribing a field (`('int)a`) narrows its dispatch guard to that type even when the
    // field's declared type has a recursive member, so it claims no other argument's result.
    quiver()
        .evaluate(
            r#"
            'ts = Nil | Cons['int, ^]
            'v = 'int | R['int] | L['ts]
            f = #['v, 'v] {
              | =[('int)a, ('int)b] => __integer_add__ [a, b]
              | =[L[t], y] => Lst
              | =[x, L[t]] => Lst
              | =[R[a], y] => R[a]
              | =[x, R[a]] => R[a]
              | =[a, b] => __integer_add__ [a, b]
            }
            f [R[1], 3]
            "#,
        )
        .expect("R[1]")
        .expect_type("R['int]");
}

#[test]
fn test_nested_function_tail_call_leaves_the_branch_precise() {
    // A `^` inside a function literal recurses into that literal, so the enclosing branch is no
    // self tail call and a call dispatching to it keeps the branch's own result type.
    quiver()
        .evaluate(
            r#"
            f = #(A | B['int]) { | =A => None | =B[n] => { g = #'int { | =0 => Z | ^ 0 }; Some[n] } }
            f B[5]
            "#,
        )
        .expect("Some[5]")
        .expect_type("Some['int]");
}
