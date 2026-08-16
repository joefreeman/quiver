mod common;
use common::*;

// An un-annotated function literal (`#{ ... }`) infers its parameter type at the Apply site
// only: it must be the argument (or a top-level field of the bracket argument) of a juxtaposed
// call whose callee is known (`f [.., #{ ... }]`), with type variables pinned by sibling
// arguments. A literal in any other position falls back to its nilary meaning.

#[test]
fn test_inferred_mapper_through_iter_map() {
    // `#{ ... }` as `%iter.map`'s `#'t -> 'u` argument: `'t` is pinned by the piped iterator.
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.iter ~ ~> %iter.map [~, #{ %num.mul [$, 10] }] ~> %list.collect ~
            "#,
        )
        .expect("Cons[10, Cons[20, Cons[30, Nil]]]");
}

#[test]
fn test_inferred_predicate_through_iter_filter() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]] ~> %list.iter ~ ~> %iter.filter [~, #{ %int.mod [$, 2] ~> =0 }] ~> %list.collect ~
            "#,
        )
        .expect("Cons[2, Cons[4, Nil]]");
}

#[test]
fn test_inferred_tuple_param_through_iter_fold() {
    // `%iter.fold`'s combiner is `#['acc, 't] -> 'acc`; the literal infers a tuple parameter,
    // so `$0`/`$1` resolve against it.
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Nil]]] ~> %list.iter ~ ~> %iter.fold [~, 0, #{ %num.add [$0, $1] }]
            "#,
        )
        .expect("6");
}

#[test]
fn test_inferred_param_via_local_higher_order_function() {
    // The variable inferred for the literal can be the enclosing function's own type parameter.
    quiver()
        .evaluate(
            r#"
            'list<'t> = Nil | Cons['t, ^]
            map = #<'t, 'u>['list<'t>, #'t -> 'u, 'list<'u>] {
              =[lst, f, acc];
              lst ~> { =Nil => acc | =Cons[h, t] => [t, f, Cons[h ~> f ~, acc]] ~> ^ ~ }
            };
            Cons[[1, 10], Cons[[2, 20], Nil]] ~> map [~, #{ $0 }, Nil]
            "#,
        )
        .expect("Cons[2, Cons[1, Nil]]");
}

#[test]
fn test_inferred_param_pinned_to_enclosing_type_parameter() {
    // A sibling argument can pin the callee's variable to the *enclosing* generic's own type
    // parameter. That is a real (if opaque) type, so the literal takes it as its parameter
    // instead of falling back to nil — a fallback would infer `#[] -> []` and, unifying that
    // against `#'t -> ('t | [])`, silently widen the caller's `'t` to `'t | []`. The declared
    // return type is what catches the widening.
    quiver()
        .evaluate(
            r#"
            keep = #<'t>[(list): '%list<'t>] -> '%list<'t> { %list.filter [$list, #{ $ }] };
            keep [Cons[1, Cons[2, Nil]]]
            "#,
        )
        .expect("Cons[1, Cons[2, Nil]]");
}

#[test]
fn test_inferred_mapper_keeps_the_enclosing_element_type() {
    // The same for `%list.map`, whose result variable is pinned by the literal's own result:
    // a nil fallback would make the mapped list `'%list<[]>`.
    quiver()
        .evaluate(
            r#"
            same = #<'t>[(list): '%list<'t>] -> '%list<'t> { %list.map [$list, #{ $ }] };
            same [Cons[1, Cons[2, Nil]]]
            "#,
        )
        .expect("Cons[1, Cons[2, Nil]]");
}

#[test]
fn test_unsolved_callee_variable_still_falls_back_to_nil() {
    // Only a *rigid* variable is usable. One the callee has yet to solve has nothing to pin
    // it, so the literal keeps its nilary meaning.
    quiver()
        .evaluate("run = #<'t>[#'t -> 'int] { =[g]; g [] }; run [#{ 42 }]")
        .expect("42");
}

#[test]
fn test_inferred_param_concrete_callee() {
    // When the callee's parameter is fully concrete, no sibling is needed to pin it.
    quiver()
        .evaluate(
            r#"
            run = #[#'int -> 'int] { =[g]; 10 ~> g ~ };
            run [#{ %num.add [$, 1] }]
            "#,
        )
        .expect("11");
}

#[test]
fn test_unannotated_literal_without_context_stays_nilary() {
    // With no expected type from context, `#{ ... }` keeps its nilary-function meaning.
    quiver().evaluate("f = #{ 42 }; f []").expect("42");
}

#[test]
fn test_explicit_nil_parameter_form() {
    // `#[] { ... }` forces a nil parameter even where a context type is available.
    quiver().evaluate("f = #[] { 7 }; f []").expect("7");
}

#[test]
fn test_chain_position_infers_from_next_callable() {
    // A literal chain term flowing into a statically-resolvable callable
    // (`[~, #{…}] ~> f`) takes that callable's parameter as its expected type — the
    // piped counterpart of Apply-site inference.
    quiver()
        .evaluate(
            "Cons[1, Cons[2, Nil]] ~> %list.iter ~ ~> [~, #{ %int.mod [$, 2] ~> =0 }] \
             ~> %iter.filter ~ ~> %list.collect ~",
        )
        .expect("Cons[2, Nil]");
}

#[test]
fn test_apply_site_argument_infers_parameter() {
    // A bare `#{…}` as a top-level field of a juxtaposed call's argument infers its
    // parameter from the callee's parameter type.
    quiver()
        .evaluate("call = #[g: #'int -> 'int] { 5 ~> $g ~ }; call [g: #{ [$, $] ~> %num.add ~ }]")
        .expect("10");
}
