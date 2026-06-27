mod common;
use common::*;

// An un-annotated function literal (`#{ ... }`) passed directly as a call argument infers its
// parameter type from the callee's signature, with type variables pinned by sibling arguments.

#[test]
fn test_inferred_mapper_through_iter_map() {
    // `#{ ... }` as `%iter.map`'s `#'t -> 'u` argument: `'t` is pinned by the piped iterator.
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Nil]]] %list.iter [~, #{ [$, 10] %num.mul }] %iter.map %list.collect
            "#,
        )
        .expect("Cons[10, Cons[20, Cons[30, Nil]]]");
}

#[test]
fn test_inferred_predicate_through_iter_filter() {
    quiver()
        .evaluate(
            r#"
            Cons[1, Cons[2, Cons[3, Cons[4, Nil]]]] %list.iter [~, #{ [$, 2] %int.mod =0 }] %iter.filter %list.collect
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
            Cons[1, Cons[2, Cons[3, Nil]]] %list.iter [~, 0, #{ [$0, $1] %num.add }] %iter.fold
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
              =[lst, f, acc],
              lst { =Nil => acc | =Cons[h, t] => [t, &f, Cons[h f, acc]] ^ }
            },
            Cons[[1, 10], Cons[[2, 20], Nil]] [~, #{ $0 }, Nil] map
            "#,
        )
        .expect("Cons[2, Cons[1, Nil]]");
}

#[test]
fn test_inferred_param_concrete_callee() {
    // When the callee's parameter is fully concrete, no sibling is needed to pin it.
    quiver()
        .evaluate(
            r#"
            run = #[#'int -> 'int] { =[g], 10 g },
            [#{ [$, 1] %num.add }] run
            "#,
        )
        .expect("11");
}

#[test]
fn test_unannotated_literal_without_context_stays_nilary() {
    // With no expected type from context, `#{ ... }` keeps its nilary-function meaning.
    quiver().evaluate("f = #{ 42 }, 99 f").expect("42");
}

#[test]
fn test_explicit_nil_parameter_form() {
    // `#[] { ... }` forces a nil parameter even where a context type is available.
    quiver().evaluate("f = #[] { 7 }, 99 f").expect("7");
}
