use crate::common::*;

// An un-annotated function literal (`#{ ... }`) means "infer my parameter", and nothing else:
// it must be the argument (or a top-level field of the bracket argument) of a juxtaposed call
// whose callee is known (`f [.., #{ ... }]`), with type variables pinned by sibling arguments.
// In any other position there is nothing to infer from, and it is a compile error — a function
// that takes nil is written `#[] { ... }`.

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
    // rather than being rejected as uninferable. The declared return type is what would catch
    // a wrong answer here: inferring nil would give `#[] -> []` and, unified against
    // `#'t -> ('t | [])`, silently widen the caller's `'t` to `'t | []`.
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
    // inferring nil would make the mapped list `'%list<[]>`.
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
fn test_unsolved_callee_variable_is_not_inferable() {
    // Only a *rigid* variable is usable. One the callee has yet to solve has nothing to pin
    // it, so there is no parameter type to infer and the literal is rejected.
    quiver()
        .evaluate("run = #<'t>[#'t -> 'int] { 42 }; run [#{ 42 }]")
        .expect_compile_error(quiver_compiler::compiler::Error::ParameterNotInferable);
}

#[test]
fn test_unsolved_callee_variable_takes_an_explicit_nil_parameter() {
    // Writing the parameter is what resolves it: `#[] { ... }` solves the callee's `'t` to nil.
    quiver()
        .evaluate("run = #<'t>[#'t -> 'int] { 42 }; run [#[] { 42 }]")
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
fn test_unannotated_literal_without_context_is_rejected() {
    // With no expected type from context there is nothing to infer, and the old nilary
    // reading of this spelling is gone.
    quiver()
        .evaluate("f = #{ 42 }; f []")
        .expect_compile_error(quiver_compiler::compiler::Error::ParameterNotInferable);
}

#[test]
fn test_explicit_nil_parameter_form() {
    // `#[] { ... }` is how a function that takes nil is written, in any position.
    quiver().evaluate("f = #[] { 7 }; f []").expect("7");
}

#[test]
fn test_spawn_shorthand_requires_an_explicit_nil_parameter() {
    // The spawn shorthand follows the same rule: `@{ ... }` has nothing to infer from, so a
    // nilary root function is spawned as `@[] { ... }`.
    quiver().evaluate("p = @[] { 42 } []; !p").expect("42");
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

#[test]
fn test_inferred_literal_sees_variables_later_fields_pin() {
    // `#{…}` compiles after the other fields, so `x` pins `'t` first, whatever the order.
    quiver()
        .evaluate(
            "twice = #<'t>[f: #'t -> 't, x: 't] -> 't { f = $f; $x ~> f ~> f }
             twice [f: #{ __integer_add__ [$, 1] }, x: 2]",
        )
        .expect("4");
    // The fields are still built in their slots.
    quiver()
        .evaluate(
            "g = #<'t>[a: 't, f: #'t -> 't, b: 'bin] -> ['t, 'bin] { [$f $a, $b] }
             g [a: 5, f: #{ __integer_multiply__ [$, 3] }, b: <0a>]",
        )
        .expect("[15, <0a>]");
    quiver()
        .evaluate(
            "p = #<'t>[#'t -> 't, 't] -> 't { $0 $1 }
             p [#{ __integer_add__ [$, 1] }, 41]",
        )
        .expect("42");
    quiver()
        .evaluate(
            "g = #<'t>[f: #'t -> 't, x: 't, y: 'int] -> ['t, 'int] { [$f $x, $y] }
             7 ~> g [f: #{ __integer_add__ [$, 1] }, x: ~, y: ~]",
        )
        .expect("[8, 7]");
}
