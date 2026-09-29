//! A runtime type test checks what a value holds, as the type checker reasons about it, rather
//! than the type it was built at: a tuple built wider than its contents, or in generic code,
//! is tested by its fields.

use crate::common::*;

#[test]
fn test_a_tuple_built_in_generic_code_is_tested_by_its_contents() {
    // Built as `A['t]`, it says nothing about what `'t` was.
    quiver()
        .evaluate(
            "'ai = A['int]
             mk = #<'t>'t { A[$] }
             [mk 5 ~> { ='ai }, mk <01> ~> { ='ai }]",
        )
        .expect("[A[5], []]");
    // `%list.map` builds its cells generically.
    quiver()
        .evaluate(
            "g = #_ { | ='%list<'int> => %list.fold [$, init: 0, f: %num.add] | Other }
             [%list{ 1 } ~> %list.map [~, #'int { <01> }] ~> g, %list{ 1 } ~> %list.map [~, #'int { 2 }] ~> g]",
        )
        .expect("[Other, 2]");
    // A value with identity inside is not data, however it was built.
    quiver()
        .evaluate(
            "g = #_ { | ='%data => Data | Other }
             h = #<'t>'t { [$] ~> g }
             r = %ref []
             [h r, h 5]",
        )
        .expect("[Other, Data]");
}

#[test]
fn test_a_tuple_built_wider_than_its_contents_passes_a_narrower_test() {
    quiver()
        .evaluate(
            "'single = Cons['int, Nil]
             (\\[] & xs) = %data.decode<'%list<'int>> \"Cons[1, Nil]\"
             [xs ~> { ='single }, %data.decode<'%list<'int>> \"Cons[1, Cons[2, Nil]]\" ~> { ='single }]",
        )
        .expect("[Cons[1, Nil], []]");
}

#[test]
fn test_narrowing_after_a_failed_test_sees_only_what_failed_it() {
    // `A[5]` was built as `A['int | 'bin]`: it is an `'ai`, so it never reaches the branch
    // narrowed to `A['bin]`.
    quiver()
        .evaluate(
            "'ai = A['int]
             f = #A[('int | 'bin)] { | ='ai => 0 | __binary_length__ $.0 }
             g = #('int | 'bin) { A[$] ~> f }
             [g 5, g <0102>]",
        )
        .expect("[0, 2]");
    quiver()
        .evaluate(
            "'single = Cons['int, Nil]
             f = #'%list<'int> { | ='single => 0 | =Nil => 1 | =Cons[_, t] => t.0 }
             (\\[] & xs) = %data.decode<'%list<'int>> \"Cons[1, Nil]\"
             f xs",
        )
        .expect("0");
}

#[test]
fn test_a_walk_checks_partials_and_rest_types() {
    quiver()
        .evaluate(
            "'row = (id: 'int, *'%str)
             f = #_ { | ='row => Yes | No }
             mk = #<'t>'t { [id: 1, $] }
             [\"a\" ~> mk ~> f, 5 ~> mk ~> f]",
        )
        .expect("[Yes, No]");
}

#[test]
fn test_a_walk_asks_functions_their_own_type() {
    // A function can't be looked inside, but its own type is exact.
    quiver()
        .evaluate(
            "f = #_ { | ='%list<#'int -> 'int> => Fns | No }
             wrap = #'int { #'int { $ } }
             [%list{ 1 } ~> %list.map [~, wrap] ~> f, %list{ #'bin { $ } } ~> f]",
        )
        .expect("[Fns, No]");
}

#[test]
fn test_a_receive_tests_messages_by_their_contents() {
    // The message is built generically, as `A['t]`.
    quiver()
        .evaluate(
            "mk = #<'t>'t { A[$] }
             p = @[] { !A['int] ~> =A[n]; %num.add [n, 1] } []
             mk 5 ~> p
             !p",
        )
        .expect("6");
}

#[test]
fn test_a_generically_built_tuple_holding_a_function_is_inspected() {
    // Built as `[key: 'int, start: #'int -> 'int, v: 't]`: whether its function fits is for
    // the function's own type to say.
    quiver()
        .evaluate(
            "mk = #<'t>'t { v = $; [key: 1, start: #'int { $ }, v: v] }
             f = #_ { | =(key: 'int, start: #'int -> 'int) => Fits | No }
             g = #_ { | =(key: 'int, start: #'bin -> 'int) => Fits | No }
             [mk <01> ~> f, mk <01> ~> g]",
        )
        .expect("[Fits, No]");
}

#[test]
fn test_verdicts_follow_the_program_as_it_grows() {
    // Verdicts are decided as tests meet each concrete type, so a later line's tuples and
    // functions — which the first line's tests never saw — are decided when they arrive.
    quiver()
        .evaluate(
            "'ai = A['int]
             check = #_ { | ='ai => Yes | No }
             'fs = [f: #'int -> 'int]
             checkf = #_ { | ='fs => Yes | No }
             [A[1] ~> check, [f: #'int { $ }] ~> checkf]",
        )
        .expect("[Yes, Yes]")
        .then_evaluate(
            "mk = #<'t>'t { A[$] }
             wrap = #<'t>'t { [f: $] }
             [mk 2 ~> check, mk <01> ~> check, B[1] ~> check]",
        )
        .expect("[Yes, No, No]")
        .then_evaluate(
            "g = #'bin { $ }
             [[f: g] ~> checkf, wrap #'int { $ } ~> checkf, wrap g ~> checkf]",
        )
        .expect("[No, Yes, No]");
}
