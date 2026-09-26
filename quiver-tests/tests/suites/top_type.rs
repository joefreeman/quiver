use crate::common::*;

#[test]
fn test_top_type_alias() {
    quiver().evaluate("'any = _").expect_alias("any", "_");
    quiver()
        .evaluate("'pair = [_, 'int]")
        .expect_alias("pair", "[_, 'int]");
}

#[test]
fn test_top_absorbs_union_members() {
    quiver()
        .evaluate("'a = 'int | _ | []")
        .expect_alias("a", "_");
}

#[test]
fn test_top_is_the_identity_of_intersection() {
    quiver().evaluate("'a = 'int & _").expect_alias("a", "'int");
    quiver()
        .evaluate("'a = (x: 'int) & _")
        .expect_alias("a", "(x: 'int)");
}

#[test]
fn test_every_value_fits_top() {
    quiver()
        .evaluate(
            "f = #_ { Ok }
             r = %ref []
             [f 1, f <01>, f Point[x: 1], f [], f f, f r]",
        )
        .expect("[Ok, Ok, Ok, Ok, Ok, Ok]");
}

#[test]
fn test_top_value_supports_no_operations() {
    // Nothing is known about a `_` value, so using it as anything narrower is an error.
    quiver()
        .evaluate("f = #_ { $ }; f 5 ~> %num.add [~, 1]")
        .expect_type_mismatch();
    quiver()
        .evaluate("f = #_ { $ }; f Point[x: 1] ~> .x")
        .expect_error_containing("Cannot access a field");
    quiver()
        .evaluate("f = #_ { $ }; g = f #'int { $ }; g 1")
        .expect_error_containing("expected a callable or process");
}

#[test]
fn test_top_value_narrows_by_type_test() {
    quiver()
        .evaluate(
            "f = #_ { | =('int)n => %num.add [n, 1] | =('bin)b => b | Other }
             [f 5, f <01>, f Blue]",
        )
        .expect("[6, <01>, Other]");
}

#[test]
fn test_top_value_destructures() {
    // A pattern's own shape is what a `_` value is tested for, with every field `_`.
    quiver()
        .evaluate(
            "f = #_ { | =Point[x: a, y: b] => [a, b] | Nope }
             [f Point[x: 1, y: 2], f 5, f Point[x: 1], f Other[x: 1, y: 2]]",
        )
        .expect("[[1, 2], Nope, Nope, Nope]");
    quiver()
        .evaluate(
            "f = #_ { | =(x: a) => a | Nope }
             [f Point[x: 1, y: 2], f [x: <01>], f 5]",
        )
        .expect("[1, <01>, Nope]");
    quiver()
        .evaluate(
            "f = #_ { | =[a, [('int)b, 1]] => %num.add [a ~> { =('int)n => n | 0 }, b] | Nope }
             [f [1, [2, 1]], f [1, [2, 3]], f [1, [<01>, 1]], f [1, 2]]",
        )
        .expect("[3, Nope, Nope, Nope]");
    quiver()
        .evaluate(
            r#"f = #_ { | ="hi" => Hi | =[] => Nil | =5 => Five | Other }
               [f "hi", f "ho", f [], f 5, f Blue]"#,
        )
        .expect("[Hi, Other, Nil, Five, Other]");
}

#[test]
fn test_top_includes_nil() {
    // A `_` result may be nil, so it short-circuits a sequence and gates a condition.
    quiver()
        .evaluate("f = #'int -> _ { [] }; { f 1; Reached }")
        .expect("[]");
    quiver()
        .evaluate("f = #'int -> _ { Ok }; { f 1; Reached }")
        .expect("Reached");
    quiver()
        .evaluate(
            "f = #'int -> _ { | =0 => [] | $ }
             g = #'int { | f $ => Yes | No }
             [g 0, g 1]",
        )
        .expect("[No, Yes]");
}

#[test]
fn test_top_result_is_covariant() {
    // Any function fits a parameter whose result is `_`.
    quiver()
        .evaluate(
            "apply = #[(f): #'int -> _, (x): 'int] { | $f $x => Kept | Dropped }
             [apply [#'int { %num.gt? [$, 2] }, 3], apply [#'int { <01> }, 1], apply [#'int { [] }, 1]]",
        )
        .expect("[Kept, Kept, Dropped]");
}

#[test]
fn test_top_parameter_is_contravariant() {
    // A function taking only ints does not accept anything.
    quiver()
        .evaluate(
            "g = #'int -> 'int { $ }
             h = #(#_ -> 'int) { $ 3 }
             h g",
        )
        .expect_type_mismatch();
    quiver()
        .evaluate(
            "g = #_ -> 'int { 3 }
             h = #(#'int -> 'int) { $ 3 }
             h g",
        )
        .expect("3");
}

#[test]
fn test_top_binds_a_type_variable() {
    quiver()
        .evaluate("f = #_ { $ }; id = #<'t>'t { $ }; x = f 5; id x")
        .expect("5")
        .expect_type("_");
}

#[test]
fn test_top_as_type_argument() {
    quiver()
        .evaluate(
            "f = #_ { $ }
             [f %list{ 1, 2 } ~> { ='%list<_> }, f 3 ~> { ='%list<_> }]",
        )
        .expect("[Cons[1, Cons[2, Nil]], []]");
}

#[test]
fn test_top_await_grant() {
    quiver()
        .evaluate(
            "done? = #(@!_) { | !$ => Yes | No }
             p = @#[] { 5 } []
             q = @#[] { [] } []
             [done? p, done? q]",
        )
        .expect("[Yes, No]");
}

#[test]
fn test_top_receive() {
    quiver()
        .evaluate(
            "p = @#[] { !#_ ~> { | =('int)n => n | 0 } } []
             %proc.send [p, <01>]
             !p",
        )
        .expect("0");
}

#[test]
fn test_predicates_may_answer_any_type() {
    quiver()
        .evaluate(
            "%list{ 1, 2, 3, 4 }
             ~> %list.filter [~, #{ | =2 => <01> | =4 => Four | [] }]",
        )
        .expect("Cons[2, Cons[4, Nil]]");
    quiver()
        .evaluate(
            "%list{ 1, 2, 3 } ~> %list.iter ~
             ~> %iter.filter [~, #{ %num.gt? [$, 1] }]
             ~> %list.collect ~",
        )
        .expect("Cons[2, Cons[3, Nil]]");
}

#[test]
fn test_parenthesised_underscore_reads_as_a_type() {
    // `(A | Cons[_])` parses as a union type rather than an alternation of patterns, which
    // matches the same values — and narrows the same way.
    quiver()
        .evaluate(
            "'l = A | B | Cons[^]
             f = #'l { | =(A | Cons[_])v => v ~> { =Cons[B] => Hit | Miss } | No }
             [f Cons[B], f Cons[A], f B]",
        )
        .expect("[Hit, Miss, No]");
    quiver()
        .evaluate("f = #_ { | =(_)v => v }; f 5")
        .expect("5");
}

#[test]
fn test_underscore_prefixed_names_are_not_top() {
    quiver()
        .evaluate("f = #['int, 'int] { __integer_add__ $ }; f [1, 2]")
        .expect("3");
}
