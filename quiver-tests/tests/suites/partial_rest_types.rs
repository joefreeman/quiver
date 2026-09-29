use crate::common::*;

#[test]
fn test_rest_type_constrains_every_field() {
    quiver()
        .evaluate("f = #(*'int) { $ }; [f [1, 2], f P[x: 3], f []]")
        .expect("[[1, 2], P[x: 3], []]");
    quiver()
        .evaluate("f = #(*'int) { $ }; f [1, <01>]")
        .expect_type_mismatch();
}

#[test]
fn test_listed_fields_escape_the_rest_type() {
    quiver()
        .evaluate("f = #(id: 'bin, *'int) { $ }; f [id: <01>, 2, n: 3]")
        .expect("[id: <01>, 2, n: 3]");
    quiver()
        .evaluate("f = #(id: 'bin, *'int) { $ }; f [id: <01>, n: <02>]")
        .expect_type_mismatch();
    quiver()
        .evaluate("f = #(id: 'bin, *'int) { $ }; f [n: 1]")
        .expect_type_mismatch();
}

#[test]
fn test_named_partial_with_rest_type() {
    quiver()
        .evaluate("f = #Str(*'bin) { $ }; f \"hi\"")
        .expect("\"hi\"");
    quiver()
        .evaluate("f = #Str(*'bin) { $ }; f Other[<01>]")
        .expect_type_mismatch();
}

#[test]
fn test_top_rest_type_is_the_empty_partial() {
    quiver()
        .evaluate("f = #(*_) { $ }; r = %ref []; f [r] ~> =[^r]; Ok")
        .expect("Ok");
    quiver().evaluate("'t = (*_)").expect_type("");
}

#[test]
fn test_partial_subtyping_with_rest_types() {
    // A rest type is covariant.
    quiver()
        .evaluate("f = #(*('int | 'bin)) { $ }; h = #(*'int) { f $ }; h [1]")
        .expect("[1]");
    quiver()
        .evaluate("f = #(*'int) { $ }; h = #(*('int | 'bin)) { f $ }")
        .expect_type_mismatch();
    // A listed field says nothing of the others, and a rest type guarantees no field exists.
    quiver()
        .evaluate("f = #(*'int) { $ }; h = #(x: 'int) { f $ }")
        .expect_type_mismatch();
    quiver()
        .evaluate("f = #(x: 'int) { $ }; h = #(*'int) { f $ }")
        .expect_type_mismatch();
    // A listed field must fit the other side's rest type too.
    quiver()
        .evaluate("f = #(*'int) { $ }; h = #(x: 'int, *'int) { f $ }; h [x: 1, 2]")
        .expect("[x: 1, 2]");
    quiver()
        .evaluate("f = #(*'int) { $ }; h = #(x: 'bin, *'int) { f $ }")
        .expect_type_mismatch();
}

#[test]
fn test_matching_reads_unlisted_fields_at_the_rest_type() {
    quiver()
        .evaluate(
            "f = #(*'int) { | =[a, b] => __integer_add__ [a, b] | =(x: ~) => ~ | 0 }; \
             [f [1, 2], f [x: 5], f []]",
        )
        .expect("[3, 5, 0]");
}

#[test]
fn test_intersecting_partials_with_rest_types() {
    quiver()
        .evaluate(
            "f = #('int | [x: 'int, y: 'bin] | [x: 'int, y: 'int]) { \
             | =(('%data & (*'int)) & v) => v.y | No }; \
             [f 1, f [x: 1, y: <02>], f [x: 1, y: 2]]",
        )
        .expect("[No, No, 2]");
    quiver()
        .evaluate(
            "f = #([x: 'int] | [x: 'bin]) { | =((*'int) & (x: 'int) & v) => v.x | No }; \
             [f [x: 1], f [x: <01>]]",
        )
        .expect("[1, No]");
}

#[test]
fn test_rest_type_must_be_last() {
    quiver()
        .evaluate("f = #(*'int, x: 'int) { $ }")
        .expect_parse_failure();
}

#[test]
fn test_partial_with_rest_type_cannot_be_spread() {
    quiver()
        .evaluate("'p = (*'int); 'q = [...'p]")
        .expect_error_containing("cannot be spread");
}

#[test]
fn test_rest_type_is_substituted() {
    quiver()
        .evaluate("'row<'t> = (*'t); f = #'row<'int> { $ }; [f [1, 2], f []]")
        .expect("[[1, 2], []]");
    quiver()
        .evaluate("'row<'t> = (*'t); f = #'row<'int> { $ }; f [<01>]")
        .expect_type_mismatch();
    quiver()
        .evaluate("f = #<'t>(*'t) { $ }; f [1, 2]")
        .expect("[1, 2]");
}
