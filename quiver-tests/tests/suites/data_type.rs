use crate::common::*;

#[test]
fn test_data_holds_the_identity_free_values() {
    quiver()
        .evaluate(
            r#"
            f = #'%data { $ }
            [f 1, f <01>, f [], f "s", f Point[x: 1, y: [2, Blue]], f %list{1, 2}]
            "#,
        )
        .expect("[1, <01>, [], \"s\", Point[x: 1, y: [2, Blue]], Cons[1, Cons[2, Nil]]]");
}

#[test]
fn test_data_rejects_values_with_identity() {
    quiver()
        .evaluate("f = #'%data { $ }; r = %ref []; f r")
        .expect_type_mismatch();
    quiver()
        .evaluate("f = #'%data { $ }; f #'int")
        .expect_type_mismatch();
    quiver()
        .evaluate("f = #'%data { $ }; p = @[] { 1 } []; f [p]")
        .expect_type_mismatch();
    quiver()
        .evaluate("f = #'%data { $ }; f %list{#'int}")
        .expect_type_mismatch();
}

#[test]
fn test_top_and_partial_values_are_not_known_to_be_data() {
    // A `_` value may be anything, and a partial says nothing of its other fields.
    quiver()
        .evaluate("f = #'%data { $ }; g = #_ { f $ }")
        .expect_type_mismatch();
    quiver()
        .evaluate("f = #'%data { $ }; g = #(x: 'int) { f $ }")
        .expect_type_mismatch();
    // A partial whose rest type is data is.
    quiver()
        .evaluate("f = #'%data { $ }; g = #(x: 'int, *'%data) { f $ }; g [x: 1, [2]]")
        .expect("[x: 1, [2]]");
}

#[test]
fn test_data_is_a_library_definition() {
    // `'%data` is `'int | 'bin | (*^)`, so the spelled-out union is the same type.
    quiver()
        .evaluate("'d = 'int | 'bin | (*^); f = #'%data { $ }; g = #'d { f $ }; g Point[x: [1]]")
        .expect("Point[x: [1]]");
}

#[test]
fn test_data_value_is_known_only_through_matching() {
    // A data value is passed on or matched; matching narrows it, and a tuple pattern's
    // fields are data themselves.
    quiver()
        .evaluate("f = #'%data { __integer_add__ [$, 1] }")
        .expect_type_mismatch();
    quiver()
        .evaluate(
            r#"
            keep = #'%data { $ }
            f = #'%data {
              | =('int & n) => __integer_add__ [n, 1]
              | =Point[x: x, y: _] => keep x
              | 0
            }
            [f 1, f Point[x: 7, y: 2], f <01>]
            "#,
        )
        .expect("[2, 7, 0]");
    quiver()
        .evaluate("f = #'%data { | =(x, y) => %data.encode [x, y] | No }; f [x: 1, y: [2]]")
        .expect("\"[1, [2]]\"");
}

#[test]
fn test_type_test_for_data_is_checked_at_runtime() {
    quiver()
        .evaluate(
            r#"
            f = #_ { | ='%data => Yes | No }
            r = %ref []
            [f 1, f Point[1], f r, f [r], f [x: 1]]
            "#,
        )
        .expect("[Yes, Yes, No, No, Yes]");
    quiver()
        .evaluate("f = #_ { =('%data & d) => %data.encode d | No }; r = %ref []; [f [1], f r]")
        .expect("[\"[1]\", No]");
}

#[test]
fn test_data_bound() {
    quiver()
        .evaluate("key = #<'k: '%data>'k { %data.encode $ }; [key 1, key Point[x: <01>]]")
        .expect("[\"1\", \"Point[x: <01>]\"]");
    quiver()
        .evaluate("key = #<'k: '%data>'k { %data.encode $ }; key [#'int]")
        .expect_error_containing("Type parameter 'k is bounded by");
    // An unbounded parameter is not known to be data.
    quiver()
        .evaluate("key = #<'k>'k { %data.encode $ }")
        .expect_type_mismatch();
}

#[test]
fn test_fields_matched_through_a_data_bound_are_data() {
    // A field taken from the bound's rest type is data, recursively, whatever the pattern.
    quiver()
        .evaluate("first = #<'k: '%data>'k { | =[head, _] => Pair[head, More] | Pair[$, Done] }; first [1, 2]")
        .expect("Pair[1, More]");
    quiver()
        .evaluate("f = #<'t: '%data>'t { =(x: a) => %data.encode a | \"no\" }; f [x: 1]")
        .expect("\"1\"");
    quiver()
        .evaluate("f = #<'t: '%data>['t] { $0 ~> =[a, _] => %data.encode a | \"no\" }; f [[1, 2]]")
        .expect("\"1\"");
    quiver()
        .evaluate(
            "f = #<'t: '%data>'t { =[a, _] => a ~> { =[b, _] => %data.encode b | \"inner\" } | \"no\" }; f [[7, 8], 2]",
        )
        .expect("\"7\"");
    // ... and no narrower than data.
    quiver()
        .evaluate(
            "inc = #'int { %num.add [$, 1] }; f = #<'k: '%data>'k { | =[head, _] => inc head | 0 }",
        )
        .expect_type_mismatch();
}

#[test]
fn test_identity_bearing_dict_key_is_a_compile_error() {
    quiver()
        .evaluate("p = @[] { 1 } []; %dict{} ~> %dict.put [~, p, 1]")
        .expect_error_containing("Type parameter 'k is bounded by");
    quiver()
        .evaluate(
            "%dict{} ~> %dict.put [~, [a: 1, b: \"x\"], 1] ~> %dict.get [~, [a: 1, b: \"x\"]]",
        )
        .expect("1");
}
