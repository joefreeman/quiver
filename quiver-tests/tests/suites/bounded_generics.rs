use crate::common::*;
use std::collections::HashMap;

#[test]
fn test_bounded_parameter_is_usable_as_its_bound() {
    quiver()
        .evaluate("f = #<'t: 'int>'t { __integer_add__ [$, 1] }; f 2")
        .expect("3");
    quiver()
        .evaluate("f = #<'t: (x: 'int)>'t { __integer_add__ [$x, 1] }; f Point[x: 1, y: 2]")
        .expect("2");
}

#[test]
fn test_bounded_result_keeps_the_callers_type() {
    // The body reads a field through the bound, but answers the argument as it came: the
    // caller still sees every field.
    quiver()
        .evaluate(
            r#"
            touch = #<'t: (x: 'int)>'t -> 't { __integer_add__ [$x, 1]; $ }
            touch Point[x: 1, y: 2] ~> .y
            "#,
        )
        .expect("2");
    quiver()
        .evaluate("f = #<'t: (x: 'int)>'t { $ }; f Point[x: 1, y: <01>]")
        .expect_type("Point[x: 'int, y: 'bin]");
}

#[test]
fn test_instantiation_must_fit_the_bound() {
    quiver()
        .evaluate("f = #<'t: 'int>'t { $ }; f <01>")
        .expect_error_containing("Type parameter 't is bounded by 'int, but 'bin does not fit it");
    quiver()
        .evaluate("f = #<'t: (x: 'int)>'t { $ }; f Point[y: 2]")
        .expect_error_containing("Type parameter 't is bounded by (x: 'int)");
    // Explicit type arguments are checked too.
    quiver()
        .evaluate("f = #<'t: 'int>'t { $ }; f<'bin>")
        .expect_error_containing("Type parameter 't is bounded by 'int, but 'bin does not fit it");
    quiver()
        .evaluate("f = #<'t: 'int>'t { $ }; g = f<'int>; g 4")
        .expect("4");
}

#[test]
fn test_union_bound() {
    quiver()
        .evaluate("f = #<'t: 'int | 'bin>'t { $ }; [f 1, f <01>]")
        .expect("[1, <01>]");
    quiver()
        .evaluate("f = #<'t: 'int | 'bin>'t { $ }; f Point[x: 1]")
        .expect_error_containing("bounded by");
}

#[test]
fn test_match_on_a_union_bound_tests_at_runtime() {
    // A tuple pattern on a `'t` bounded by a union still tests the member at runtime.
    quiver()
        .evaluate(
            r#"
            f = #<'t: A['int] | B['bin]>'t {
              | =A[n] => __integer_add__ [n, 1]
              | =B[_] => 0
            }
            [f A[1], f B[<01>]]
            "#,
        )
        .expect("[2, 0]");
    quiver()
        .evaluate("f = #<'t: A['int] | B['bin]>'t { =A[n]; n }; [f A[1], f B[<01>]]")
        .expect("[1, []]");
}

#[test]
fn test_destructuring_through_the_bound() {
    quiver()
        .evaluate("f = #<'t: (x: 'int)>'t { (x) = $; __integer_add__ [x, 1] }; f Point[x: 1, y: 2]")
        .expect("2");
    quiver()
        .evaluate(
            r#"
            f = #<'t: Point[x: 'int, y: 'int]>'t { Point[x: a, y: b] = $; __integer_add__ [a, b] }
            f Point[x: 1, y: 2]
            "#,
        )
        .expect("3");
}

#[test]
fn test_type_test_disjoint_from_the_bound_never_matches() {
    quiver()
        .evaluate("f = #<'t: A['int] | B['bin]>'t { =('int & n); n }; f A[1]")
        .expect("[]");
}

#[test]
fn test_field_update_answers_the_bounds_shape() {
    // Replacing a field could break whatever narrower type the caller's `'t` has, so the
    // result is the bound's shape rather than `'t`.
    quiver()
        .evaluate("f = #<'t: Point[x: 'int, y: 'int]>'t { $[..., x: 5] }; f Point[x: 1, y: 2]")
        .expect("Point[x: 5, y: 2]");
    quiver()
        .evaluate("f = #<'t: Point[x: 'int, y: 'int]>'t -> 't { $[..., x: 5] }")
        .expect_error_containing("declared result: Point[x: 'int, y: 'int] is not 't");
}

#[test]
fn test_bound_names_earlier_parameters() {
    quiver()
        .evaluate(
            r#"
            first = #<'e, 'l: Cons['e, _]>'l { =Cons[h, _]; h }
            first Cons[1, Cons[2, Nil]]
            "#,
        )
        .expect("1");
    // Not itself, nor a later parameter.
    quiver()
        .evaluate("f = #<'t: ['t]>'t { $ }")
        .expect_error_containing("Unknown type alias: t");
    quiver()
        .evaluate("f = #<'t: 'u, 'u>'t { $ }")
        .expect_error_containing("Unknown type alias: u");
}

#[test]
fn test_bounded_variable_satisfies_a_bound_through_its_own() {
    // A caller's `'u` fits `'t: 'int` when its own bound does.
    quiver()
        .evaluate(
            r#"
            inc = #<'t: 'int>'t { __integer_add__ [$, 1] }
            twice = #<'u: 'int>'u { inc $ ~> inc }
            twice 1
            "#,
        )
        .expect("3");
    quiver()
        .evaluate(
            r#"
            inc = #<'t: 'int>'t { __integer_add__ [$, 1] }
            f = #<'u>'u { inc $ }
            "#,
        )
        .expect_error_containing("Type parameter 't is bounded by 'int, but 'u does not fit it");
}

#[test]
fn test_nested_literal_keeps_the_enclosing_bound() {
    // A nested literal re-declaring `'t` names the enclosing parameter, bound included.
    quiver()
        .evaluate(
            r#"
            f = #<'t: 'int>'t {
              g = #<'t>'t { __integer_add__ [$, 1] }
              g $
            }
            f 1
            "#,
        )
        .expect("2");
    quiver()
        .evaluate("f = #<'t: 'int>'t { g = #<'t: 'bin>'t { $ }; g $ }")
        .expect_error_containing("keeps that declaration's bound");
}

#[test]
fn test_alias_bound_is_checked_where_applied() {
    quiver()
        .evaluate("'p<'t: 'int> = P['t]; f = #'p<'int> { $ }; f P[3]")
        .expect("P[3]");
    quiver()
        .evaluate("'p<'t: 'int> = P['t]; f = #'p<'bin> { $ }")
        .expect_error_containing("Type parameter 't is bounded by 'int, but 'bin does not fit it");
}

#[test]
fn test_alias_applied_to_a_parameter_needs_a_fitting_bound() {
    // A generic applying a bounded alias to its own parameter must bound it at least as
    // tightly; the alias's own parameters are held to the same rule.
    quiver()
        .evaluate("'p<'t: 'int> = P['t]; f = #<'u>'p<'u> { $ }")
        .expect_error_containing("Type parameter 't is bounded by 'int, but 'u does not fit it");
    quiver()
        .evaluate("'p<'t: 'int> = P['t]; f = #<'u: 'int>'p<'u> { $ }; f P[3]")
        .expect("P[3]");
    quiver()
        .evaluate("'p<'t: 'int> = P['t]; 'q<'s> = Q['p<'s>]")
        .expect_error_containing("Type parameter 't is bounded by 'int, but 's does not fit it");
    quiver()
        .evaluate("'p<'t: 'int> = P['t]; 'q<'s: 'int> = Q['p<'s>]; f = #'q<'int> { $ }; f Q[P[1]]")
        .expect("Q[P[1]]");
}

#[test]
fn test_imported_generic_bounds_are_checked() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["bounded".to_string()],
        r#"
        'keyed<'k: 'int | 'bin, 'v> = Keyed['k, 'v]
        [
          inc: #<'t: 'int>'t { __integer_add__ [$, 1] },
          key: #<'k: 'int | 'bin, 'v>'keyed<'k, 'v> { $0 },
        ]
        "#
        .to_string(),
    );
    quiver()
        .with_modules(modules.clone())
        .evaluate("[%bounded.inc 1, %bounded.key Keyed[<01>, 2]]")
        .expect("[2, <01>]");
    quiver()
        .with_modules(modules.clone())
        .evaluate("%bounded.inc <01>")
        .expect_error_containing("Type parameter 't is bounded by 'int, but 'bin does not fit it");
    quiver()
        .with_modules(modules.clone())
        .evaluate("f = #'%bounded.keyed<Point, 'int> { $ }")
        .expect_error_containing("Type parameter 'k is bounded by 'bin | 'int");
    quiver()
        .with_modules(modules)
        .evaluate("%bounded.inc<'bin>")
        .expect_error_containing("Type parameter 't is bounded by 'int, but 'bin does not fit it");
}

#[test]
fn test_spawned_generic_init_is_checked_against_bounds() {
    quiver()
        .evaluate("p = @<'t: 'int>'t { $ } <01>")
        .expect_error_containing("Type parameter 't is bounded by 'int, but 'bin does not fit it");
    quiver()
        .evaluate("p = @<'t: 'int>'t { __integer_add__ [$, 1] } 2; !p")
        .expect("3");
}
