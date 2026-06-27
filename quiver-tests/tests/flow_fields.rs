// Tests for the "flow the piped value into tuple fields / arguments" semantics
// and the requirement to use `&` to pass a callable by value.
mod common;
use common::*;

const INC: &str = "inc = #'int { [~, 1] __integer_add__ },";

#[test]
fn callable_field_is_called_with_flow() {
    // A bare callable variable in a tuple field is called with the piped value.
    quiver()
        .evaluate(&format!("{INC} 5 [inc, 100]"))
        .expect("[6, 100]");
}

#[test]
fn each_field_independently_receives_flow() {
    quiver()
        .evaluate(&format!("{INC} 5 [inc, inc]"))
        .expect("[6, 6]");
}

#[test]
fn nil_arg_callable_field_called_with_nil_param() {
    // Standalone record: the implicit nil parameter flows in, so a nil-arg callable
    // field is called. `&` is required to store the function instead.
    quiver().evaluate("g = #{ 42 }, [g]").expect("[42]");
}

#[test]
fn amp_builtin_reference_in_record() {
    // `&__builtin__` stores a builtin as a value (e.g. a module export tuple).
    quiver()
        .evaluate("r = [a: &__integer_add__], [3, 4] r.a")
        .expect("7");
}

#[test]
fn amp_passes_callable_by_value() {
    // `&inc` stores the function (not called with 5); it can be called later.
    quiver()
        .evaluate(&format!("{INC} t = 5 [&inc, 100], 10 t.0"))
        .expect("11");
}

#[test]
fn non_callable_field_drops_flow() {
    quiver().evaluate("5 [9, ~]").expect("[9, 5]");
}

#[test]
fn ripple_field_keeps_value() {
    quiver().evaluate("5 [~, ~]").expect("[5, 5]");
}

#[test]
fn ripple_beside_constructor_sibling() {
    // Regression: `~` next to a constructor/nested-tuple sibling must not underflow.
    quiver().evaluate("5 [~, [1, 2]]").expect("[5, [1, 2]]");
    quiver().evaluate("5 [[1], ~]").expect("[[1], 5]");
    quiver().evaluate(r#"5 [~, "x"]"#).expect(r#"[5, "x"]"#);
}

#[test]
fn higher_order_argument_needs_amp() {
    // Passing a function as an argument requires `&`; it is then applied inside.
    quiver()
        .evaluate(&format!(
            "{INC} twice = #[#'int -> 'int, 'int] {{ $.1 $.0 $.0 }}, [&inc, 5] twice"
        ))
        .expect("7");
}

#[test]
fn nil_arg_callable_passed_then_called() {
    // A nil-arg function passed by `&`, then explicitly called.
    quiver()
        .evaluate("g = #{ 42 }, t = [&g], [] t.0")
        .expect("42");
}

#[test]
fn tuple_field_provenance_preserved() {
    // The `~` field must preserve provenance so field access still narrows.
    quiver()
        .evaluate(
            "make_ab = #'int { =0 => A[a: 1] | B[b: 2] },
             x = 0 make_ab,
             t = x [~, 1],
             t.0 =A[a: 'int], x.a",
        )
        .expect("1");
}
