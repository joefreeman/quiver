// Tests for the "flow the piped value into tuple fields / arguments" semantics: each field
// chain receives a copy of the piped value, and reads it by naming `~`. A field names a
// value and never calls it, so a callable field is the function itself.
mod common;
use common::*;

const INC: &str = "inc = #'int { [~, 1] ~> __integer_add__ ~ };";

#[test]
fn callable_field_is_the_function_not_a_call() {
    // A field names a value; calling is written. So a bare callable field is the function,
    // and applying it to the piped value is spelled out.
    quiver()
        .evaluate(&format!("{INC} 5 ~> [inc ~, 100]"))
        .expect("[6, 100]");
    // Naming it instead stores it — and then no field reads the flow, so the tuple would
    // drop it.
    quiver()
        .evaluate(&format!("{INC} 5 ~> [inc, 100]"))
        .expect_compile_error(quiver_compiler::compiler::Error::DiscardedChainValue);
}

#[test]
fn each_field_independently_receives_flow() {
    quiver()
        .evaluate(&format!("{INC} 5 ~> [inc ~, inc ~]"))
        .expect("[6, 6]");
}

#[test]
fn nil_arg_callable_field_is_stored_not_called() {
    // A record of functions is the ordinary case: the field is the function, and `&` adds
    // nothing to it.
    quiver()
        .evaluate("g = #[] { 42 }; [g] ~> .0 ~> ~ []")
        .expect("42");
    quiver().evaluate("g = #[] { 42 }; [g []]").expect("[42]");
}

#[test]
fn amp_builtin_reference_in_record() {
    // `&__builtin__` stores a builtin as a value (e.g. a module export tuple).
    quiver()
        .evaluate("r = [a: __integer_add__]; [3, 4] ~> r.a ~")
        .expect("7");
}

#[test]
fn amp_passes_callable_by_value() {
    // `&inc` stores the function, as the bare name now does; it can be called later.
    quiver()
        .evaluate(&format!("{INC} t = 5 ~> [inc, ~]; 10 ~> t.0 ~"))
        .expect("11");
}

#[test]
fn non_callable_field_drops_flow() {
    quiver().evaluate("5 ~> [9, ~]").expect("[9, 5]");
}

#[test]
fn ripple_field_keeps_value() {
    quiver().evaluate("5 ~> [~, ~]").expect("[5, 5]");
}

#[test]
fn ripple_beside_constructor_sibling() {
    // Regression: `~` next to a constructor/nested-tuple sibling must not underflow.
    quiver().evaluate("5 ~> [~, [1, 2]]").expect("[5, [1, 2]]");
    quiver().evaluate("5 ~> [[1], ~]").expect("[[1], 5]");
    quiver().evaluate(r#"5 ~> [~, "x"]"#).expect(r#"[5, "x"]"#);
}

#[test]
fn higher_order_argument_is_a_plain_name() {
    // A function passed as an argument is named, and applied inside.
    quiver()
        .evaluate(&format!(
            "{INC} twice = #[#'int -> 'int, 'int] {{ $.1 ~> $.0 ~ ~> $.0 ~ }}; [inc, 5] ~> twice ~"
        ))
        .expect("7");
}

#[test]
fn nil_arg_callable_passed_then_called() {
    // A nil-arg function passed by `&`, then explicitly called.
    quiver()
        .evaluate("g = #[] { 42 }; t = [g]; [] ~> t.0 ~")
        .expect("42");
}

#[test]
fn tuple_field_provenance_preserved() {
    // The `~` field must preserve provenance so field access still narrows.
    quiver()
        .evaluate(
            "make_ab = #'int { =0 => A[a: 1] | B[b: 2] };
             x = 0 ~> make_ab ~;
             t = x ~> [~, 1];
             t.0 ~> =A[a: 'int]; x.a",
        )
        .expect("1");
}
