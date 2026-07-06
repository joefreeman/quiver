// Field access through partial types must be layout-independent: a partial `(x: 'int)`
// admits any tuple carrying the field, so its position varies per concrete layout and is
// resolved by name at runtime (GetNamed) rather than compiled to a fixed offset.

mod common;
use common::*;

#[test]
fn test_partial_field_access_is_layout_independent() {
    quiver()
        .evaluate("f = #(x: 'int) { $.x }, a = [x: 2, y: 1] f, b = [y: 1, x: 2] f, [a, b]")
        .expect("[2, 2]");
}

#[test]
fn test_partial_pattern_destructure_is_layout_independent() {
    quiver()
        .evaluate(
            "f = #(x: 'int, y: 'int) { $ =(y: v), v }, a = [x: 2, y: 1] f, b = [y: 1, x: 2] f, [a, b]",
        )
        .expect("[1, 1]");
}

#[test]
fn test_star_pattern_through_partial() {
    quiver()
        .evaluate(
            "f = #(a: 'int, b: 'int) { $ =*, [a, b] %num.add }, p = [a: 1, b: 2] f, q = [b: 2, a: 1] f, [p, q]",
        )
        .expect("[3, 3]");
}

#[test]
fn test_named_partial_access() {
    quiver()
        .evaluate("f = #P(v: 'int) { $.v }, a = P[v: 7, w: 0] f, b = P[w: 0, v: 7] f, [a, b]")
        .expect("[7, 7]");
}

#[test]
fn test_nested_partial_access() {
    quiver()
        .evaluate(
            "f = #(inner: (n: 'int)) { $.inner.n }, a = [inner: [n: 5, m: 1], other: 0] f, b = [other: 0, inner: [m: 1, n: 5]] f, [a, b]",
        )
        .expect("[5, 5]");
}

#[test]
fn test_first_class_function_with_partial_parameter() {
    quiver()
        .evaluate("f = #(x: 'int) { $.x }, g = &f, a = [x: 9, y: 0] g, b = [y: 0, x: 9] g, [a, b]")
        .expect("[9, 9]");
}

#[test]
fn test_union_with_field_at_different_positions() {
    // No partial involved: a union of concrete tuples whose members carry the field at
    // different positions also resolves by name.
    quiver()
        .evaluate(
            "'t = A[x: 'int, y: 'int] | B[y: 'int, x: 'int], f = #'t { $.x }, a = A[x: 1, y: 9] f, b = B[y: 9, x: 2] f, [a, b]",
        )
        .expect("[1, 2]");
}

#[test]
fn test_non_linear_partial_pattern_matches_by_name() {
    quiver()
        .evaluate(
            "f = #(x: 'int, y: 'int) { $ =(x: v, y: v) }, a = [y: 4, x: 4] f, b = [y: 4, x: 5] f, [a, b]",
        )
        .expect("[Ok, []]");
}

#[test]
fn test_literal_field_check_through_partial() {
    quiver()
        .evaluate("f = #(x: 'int) { $ =(x: 2) }, a = [y: 0, x: 2] f, b = [x: 3, y: 0] f, [a, b]")
        .expect("[Ok, []]");
}

#[test]
fn test_partial_typed_message_receive() {
    quiver()
        .evaluate("p = @{ !#(n: 'int) { =(n: v) => v } }, [z: 0, n: 8] p, !p")
        .expect("8");
}

#[test]
fn test_positional_access_through_partial_is_an_error() {
    // A partial constrains fields by name only; the runtime layout — and hence any
    // position — is unknown.
    quiver()
        .evaluate("f = #(x: 'int, y: 'int) { $.1 }, [x: 1, y: 2] f")
        .expect_compile_error(
            quiver_compiler::compiler::Error::PositionalAccessOnPartial { index: 1 },
        );
}
