mod common;
use common::*;

#[test]
fn test_sequence_returns_last() {
    quiver().evaluate("1; 2; 3").expect("3");
}

#[test]
fn test_sequence_with_nil() {
    quiver().evaluate("1; []; 3").expect("[]");
}

#[test]
fn test_step_comma_is_rejected() {
    // `,` is bracket-only (tuple fields, type args, select sources); steps are separated by
    // `;` or a newline.
    quiver().evaluate("1, 2").expect_parse_failure();
    quiver().evaluate("x = { 1, 2 }; x").expect_parse_failure();
    quiver()
        .evaluate("5 { =5 => 1, 2 | 0 }")
        .expect_parse_failure();
}
