mod common;
use common::*;

#[test]
fn test_block_scope() {
    quiver().evaluate("x = 1, { x = 2 }, x").expect_int(1);
}

#[test]
fn test_block_evaluation() {
    quiver().evaluate("x = { 1, 2, 3 }, x").expect_int(3);
}

#[test]
fn test_block_with_closure() {
    quiver()
        .evaluate("x = 1, { y = 2, [x, y] ~> <add> }")
        .expect_int(3);
}

#[test]
fn test_block_with_parameter() {
    quiver().evaluate("1 ~> { $ }").expect_int(1);
}

#[test]
fn test_block_with_repeated_parameter() {
    quiver().evaluate("3 ~> { [$, $] ~> <add> }").expect_int(6);
}

#[test]
fn test_block_with_positional_parameter() {
    quiver().evaluate("[1, 2] ~> { $.0 }").expect_int(1);
}
