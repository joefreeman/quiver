mod common;
use common::*;

#[test]
fn test_block_scope() {
    quiver().evaluate("x = 1; { x = 2 }; x").expect("1");
}

#[test]
fn test_block_evaluation() {
    quiver().evaluate("x = { 1, 2, 3 }; x").expect("3");
}

#[test]
fn test_block_with_closure() {
    quiver()
        .evaluate("x = 1; { 2 ~> =y, [x, y] ~> __add__ }")
        .expect("3");
}

#[test]
fn test_block_with_parameter() {
    quiver().evaluate("1 ~> { ~> =x => x }").expect("1");
}

#[test]
fn test_block_with_repeated_parameter() {
    quiver()
        .evaluate("3 ~> { ~> =x => [x, x] ~> __add__ }")
        .expect("6");
}

#[test]
fn test_block_with_positional_parameter() {
    quiver().evaluate("[1, 2] ~> { ~> =x => x.0 }").expect("1");
}
