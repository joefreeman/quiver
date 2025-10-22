mod common;
use common::*;

#[test]
fn test_builtin() {
    quiver().evaluate("-5 ~> __abs__").expect("5");
}
