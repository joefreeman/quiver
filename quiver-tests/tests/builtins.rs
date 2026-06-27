mod common;
use common::*;

#[test]
fn test_builtin() {
    quiver().evaluate("-5 __integer_abs__").expect("5");
}
