mod common;
use common::*;

#[test]
fn test_builtin() {
    quiver().evaluate("-5 ~> <abs>").expect("5")
}
