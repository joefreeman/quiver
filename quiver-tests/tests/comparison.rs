mod common;
use common::*;

#[test]
fn test_less_than_true() {
    quiver().evaluate("[1, 5] ~> <compare> ~> =-1").expect("Ok");
}

#[test]
fn test_less_than_false() {
    quiver().evaluate("[3, 2] ~> <compare> ~> =-1").expect("[]");
}

#[test]
fn test_greater_than() {
    quiver().evaluate("[7, 2] ~> <compare> ~> =1").expect("Ok");
}

#[test]
fn test_less_than_or_equal() {
    quiver()
        .evaluate("[1, 2] ~> <compare> ~> { ~> =-1 | ~> =0 }")
        .expect("Ok");
}

#[test]
fn test_greater_than_or_equal() {
    quiver()
        .evaluate("[4, 3] ~> <compare> ~> { ~> =0 | ~> =1 }")
        .expect("Ok");
}
