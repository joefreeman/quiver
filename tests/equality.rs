mod common;
use common::*;

#[test]
fn test_equal_integers() {
    quiver().evaluate("[42, 42] ~> ==").expect_int(42);
}

#[test]
fn test_unequal_integers() {
    quiver().evaluate("[1, 2] ~> ==").expect_nil();
}

#[test]
fn test_equal_strings() {
    quiver()
        .evaluate("[\"abc\", \"abc\"] ~> ==")
        .expect_binary(0);
}

#[test]
fn test_not_equal() {
    // TODO: return ok?
    quiver().evaluate("[1, 2] ~> !=").expect_int(1);
}
