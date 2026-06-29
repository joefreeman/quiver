mod common;
use common::*;

// Matching against the nil pattern `=[]` is the test for nil: it evaluates to Ok when the value is
// nil and [] otherwise. (This replaces the former `<>` operator.)

#[test]
fn test_nil_match_nil() {
    quiver().evaluate("[] =[]").expect("Ok");
}

#[test]
fn test_nil_match_ok() {
    quiver().evaluate("Ok =[]").expect("[]");
}

#[test]
fn test_nil_match_integer() {
    quiver().evaluate("42 =[]").expect("[]");
}

#[test]
fn test_nil_match_tuple() {
    quiver().evaluate("[1, 2] =[]").expect("[]");
}

#[test]
fn test_nil_match_equality_result() {
    quiver().evaluate("[1, 2] == =[]").expect("Ok");
    quiver().evaluate("[42, 42] == =[]").expect("[]");
}

#[test]
fn test_double_nil_match() {
    quiver().evaluate("[] =[] =[]").expect("[]");
    quiver().evaluate("42 =[] =[]").expect("Ok");
}
