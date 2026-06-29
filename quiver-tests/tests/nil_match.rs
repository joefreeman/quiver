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
fn test_nil_match_of_prior_result() {
    // `=[]` tests the result of a preceding guard: a failed equality ([]) matches nil...
    quiver().evaluate("a = 1, 2 =&a =[]").expect("Ok");
    // ...while a successful one (Ok) does not.
    quiver().evaluate("a = 42, 42 =&a =[]").expect("[]");
}

#[test]
fn test_double_nil_match() {
    quiver().evaluate("[] =[] =[]").expect("[]");
    quiver().evaluate("42 =[] =[]").expect("Ok");
}
