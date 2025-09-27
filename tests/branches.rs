mod common;
use common::*;

#[test]
fn test_branch_first_succeeds() {
    quiver().evaluate("{ 1, 2 | 3 }").expect("2");
}

#[test]
fn test_branch_first_fails_first_term() {
    quiver().evaluate("{ [], 2 | 3 }").expect("3");
}

#[test]
fn test_branch_first_fails_second_term() {
    quiver().evaluate("{ 1, [] | 3 }").expect("3");
}

#[test]
fn test_branch_with_consequence() {
    quiver().evaluate("{ 1 => 10 | 2 => 20 }").expect("10");
}

#[test]
fn test_branch_with_failing_consequence() {
    quiver().evaluate("{ 1 => [], 10 | 2 => 20 }").expect("[]");
}

#[test]
fn test_branch_pattern_matching() {
    quiver()
        .evaluate("B[42] ~> { A[a] => 1 | B[b] => 2 }")
        .expect("2");
}

#[test]
fn test_string_match() {
    quiver()
        .evaluate("\"bar\" ~> { \"foo\" => 1 | \"bar\" => 2 }")
        .expect("2");
}
