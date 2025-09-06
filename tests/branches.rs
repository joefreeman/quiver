mod common;
use common::*;

#[test]
fn test_branch_first_succeeds() {
    quiver().evaluate("{ 1, 2 | 3 }").expect_int(2);
}

#[test]
fn test_branch_first_fails_first_term() {
    quiver().evaluate("{ [], 2 | 3 }").expect_int(3);
}

#[test]
fn test_branch_first_fails_second_term() {
    quiver().evaluate("{ 1, [] | 3 }").expect_int(3);
}

#[test]
fn test_branch_with_consequence() {
    quiver().evaluate("{ 1 => 10 | 2 => 20 }").expect_int(10);
}

#[test]
fn test_branch_with_failing_consequence() {
    quiver().evaluate("{ 1 => [], 10 | 2 => 20 }").expect_nil();
}

#[test]
fn test_branch_pattern_matching() {
    quiver()
        .evaluate("B[42] ~> { A[a] = $ => 1 | B[b] = $ => 2 }")
        .expect_int(2);
}

// #[test]
// fn test_branch_pattern_no_match() {
//     quiver()
//         .evaluate("C ~> { A = $ => 1 | B = $ => 2 }")
//         .expect_nil();
// }
