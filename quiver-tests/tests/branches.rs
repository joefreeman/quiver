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
        .evaluate("B[42] ~> { =A[a] => 1 | =B[b] => 2 }")
        .expect("2");
}

#[test]
fn test_use_continuation_in_consequence() {
    // Consequence receives the block parameter via implicit continuation
    quiver()
        .evaluate("42 ~> { =10 => 100 | =42 => [~, ~] }")
        .expect("[42, 42]");
}

#[test]
fn test_string_match() {
    quiver()
        .evaluate("\"bar\" ~> { =\"foo\" => 1 | =\"bar\" => 2 }")
        .expect("2");
}

#[test]
fn test_consequence_ripple_is_block_parameter_not_condition_result() {
    // When a condition transforms its input (e.g., returns Ok), the consequence's
    // ripple (~) should still refer to the block parameter, not the condition result.
    // Here ok? returns Ok when input is 0, but ~ in the consequence should be 0.
    quiver()
        .evaluate(
            r#"
            ok? = #int { =0 => Ok },
            0 ~> { | ok? => ~ | 999 }
            "#,
        )
        .expect("0");
}

#[test]
fn test_consequence_ripple_in_tuple() {
    // Verify ripple works correctly in tuple construction within consequence
    quiver()
        .evaluate(
            r#"
            ok? = #int { =0 => Ok },
            0 ~> { | ok? => [~, 1] | [~, 2] }
            "#,
        )
        .expect("[0, 1]");
}

#[test]
fn test_consequence_ripple_fallback_branch() {
    // When first branch fails, fallback branch should also get block parameter
    quiver()
        .evaluate(
            r#"
            ok? = #int { =0 => Ok },
            5 ~> { | ok? => [~, 1] | [~, 2] }
            "#,
        )
        .expect("[5, 2]");
}
