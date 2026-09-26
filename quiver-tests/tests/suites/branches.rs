use crate::common::*;

#[test]
fn test_branch_first_succeeds() {
    quiver().evaluate("{ 1; 2 | 3 }").expect("2");
}

#[test]
fn test_branch_first_fails_first_term() {
    quiver().evaluate("{ []; 2 | 3 }").expect("3");
}

#[test]
fn test_branch_first_fails_second_term() {
    quiver().evaluate("{ 1; [] | 3 }").expect("3");
}

#[test]
fn test_branch_with_consequence() {
    quiver().evaluate("{ 1 => 10 | 2 => 20 }").expect("10");
}

#[test]
fn test_branch_with_failing_consequence() {
    quiver().evaluate("{ 1 => []; 10 | 2 => 20 }").expect("[]");
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
fn test_consequence_ripple_is_condition_result() {
    // A consequence starts from its condition's value: here `ok?` answers `Ok` for 0, so
    // that is what `~` is in the consequence.
    quiver()
        .evaluate(
            r#"
            ok? = #'int { =0 => Ok };
            0 ~> { | ok? ~ => ~ | 999 }
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_consequence_ripple_after_match_is_block_parameter() {
    // A match yields its scrutinee, so after one the consequence starts from the block's input.
    quiver()
        .evaluate("f = #('int | 'bin) { | ='int => [~, $] | No }; f 3")
        .expect("[3, 3]");
}

#[test]
fn test_consequence_ripple_after_ripple_match() {
    quiver()
        .evaluate("Lit[5] ~> { | =Lit[~] => %num.add [~, 1] | 0 }")
        .expect("6");
}

#[test]
fn test_consequence_uses_found_value() {
    // A lookup as the condition: the consequence gets what was found, with nil ruled out.
    quiver()
        .evaluate(
            r#"
            m = %dict{ "a" => 1 }
            f = #'%str { m ~> { | %dict.get [~, $] => %num.add [~, 10] | 0 } }
            [f "a", f "b"]
            "#,
        )
        .expect("[11, 0]");
}

#[test]
fn test_consequence_steps_all_start_from_condition_value() {
    quiver()
        .evaluate("Lit[5] ~> { | =Lit[~] => [~, 1]; [~, 2] | No }")
        .expect("[5, 2]");
}

#[test]
fn test_guard_ending_in_ripple_restores_input() {
    // After a guard, `~` would be the guard's answer; ending the condition with a `~` step
    // passes the block's input on instead.
    quiver()
        .evaluate("5 ~> { | %num.gt? [~, 0]; ~ => [~, 1] | No }")
        .expect("[5, 1]");
    // A nil input fails a plain `~` step, so a match (which succeeds on nil) passes it on.
    quiver()
        .evaluate("[] ~> { | =[]; =_ => [~, 1] | No }")
        .expect("[[], 1]");
}

#[test]
fn test_consequence_ripple_in_tuple() {
    // Verify ripple works correctly in tuple construction within consequence
    quiver()
        .evaluate(
            r#"
            ok? = #'int { =0 => Ok };
            0 ~> { | ok? ~ => [~, 1] | [~, 2] }
            "#,
        )
        .expect("[Ok, 1]");
}

#[test]
fn test_consequence_ripple_fallback_branch() {
    // When first branch fails, fallback branch should also get block parameter
    quiver()
        .evaluate(
            r#"
            ok? = #'int { =0 => Ok };
            5 ~> { | ok? ~ => [~, 1] | [~, 2] }
            "#,
        )
        .expect("[5, 2]");
}

// Branches (`|` and `=>`) belong to a block. A statement is a single branchless sequence, so a
// bare `|` at the statement level is a parse error - the branches must be wrapped in braces.

#[test]
fn test_toplevel_branch_requires_block() {
    quiver().evaluate("1 | 2").expect_parse_failure();
}

#[test]
fn test_toplevel_consequence_requires_block() {
    quiver().evaluate("1 => 2").expect_parse_failure();
}

#[test]
fn test_toplevel_branch_in_block() {
    quiver().evaluate("{ 1 | 2 }").expect("1");
}

#[test]
fn test_toplevel_sequence_bindings_persist() {
    // A statement is a branchless sequence sharing the enclosing scope, so its bindings persist.
    quiver().evaluate("x = 5; y = 10; [x, y]").expect("[5, 10]");
}
