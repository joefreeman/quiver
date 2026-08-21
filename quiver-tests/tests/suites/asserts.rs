use crate::common::*;

// `//= P` assertions observe the value flowing at the end of the line they terminate: in a
// debug build that value is matched against the pattern and a mismatch aborts, like a violated
// contract; a release build skips the check but still parses and type-checks the pattern, so
// types are identical across build modes. The value flows on unchanged either way — an asserted
// nil still short-circuits — and the pattern may not bind.

#[test]
fn test_assertion_passes_and_value_flows() {
    quiver().debug().evaluate("5 //= 5").expect("5");
}

#[test]
fn test_binding_chain_observes_its_value_not_the_verdict() {
    // The binding matches the chain's result, and the assertion ends the chain — so it sees
    // what is about to be bound, not the `Ok` the step goes on to evaluate to.
    quiver().debug().evaluate("x = 5 //= 5\nx").expect("5");
    quiver()
        .debug()
        .evaluate("x = 5 //= Ok\nx")
        .expect_compile_error(quiver_compiler::compiler::Error::PatternNoMatchingTypes {
            pattern: "Ok".to_string(),
        });
    // The verdict is what the match spelling's chain evaluates to, so that is where it is
    // observable — including a failing one, which short-circuits as usual.
    quiver()
        .debug()
        .evaluate("{ 5 ~> =6 //= []\nUnreached | Reached }")
        .expect("Reached");
}

#[test]
fn test_assertion_mismatch_aborts_in_debug() {
    quiver()
        .debug()
        .evaluate("5 //= 6")
        .expect_runtime_error(quiver_core::error::Error::Panic(
            "Assertion '6' failed at test:1:3".to_string(),
        ));
}

#[test]
fn test_release_skips_the_check() {
    quiver().evaluate("5 //= 6").expect("5");
}

#[test]
fn test_unsatisfiable_pattern_rejected_in_both_modes() {
    // An assertion that can never match is a stale expectation — a compile error even in
    // release builds, where the check itself costs nothing.
    quiver().evaluate(r#"5 //= "no""#).expect_compile_error(
        quiver_compiler::compiler::Error::PatternNoMatchingTypes {
            pattern: "\"no\"".to_string(),
        },
    );
    // The exact tuple-name rule reaches assertions: an unnamed pattern never matches a
    // named value.
    quiver()
        .debug()
        .evaluate("Point[1, 2] //= [1, 2]")
        .expect_compile_error(quiver_compiler::compiler::Error::PatternNoMatchingTypes {
            pattern: "[1, 2]".to_string(),
        });
}

#[test]
fn test_assertion_bindings_rejected() {
    quiver().debug().evaluate("5 //= z").expect_compile_error(
        quiver_compiler::compiler::Error::AssertionBindings {
            bindings: vec!["z".to_string()],
        },
    );
    // Rejected in release builds too — the check would never run there.
    quiver().evaluate("5 //= ('int)n").expect_compile_error(
        quiver_compiler::compiler::Error::AssertionBindings {
            bindings: vec!["n".to_string()],
        },
    );
    // A star pattern's bindings are type-derived rather than written, and are still bindings.
    quiver()
        .debug()
        .evaluate("[x: 1] //= *")
        .expect_compile_error(quiver_compiler::compiler::Error::AssertionBindings {
            bindings: vec!["x".to_string()],
        });
}

#[test]
fn test_nil_is_assertable_and_still_short_circuits() {
    // The assertion observes: a passing `//= []` does not turn the nil step into Ok.
    quiver()
        .debug()
        .evaluate("{ | [] //= []\nUnreached | Reached }")
        .expect("Reached");
    // A chain-final match's verdict is the step's value, so its failure is assertable.
    quiver()
        .debug()
        .evaluate("{ | 42 ~> =41 //= []\nUnreached | Reached }")
        .expect("Reached");
}

#[test]
fn test_pattern_vocabulary() {
    // Pins, types, tuples, and alternations all assert; none bind.
    quiver()
        .debug()
        .evaluate("y = 2; 2 //= &y\nDone")
        .expect("Done");
    quiver().debug().evaluate("5 //= 'int").expect("5");
    quiver()
        .debug()
        .evaluate("Point[x: 1, y: 2] //= Point[x: 1, y: 2]")
        .expect("Point[x: 1, y: 2]");
    quiver()
        .debug()
        .evaluate("3 ~> { =0 => Zero | Other } //= (Zero | Other)")
        .expect("Other");
}

#[test]
fn test_prose_note_after_three_spaces() {
    quiver()
        .debug()
        .evaluate("5 //= 5   an explanatory note, to end of line")
        .expect("5");
    // Three-space runs inside a string pattern are pattern, not note.
    quiver()
        .debug()
        .evaluate(r#""a   b" //= "a   b""#)
        .expect("\"a   b\"");
}

// A chain spread over lines can assert on each of them: an assertion ends its line, so what
// follows is necessarily a `~>` continuation, and the value it observes is the one flowing into
// that continuation.

#[test]
fn test_mid_chain_assertions_observe_each_line() {
    quiver()
        .debug()
        .evaluate("1 //= 1\n~> %num.add [~, 2] //= 3\n~> %num.mul [~, 3] //= 9")
        .expect("9");
    quiver()
        .debug()
        .evaluate("1 //= 1\n~> %num.add [~, 2] //= 4")
        .expect_runtime_error(quiver_core::error::Error::Panic(
            "Assertion '4' failed at test:2:20".to_string(),
        ));
    // The head's value, before anything has consumed it.
    quiver()
        .debug()
        .evaluate("1 //= 2\n~> %num.add [~, 2]")
        .expect_runtime_error(quiver_core::error::Error::Panic(
            "Assertion '2' failed at test:1:3".to_string(),
        ));
    // A stale mid-chain expectation fails the build, in either mode, as a step-final one does.
    quiver()
        .evaluate("1 //= Ok\n~> %num.add [~, 2]")
        .expect_compile_error(quiver_compiler::compiler::Error::PatternNoMatchingTypes {
            pattern: "Ok".to_string(),
        });
}

#[test]
fn test_mid_chain_assertion_forms() {
    // Stacked, on their own lines, mid-chain.
    quiver()
        .debug()
        .evaluate("1\n//= 'int\n//= 1\n~> %num.add [~, 2]")
        .expect("3");
    // In a value chain — a tuple field spread over lines — where there is no step to end.
    quiver()
        .debug()
        .evaluate("[a: 1 //= 1\n~> %num.add [~, 2]]")
        .expect("[a: 3]");
    // A binding's value chain is asserted the same way, line by line.
    quiver()
        .debug()
        .evaluate("x = 1 //= 1\n~> %num.add [~, 2] //= 3\nx")
        .expect("3");
}

#[test]
fn test_comments_may_sit_in_a_continuation_gap() {
    quiver()
        .debug()
        .evaluate("1 // the head\n// and a note above the continuation\n~> %num.add [~, 2]")
        .expect("3");
    // Alongside an assertion, in either order.
    quiver()
        .debug()
        .evaluate("1 //= 1\n// a note\n~> %num.add [~, 2]")
        .expect("3");
    quiver()
        .debug()
        .evaluate("1 // a note\n//= 1\n~> %num.add [~, 2]")
        .expect("3");
}

// A leading `//=` continues the step above — the trailing form with a line break — so it
// observes that step's value, and any run of newlines, blank lines, comments and `;`
// separators may sit between. At the start of a sequence there is no step to continue: the
// chain is empty and the assertion observes the block's input, exactly as a bare `~` step
// would.

#[test]
fn test_own_line_assertion_continues_the_step() {
    quiver()
        .debug()
        .evaluate("5 ~> [~, 1]\n//= [5, 1]")
        .expect("[5, 1]");
    quiver()
        .debug()
        .evaluate("5\n//= 6")
        .expect_runtime_error(quiver_core::error::Error::Panic(
            "Assertion '6' failed at test:2:1".to_string(),
        ));
    // Release builds skip the check, as with the trailing form.
    quiver().evaluate("5\n//= 6").expect("5");
}

#[test]
fn test_own_line_assertion_attaches_across_separators() {
    // Blank lines, comments, and `;` (a newline's synonym) all belong to the step.
    quiver()
        .debug()
        .evaluate("x = 5;\n\n// carried\n//= 5\nx")
        .expect("5");
}

#[test]
fn test_stacked_assertions_each_fire() {
    quiver().debug().evaluate("5\n//= 'int\n//= 5").expect("5");
    quiver()
        .debug()
        .evaluate("5\n//= 'int\n//= 6")
        .expect_runtime_error(quiver_core::error::Error::Panic(
            "Assertion '6' failed at test:3:1".to_string(),
        ));
}

#[test]
fn test_opening_assertion_observes_the_block_input() {
    quiver().debug().evaluate("5 ~> { //= 5\nOk }").expect("Ok");
    // The empty chain's value is the input, so it is also the step's value.
    quiver().debug().evaluate("5 ~> { //= 5\n}").expect("5");
    quiver()
        .debug()
        .evaluate("f = #\'int { //= 5\n$ }; f 6")
        .expect_runtime_error(quiver_core::error::Error::Panic(
            "Assertion '5' failed at test:1:13".to_string(),
        ));
}

#[test]
fn test_opening_assertion_on_nil_input_still_gates() {
    // An assertion-only step is a `~` step: with a nil input it short-circuits its
    // sequence, assertion verdict notwithstanding.
    quiver()
        .debug()
        .evaluate("{ | //= []\nUnreached | Reached }")
        .expect("Reached");
}

#[test]
fn test_assertion_inside_stripped_block_still_fires() {
    // The compiler splices redundant blocks away; one whose body carries an assertion
    // must keep it firing.
    quiver()
        .debug()
        .evaluate("{ 5 //= 6\n}")
        .expect_runtime_error(quiver_core::error::Error::Panic(
            "Assertion '6' failed at test:1:5".to_string(),
        ));
}
