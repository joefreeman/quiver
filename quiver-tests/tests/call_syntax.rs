// Tests for the argument-first call syntax: `[args] ~> f` and `x ~> f`, with adjacent
// brackets reserved for tuple construction / spread.
mod common;
use common::*;

const ADD: &str = "add = #['int, 'int] { __integer_add__ },";
const INC: &str = "inc = #'int { [~, 1] ~> %num.add },";

#[test]
fn spaced_bracket_call() {
    quiver()
        .evaluate(&format!("{ADD} [3, 4] ~> add"))
        .expect("7");
}

#[test]
fn bare_argument_call() {
    // `x ~> f` applies f to the bare value x (not wrapped in a tuple).
    quiver().evaluate(&format!("{INC} 5 ~> inc")).expect("6");
    quiver()
        .evaluate(&format!("{INC} x = 5, x ~> inc"))
        .expect("6");
}

#[test]
fn adjacent_call_is_a_parse_error() {
    quiver()
        .evaluate("add = #['int, 'int] { __integer_add__ }, add[3, 4]")
        .expect_parse_failure();
}

#[test]
fn nil_call_is_spaced() {
    quiver().evaluate("f = #{ 42 }, [] ~> f").expect("42");
}

#[test]
fn named_tuple_stays_adjacent() {
    quiver()
        .evaluate("Point[x: 1, y: 2]")
        .expect("Point[x: 1, y: 2]");
}

#[test]
fn spread_stays_adjacent() {
    quiver()
        .evaluate("a = A[x: 1, y: 2], a[..., y: 3]")
        .expect("A[x: 1, y: 3]");
}

#[test]
fn field_access_call_is_spaced() {
    quiver()
        .evaluate("m = [add: #['int, 'int] { __integer_add__ }], [3, 4] ~> m.add")
        .expect("7");
}

#[test]
fn tail_call_is_spaced() {
    quiver()
        .evaluate(
            "count_down = #'int {
               | =0 => Done
               | [~, 1] ~> %num.sub ~> ^
             },
             3 ~> count_down",
        )
        .expect("Done");
}

#[test]
fn bare_amp_passes_function() {
    quiver()
        .evaluate(&format!(
            "{INC} apply = #[#'int -> 'int, 'int] {{ $.1 ~> $.0 }}, [&inc, 5] ~> apply"
        ))
        .expect("6");
}

#[test]
fn multi_argument_application_is_a_parse_error() {
    // Application takes a single argument; `f 1 2` is rejected rather than being
    // silently treated as `(f 1)` followed by a stray statement `2`.
    quiver()
        .evaluate("f = #'int { $ }, f 1 2")
        .expect_parse_failure();
}

#[test]
fn application_does_not_cross_newline() {
    // A newline separates statements; `f` on its own line is not applied to the next.
    quiver().evaluate("x = 5\nx").expect("5");
}

#[test]
fn comment_after_call() {
    quiver()
        .evaluate(&format!("{ADD} [3, 4] ~> add  // sum"))
        .expect("7");
}
