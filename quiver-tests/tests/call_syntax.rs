// Tests for the two call syntaxes: argument-first (`[args] ~> f`, `x ~> f`) and juxtaposed
// application (`f [args]`, `f x`, and the `^f`/`^`/`~`/`~.f`/`^~`/`@f`/`@~` family), with
// adjacent brackets reserved for tuple construction / spread.
mod common;
use common::*;

const ADD: &str = "add = #['int, 'int] { __integer_add__ ~ };";
const INC: &str = "inc = #'int { [~, 1] ~> %num.add ~ };";

#[test]
fn juxtaposed_bracket_call() {
    quiver().evaluate(&format!("{ADD} add [3, 4]")).expect("7");
}

#[test]
fn juxtaposed_bare_argument_call() {
    // `f x` applies f to the bare value x (not wrapped in a tuple).
    quiver().evaluate(&format!("{INC} inc 5")).expect("6");
    quiver()
        .evaluate(&format!("{INC} x = 5; inc x"))
        .expect("6");
}

#[test]
fn flowing_value_reaches_juxtaposed_argument() {
    // The flowing value flows into the argument of a juxtaposed call, so `add [~, 100]` works.
    quiver()
        .evaluate(&format!("{ADD} 5 ~> add [~, 100]"))
        .expect("105");
}

#[test]
fn juxtaposed_builtin_call() {
    quiver().evaluate("__integer_add__ [3, 4]").expect("7");
}

#[test]
fn juxtaposed_second_argument_is_a_parse_error() {
    // Juxtaposition takes exactly one argument; a second needs a `~>` (or `;`).
    quiver()
        .evaluate(&format!("{INC} inc 1 2"))
        .expect_parse_failure();
}

#[test]
fn juxtaposed_argument_to_nilary_is_a_type_error() {
    // A nilary function takes nil and nothing else. Every call is written, so there is no
    // spelling that quietly hands it something and no spelling that quietly drops one.
    quiver()
        .evaluate("f = #[] { 42 }; f [1, 2]")
        .expect_type_mismatch();
    quiver()
        .evaluate("f = #[] { 42 }; 5 ~> f ~")
        .expect_type_mismatch();
    quiver().evaluate("f = #[] { 42 }; f []").expect("42");
}

#[test]
fn juxtaposed_tail_calls() {
    // `^f [args]` — a named tail call with a juxtaposed argument.
    quiver()
        .evaluate(
            "g = #['int, 'int] { __integer_multiply__ ~ };
             f = #'int { ^g [~, 3] };
             f 7",
        )
        .expect("21");
    // `^ [args]` — self tail call.
    quiver()
        .evaluate(
            "f = #['int, 'int] {
               | =[0, acc] => acc
               | =[n, acc] => ^ [[n, 1] ~> %num.sub ~, [acc, n] ~> %num.add ~]
             };
             f [4, 0]",
        )
        .expect("10");
}

#[test]
fn juxtaposed_ripple_application() {
    // `~ [args]` applies the flowing value (a function) to the argument.
    quiver()
        .evaluate(&format!("{ADD} add ~> ~ [3, 4]"))
        .expect("7");
    // `~.f [args]` reads the callable off the flowing value, then applies it.
    quiver()
        .evaluate("m = [add: __integer_add__]; m ~> ~.add [1, 2]")
        .expect("3");
}

#[test]
fn juxtaposed_ripple_tail_call() {
    // `^~ arg` tail-calls the flowing function with an argument (a non-nilary target).
    quiver()
        .evaluate(
            "g = #'int { [~, 2] ~> %num.mul ~ };
             f = #'int { g ~> ^~ $ };
             f 5",
        )
        .expect("10");
}

#[test]
fn juxtaposed_spawn_argument() {
    // `@f x` supplies the spawned function's init argument.
    quiver()
        .evaluate("c = #'int { $ }; p = @c 42; !p")
        .expect("42");
    // `@~ x` spawns the flowing function with an init argument.
    quiver()
        .evaluate("c = #'int { $ }; p = c ~> @~ 7; !p")
        .expect("7");
    // An explicit init to a nilary process function is rejected.
    quiver()
        .evaluate("c = #[] { 5 }; @c 42")
        .expect_type_mismatch();
}

#[test]
fn non_callable_heads_are_not_applicable() {
    // A literal/tuple/function-literal head cannot take a juxtaposed argument.
    quiver().evaluate("5 six").expect_parse_failure();
    quiver().evaluate("#[] { 42 } 5").expect_parse_failure();
    quiver().evaluate("[1] [2]").expect_parse_failure();
}

#[test]
fn spaced_bracket_call() {
    quiver()
        .evaluate(&format!("{ADD} [3, 4] ~> add ~"))
        .expect("7");
}

#[test]
fn bare_argument_call() {
    // `x f` applies f to the bare value x (not wrapped in a tuple).
    quiver().evaluate(&format!("{INC} 5 ~> inc ~")).expect("6");
    quiver()
        .evaluate(&format!("{INC} x = 5; x ~> inc ~"))
        .expect("6");
}

#[test]
fn adjacent_call_is_a_parse_error() {
    quiver()
        .evaluate("add = #['int, 'int] { __integer_add__ ~ }; add[3, 4]")
        .expect_parse_failure();
}

#[test]
fn nil_call_is_spaced() {
    quiver().evaluate("f = #[] { 42 }; [] ~> f ~").expect("42");
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
        .evaluate("a = A[x: 1, y: 2]; a[..., y: 3]")
        .expect("A[x: 1, y: 3]");
}

#[test]
fn field_access_call_is_spaced() {
    quiver()
        .evaluate("m = [add: #['int, 'int] { __integer_add__ ~ }]; [3, 4] ~> m.add ~")
        .expect("7");
}

#[test]
fn tail_call_is_spaced() {
    quiver()
        .evaluate(
            "count_down = #'int {
               | =0 => Done
               | [~, 1] ~> %num.sub ~ ~> ^ ~
             };
             3 ~> count_down ~",
        )
        .expect("Done");
}

#[test]
fn bare_amp_passes_function() {
    quiver()
        .evaluate(&format!(
            "{INC} apply = #[#'int -> 'int, 'int] {{ $.1 ~> $.0 ~ }}; [inc, 5] ~> apply ~"
        ))
        .expect("6");
}

#[test]
fn application_does_not_cross_newline() {
    // A newline separates statements; `f` on its own line is not applied to the next.
    quiver().evaluate("x = 5\nx").expect("5");
}

#[test]
fn comment_after_call() {
    quiver()
        .evaluate(&format!("{ADD} [3, 4] ~> add ~  // sum"))
        .expect("7");
}
