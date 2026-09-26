use crate::common::*;
use quiver_compiler::compiler::Error;

#[test]
fn test_negated_nil() {
    quiver().evaluate("5 ~> =\\[]").expect("5");
    quiver().evaluate("[] ~> =\\[]").expect("[]");
    quiver()
        .evaluate("f = #('int | []) { | =\\[] => Some | None }; [f 1, f []]")
        .expect("[Some, None]");
}

#[test]
fn test_negated_type_literal_and_pin() {
    quiver()
        .evaluate("f = #_ { | =\\'int => Other | Int }; [f 1, f <01>, f []]")
        .expect("[Int, Other, Other]");
    quiver()
        .evaluate("f = #'int { | =\\42 => Other | Answer }; [f 42, f 1]")
        .expect("[Answer, Other]");
    quiver()
        .evaluate("x = 3; f = #'int { | =\\^x => Other | Same }; [f 3, f 4]")
        .expect("[Same, Other]");
}

#[test]
fn test_negated_alternation() {
    quiver()
        .evaluate("f = #('int | 'bin | []) { | =\\('bin | []) => Int | Rest }; [f 1, f <01>, f []]")
        .expect("[Int, Rest, Rest]");
    quiver()
        .evaluate("f = #'int { | =\\(0 | 1) => Big | Small }; [f 0, f 1, f 2]")
        .expect("[Small, Small, Big]");
}

#[test]
fn test_negated_field() {
    quiver()
        .evaluate(
            "f = #[b: ('int | 'bin)] { | =[b: \\'int] => Bin | Int }
             [f [b: 1], f [b: <01>]]",
        )
        .expect("[Int, Bin]");
    quiver()
        .evaluate(
            "f = #'%list<'int> { | =Cons[_, \\Nil] => Long | Short }
             [f %list{1, 2}, f %list{1}, f Nil]",
        )
        .expect("[Long, Short, Short]");
}

#[test]
fn test_negation_narrows_to_complement() {
    // `$` is `'int` in the first branch, so the addition type-checks.
    quiver()
        .evaluate("f = #('int | []) { | =\\[] => %num.add [$, 1] | 0 }; [f 4, f []]")
        .expect("[5, 0]");
    quiver()
        .evaluate("f = #'%list<'int> { | =\\Nil => $.0 | 0 }; [f %list{7, 8}, f Nil]")
        .expect("[7, 0]");
    // A binder over a negation narrows too.
    quiver()
        .evaluate(
            "g = #'bin { $ }
             f = #[b: ('int | 'bin)] { | $ ~> =[b: (\\'int & v)] => g v | None }
             [f [b: 1], f [b: <01>]]",
        )
        .expect("[None, <01>]");
}

#[test]
fn test_value_negation_does_not_narrow() {
    // `\42` says nothing about the type, so `$` stays `'int` — and a later branch still
    // sees 42, rather than being narrowed away by the negation's complement.
    quiver()
        .evaluate("f = #'int { | =\\42 => A | %num.add [$, 1] }; [f 1, f 42]")
        .expect("[A, 43]");
}

#[test]
fn test_exact_negation_narrows_later_branches() {
    // Falling through `\[]` means the value was nil, so a later branch sees it as nil: `g`
    // takes only nil.
    quiver()
        .evaluate(
            "g = #[] { Nil }
             f = #('int | []) { | =\\[] => A | g $ }
             [f 1, f []]",
        )
        .expect("[A, Nil]");
    quiver()
        .evaluate("f = #('int | []) { | =\\[] => A | ='int => B | C }; [f 1, f []]")
        .expect("[A, C]");
}

#[test]
fn test_inexact_negation_does_not_narrow_later_branches() {
    // Falling through `\42` says only that the value was 42, which no type expresses: a later
    // branch keeps the full `'int` rather than being narrowed away.
    quiver()
        .evaluate("f = #'int { | =\\42 => A | %num.add [$, 1] }; [f 1, f 42]")
        .expect("[A, 43]");
}

#[test]
fn test_strict_binder() {
    // `(\[] & x)` binds `x` and fails the step when the value is nil.
    quiver()
        .evaluate("f = #('int | []) { (\\[] & n) = $; %num.add [n, 1] }; [f 1, f []]")
        .expect("[2, []]");
    quiver()
        .evaluate("f = #('int | []) { $ ~> =(\\[] & n); n }; f")
        .expect_type("#('int | []) -> ('int | [])");
}

#[test]
fn test_negated_statically_decided() {
    // `\'bin` on an int always matches; `\'int` on an int never does.
    quiver().evaluate("5 ~> =\\'bin").expect("5");
    quiver()
        .evaluate("f = #'int { | =\\'int => A | B }; f 5")
        .expect("B");
}

#[test]
fn test_negation_cannot_bind_where_it_cannot_match() {
    // `B[x]` can never match an `A`, but the binding inside the negation is still rejected.
    quiver()
        .evaluate("f = #A['int] { | =\\B[x] => Yes | No }; f A[1]")
        .expect_compile_error(Error::NegatedPatternBindings {
            bindings: vec!["x".to_string()],
        });
    quiver()
        .evaluate("f = #A['int] { | =\\B[~] => Yes | No }; f A[1]")
        .expect_compile_error(Error::NegatedPatternRipple);
}

#[test]
fn test_negation_cannot_bind() {
    quiver()
        .evaluate("5 ~> =\\x")
        .expect_compile_error(Error::NegatedPatternBindings {
            bindings: vec!["x".to_string()],
        });
    quiver()
        .evaluate("[1, 2] ~> =\\[a, b]")
        .expect_compile_error(Error::NegatedPatternBindings {
            bindings: vec!["a".to_string(), "b".to_string()],
        });
}

#[test]
fn test_useless_negations_are_errors() {
    quiver()
        .evaluate("5 ~> =\\_")
        .expect_compile_error(Error::NegatedWildcard);
    quiver()
        .evaluate("5 ~> =\\\\5")
        .expect_compile_error(Error::DoubleNegation);
}

#[test]
fn test_negated_tuple_with_field_test() {
    // `\A[b: 'int]`: anything but an `A` whose `b` is an int — nil and other shapes included.
    quiver()
        .evaluate(
            "f = #(A[b: ('int | 'bin)] | B | []) { | =\\A[b: 'int] => Yes | No }
             [f A[b: 1], f A[b: <01>], f B, f []]",
        )
        .expect("[No, Yes, Yes, Yes]");
    quiver()
        .evaluate(
            "f = #_ { | =\\A[b: 'int] => Yes | No }
             [f A[b: 1], f A[b: <01>], f B, f 5]",
        )
        .expect("[No, Yes, Yes, Yes]");
    // When the tuple's shape is fully decided by the type, it narrows to the rest.
    quiver()
        .evaluate(
            "g = #(B | []) { Ok }
             f = #(A[b: 'int] | B | []) { | =\\A[b: 'int] => g $ | No }
             [f A[b: 1], f B, f []]",
        )
        .expect("[No, Ok, Ok]");
}

#[test]
fn test_nested_negation() {
    // `\A[b: \'int]` is not a double negation: it matches anything but an `A` whose `b` is
    // *not* an int.
    quiver()
        .evaluate(
            "f = #(A[b: ('int | 'bin)] | B | []) { | =\\A[b: \\'int] => Yes | No }
             [f A[b: 1], f A[b: <01>], f B, f []]",
        )
        .expect("[Yes, No, Yes, Yes]");
    quiver()
        .evaluate(
            "f = #_ { | =\\A[b: \\'int] => Yes | No }
             [f A[b: 1], f A[b: <01>], f B, f 5]",
        )
        .expect("[Yes, No, Yes, Yes]");
    // The inner negation is exact, so the outer one narrows too: `$` is `A[b: 'int] | B | []`
    // in the first branch, which `g` takes.
    quiver()
        .evaluate(
            "g = #(A[b: 'int] | B | []) { Ok }
             f = #(A[b: ('int | 'bin)] | B | []) { | =\\A[b: \\'int] => g $ | No }
             [f A[b: 1], f A[b: <01>], f B]",
        )
        .expect("[Ok, No, Ok]");
    // Over an inexact inner negation (a value test), the outer one narrows nothing.
    quiver()
        .evaluate(
            "g = #(A[b: 'int] | B | []) { Ok }
             f = #(A[b: 'int] | B | []) { | =\\A[b: \\5] => g $ | No }
             [f A[b: 1], f A[b: 5], f B]",
        )
        .expect("[No, Ok, Ok]");
    // Nor can a binder appear inside the inner negation, however deep.
    quiver()
        .evaluate("f = #(A[b: ('int | 'bin)] | B) { | =\\A[b: (\\'int & x)] => x | No }; f B")
        .expect_compile_error(Error::NegatedPatternBindings {
            bindings: vec!["x".to_string()],
        });
}

#[test]
fn test_negation_over_the_top_type_stays_a_runtime_test() {
    // `_` less `'int` is still `_`, so the complement is inexact: the test must stay at runtime
    // rather than the pattern being judged irrefutable.
    quiver()
        .evaluate("f = #_ { | =\\'int => Other | Int }; [f 1, f <01>, f []]")
        .expect("[Int, Other, Other]");
    // Over a recursive type the complement keeps members whole, so a later branch is not
    // narrowed by it either.
    quiver()
        .evaluate(
            "f = #'%list<'int> { | =\\Nil => $.0 | 0 }
             [f %list{7}, f Nil]",
        )
        .expect("[7, 0]");
}
