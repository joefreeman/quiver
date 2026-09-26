use crate::common::*;
use quiver_compiler::compiler::Error;

#[test]
fn test_ripple_yields_its_position() {
    quiver().evaluate("[42] ~> =[~]").expect("42");
    quiver().evaluate("[42] ~> =[~] ~> =42").expect("42");
    quiver()
        .evaluate("Point[x: 1, y: 2] ~> =Point[x: _, y: ~]")
        .expect("2");
    quiver().evaluate("A[B[7]] ~> =A[B[~]]").expect("7");
}

#[test]
fn test_ripple_continues_the_chain() {
    // The value at the ripple flows on, typed as that position.
    quiver()
        .evaluate("Ok[41] ~> =Ok[~] ~> %num.add [~, 1]")
        .expect("42");
    quiver()
        .evaluate("f = #(Ok['int] | Err['bin]) { =Ok[~] ~> %num.add [~, 1] }; [f Ok[1], f Err[<>]]")
        .expect("[2, []]");
    quiver()
        .evaluate("[1] ~> =[~] ~> %str.concat [~, \"x\"]")
        .expect_type_mismatch();
}

#[test]
fn test_repeated_ripples_must_be_equal() {
    quiver()
        .evaluate("[1, [1, 2], 3] ~> =[~, [~, 'int], _] ~> =1")
        .expect("1");
    quiver()
        .evaluate("{ [1, [2, 2], 3] ~> =[~, [~, _], _] }")
        .expect("[]");
}

#[test]
fn test_ripple_as_function_result() {
    quiver()
        .evaluate("unwrap = #(Ok['int] | Err['bin]) { =Ok[~] }; [unwrap Ok[4], unwrap Err[<01>]]")
        .expect("[4, []]");
}

#[test]
fn test_ripple_in_binding_form() {
    quiver().evaluate("v = [7] ~> =[~]; v").expect("7");
    quiver().evaluate("[~] = [7]").expect("7");
}

#[test]
fn test_ripple_in_partial_pattern() {
    quiver().evaluate("[x: 1, y: 2] ~> =(y: ~)").expect("2");
    quiver()
        .evaluate("Point[x: 1, y: 2] ~> =Point(x: ~)")
        .expect("1");
}

#[test]
fn test_ripple_in_alternation() {
    quiver()
        .evaluate("f = #(A['int] | B['int]) { =(A[~] | B[~]) }; [f A[1], f B[2]]")
        .expect("[1, 2]");
    // A bare ripple is the whole value, so this unwraps an `Ok` and passes anything else on.
    quiver()
        .evaluate("f = #(Ok['int] | Other) { =(Ok[~] | ~) }; [f Ok[3], f Other]")
        .expect("[3, Other]");
}

#[test]
fn test_ripple_in_conjunction() {
    // A bare ripple conjunct yields the value at the type the other conjuncts narrowed it to.
    quiver()
        .evaluate("f = #('int | 'bin) { =(~ & 'int) ~> %num.add [~, 1] }; [f 1, f <01>]")
        .expect("[2, []]");
    quiver()
        .evaluate("Point[x: 1, y: 2] ~> =(Point[x: ~, y: _] & p) ~> [~, p.y]")
        .expect("[1, 2]");
}

#[test]
fn test_bare_ripple_is_the_scrutinee() {
    quiver().evaluate("[3] ~> =~").expect("[3]");
}

#[test]
fn test_ripple_alternatives_must_agree() {
    quiver()
        .evaluate("f = #(A['int] | B) { =(A[~] | B) }; f B")
        .expect_compile_error(Error::OrPatternRippleMismatch);
}

#[test]
fn test_ripple_cannot_be_negated() {
    quiver()
        .evaluate("[5] ~> =\\[~]")
        .expect_compile_error(Error::NegatedPatternRipple);
}

#[test]
fn test_ripple_cannot_be_asserted() {
    quiver()
        .evaluate("[3]\n//= [~]")
        .expect_compile_error(Error::AssertionRipple);
}

#[test]
fn test_ripple_does_not_open_a_continuation() {
    // `~>` after a pattern is still the chain continuation, not a ripple.
    quiver()
        .evaluate("[5] ~> =[x] ~> [~, x]")
        .expect("[[5], 5]");
}
