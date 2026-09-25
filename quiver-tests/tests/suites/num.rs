//! How numbers *render*.
//!
//! `%num`'s semantics are specified and checked in `std/docs/num.md`, which `quiv test` runs:
//! a `//= P` there matches the value, which is the right test for almost everything the
//! module does. It cannot reach these, because the mathematical notation of a surd, a
//! transcendental polynomial or a logarithmic form is a display convention over an ordinary
//! tuple — `√2` and `2π` are what the formatter writes, not what the values are, and no
//! pattern can assert it. So these stay here, where the
//! assertion compares the formatted string.

use crate::common::*;

#[test]
fn test_surd_formatting() {
    quiver().evaluate("2 ~> %num.sqrt ~").expect("√2");
    quiver().evaluate("8 ~> %num.sqrt ~").expect("2√2");
    quiver().evaluate("1/2 ~> %num.sqrt ~").expect("(1/2)√2");
    quiver()
        .evaluate("s = 2 ~> %num.sqrt ~; [1, s] ~> %num.add ~")
        .expect("1 + √2");
    quiver()
        .evaluate("s = 2 ~> %num.sqrt ~; n = s ~> %num.neg ~; [3, n] ~> %num.add ~")
        .expect("3 - √2");
    quiver()
        .evaluate("2 ~> %num.sqrt ~ ~> %num.neg ~")
        .expect("-√2");
    // φ = 1/2 + (1/2)√5.
    quiver()
        .evaluate("f = 5 ~> %num.sqrt ~; s = [1, f] ~> %num.add ~; [s, 2] ~> %num.div ~")
        .expect("1/2 + (1/2)√5");
}

#[test]
fn test_transcendental_formatting() {
    let cases = [
        ("%num.pi", "π"),
        ("%num.mul [2, %num.pi]", "2π"),
        ("%num.div [%num.pi, 4]", "(1/4)π"),
        ("%num.mul [%num.pi, %num.pi]", "π²"),
        ("%num.div [180, %num.pi]", "180π⁻¹"),
        ("%num.add [1, %num.pi]", "1 + π"),
        ("%num.sub [3, %num.mul [2, %num.pi]]", "3 - 2π"),
        ("%num.pi ~> %num.neg ~", "-π"),
        ("%num.e", "e"),
        ("%num.exp 2", "e²"),
        ("%num.exp 1/2", "e^(1/2)"),
        ("%num.exp -3/2", "e^(-3/2)"),
        ("%num.exp -1", "e⁻¹"),
        ("%num.add [2, %num.mul [3, %num.e]]", "2 + 3e"),
    ];
    for (source, expected) in cases {
        quiver().evaluate(source).expect(expected);
    }
}

#[test]
fn test_logarithm_formatting() {
    let cases = [
        ("%num.ln 2", "ln 2"),
        ("%num.ln 12", "2 ln 2 + ln 3"),
        ("%num.ln 3/4", "-2 ln 2 + ln 3"),
        ("%num.add [1, %num.ln 2]", "1 + ln 2"),
        (
            "%num.ln 3 ~> %num.mul [1/2, ~] ~> %num.sub [1, ~]",
            "1 - (1/2) ln 3",
        ),
        ("%num.ln 2 ~> %num.neg ~", "-ln 2"),
    ];
    for (source, expected) in cases {
        quiver().evaluate(source).expect(expected);
    }
}
