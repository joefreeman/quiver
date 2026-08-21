//! How numbers *render*.
//!
//! `%num`'s semantics are specified and checked in `std/docs/num.md`, which `quiv test` runs:
//! a `//= P` there matches the value, which is the right test for almost everything the
//! module does. It cannot reach these, because a surd's mathematical notation is a display
//! convention over an ordinary `Surd[a, b, n]` tuple — `√2` is what the formatter writes,
//! not what the value is, and no pattern can assert it. So these stay here, where the
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
