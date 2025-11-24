mod common;
use common::*;

#[test]
fn test_add() {
    quiver().evaluate("%math.add[3, 4]").expect("7");
}

#[test]
fn test_sub() {
    quiver().evaluate("%math.sub[10, 3]").expect("7");
}

#[test]
fn test_mul() {
    quiver().evaluate("%math.mul[6, 7]").expect("42");
}

#[test]
fn test_div() {
    quiver().evaluate("%math.div[20, 4]").expect("5");
}

#[test]
fn test_mod() {
    quiver().evaluate("%math.mod[17, 5]").expect("2");
}

#[test]
fn test_div_by_zero_returns_nil() {
    quiver().evaluate("%math.div[10, 0]").expect("[]");
}

#[test]
fn test_mod_by_zero_returns_nil() {
    quiver().evaluate("%math.mod[10, 0]").expect("[]");
}

#[test]
fn test_mod_by_zero_with_fallback() {
    quiver().evaluate("{ %math.mod[10, 0] | -1 }").expect("-1");
}

#[test]
fn test_lt_true() {
    quiver().evaluate("%math.lt[5, 10]").expect("-1");
}

#[test]
fn test_lt_false() {
    quiver().evaluate("%math.lt[10, 5]").expect("[]");
}

#[test]
fn test_lt_equal() {
    quiver().evaluate("%math.lt[5, 5]").expect("[]");
}

#[test]
fn test_le_less() {
    quiver().evaluate("%math.le[5, 10]").expect("-1");
}

#[test]
fn test_le_equal() {
    quiver().evaluate("%math.le[5, 5]").expect("0");
}

#[test]
fn test_le_greater() {
    quiver().evaluate("%math.le[10, 5]").expect("[]");
}

#[test]
fn test_gt_true() {
    quiver().evaluate("%math.gt[10, 5]").expect("1");
}

#[test]
fn test_gt_false() {
    quiver().evaluate("%math.gt[5, 10]").expect("[]");
}

#[test]
fn test_gt_equal() {
    quiver().evaluate("%math.gt[5, 5]").expect("[]");
}

#[test]
fn test_ge_greater() {
    quiver().evaluate("%math.ge[10, 5]").expect("1");
}

#[test]
fn test_ge_equal() {
    quiver().evaluate("%math.ge[5, 5]").expect("0");
}

#[test]
fn test_ge_less() {
    quiver().evaluate("%math.ge[5, 10]").expect("[]");
}

#[test]
fn test_abs_positive() {
    quiver().evaluate("42 ~> %math.abs").expect("42");
}

#[test]
fn test_abs_negative() {
    quiver().evaluate("-42 ~> %math.abs").expect("42");
}

#[test]
fn test_abs_zero() {
    quiver().evaluate("0 ~> %math.abs").expect("0");
}

#[test]
fn test_sqrt() {
    quiver().evaluate("16 ~> %math.sqrt").expect("4");
}

#[test]
fn test_sqrt_truncates() {
    quiver().evaluate("17 ~> %math.sqrt").expect("4");
}

#[test]
fn test_sqrt_zero() {
    quiver().evaluate("0 ~> %math.sqrt").expect("0");
}

#[test]
fn test_sqrt_negative_returns_nil() {
    quiver().evaluate("-4 ~> %math.sqrt").expect("[]");
}

#[test]
fn test_sqrt_negative_with_fallback() {
    quiver().evaluate("-4 ~> { ~> %math.sqrt | 0 }").expect("0");
}
