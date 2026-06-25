mod common;
use common::*;

// Each program imports the `vec` module and binds `a = [1, 2, 3]` as an I32 vector with unit
// scale, then runs `expr`. Reductions (sum/dot/get/len) lower integer-valued results to plain
// integers, so assertions stay robust to the packed byte layout.
fn prog(expr: &str) -> String {
    format!(
        "vec = %vec\n\
         (prepend, new) = %list\n\
         xs = new ~> prepend [~, 3] ~> prepend [~, 2] ~> prepend [~, 1]\n\
         a = vec.of [I32, 1, xs]\n\
         {expr}"
    )
}

#[test]
fn test_sum() {
    quiver().evaluate(&prog("vec.sum a")).expect("6");
}

#[test]
fn test_len() {
    quiver().evaluate(&prog("vec.len a")).expect("3");
}

#[test]
fn test_get() {
    quiver().evaluate(&prog("vec.get [a, 0]")).expect("1");
    quiver().evaluate(&prog("vec.get [a, 2]")).expect("3");
}

#[test]
fn test_get_out_of_bounds_is_nil() {
    quiver().evaluate(&prog("vec.get [a, 3]")).expect("[]");
    quiver().evaluate(&prog("vec.get [a, -1]")).expect("[]");
}

#[test]
fn test_add_then_sum() {
    // [1,2,3] + [10,10,10] = [11,12,13], sum 36
    quiver()
        .evaluate(&prog(
            "b = vec.fill [I32, 1, 10, 3], r = vec.add [a, b], vec.sum r",
        ))
        .expect("36");
}

#[test]
fn test_sub_self_is_zero() {
    quiver()
        .evaluate(&prog("r = vec.sub [a, a], vec.sum r"))
        .expect("0");
}

#[test]
fn test_mul_then_sum() {
    // [1,2,3] * [1,2,3] = [1,4,9], sum 14
    quiver()
        .evaluate(&prog("r = vec.mul [a, a], vec.sum r"))
        .expect("14");
}

#[test]
fn test_dot() {
    quiver().evaluate(&prog("vec.dot [a, a]")).expect("14");
}

#[test]
fn test_negative_lanes_i64() {
    // I64 vector [5, -7], sum -2 — exercises width 8 and signed two's-complement.
    quiver()
        .evaluate(
            "vec = %vec\n\
             (prepend, new) = %list\n\
             xs = new ~> prepend [~, -7] ~> prepend [~, 5]\n\
             a = vec.of [I64, 1, xs]\n\
             vec.sum a",
        )
        .expect("-2");
}

#[test]
fn test_add_overflow_is_nil() {
    // i32::MAX + 1 overflows the lane → nil rather than wrapping.
    quiver()
        .evaluate(&prog(
            "hi = vec.of [I32, 1, new ~> prepend [~, 2147483647]],\n\
             one = vec.of [I32, 1, new ~> prepend [~, 1]],\n\
             vec.add [hi, one]",
        ))
        .expect("[]");
}

#[test]
fn test_push_out_of_range_is_nil() {
    // 9999999999 doesn't fit a signed i32 lane → nil.
    quiver()
        .evaluate(&prog("vec.of [I32, 1, new ~> prepend [~, 9999999999]]"))
        .expect("[]");
}

#[test]
fn test_dtype_mismatch_is_nil() {
    quiver()
        .evaluate(&prog(
            "b = vec.of [I64, 1, xs],\n\
             vec.add [a, b]",
        ))
        .expect("[]");
}

#[test]
fn test_fill_broadcast_sum() {
    // fill stores a single tiled lane; summing realises it.
    quiver()
        .evaluate(&prog("f = vec.fill [I32, 1, 100, 3], vec.sum f"))
        .expect("300");
}

#[test]
fn test_binary_repeat_tiles_buffer() {
    // A tiled buffer is consumed transparently by the vec kernels: tile one i32 lane (7)
    // three times, then sum → 21. This is the broadcast primitive the scalar ops reuse.
    quiver()
        .evaluate(
            "vec = %vec\n\
             (new) = %list\n\
             unit = vec.of [I32, 1, new ~> %list.prepend [~, 7]],\n\
             tiled = __binary_repeat__ [unit.data, 3],\n\
             [tiled, 4] ~> __vector_sum__",
        )
        .expect("21");
}

// --- Fixed-point: a rational scale makes integer lanes represent fractional values. ---

#[test]
fn test_fractional_scale_sum() {
    // stored [1,2,3] at scale 1/2 → logical [0.5, 1, 1.5], sum 3.
    quiver()
        .evaluate(
            "vec = %vec\n\
             (prepend, new) = %list\n\
             xs = new ~> prepend [~, 3] ~> prepend [~, 2] ~> prepend [~, 1]\n\
             a = vec.of [I32, 1/2, xs]\n\
             vec.sum a",
        )
        .expect("3");
}

#[test]
fn test_fractional_scale_get_returns_rational() {
    // lane 2 is stored 3 at scale 1/2 → 3/2.
    quiver()
        .evaluate(
            "vec = %vec\n\
             (prepend, new) = %list\n\
             xs = new ~> prepend [~, 3] ~> prepend [~, 2] ~> prepend [~, 1]\n\
             a = vec.of [I32, 1/2, xs]\n\
             vec.get [a, 2]",
        )
        .expect("3/2");
}

#[test]
fn test_scale_by_is_metadata() {
    // Scaling unit-scale [1,2,3] by 1/2 changes the value, not the buffer: sum 6 → 3.
    quiver()
        .evaluate(&prog("b = vec.scale_by [a, 1/2], vec.sum b"))
        .expect("3");
}

#[test]
fn test_scale_accessor() {
    quiver()
        .evaluate(&prog("vec.scale_by [a, 1/2] ~> =b, vec.scale b"))
        .expect("1/2");
}

#[test]
fn test_mul_combines_scales() {
    // Two vectors [2,2] at scale 1/2 (logical [1,1]); product is logical [1,1] = stored
    // [4,4] at scale 1/4, summing to 2.
    quiver()
        .evaluate(
            "vec = %vec\n\
             (prepend, new) = %list\n\
             xs = new ~> prepend [~, 2] ~> prepend [~, 2]\n\
             a = vec.of [I32, 1/2, xs]\n\
             r = vec.mul [a, a],\n\
             vec.sum r",
        )
        .expect("2");
}

#[test]
fn test_add_reconciles_unequal_scales() {
    // a = [1,2,3] @ scale 1 (logical [1,2,3]); b = [10,10,10] @ scale 1/2 (logical [5,5,5]).
    // Reconciled to scale 1/2, sum = (6 + 15) = 21.
    quiver()
        .evaluate(&prog(
            "b = vec.fill [I32, 1/2, 10, 3], r = vec.add [a, b], vec.sum r",
        ))
        .expect("21");
}

#[test]
fn test_add_reconciles_coprime_scales() {
    // [0.5] @ 1/2 plus [1/3] @ 1/3 → common scale 1/6, logical 1/2 + 1/3 = 5/6.
    quiver()
        .evaluate(
            "vec = %vec\n\
             p = vec.fill [I32, 1/2, 1, 1],\n\
             q = vec.fill [I32, 1/3, 1, 1],\n\
             r = vec.add [p, q],\n\
             vec.sum r",
        )
        .expect("5/6");
}

#[test]
fn test_reconciled_result_scale() {
    // The reconciled result carries the coarsest common scale (1/6 here).
    quiver()
        .evaluate(
            "vec = %vec\n\
             p = vec.fill [I32, 1/2, 1, 1],\n\
             q = vec.fill [I32, 1/3, 1, 1],\n\
             r = vec.add [p, q],\n\
             vec.scale r",
        )
        .expect("1/6");
}

#[test]
fn test_sub_reconciles_unequal_scales() {
    // [1.0] @ 1 minus [0.25] @ 1/4 → 3/4.
    quiver()
        .evaluate(
            "vec = %vec\n\
             p = vec.fill [I32, 1, 1, 1],\n\
             q = vec.fill [I32, 1/4, 1, 1],\n\
             r = vec.sub [p, q],\n\
             vec.sum r",
        )
        .expect("3/4");
}

#[test]
fn test_add_equal_fractional_scale() {
    // Two unit-stored vectors at scale 1/2 add lane-wise: [1,1,1]+[1,1,1] = stored [2,2,2]
    // at scale 1/2 → logical [1,1,1], sum 3.
    quiver()
        .evaluate(
            "vec = %vec\n\
             a = vec.fill [I32, 1/2, 1, 3],\n\
             r = vec.add [a, a],\n\
             vec.sum r",
        )
        .expect("3");
}

// --- Rational-input construction: build from values rather than raw stored lanes. ---

#[test]
fn test_of_exact() {
    // values [1/2, 3/2] at scale 1/2 → stored [1, 3], sum 2.
    quiver()
        .evaluate(
            "vec = %vec\n\
             (prepend, new) = %list\n\
             vs = new ~> prepend [~, 3/2] ~> prepend [~, 1/2]\n\
             vec.of_exact [I32, 1/2, vs] ~> =a, vec.sum a",
        )
        .expect("2");
}

#[test]
fn test_of_exact_not_representable_is_nil() {
    // 1/3 is not an integer multiple of scale 1/2 → nil.
    quiver()
        .evaluate(
            "vec = %vec\n\
             (prepend, new) = %list\n\
             vs = new ~> prepend [~, 1/3]\n\
             vec.of_exact [I32, 1/2, vs] ~> =r, vec.sum r",
        )
        .expect("[]");
}

#[test]
fn test_of_values_infers_finest_scale() {
    // [1/2, 1/3] → scale 1/lcm(2,3) = 1/6, stored [3, 2], sum 5/6.
    quiver()
        .evaluate(
            "vec = %vec\n\
             (prepend, new) = %list\n\
             vs = new ~> prepend [~, 1/3] ~> prepend [~, 1/2]\n\
             vec.of_values [I32, vs] ~> =a, [scale: vec.scale a, sum: vec.sum a]",
        )
        .expect("[scale: 1/6, sum: 5/6]");
}

#[test]
fn test_of_round() {
    // 333/1000 and 1/2 at scale 1/100 → stored [33, 50], sum 83/100.
    quiver()
        .evaluate(
            "vec = %vec\n\
             (prepend, new) = %list\n\
             vs = new ~> prepend [~, 1/2] ~> prepend [~, 333/1000]\n\
             vec.of_round [I32, 1/100, vs] ~> =a, vec.sum a",
        )
        .expect("83/100");
}

#[test]
fn test_of_round_halves_away_from_zero() {
    // -3/2 → -2, 3/2 → 2 (at scale 1).
    quiver()
        .evaluate(
            "vec = %vec\n\
             (prepend, new) = %list\n\
             vs = new ~> prepend [~, 3/2] ~> prepend [~, -3/2]\n\
             vec.of_round [I32, 1, vs] ~> =a, [v0: vec.get [a, 0], v1: vec.get [a, 1]]",
        )
        .expect("[v0: -2, v1: 2]");
}

#[test]
fn test_filter_lt() {
    // [1,2,3] < [2,2,2] → mask [1,0,0] → keep [1].
    quiver()
        .evaluate(&prog(
            "b = vec.fill [I32, 1, 2, 3], vec.lt [a, b] ~> =m, vec.filter [a, m] ~> =r, vec.sum r",
        ))
        .expect("1");
}

#[test]
fn test_filter_gt() {
    // [1,2,3] > [2,2,2] → mask [0,0,1] → keep [3].
    quiver()
        .evaluate(&prog(
            "b = vec.fill [I32, 1, 2, 3], vec.gt [a, b] ~> =m, vec.filter [a, m] ~> =r, vec.sum r",
        ))
        .expect("3");
}

#[test]
fn test_filter_eq() {
    // [1,2,3] == [2,2,2] → mask [0,1,0] → keep [2].
    quiver()
        .evaluate(&prog(
            "b = vec.fill [I32, 1, 2, 3], vec.eq [a, b] ~> =m, vec.filter [a, m] ~> =r, vec.sum r",
        ))
        .expect("2");
}

#[test]
fn test_filter_all_selected() {
    // Every lane below 10 → keep [1,2,3], sum 6, len 3.
    quiver()
        .evaluate(&prog(
            "b = vec.fill [I32, 1, 10, 3], vec.lt [a, b] ~> =m, vec.filter [a, m] ~> =r, \
             [sum: vec.sum r, len: vec.len r]",
        ))
        .expect("[sum: 6, len: 3]");
}

#[test]
fn test_filter_none_selected() {
    // No lane below 0 → empty vector, len 0, sum 0.
    quiver()
        .evaluate(&prog(
            "b = vec.fill [I32, 1, 0, 3], vec.lt [a, b] ~> =m, vec.filter [a, m] ~> =r, \
             [sum: vec.sum r, len: vec.len r]",
        ))
        .expect("[sum: 0, len: 0]");
}

#[test]
fn test_compare_respects_scale() {
    // a is logical [1,2,3] (scale 1); c is logical [2,2,2] (stored [4,4,4] at scale 1/2). The
    // comparison must align scales, so [1,2,3] < [2,2,2] → mask [1,0,0] → keep the logical 1.
    quiver()
        .evaluate(&prog(
            "c = vec.fill [I32, 1/2, 4, 3], vec.lt [a, c] ~> =m, vec.filter [a, m] ~> =r, \
             [sum: vec.sum r, len: vec.len r]",
        ))
        .expect("[sum: 1, len: 1]");
}

#[test]
fn test_compare_length_mismatch_is_nil() {
    // Comparing vectors of different lengths yields nil (no mask).
    quiver()
        .evaluate(&prog("b = vec.fill [I32, 1, 2, 2], vec.lt [a, b]"))
        .expect("[]");
}

#[test]
fn test_compare_dtype_mismatch_is_nil() {
    // I32 vs I64 cannot be aligned → nil.
    quiver()
        .evaluate(&prog("b = vec.fill [I64, 1, 2, 3], vec.lt [a, b]"))
        .expect("[]");
}
