mod common;
use common::*;

#[test]
fn test_new() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            "#,
        )
        .expect("Nil")
}

#[test]
fn test_prepend() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            "#,
        )
        .expect("Cons[20, Cons[10, Nil]]");
}

#[test]
fn test_head() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[] ~> list.head
            "#,
        )
        .expect("[]");

    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.head
            "#,
        )
        .expect("20");
}

#[test]
fn test_tail() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.tail
            "#,
        )
        .expect("Cons[10, Nil]");

    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.tail
            "#,
        )
        .expect("Nil");

    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[] ~> list.tail
            "#,
        )
        .expect("[]");
}

#[test]
fn test_is_empty() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[] ~> list.empty?
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[] ~> list.prepend[~, 10] ~> list.empty?
            "#,
        )
        .expect("[]");
}

#[test]
fn test_length() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[] ~> list.length
            "#,
        )
        .expect("0");

    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[] ~> list.prepend[~, 10] ~> list.length
            "#,
        )
        .expect("1");

    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.length
            "#,
        )
        .expect("2");
}

#[test]
fn test_append() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[] ~> list.append[~, 10]
            "#,
        )
        .expect("Cons[10, Nil]");

    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.append[~, 10]
            ~> list.append[~, 20]
            "#,
        )
        .expect("Cons[10, Cons[20, Nil]]");

    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.append[~, 20]
            "#,
        )
        .expect("Cons[10, Cons[20, Nil]]");
}

#[test]
fn test_reverse() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[] ~> list.reverse
            "#,
        )
        .expect("Nil");

    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[] ~> list.prepend[~, 10] ~> list.reverse
            "#,
        )
        .expect("Cons[10, Nil]");

    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.reverse
            "#,
        )
        .expect("Cons[10, Cons[20, Nil]]");

    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.prepend[~, 30]
            ~> list.reverse
            "#,
        )
        .expect("Cons[10, Cons[20, Cons[30, Nil]]]");
}

#[test]
fn test_concat() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            [list.new[], list.new[]] ~> list.concat
            "#,
        )
        .expect("Nil");

    quiver()
        .evaluate(
            r#"
            list = %"list"
            first = list.new[] ~> list.prepend[~, 10] ~> list.prepend[~, 20],
            second = list.new[],
            [first, second] ~> list.concat
            "#,
        )
        .expect("Cons[20, Cons[10, Nil]]");

    quiver()
        .evaluate(
            r#"
            list = %"list"
            first = list.new[],
            second = list.new[] ~> list.prepend[~, 30] ~> list.prepend[~, 40]
            [first, second] ~> list.concat
            "#,
        )
        .expect("Cons[40, Cons[30, Nil]]");

    quiver()
        .evaluate(
            r#"
            list = %"list"
            first = list.new[] ~> list.prepend[~, 10] ~> list.prepend[~, 20],
            second = list.new[] ~> list.prepend[~, 30] ~> list.prepend[~, 40],
            [first, second] ~> list.concat
            "#,
        )
        .expect("Cons[20, Cons[10, Cons[40, Cons[30, Nil]]]]");
}

#[test]
fn test_at() {
    // Test empty list
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[] ~> list.at[~, 0]
            "#,
        )
        .expect("[]");

    // Test index 0
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.prepend[~, 30]
            ~> list.at[~, 0]
            "#,
        )
        .expect("30");

    // Test index 1
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.prepend[~, 30]
            ~> list.at[~, 1]
            "#,
        )
        .expect("20");

    // Test index 2
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.prepend[~, 30]
            ~> list.at[~, 2]
            "#,
        )
        .expect("10");

    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.at[~, 5]
            "#,
        )
        .expect("[]");
}

#[test]
fn test_map_empty_list() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            math = %"math"
            #int { ~> =x => math.mul[x, 2] } ~> =double,
            list.new[] ~> list.map[~, double]
            "#,
        )
        .expect("Nil");
}

#[test]
fn test_map_single_element() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.mul[x, 2] } ~> =double,
            list.new[]
            ~> list.prepend[~, 5]
            ~> list.map[~, double]
            "#,
        )
        .expect("Cons[10, Nil]");
}

#[test]
fn test_map_multiple_elements() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.mul[x, 2] } ~> =double,
            list.new[]
            ~> list.prepend[~, 1]
            ~> list.prepend[~, 2]
            ~> list.prepend[~, 3]
            ~> list.map[~, double]
            "#,
        )
        .expect("Cons[6, Cons[4, Cons[2, Nil]]]");
}

#[test]
fn test_map_increment() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.add[x, 1] } ~> =inc,
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.prepend[~, 30]
            ~> list.map[~, inc]
            "#,
        )
        .expect("Cons[31, Cons[21, Cons[11, Nil]]]");
}

#[test]
fn test_map_identity() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            #int { ~> =x => x } ~> =id,
            list.new[]
            ~> list.prepend[~, 5]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 15]
            ~> list.map[~, id]
            "#,
        )
        .expect("Cons[15, Cons[10, Cons[5, Nil]]]");
}

#[test]
fn test_map_square() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.mul[x, x] } ~> =square,
            list.new[]
            ~> list.prepend[~, 2]
            ~> list.prepend[~, 3]
            ~> list.prepend[~, 4]
            ~> list.map[~, square]
            "#,
        )
        .expect("Cons[16, Cons[9, Cons[4, Nil]]]");
}

#[test]
fn test_filter_empty_list() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.mod[x, 2] ~> =0 } ~> =even?,
            list.new[] ~> list.filter[~, even?]
            "#,
        )
        .expect("Nil");
}

#[test]
fn test_filter_no_matches() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.lt[x, 0] } ~> =negative?,
            list.new[]
            ~> list.prepend[~, 1]
            ~> list.prepend[~, 2]
            ~> list.prepend[~, 3]
            ~> list.filter[~, negative?]
            "#,
        )
        .expect("Nil");
}

#[test]
fn test_filter_all_match() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.gt[x, 0] } ~> =positive?,
            list.new[]
            ~> list.prepend[~, 1]
            ~> list.prepend[~, 2]
            ~> list.prepend[~, 3]
            ~> list.filter[~, positive?]
            "#,
        )
        .expect("Cons[3, Cons[2, Cons[1, Nil]]]");
}

#[test]
fn test_filter_even_numbers() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.mod[x, 2] ~> =0 } ~> =even?,
            list.new[]
            ~> list.prepend[~, 1]
            ~> list.prepend[~, 2]
            ~> list.prepend[~, 3]
            ~> list.prepend[~, 4]
            ~> list.prepend[~, 5]
            ~> list.prepend[~, 6]
            ~> list.filter[~, even?]
            "#,
        )
        .expect("Cons[6, Cons[4, Cons[2, Nil]]]");
}

#[test]
fn test_filter_greater_than() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.gt[x, 10] } ~> =gt_10,
            list.new[]
            ~> list.prepend[~, 5]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 15]
            ~> list.prepend[~, 20]
            ~> list.prepend[~, 3]
            ~> list.filter[~, gt_10]
            "#,
        )
        .expect("Cons[20, Cons[15, Nil]]");
}

#[test]
fn test_filter_single_match() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            #int { ~> =x, [x, 5] ~> == => Ok } ~> =equals_5,
            list.new[]
            ~> list.prepend[~, 1]
            ~> list.prepend[~, 5]
            ~> list.prepend[~, 10]
            ~> list.filter[~, equals_5]
            "#,
        )
        .expect("Cons[5, Nil]");
}

#[test]
fn test_drop_empty_list() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[] ~> list.drop[~, 0]
            "#,
        )
        .expect("Nil");

    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[] ~> list.drop[~, 5]
            "#,
        )
        .expect("Nil");
}

#[test]
fn test_drop_zero() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.prepend[~, 30]
            ~> list.drop[~, 0]
            "#,
        )
        .expect("Cons[30, Cons[20, Cons[10, Nil]]]");
}

#[test]
fn test_drop_some_elements() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.prepend[~, 30]
            ~> list.prepend[~, 40]
            ~> list.prepend[~, 50]
            ~> list.drop[~, 2]
            "#,
        )
        .expect("Cons[30, Cons[20, Cons[10, Nil]]]");
}

#[test]
fn test_drop_all_elements() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.prepend[~, 30]
            ~> list.drop[~, 3]
            "#,
        )
        .expect("Nil");
}

#[test]
fn test_drop_more_than_length() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.drop[~, 10]
            "#,
        )
        .expect("Nil");
}

#[test]
fn test_take_empty_list() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[] ~> list.take[~, 0]
            "#,
        )
        .expect("Nil");

    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[] ~> list.take[~, 5]
            "#,
        )
        .expect("Nil");
}

#[test]
fn test_take_zero() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.prepend[~, 30]
            ~> list.take[~, 0]
            "#,
        )
        .expect("Nil");
}

#[test]
fn test_take_some_elements() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.prepend[~, 30]
            ~> list.prepend[~, 40]
            ~> list.prepend[~, 50]
            ~> list.take[~, 2]
            "#,
        )
        .expect("Cons[50, Cons[40, Nil]]");
}

#[test]
fn test_take_all_elements() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.prepend[~, 30]
            ~> list.take[~, 3]
            "#,
        )
        .expect("Cons[30, Cons[20, Cons[10, Nil]]]");
}

#[test]
fn test_take_more_than_length() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.take[~, 10]
            "#,
        )
        .expect("Cons[20, Cons[10, Nil]]");
}

#[test]
fn test_contains_empty_list() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[] ~> list.contains?[~, 5]
            "#,
        )
        .expect("[]");
}

#[test]
fn test_contains_single_element_found() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.contains?[~, 10]
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_contains_single_element_not_found() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.contains?[~, 20]
            "#,
        )
        .expect("[]");
}

#[test]
fn test_contains_multiple_elements_first() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.prepend[~, 30]
            ~> list.contains?[~, 30]
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_contains_multiple_elements_middle() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.prepend[~, 30]
            ~> list.contains?[~, 20]
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_contains_multiple_elements_last() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.prepend[~, 30]
            ~> list.contains?[~, 10]
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_contains_not_found() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            list.new[]
            ~> list.prepend[~, 10]
            ~> list.prepend[~, 20]
            ~> list.prepend[~, 30]
            ~> list.contains?[~, 40]
            "#,
        )
        .expect("[]");
}

#[test]
fn test_all_empty_list() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.gt[x, 0] } ~> =positive?,
            list.new[] ~> list.all?[~, positive?]
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_all_single_true() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.gt[x, 0] } ~> =positive?,
            list.new[]
            ~> list.prepend[~, 5]
            ~> list.all?[~, positive?]
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_all_single_false() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.gt[x, 0] } ~> =positive?,
            list.new[]
            ~> list.prepend[~, -5]
            ~> list.all?[~, positive?]
            "#,
        )
        .expect("[]");
}

#[test]
fn test_all_multiple_all_true() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.gt[x, 0] } ~> =positive?,
            list.new[]
            ~> list.prepend[~, 1]
            ~> list.prepend[~, 2]
            ~> list.prepend[~, 3]
            ~> list.all?[~, positive?]
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_all_multiple_some_false() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.gt[x, 0] } ~> =positive?,
            list.new[]
            ~> list.prepend[~, 1]
            ~> list.prepend[~, -2]
            ~> list.prepend[~, 3]
            ~> list.all?[~, positive?]
            "#,
        )
        .expect("[]");
}

#[test]
fn test_all_even_numbers() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.mod[x, 2] ~> =0 } ~> =even?,
            list.new[]
            ~> list.prepend[~, 2]
            ~> list.prepend[~, 4]
            ~> list.prepend[~, 6]
            ~> list.all?[~, even?]
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.mod[x, 2] ~> =0 } ~> =even?,
            list.new[]
            ~> list.prepend[~, 2]
            ~> list.prepend[~, 3]
            ~> list.prepend[~, 4]
            ~> list.all?[~, even?]
            "#,
        )
        .expect("[]");
}

#[test]
fn test_any_empty_list() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.gt[x, 0] } ~> =positive?,
            list.new[] ~> list.any?[~, positive?]
            "#,
        )
        .expect("[]");
}

#[test]
fn test_any_single_true() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.gt[x, 0] } ~> =positive?,
            list.new[]
            ~> list.prepend[~, 5]
            ~> list.any?[~, positive?]
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_any_single_false() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.gt[x, 0] } ~> =positive?,
            list.new[]
            ~> list.prepend[~, -5]
            ~> list.any?[~, positive?]
            "#,
        )
        .expect("[]");
}

#[test]
fn test_any_multiple_all_false() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.gt[x, 0] } ~> =positive?,
            list.new[]
            ~> list.prepend[~, -1]
            ~> list.prepend[~, -2]
            ~> list.prepend[~, -3]
            ~> list.any?[~, positive?]
            "#,
        )
        .expect("[]");
}

#[test]
fn test_any_multiple_some_true() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.gt[x, 0] } ~> =positive?,
            list.new[]
            ~> list.prepend[~, -1]
            ~> list.prepend[~, 2]
            ~> list.prepend[~, -3]
            ~> list.any?[~, positive?]
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_any_first_matches() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.mod[x, 2] ~> =0 } ~> =even?,
            list.new[]
            ~> list.prepend[~, 3]
            ~> list.prepend[~, 5]
            ~> list.prepend[~, 2]
            ~> list.any?[~, even?]
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_any_last_matches() {
    quiver()
        .evaluate(
            r#"
            list = %"list"
            %"math" ~> =math,
            #int { ~> =x => math.mod[x, 2] ~> =0 } ~> =even?,
            list.new[]
            ~> list.prepend[~, 2]
            ~> list.prepend[~, 3]
            ~> list.prepend[~, 5]
            ~> list.any?[~, even?]
            "#,
        )
        .expect("Ok");
}
