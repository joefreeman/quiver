mod common;
use common::*;

#[test]
fn test_new() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            list.new!
            "#,
        )
        .expect("Nil")
}

#[test]
fn test_prepend() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            "#,
        )
        .expect("Cons[20, Cons[10, Nil]]");
}

#[test]
fn test_head() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            list.new! ~> list.head!
            "#,
        )
        .expect("[]");

    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> list.head!
            "#,
        )
        .expect("20");
}

#[test]
fn test_tail() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> list.tail!
            "#,
        )
        .expect("Cons[10, Nil]");

    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> list.tail!
            "#,
        )
        .expect("Nil");

    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> list.tail!
            "#,
        )
        .expect("[]");
}

#[test]
fn test_is_empty() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            list.new! ~> list.empty?!
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            list.new! ~> [~, 10] ~> list.prepend! ~> list.empty?!
            "#,
        )
        .expect("[]");
}

#[test]
fn test_length() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            list.new! ~> list.length!
            "#,
        )
        .expect("0");

    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            list.new! ~> [~, 10] ~> list.prepend! ~> list.length!
            "#,
        )
        .expect("1");

    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> list.length!
            "#,
        )
        .expect("2");
}

#[test]
fn test_append() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.append!
            "#,
        )
        .expect("Cons[10, Nil]");

    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.append!
            ~> [~, 20]
            ~> list.append!
            "#,
        )
        .expect("Cons[10, Cons[20, Nil]]");

    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.append!
            "#,
        )
        .expect("Cons[10, Cons[20, Nil]]");
}

#[test]
fn test_reverse() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> list.reverse!
            "#,
        )
        .expect("Nil");

    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> list.reverse!
            "#,
        )
        .expect("Cons[10, Nil]");

    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> list.reverse!
            "#,
        )
        .expect("Cons[10, Cons[20, Nil]]");

    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> [~, 30]
            ~> list.prepend!
            ~> list.reverse!
            "#,
        )
        .expect("Cons[10, Cons[20, Cons[30, Nil]]]");
}

#[test]
fn test_concat() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            [list.new!, list.new!]
            ~> list.concat!
            "#,
        )
        .expect("Nil");

    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> first,
            list.new! ~> second,
            [first, second] ~> list.concat!
            "#,
        )
        .expect("Cons[20, Cons[10, Nil]]");

    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            list.new! ~> first,
            []
            ~> list.new!
            ~> [~, 30]
            ~> list.prepend!
            ~> [~, 40]
            ~> list.prepend!
            ~> second,
            [first, second] ~> list.concat!
            "#,
        )
        .expect("Cons[40, Cons[30, Nil]]");

    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> first,
            []
            ~> list.new!
            ~> [~, 30]
            ~> list.prepend!
            ~> [~, 40]
            ~> list.prepend!
            ~> second,
            [first, second] ~> list.concat!
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
            %"list" ~> list,
            list.new! ~> [~, 0] ~> list.at!
            "#,
        )
        .expect("[]");

    // Test index 0
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> [~, 30]
            ~> list.prepend!
            ~> [~, 0]
            ~> list.at!
            "#,
        )
        .expect("30");

    // Test index 1
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> [~, 30]
            ~> list.prepend!
            ~> [~, 1]
            ~> list.at!
            "#,
        )
        .expect("20");

    // Test index 2
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> [~, 30]
            ~> list.prepend!
            ~> [~, 2]
            ~> list.at!
            "#,
        )
        .expect("10");

    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> [~, 5]
            ~> list.at!
            "#,
        )
        .expect("[]");
}

#[test]
fn test_map_empty_list() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 2] ~> math.mul! } ~> double,
            list.new! ~> [~, double] ~> list.map!
            "#,
        )
        .expect("Nil");
}

#[test]
fn test_map_single_element() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 2] ~> math.mul! } ~> double,
            []
            ~> list.new!
            ~> [~, 5]
            ~> list.prepend!
            ~> [~, double]
            ~> list.map!
            "#,
        )
        .expect("Cons[10, Nil]");
}

#[test]
fn test_map_multiple_elements() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 2] ~> math.mul! } ~> double,
            []
            ~> list.new!
            ~> [~, 1]
            ~> list.prepend!
            ~> [~, 2]
            ~> list.prepend!
            ~> [~, 3]
            ~> list.prepend!
            ~> [~, double]
            ~> list.map!
            "#,
        )
        .expect("Cons[6, Cons[4, Cons[2, Nil]]]");
}

#[test]
fn test_map_increment() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 1] ~> math.add! } ~> inc,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> [~, 30]
            ~> list.prepend!
            ~> [~, inc]
            ~> list.map!
            "#,
        )
        .expect("Cons[31, Cons[21, Cons[11, Nil]]]");
}

#[test]
fn test_map_identity() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            #int { ~> x => x } ~> id,
            []
            ~> list.new!
            ~> [~, 5]
            ~> list.prepend!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 15]
            ~> list.prepend!
            ~> [~, id]
            ~> list.map!
            "#,
        )
        .expect("Cons[15, Cons[10, Cons[5, Nil]]]");
}

#[test]
fn test_map_square() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, x] ~> math.mul! } ~> square,
            []
            ~> list.new!
            ~> [~, 2]
            ~> list.prepend!
            ~> [~, 3]
            ~> list.prepend!
            ~> [~, 4]
            ~> list.prepend!
            ~> [~, square]
            ~> list.map!
            "#,
        )
        .expect("Cons[16, Cons[9, Cons[4, Nil]]]");
}

#[test]
fn test_filter_empty_list() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 2] ~> math.mod! ~> 0 } ~> is_even,
            list.new! ~> [~, is_even] ~> list.filter!
            "#,
        )
        .expect("Nil");
}

#[test]
fn test_filter_no_matches() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 0] ~> math.lt! } ~> is_negative,
            []
            ~> list.new!
            ~> [~, 1]
            ~> list.prepend!
            ~> [~, 2]
            ~> list.prepend!
            ~> [~, 3]
            ~> list.prepend!
            ~> [~, is_negative]
            ~> list.filter!
            "#,
        )
        .expect("Nil");
}

#[test]
fn test_filter_all_match() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 0] ~> math.gt! } ~> is_positive,
            []
            ~> list.new!
            ~> [~, 1]
            ~> list.prepend!
            ~> [~, 2]
            ~> list.prepend!
            ~> [~, 3]
            ~> list.prepend!
            ~> [~, is_positive]
            ~> list.filter!
            "#,
        )
        .expect("Cons[3, Cons[2, Cons[1, Nil]]]");
}

#[test]
fn test_filter_even_numbers() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 2] ~> math.mod! ~> 0 } ~> is_even,
            []
            ~> list.new!
            ~> [~, 1]
            ~> list.prepend!
            ~> [~, 2]
            ~> list.prepend!
            ~> [~, 3]
            ~> list.prepend!
            ~> [~, 4]
            ~> list.prepend!
            ~> [~, 5]
            ~> list.prepend!
            ~> [~, 6]
            ~> list.prepend!
            ~> [~, is_even]
            ~> list.filter!
            "#,
        )
        .expect("Cons[6, Cons[4, Cons[2, Nil]]]");
}

#[test]
fn test_filter_greater_than() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 10] ~> math.gt! } ~> gt_10,
            []
            ~> list.new!
            ~> [~, 5]
            ~> list.prepend!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 15]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> [~, 3]
            ~> list.prepend!
            ~> [~, gt_10]
            ~> list.filter!
            "#,
        )
        .expect("Cons[20, Cons[15, Nil]]");
}

#[test]
fn test_filter_single_match() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            #int { ~> x, [x, 5] ~> == => Ok } ~> equals_5,
            []
            ~> list.new!
            ~> [~, 1]
            ~> list.prepend!
            ~> [~, 5]
            ~> list.prepend!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, equals_5]
            ~> list.filter!
            "#,
        )
        .expect("Cons[5, Nil]");
}

#[test]
fn test_drop_empty_list() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            list.new! ~> [~, 0] ~> list.drop!
            "#,
        )
        .expect("Nil");

    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            list.new! ~> [~, 5] ~> list.drop!
            "#,
        )
        .expect("Nil");
}

#[test]
fn test_drop_zero() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> [~, 30]
            ~> list.prepend!
            ~> [~, 0]
            ~> list.drop!
            "#,
        )
        .expect("Cons[30, Cons[20, Cons[10, Nil]]]");
}

#[test]
fn test_drop_some_elements() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> [~, 30]
            ~> list.prepend!
            ~> [~, 40]
            ~> list.prepend!
            ~> [~, 50]
            ~> list.prepend!
            ~> [~, 2]
            ~> list.drop!
            "#,
        )
        .expect("Cons[30, Cons[20, Cons[10, Nil]]]");
}

#[test]
fn test_drop_all_elements() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> [~, 30]
            ~> list.prepend!
            ~> [~, 3]
            ~> list.drop!
            "#,
        )
        .expect("Nil");
}

#[test]
fn test_drop_more_than_length() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> [~, 10]
            ~> list.drop!
            "#,
        )
        .expect("Nil");
}

#[test]
fn test_take_empty_list() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            list.new! ~> [~, 0] ~> list.take!
            "#,
        )
        .expect("Nil");

    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            list.new! ~> [~, 5] ~> list.take!
            "#,
        )
        .expect("Nil");
}

#[test]
fn test_take_zero() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> [~, 30]
            ~> list.prepend!
            ~> [~, 0]
            ~> list.take!
            "#,
        )
        .expect("Nil");
}

#[test]
fn test_take_some_elements() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> [~, 30]
            ~> list.prepend!
            ~> [~, 40]
            ~> list.prepend!
            ~> [~, 50]
            ~> list.prepend!
            ~> [~, 2]
            ~> list.take!
            "#,
        )
        .expect("Cons[50, Cons[40, Nil]]");
}

#[test]
fn test_take_all_elements() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> [~, 30]
            ~> list.prepend!
            ~> [~, 3]
            ~> list.take!
            "#,
        )
        .expect("Cons[30, Cons[20, Cons[10, Nil]]]");
}

#[test]
fn test_take_more_than_length() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> [~, 10]
            ~> list.take!
            "#,
        )
        .expect("Cons[20, Cons[10, Nil]]");
}

#[test]
fn test_contains_empty_list() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            list.new! ~> [~, 5] ~> list.contains?!
            "#,
        )
        .expect("[]");
}

#[test]
fn test_contains_single_element_found() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 10]
            ~> list.contains?!
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_contains_single_element_not_found() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.contains?!
            "#,
        )
        .expect("[]");
}

#[test]
fn test_contains_multiple_elements_first() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> [~, 30]
            ~> list.prepend!
            ~> [~, 30]
            ~> list.contains?!
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_contains_multiple_elements_middle() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> [~, 30]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.contains?!
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_contains_multiple_elements_last() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> [~, 30]
            ~> list.prepend!
            ~> [~, 10]
            ~> list.contains?!
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_contains_not_found() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            []
            ~> list.new!
            ~> [~, 10]
            ~> list.prepend!
            ~> [~, 20]
            ~> list.prepend!
            ~> [~, 30]
            ~> list.prepend!
            ~> [~, 40]
            ~> list.contains?!
            "#,
        )
        .expect("[]");
}

#[test]
fn test_all_empty_list() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 0] ~> math.gt! } ~> is_positive,
            list.new! ~> [~, is_positive] ~> list.all?!
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_all_single_true() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 0] ~> math.gt! } ~> is_positive,
            []
            ~> list.new!
            ~> [~, 5]
            ~> list.prepend!
            ~> [~, is_positive]
            ~> list.all?!
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_all_single_false() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 0] ~> math.gt! } ~> is_positive,
            []
            ~> list.new!
            ~> [~, -5]
            ~> list.prepend!
            ~> [~, is_positive]
            ~> list.all?!
            "#,
        )
        .expect("[]");
}

#[test]
fn test_all_multiple_all_true() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 0] ~> math.gt! } ~> is_positive,
            []
            ~> list.new!
            ~> [~, 1]
            ~> list.prepend!
            ~> [~, 2]
            ~> list.prepend!
            ~> [~, 3]
            ~> list.prepend!
            ~> [~, is_positive]
            ~> list.all?!
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_all_multiple_some_false() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 0] ~> math.gt! } ~> is_positive,
            []
            ~> list.new!
            ~> [~, 1]
            ~> list.prepend!
            ~> [~, -2]
            ~> list.prepend!
            ~> [~, 3]
            ~> list.prepend!
            ~> [~, is_positive]
            ~> list.all?!
            "#,
        )
        .expect("[]");
}

#[test]
fn test_all_even_numbers() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 2] ~> math.mod! ~> 0 } ~> is_even,
            []
            ~> list.new!
            ~> [~, 2]
            ~> list.prepend!
            ~> [~, 4]
            ~> list.prepend!
            ~> [~, 6]
            ~> list.prepend!
            ~> [~, is_even]
            ~> list.all?!
            "#,
        )
        .expect("Ok");

    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 2] ~> math.mod! ~> 0 } ~> is_even,
            []
            ~> list.new!
            ~> [~, 2]
            ~> list.prepend!
            ~> [~, 3]
            ~> list.prepend!
            ~> [~, 4]
            ~> list.prepend!
            ~> [~, is_even]
            ~> list.all?!
            "#,
        )
        .expect("[]");
}

#[test]
fn test_any_empty_list() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 0] ~> math.gt! } ~> is_positive,
            list.new! ~> [~, is_positive] ~> list.any?!
            "#,
        )
        .expect("[]");
}

#[test]
fn test_any_single_true() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 0] ~> math.gt! } ~> is_positive,
            []
            ~> list.new!
            ~> [~, 5]
            ~> list.prepend!
            ~> [~, is_positive]
            ~> list.any?!
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_any_single_false() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 0] ~> math.gt! } ~> is_positive,
            []
            ~> list.new!
            ~> [~, -5]
            ~> list.prepend!
            ~> [~, is_positive]
            ~> list.any?!
            "#,
        )
        .expect("[]");
}

#[test]
fn test_any_multiple_all_false() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 0] ~> math.gt! } ~> is_positive,
            []
            ~> list.new!
            ~> [~, -1]
            ~> list.prepend!
            ~> [~, -2]
            ~> list.prepend!
            ~> [~, -3]
            ~> list.prepend!
            ~> [~, is_positive]
            ~> list.any?!
            "#,
        )
        .expect("[]");
}

#[test]
fn test_any_multiple_some_true() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 0] ~> math.gt! } ~> is_positive,
            []
            ~> list.new!
            ~> [~, -1]
            ~> list.prepend!
            ~> [~, 2]
            ~> list.prepend!
            ~> [~, -3]
            ~> list.prepend!
            ~> [~, is_positive]
            ~> list.any?!
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_any_first_matches() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 2] ~> math.mod! ~> 0 } ~> is_even,
            []
            ~> list.new!
            ~> [~, 3]
            ~> list.prepend!
            ~> [~, 5]
            ~> list.prepend!
            ~> [~, 2]
            ~> list.prepend!
            ~> [~, is_even]
            ~> list.any?!
            "#,
        )
        .expect("Ok");
}

#[test]
fn test_any_last_matches() {
    quiver()
        .evaluate(
            r#"
            %"list" ~> list,
            %"math" ~> math,
            #int { ~> x => [x, 2] ~> math.mod! ~> 0 } ~> is_even,
            []
            ~> list.new!
            ~> [~, 2]
            ~> list.prepend!
            ~> [~, 3]
            ~> list.prepend!
            ~> [~, 5]
            ~> list.prepend!
            ~> [~, is_even]
            ~> list.any?!
            "#,
        )
        .expect("Ok");
}
