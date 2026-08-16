mod common;
use common::*;

// --- %str conversions -------------------------------------------------------------

#[test]
fn test_parse_int() {
    quiver().evaluate("\"42\" ~> %str.parse_int ~").expect("42");
    quiver()
        .evaluate("\"-17\" ~> %str.parse_int ~")
        .expect("-17");
    quiver().evaluate("\"0\" ~> %str.parse_int ~").expect("0");
}

#[test]
fn test_parse_int_rejects_garbage() {
    quiver()
        .evaluate("\"4x\" ~> %str.parse_int ~ ~> { =[] => No | Yes }")
        .expect("No");
    quiver()
        .evaluate("\"\" ~> %str.parse_int ~ ~> { =[] => No | Yes }")
        .expect("No");
    quiver()
        .evaluate("\"-\" ~> %str.parse_int ~ ~> { =[] => No | Yes }")
        .expect("No");
}

#[test]
fn test_from_int() {
    quiver().evaluate("0 ~> %str.from_int ~").expect("\"0\"");
    quiver()
        .evaluate("-17 ~> %str.from_int ~")
        .expect("\"-17\"");
    quiver()
        .evaluate("1000 ~> %str.from_int ~")
        .expect("\"1000\"");
}

// --- %parse primitives ------------------------------------------------------------

#[test]
fn test_int_parser() {
    quiver()
        .evaluate("[\"42\", %parse.int] ~> %parse.run ~")
        .expect("42");
    quiver()
        .evaluate("[\"-7\", %parse.int] ~> %parse.run ~")
        .expect("-7");
}

#[test]
fn test_ident_and_quoted() {
    quiver()
        .evaluate("[\"hello_1\", %parse.ident] ~> %parse.run ~")
        .expect("\"hello_1\"");
    quiver()
        .evaluate(r#"["\"hi\"", &%parse.quoted] ~> %parse.run ~"#)
        .expect("\"hi\"");
}

#[test]
fn test_literal() {
    quiver()
        .evaluate("p = [\"let\", \"'let'\"] ~> %parse.literal ~; [\"let\", p] ~> %parse.run ~")
        .expect("Ok");
}

#[test]
fn test_run_requires_end_of_input() {
    quiver()
        .evaluate(
            r#"
            'e = Expected[offset: 'int, message: Str['bin]]
            r = ["12x", %parse.int] ~> %parse.run ~
            { | r:('e)error ~> =Expected(offset: 2) => Pass | Fail }
            "#,
        )
        .expect("Pass");
}

// --- combinators --------------------------------------------------------------------

#[test]
fn test_sep_by_and_between() {
    quiver()
        .evaluate(
            r#"
            comma = [44, "','"] ~> %parse.byte ~
            elems = [%parse.int, comma] ~> %parse.sep_by ~
            p = [[91, "'['"] ~> %parse.byte ~, elems, [93, "']'"] ~> %parse.byte ~] ~> %parse.between ~
            ["[1,2,3]", p] ~> %parse.run ~
            "#,
        )
        .expect("Cons[1, Cons[2, Cons[3, Nil]]]");
}

#[test]
fn test_sep_by_backtracks_trailing_separator() {
    quiver()
        .evaluate(
            r#"
            comma = [44, "','"] ~> %parse.byte ~
            elems = [%parse.int, comma] ~> %parse.sep_by ~
            trailing = [elems, comma ~> %parse.opt ~] ~> %parse.left ~
            ["1,2,", trailing] ~> %parse.run ~
            "#,
        )
        .expect("Cons[1, Cons[2, Nil]]");
}

#[test]
fn test_alt_keeps_furthest_failure() {
    quiver()
        .evaluate(
            r#"
            'e = Expected[offset: 'int, message: Str['bin]]
            p = [%parse.int, %parse.quoted] ~> %parse.alt2 ~
            r = ["\"ab", &p] ~> %parse.run ~
            { | r:('e)error ~> =Expected(offset: 3) => Pass | Fail }
            "#,
        )
        .expect("Pass");
}

#[test]
fn test_many0() {
    quiver()
        .evaluate(
            r#"
            digit = [%parse.int, [59, "';'"] ~> %parse.byte ~] ~> %parse.left ~
            p = digit ~> %parse.many0 ~
            ["1;2;", p] ~> %parse.run ~
            "#,
        )
        .expect("Cons[1, Cons[2, Nil]]");
}

#[test]
fn test_chainl_left_associativity() {
    quiver()
        .evaluate(
            r#"
            sub_op = [[45, "'-'"] ~> %parse.byte ~, #{ __integer_subtract__ }] ~> %parse.map ~
            p = [%parse.int, sub_op] ~> %parse.chainl ~
            ["10-3-2", p] ~> %parse.run ~
            "#,
        )
        .expect("5");
}

#[test]
fn test_label_replaces_message() {
    quiver()
        .evaluate(
            r#"
            'e = Expected[offset: 'int, message: Str['bin]]
            p = [%parse.int, "a count"] ~> %parse.label ~
            r = ["x", p] ~> %parse.run ~
            { | r:('e)error ~> =Expected(message: "a count") => Pass | Fail }
            "#,
        )
        .expect("Pass");
}

#[test]
fn test_rec_ties_a_recursive_grammar() {
    quiver()
        .evaluate(
            r#"
            value = #['%parse.p<'int>, '%parse] {
              =[v, st]
              p1 = [[40, "'('"] ~> %parse.byte ~, v] ~> %parse.right ~
              paren = [p1, [41, "')'"] ~> %parse.byte ~] ~> %parse.left ~
              core = [%parse.int, paren] ~> %parse.alt2 ~
              st ~> core ~
            } ~> %parse.rec ~
            ["((7))", value] ~> %parse.run ~
            "#,
        )
        .expect("7");
}

#[test]
fn test_sep_by_nullable_parsers_terminate() {
    // Element and separator that both succeed empty must not loop forever.
    quiver()
        .evaluate(
            r#"
            p = [%parse.ws, %parse.ws] ~> %parse.sep_by ~
            ["abc", p] ~> %parse.run ~ ~> { =[] => Terminated | Fail }
            "#,
        )
        .expect("Terminated");
}

#[test]
fn test_many0_zero_width_match_has_no_phantom_element() {
    // A parser that succeeds without consuming stops the repetition without
    // contributing a spurious empty trailing element.
    quiver()
        .evaluate(
            r#"
            digit? = #'int { [$, 48] ~> __integer_compare__ ~ ~> =(0 | 1); [$, 57] ~> __integer_compare__ ~ ~> =(-1 | 0); Ok }
            p = digit? ~> %parse.take_while ~ ~> %parse.many0 ~
            ["12", p] ~> %parse.run ~
            "#,
        )
        .expect("Cons[0x3132, Nil]");
}

#[test]
fn test_chainl_nullable_operator_terminates() {
    // Operator and operand that both succeed empty must not loop forever.
    quiver()
        .evaluate(
            r#"
            digit? = #'int { [$, 48] ~> __integer_compare__ ~ ~> =(0 | 1); [$, 57] ~> __integer_compare__ ~ ~> =(-1 | 0); Ok }
            p = digit? ~> %parse.take_while ~
            op = [%parse.ws, #{ %bin.concat }] ~> %parse.map ~
            c = [p, op] ~> %parse.chainl ~
            ["57", c] ~> %parse.run ~
            "#,
        )
        .expect("0x3537");
}

#[test]
fn test_ident_accepts_host_identifier_grammar() {
    // Uppercase letters after the first character, and `?` then `!` suffixes.
    quiver()
        .evaluate(r#"["xY_9z", %parse.ident] ~> %parse.run ~"#)
        .expect("\"xY_9z\"");
    quiver()
        .evaluate(r#"["valid?", %parse.ident] ~> %parse.run ~"#)
        .expect("\"valid?\"");
    quiver()
        .evaluate(r#"["go!", %parse.ident] ~> %parse.run ~"#)
        .expect("\"go!\"");
    quiver()
        .evaluate(r#"["ok?!", %parse.ident] ~> %parse.run ~"#)
        .expect("\"ok?!\"");
}

#[test]
fn test_quoted_backspace_and_formfeed_escapes() {
    quiver()
        .evaluate(r#"["\"a\\b\\f\"", &%parse.quoted] ~> %parse.run ~ ~> =Str[b]; b"#)
        .expect("0x61080c");
}

#[test]
fn test_sep_by_keeps_furthest_failure_through_backtracking() {
    // Backtracking over a trailing separator must not discard the element's failure:
    // the surfaced error is the element's ("a number" at 4), not "end of input" at 3.
    quiver()
        .evaluate(
            r#"
            'e = Expected[offset: 'int, message: Str['bin]]
            comma = [44, "','"] ~> %parse.byte ~
            elems = [%parse.int, comma] ~> %parse.sep_by ~
            r = ["1,2,x", elems] ~> %parse.run ~
            { | r:('e)error ~> =Expected(offset: 4, message: "a number") => Pass | Fail }
            "#,
        )
        .expect("Pass");
}

#[test]
fn test_opt_keeps_furthest_failure_through_backtracking() {
    // `opt` converts a failure into None; the discarded failure must still surface when
    // the parse then fails at a shallower position (run's end-of-input check at 0).
    quiver()
        .evaluate(
            r#"
            'e = Expected[offset: 'int, message: Str['bin]]
            ab_cd = [["ab", "'ab'"] ~> %parse.literal ~, ["cd", "'cd'"] ~> %parse.literal ~] ~> %parse.then ~
            p = ab_cd ~> %parse.opt ~
            r = ["abx", p] ~> %parse.run ~
            { | r:('e)error ~> =Expected(offset: 2, message: "'cd'") => Pass | Fail }
            "#,
        )
        .expect("Pass");
}
