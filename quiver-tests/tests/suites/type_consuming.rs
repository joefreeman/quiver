use crate::common::*;
use std::collections::HashMap;

/// `dec<'t> text`: text decoded as data notation at `'t` — a function that consumes its type
/// parameter, by naming `%data.decode` at it.
const DEC: &str = "dec = #<'t>'%str -> ('t | []) { %data.decode<'t> $ }";

#[test]
fn test_type_argument_written_where_named() {
    quiver()
        .evaluate(&format!(
            "{DEC}\n[dec<'int> \"5\", dec<Point[x: 'int]> \"Point[x: 1]\"]"
        ))
        .expect("[5, Point[x: 1]]");
}

#[test]
fn test_type_argument_inferred_from_the_argument() {
    quiver()
        .evaluate(
            "like = #<'t>['t, '%str] -> ('t | []) { %data.decode<'t> $1 }
             like [0, \"7\"]",
        )
        .expect("7");
}

#[test]
fn test_identity_travels_through_bindings() {
    // A binding, a tuple field and a destructured tuple keep the function's identity in its
    // type, so it is instantiated where it is next named.
    quiver()
        .evaluate(&format!(
            "{DEC}
             g = dec
             t = [d: dec]
             (d) = t
             [g<'int> \"1\", t.d<'int> \"2\", d<'int> \"3\"]"
        ))
        .expect("[1, 2, 3]");
}

#[test]
fn test_a_module_member_is_instantiated_where_named() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["w".to_string()],
        "decode = #<'t>'%str -> ('t | []) { %data.decode<'t> $ }\n(decode)".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate(
            "(decode) = %w
             [%w.decode<'int> \"5\", decode<'int> \"6\"]",
        )
        .expect("[5, 6]");
}

#[test]
fn test_requirements_are_transitive() {
    // `twice` names `dec` at its own type parameter, so it consumes it too.
    quiver()
        .evaluate(&format!(
            "{DEC}
             twice = #<'t>'%str -> ['t | [], 't | []] {{ [dec<'t> $, dec<'t> $] }}
             twice<'int> \"3\""
        ))
        .expect("[3, 3]");
}

#[test]
fn test_a_nested_function_reads_the_enclosing_instantiation() {
    quiver()
        .evaluate(
            "outer = #<'t>'%str -> ('t | []) { inner = #'%str { %data.decode<'t> $ }; inner $ }
             outer<'int> \"9\"",
        )
        .expect("9");
}

#[test]
fn test_named_at_runtime_inside_a_function() {
    // Named inside a function body, at concrete types, from a value it captured.
    quiver()
        .evaluate(&format!(
            "{DEC}
             use = #'%str {{ dec<'int> $ }}
             [use \"4\", use \"5\"]"
        ))
        .expect("[4, 5]");
}

#[test]
fn test_recursion_keeps_the_instantiation() {
    quiver()
        .evaluate(
            "all = #<'t>['%list<'%str>, '%list<('t | [])>] -> '%list<('t | [])> {
               | =[Nil, acc] => acc
               | =[Cons[s, rest], acc] => ^ [rest, Cons[%data.decode<'t> s, acc]]
             }
             all<'int> [%list{ \"1\", \"2\" }, Nil]",
        )
        .expect("Cons[2, Cons[1, Nil]]");
}

#[test]
fn test_a_spawned_instantiation_keeps_it() {
    quiver()
        .evaluate(&format!(
            "{DEC}
             d = dec<'int>
             p = @d \"8\"
             !p"
        ))
        .expect("8");
}

#[test]
fn test_an_aliased_builtin_is_instantiated_where_named() {
    // A type-consuming builtin held uninstantiated is instantiated where it is named, rather
    // than failing when it runs.
    quiver()
        .evaluate("(decode) = %data; decode<'int> \"5\"")
        .expect("5");
}

#[test]
fn test_passed_on_uninstantiated_is_an_error() {
    quiver()
        .evaluate(&format!("{DEC}\n%list.map [%list{{ \"1\" }}, dec]"))
        .expect_error_containing("passed on uninstantiated");
}

#[test]
fn test_an_undetermined_type_argument_is_an_error() {
    quiver()
        .evaluate(&format!("{DEC}\ndec \"1\""))
        .expect_error_containing("`'t` is neither");
}

#[test]
fn test_answered_uninstantiated_through_a_declared_result_is_an_error() {
    quiver()
        .evaluate(&format!("{DEC}\nf = #[] -> (#'%str -> _) {{ dec }}\nf []"))
        .expect_error_containing("passed on uninstantiated");
}

#[test]
fn test_spawned_with_its_type_argument_inferred() {
    quiver()
        .evaluate(
            "like = #<'t>['t, '%str] -> ('t | []) { %data.decode<'t> $1 }
             p = @like [0, \"6\"]
             !p",
        )
        .expect("6");
}

#[test]
fn test_spawned_with_an_undetermined_type_argument_is_an_error() {
    quiver()
        .evaluate(&format!("{DEC}\np = @dec \"1\"; !p"))
        .expect_error_containing("`'t` is neither");
}

#[test]
fn test_named_through_the_ripple() {
    quiver()
        .evaluate(&format!("{DEC}\ndec ~> ~<'int> \"5\""))
        .expect("5");
}

#[test]
fn test_called_uninstantiated_through_the_ripple_is_an_error() {
    quiver()
        .evaluate(&format!("{DEC}\ndec ~> ~ \"5\""))
        .expect_error_containing("passed on uninstantiated");
}

#[test]
fn test_tail_called_uninstantiated_is_an_error() {
    quiver()
        .evaluate(&format!("{DEC}\nf = #'%str {{ dec ~> ^~ $ }}; f \"1\""))
        .expect_error_containing("passed on uninstantiated");
}

#[test]
fn test_an_uninstantiated_builtin_passed_on_is_an_error() {
    quiver()
        .evaluate("(decode) = %data; %list.map [%list{ \"1\" }, decode]")
        .expect_error_containing("passed on uninstantiated");
}

#[test]
fn test_a_type_parameter_cant_hold_one_uninstantiated() {
    // A type variable ranges over complete values: inside the generic, its type would no
    // longer say what the function still needs.
    quiver()
        .evaluate(&format!("{DEC}\nid = #<'a>'a {{ $ }}; id dec"))
        .expect_error_containing("passed on uninstantiated");
    quiver()
        .evaluate(&format!(
            "{DEC}\nid = #<'a>'a {{ $ }}; g = id dec<'int>; g \"4\""
        ))
        .expect("4");
}

#[test]
fn test_the_top_type_cant_hold_one_uninstantiated() {
    // Nor does the top type, at any depth.
    quiver()
        .evaluate(&format!("{DEC}\nt = #_ {{ $ }}; t [d: dec]"))
        .expect_error_containing("passed on uninstantiated");
    quiver()
        .evaluate(&format!("{DEC}\nf = #[] -> _ {{ dec }}; f []"))
        .expect_error_containing("passed on uninstantiated");
}

#[test]
fn test_an_annotation_cant_hold_one_uninstantiated() {
    quiver()
        .evaluate(&format!("{DEC}\nx = [1] ~> {{ :decoder dec; ~ }}; x"))
        .expect_error_containing("passed on uninstantiated");
}

#[test]
fn test_returned_from_a_call_keeps_its_parameters_open() {
    // The function's type parameters appear only in its result, but it consumes them, so
    // the call that answers it doesn't close them.
    quiver()
        .evaluate(
            "mk = #[] { #<'u>'%str -> ('u | []) { %data.decode<'u> $ } }
             g = mk []
             g<'int> \"5\"",
        )
        .expect("5");
}

#[test]
fn test_returned_from_a_generic_call_takes_the_enclosing_instantiation() {
    quiver()
        .evaluate(
            "mk = #<'t>[] {
               #<'u>['u, '%str] -> ['t | [], 'u | []] { [%data.decode<'t> $1, %data.decode<'u> $1] }
             }
             g = mk<'int> []
             g [<00>, \"5\"]",
        )
        .expect("[5, []]");
}

#[test]
fn test_a_type_test_admits_only_an_instantiated_closure() {
    // At run time a closure is judged as the plain function it is once named, so a test on a
    // value that may hold one not yet named is made against what its type says it can be.
    quiver()
        .evaluate(&format!(
            "{DEC}
             x = [] ~> {{ | =[] => dec | 5 }}
             y = [] ~> {{ | =[] => dec<'int> | 5 }}
             [x ~> {{ =(#'%str -> _) => Fn | No }}, y ~> {{ =(#'%str -> _) => Fn | No }}]"
        ))
        .expect("[No, Fn]");
}

#[test]
fn test_a_type_test_against_a_type_parameter_consumes_it() {
    // `='t` is decided by the type the function is named with, so the function consumes it,
    // as it would by naming a type-consuming builtin.
    quiver()
        .evaluate(
            "is = #<'t>_ { $ ~> { ='t => Yes | No } }
             [is<'int> 5, is<'int> <00>, is<Point[x: 'int]> Point[x: 1]]",
        )
        .expect("[Yes, No, Yes]");
    quiver()
        .evaluate("is = #<'t>_ { $ ~> { ='t => Yes | No } }; is 5")
        .expect_error_containing("`'t` is neither");
}

#[test]
fn test_a_witness_is_transitive() {
    quiver()
        .evaluate(
            "is = #<'t>_ { $ ~> { ='t => Yes | No } }
             both = #<'t>[_, _] { [is<'t> $0, is<'t> $1] }
             both<'bin> [<01>, 2]",
        )
        .expect("[Yes, No]");
}

#[test]
fn test_a_module_member_testing_its_type_parameter() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["w".to_string()],
        "keep = #<'t>'%list<_> -> '%list<'t> {
           %list.flat_map [$, #{ $ ~> { =('t & v) => %list{ v } | Nil } }]
         }
         ints = keep<'int>
         (keep, ints)"
            .to_string(),
    );
    // Named at concrete types in the module itself, the instantiation is part of its value.
    quiver()
        .with_modules(modules)
        .evaluate("[%w.keep<'bin> %list{ 1, <00>, 2 }, %w.ints %list{ 1, <00>, 2 }]")
        .expect("[Cons[<00>, Nil], Cons[1, Cons[2, Nil]]]");
}

#[test]
fn test_naming_leaves_the_named_binding_alone() {
    // Naming makes a new closure, so binding it doesn't narrow the function it was named from.
    quiver()
        .evaluate(&format!(
            "{DEC}\na = dec<'int>\n[a \"1\", dec<'bin> \"<01>\"]"
        ))
        .expect("[1, <01>]");
}
