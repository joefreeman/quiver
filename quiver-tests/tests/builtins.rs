mod common;
use common::*;

#[test]
fn test_builtin() {
    quiver().evaluate("-5 ~> __integer_abs__ ~").expect("5");
}

// === Type-consuming builtins (`__data_decode__<'t>`) ====================================
// A type-consuming builtin's behavior depends on its explicit type argument: the compiler
// resolves it to a concrete type id, embeds it in the emitted instruction, and the value
// carries it to the eventual call — through bindings, generic boundaries, captures, and
// process spawns — where the implementation reads it from the context.

#[test]
fn test_type_argument_arity_is_checked() {
    // A direct *application* of the bare builtin without the type argument is a compile
    // error…
    quiver()
        .evaluate(r#"__data_decode__ "5""#)
        .expect_compile_error(quiver_compiler::compiler::Error::TypeArgumentsRequired {
            builtin: "data_decode".to_string(),
            declared: 1,
        });
    quiver()
        .evaluate(r#"__data_decode__<'int, 'bin>"#)
        .expect_compile_error(quiver_compiler::compiler::Error::TypeArgumentsTooMany {
            declared: 1,
            given: 2,
        });
}

#[test]
fn test_type_argument_rides_the_value() {
    // A reference captures the instantiation; the later call reads it.
    quiver()
        .evaluate(r#"f = __data_decode__<'int>; f "5""#)
        .expect("5");
    // Through a *generic* declared boundary: the static type sheds the instantiation,
    // the value keeps it — the transport the wire-decode design relies on.
    quiver()
        .evaluate(
            r#"call = #<'u>[#'%str -> 'u] { $.0 "42" }
               [__data_decode__<'int>] ~> call ~"#,
        )
        .expect("42");
    // Into a spawned process, by capture and by init argument.
    quiver()
        .evaluate(r#"f = __data_decode__<'int>; p = @#[] { f "42" } []; !p"#)
        .expect("42");
    quiver()
        .evaluate(
            r#"'decoder = #'%str -> ('int | [])
               w = #'decoder { $ "7" }
               p = __data_decode__<'int> ~> @w ~; !p"#,
        )
        .expect("7");
}

#[test]
fn test_type_argument_participates_in_equality() {
    // The type argument is operational (unlike annotations), so differently-instantiated
    // builtins are unequal and same-instantiated ones equal. (Referenced inside tuples —
    // a bare callable in a value position would be *called*.)
    quiver()
        .evaluate(r#"f = __data_decode__<'int>; g = __data_decode__<'bin>; [f] ~> =[&g]"#)
        .expect("[]");
    quiver()
        .evaluate(r#"f = __data_decode__<'int>; g = __data_decode__<'int>; [f] ~> =[&g]"#)
        .expect("Ok");
}
