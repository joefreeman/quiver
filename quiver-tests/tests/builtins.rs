mod common;
use common::*;

#[test]
fn test_builtin() {
    quiver().evaluate("-5 ~> __integer_abs__").expect("5");
}

// === Type-consuming builtins (`__type_name__<'t>`) ======================================
// A type-consuming builtin's behavior depends on its explicit type argument: the compiler
// resolves it to a concrete type id, embeds it in the emitted instruction, and the value
// carries it to the eventual call — through bindings, generic boundaries, captures, and
// process spawns — where the implementation reads it from the context.

#[test]
fn test_type_name_formats_type_argument() {
    quiver()
        .evaluate(r#"__type_name__<'int> ~> Str[~]"#)
        .expect(r#""'int""#);
    // Aliases resolve before embedding: the runtime formats the structural type.
    quiver()
        .evaluate(r#"'pt = P[x: 'int, y: (A | B)]; __type_name__<'pt> ~> Str[~]"#)
        .expect(r#""P[x: 'int, y: (A | B)]""#);
    // The static result type is the signature's.
    quiver()
        .evaluate(r#"__type_name__<'int>"#)
        .expect_type("'bin");
}

#[test]
fn test_type_name_requires_explicit_concrete_argument() {
    // A direct *application* without the type argument is a compile error…
    quiver()
        .evaluate(r#"__type_name__ []"#)
        .expect_compile_error(quiver_compiler::compiler::Error::TypeArgumentsRequired {
            builtin: "type_name".to_string(),
            declared: 1,
        });
    quiver()
        .evaluate(r#"__type_name__<'int, 'bin>"#)
        .expect_compile_error(quiver_compiler::compiler::Error::TypeArgumentsTooMany {
            declared: 1,
            given: 2,
        });
}

#[test]
fn test_type_argument_rides_the_value() {
    // A reference captures the instantiation; the later call reads it.
    quiver()
        .evaluate(r#"f = &__type_name__<'int>; f [] ~> Str[~]"#)
        .expect(r#""'int""#);
    // Through a *generic* declared boundary: the static type sheds the instantiation,
    // the value keeps it — the transport the wire-decode design relies on.
    quiver()
        .evaluate(
            r#"call = #<'u>[#[] -> 'u] { $.0 }
               [&__type_name__<'int>] ~> call ~> Str[~]"#,
        )
        .expect(r#""'int""#);
    // Into a spawned process, by capture and by init argument.
    quiver()
        .evaluate(r#"f = &__type_name__<'int>; p = @#{ f [] ~> Str[~] }; !p"#)
        .expect(r#""'int""#);
    quiver()
        .evaluate(
            r#"'thunk = #[] -> 'bin
               w = #'thunk { $ [] ~> Str[~] }
               p = &__type_name__<'bin> ~> @w; !p"#,
        )
        .expect(r#""'bin""#);
}

#[test]
fn test_type_argument_participates_in_equality() {
    // The type argument is operational (unlike annotations), so differently-instantiated
    // builtins are unequal and same-instantiated ones equal. (Wrapped in tuples — a bare
    // nilary builtin variable in a chain would be *called*.)
    quiver()
        .evaluate(r#"f = &__type_name__<'int>; g = &__type_name__<'bin>; [&f] ~> =[&g]"#)
        .expect("[]");
    quiver()
        .evaluate(r#"f = &__type_name__<'int>; g = &__type_name__<'int>; [&f] ~> =[&g]"#)
        .expect("Ok");
}
