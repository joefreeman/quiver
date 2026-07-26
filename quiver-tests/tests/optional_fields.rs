// Optional fields: a function may declare per-field defaults with a `:defaults`
// annotation, and a call argument may then omit those fields. One rule — a field may be
// omitted iff it has a default, and omission fills that default — so a field without one
// stays mandatory and the arity check survives wherever the author didn't opt in.
//
// Defaults ride the *closure*, not the type: they are read off the callee value at the
// call site, exactly as `:pre`/`:post` contracts are. So two functions with the same
// signature can declare different defaults, and a declared boundary — which erases the
// annotation row — sheds them.

mod common;
use common::*;
use std::collections::HashMap;

#[test]
fn test_omit_trailing_field() {
    quiver()
        .evaluate("f = #[(a): 'int, b: 'int] { :defaults [b: 20]; [$a, $b] }; f [1]")
        .expect("[1, 20]");
}

#[test]
fn test_omit_all_fields() {
    quiver()
        .evaluate("f = #[a: 'int, b: 'int] { :defaults [a: 10, b: 20]; [$a, $b] }; f []")
        .expect("[10, 20]");
}

#[test]
fn test_stating_every_field_is_unchanged() {
    quiver()
        .evaluate("f = #[(a): 'int, b: 'int] { :defaults [b: 20]; [$a, $b] }; f [1, b: 2]")
        .expect("[1, 2]");
}

#[test]
fn test_omission_with_reordered_labels() {
    // Omission and free label ordering compose: `b` is defaulted, `c` is stated out of
    // declared order.
    quiver()
        .evaluate(
            "f = #[(a): 'int, b: 'int, c: 'int] { :defaults [b: 20, c: 30]; [$a, $b, $c] }
             f [1, c: 3]",
        )
        .expect("[1, 20, 3]");
}

#[test]
fn test_canonical_options_call() {
    quiver()
        .evaluate(
            "open = #[(path): 'int, mode: (R | W | A), buffer: 'int] {
               :defaults [mode: R, buffer: 1024]
               [$path, $mode, $buffer]
             }
             [open [0], open [0, mode: A], open [0, buffer: 4096, mode: A]]",
        )
        .expect("[[0, R, 1024], [0, A, 1024], [0, A, 4096]]");
}

#[test]
fn test_piped_literal_omits_too() {
    // A literal piped into a statically-known callable elaborates exactly as an argument
    // does — the same position in which it already adopts omittable labels.
    quiver()
        .evaluate("f = #[(a): 'int, b: 'int] { :defaults [b: 20]; [$a, $b] }; [1] ~> f")
        .expect("[1, 20]");
    quiver()
        .evaluate(
            "f = #[(a): 'int, b: 'int, c: 'int] { :defaults [b: 20, c: 30]; [$a, $b, $c] }
             [1, c: 3] ~> f",
        )
        .expect("[1, 20, 3]");
}

#[test]
fn test_same_signature_different_defaults() {
    // The property that forces defaults to be read from the value: these two functions
    // share a type, since a `:defaults` row records the entry's *type*, not its value.
    quiver()
        .evaluate(
            "f = #[a: 'int] { :defaults [a: 1]; $a }
             g = #[a: 'int] { :defaults [a: 2]; $a }
             [f [], g []]",
        )
        .expect("[1, 2]");
}

#[test]
fn test_field_without_default_stays_mandatory() {
    quiver()
        .evaluate("f = #[(a): 'int, b: 'int, c: 'int] { :defaults [c: 30]; $a }; f [1]")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "Missing field 'b' of [a: 'int, b: 'int, c: 'int], which declares no default for it"
                .to_string(),
        ));
}

#[test]
fn test_no_defaults_is_an_ordinary_arity_error() {
    // A callee that declares nothing reports as before — the relaxation is opt-in.
    quiver()
        .evaluate("f = #[(a): 'int, (b): 'int] { $a }; f [1]")
        .expect_type_mismatch();
}

#[test]
fn test_defaults_shed_at_declared_boundary() {
    // The value crosses the boundary fine — it is the annotation *row* that a written
    // function type erases, so inside `h` the field is mandatory again.
    quiver()
        .evaluate(
            "f = #[a: 'int] { :defaults [a: 1]; $a }
             h = #[g: #[a: 'int] -> 'int] { $g [a: 5] }
             h [g: &f]",
        )
        .expect("5");
    quiver()
        .evaluate(
            "f = #[a: 'int] { :defaults [a: 1]; $a }
             h = #[g: #[a: 'int] -> 'int] { $g [] }
             h [g: &f]",
        )
        .expect_type_mismatch();
    // While a direct call still fills it.
    quiver()
        .evaluate("f = #[a: 'int] { :defaults [a: 1]; $a }; f []")
        .expect("1");
}

#[test]
fn test_defaults_across_a_module_boundary() {
    // Annotation rows are part of the type and serialise with the module, so an imported
    // callee's defaults are visible with no extra machinery.
    let mut modules = HashMap::new();
    modules.insert(
        vec!["opts".to_string()],
        "[ make: #[(a): 'int, b: 'int] { :defaults [b: 20]; [$a, $b] } ]".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate("%opts.make [1]")
        .expect("[1, 20]");
}

#[test]
fn test_default_value_is_evaluated_once_at_closure_build() {
    // `:defaults` is an ordinary annotation: its value is a chain evaluated in the
    // enclosing scope when the closure is built, so it can draw on lexical bindings.
    quiver()
        .evaluate(
            "base = 5
             f = #[(a): 'int, b: 'int] { :defaults [b: [base, 15] ~> __integer_add__]; [$a, $b] }
             f [1]",
        )
        .expect("[1, 20]");
}

#[test]
fn test_omission_does_not_apply_to_nested_literals() {
    // Defaults are per-function-parameter, so only a call's own argument tuple has a
    // source for them; a nested literal has an expected type but no defaults.
    quiver()
        .evaluate(
            "f = #[outer: [x: 'int, y: 'int]] { :defaults [outer: [x: 1, y: 2]]; $outer }
             f [outer: [x: 9]]",
        )
        .expect_type_mismatch();
}

// The `name: type = value` sugar. Legal only in a function literal's parameter spelling,
// where it lowers to the same `:defaults` annotation — so the two spellings are one
// mechanism, and the default never becomes part of the parameter type.

#[test]
fn test_sugar_declares_a_default() {
    quiver()
        .evaluate("f = #[(a): 'int, b: 'int = 20] { [$a, $b] }; f [1]")
        .expect("[1, 20]");
}

#[test]
fn test_sugar_canonical_options_call() {
    quiver()
        .evaluate(
            "open = #[(path): 'int, mode: (R | W | A) = R, buffer: 'int = 1024] {
               [$path, $mode, $buffer]
             }
             [open [0], open [0, mode: A], open [0, buffer: 4096, mode: A]]",
        )
        .expect("[[0, R, 1024], [0, A, 1024], [0, A, 4096]]");
}

#[test]
fn test_sugar_lowers_to_the_defaults_annotation() {
    quiver()
        .evaluate("f = #[a: 'int = 1, b: 'int = 2] { $a }; f:defaults")
        .expect("[a: 1, b: 2]");
}

#[test]
fn test_sugar_leaves_the_parameter_type_alone() {
    // The default belongs to the function, so it must not reach the type: a function type
    // written without it still accepts the value.
    quiver()
        .evaluate(
            "f = #[a: 'int = 1] { $a }
             h = #[g: #[a: 'int] -> 'int] { $g [a: 5] }
             h [g: &f]",
        )
        .expect("5");
}

#[test]
fn test_default_rejected_in_nested_tuple_type() {
    quiver()
        .evaluate("f = #[outer: [x: 'int = 1]] { $outer }; f")
        .expect_error_containing("only allowed in a function literal's parameter type");
}

#[test]
fn test_default_rejected_in_function_type() {
    quiver()
        .evaluate("f = #[g: #[a: 'int = 1] -> 'int] { $g }; f")
        .expect_error_containing("only allowed in a function literal's parameter type");
}

#[test]
fn test_sugar_and_explicit_defaults_conflict() {
    // Mixing the spellings is confusing even when they name different fields, so the
    // ordinary duplicate-annotation check rejects the pair.
    quiver()
        .evaluate("f = #[a: 'int = 1, b: 'int] { :defaults [b: 2]; $a }; f []")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "Duplicate annotation :defaults".to_string(),
        ));
}

#[test]
fn test_unnamed_field_default_is_rejected() {
    // A default on an unlabeled field could never be skipped-past: a later entry would
    // have no label to state.
    quiver()
        .evaluate("f = #['int = 1, x: 'int] { $x }; f [x: 2]")
        .expect_error_containing("has a default but no label");
}
