mod common;
use common::*;
use std::collections::HashMap;

// Annotations: typed key/value metadata attached to tuple and function values.
// `{ :key value ... }` attaches to the value the braces denote; glued `x:key` retrieves.
// Keys need no declaration: value types are inferred per attach site and tracked in
// annotation rows on types. Retrieval requires the row to be statically visible —
// declared boundaries (function parameters, receive types, ascriptions) erase rows, and
// retrieval on a possibly-erased or provably-absent key is a compile error. Annotations
// are invisible to matching and equality and never propagate through construction.

#[test]
fn test_attach_and_retrieve_on_tuple() {
    quiver()
        .evaluate("a = A[b: 1] { :foo 123 }; a:foo")
        .expect("123");
}

#[test]
fn test_retrieve_absent_key_is_rejected() {
    // A construction is provably annotation-free (exact-empty row), so a retrieval that
    // could never yield a value is rejected as always-nil (rule 5) — the typo firewall.
    quiver()
        .evaluate("a = A[b: 1]; a:foo")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "No member of A[b: 'int] can carry annotation :foo — the retrieval would always be nil"
                .to_string(),
        ));
}

#[test]
fn test_annotation_only_block_is_identity() {
    quiver().evaluate("A[b: 41] { :foo 1 } .b").expect("41");
}

#[test]
fn test_doc_on_function_retrieved_without_calling() {
    quiver()
        .evaluate("f = #'int { :doc \"Doubles\"; [~, 2] __integer_multiply__ }; f:doc")
        .expect("\"Doubles\"");
}

#[test]
fn test_function_annotation_does_not_affect_calls() {
    quiver()
        .evaluate("f = #'int { :doc \"Doubles\"; [~, 2] __integer_multiply__ }; 21 f")
        .expect("42");
}

#[test]
fn test_annotated_nil_still_matches_nil() {
    quiver().evaluate("[] { :doc \"why\" } =[]").expect("Ok");
}

#[test]
fn test_annotated_nil_still_short_circuits() {
    // The annotated nil is still nil at a step boundary: the sequence short-circuits.
    quiver().evaluate("[] { :e Boom }; 42").expect("[]");
}

#[test]
fn test_annotations_invisible_to_equality() {
    quiver()
        .evaluate("a = A[x: 1]; b = A[x: 1] { :foo 9 }; a =&b")
        .expect("Ok");
}

#[test]
fn test_error_payload_roundtrip() {
    let source = "
        div = #['int, 'int] {
          | =[_, 0] => [] { :error DivisionByZero }
          | __integer_divide__
        };
    ";
    // Failure path: the annotated nil flows through the chain into the retrieval.
    quiver()
        .evaluate(&format!(
            "{source} [4, 0] div :error {{ =DivisionByZero => 111 | 222 }}"
        ))
        .expect("111");
    // Success path: the result carries no :error, so retrieval yields nil.
    quiver()
        .evaluate(&format!(
            "{source} [4, 2] div :error {{ =DivisionByZero => 111 | 222 }}"
        ))
        .expect("222");
}

#[test]
fn test_error_payload_propagates_through_calls() {
    // The *same* nil value (with its payload) survives short-circuiting through a caller.
    quiver()
        .evaluate(
            "
            div = #['int, 'int] {
              | =[_, 0] => [] { :error DivisionByZero }
              | __integer_divide__
            };
            half_inc = #'int { [~, 0] div; [~, 1] __integer_add__ };
            10 half_inc :error
            ",
        )
        .expect("DivisionByZero");
}

#[test]
fn test_branch_fallback_discards_payload() {
    // A branch recovering from the failure swallows the payload, like catching an
    // exception: the block's result is the fallback value, no annotation attached.
    // In-chain (a binding would short-circuit the nil case away entirely): the
    // recovering branch yields a fresh Ok, so the payload is gone and :e reads nil.
    quiver()
        .evaluate("[] { :e Boom } { =[] => Ok | ~ } :e =[]")
        .expect("Ok");
}

#[test]
fn test_spread_drops_annotations() {
    // Construction drops annotations: the spread result is freshly built (exact-empty
    // row), so retrieval on it is rejected as always-nil (rule 5).
    quiver()
        .evaluate("a = A[b: 1] { :foo 9 }; x = A[...a]; x:foo")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "No member of A[b: 'int] can carry annotation :foo — the retrieval would always be nil"
                .to_string(),
        ));
}

#[test]
fn test_reattach_replaces_key() {
    quiver()
        .evaluate("a = A[b: 1] { :foo 1 } { :foo 2 }; a:foo")
        .expect("2");
}

#[test]
fn test_multiple_annotations() {
    quiver()
        .evaluate("a = A[b: 1] {\n:foo 10\n:bar 20\n}; [a:foo, a:bar]")
        .expect("[10, 20]");
}

#[test]
fn test_ripple_retrieval_in_chain() {
    quiver().evaluate("A[b: 1] { :foo 7 } ~:foo").expect("7");
}

#[test]
fn test_annotated_process_result() {
    quiver()
        .evaluate("make = #{ [x: 1] { :tag 5 } }; p = @make; !p :tag")
        .expect("5");
}

#[test]
fn test_pre_contract_attaches_and_is_callable() {
    // :pre expects `#'int -> ok?` here; a bare `#{ ... }` infers its parameter from it.
    // The entry is definite, so retrieval types as the bare contract — reference it
    // with `&` (a flowing value would call it) and then call it explicitly.
    quiver()
        .evaluate("f = #'int { :pre #{ Ok }; [~, 1] __integer_add__ }; p = &f:pre; 5 p")
        .expect("Ok");
}

#[test]
fn test_pre_contract_holds_is_transparent() {
    // Debug builds enforce `:pre`; a satisfied precondition is invisible to the result.
    quiver()
        .debug()
        .evaluate("f = #'int { :pre #{ [~, 0] __integer_compare__ =1 }; [~, 2] __integer_multiply__ }; 5 f")
        .expect("10");
}

#[test]
fn test_pre_contract_violation_aborts() {
    // A violated precondition raises a contract-violation runtime error at the call site.
    quiver()
        .debug()
        .evaluate("f = #'int { :pre #{ [~, 0] __integer_compare__ =1 }; [~, 2] __integer_multiply__ }; -5 f")
        .expect_runtime_error(quiver_core::error::Error::Panic(
            "Precondition violated at test:1:88".to_string(),
        ));
}

#[test]
fn test_post_contract_violation_aborts() {
    // The post-contract receives `[in: arg, out: result]`; a broken result aborts.
    quiver()
        .debug()
        .evaluate("f = #'int { :post #{ $ =[in: i, out: o]; [o, i] __integer_compare__ =0 }; [~, 1] __integer_add__ }; 5 f")
        .expect_runtime_error(quiver_core::error::Error::Panic(
            "Postcondition violated at test:1:103".to_string(),
        ));
}

#[test]
fn test_post_contract_holds_is_transparent() {
    // A satisfied postcondition leaves the result untouched.
    quiver()
        .debug()
        .evaluate("f = #'int { :post #{ $ =[in: i, out: o]; [o, i] __integer_compare__ =1 }; [~, 1] __integer_add__ }; 5 f")
        .expect("6");
}

#[test]
fn test_panic_builtin_aborts_with_message() {
    // Contract checks compile to `__panic__`, which is a general abort primitive: it takes
    // a `Str` message and raises a runtime error rather than flowing on as a (recoverable)
    // nil. Usable directly, e.g. to back an `assert`/`unreachable` helper.
    quiver()
        .evaluate("\"boom\" __panic__")
        .expect_runtime_error(quiver_core::error::Error::Panic("boom".to_string()));
}

#[test]
fn test_contracts_not_enforced_in_release() {
    // Release builds emit a plain call: the violated precondition is not checked.
    quiver()
        .evaluate("f = #'int { :pre #{ [~, 0] __integer_compare__ =1 }; [~, 2] __integer_multiply__ }; -5 f")
        .expect("-10");
}

#[test]
fn test_contract_erased_through_parameter_is_not_enforced() {
    // Passing the function through a declared parameter erases its annotation row, so the
    // contract is no longer statically visible and the call is not wrapped — consistent
    // with annotation visibility. The precondition would fail, but it is never checked.
    quiver()
        .debug()
        .evaluate("f = #'int { :pre #{ [~, 0] __integer_compare__ =1 }; [~, 2] __integer_multiply__ }; call = #[g: (#'int -> 'int), x: 'int] { $x $g }; [g: &f, x: -5] call")
        .expect("-10");
}

#[test]
fn test_absent_post_on_exact_row_is_rejected() {
    // The literal's row is exact (:pre only), so :post is provably absent — an
    // always-nil retrieval, rejected by rule 5.
    quiver()
        .evaluate("f = #'int { :pre #{ Ok }; [~, 1] __integer_add__ }; f:post")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "No member of (#'int -> 'int) { :pre #'int -> Ok } can carry annotation :post — the retrieval would always be nil"
                .to_string(),
        ));
}

#[test]
fn test_keys_need_no_declaration() {
    // Keys are just names; value types are inferred per attach and carried in the row.
    quiver().evaluate("A[b: 1] { :nope 1 } :nope").expect("1");
}

#[test]
fn test_doc_value_type_is_checked() {
    // User key value types are inferred (flow-typed), but the builtin :doc stays checked.
    quiver()
        .evaluate("A[b: 1] { :doc 42 }")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "Str['bin]".to_string(),
            found: "'int".to_string(),
        });
}

#[test]
fn test_same_key_with_different_value_types_unions_at_retrieval() {
    // The same key may carry different value types on different paths; retrieval
    // contributes each visible entry type to the union.
    quiver()
        .evaluate(
            "x = A[c: 1] { =A(c) => [] { :foo 1 } | [] { :foo Bar } };
             [] { =&x => 0 | x:foo { =Bar => 111 | 222 } }",
        )
        .expect("222");
}

#[test]
fn test_duplicate_annotation_is_compile_error() {
    quiver()
        .evaluate("A[b: 1] {\n:foo 1\n:foo 2\n}")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "Duplicate annotation :foo".to_string(),
        ));
}

#[test]
fn test_primitive_carrier_is_compile_error() {
    quiver()
        .evaluate("5 { :foo 1 }")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
        "Annotations require a tuple or function carrier, but the annotated value has type 'int"
            .to_string(),
    ));
}

#[test]
fn test_primitive_retrieval_is_compile_error() {
    quiver().evaluate("x = 5; x:foo").expect_compile_error(
        quiver_compiler::compiler::Error::TypeUnresolved(
            "No member of 'int can carry annotation :foo — the retrieval would always be nil"
                .to_string(),
        ),
    );
}

#[test]
fn test_module_member_doc() {
    let mut modules = HashMap::new();
    modules.insert(
        vec!["greeter".to_string()],
        "[ greet: #'int { :doc \"Greets\"; [~, 1] __integer_add__ } ]".to_string(),
    );
    quiver()
        .with_modules(modules)
        .evaluate("%greeter.greet:doc")
        .expect("\"Greets\"");
}

#[test]
fn test_field_label_still_parses_with_space() {
    // `a: foo` (spaced) is a field label; `a:foo` (glued) is retrieval. The near-miss
    // must keep parsing as a field.
    quiver().evaluate("foo = 9; [a: foo] .a").expect("9");
}

#[test]
fn test_laundering_is_rejected() {
    // The rule-4 soundness case: an annotation erased through a declared parameter must
    // not re-enter a union where retrieval would type it as a different key's value.
    quiver()
        .evaluate(
            "launder = #[] { $ };
             a = [] { :error Overflow } launder;
             b = A[c: 1] { =A(c) => a | [] { :error DivisionByZero } };
             b:error",
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "Cannot retrieve :error — a value of type [] may carry erased annotations \
             (annotations aren't statically visible through declared parameter/receive \
             types; state the expected shape with a checked retrieval, `:('t)error`)"
                .to_string(),
        ));
}

#[test]
fn test_parameter_erases_annotations() {
    // Worked example 5: function parameters are declared boundaries; *bare* retrieval
    // inside is rejected — the checked form (below) is the way through.
    quiver()
        .evaluate("report = #[] { $:error }; &report")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "Cannot retrieve :error — a value of type [] may carry erased annotations \
             (annotations aren't statically visible through declared parameter/receive \
             types; state the expected shape with a checked retrieval, `:('t)error`)"
                .to_string(),
        ));
}

#[test]
fn test_reattach_after_boundary_restores_visibility() {
    // Worked example 6: attach makes the entry definite even on an open row.
    quiver()
        .evaluate("tag = #[] { $ { :why Unknown } }; [] tag :why")
        .expect("Unknown");
}

#[test]
fn test_fresh_nil_mixes_with_annotated_nil() {
    // Worked example 3: a nil literal is a closed row (provably annotation-free), so it
    // contributes nil to retrieval instead of being rejected as possibly-erased.
    quiver()
        .evaluate(
            "div = #['int, 'int] { | =[_, 0] => [] { :error DivisionByZero } | __integer_divide__ };
             f = #'int { | =0 => [] | [~, 0] div };
             1 f :error { =DivisionByZero => 111 | 222 }",
        )
        .expect("111");
}

#[test]
fn test_recursive_inference_with_annotated_nil() {
    // Open question 2 from the v2 spec: rows through a tail-recursive fixpoint. The
    // annotated nil produced at the base case must survive inference of the recursive
    // result type and retrieve at the call site.
    quiver()
        .evaluate(
            "f = #['int, 'int] {
               | =[0, acc] => [] { :error Done[acc] }
               | =[n, acc] => [[n, 1] __integer_subtract__, [acc, n] __integer_add__] ^
             };
             [3, 0] f :error { =Done[total] => total | 999 }",
        )
        .expect("6");
}

#[test]
fn test_block_attach_annotation_chain_with_locals() {
    // Regression: a *block* attach compiles its annotation chains at the convergence,
    // after the runtime locals reset — a chain that allocates locals (a binding, an
    // interpolation hole) must renumber from the reset point, and clear them after.
    quiver()
        .evaluate("f = [1] { :doc { y = \"q\"; y } }; f:doc")
        .expect("\"q\"");
    quiver()
        .evaluate("noun = \"ints\"; f = [1] { :doc \"adds {noun}\" }; z = 5; [f:doc, z]")
        .expect("[\"adds ints\", 5]");
}

#[test]
fn test_builtin_attach_and_retrieve() {
    // Builtins carry annotations like any other callable (the payload slot on
    // `Value::Builtin`); attaching must not affect calling.
    quiver()
        .evaluate("f = &__integer_add__ { :doc \"Adds.\" }; f:doc")
        .expect("\"Adds.\"");
    quiver()
        .evaluate("f = &__integer_add__ { :doc \"Adds.\" }; [20, 22] f")
        .expect("42");
}

#[test]
fn test_builtin_annotations_invisible_to_equality() {
    quiver()
        .evaluate("a = &__integer_add__; b = &__integer_add__ { :doc \"Adds.\" }; &a =&b")
        .expect("Ok");
}

#[test]
fn test_module_member_builtin_doc() {
    // An annotated builtin as a module member: the payload survives the compile-time
    // module cache (extraction and reconstruction), and the member still calls bare.
    // The interpolated docstring lands in the executor heap, exercising binary
    // extraction from builtin annotations.
    let mut modules = HashMap::new();
    modules.insert(
        vec!["ops".to_string()],
        "noun = \"integers\"; [ add: &__integer_add__ { :doc \"Adds two {noun}.\" } ]".to_string(),
    );
    quiver()
        .with_modules(modules.clone())
        .evaluate("%ops.add:doc")
        .expect("\"Adds two integers.\"");
    quiver()
        .with_modules(modules)
        .evaluate("[20, 22] %ops.add")
        .expect("42");
}

// Checked retrieval `x:('t)key`: one retrieval operation with an optional expected
// shape. Bare form: shape inferred from the rows (the five visibility rules). Checked
// form: shape explicit — total on any carrier, typed `'t | []`, gated by the same
// runtime structural test as `=('t)v` ascription; an entry outside the shape answers
// nil. The gate is elided when the rows already entail the shape.

#[test]
fn test_checked_retrieval_definite_entry() {
    // Rows entail the shape: the gate is elided and the type stays definite (non-nil).
    quiver()
        .evaluate("f = [1] { :count 42 }; f:('int)count")
        .expect("42");
}

#[test]
fn test_checked_retrieval_wrong_shape_is_nil() {
    quiver()
        .evaluate("f = [1] { :count \"hi\" }; f:('int)count =[]")
        .expect("Ok");
}

#[test]
fn test_checked_retrieval_absent_key_is_nil() {
    // No typo firewall on the checked form: the explicit shape is the programmer's
    // declaration that this is beyond static tracking.
    quiver()
        .evaluate("a = A[b: 1]; a:('int)foo =[]")
        .expect("Ok");
}

#[test]
fn test_checked_retrieval_through_erased_parameter() {
    // The generic-reader case rule 4 forbids for the bare form: the value crossed a
    // declared parameter (rows erased), yet the entry is recovered by shape.
    quiver()
        .evaluate("report = #[] { $:('int)err }; [] { :err 9 } report")
        .expect("9");
}

#[test]
fn test_checked_retrieval_gates_laundered_entry() {
    // The laundering counterexample, closed dynamically: an :error attached at Str and
    // erased through a declared parameter must not escape a retrieval typed 'int.
    quiver()
        .evaluate(
            "launder = #[] { $ };
             a = [] { :error \"overflow\" } launder;
             b = A[c: 1] { =A(c) => a | [] { :error 404 } };
             b:('int)error { =[] => Gated | Escaped }",
        )
        .expect("Gated");
}

#[test]
fn test_checked_retrieval_narrows_by_partial_shape() {
    // The shape doubles as a filter: a partial gate narrows the entry, so field access
    // on the result typechecks.
    quiver()
        .evaluate("f = [1] { :error Overflow[line: 9] }; f:(Overflow(line: 'int))error .line")
        .expect("9");
}

#[test]
fn test_checked_retrieval_on_module_member() {
    // Compile-time module values decide the gate statically, both ways.
    quiver()
        .evaluate("%list.head:(Str['bin])doc")
        .expect("\"The first element of a list, or nil when it is empty.\"");
    quiver().evaluate("%list.head:('int)doc =[]").expect("Ok");
}

#[test]
fn test_std_module_docstring() {
    // std/list.qv members carry :doc; retrieval crosses the module boundary and calling
    // the documented function is unaffected by its row-wrapped type.
    quiver()
        .evaluate("%list.head:doc")
        .expect("\"The first element of a list, or nil when it is empty.\"");
    quiver()
        .evaluate("Cons[1, Cons[2, Nil]] %list.head")
        .expect("1");
}
