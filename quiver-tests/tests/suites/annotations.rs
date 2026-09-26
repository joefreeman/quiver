use crate::common::*;
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
        .evaluate("a = A[b: 1] ~> { :foo 123 }; a:foo")
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
    quiver()
        .evaluate("A[b: 41] ~> { :foo 1 } ~> .b")
        .expect("41");
}

#[test]
fn test_doc_on_function_retrieved_without_calling() {
    quiver()
        .evaluate("f = #'int { :doc \"Doubles\"; [~, 2] ~> __integer_multiply__ ~ }; f:doc")
        .expect("\"Doubles\"");
}

#[test]
fn test_function_annotation_does_not_affect_calls() {
    quiver()
        .evaluate("f = #'int { :doc \"Doubles\"; [~, 2] ~> __integer_multiply__ ~ }; 21 ~> f ~")
        .expect("42");
}

#[test]
fn test_annotated_nil_still_matches_nil() {
    quiver()
        .evaluate("[] ~> { :doc \"why\" } ~> { =[] => IsNil | NotNil }")
        .expect("IsNil");
}

#[test]
fn test_annotated_nil_still_short_circuits() {
    // The annotated nil is still nil at a step boundary: the sequence short-circuits.
    quiver().evaluate("[] ~> { :e Boom }; 42").expect("[]");
}

#[test]
fn test_annotations_invisible_to_equality() {
    quiver()
        .evaluate("a = A[x: 1]; b = A[x: 1] ~> { :foo 9 }; a ~> =^b")
        .expect("A[x: 1]");
}

#[test]
fn test_error_payload_roundtrip() {
    let source = "
        div = #['int, 'int] {
          | =[_, 0] => [] ~> { :error DivisionByZero }
          | __integer_divide__ ~
        };
    ";
    // Failure path: the annotated nil flows through the chain into the retrieval.
    quiver()
        .evaluate(&format!(
            "{source} [4, 0] ~> div ~ ~> :error ~> {{ =DivisionByZero => 111 | 222 }}"
        ))
        .expect("111");
    // Success path: the result carries no :error, so retrieval yields nil.
    quiver()
        .evaluate(&format!(
            "{source} [4, 2] ~> div ~ ~> :error ~> {{ =DivisionByZero => 111 | 222 }}"
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
              | =[_, 0] => [] ~> { :error DivisionByZero }
              | __integer_divide__ ~
            };
            half_inc = #'int { [~, 0] ~> div ~; [~, 1] ~> __integer_add__ ~ };
            10 ~> half_inc ~ ~> :error
            ",
        )
        .expect("DivisionByZero");
}

#[test]
fn test_branch_fallback_discards_payload() {
    // A branch recovering from the failure swallows the payload, like catching an
    // exception: the block's result is the fallback value, no annotation attached.
    // In-chain (a binding would short-circuit the nil case away entirely): the
    // recovering branch yields a fresh value, so the payload is gone and :e reads nil.
    quiver()
        .evaluate(
            "[] ~> { :e Boom } ~> { =[] => Recovered | ~ } ~> :(Boom)e ~> { =[] => Gone | Kept }",
        )
        .expect("Gone");
}

#[test]
fn test_spread_drops_annotations() {
    // Construction drops annotations: the spread result is freshly built (exact-empty
    // row), so retrieval on it is rejected as always-nil (rule 5).
    quiver()
        .evaluate("a = A[b: 1] ~> { :foo 9 }; x = A[...a]; x:foo")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "No member of A[b: 'int] can carry annotation :foo — the retrieval would always be nil"
                .to_string(),
        ));
}

#[test]
fn test_reattach_replaces_key() {
    quiver()
        .evaluate("a = A[b: 1] ~> { :foo 1 } ~> { :foo 2 }; a:foo")
        .expect("2");
}

#[test]
fn test_multiple_annotations() {
    quiver()
        .evaluate("a = A[b: 1] ~> {\n:foo 10\n:bar 20\n}; [a:foo, a:bar]")
        .expect("[10, 20]");
}

#[test]
fn test_ripple_retrieval_in_chain() {
    quiver()
        .evaluate("A[b: 1] ~> { :foo 7 } ~> ~:foo")
        .expect("7");
}

#[test]
fn test_annotated_process_result() {
    quiver()
        .evaluate("make = #[] { [x: 1] ~> { :tag 5 } }; p = @make []; !p ~> :tag")
        .expect("5");
}

#[test]
fn test_pre_contract_attaches_and_is_callable() {
    // :pre expects `#'int -> ok?` here; a bare `#{ ... }` infers its parameter from it.
    // The entry is definite, so retrieval types as the bare contract; binding it names the
    // contract without applying it, and the call is then explicit.
    quiver()
        .evaluate("f = #'int { :pre #{ Ok }; [~, 1] ~> __integer_add__ ~ }; p = f:pre; 5 ~> p ~")
        .expect("Ok");
}

#[test]
fn test_pre_contract_holds_is_transparent() {
    // Debug builds enforce `:pre`; a satisfied precondition is invisible to the result.
    quiver()
        .debug()
        .evaluate("f = #'int { :pre #{ [~, 0] ~> __integer_compare__ ~ ~> =1 }; [~, 2] ~> __integer_multiply__ ~ }; 5 ~> f ~")
        .expect("10");
}

#[test]
fn test_pre_contract_violation_aborts() {
    // A violated precondition raises a contract-violation runtime error at the call site —
    // the callee (`f`), not the `~` standing in for its argument.
    quiver()
        .debug()
        .evaluate("f = #'int { :pre #{ [~, 0] ~> __integer_compare__ ~ ~> =1 }; [~, 2] ~> __integer_multiply__ ~ }; -5 ~> f ~")
        .expect_runtime_error(quiver_core::error::Error::Panic(
            "Precondition violated at test:1:104".to_string(),
        ));
}

#[test]
fn test_imported_contract_is_enforced_wherever_the_call_stands() {
    // A module member's contracts are found on its linked type, whose annotation keys were
    // renumbered into the session; the row must stay sorted for the lookup to find them, in a
    // tuple field as much as in tail position.
    quiver()
        .debug()
        .evaluate("[%num.clamp [5, 1, 10], %num.clamp [0, 1, 10], %num.clamp [[], 1, 10]]")
        .expect("[5, 1, []]");
    quiver()
        .debug()
        .evaluate("[%num.clamp [5, 10, 1]]")
        .expect_runtime_error(quiver_core::error::Error::Panic(
            "Precondition violated at test:1:2".to_string(),
        ));
    quiver()
        .debug()
        .evaluate("%num.clamp [5, 10, 1]")
        .expect_runtime_error(quiver_core::error::Error::Panic(
            "Precondition violated at test:1:1".to_string(),
        ));
}

#[test]
fn test_post_contract_violation_aborts() {
    // The post-contract receives `[in: arg, out: result]`; a broken result aborts.
    quiver()
        .debug()
        .evaluate("f = #'int { :post #{ $ ~> =[in: i, out: o]; [o, i] ~> __integer_compare__ ~ ~> =0 }; [~, 1] ~> __integer_add__ ~ }; 5 ~> f ~")
        .expect_runtime_error(quiver_core::error::Error::Panic(
            "Postcondition violated at test:1:122".to_string(),
        ));
}

#[test]
fn test_post_contract_holds_is_transparent() {
    // A satisfied postcondition leaves the result untouched.
    quiver()
        .debug()
        .evaluate("f = #'int { :post #{ $ ~> =[in: i, out: o]; [o, i] ~> __integer_compare__ ~ ~> =1 }; [~, 1] ~> __integer_add__ ~ }; 5 ~> f ~")
        .expect("6");
}

#[test]
fn test_panic_builtin_aborts_with_message() {
    // Contract checks compile to `__panic__`, which is a general abort primitive: it takes
    // a `Str` message and raises a runtime error rather than flowing on as a (recoverable)
    // nil. Usable directly, e.g. to back an `assert`/`unreachable` helper.
    quiver()
        .evaluate("\"boom\" ~> __panic__ ~")
        .expect_runtime_error(quiver_core::error::Error::Panic("boom".to_string()));
}

#[test]
fn test_contracts_not_enforced_in_release() {
    // Release builds emit a plain call: the violated precondition is not checked.
    quiver()
        .evaluate("f = #'int { :pre #{ [~, 0] ~> __integer_compare__ ~ ~> =1 }; [~, 2] ~> __integer_multiply__ ~ }; -5 ~> f ~")
        .expect("-10");
}

#[test]
fn test_contract_erased_through_parameter_is_not_enforced() {
    // Passing the function through a declared parameter erases its annotation row, so the
    // contract is no longer statically visible and the call is not wrapped — consistent
    // with annotation visibility. The precondition would fail, but it is never checked.
    quiver()
        .debug()
        .evaluate("f = #'int { :pre #{ [~, 0] ~> __integer_compare__ ~ ~> =1 }; [~, 2] ~> __integer_multiply__ ~ }; call = #[g: (#'int -> 'int), x: 'int] { $x ~> $g ~ }; [g: f, x: -5] ~> call ~")
        .expect("-10");
}

#[test]
fn test_absent_post_on_exact_row_is_rejected() {
    // The literal's row is exact (:pre only), so :post is provably absent — an
    // always-nil retrieval, rejected by rule 5.
    quiver()
        .evaluate("f = #'int { :pre #{ Ok }; [~, 1] ~> __integer_add__ ~ }; f:post")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "No member of (#'int -> 'int) { :pre #'int -> Ok } can carry annotation :post — the retrieval would always be nil"
                .to_string(),
        ));
}

#[test]
fn test_keys_need_no_declaration() {
    // Keys are just names; value types are inferred per attach and carried in the row.
    quiver()
        .evaluate("A[b: 1] ~> { :nope 1 } ~> :nope")
        .expect("1");
}

#[test]
fn test_doc_value_type_is_checked() {
    // User key value types are inferred (flow-typed), but the builtin :doc stays checked.
    quiver()
        .evaluate("A[b: 1] ~> { :doc 42 }")
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
            "x = A[c: 1] ~> { =A(c) => [] ~> { :foo 1 } | [] ~> { :foo Bar } };
             x:foo ~> { =Bar => 111 | 222 }",
        )
        .expect("222");
}

#[test]
fn test_annotations_are_invisible_to_pin_equality() {
    // An annotated nil pinned against a plain nil matches — equality ignores
    // annotations, and a pin of a nil-valued variable is an ordinary equality test.
    quiver()
        .evaluate("x = [] ~> { :foo 1 }; [] ~> { =^x => Same | Different }")
        .expect("Same");
}

#[test]
fn test_error_payload_propagates_through_a_failed_match() {
    // A match that fails *because its scrutinee was nil* re-emits that nil rather than minting
    // a fresh one, so a payload survives the one-step `expr ~> =pat` exactly as it survives the
    // two-step `expr; =pat`. Without this the single-step form silently drops the payload,
    // which is what broke %parse's furthest-error tracking and the dialect error positions.
    let source = "
        div = #['int, 'int] {
          | =[_, 0] => [] ~> { :error DivisionByZero }
          | __integer_divide__ ~
        };
    ";
    quiver()
        .evaluate(&format!(
            "{source} g = #[] {{ [4, 0] ~> div ~ ~> =('int)x; 5 }}; g [] ~> :error"
        ))
        .expect("DivisionByZero");
    // The two-step spelling answers the same, for a different reason: the first step is the
    // nil, so the boundary short-circuits and the later steps never run. Both forms carry the
    // payload, which is what makes the one-liner a faithful rewrite of the two-stepper.
    quiver()
        .evaluate(&format!(
            "{source} g = #[] {{ [4, 0] ~> div ~; =x; 5 }}; g [] ~> :error"
        ))
        .expect("DivisionByZero");
}

#[test]
fn test_failed_match_carries_only_a_nil_scrutinee() {
    // Same code, same types — only the runtime value differs. A nil scrutinee's failure is that
    // nil propagating, so its payload survives; a non-nil value that simply doesn't fit the
    // pattern is a fresh failure, and cannot inherit the payload of the value it rejected.
    let source = "
        f = #'int { | =0 => [] ~> { :error Boom } | A[1] ~> { :error Stale } };
        g = #'int { f ~ ~> =B[x]; 5 };
    ";
    quiver()
        .evaluate(&format!("{source} 0 ~> g ~ ~> :error"))
        .expect("Boom");
    quiver()
        .evaluate(&format!("{source} 1 ~> g ~ ~> :error"))
        .expect("[]");
}

#[test]
fn test_duplicate_annotation_is_compile_error() {
    quiver()
        .evaluate("A[b: 1] ~> {\n:foo 1\n:foo 2\n}")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "Duplicate annotation :foo".to_string(),
        ));
}

#[test]
fn test_primitive_carrier_is_compile_error() {
    quiver()
        .evaluate("5 ~> { :foo 1 }")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
        "Annotations require a tuple or function carrier, but the annotated value has type 'int"
            .to_string(),
    ));
}

#[test]
fn test_generic_carrier_attaches_and_reads_back_checked() {
    // A generic attach site — the carrier is a bare type variable, so the compiler
    // cannot decide and defers the check to the runtime. Nothing is recorded in the
    // row (nothing is provably visible through the variable), so the entry reads back
    // through a checked retrieval, exactly as a runtime-attached annotation does.
    quiver()
        .evaluate(
            r#"stamp = #<'t>['t, 'int] { $0 ~> { :seen $1 } }
               e = stamp [Inc, 41]
               [e, e:('int)seen]"#,
        )
        .expect("[Inc, 41]");
}

#[test]
fn test_generic_carrier_is_invisible_to_the_row() {
    // The attach does not widen the carrier's type, so a bare retrieval is still the
    // same compile error it would be without the stamp — the checked form is the only
    // way in, and the value itself is unchanged for matching.
    quiver()
        .evaluate(
            r#"stamp = #<'t>['t, 'int] { $0 ~> { :seen $1 } }
               stamp [Inc, 41] ~> =Inc"#,
        )
        .expect("Inc");
}

#[test]
fn test_generic_carrier_with_primitive_is_runtime_error() {
    // The deferred check, firing: a generic stamp instantiated with a primitive fails
    // fast where a static check could only have rejected the whole (valid) function.
    quiver()
        .evaluate(
            r#"stamp = #<'t>['t, 'int] { $0 ~> { :seen $1 } }
               stamp [5, 41]"#,
        )
        .expect_runtime_error(quiver_core::error::Error::InvalidArgument(
            "Annotations require a tuple or function carrier, but the annotated value is integer"
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
        "[ greet: #'int { :doc \"Greets\"; [~, 1] ~> __integer_add__ ~ } ]".to_string(),
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
    quiver().evaluate("foo = 9; [a: foo] ~> .a").expect("9");
}

#[test]
fn test_laundering_is_rejected() {
    // The rule-4 soundness case: an annotation erased through a declared parameter must
    // not re-enter a union where retrieval would type it as a different key's value.
    quiver()
        .evaluate(
            "launder = #[] { $ };
             a = [] ~> { :error Overflow } ~> launder ~;
             b = A[c: 1] ~> { =A(c) => a | [] ~> { :error DivisionByZero } };
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
        .evaluate("report = #[] { $:error }; report")
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
        .evaluate("tag = #[] { $ ~> { :why Unknown } }; [] ~> tag ~ ~> :why")
        .expect("Unknown");
}

#[test]
fn test_fresh_nil_mixes_with_annotated_nil() {
    // Worked example 3: a nil literal is a closed row (provably annotation-free), so it
    // contributes nil to retrieval instead of being rejected as possibly-erased.
    quiver()
        .evaluate(
            "div = #['int, 'int] { | =[_, 0] => [] ~> { :error DivisionByZero } | __integer_divide__ ~ };
             f = #'int { | =0 => [] | [~, 0] ~> div ~ };
             1 ~> f ~ ~> :error ~> { =DivisionByZero => 111 | 222 }",
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
               | =[0, acc] => [] ~> { :error Done[acc] }
               | =[n, acc] => [[n, 1] ~> __integer_subtract__ ~, [acc, n] ~> __integer_add__ ~] ~> ^ ~
             };
             [3, 0] ~> f ~ ~> :error ~> { =Done[total] => total | 999 }",
        )
        .expect("6");
}

#[test]
fn test_block_attach_annotation_chain_with_locals() {
    // Regression: a *block* attach compiles its annotation chains at the convergence,
    // after the runtime locals reset — a chain that allocates locals (a binding, an
    // interpolation hole) must renumber from the reset point, and clear them after.
    quiver()
        .evaluate("f = [1] ~> { :doc { y = \"q\"; y } }; f:doc")
        .expect("\"q\"");
    quiver()
        .evaluate("noun = \"ints\"; f = [1] ~> { :doc \"adds {noun}\" }; z = 5; [f:doc, z]")
        .expect("[\"adds ints\", 5]");
}

#[test]
fn test_builtin_attach_and_retrieve() {
    // Builtins carry annotations like any other callable (the payload slot on
    // `Value::Builtin`); attaching must not affect calling.
    quiver()
        .evaluate("f = __integer_add__ ~> { :doc \"Adds.\" }; f:doc")
        .expect("\"Adds.\"");
    quiver()
        .evaluate("f = __integer_add__ ~> { :doc \"Adds.\" }; [20, 22] ~> f ~")
        .expect("42");
}

#[test]
fn test_builtin_annotations_invisible_to_equality() {
    quiver()
        .evaluate("a = __integer_add__; b = __integer_add__ ~> { :doc \"Adds.\" }; a ~> { =^b => Equal | Different }")
        .expect("Equal");
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
        "noun = \"integers\"; [ add: __integer_add__ ~> { :doc \"Adds two {noun}.\" } ]"
            .to_string(),
    );
    quiver()
        .with_modules(modules.clone())
        .evaluate("%ops.add:doc")
        .expect("\"Adds two integers.\"");
    quiver()
        .with_modules(modules)
        .evaluate("[20, 22] ~> %ops.add ~")
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
        .evaluate("f = [1] ~> { :count 42 }; f:('int)count")
        .expect("42");
}

#[test]
fn test_checked_retrieval_wrong_shape_is_nil() {
    quiver()
        .evaluate("f = [1] ~> { :count \"hi\" }; f:('int)count ~> { =[] => IsNil | NotNil }")
        .expect("IsNil");
}

#[test]
fn test_checked_retrieval_absent_key_is_nil() {
    // No typo firewall on the checked form: the explicit shape is the programmer's
    // declaration that this is beyond static tracking.
    quiver()
        .evaluate("a = A[b: 1]; a:('int)foo ~> { =[] => IsNil | NotNil }")
        .expect("IsNil");
}

#[test]
fn test_checked_retrieval_through_erased_parameter() {
    // The generic-reader case rule 4 forbids for the bare form: the value crossed a
    // declared parameter (rows erased), yet the entry is recovered by shape.
    quiver()
        .evaluate("report = #[] { $:('int)err }; [] ~> { :err 9 } ~> report ~")
        .expect("9");
}

#[test]
fn test_checked_retrieval_gates_laundered_entry() {
    // The laundering counterexample, closed dynamically: an :error attached at Str and
    // erased through a declared parameter must not escape a retrieval typed 'int.
    quiver()
        .evaluate(
            "launder = #[] { $ };
             a = [] ~> { :error \"overflow\" } ~> launder ~;
             b = A[c: 1] ~> { =A(c) => a | [] ~> { :error 404 } };
             b:('int)error ~> { =[] => Gated | Escaped }",
        )
        .expect("Gated");
}

#[test]
fn test_checked_retrieval_narrows_by_partial_shape() {
    // The shape doubles as a filter: a partial gate narrows the entry, so field access
    // on the result typechecks.
    quiver()
        .evaluate("f = [1] ~> { :error Overflow[line: 9] }; f:(Overflow(line: 'int))error ~> .line")
        .expect("9");
}

#[test]
fn test_checked_retrieval_on_module_member() {
    // Compile-time module values decide the gate statically, both ways.
    quiver()
        .evaluate("%list.head:(Str['bin])doc")
        .expect("\"The first element of a list, or nil when it is empty.\"");
    quiver()
        .evaluate("%list.head:('int)doc ~> { =[] => IsNil | NotNil }")
        .expect("IsNil");
}

#[test]
fn test_std_module_docstring() {
    // std/list.qv members carry :doc; retrieval crosses the module boundary and calling
    // the documented function is unaffected by its row-wrapped type.
    quiver()
        .evaluate("%list.head:doc")
        .expect("\"The first element of a list, or nil when it is empty.\"");
    quiver()
        .evaluate("Cons[1, Cons[2, Nil]] ~> %list.head ~")
        .expect("1");
}

// `:defaults` — per-field defaults for a function's parameter tuple. Checked at attach
// against the parameter's fields; read off the callee value at a call site (like the
// contract keys), so it carries no type identity of its own.

#[test]
fn test_defaults_attaches_and_retrieves() {
    quiver()
        .evaluate("f = #[a: 'int, b: 'int] { :defaults [b: 2]; $a }; f:defaults")
        .expect("[b: 2]");
    // Attaching does not disturb calling.
    quiver()
        .evaluate("f = #[a: 'int, b: 'int] { :defaults [b: 2]; $a }; f [a: 1, b: 9]")
        .expect("1");
}

#[test]
fn test_defaults_same_signature_distinct_values() {
    // Two functions with the same parameter type share a *type*, so the entry cannot
    // live in the type — the values are distinguished by the closures they ride on.
    quiver()
        .evaluate(
            "f = #[a: 'int] { :defaults [a: 1]; $a }
             g = #[a: 'int] { :defaults [a: 2]; $a }
             [f:defaults, g:defaults]",
        )
        .expect("[[a: 1], [a: 2]]");
}

#[test]
fn test_defaults_visible_through_record_member() {
    quiver()
        .evaluate("f = #[a: 'int] { :defaults [a: 7]; $a }; m = [open: f]; m.open:defaults")
        .expect("[a: 7]");
}

#[test]
fn test_defaults_shed_at_declared_boundary() {
    // A declared parameter type erases the row, so the defaults are no longer visible —
    // the same rule that stops a contract being enforced through a declared boundary.
    quiver()
        .evaluate("h = #[f: #[a: 'int] -> 'int] { $f:defaults }; h")
        .expect_error_containing("may carry erased annotations");
}

#[test]
fn test_defaults_requires_function_carrier() {
    quiver()
        .evaluate("x = { :defaults [a: 1]; [a: 5] }; x")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "Annotation :defaults can only be attached to a function".to_string(),
        ));
}

#[test]
fn test_defaults_requires_tuple_parameter() {
    quiver()
        .evaluate("f = #'int { :defaults [b: 2]; $ }; f 1")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "Annotation :defaults requires a tuple parameter, but the function takes 'int"
                .to_string(),
        ));
}

#[test]
fn test_defaults_rejects_unknown_field() {
    quiver()
        .evaluate("f = #[a: 'int, b: 'int] { :defaults [c: 2]; $a }; f:defaults")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "Annotation :defaults names 'c', which is not a field of the parameter \
             [a: 'int, b: 'int]"
                .to_string(),
        ));
}

#[test]
fn test_defaults_rejects_unlabeled_field() {
    quiver()
        .evaluate("f = #[a: 'int, b: 'int] { :defaults [2]; $a }; f:defaults")
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "Annotation :defaults takes labeled fields, each naming a parameter field".to_string(),
        ));
}

#[test]
fn test_defaults_rejects_duplicate_field() {
    // Caught by tuple construction, before the attach-time check sees the value.
    quiver()
        .evaluate("f = #[a: 'int, b: 'int] { :defaults [b: 1, b: 2]; $a }; f:defaults")
        .expect_compile_error(quiver_compiler::compiler::Error::FieldDuplicated(
            "b".to_string(),
        ));
}

#[test]
fn test_defaults_rejects_mistyped_value() {
    quiver()
        .evaluate("f = #[a: 'int, b: 'int] { :defaults [b: <00>]; $a }; f:defaults")
        .expect_error_containing("default for 'b' compatible with 'int");
}
