mod common;
use common::*;

#[test]
fn test_basic_generic_type() {
    quiver()
        .evaluate(
            r#"
            'list<'t> = Nil | Cons['t, ^];
            xs = Cons[42, Cons[99, Nil]];
            xs.0
            "#,
        )
        .expect("42");
}

#[test]
fn test_generic_type_with_different_instantiations() {
    quiver()
        .evaluate(
            r#"
            'list<'t> = Nil | Cons['t, ^];
            Cons[1, Cons[2, Nil]]
            "#,
        )
        .expect_type("Cons['int, Cons['int, Nil]]");

    quiver()
        .evaluate(
            r#"
            'list<'t> = Nil | Cons['t, ^];
            Cons[<aa>, Cons[<bb>, Nil]];
            "#,
        )
        .expect_type("Cons['bin, Cons['bin, Nil]]");
}

#[test]
fn test_generic_function_single_type_param() {
    quiver()
        .evaluate("#<'t>'t { =x => x }")
        .expect_type("#'t -> 't");

    // Inline functions are not auto-called; bind first, then call
    quiver()
        .evaluate("f = #<'t>'t { =x => x }; 42 ~> f ~")
        .expect_type("'int");

    quiver()
        .evaluate("f = #<'t>'t { =x => x }; <00> ~> f ~")
        .expect_type("'bin");
}

#[test]
fn test_generic_function_multiple_type_params() {
    quiver()
        .evaluate(
            r#"
            pair = #<'a, 'b>['a, 'b] { =[x, y] => [y, x] };
            [1, <00>] ~> pair ~
            "#,
        )
        .expect("[<00>, 1]");
}

#[test]
fn test_generic_function_with_same_type_param_widening() {
    // Inline functions are not auto-called; bind first, then call
    quiver()
        .evaluate("f = #<'t>['t, 't] { =[a, _] => a }; [1, <00>] ~> f ~")
        .expect_type("'bin | 'int");
}

#[test]
fn test_generic_function_with_recursive_type() {
    quiver()
        .evaluate(
            r#"
            'list<'t> = Nil | Cons['t, ^];
            head = #<'t>'list<'t> {
              | =Cons[h, _] => h
              | =Nil => 0
            };
            Cons[42, Cons[99, Nil]] ~> head ~
            "#,
        )
        .expect("42");
}

#[test]
fn test_generic_type_structural_equivalence() {
    quiver()
        .evaluate(
            r#"
            'list<'t> = Nil | Cons['t, ^];
            f = #'list<'int> { =xs => xs };
            g = #'list<'int> { =xs => xs };
            xs = Cons[42, Nil];
            xs ~> f ~ ~> g ~
            "#,
        )
        .expect("Cons[42, Nil]");
}

#[test]
fn test_heterogeneous_list_via_widening() {
    quiver()
        .evaluate(
            r#"
            %list.new [] ~> [~, 5] ~> %list.append ~ ~> [~, "a"] ~> %list.append ~;
            "#,
        )
        .expect_type("Cons[('int | Str['bin]), μ1] | Nil");
}

#[test]
fn test_nested_generic_types() {
    quiver()
        .evaluate(
            r#"
            'list<'t> = Nil | Cons['t, ^];
            'pair<'a, 'b> = Pair[fst: 'a, snd: 'b];

            xs = Cons[Pair[fst: 1, snd: 2], Cons[Pair[fst: 3, snd: 4], Nil]];
            xs.0.fst
            "#,
        )
        .expect("1");
}

#[test]
fn test_generic_function_return_type_inference() {
    quiver()
        .evaluate("#<'t>'t { =x => A[x] }")
        .expect_type("#'t -> A['t]");
}

#[test]
fn test_generic_function_with_tuple_fields() {
    quiver()
        .evaluate(
            r#"
            'pair<'a, 'b> = Pair[fst: 'a, snd: 'b];
            swap = #<'a, 'b>'pair<'a, 'b> {
              =Pair[fst: x, snd: y] => Pair[fst: y, snd: x]
            };
            Pair[fst: 1, snd: "a"] ~> swap ~
            "#,
        )
        .expect("Pair[fst: \"a\", snd: 1]");
}

#[test]
fn test_multiple_generic_instantiations_same_structure() {
    quiver()
        .evaluate(
            r#"
            'pair<'t> = Pair[fst: 't, snd: 't];
            p1 = Pair[fst: 1, snd: 2];
            p2 = Pair[fst: 3, snd: 4];
            [p1.fst, p2.fst]
            "#,
        )
        .expect("[1, 3]");
}

#[test]
fn test_generic_type_with_partial_type() {
    quiver()
        .evaluate(
            r#"
            'pair<'t> = Pair[fst: 't, snd: 't];
            get_fst = #<'t>'pair<'t> { .fst };
            Pair[fst: 42, snd: 99] ~> get_fst ~
            "#,
        )
        .expect("42");
}

#[test]
fn test_generic_function_pattern_matching() {
    quiver()
        .evaluate(
            r#"
            'list<'t> = Nil | Cons['t, ^];
            sum_first_two = #'list<'int> {
              | =Cons[a, Cons[b, _]] => [a, b] ~> __integer_add__ ~
              | 0
            };
            Cons[10, Cons[20, Cons[30, Nil]]] ~> sum_first_two ~
            "#,
        )
        .expect("30");
}

#[test]
fn test_wrong_number_of_type_arguments() {
    quiver()
        .evaluate(
            r#"
            'pair<'a, 'b> = Pair['a, 'b];
            f = #'pair<'int> { =x => x }
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "Generic type 'pair' expects 2 type argument(s), got 1".to_string(),
        ));
}

#[test]
fn test_generic_type_cycle_reference() {
    quiver()
        .evaluate(
            r#"
            'tree<'t> = Leaf['t] | Node[^, ^];
            t = Node[Node[Leaf[1], Leaf[2]], Leaf[3]];
            t.0.0.0
            "#,
        )
        .expect("1");
}

#[test]
fn test_generic_function_with_union_result() {
    quiver()
        .evaluate(
            r#"
            'maybe<'t> = None | Some['t];
            wrap_some = #<'t>'t { =x => Some[x] };
            42 ~> wrap_some ~
            "#,
        )
        .expect("Some[42]");
}

#[test]
fn test_generic_nested_function_calls() {
    quiver()
        .evaluate(
            r#"
            'list<'t> = Nil | Cons['t, ^];
            singleton = #<'t>'t { =x => Cons[x, Nil] };
            head = #<'t>'list<'t> { =Cons[h, _] => h };
            42 ~> singleton ~ ~> head ~
            "#,
        )
        .expect("42");
}

#[test]
fn test_generic_type_with_multiple_fields() {
    quiver()
        .evaluate(
            r#"
            'triple<'a, 'b, 'c> = Triple['a, 'b, 'c];
            t = Triple[1, "a", <00>];
            [t.0, t.1, t.2]
            "#,
        )
        .expect("[1, \"a\", <00>]");
}

#[test]
fn test_rigid_type_variable_rejects_concrete_requirement() {
    // A rigid type variable (an enclosing generic's parameter) must not satisfy a
    // concrete requirement: `f`'s 'u is not known to be 'int, so passing it to `g`
    // is a compile error, not a latent runtime TypeMismatch.
    quiver()
        .evaluate(
            r#"
            g = #<'t>['int, 't] { =[a, b]; [a, 1] ~> __integer_add__ ~ };
            f = #<'u>'u { [$, 5] ~> g ~ };
            #[] { "x" ~> f ~ }
            "#,
        )
        .expect_compile_error(quiver_compiler::compiler::Error::TypeUnresolved(
            "in `0`: Cannot unify rigid type variable 'u with expected type 'int".to_string(),
        ));
}

#[test]
fn test_generic_unification_of_resource_and_reference_types() {
    // Unifying Resource with Resource (and Reference with Reference) used to fall
    // through to unify's catch-all and fail — resource-bearing types could not flow
    // through generic parameters.
    quiver()
        .with_io()
        .evaluate(
            r#"pick = #<'t>['t, 't] { $0 }
               ["/tmp/quiver-generic-res-test" ~> .0, 577, 420] ~> __file_open__ ~ ~> =(\File)f
               pick [f, f]"#,
        )
        .expect_type("[] | \\File");
    quiver()
        .evaluate(
            r#"pick = #<'t>['t, 't] { $0 }
               r = %ref []
               pick [r, r] ~> =&r"#,
        )
        .expect("Ok");
}

// === Explicit type application (`f<'t>`) ================================================
// A glued `<…>` suffix on an access head instantiates the callable's declared type
// parameters positionally, pinning them before inference. Purely static: generics are
// erased, so instantiation only narrows the type the use site sees — a pinned parameter
// turns what inference would have widened into a checked assertion.

#[test]
fn test_explicit_type_application_pins_and_checks() {
    // Pinned and compatible: behaves like the inferred call.
    quiver()
        .evaluate(r#"id = #<'t>'t { $ }; id<'int> 42"#)
        .expect("42");
    // Pinned and incompatible: the pin is an assertion, so the call errors instead of
    // inferring 't = 'int.
    quiver()
        .evaluate(r#"id = #<'t>'t { $ }; id<'bin> 42"#)
        .expect_compile_error(quiver_compiler::compiler::Error::TypeMismatch {
            expected: "function parameter compatible with 'bin".to_string(),
            found: "'int".to_string(),
        });
}

#[test]
fn test_explicit_type_application_prefix_partial() {
    // A prefix of the declared parameters may be pinned; the rest stay inferred.
    quiver()
        .evaluate(r#"pair = #<'t, 'u>['t, 'u] { $ }; pair<'int> [1, <0a>]"#)
        .expect("[1, <0a>]");
}

#[test]
fn test_explicit_type_application_arity_and_target_errors() {
    quiver()
        .evaluate(r#"id = #<'t>'t { $ }; id<'int, 'bin> 42"#)
        .expect_compile_error(quiver_compiler::compiler::Error::TypeArgumentsTooMany {
            declared: 1,
            given: 2,
        });
    // A non-generic function has nothing to instantiate.
    quiver()
        .evaluate(r#"f = #'int { $ }; f<'int> 42"#)
        .expect_compile_error(
            quiver_compiler::compiler::Error::TypeArgumentsNotApplicable {
                target: "#'int -> 'int".to_string(),
            },
        );
    // A non-callable value can't take type arguments at all.
    quiver().evaluate(r#"x = 5; x<'int>"#).expect_compile_error(
        quiver_compiler::compiler::Error::TypeArgumentsNotApplicable {
            target: "'int".to_string(),
        },
    );
}

#[test]
fn test_explicit_type_application_declared_boundary_sheds_parameters() {
    // A callable reached through a *written* parameter type has no statically-known
    // declaration, so it cannot be explicitly instantiated — the boundary sheds the
    // type-parameter list exactly as it sheds other definition-carried capabilities.
    quiver()
        .evaluate(r#"h = #<'t>[#'t -> 't] { $0<'int> 5 }"#)
        .expect_compile_error(
            quiver_compiler::compiler::Error::TypeArgumentsNotApplicable {
                target: "#'t -> 't".to_string(),
            },
        );
}

#[test]
fn test_explicit_type_application_reference_and_spawn() {
    // `&f<'int>` instantiates without applying: the loaded value's type is pinned.
    quiver()
        .evaluate(r#"id = #<'t>'t { $ }; f = id<'int>; f 7"#)
        .expect("7");
    // A spawn target takes the suffix; the init argument checks against the pin.
    quiver()
        .evaluate(r#"w = #<'t>'t { $ }; p = 5 ~> @w<'int> ~; !p"#)
        .expect("5");
}

#[test]
fn test_explicit_type_application_pins_literal_inference() {
    // The pinned parameter reaches a `#{…}` literal argument through the Apply-site
    // expected type, exactly as sibling-argument inference does.
    quiver()
        .evaluate(
            r#"ap = #<'t, 'u>['t, #'t -> 'u] { $.1 $.0 }
               ap<'int, 'int> [5, #{ __integer_add__ [$, 1] }]"#,
        )
        .expect("6");
}

#[test]
fn test_explicit_type_application_imported_generic() {
    // An imported generic's declared parameters survive module caching (restored like
    // the dispatch tables), so explicit instantiation works on module members.
    quiver()
        .evaluate(
            r#"0 ~> %iter.unfold [~, #{ [$, __integer_add__ [$, 1]] }]
               ~> %iter.take [~, 3]
               ~> %iter.fold<'int, 'int> [~, 0, #{ __integer_add__ [$0, $1] }]"#,
        )
        .expect("3");
}

#[test]
fn test_explicit_type_application_alias_argument() {
    // Type arguments are ordinary written types: aliases resolve through scope.
    quiver()
        .evaluate(r#"'pt = P[x: 'int]; id = #<'t>'t { $ }; id<'pt> P[x: 1]"#)
        .expect("P[x: 1]");
}

#[test]
fn test_type_parameters_visible_in_body_positions() {
    // A declared type parameter is body-visible as an ordinary alias, so patterns,
    // ascriptions, and checked retrievals can name it.
    quiver()
        .evaluate(
            r#"'box<'t> = Full['t] | Empty
               first_full = #<'t>['box<'t>, 'box<'t>] {
                 | $.0 ~> =('box<'t>)b; b ~> =Full[_] => b
                 | $.1
               }
               first_full [Empty, Full[7]] ~> =Full[x]; x"#,
        )
        .expect("7");
    // A checked annotation retrieval can state a shape mentioning the parameter.
    quiver()
        .evaluate(
            r#"'meta<'t> = [it: 't]
               tag = #<'t>[Box['t], 't] { =[v, m]; v ~> { :meta [it: m] } }
               peek = #<'t>Box['t] { $:('meta<'t>)meta ~> =(it: x); x }
               tag [Box[1], 2] ~> peek ~"#,
        )
        .expect("2");
}

#[test]
fn test_unpinned_result_parameter_closes_to_never() {
    // A call that pins nothing for a result-side type parameter closes it to the
    // empty union: `norm "x"` provably carries no `Evt`, so the result fits a sink
    // whose event union is concrete (a leaked rigid variable would be rejected).
    quiver()
        .evaluate(
            r#"'node<'e> = Leaf[Str['bin]] | Evt['e]
               norm = #<'e>('node<'e> | Str['bin]) {
                 | =Str[b] => Leaf[Str[b]]
                 | =('node<'e>)n => n
               }
               sink = #(Leaf[Str['bin]] | Evt[(Inc | Dec)]) { Ok }
               norm "x" ~> sink ~"#,
        )
        .expect("Ok");
}

#[test]
fn test_enclosing_rigid_variable_survives_inner_calls() {
    // Calling a callable-typed *parameter* inside a generic body leaves the enclosing
    // `'t` rigid in the inner result — it is not the callee's to close.
    quiver()
        .evaluate(
            r#"apply = #<'t>[#[] -> 't] { =[f]; f [] }
               apply [#[] { 42 }] ~> __integer_add__ [~, 1]"#,
        )
        .expect("43");
}

#[test]
fn test_explicit_instantiation_across_repl_entries() {
    // The declared type parameters of a generic defined in an earlier REPL entry must still be
    // known when a later one instantiates it explicitly; they are recorded per compilation, so
    // the session has to carry them (this was `TypeArgumentsNotApplicable`).
    quiver()
        .evaluate("id = #<'t>'t { $ }; Ok")
        .then_evaluate("id<'int> 42")
        .expect("42");

    quiver()
        .evaluate("id = #<'t>'t { $ }; Ok")
        .then_evaluate("f = id<'int>; f 7")
        .expect("7");
}
