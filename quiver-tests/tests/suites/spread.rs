use crate::common::*;

#[test]
fn test_spread_simple() {
    quiver()
        .evaluate("a = A[x: 1, y: 2]; [...a]")
        .expect("[x: 1, y: 2]");
}

#[test]
fn test_spread_with_field_replacement() {
    quiver()
        .evaluate("a = A[x: 1, y: 2]; [...a, y: 3]")
        .expect("[x: 1, y: 3]");
}

#[test]
fn test_spread_with_field_addition() {
    quiver()
        .evaluate("a = A[x: 1, y: 2]; [...a, z: 3]")
        .expect("[x: 1, y: 2, z: 3]");
}

#[test]
fn test_spread_with_unnamed_fields_appended() {
    quiver()
        .evaluate("a = A[x: 1, y: 2]; [...a, 5, 6]")
        .expect("[x: 1, y: 2, 5, 6]");
}

#[test]
fn test_spread_with_unnamed_fields_prepended() {
    quiver()
        .evaluate("a = [x: 1, y: 2]; [5, ...a]")
        .expect("[5, x: 1, y: 2]");
}

#[test]
fn test_spread_multiple_merges() {
    quiver()
        .evaluate("a = [y: 2]; [x: 3] ~> [...a, ..., z: 4]")
        .expect("[y: 2, x: 3, z: 4]");
}

#[test]
fn test_spread_with_new_name() {
    quiver()
        .evaluate("a = A[x: 1, y: 2]; B[...a, y: 3]")
        .expect("B[x: 1, y: 3]");
}

#[test]
fn test_spread_empty_tuple() {
    quiver().evaluate("a = []; [...a, x: 1]").expect("[x: 1]");
}

#[test]
fn test_spread_multiple_spreads() {
    quiver()
        .evaluate("a = [x: 1]; b = [y: 2]; [...a, ...b]")
        .expect("[x: 1, y: 2]");
}

#[test]
fn test_spread_with_overlapping_names() {
    quiver()
        .evaluate("a = [x: 1, y: 2]; b = [y: 3, z: 4]; [...a, ...b]")
        .expect("[x: 1, y: 3, z: 4]");
}

#[test]
fn test_spread_preserves_only_unnamed_fields() {
    quiver()
        .evaluate("a = [1, 2]; b = [3, 4]; [...a, ...b]")
        .expect("[1, 2, 3, 4]");
}

#[test]
fn test_spread_union_single_spread() {
    quiver()
        .evaluate(
            r#"
            't = [x: 'int] | [y: 'int];
            f = #[v: 't] { =[v: v] => [z: 0, ...v] };
            [v: [x: 1]] ~> f ~
            "#,
        )
        .expect("[z: 0, x: 1]");
}

#[test]
fn test_spread_union_cartesian_product() {
    quiver()
        .evaluate(
            r#"
            'ta = [x: 'int] | [y: 'int];
            'tb = [z: 'int] | [w: 'int];
            f = #[a: 'ta, b: 'tb] { =[a: a, b: b] => [...a, ...b] };
            [a: [x: 1], b: [z: 2]] ~> f ~
            "#,
        )
        .expect("[x: 1, z: 2]");
}

#[test]
fn test_spread_union_multiple_spreads() {
    quiver()
        .evaluate(
            r#"
            'ta = [x: 'int];
            'tb = [z: 'int] | [w: 'int];
            f = #[a: 'ta, b: 'tb] { =[a: a, b: b] => [...a, ...b] };
            [a: [x: 1], b: [w: 2]] ~> f ~
            "#,
        )
        .expect("[x: 1, w: 2]");
}

#[test]
fn test_spread_same_fields_different_sources() {
    quiver()
        .evaluate(
            r#"
            'ta = [x: 'int];
            'tb = [x: 'int, y: 'int] | [y: 'int];
            f = #[a: 'ta, b: 'tb] { =[a: a, b: b] => [...a, ...b] };
            [[a: [x: 1], b: [x: 2, y: 3]] ~> f ~, [a: [x: 4], b: [y: 5]] ~> f ~]
            "#,
        )
        .expect("[[x: 2, y: 3], [x: 4, y: 5]]");
}

#[test]
fn test_identifier_spread_preserves_name() {
    quiver()
        .evaluate("a = A[x: 1]; a[..., y: 2]")
        .expect("A[x: 1, y: 2]");
}

#[test]
fn test_identifier_spread_field_override() {
    quiver()
        .evaluate("a = A[x: 1, y: 2]; a[..., y: 99]")
        .expect("A[x: 1, y: 99]");
}

#[test]
fn test_identifier_spread_unnamed_tuple() {
    quiver()
        .evaluate("a = [x: 1]; a[..., y: 2]")
        .expect("[x: 1, y: 2]");
}

#[test]
fn test_identifier_spread_multiple_fields() {
    quiver()
        .evaluate("p = Point[x: 10, y: 20]; p[..., z: 30, color: <ff>]")
        .expect("Point[x: 10, y: 20, z: 30, color: <ff>]");
}

#[test]
fn test_ripple_spread_preserves_name() {
    quiver()
        .evaluate("A[x: 1] ~> ~[..., y: 2]")
        .expect("A[x: 1, y: 2]");
}

#[test]
fn test_ripple_spread_field_override() {
    quiver()
        .evaluate("A[x: 1, y: 2] ~> ~[..., y: 99]")
        .expect("A[x: 1, y: 99]");
}

#[test]
fn test_ripple_spread_unnamed_tuple() {
    quiver()
        .evaluate("[x: 1] ~> ~[..., y: 2]")
        .expect("[x: 1, y: 2]");
}

#[test]
fn test_ripple_spread_multiple_fields() {
    quiver()
        .evaluate("Point[x: 10, y: 20] ~> ~[..., z: 30, color: <ff>]")
        .expect("Point[x: 10, y: 20, z: 30, color: <ff>]");
}

#[test]
fn test_nested_spread_with_ripple_context() {
    quiver()
        .evaluate("[1, 2, 3] ~> [outer: ~, nested: [inner: ~, ...]]")
        .expect("[outer: [1, 2, 3], nested: [inner: [1, 2, 3], 1, 2, 3]]");
}

#[test]
fn test_identifier_spread_with_ripple_in_field() {
    // Ripple in field value should resolve to the chained value, not the spread source
    quiver()
        .evaluate("a = A[x: 1, y: 2]; 3 ~> a[..., y: ~]")
        .expect("A[x: 1, y: 3]");
}

#[test]
fn spread_source_is_captured_in_a_closure() {
    // A closure that spreads a variable (`[...a]` or the `a` of `a[..., y]`) must capture it.
    quiver()
        .evaluate("a = A[x: 1]; g = #[] { a[..., y: 2] }; [] ~> g ~")
        .expect("A[x: 1, y: 2]");
    quiver()
        .evaluate("a = [x: 1]; g = #[] { [...a, y: 2] }; [] ~> g ~")
        .expect("[x: 1, y: 2]");
}

#[test]
fn test_identifier_spread_with_ripple_replacing_multiple_fields() {
    // Ripple can be used in multiple fields
    quiver()
        .evaluate("a = A[x: 1, y: 2, z: 3]; 99 ~> a[..., y: ~, z: ~]")
        .expect("A[x: 1, y: 99, z: 99]");
}

#[test]
fn test_name_inheriting_spread_over_union_source() {
    // A name-inheriting spread whose source is a *union* — e.g. an ascribed value
    // alongside a fallback, whose constructions intern as distinct tuple ids — keeps
    // the shared name in its TYPE (the value always kept it): every member named
    // `Response` means the result is a `Response`, so name-matched calls compile.
    quiver()
        .evaluate(
            r#"'hdrs = Nil | Cons[Str['bin], ^]
               'r = Response[status: 'int, headers: 'hdrs]
               check = #'r { $status }
               f = #('int | []) {
                 resp = $ ~> { =('int & s) => Response[status: s, headers: Nil] | Response[status: 500, headers: Cons["a", Nil]] }
                 resp ~> ~[..., status: 201] ~> check ~
               }
               f 200"#,
        )
        .expect("201");
    // Mixed names: each member keeps its own, so an `Other` still doesn't fit a
    // `Response` call.
    quiver()
        .evaluate(
            r#"check = #Response(status: 'int) { $status }
               f = #('int | []) {
                 v = $ ~> { =('int & s) => Response[status: s] | Other[status: 500] }
                 v ~> ~[..., status: 201] ~> check ~
               }
               f 200"#,
        )
        .expect_type_mismatch();
}

#[test]
fn test_name_inheriting_spread_keeps_each_members_name() {
    quiver()
        .evaluate(
            "'e = A[x: 'int] | B[y: 'bin]
             f = #'e { $[..., at: 1] }
             [f A[x: 5], f B[y: <01>]]",
        )
        .expect("[A[x: 5, at: 1], B[y: <01>, at: 1]]");
    quiver()
        .evaluate("'e = A[x: 'int] | B[y: 'bin]; #'e { $[..., at: 1] }")
        .expect_type("#(A[x: 'int] | B[y: 'bin]) -> (A[x: 'int, at: 'int] | B[y: 'bin, at: 'int])");
    // Nil is unnamed, so its update is too.
    quiver()
        .evaluate("f = #(A[x: 'int] | []) { $[..., y: 1] }; [f A[x: 1], f []]")
        .expect("[A[x: 1, y: 1], [y: 1]]");
}

#[test]
fn test_spread_of_a_union_with_a_non_tuple_member_is_rejected() {
    // The fields are read from the source's layout, so every member must be a tuple.
    quiver()
        .evaluate("f = #(A[x: 'int] | 'int) { [...$, y: 1] }; f 5")
        .expect_error_containing("expected tuple, found 'int | A[x: 'int]");
}

#[test]
fn test_spread_of_a_partial_is_rejected() {
    // A partial's values keep their fields wherever their own construction put them, so
    // there is no layout to copy by.
    quiver()
        .evaluate("f = #(x: 'int) { [...$, y: 1] }; f [x: 1]")
        .expect_error_containing("Cannot spread (x: 'int): a partial type has no fixed layout");
    quiver()
        .evaluate("f = #(A[x: 'int] | (x: 'int)) { $[..., y: 1] }; f A[x: 1]")
        .expect_error_containing("Cannot spread (x: 'int)");
    // Through a bound too: a field update needs a concrete-tuple bound.
    quiver()
        .evaluate("touch = #<'t: (at: 'int)>'t { $[..., at: 0] }; touch Post[at: 3]")
        .expect_error_containing("Cannot spread (at: 'int)");
}

// Sourced spreads: the spread's source may be an access path — a variable path (`a.b`),
// a parameter or its fields (`$`, `$conn`, `$$x`), or a ripple field (`~.f`) — in both
// the in-tuple form (`[...$c, y]`) and the name-preserving update (`$conn[..., y]`).

#[test]
fn test_spread_update_parameter_field() {
    quiver()
        .evaluate(
            "f = #[conn: Conn[sock: 'int, buf: 'int]] { $conn[..., buf: 9] }; f [conn: Conn[sock: 1, buf: 2]]",
        )
        .expect("Conn[sock: 1, buf: 9]");
}

#[test]
fn test_spread_update_whole_parameter() {
    quiver()
        .evaluate("f = #P[a: 'int, b: 'int] { $[..., b: 9] }; f P[a: 1, b: 2]")
        .expect("P[a: 1, b: 9]");
}

#[test]
fn test_spread_update_variable_path() {
    quiver()
        .evaluate("a = [inner: P[x: 1, y: 2]]; a.inner[..., y: 5]")
        .expect("P[x: 1, y: 5]");
}

#[test]
fn test_spread_update_ripple_field() {
    // `~.f[..., y]` updates a field of the flowing value, keeping its name.
    quiver()
        .evaluate("[w: P[x: 1, y: 2]] ~> ~.w[..., y: 8]")
        .expect("P[x: 1, y: 8]");
}

#[test]
fn test_spread_update_outer_parameter() {
    quiver()
        .evaluate("f = #[c: P[x: 'int]] { g = #[] { $$c[..., x: 9] }; g [] }; f [c: P[x: 1]]")
        .expect("P[x: 9]");
}

#[test]
fn test_sourced_spread_in_tuple() {
    // The in-tuple form drops or renames the source's tuple name, as for `...a`.
    quiver()
        .evaluate("f = #[c: P[x: 'int]] { [...$c, extra: 7] }; f [c: P[x: 1]]")
        .expect("[x: 1, extra: 7]");
    quiver()
        .evaluate("a = [q: [x: 1, y: 2]]; B[...a.q, y: 3]")
        .expect("B[x: 1, y: 3]");
    quiver()
        .evaluate("[w: P[x: 1]] ~> [...~.w, y: 5]")
        .expect("[x: 1, y: 5]");
}

#[test]
fn test_spread_source_captured_path_in_closure() {
    // A sourced spread in a closure captures its access path, like the expression would.
    quiver()
        .evaluate("p = [q: A[x: 1]]; f = #[] { p.q[..., x: 2] }; f []")
        .expect("A[x: 2]");
}

#[test]
fn test_spread_source_unknown_field_is_a_compile_error() {
    quiver()
        .evaluate("f = #[c: P[x: 'int]] { $c[..., y: 2] ~> .y }; f [c: P[x: 1]]")
        .expect("2");
    quiver()
        .evaluate("a = [x: 1]; a.z[..., y: 2]")
        .expect_compile_error(quiver_compiler::compiler::Error::MemberFieldNotFound {
            field_name: "z".to_string(),
            target: "a".to_string(),
        });
}
