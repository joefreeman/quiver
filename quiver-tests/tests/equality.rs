mod common;
use common::*;

// Value equality is tested by matching against a pinned reference (`x =&y`), which compiles to the
// same structural equality check the language uses everywhere. A successful match yields Ok; a
// failed one yields nil ([]).

#[test]
fn test_equal_integers() {
    quiver().evaluate("a = 42, 42 =&a").expect("Ok");
}

#[test]
fn test_unequal_integers() {
    quiver().evaluate("a = 1, 2 =&a").expect("[]");
}

#[test]
fn test_binary_equality_content() {
    // Binaries with the same content are equal even if built as different values.
    quiver()
        .evaluate("a = 0x68656c6c6f, 0x68656c6c6f =&a")
        .expect("Ok");
    quiver().evaluate("a = 0xabcd, 0xabcd =&a").expect("Ok");
}

#[test]
fn test_binary_inequality_content() {
    quiver()
        .evaluate("a = 0x68656c6c6f, 0x776f726c64 =&a")
        .expect("[]");
    quiver()
        .evaluate("a = 0x61626364, 0x65666768 =&a")
        .expect("[]");
}

#[test]
fn test_equal_strings() {
    quiver().evaluate("a = \"abc\", \"abc\" =&a").expect("Ok");
}

#[test]
fn test_structural_equality_across_construction() {
    // Value equality is structural: two tuples with the same name, field labels, and elements are
    // equal even when built via paths that inferred different field types (so they carry different
    // internal tuple type-ids). Here a literal list vs. one built by a generic function.
    quiver()
        .evaluate(
            "mk = #<'t>['t, 't] { =[x, y], Cons[x, Cons[y, Nil]] }, \
             a = Cons[1, Cons[2, Nil]], [1, 2] mk =&a",
        )
        .expect("Ok");
}

#[test]
fn test_structural_equality_distinct_shapes_differ() {
    // Same arity/elements but different field labels are NOT equal...
    quiver().evaluate("a = [x: 1], [y: 1] =&a").expect("[]");
    // ...nor are different tuple names.
    quiver().evaluate("a = A[1], B[1] =&a").expect("[]");
}
