mod common;
use common::*;
use quiver_compiler::compiler::Error;

// Exhaustiveness is opt-in via a non-nil return annotation (Quiver's partial matches are a
// first-class idiom). When such a body is a non-exhaustive *enumeration*, the error names the
// unhandled cases instead of the opaque `found T | []`.

#[test]
fn test_non_exhaustive_enumeration_names_unhandled() {
    quiver()
        .evaluate(
            r#"
            'shape = Circle['int] | Rectangle['int, 'int] | Triangle['int];
            area = #'shape -> 'int {
              | =Circle[r] => r
              | =Rectangle[w, h] => w
            },
            Circle[1] ~> area
            "#,
        )
        .expect_compile_error(Error::NonExhaustiveReturn {
            unhandled: "Triangle['int]".to_string(),
            declared: "'int".to_string(),
        });
}

#[test]
fn test_exhaustive_enumeration_compiles() {
    quiver()
        .evaluate(
            r#"
            'shape = Circle['int] | Rectangle['int, 'int];
            area = #'shape -> 'int {
              | =Circle[r] => r
              | =Rectangle[w, h] => w
            },
            Rectangle[3, 4] ~> area
            "#,
        )
        .expect("3");
}

#[test]
fn test_partial_match_without_annotation_is_allowed() {
    // The predicate idiom: a partial match returning nil for unhandled variants is fine when
    // there is no totality annotation to opt into.
    quiver()
        .evaluate(
            r#"
            'shape = Circle['int] | Rectangle['int, 'int];
            is_circle? = #'shape { =Circle[_] => Ok },
            Rectangle[1, 2] ~> is_circle?
            "#,
        )
        .expect("[]");
}

#[test]
fn test_genuine_mismatch_still_reports_type_mismatch() {
    // A real type error (not just missing cases) keeps the ordinary mismatch error.
    quiver()
        .evaluate(
            r#"
            'shape = Circle['int] | Rectangle['int, 'int];
            f = #'shape -> 'bin {
              | =Circle[r] => r
              | =Rectangle[w, h] => w
            },
            Circle[1] ~> f
            "#,
        )
        .expect_compile_error(Error::TypeMismatch {
            expected: "'bin".to_string(),
            found: "'int".to_string(),
        });
}
