mod common;
use common::*;

#[test]
fn test_ref_creation() {
    // Each %ref creates a unique ref
    quiver().evaluate("%ref =r, r").expect_type("'ref");
}

#[test]
fn test_ref_uniqueness() {
    // Multiple refs are unique
    quiver()
        .evaluate("[%ref, %ref, %ref] =[a, b, c], [[a, b] ==, [b, c] ==, [a, c] ==]")
        .expect("[[], [], []]");
}

#[test]
fn test_ref_via_reference_binding() {
    // Binding the function with `&%ref` mints a fresh ref on each call
    quiver().evaluate("ref = &%ref, [ref, ref] ==").expect("[]");
}

#[test]
fn test_ref_equality_same() {
    // Same ref compared to itself is equal (returns the ref, not nil)
    quiver().evaluate("r = %ref, [r, r] == =[]").expect("[]"); // Negation of non-nil is nil
}

#[test]
fn test_ref_equality_different() {
    // Different refs are not equal
    quiver().evaluate("[%ref, %ref] ==").expect("[]");
}

#[test]
fn test_ref_in_tuple() {
    // Refs can be stored in tuples
    quiver()
        .evaluate("r = %ref, [tag: r, data: 42] .tag")
        .expect_type("'ref");
}

#[test]
fn test_ref_pattern_matching() {
    // Refs can be used in pattern matching with pinning
    quiver()
        .evaluate("tag = %ref, [tag: tag, data: 42] =[tag: &tag, data: d], d")
        .expect("42");
}

#[test]
fn test_ref_pattern_matching_mismatch() {
    // Pattern match fails when ref doesn't match
    quiver()
        .evaluate("tag1 = %ref, tag2 = %ref, [tag: tag1, data: 42] =[tag: &tag2, data: d], d")
        .expect("[]");
}

#[test]
fn test_ref_type_annotation() {
    // ref type can be used in function signatures
    quiver()
        .evaluate("f = #'ref { $ }, %ref f")
        .expect_type("'ref");
}

#[test]
fn test_ref_in_union_type() {
    // ref can be part of union types
    quiver()
        .evaluate("f = #('int | 'ref) { | ='int => 1 | 2 }, %ref f")
        .expect("2");
    quiver()
        .evaluate("f = #('int | 'ref) { | ='int => 1 | 2 }, 42 f")
        .expect("1");
}
