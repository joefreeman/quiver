use crate::common::*;

#[test]
fn test_tagged_union_plans_in_bounded_steps() {
    // Every variant of a union has read and write alternatives of its own, each a tag check
    // around its content, so a union of many variants must still plan in few steps.
    let variants: Vec<String> = (0..30)
        .map(|n| match n % 3 {
            0 => format!("V{n}[x{n}: 'int, y: '%str]"),
            1 => format!("V{n}['int, 'int]"),
            _ => format!("V{n}"),
        })
        .collect();
    quiver()
        .evaluate(&format!(
            "'u = {}
             tag = Adjacent[tag: \"t\", content: \"c\"]
             doc = %codec.convert<'%json, 'u> [V28[1, 2], using: %json.edges, tag: tag]
             %codec.convert<'u, ('%json | [])> [doc, using: %json.edges, tag: tag]",
            variants.join(" | ")
        ))
        .expect("V28[1, 2]");
}

#[test]
fn test_newtype_wraps_once_without_a_structural_step() {
    // A newtype reads its content from the value it is given, so wrapping alone never moves into
    // the value: a type wrapping itself must not wrap without end.
    quiver()
        .evaluate("'t = Id[^] | End\n%codec.convert<'t> [%json{ \"End\" }, using: %json.edges]")
        .expect("Id[End]");
    quiver()
        .evaluate("'t = Id[^] | Nil\n%codec.convert<'t> [%json{ 5 }, using: %json.edges]")
        .expect_error_containing("no route");
}

#[test]
fn test_standard_types_convert_only_through_their_own_functions() {
    // A dict's representation is positional tuples, which a JSON array would otherwise read
    // field by field, filing an entry under a hash that isn't its key's. A format's edges include
    // the standard library's, which claim them: a dict converts only through `entries` and `from`.
    quiver()
        .evaluate(
            "%codec.convert<'%dict<'%str, 'int>> [%json{ [0, \"a\", 1] }, using: %json.edges]",
        )
        .expect("[]");
    quiver()
        .evaluate(
            "d = %codec.convert<'%dict<'%str, 'int>> [%json{ {\"a\": 1} }, using: %json.edges]
             ~> ='%dict<'%str, 'int>
             [%dict.get [d, \"a\"], %codec.convert<'%json> [d, using: %json.edges] ~> ='%json ~> %json.stringify]",
        )
        .expect("[1, \"{\\\"a\\\":1}\"]");
}

#[test]
fn test_a_program_tuple_sharing_a_dicts_tuple_name_is_its_own() {
    // The standard library's dict is built of `Node[…]`s, and so is a program's tree: a claim is
    // by type, and a generic edge reaches a target only by overlapping it, so the tree converts
    // as itself.
    quiver()
        .evaluate(
            "'tree = Leaf['int] | Node[^, ^]
             %codec.convert<'%json, 'tree> [Node[Leaf[1], Leaf[2]], using: %json.edges, tag: External]
             ~> ='%json ~> %json.stringify",
        )
        .expect("\"{\\\"Node\\\":[{\\\"Leaf\\\":1},{\\\"Leaf\\\":2}]}\"");
}

#[test]
fn test_unnamed_tuples_are_never_claimed() {
    // JSON's document holds `[key, value]` pairs, but an unnamed tuple has no identity to claim,
    // so a program's own pair still reads from an array.
    quiver()
        .evaluate("%codec.convert<['int, 'int]> [%json{ [1, 2] }, using: %json.edges]")
        .expect("[1, 2]");
}

#[test]
fn test_document_symbols_are_not_text() {
    // JSON's `true` is its document's own symbol, not a program's, so it is never written as the
    // text "True".
    quiver()
        .evaluate("%codec.convert<[name: '%str]> [%json{ {\"name\": true} }, using: %json.edges]")
        .expect("[]");
}
