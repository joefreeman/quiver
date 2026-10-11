//! Building compile-time values with their exact types: the context a dialect is given, and its
//! content read as code (`'%meta.expr`, which `std/meta.qv` defines). Each value is built with
//! its exact tuple type, as a literal would be.

use std::rc::Rc;

use quiver_core::binary::BinaryData;
use quiver_core::program::Program;
use quiver_core::types::Type;
use quiver_core::value::{Binary, Value};

pub fn binary(program: &mut Program, bytes: &[u8]) -> (Value, usize) {
    (
        Value::Binary(Binary::Data(Rc::new(BinaryData::new(bytes.to_vec())))),
        program.register_type(Type::Binary),
    )
}

/// A named tuple of `fields`, typed exactly.
pub fn node(
    program: &mut Program,
    name: &str,
    fields: Vec<(Option<&str>, (Value, usize))>,
) -> (Value, usize) {
    let fields = fields
        .into_iter()
        .map(|(label, field)| (label.map(str::to_string), field))
        .collect();
    tuple(program, Some(name), fields)
}

/// A tuple of `fields`, typed exactly.
pub fn tuple(
    program: &mut Program,
    name: Option<&str>,
    fields: Vec<(Option<String>, (Value, usize))>,
) -> (Value, usize) {
    let (labels, values): (Vec<_>, Vec<_>) = fields
        .into_iter()
        .map(|(label, (value, type_id))| ((label, type_id), value))
        .unzip();
    let tuple = program.register_tuple(name.map(str::to_string), labels);
    let type_id = program.register_type(Type::Tuple(tuple));
    (Value::tuple(tuple, values), type_id)
}

/// `items` as a `Nil | Cons[…]` list.
pub fn list(program: &mut Program, items: Vec<(Value, usize)>) -> (Value, usize) {
    items
        .into_iter()
        .rev()
        .fold(node(program, "Nil", vec![]), |tail, head| {
            node(program, "Cons", vec![(None, head), (None, tail)])
        })
}

pub fn string(program: &mut Program, text: &str) -> (Value, usize) {
    binary_string(program, text.as_bytes().to_vec())
}

pub fn binary_string(program: &mut Program, bytes: Vec<u8>) -> (Value, usize) {
    let bytes = binary(program, &bytes);
    node(program, "Str", vec![(None, bytes)])
}

/// `[]`, for an absent optional field.
pub fn absent(program: &mut Program) -> (Value, usize) {
    tuple(program, None, vec![])
}

pub fn integer(program: &mut Program, n: usize) -> (Value, usize) {
    let integer = program.register_type(Type::Integer);
    let n = i64::try_from(n).expect("a count fits an integer");
    (Value::Int(n), integer)
}
