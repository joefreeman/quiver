use crate::bytecode::Constant;
use crate::program::Program;
use crate::types::{TupleTypeInfo, Type, TypeLookup};
use crate::value::{Binary, Value};

/// Trait for looking up binary data from Binary references
pub trait BinaryLookup {
    /// Get the bytes for a binary reference, or None if not found
    fn get_bytes(&self, binary: &Binary) -> Option<&[u8]>;
}

/// Helper function to format bytes as a string if they represent valid UTF-8 text
fn try_format_as_string(bytes: &[u8]) -> Option<String> {
    let s = String::from_utf8(bytes.to_vec()).ok()?;

    if s.contains('\0')
        || s.chars()
            .any(|c| c.is_control() && !matches!(c, '\n' | '\r' | '\t'))
    {
        return None;
    }

    let escaped = s
        .chars()
        .map(|ch| match ch {
            '"' => "\\\"".to_string(),
            '\\' => "\\\\".to_string(),
            '\n' => "\\n".to_string(),
            '\r' => "\\r".to_string(),
            '\t' => "\\t".to_string(),
            c if c.is_control() => format!("\\u{{{:04x}}}", c as u32),
            c => c.to_string(),
        })
        .collect::<String>();

    Some(format!("\"{}\"", escaped))
}

/// Format a type by its ID
pub fn format_type_by_id(lookup: &impl TypeLookup, type_id: usize) -> String {
    if let Some(type_def) = lookup.lookup_type(type_id) {
        format_type(lookup, type_def)
    } else {
        format!("Type{}", type_id)
    }
}

pub fn format_type(lookup: &impl TypeLookup, type_def: &Type) -> String {
    format_type_impl(lookup, type_def, false)
}

fn format_type_impl(lookup: &impl TypeLookup, type_def: &Type, nested: bool) -> String {
    match type_def {
        Type::Integer => "'int".to_string(),
        Type::Binary => "'bin".to_string(),
        Type::Reference => "'ref".to_string(),
        Type::Process { send, receive } => {
            let formatted = match (send, receive) {
                (Some(send_id), Some(receive_id)) => {
                    let send_str = lookup
                        .lookup_type(*send_id)
                        .map(|t| format_type_impl(lookup, t, true))
                        .unwrap_or_else(|| format!("Type{}", send_id));
                    let receive_str = lookup
                        .lookup_type(*receive_id)
                        .map(|t| format_type_impl(lookup, t, true))
                        .unwrap_or_else(|| format!("Type{}", receive_id));
                    format!("@{} -> {}", send_str, receive_str)
                }
                (Some(send_id), None) => {
                    let send_str = lookup
                        .lookup_type(*send_id)
                        .map(|t| format_type_impl(lookup, t, true))
                        .unwrap_or_else(|| format!("Type{}", send_id));
                    format!("@{} -> ?", send_str)
                }
                (None, Some(receive_id)) => {
                    let receive_str = lookup
                        .lookup_type(*receive_id)
                        .map(|t| format_type_impl(lookup, t, true))
                        .unwrap_or_else(|| format!("Type{}", receive_id));
                    format!("@-> {}", receive_str)
                }
                (None, None) => "@".to_string(),
            };

            if nested {
                format!("({})", formatted)
            } else {
                formatted
            }
        }
        Type::Tuple(tuple_id) => format_tuple_type(lookup, *tuple_id),
        Type::Partial { name, fields } => format_partial_type(lookup, name.as_ref(), fields),
        Type::Callable {
            parameter,
            result,
            receive: _,
        } => {
            let param_str = lookup
                .lookup_type(*parameter)
                .map(|t| format_type_impl(lookup, t, true))
                .unwrap_or_else(|| format!("Type{}", parameter));
            let result_str = lookup
                .lookup_type(*result)
                .map(|t| format_type_impl(lookup, t, true))
                .unwrap_or_else(|| format!("Type{}", result));
            let formatted = format!("#{} -> {}", param_str, result_str);

            if nested {
                format!("({})", formatted)
            } else {
                formatted
            }
        }
        Type::Cycle(depth) => format!("μ{}", depth),
        Type::Union(type_ids) => {
            if type_ids.is_empty() {
                "never".to_string()
            } else {
                let mut type_strs: Vec<String> = type_ids
                    .iter()
                    .map(|&id| {
                        lookup
                            .lookup_type(id)
                            .map(|t| format_type_impl(lookup, t, true))
                            .unwrap_or_else(|| format!("Type{}", id))
                    })
                    .collect();

                // Sort for consistent output
                type_strs.sort();

                if nested {
                    format!("({})", type_strs.join(" | "))
                } else {
                    type_strs.join(" | ")
                }
            }
        }
        Type::Resource(name) => format!("\\{}", name),
        Type::Variable(name) => format!("'{}", name),
    }
}

/// Format a tuple type by its tuple_id
fn format_tuple_type(lookup: &impl TypeLookup, tuple_id: usize) -> String {
    if let Some(type_info) = lookup.lookup_tuple(tuple_id) {
        format_tuple_info(lookup, type_info)
    } else {
        format!("Tuple{}", tuple_id)
    }
}

/// Format a partial type with inline fields
fn format_partial_type(
    lookup: &impl TypeLookup,
    name: Option<&String>,
    fields: &[(String, usize)],
) -> String {
    let field_strs: Vec<String> = fields
        .iter()
        .map(|(field_name, field_type_id)| {
            let field_type_str = lookup
                .lookup_type(*field_type_id)
                .map(|t| format_type_impl(lookup, t, true))
                .unwrap_or_else(|| format!("Type{}", field_type_id));
            format!("{}: {}", field_name, field_type_str)
        })
        .collect();

    if let Some(type_name) = name {
        if field_strs.is_empty() {
            format!("{}()", type_name)
        } else {
            format!("{}({})", type_name, field_strs.join(", "))
        }
    } else {
        format!("({})", field_strs.join(", "))
    }
}

/// Format a TupleTypeInfo using a TypeLookup for field type resolution
pub fn format_tuple_info(lookup: &impl TypeLookup, tuple_info: &TupleTypeInfo) -> String {
    let field_strs: Vec<String> = tuple_info
        .fields
        .iter()
        .map(|(field_name, field_type_id)| {
            let field_type_str = lookup
                .lookup_type(*field_type_id)
                .map(|t| format_type_impl(lookup, t, true))
                .unwrap_or_else(|| format!("Type{}", field_type_id));
            if let Some(field_name) = field_name {
                format!("{}: {}", field_name, field_type_str)
            } else {
                field_type_str
            }
        })
        .collect();

    if let Some(type_name) = &tuple_info.name {
        if field_strs.is_empty() {
            type_name.to_string()
        } else {
            format!("{}[{}]", type_name, field_strs.join(", "))
        }
    } else {
        format!("[{}]", field_strs.join(", "))
    }
}

/// Format a binary value showing its actual content
/// Number of leading bytes shown before a long binary is truncated.
const BINARY_DISPLAY_BYTES: usize = 8;

fn format_binary(bytes: &[u8]) -> String {
    if bytes.len() <= BINARY_DISPLAY_BYTES {
        format!("0x{}", hex::encode(bytes))
    } else {
        // Show a prefix and the total length for long binaries.
        format!(
            "0x{}… ({} bytes)",
            hex::encode(&bytes[..BINARY_DISPLAY_BYTES]),
            bytes.len()
        )
    }
}

/// Standard implementation of BinaryLookup using heap and program constants
pub struct HeapAndProgramLookup<'a> {
    pub heap: &'a [Vec<u8>],
    pub program: &'a Program,
}

impl BinaryLookup for HeapAndProgramLookup<'_> {
    fn get_bytes(&self, binary: &Binary) -> Option<&[u8]> {
        match binary {
            Binary::Constant(idx) => {
                if let Some(Constant::Binary(bytes)) = self.program.get_constant(*idx) {
                    Some(bytes.as_slice())
                } else {
                    None
                }
            }
            Binary::Heap(idx) => self.heap.get(*idx).map(|v| v.as_slice()),
        }
    }
}

/// Implementation of BinaryLookup using bytecode constants and heap
pub struct BytecodeBinaryLookup<'a> {
    pub constants: &'a [Constant],
    pub heap: &'a [Vec<u8>],
}

impl BinaryLookup for BytecodeBinaryLookup<'_> {
    fn get_bytes(&self, binary: &Binary) -> Option<&[u8]> {
        match binary {
            Binary::Constant(idx) => self.constants.get(*idx).and_then(|c| match c {
                Constant::Binary(bytes) => Some(bytes.as_slice()),
                _ => None,
            }),
            Binary::Heap(idx) => self.heap.get(*idx).map(|v| v.as_slice()),
        }
    }
}

pub fn format_value<T: TypeLookup, B: BinaryLookup>(
    value: &Value,
    type_lookup: &T,
    binary_lookup: &B,
) -> String {
    match value {
        Value::Function(function, _) => format!("#{}", function),
        Value::Builtin(name) => format!("__{}__", name),
        Value::Integer(i) => i.to_string(),
        Value::Binary(binary) => {
            if let Some(bytes) = binary_lookup.get_bytes(binary) {
                format_binary(bytes)
            } else {
                "<binary>".to_string()
            }
        }
        Value::Process(process_id, _) => format!("@{}", process_id),
        Value::Resource(resource_id, _) => format!("\\#{}", resource_id),
        Value::Reference(r) => {
            let worker_id = r >> 48;
            let counter = r & 0xFFFFFFFFFFFF;
            format!("&{}:{}", worker_id, counter)
        }
        Value::Tuple(tuple_id, elements) => {
            if let Some(tuple_info) = type_lookup.lookup_tuple(*tuple_id) {
                // Check for Str type and format as string if possible
                if tuple_info.name.as_deref() == Some("Str")
                    && let [Value::Binary(binary)] = elements.as_slice()
                    && let Some(bytes) = binary_lookup.get_bytes(binary)
                    && let Some(s) = try_format_as_string(bytes)
                {
                    return s;
                }

                let name = tuple_info.name.as_deref();
                let field_strs: Vec<String> = elements
                    .iter()
                    .enumerate()
                    .map(|(i, elem)| {
                        let formatted = format_value(elem, type_lookup, binary_lookup);
                        if let Some((Some(field_name), _)) = tuple_info.fields.get(i) {
                            format!("{}: {}", field_name, formatted)
                        } else {
                            formatted
                        }
                    })
                    .collect();

                if let Some(name) = name {
                    if field_strs.is_empty() {
                        name.to_string()
                    } else {
                        format!("{}[{}]", name, field_strs.join(", "))
                    }
                } else {
                    format!("[{}]", field_strs.join(", "))
                }
            } else {
                // Fallback to simple format
                if elements.is_empty() {
                    format!("T{}", tuple_id)
                } else {
                    let formatted_elements: Vec<String> = elements
                        .iter()
                        .map(|e| format_value(e, type_lookup, binary_lookup))
                        .collect();
                    format!("T{}[{}]", tuple_id, formatted_elements.join(", "))
                }
            }
        }
    }
}
