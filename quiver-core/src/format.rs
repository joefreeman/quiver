use crate::bytecode::Constant;
use crate::program::Program;
use crate::types::{TupleLookup, Type};
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
        || !s
            .chars()
            .any(|c| !c.is_control() || matches!(c, '\n' | '\r' | '\t'))
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

pub fn format_type(program: &crate::program::Program, type_def: &Type) -> String {
    format_type_impl(program, type_def, false)
}

fn format_type_impl(program: &crate::program::Program, type_def: &Type, nested: bool) -> String {
    match type_def {
        Type::Integer => "int".to_string(),
        Type::Binary => "bin".to_string(),
        Type::Process(process_type) => {
            let formatted = match (&process_type.receive, &process_type.returns) {
                (Some(receive), Some(returns)) => {
                    format!(
                        "@{} -> {}",
                        format_type_impl(program, receive, true),
                        format_type_impl(program, returns, true)
                    )
                }
                (Some(receive), None) => {
                    format!("@{}", format_type_impl(program, receive, true))
                }
                (None, Some(returns)) => {
                    format!("@-> {}", format_type_impl(program, returns, true))
                }
                (None, None) => "@".to_string(),
            };

            if nested {
                format!("({})", formatted)
            } else {
                formatted
            }
        }
        Type::Tuple(type_id) | Type::Partial(type_id) => {
            let is_partial = matches!(type_def, Type::Partial(_));
            if let Some(type_info) = program.lookup_tuple(*type_id) {
                let field_strs: Vec<String> = type_info
                    .fields
                    .iter()
                    .map(|(field_name, field_type)| {
                        if let Some(field_name) = field_name {
                            format!(
                                "{}: {}",
                                field_name,
                                format_type_impl(program, field_type, true)
                            )
                        } else {
                            format_type_impl(program, field_type, true)
                        }
                    })
                    .collect();

                let bracket = if is_partial { ('(', ')') } else { ('[', ']') };

                if let Some(type_name) = &type_info.name {
                    if field_strs.is_empty() {
                        type_name.to_string()
                    } else {
                        format!(
                            "{}{}{}{}",
                            type_name,
                            bracket.0,
                            field_strs.join(", "),
                            bracket.1
                        )
                    }
                } else {
                    format!("{}{}{}", bracket.0, field_strs.join(", "), bracket.1)
                }
            } else {
                format!("Type{}", type_id)
            }
        }
        Type::Callable(func_type) => {
            let param_str = format_type_impl(program, &func_type.parameter, true);
            let result_str = format_type_impl(program, &func_type.result, true);
            let formatted = format!("#{} -> {}", param_str, result_str);

            if nested {
                format!("({})", formatted)
            } else {
                formatted
            }
        }
        Type::Cycle(depth) => format!("μ{}", depth),
        Type::Union(types_list) => {
            if types_list.is_empty() {
                "never".to_string()
            } else {
                let mut type_strs: Vec<String> = types_list
                    .iter()
                    .map(|t| format_type_impl(program, t, true))
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
        Type::Variable(name) => name.to_string(),
    }
}

/// Format a binary value showing its actual content
fn format_binary(bytes: &[u8]) -> String {
    if bytes.len() <= 8 {
        // Show short binaries in full
        format!("'{}'", hex::encode(bytes))
    } else {
        // Show truncated for long binaries
        format!("'{}…'", hex::encode(&bytes[..8]))
    }
}

/// Standard implementation of BinaryLookup using heap and program
pub struct HeapAndProgramLookup<'a> {
    pub heap: &'a [Vec<u8>],
    pub program: &'a Program,
}

impl<'a> BinaryLookup for HeapAndProgramLookup<'a> {
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

pub fn format_value<L: BinaryLookup>(value: &Value, lookup: &L, program: &Program) -> String {
    match value {
        Value::Function(function, _) => format!("#{}", function),
        Value::Builtin(name) => format!("__{}__", name),
        Value::Integer(i) => i.to_string(),
        Value::Binary(binary) => {
            if let Some(bytes) = lookup.get_bytes(binary) {
                format_binary(bytes)
            } else {
                "<invalid binary>".to_string()
            }
        }
        Value::Process(process_id, _) => format!("@{}", process_id),
        Value::Tuple(type_id, elements) => {
            if let Some(type_info) = program.lookup_tuple(*type_id) {
                if type_info.name.as_deref() == Some("Str")
                    && let [Value::Binary(binary)] = elements.as_slice()
                    && let Some(bytes) = lookup.get_bytes(binary)
                    && let Some(formatted) = try_format_as_string(bytes)
                {
                    return formatted;
                }

                if elements.is_empty() {
                    type_info.name.as_deref().unwrap_or("[]").to_string()
                } else {
                    let formatted_elements = elements
                        .iter()
                        .enumerate()
                        .map(|(i, elem)| {
                            let formatted = format_value(elem, lookup, program);
                            match type_info.fields.get(i).and_then(|(name, _)| name.as_ref()) {
                                Some(name) => format!("{}: {}", name, formatted),
                                None => formatted,
                            }
                        })
                        .collect::<Vec<_>>()
                        .join(", ");

                    match type_info.name.as_deref() {
                        Some(n) => format!("{}[{}]", n, formatted_elements),
                        None => format!("[{}]", formatted_elements),
                    }
                }
            } else {
                let type_name = format!("Type{}", type_id);
                if elements.is_empty() {
                    return type_name;
                }

                let mut result = format!("{}[", type_name);
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&format_value(element, lookup, program));
                }
                result.push(']');
                result
            }
        }
    }
}
