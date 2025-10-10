use quiver_core::bytecode::Constant;
use quiver_core::error::Error;
use quiver_core::types::{Type, TypeLookup};
use quiver_core::value::{Binary, Value};

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

pub fn format_type(type_lookup: &impl TypeLookup, type_def: &Type) -> String {
    match type_def {
        Type::Integer => "int".to_string(),
        Type::Binary => "bin".to_string(),
        Type::Process(process_type) => match (&process_type.receive, &process_type.returns) {
            (Some(receive), Some(returns)) => {
                format!(
                    "(@{} -> {})",
                    format_type(type_lookup, receive),
                    format_type(type_lookup, returns)
                )
            }
            (Some(receive), None) => {
                format!("@{}", format_type(type_lookup, receive))
            }
            (None, Some(returns)) => {
                format!("(@-> {})", format_type(type_lookup, returns))
            }
            (None, None) => "@".to_string(),
        },
        Type::Tuple(type_id) => {
            if let Some((name, fields)) = type_lookup.lookup_type(type_id) {
                let field_strs: Vec<String> = fields
                    .iter()
                    .map(|(field_name, field_type)| {
                        if let Some(field_name) = field_name {
                            format!("{}: {}", field_name, format_type(type_lookup, field_type))
                        } else {
                            format_type(type_lookup, field_type)
                        }
                    })
                    .collect();

                if let Some(type_name) = name {
                    if field_strs.is_empty() {
                        format!("{}", type_name)
                    } else {
                        format!("{}[{}]", type_name, field_strs.join(", "))
                    }
                } else {
                    format!("[{}]", field_strs.join(", "))
                }
            } else {
                format!("Type{}", type_id.0)
            }
        }
        Type::Callable(func_type) => {
            // Add parentheses around parameter if it's a function type
            let param_str = match &func_type.parameter {
                Type::Callable(_) => {
                    format!("({})", format_type(type_lookup, &func_type.parameter))
                }
                _ => format_type(type_lookup, &func_type.parameter),
            };

            // Result type already has parentheses if it's a union
            let result_str = format_type(type_lookup, &func_type.result);
            format!("#{} -> {}", param_str, result_str)
        }
        Type::Cycle(depth) => format!("μ{}", depth),
        Type::Union(types_list) => {
            if types_list.is_empty() {
                "never".to_string()
            } else {
                let type_strs: Vec<String> = types_list
                    .iter()
                    .map(|t| {
                        match t {
                            // Add parentheses around function types in unions for clarity
                            Type::Callable(_) => format!("({})", format_type(type_lookup, t)),
                            _ => format_type(type_lookup, t),
                        }
                    })
                    .collect();
                format!("({})", type_strs.join(" | "))
            }
        }
    }
}

/// Get binary bytes from either a constant or heap
fn get_binary_bytes(
    binary: &Binary,
    heap: &[Vec<u8>],
    constants: &[Constant],
) -> Result<Vec<u8>, Error> {
    match binary {
        Binary::Constant(index) => {
            let constant = constants
                .get(*index)
                .ok_or(Error::ConstantUndefined(*index))?;
            match constant {
                Constant::Binary(bytes) => Ok(bytes.clone()),
                _ => Err(Error::TypeMismatch {
                    expected: "binary".to_string(),
                    found: "integer".to_string(),
                }),
            }
        }
        Binary::Heap(index) => heap.get(*index).cloned().ok_or_else(|| {
            Error::InvalidArgument(format!("Heap binary index {} not found", index))
        }),
    }
}

/// Format a binary value showing its actual content
pub fn format_binary(binary: &Binary, heap: &[Vec<u8>], constants: &[Constant]) -> String {
    match get_binary_bytes(binary, heap, constants) {
        Ok(bytes) => {
            if bytes.len() <= 8 {
                // Show short binaries in full
                format!("'{}'", hex::encode(bytes))
            } else {
                // Show truncated for long binaries
                format!("'{}…'", hex::encode(&bytes[..8]))
            }
        }
        Err(_) => "<invalid binary>".to_string(),
    }
}

pub fn format_value(
    value: &Value,
    heap: &[Vec<u8>],
    constants: &[Constant],
    type_lookup: &impl TypeLookup,
) -> String {
    match value {
        Value::Function(function, _) => format!("#{}", function),
        Value::Builtin(name) => format!("<{}>", name),
        Value::Integer(i) => i.to_string(),
        Value::Binary(binary) => format_binary(binary, heap, constants),
        Value::Pid(process_id) => format!("@{}", process_id.0),
        Value::Tuple(type_id, elements) => {
            if let Some((name, fields)) = type_lookup.lookup_type(type_id) {
                if name.as_deref() == Some("Str") {
                    if let [Value::Binary(binary)] = elements.as_slice() {
                        if let Ok(bytes) = get_binary_bytes(binary, heap, constants) {
                            if let Some(formatted) = try_format_as_string(&bytes) {
                                return formatted;
                            }
                        }
                    }
                }

                if elements.is_empty() {
                    name.as_deref().unwrap_or("[]").to_string()
                } else {
                    let formatted_elements = elements
                        .iter()
                        .enumerate()
                        .map(|(i, elem)| {
                            let formatted = format_value(elem, heap, constants, type_lookup);
                            match fields.get(i).and_then(|(name, _)| name.as_ref()) {
                                Some(name) => format!("{}: {}", name, formatted),
                                None => formatted,
                            }
                        })
                        .collect::<Vec<_>>()
                        .join(", ");

                    match name.as_deref() {
                        Some(n) => format!("{}[{}]", n, formatted_elements),
                        None => format!("[{}]", formatted_elements),
                    }
                }
            } else {
                let type_name = format!("Type{}", type_id.0);
                if elements.is_empty() {
                    return type_name;
                }

                let mut result = format!("{}[", type_name);
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&format_value(element, heap, constants, type_lookup));
                }
                result.push(']');
                result
            }
        }
    }
}
