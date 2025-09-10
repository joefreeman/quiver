//! Binary builtin function implementations

use crate::bytecode::Constant;
use crate::vm::{BinaryRef, Error, Value};

/// Helper function to get bytes from a BinaryRef using the constants array
fn get_binary_bytes_from_ref<'a>(
    binary_ref: &'a BinaryRef,
    constants: &'a [Constant],
) -> Result<&'a [u8], Error> {
    match binary_ref {
        BinaryRef::Constant(index) => match constants.get(*index) {
            Some(Constant::Binary(bytes)) => Ok(bytes),
            Some(_) => Err(Error::TypeMismatch {
                expected: "binary constant".to_string(),
                found: "non-binary constant".to_string(),
            }),
            None => Err(Error::ConstantUndefined(*index)),
        },
        BinaryRef::Heap(rc_bytes) => Ok(rc_bytes),
    }
}

/// Create a new zero-filled binary of the specified size
/// binary_new(size: int) -> bin
pub fn builtin_binary_new(arg: &Value, _constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Integer(size) => {
            if *size < 0 {
                return Err(Error::InvalidArgument(
                    "Size cannot be negative".to_string(),
                ));
            }
            let size = *size as usize;
            if size > crate::vm::MAX_BINARY_SIZE {
                return Err(Error::InvalidArgument(format!(
                    "Size {} exceeds maximum {}",
                    size,
                    crate::vm::MAX_BINARY_SIZE
                )));
            }

            let bytes = vec![0u8; size];
            let binary_ref = BinaryRef::Heap(std::rc::Rc::new(bytes));
            Ok(Value::Binary(binary_ref))
        }
        other => Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: other.type_name().to_string(),
        }),
    }
}

/// Get the length of a binary
/// binary_length(bin) -> int
pub fn builtin_binary_length(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Binary(binary_ref) => {
            let bytes = get_binary_bytes_from_ref(binary_ref, constants)?;
            Ok(Value::Integer(bytes.len() as i64))
        }
        other => Err(Error::TypeMismatch {
            expected: "binary".to_string(),
            found: other.type_name().to_string(),
        }),
    }
}

/// Get a byte at a specific index (returns 0-255)
/// binary_get_byte(bin, index: int) -> int
pub fn builtin_binary_get_byte(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 2 => match (&elements[0], &elements[1]) {
            (Value::Binary(binary_ref), Value::Integer(index)) => {
                let bytes = get_binary_bytes_from_ref(binary_ref, constants)?;
                if *index < 0 {
                    return Err(Error::InvalidArgument(
                        "Index cannot be negative".to_string(),
                    ));
                }
                let index = *index as usize;
                if index >= bytes.len() {
                    return Err(Error::InvalidArgument(format!(
                        "Index {} out of bounds for binary of length {}",
                        index,
                        bytes.len()
                    )));
                }
                Ok(Value::Integer(bytes[index] as i64))
            }
            _ => Err(Error::TypeMismatch {
                expected: "[binary, integer]".to_string(),
                found: "invalid tuple contents".to_string(),
            }),
        },
        _ => Err(Error::TypeMismatch {
            expected: "[binary, integer]".to_string(),
            found: "not a 2-element tuple".to_string(),
        }),
    }
}

/// Concatenate two binaries
/// binary_concat([bin, bin]) -> bin
pub fn builtin_binary_concat(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 2 => match (&elements[0], &elements[1]) {
            (Value::Binary(binary_ref_a), Value::Binary(binary_ref_b)) => {
                let bytes_a = get_binary_bytes_from_ref(binary_ref_a, constants)?;
                let bytes_b = get_binary_bytes_from_ref(binary_ref_b, constants)?;

                let total_len = bytes_a.len() + bytes_b.len();
                if total_len > crate::vm::MAX_BINARY_SIZE {
                    return Err(Error::InvalidArgument(format!(
                        "Combined size {} exceeds maximum {}",
                        total_len,
                        crate::vm::MAX_BINARY_SIZE
                    )));
                }

                let mut result = Vec::with_capacity(total_len);
                result.extend_from_slice(bytes_a);
                result.extend_from_slice(bytes_b);

                let binary_ref = BinaryRef::Heap(std::rc::Rc::new(result));
                Ok(Value::Binary(binary_ref))
            }
            _ => Err(Error::TypeMismatch {
                expected: "[binary, binary]".to_string(),
                found: "invalid tuple contents".to_string(),
            }),
        },
        _ => Err(Error::TypeMismatch {
            expected: "[binary, binary]".to_string(),
            found: "not a 2-element tuple".to_string(),
        }),
    }
}

// =============================================================================
// BITWISE OPERATIONS
// =============================================================================

/// Bitwise AND of two binaries
/// binary_and([bin, bin]) -> bin
pub fn builtin_binary_and(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 2 => {
            match (&elements[0], &elements[1]) {
                (Value::Binary(binary_ref_a), Value::Binary(binary_ref_b)) => {
                    let bytes_a = get_binary_bytes_from_ref(binary_ref_a, constants)?;
                    let bytes_b = get_binary_bytes_from_ref(binary_ref_b, constants)?;

                    // For bitwise operations, take the shorter length
                    let result_len = bytes_a.len().min(bytes_b.len());
                    let mut result = Vec::with_capacity(result_len);

                    for i in 0..result_len {
                        result.push(bytes_a[i] & bytes_b[i]);
                    }

                    let binary_ref = BinaryRef::Heap(std::rc::Rc::new(result));
                    Ok(Value::Binary(binary_ref))
                }
                _ => Err(Error::TypeMismatch {
                    expected: "[binary, binary]".to_string(),
                    found: "invalid tuple contents".to_string(),
                }),
            }
        }
        _ => Err(Error::TypeMismatch {
            expected: "[binary, binary]".to_string(),
            found: "not a 2-element tuple".to_string(),
        }),
    }
}

/// Bitwise OR of two binaries
/// binary_or([bin, bin]) -> bin
pub fn builtin_binary_or(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 2 => {
            match (&elements[0], &elements[1]) {
                (Value::Binary(binary_ref_a), Value::Binary(binary_ref_b)) => {
                    let bytes_a = get_binary_bytes_from_ref(binary_ref_a, constants)?;
                    let bytes_b = get_binary_bytes_from_ref(binary_ref_b, constants)?;

                    // For bitwise operations, take the longer length, padding with zeros
                    let result_len = bytes_a.len().max(bytes_b.len());
                    let mut result = Vec::with_capacity(result_len);

                    for i in 0..result_len {
                        let a = bytes_a.get(i).copied().unwrap_or(0);
                        let b = bytes_b.get(i).copied().unwrap_or(0);
                        result.push(a | b);
                    }

                    let binary_ref = BinaryRef::Heap(std::rc::Rc::new(result));
                    Ok(Value::Binary(binary_ref))
                }
                _ => Err(Error::TypeMismatch {
                    expected: "[binary, binary]".to_string(),
                    found: "invalid tuple contents".to_string(),
                }),
            }
        }
        _ => Err(Error::TypeMismatch {
            expected: "[binary, binary]".to_string(),
            found: "not a 2-element tuple".to_string(),
        }),
    }
}

/// Bitwise XOR of two binaries
/// binary_xor([bin, bin]) -> bin
pub fn builtin_binary_xor(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 2 => {
            match (&elements[0], &elements[1]) {
                (Value::Binary(binary_ref_a), Value::Binary(binary_ref_b)) => {
                    let bytes_a = get_binary_bytes_from_ref(binary_ref_a, constants)?;
                    let bytes_b = get_binary_bytes_from_ref(binary_ref_b, constants)?;

                    // For XOR, take the longer length, padding with zeros
                    let result_len = bytes_a.len().max(bytes_b.len());
                    let mut result = Vec::with_capacity(result_len);

                    for i in 0..result_len {
                        let a = bytes_a.get(i).copied().unwrap_or(0);
                        let b = bytes_b.get(i).copied().unwrap_or(0);
                        result.push(a ^ b);
                    }

                    let binary_ref = BinaryRef::Heap(std::rc::Rc::new(result));
                    Ok(Value::Binary(binary_ref))
                }
                _ => Err(Error::TypeMismatch {
                    expected: "[binary, binary]".to_string(),
                    found: "invalid tuple contents".to_string(),
                }),
            }
        }
        _ => Err(Error::TypeMismatch {
            expected: "[binary, binary]".to_string(),
            found: "not a 2-element tuple".to_string(),
        }),
    }
}

/// Bitwise NOT of a binary
/// binary_not(bin) -> bin
pub fn builtin_binary_not(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Binary(binary_ref) => {
            let bytes = get_binary_bytes_from_ref(binary_ref, constants)?;
            let result: Vec<u8> = bytes.iter().map(|&byte| !byte).collect();

            let binary_ref = BinaryRef::Heap(std::rc::Rc::new(result));
            Ok(Value::Binary(binary_ref))
        }
        other => Err(Error::TypeMismatch {
            expected: "binary".to_string(),
            found: other.type_name().to_string(),
        }),
    }
}

// =============================================================================
// SHIFT OPERATIONS
// =============================================================================

/// Left shift binary by n bits
/// binary_shift_left([bin, int]) -> bin
pub fn builtin_binary_shift_left(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 2 => {
            match (&elements[0], &elements[1]) {
                (Value::Binary(binary_ref), Value::Integer(shift_bits)) => {
                    let bytes = get_binary_bytes_from_ref(binary_ref, constants)?;

                    if *shift_bits < 0 {
                        return Err(Error::InvalidArgument(
                            "Shift amount cannot be negative".to_string(),
                        ));
                    }

                    let shift_bits = *shift_bits as u32;
                    if shift_bits >= (bytes.len() as u32 * 8) {
                        // Shift larger than total bits results in zeros
                        let result = vec![0u8; bytes.len()];
                        let binary_ref = BinaryRef::Heap(std::rc::Rc::new(result));
                        return Ok(Value::Binary(binary_ref));
                    }

                    let mut result = vec![0u8; bytes.len()];
                    let byte_shift = (shift_bits / 8) as usize;
                    let bit_shift = shift_bits % 8;

                    if bit_shift == 0 {
                        // Simple byte-aligned shift
                        for i in 0..bytes.len() {
                            if i + byte_shift < bytes.len() {
                                result[i] = bytes[i + byte_shift];
                            }
                        }
                    } else {
                        // Bit-level shift
                        let mut carry = 0u8;
                        for i in (0..bytes.len()).rev() {
                            if i + byte_shift < bytes.len() {
                                let src_byte = bytes[i + byte_shift];
                                result[i] = (src_byte << bit_shift) | carry;
                                carry = src_byte >> (8 - bit_shift);
                            }
                        }
                    }

                    let binary_ref = BinaryRef::Heap(std::rc::Rc::new(result));
                    Ok(Value::Binary(binary_ref))
                }
                _ => Err(Error::TypeMismatch {
                    expected: "[binary, integer]".to_string(),
                    found: "invalid tuple contents".to_string(),
                }),
            }
        }
        _ => Err(Error::TypeMismatch {
            expected: "[binary, integer]".to_string(),
            found: "not a 2-element tuple".to_string(),
        }),
    }
}

/// Right shift binary by n bits (logical shift)
/// binary_shift_right([bin, int]) -> bin
pub fn builtin_binary_shift_right(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 2 => {
            match (&elements[0], &elements[1]) {
                (Value::Binary(binary_ref), Value::Integer(shift_bits)) => {
                    let bytes = get_binary_bytes_from_ref(binary_ref, constants)?;

                    if *shift_bits < 0 {
                        return Err(Error::InvalidArgument(
                            "Shift amount cannot be negative".to_string(),
                        ));
                    }

                    let shift_bits = *shift_bits as u32;
                    if shift_bits >= (bytes.len() as u32 * 8) {
                        // Shift larger than total bits results in zeros
                        let result = vec![0u8; bytes.len()];
                        let binary_ref = BinaryRef::Heap(std::rc::Rc::new(result));
                        return Ok(Value::Binary(binary_ref));
                    }

                    let mut result = vec![0u8; bytes.len()];
                    let byte_shift = (shift_bits / 8) as usize;
                    let bit_shift = shift_bits % 8;

                    if bit_shift == 0 {
                        // Simple byte-aligned shift
                        for i in byte_shift..bytes.len() {
                            result[i] = bytes[i - byte_shift];
                        }
                    } else {
                        // Bit-level shift
                        let mut carry = 0u8;
                        for i in 0..bytes.len() {
                            if i >= byte_shift {
                                let src_byte = bytes[i - byte_shift];
                                result[i] = (src_byte >> bit_shift) | carry;
                                carry = src_byte << (8 - bit_shift);
                            }
                        }
                    }

                    let binary_ref = BinaryRef::Heap(std::rc::Rc::new(result));
                    Ok(Value::Binary(binary_ref))
                }
                _ => Err(Error::TypeMismatch {
                    expected: "[binary, integer]".to_string(),
                    found: "invalid tuple contents".to_string(),
                }),
            }
        }
        _ => Err(Error::TypeMismatch {
            expected: "[binary, integer]".to_string(),
            found: "not a 2-element tuple".to_string(),
        }),
    }
}
// =============================================================================
// BIT-LEVEL ACCESS OPERATIONS
// =============================================================================

/// Get bit at specific position (0 = rightmost bit)
/// binary_get_bit_pos([bin, int]) -> int
pub fn builtin_binary_get_bit_pos(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 2 => {
            match (&elements[0], &elements[1]) {
                (Value::Binary(binary_ref), Value::Integer(bit_index)) => {
                    let bytes = get_binary_bytes_from_ref(binary_ref, constants)?;

                    if *bit_index < 0 {
                        return Err(Error::InvalidArgument(
                            "Bit index cannot be negative".to_string(),
                        ));
                    }

                    let bit_index = *bit_index as usize;
                    let total_bits = bytes.len() * 8;

                    if bit_index >= total_bits {
                        return Err(Error::InvalidArgument(format!(
                            "Bit index {} out of bounds for binary with {} bits",
                            bit_index, total_bits
                        )));
                    }

                    // Calculate byte and bit position (big-endian bit numbering)
                    let byte_index = bit_index / 8;
                    let bit_position = bit_index % 8;
                    let byte_value = bytes[byte_index];
                    let bit_value = (byte_value >> (7 - bit_position)) & 1;

                    Ok(Value::Integer(bit_value as i64))
                }
                _ => Err(Error::TypeMismatch {
                    expected: "[binary, integer]".to_string(),
                    found: "invalid tuple contents".to_string(),
                }),
            }
        }
        _ => Err(Error::TypeMismatch {
            expected: "[binary, integer]".to_string(),
            found: "not a 2-element tuple".to_string(),
        }),
    }
}

/// Count number of set bits (popcount) - CRITICAL for HAMT
/// binary_popcount(bin) -> int
pub fn builtin_binary_popcount(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Binary(binary_ref) => {
            let bytes = get_binary_bytes_from_ref(binary_ref, constants)?;

            // Use efficient bit counting algorithm
            let mut count = 0u64;
            for &byte in bytes {
                count += byte.count_ones() as u64;
            }

            Ok(Value::Integer(count as i64))
        }
        other => Err(Error::TypeMismatch {
            expected: "binary".to_string(),
            found: other.type_name().to_string(),
        }),
    }
}

/// Set bit at specific position to value (0 or 1)
/// binary_set_bit([bin, int, int]) -> bin
pub fn builtin_binary_set_bit(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 3 => {
            match (&elements[0], &elements[1], &elements[2]) {
                (
                    Value::Binary(binary_ref),
                    Value::Integer(bit_index),
                    Value::Integer(bit_value),
                ) => {
                    let bytes = get_binary_bytes_from_ref(binary_ref, constants)?;

                    if *bit_index < 0 {
                        return Err(Error::InvalidArgument(
                            "Bit index cannot be negative".to_string(),
                        ));
                    }

                    if *bit_value != 0 && *bit_value != 1 {
                        return Err(Error::InvalidArgument(
                            "Bit value must be 0 or 1".to_string(),
                        ));
                    }

                    let bit_index = *bit_index as usize;
                    let total_bits = bytes.len() * 8;

                    if bit_index >= total_bits {
                        return Err(Error::InvalidArgument(format!(
                            "Bit index {} out of bounds for binary with {} bits",
                            bit_index, total_bits
                        )));
                    }

                    let mut result = bytes.to_vec();
                    let byte_index = bit_index / 8;
                    let bit_position = bit_index % 8;

                    if *bit_value == 1 {
                        result[byte_index] |= 1 << (7 - bit_position);
                    } else {
                        result[byte_index] &= !(1 << (7 - bit_position));
                    }

                    let binary_ref = BinaryRef::Heap(std::rc::Rc::new(result));
                    Ok(Value::Binary(binary_ref))
                }
                _ => Err(Error::TypeMismatch {
                    expected: "[binary, integer, integer]".to_string(),
                    found: "invalid tuple contents".to_string(),
                }),
            }
        }
        _ => Err(Error::TypeMismatch {
            expected: "[binary, integer, integer]".to_string(),
            found: "not a 3-element tuple".to_string(),
        }),
    }
}

// =============================================================================
// MULTI-BYTE ACCESS OPERATIONS
// =============================================================================

/// Get a 32-bit unsigned integer from binary at specific byte offset (big-endian)
/// binary_get_u32([bin, int]) -> int
pub fn builtin_binary_get_u32(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 2 => match (&elements[0], &elements[1]) {
            (Value::Binary(binary_ref), Value::Integer(offset)) => {
                let bytes = get_binary_bytes_from_ref(binary_ref, constants)?;

                if *offset < 0 {
                    return Err(Error::InvalidArgument(
                        "Offset cannot be negative".to_string(),
                    ));
                }

                let offset = *offset as usize;
                if offset + 4 > bytes.len() {
                    return Err(Error::InvalidArgument(
                        "Not enough bytes for u32".to_string(),
                    ));
                }

                let value = u32::from_be_bytes([
                    bytes[offset],
                    bytes[offset + 1],
                    bytes[offset + 2],
                    bytes[offset + 3],
                ]);

                Ok(Value::Integer(value as i64))
            }
            _ => Err(Error::TypeMismatch {
                expected: "[binary, integer]".to_string(),
                found: "invalid tuple contents".to_string(),
            }),
        },
        _ => Err(Error::TypeMismatch {
            expected: "[binary, integer]".to_string(),
            found: "not a 2-element tuple".to_string(),
        }),
    }
}

/// Set a 32-bit unsigned integer in binary at specific byte offset (big-endian)
/// binary_set_u32([bin, int, int]) -> bin
pub fn builtin_binary_set_u32(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 3 => {
            match (&elements[0], &elements[1], &elements[2]) {
                (Value::Binary(binary_ref), Value::Integer(offset), Value::Integer(value)) => {
                    let bytes = get_binary_bytes_from_ref(binary_ref, constants)?;

                    if *offset < 0 {
                        return Err(Error::InvalidArgument(
                            "Offset cannot be negative".to_string(),
                        ));
                    }

                    if *value < 0 || *value > u32::MAX as i64 {
                        return Err(Error::InvalidArgument(
                            "Value must be a valid u32".to_string(),
                        ));
                    }

                    let offset = *offset as usize;
                    if offset + 4 > bytes.len() {
                        return Err(Error::InvalidArgument(
                            "Not enough bytes for u32".to_string(),
                        ));
                    }

                    let mut result = bytes.to_vec();
                    let value_bytes = (*value as u32).to_be_bytes();

                    result[offset] = value_bytes[0];
                    result[offset + 1] = value_bytes[1];
                    result[offset + 2] = value_bytes[2];
                    result[offset + 3] = value_bytes[3];

                    let binary_ref = BinaryRef::Heap(std::rc::Rc::new(result));
                    Ok(Value::Binary(binary_ref))
                }
                _ => Err(Error::TypeMismatch {
                    expected: "[binary, integer, integer]".to_string(),
                    found: "invalid tuple contents".to_string(),
                }),
            }
        }
        _ => Err(Error::TypeMismatch {
            expected: "[binary, integer, integer]".to_string(),
            found: "not a 3-element tuple".to_string(),
        }),
    }
}

/// Get a 64-bit unsigned integer from binary at specific byte offset (big-endian)
/// binary_get_u64([bin, int]) -> int
pub fn builtin_binary_get_u64(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 2 => {
            match (&elements[0], &elements[1]) {
                (Value::Binary(binary_ref), Value::Integer(offset)) => {
                    let bytes = get_binary_bytes_from_ref(binary_ref, constants)?;

                    if *offset < 0 {
                        return Err(Error::InvalidArgument(
                            "Offset cannot be negative".to_string(),
                        ));
                    }

                    let offset = *offset as usize;
                    if offset + 8 > bytes.len() {
                        return Err(Error::InvalidArgument(
                            "Not enough bytes for u64".to_string(),
                        ));
                    }

                    let value = u64::from_be_bytes([
                        bytes[offset],
                        bytes[offset + 1],
                        bytes[offset + 2],
                        bytes[offset + 3],
                        bytes[offset + 4],
                        bytes[offset + 5],
                        bytes[offset + 6],
                        bytes[offset + 7],
                    ]);

                    // Note: This may overflow i64 but Quiver only has i64 integers
                    Ok(Value::Integer(value as i64))
                }
                _ => Err(Error::TypeMismatch {
                    expected: "[binary, integer]".to_string(),
                    found: "invalid tuple contents".to_string(),
                }),
            }
        }
        _ => Err(Error::TypeMismatch {
            expected: "[binary, integer]".to_string(),
            found: "not a 2-element tuple".to_string(),
        }),
    }
}

/// Set a 64-bit unsigned integer in binary at specific byte offset (big-endian)
/// binary_set_u64([bin, int, int]) -> bin
pub fn builtin_binary_set_u64(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 3 => {
            match (&elements[0], &elements[1], &elements[2]) {
                (Value::Binary(binary_ref), Value::Integer(offset), Value::Integer(value)) => {
                    let bytes = get_binary_bytes_from_ref(binary_ref, constants)?;

                    if *offset < 0 {
                        return Err(Error::InvalidArgument(
                            "Offset cannot be negative".to_string(),
                        ));
                    }

                    let offset = *offset as usize;
                    if offset + 8 > bytes.len() {
                        return Err(Error::InvalidArgument(
                            "Not enough bytes for u64".to_string(),
                        ));
                    }

                    let mut result = bytes.to_vec();
                    let value_bytes = (*value as u64).to_be_bytes();

                    for i in 0..8 {
                        result[offset + i] = value_bytes[i];
                    }

                    let binary_ref = BinaryRef::Heap(std::rc::Rc::new(result));
                    Ok(Value::Binary(binary_ref))
                }
                _ => Err(Error::TypeMismatch {
                    expected: "[binary, integer, integer]".to_string(),
                    found: "invalid tuple contents".to_string(),
                }),
            }
        }
        _ => Err(Error::TypeMismatch {
            expected: "[binary, integer, integer]".to_string(),
            found: "not a 3-element tuple".to_string(),
        }),
    }
}

// =============================================================================
// BINARY SLICING OPERATIONS
// =============================================================================

/// Extract a slice from binary [start, end) (end is exclusive)
/// binary_slice([bin, int, int]) -> bin
pub fn builtin_binary_slice(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 3 => {
            match (&elements[0], &elements[1], &elements[2]) {
                (Value::Binary(binary_ref), Value::Integer(start), Value::Integer(end)) => {
                    let bytes = get_binary_bytes_from_ref(binary_ref, constants)?;

                    if *start < 0 || *end < 0 {
                        return Err(Error::InvalidArgument(
                            "Indices cannot be negative".to_string(),
                        ));
                    }

                    let start = *start as usize;
                    let end = *end as usize;

                    if start > bytes.len() || end > bytes.len() {
                        return Err(Error::InvalidArgument("Index out of bounds".to_string()));
                    }

                    if start > end {
                        return Err(Error::InvalidArgument(
                            "Start index must be <= end index".to_string(),
                        ));
                    }

                    let slice = bytes[start..end].to_vec();
                    let binary_ref = BinaryRef::Heap(std::rc::Rc::new(slice));
                    Ok(Value::Binary(binary_ref))
                }
                _ => Err(Error::TypeMismatch {
                    expected: "[binary, integer, integer]".to_string(),
                    found: "invalid tuple contents".to_string(),
                }),
            }
        }
        _ => Err(Error::TypeMismatch {
            expected: "[binary, integer, integer]".to_string(),
            found: "not a 3-element tuple".to_string(),
        }),
    }
}

/// Take first N bytes from binary
/// binary_take([bin, int]) -> bin
pub fn builtin_binary_take(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 2 => match (&elements[0], &elements[1]) {
            (Value::Binary(binary_ref), Value::Integer(count)) => {
                let bytes = get_binary_bytes_from_ref(binary_ref, constants)?;

                if *count < 0 {
                    return Err(Error::InvalidArgument(
                        "Count cannot be negative".to_string(),
                    ));
                }

                let count = (*count as usize).min(bytes.len());
                let slice = bytes[..count].to_vec();

                let binary_ref = BinaryRef::Heap(std::rc::Rc::new(slice));
                Ok(Value::Binary(binary_ref))
            }
            _ => Err(Error::TypeMismatch {
                expected: "[binary, integer]".to_string(),
                found: "invalid tuple contents".to_string(),
            }),
        },
        _ => Err(Error::TypeMismatch {
            expected: "[binary, integer]".to_string(),
            found: "not a 2-element tuple".to_string(),
        }),
    }
}

/// Drop first N bytes from binary
/// binary_drop([bin, int]) -> bin
pub fn builtin_binary_drop(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 2 => match (&elements[0], &elements[1]) {
            (Value::Binary(binary_ref), Value::Integer(count)) => {
                let bytes = get_binary_bytes_from_ref(binary_ref, constants)?;

                if *count < 0 {
                    return Err(Error::InvalidArgument(
                        "Count cannot be negative".to_string(),
                    ));
                }

                let count = (*count as usize).min(bytes.len());
                let slice = bytes[count..].to_vec();

                let binary_ref = BinaryRef::Heap(std::rc::Rc::new(slice));
                Ok(Value::Binary(binary_ref))
            }
            _ => Err(Error::TypeMismatch {
                expected: "[binary, integer]".to_string(),
                found: "invalid tuple contents".to_string(),
            }),
        },
        _ => Err(Error::TypeMismatch {
            expected: "[binary, integer]".to_string(),
            found: "not a 2-element tuple".to_string(),
        }),
    }
}

/// Pad binary to specified length with zeros at the end
/// binary_pad([bin, int]) -> bin
pub fn builtin_binary_pad(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 2 => {
            match (&elements[0], &elements[1]) {
                (Value::Binary(binary_ref), Value::Integer(target_length)) => {
                    let bytes = get_binary_bytes_from_ref(binary_ref, constants)?;

                    if *target_length < 0 {
                        return Err(Error::InvalidArgument(
                            "Target length cannot be negative".to_string(),
                        ));
                    }

                    let target_length = *target_length as usize;

                    if target_length <= bytes.len() {
                        // No padding needed, return copy of original
                        let binary_ref = BinaryRef::Heap(std::rc::Rc::new(bytes.to_vec()));
                        return Ok(Value::Binary(binary_ref));
                    }

                    let mut result = bytes.to_vec();
                    result.resize(target_length, 0);

                    let binary_ref = BinaryRef::Heap(std::rc::Rc::new(result));
                    Ok(Value::Binary(binary_ref))
                }
                _ => Err(Error::TypeMismatch {
                    expected: "[binary, integer]".to_string(),
                    found: "invalid tuple contents".to_string(),
                }),
            }
        }
        _ => Err(Error::TypeMismatch {
            expected: "[binary, integer]".to_string(),
            found: "not a 2-element tuple".to_string(),
        }),
    }
}

// =============================================================================
// HASHING OPERATIONS
// =============================================================================

/// Simple FNV-1a hash implementation for 32-bit hashes
/// binary_hash32(bin) -> int
pub fn builtin_binary_hash32(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Binary(binary_ref) => {
            let bytes = get_binary_bytes_from_ref(binary_ref, constants)?;

            // FNV-1a 32-bit hash
            let mut hash: u32 = 2166136261; // FNV offset basis
            for &byte in bytes {
                hash ^= byte as u32;
                hash = hash.wrapping_mul(16777619); // FNV prime
            }

            Ok(Value::Integer(hash as i64))
        }
        _ => Err(Error::TypeMismatch {
            expected: "binary".to_string(),
            found: "not binary".to_string(),
        }),
    }
}

/// Simple FNV-1a hash implementation for 64-bit hashes
/// binary_hash64(bin) -> int
pub fn builtin_binary_hash64(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Binary(binary_ref) => {
            let bytes = get_binary_bytes_from_ref(binary_ref, constants)?;

            // FNV-1a 64-bit hash
            let mut hash: u64 = 14695981039346656037; // FNV offset basis
            for &byte in bytes {
                hash ^= byte as u64;
                hash = hash.wrapping_mul(1099511628211); // FNV prime
            }

            // Note: This may not fit in i64 but we cast it anyway
            Ok(Value::Integer(hash as i64))
        }
        _ => Err(Error::TypeMismatch {
            expected: "binary".to_string(),
            found: "not binary".to_string(),
        }),
    }
}

/// Hash a string (converted to binary) for use in data structures
/// string_hash(bin) -> int
pub fn builtin_string_hash(arg: &Value, constants: &[Constant]) -> Result<Value, Error> {
    // For strings, we use the same implementation as hash32 but with a different name
    // This is common in many languages to have separate string vs binary hash functions
    builtin_binary_hash32(arg, constants)
}

/// Extract N-bit chunks from hash for HAMT navigation
/// hash_chunk([int, int, int]) -> int (hash, shift_bits, chunk_size_bits)
pub fn builtin_hash_chunk(arg: &Value, _constants: &[Constant]) -> Result<Value, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 3 => {
            match (&elements[0], &elements[1], &elements[2]) {
                (Value::Integer(hash), Value::Integer(shift_bits), Value::Integer(chunk_bits)) => {
                    if *shift_bits < 0 || *chunk_bits < 0 || *chunk_bits > 6 {
                        return Err(Error::InvalidArgument("Invalid bit parameters".to_string()));
                    }

                    let hash = *hash as u64;
                    let shift = *shift_bits as u32;
                    let chunk_size = *chunk_bits as u32;

                    if shift >= 64 {
                        return Ok(Value::Integer(0));
                    }

                    // Create mask for the chunk size (e.g., 5 bits = 0x1F)
                    let mask = (1u64 << chunk_size) - 1;

                    // Extract the chunk
                    let chunk = (hash >> shift) & mask;

                    Ok(Value::Integer(chunk as i64))
                }
                _ => Err(Error::TypeMismatch {
                    expected: "[integer, integer, integer]".to_string(),
                    found: "invalid tuple contents".to_string(),
                }),
            }
        }
        _ => Err(Error::TypeMismatch {
            expected: "[integer, integer, integer]".to_string(),
            found: "not a 3-element tuple".to_string(),
        }),
    }
}
