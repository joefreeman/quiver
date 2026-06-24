//! Binary builtin function implementations
use crate::binary::BinaryData;
use crate::builtins::{BuiltinResult, bigint_to_i64, bigint_to_u8, bigint_to_usize};
use crate::effects::Effect;
use crate::error::Error;
use crate::executor::Executor;
use crate::process::ProcessId;
use crate::value::Value;
use num_bigint::BigInt;
use std::rc::Rc;

/// Repeat a binary `count` times, sharing the unit lazily (O(1), no materialization).
/// binary_repeat([bin, count]) -> bin
pub fn builtin_binary_repeat<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 2 => match (&elements[0], &elements[1]) {
            (Value::Binary(binary), Value::Integer(count)) => {
                if count.sign() == num_bigint::Sign::Minus {
                    return Err(Error::InvalidArgument(
                        "Repeat count cannot be negative".to_string(),
                    ));
                }
                let count = bigint_to_usize(count)?;
                let unit = executor.get_binary_data(binary)?.clone();
                let tiled = BinaryData::tiled(Rc::new(unit), count);
                // allocate_binary_data enforces MAX_BINARY_SIZE against the realized length.
                let binary = executor.allocate_binary_data(tiled)?;
                Ok(BuiltinResult::Value(Value::Binary(binary)))
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

/// Create a new zero-filled binary of the specified size
/// binary_new(size: int) -> bin
pub fn builtin_binary_new<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Integer(size) => {
            if size.sign() == num_bigint::Sign::Minus {
                return Err(Error::InvalidArgument(
                    "Size cannot be negative".to_string(),
                ));
            }

            let size = bigint_to_usize(size)?;
            if size > crate::value::MAX_BINARY_SIZE {
                return Err(Error::InvalidArgument(format!(
                    "Size {} exceeds maximum {}",
                    size,
                    crate::value::MAX_BINARY_SIZE
                )));
            }

            // Use BinaryData::zeroed() for efficient zero-filled binary (no allocation)
            let binary = executor.allocate_binary_data(BinaryData::zeroed(size))?;
            Ok(BuiltinResult::Value(Value::Binary(binary)))
        }
        other => Err(Error::TypeMismatch {
            expected: "integer".to_string(),
            found: other.type_name().to_string(),
        }),
    }
}

/// Get the length of a binary
/// binary_length(bin) -> int
pub fn builtin_binary_length<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Binary(binary) => {
            let binary_data = executor.get_binary_data(binary)?;
            Ok(BuiltinResult::Value(Value::Integer(BigInt::from(
                binary_data.len(),
            ))))
        }
        other => Err(Error::TypeMismatch {
            expected: "binary".to_string(),
            found: other.type_name().to_string(),
        }),
    }
}

/// Concatenate two binaries
/// binary_concat([bin, bin]) -> bin
pub fn builtin_binary_concat<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 2 => match (&elements[0], &elements[1]) {
            (Value::Binary(binary_a), Value::Binary(binary_b)) => {
                let binary_data_a = executor.get_binary_data(binary_a)?;
                let binary_data_b = executor.get_binary_data(binary_b)?;

                let total_len = binary_data_a.len() + binary_data_b.len();
                if total_len > crate::value::MAX_BINARY_SIZE {
                    return Err(Error::InvalidArgument(format!(
                        "Combined size {} exceeds maximum {}",
                        total_len,
                        crate::value::MAX_BINARY_SIZE
                    )));
                }

                // O(1) concatenation through structural sharing
                // Clone the BinaryData and wrap in Rc for concat
                let concat = BinaryData::concat(
                    Rc::new(binary_data_a.clone()),
                    Rc::new(binary_data_b.clone()),
                );
                let binary = executor.allocate_binary_data(concat)?;
                Ok(BuiltinResult::Value(Value::Binary(binary)))
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
pub fn builtin_binary_and<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 2 => {
            match (&elements[0], &elements[1]) {
                (Value::Binary(binary_a), Value::Binary(binary_b)) => {
                    let binary_data_a = executor.get_binary_data(binary_a)?;
                    let binary_data_b = executor.get_binary_data(binary_b)?;

                    // For bitwise operations, take the shorter length
                    // Use iterators to avoid materializing both binaries
                    let result: Vec<u8> = binary_data_a
                        .iter()
                        .zip(binary_data_b.iter())
                        .map(|(a, b)| a & b)
                        .collect();

                    let binary = executor.allocate_binary(result)?;
                    Ok(BuiltinResult::Value(Value::Binary(binary)))
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
pub fn builtin_binary_or<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 2 => {
            match (&elements[0], &elements[1]) {
                (Value::Binary(binary_a), Value::Binary(binary_b)) => {
                    let binary_data_a = executor.get_binary_data(binary_a)?;
                    let binary_data_b = executor.get_binary_data(binary_b)?;

                    // For bitwise operations, take the longer length, padding with zeros
                    let len_a = binary_data_a.len();
                    let len_b = binary_data_b.len();
                    let result_len = len_a.max(len_b);

                    let mut result = Vec::with_capacity(result_len);
                    for i in 0..result_len {
                        let a = if i < len_a {
                            binary_data_a.byte_at(i).unwrap()
                        } else {
                            0
                        };
                        let b = if i < len_b {
                            binary_data_b.byte_at(i).unwrap()
                        } else {
                            0
                        };
                        result.push(a | b);
                    }

                    let binary = executor.allocate_binary(result)?;
                    Ok(BuiltinResult::Value(Value::Binary(binary)))
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
pub fn builtin_binary_xor<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 2 => {
            match (&elements[0], &elements[1]) {
                (Value::Binary(binary_a), Value::Binary(binary_b)) => {
                    let binary_data_a = executor.get_binary_data(binary_a)?;
                    let binary_data_b = executor.get_binary_data(binary_b)?;

                    // For XOR, take the longer length, padding with zeros
                    let len_a = binary_data_a.len();
                    let len_b = binary_data_b.len();
                    let result_len = len_a.max(len_b);

                    let mut result = Vec::with_capacity(result_len);
                    for i in 0..result_len {
                        let a = if i < len_a {
                            binary_data_a.byte_at(i).unwrap()
                        } else {
                            0
                        };
                        let b = if i < len_b {
                            binary_data_b.byte_at(i).unwrap()
                        } else {
                            0
                        };
                        result.push(a ^ b);
                    }

                    let binary = executor.allocate_binary(result)?;
                    Ok(BuiltinResult::Value(Value::Binary(binary)))
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

/// binary_index([bin, byte, offset]) -> int | []
/// Index of the first occurrence of `byte` (0..=255) at or after `offset`, or nil if absent.
pub fn builtin_binary_index<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 3 => {
            match (&elements[0], &elements[1], &elements[2]) {
                (Value::Binary(binary), Value::Integer(byte), Value::Integer(offset)) => {
                    let byte = bigint_to_u8(byte).map_err(|_| {
                        Error::InvalidArgument("Byte must be in the range 0..=255".to_string())
                    })?;
                    if offset.sign() == num_bigint::Sign::Minus {
                        return Err(Error::InvalidArgument(
                            "Offset cannot be negative".to_string(),
                        ));
                    }
                    let offset = bigint_to_usize(offset)?;
                    let binary_data = executor.get_binary_data(binary)?;
                    let result = binary_data.find_byte(byte, offset);
                    Ok(BuiltinResult::Value(match result {
                        Some(index) => Value::Integer(BigInt::from(index)),
                        None => Value::nil(),
                    }))
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

/// Bitwise NOT of a binary
/// binary_not(bin) -> bin
pub fn builtin_binary_not<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Binary(binary) => {
            let binary_data = executor.get_binary_data(binary)?;
            // Use iterator to avoid double materialization
            let result: Vec<u8> = binary_data.iter().map(|byte| !byte).collect();

            let binary = executor.allocate_binary(result)?;
            Ok(BuiltinResult::Value(Value::Binary(binary)))
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

/// Shift binary by n bits (positive = left, negative = right, logical shift)
/// binary_shift([bin, int]) -> bin
pub fn builtin_binary_shift<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 2 => {
            match (&elements[0], &elements[1]) {
                (Value::Binary(binary), Value::Integer(shift_amount)) => {
                    let shift_amount = bigint_to_i64(shift_amount)?;
                    if shift_amount == 0 {
                        // No shift needed
                        return Ok(BuiltinResult::Value(Value::Binary(*binary)));
                    }

                    let binary_data = executor.get_binary_data(binary)?;
                    let bytes = binary_data.to_vec();

                    let shift_left = shift_amount > 0;
                    let shift_bits = shift_amount.unsigned_abs() as u32;

                    if shift_bits >= (bytes.len() as u32 * 8) {
                        // Shift larger than total bits results in zeros
                        let result = vec![0u8; bytes.len()];
                        let binary = executor.allocate_binary(result)?;
                        return Ok(BuiltinResult::Value(Value::Binary(binary)));
                    }

                    let mut result = vec![0u8; bytes.len()];
                    let byte_shift = (shift_bits / 8) as usize;
                    let bit_shift = shift_bits % 8;

                    if shift_left {
                        // Left shift
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
                    } else {
                        // Right shift
                        if bit_shift == 0 {
                            // Simple byte-aligned shift
                            result[byte_shift..bytes.len()]
                                .copy_from_slice(&bytes[..(bytes.len() - byte_shift)]);
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
                    }

                    let binary = executor.allocate_binary(result)?;
                    Ok(BuiltinResult::Value(Value::Binary(binary)))
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
// BIT/BYTE-LEVEL ACCESS OPERATIONS
// =============================================================================

/// Count number of set bits (popcount) - CRITICAL for HAMT
/// binary_popcount(bin) -> int
pub fn builtin_binary_popcount<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Binary(binary) => {
            let binary_data = executor.get_binary_data(binary)?;

            // Use iterator to avoid materializing the entire binary
            let count: u64 = binary_data
                .iter()
                .map(|byte| byte.count_ones() as u64)
                .sum();

            Ok(BuiltinResult::Value(Value::Integer(BigInt::from(count))))
        }
        other => Err(Error::TypeMismatch {
            expected: "binary".to_string(),
            found: other.type_name().to_string(),
        }),
    }
}

/// Get an integer from binary at specific position (big-endian)
/// binary_get([bin, byte_offset, bit_offset, num_bits]) -> int
/// byte_offset: which byte to start at (0-indexed)
/// bit_offset: which bit within that byte (0-7, 0 is MSB)
/// num_bits: how many bits to read (1-64)
pub fn builtin_binary_get<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 4 => {
            match (&elements[0], &elements[1], &elements[2], &elements[3]) {
                (
                    Value::Binary(binary),
                    Value::Integer(byte_offset),
                    Value::Integer(bit_offset),
                    Value::Integer(num_bits),
                ) => {
                    let binary_data = executor.get_binary_data(binary)?;

                    let byte_offset = bigint_to_i64(byte_offset)?;
                    let bit_offset = bigint_to_i64(bit_offset)?;
                    let num_bits = bigint_to_i64(num_bits)?;

                    if byte_offset < 0 {
                        return Err(Error::InvalidArgument(
                            "Byte offset cannot be negative".to_string(),
                        ));
                    }

                    if !(0..=7).contains(&bit_offset) {
                        return Err(Error::InvalidArgument("Bit offset must be 0-7".to_string()));
                    }

                    if !(1..=64).contains(&num_bits) {
                        return Err(Error::InvalidArgument(
                            "Number of bits must be between 1 and 64".to_string(),
                        ));
                    }

                    let byte_offset = byte_offset as usize;
                    let bit_offset = bit_offset as usize;
                    let num_bits = num_bits as usize;

                    // Calculate which bytes we need to read
                    let total_bit_start = byte_offset * 8 + bit_offset;
                    let total_bit_end = total_bit_start + num_bits;
                    let last_byte_needed = total_bit_end.div_ceil(8);

                    if last_byte_needed > binary_data.len() {
                        return Err(Error::InvalidArgument(format!(
                            "Not enough bits: need {} bits starting at byte {} bit {}",
                            num_bits, byte_offset, bit_offset
                        )));
                    }

                    // Read all bytes we need
                    let mut value = 0u64;
                    let bytes_to_read = last_byte_needed - byte_offset;

                    for i in 0..bytes_to_read {
                        value =
                            (value << 8) | (binary_data.byte_at(byte_offset + i).unwrap() as u64);
                    }

                    // Shift to align our bits to the right
                    let bits_read = bytes_to_read * 8;
                    let bits_after = bits_read - bit_offset - num_bits;

                    value >>= bits_after;

                    // Mask to keep only the bits we want
                    let mask = if num_bits == 64 {
                        u64::MAX
                    } else {
                        (1u64 << num_bits) - 1
                    };
                    value &= mask;

                    Ok(BuiltinResult::Value(Value::Integer(BigInt::from(value))))
                }
                _ => Err(Error::TypeMismatch {
                    expected: "[binary, integer, integer, integer]".to_string(),
                    found: "invalid tuple contents".to_string(),
                }),
            }
        }
        _ => Err(Error::TypeMismatch {
            expected: "[binary, integer, integer, integer]".to_string(),
            found: "not a 4-element tuple".to_string(),
        }),
    }
}

/// Set an integer in binary at specific position (big-endian)
/// binary_set([bin, byte_offset, bit_offset, value, num_bits]) -> bin
/// byte_offset: which byte to start at (0-indexed)
/// bit_offset: which bit within that byte (0-7, 0 is MSB)
/// num_bits: how many bits to write (1-64)
pub fn builtin_binary_set<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 5 => {
            match (
                &elements[0],
                &elements[1],
                &elements[2],
                &elements[3],
                &elements[4],
            ) {
                (
                    Value::Binary(binary),
                    Value::Integer(byte_offset),
                    Value::Integer(bit_offset),
                    Value::Integer(value),
                    Value::Integer(num_bits),
                ) => {
                    let binary_data = executor.get_binary_data(binary)?;

                    let byte_offset = bigint_to_i64(byte_offset)?;
                    let bit_offset = bigint_to_i64(bit_offset)?;
                    let num_bits = bigint_to_i64(num_bits)?;

                    if byte_offset < 0 {
                        return Err(Error::InvalidArgument(
                            "Byte offset cannot be negative".to_string(),
                        ));
                    }

                    if !(0..=7).contains(&bit_offset) {
                        return Err(Error::InvalidArgument("Bit offset must be 0-7".to_string()));
                    }

                    if !(1..=64).contains(&num_bits) {
                        return Err(Error::InvalidArgument(
                            "Number of bits must be between 1 and 64".to_string(),
                        ));
                    }

                    let byte_offset = byte_offset as usize;
                    let bit_offset = bit_offset as usize;
                    let num_bits = num_bits as usize;
                    let len = binary_data.len();

                    // Calculate which bytes we need to modify
                    let total_bit_start = byte_offset * 8 + bit_offset;
                    let total_bit_end = total_bit_start + num_bits;
                    let last_byte_needed = total_bit_end.div_ceil(8);

                    if last_byte_needed > len {
                        return Err(Error::InvalidArgument(format!(
                            "Not enough bits: need {} bits starting at byte {} bit {}",
                            num_bits, byte_offset, bit_offset
                        )));
                    }

                    // Validate value fits in num_bits
                    let max_value = if num_bits == 64 {
                        u64::MAX
                    } else {
                        (1u64 << num_bits) - 1
                    };

                    let value_i64 = bigint_to_i64(value)?;
                    if value_i64 < 0 || (value_i64 as u64) > max_value {
                        return Err(Error::InvalidArgument(format!(
                            "Value {} does not fit in {} bits",
                            value, num_bits
                        )));
                    }

                    let value_u64 = value_i64 as u64;

                    // Read the bytes we need to modify
                    let bytes_to_modify = last_byte_needed - byte_offset;
                    let mut modified_bytes = Vec::with_capacity(bytes_to_modify);

                    for i in 0..bytes_to_modify {
                        modified_bytes.push(binary_data.byte_at(byte_offset + i).unwrap());
                    }

                    // Create a mask for the bits we want to modify
                    let bits_in_modified = bytes_to_modify * 8;
                    let bits_after = bits_in_modified - bit_offset - num_bits;

                    // Shift value to correct position
                    let shifted_value = value_u64 << bits_after;

                    // Create mask: all 1s except in our target bits
                    let mask = if num_bits == 64 {
                        0
                    } else {
                        let target_mask = ((1u64 << num_bits) - 1) << bits_after;
                        !target_mask
                    };

                    // Reconstruct the bytes
                    let mut current_bytes = 0u64;
                    for &byte in &modified_bytes {
                        current_bytes = (current_bytes << 8) | (byte as u64);
                    }

                    let new_bytes_value = (current_bytes & mask) | shifted_value;

                    // Convert back to bytes
                    let mut new_bytes = Vec::with_capacity(bytes_to_modify);
                    for i in (0..bytes_to_modify).rev() {
                        new_bytes.push(((new_bytes_value >> (i * 8)) & 0xFF) as u8);
                    }

                    // Use O(log n) slicing to construct result
                    let result = if byte_offset == 0 && last_byte_needed == len {
                        // Replacing entire binary
                        BinaryData::new(new_bytes)
                    } else if byte_offset == 0 {
                        // Modified bytes at start
                        let right = BinaryData::slice(
                            Rc::new(binary_data.clone()),
                            last_byte_needed,
                            len - last_byte_needed,
                        )
                        .unwrap();
                        BinaryData::concat(Rc::new(BinaryData::new(new_bytes)), Rc::new(right))
                    } else if last_byte_needed == len {
                        // Modified bytes at end
                        let left = BinaryData::slice(Rc::new(binary_data.clone()), 0, byte_offset)
                            .unwrap();
                        BinaryData::concat(Rc::new(left), Rc::new(BinaryData::new(new_bytes)))
                    } else {
                        // Modified bytes in middle
                        let left = BinaryData::slice(Rc::new(binary_data.clone()), 0, byte_offset)
                            .unwrap();
                        let right = BinaryData::slice(
                            Rc::new(binary_data.clone()),
                            last_byte_needed,
                            len - last_byte_needed,
                        )
                        .unwrap();
                        let with_middle =
                            BinaryData::concat(Rc::new(left), Rc::new(BinaryData::new(new_bytes)));
                        BinaryData::concat(Rc::new(with_middle), Rc::new(right))
                    };

                    let binary = executor.allocate_binary_data(result)?;
                    Ok(BuiltinResult::Value(Value::Binary(binary)))
                }
                _ => Err(Error::TypeMismatch {
                    expected: "[binary, integer, integer, integer, integer]".to_string(),
                    found: "invalid tuple contents".to_string(),
                }),
            }
        }
        _ => Err(Error::TypeMismatch {
            expected: "[binary, integer, integer, integer, integer]".to_string(),
            found: "not a 5-element tuple".to_string(),
        }),
    }
}

// =============================================================================
// BINARY SLICING OPERATIONS
// =============================================================================

/// Extract a slice from binary [start, end) (end is exclusive)
/// binary_slice([bin, int, int]) -> bin
pub fn builtin_binary_slice<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 3 => {
            match (&elements[0], &elements[1], &elements[2]) {
                (Value::Binary(binary), Value::Integer(start), Value::Integer(end)) => {
                    let binary_data = executor.get_binary_data(binary)?;

                    if start.sign() == num_bigint::Sign::Minus
                        || end.sign() == num_bigint::Sign::Minus
                    {
                        return Err(Error::InvalidArgument(
                            "Indices cannot be negative".to_string(),
                        ));
                    }

                    let start = bigint_to_usize(start)?;
                    let end = bigint_to_usize(end)?;
                    let len = binary_data.len();

                    if start > len || end > len {
                        return Err(Error::InvalidArgument("Index out of bounds".to_string()));
                    }

                    if start > end {
                        return Err(Error::InvalidArgument(
                            "Start index must be <= end index".to_string(),
                        ));
                    }

                    // Use O(1) structural slicing instead of materializing
                    let sliced =
                        BinaryData::slice(Rc::new(binary_data.clone()), start, end - start)
                            .ok_or_else(|| {
                                Error::InvalidArgument("Failed to create slice".to_string())
                            })?;
                    let binary = executor.allocate_binary_data(sliced)?;
                    Ok(BuiltinResult::Value(Value::Binary(binary)))
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
// HASHING OPERATIONS
// =============================================================================

/// Simple FNV-1a hash implementation for 32-bit hashes
/// binary_hash32(bin) -> int
pub fn builtin_binary_hash32<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Binary(binary) => {
            let binary_data = executor.get_binary_data(binary)?;

            // FNV-1a 32-bit hash using iterator to avoid materializing
            let hash = binary_data.iter().fold(2166136261u32, |hash, byte| {
                (hash ^ (byte as u32)).wrapping_mul(16777619)
            });

            Ok(BuiltinResult::Value(Value::Integer(BigInt::from(hash))))
        }
        _ => Err(Error::TypeMismatch {
            expected: "binary".to_string(),
            found: "not binary".to_string(),
        }),
    }
}

/// Simple FNV-1a hash implementation for 64-bit hashes
/// binary_hash64(bin) -> int
pub fn builtin_binary_hash64<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Binary(binary) => {
            let binary_data = executor.get_binary_data(binary)?;

            // FNV-1a 64-bit hash using iterator to avoid materializing
            let hash = binary_data
                .iter()
                .fold(14695981039346656037u64, |hash, byte| {
                    (hash ^ (byte as u64)).wrapping_mul(1099511628211)
                });

            // Note: This may not fit in i64 but we cast it anyway, preserving the
            // historical wrapping behaviour of the 64-bit hash.
            Ok(BuiltinResult::Value(Value::Integer(BigInt::from(
                hash as i64,
            ))))
        }
        _ => Err(Error::TypeMismatch {
            expected: "binary".to_string(),
            found: "not binary".to_string(),
        }),
    }
}

/// Append an integer as bytes to a binary
/// binary_append([bin, int, num_bytes]) -> bin
pub fn builtin_binary_append<E: Effect>(
    _process_id: ProcessId,
    arg: &Value,
    executor: &mut Executor<E>,
) -> Result<BuiltinResult<E>, Error> {
    match arg {
        Value::Tuple(_, elements) if elements.len() == 3 => {
            match (&elements[0], &elements[1], &elements[2]) {
                (Value::Binary(binary), Value::Integer(value), Value::Integer(num_bytes)) => {
                    let num_bytes = bigint_to_i64(num_bytes)?;
                    if !(1..=8).contains(&num_bytes) {
                        return Err(Error::InvalidArgument(
                            "Number of bytes must be between 1 and 8".to_string(),
                        ));
                    }

                    if value.sign() == num_bigint::Sign::Minus {
                        return Err(Error::InvalidArgument(
                            "Value cannot be negative".to_string(),
                        ));
                    }

                    let num_bytes = num_bytes as usize;
                    let value = bigint_to_i64(value)? as u64;

                    // Check if value fits in the specified number of bytes
                    let max_value = if num_bytes == 8 {
                        u64::MAX
                    } else {
                        (1u64 << (num_bytes * 8)) - 1
                    };

                    if value > max_value {
                        return Err(Error::InvalidArgument(format!(
                            "Value {} does not fit in {} bytes",
                            value, num_bytes
                        )));
                    }

                    // Convert integer to bytes (big-endian)
                    let mut new_bytes = Vec::with_capacity(num_bytes);
                    for i in (0..num_bytes).rev() {
                        new_bytes.push(((value >> (i * 8)) & 0xFF) as u8);
                    }

                    // Append using efficient concat
                    let binary_data = executor.get_binary_data(binary)?;
                    let new_data = BinaryData::new(new_bytes);
                    let result =
                        BinaryData::concat(Rc::new(binary_data.clone()), Rc::new(new_data));

                    let binary = executor.allocate_binary_data(result)?;
                    Ok(BuiltinResult::Value(Value::Binary(binary)))
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
