use std::rc::Rc;

/// Maximum size of a binary value in bytes (16MB)
pub const MAX_BINARY_SIZE: usize = 16 * 1024 * 1024;

/// Rope-like structure for efficient binary operations.
/// Supports O(1) slicing and concatenation through structural sharing.
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryData {
    /// Raw owned bytes
    Owned(Vec<u8>),

    /// Zero-filled binary of given length (no allocation until materialized)
    Zeroed(usize),

    /// Slice of another binary - O(1) substring
    Slice {
        parent: Rc<BinaryData>,
        offset: usize,
        length: usize,
    },

    /// Concatenation of two binaries - O(1) append
    Concat {
        left: Rc<BinaryData>,
        right: Rc<BinaryData>,
        total_length: usize,
    },

    /// A `unit` binary repeated `count` times (no allocation until materialized).
    /// Generalises `Zeroed` (which is `[0x00]` tiled) and gives compact constant/broadcast
    /// binaries — e.g. a scalar lane tiled across a vector — that realise lazily.
    Tiled { unit: Rc<BinaryData>, count: usize },
}

impl BinaryData {
    /// Create a new owned binary from a Vec<u8>
    pub fn new(bytes: Vec<u8>) -> Self {
        BinaryData::Owned(bytes)
    }

    /// Create a zero-filled binary of the given length without allocating
    pub fn zeroed(length: usize) -> Self {
        BinaryData::Zeroed(length)
    }

    /// Get the length of this binary in O(1) time
    pub fn len(&self) -> usize {
        match self {
            BinaryData::Owned(vec) => vec.len(),
            BinaryData::Zeroed(len) => *len,
            BinaryData::Slice { length, .. } => *length,
            BinaryData::Concat { total_length, .. } => *total_length,
            BinaryData::Tiled { unit, count } => unit.len() * count,
        }
    }

    /// Check if the binary is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Concatenate two binaries in O(1) time through structural sharing
    pub fn concat(left: Rc<BinaryData>, right: Rc<BinaryData>) -> Self {
        let total_length = left.len() + right.len();
        BinaryData::Concat {
            left,
            right,
            total_length,
        }
    }

    /// Create a slice of this binary in O(1) time
    /// Returns None if the slice is out of bounds
    pub fn slice(parent: Rc<BinaryData>, offset: usize, length: usize) -> Option<Self> {
        let parent_len = parent.len();

        // Check bounds
        if offset > parent_len || offset.checked_add(length)? > parent_len {
            return None;
        }

        // Empty slice
        if length == 0 {
            return Some(BinaryData::Owned(Vec::new()));
        }

        // Full slice - return parent
        if offset == 0 && length == parent_len {
            return Some((*parent).clone());
        }

        Some(BinaryData::Slice {
            parent,
            offset,
            length,
        })
    }

    /// Create a binary made of `unit` repeated `count` times in O(1) time, sharing `unit`.
    /// Normalises the degenerate cases so callers needn't special-case them.
    pub fn tiled(unit: Rc<BinaryData>, count: usize) -> Self {
        if count == 0 || unit.is_empty() {
            return BinaryData::Owned(Vec::new());
        }
        if count == 1 {
            return (*unit).clone();
        }
        BinaryData::Tiled { unit, count }
    }

    /// Get the byte at the given index
    /// Returns None if index is out of bounds
    pub fn byte_at(&self, index: usize) -> Option<u8> {
        if index >= self.len() {
            return None;
        }

        match self {
            BinaryData::Owned(vec) => Some(vec[index]),
            BinaryData::Zeroed(_) => Some(0),
            BinaryData::Slice {
                parent,
                offset,
                length: _,
            } => parent.byte_at(offset + index),
            BinaryData::Concat { left, right, .. } => {
                let left_len = left.len();
                if index < left_len {
                    left.byte_at(index)
                } else {
                    right.byte_at(index - left_len)
                }
            }
            // `index < self.len()` is guaranteed above, so a zero-length unit (which makes a
            // zero-length tile) can't reach here — the modulo is safe.
            BinaryData::Tiled { unit, .. } => unit.byte_at(index % unit.len()),
        }
    }

    /// Flatten this binary into a Vec<u8>
    /// This traverses the structure and materializes all bytes
    pub fn to_vec(&self) -> Vec<u8> {
        let len = self.len();
        let mut result = Vec::with_capacity(len);
        self.write_to_vec(&mut result);
        result
    }

    /// Helper to write bytes to a vector (used by to_vec)
    fn write_to_vec(&self, vec: &mut Vec<u8>) {
        match self {
            BinaryData::Owned(bytes) => vec.extend_from_slice(bytes),
            BinaryData::Zeroed(len) => vec.resize(vec.len() + len, 0),
            BinaryData::Slice {
                parent,
                offset,
                length,
            } => {
                // For slices, we need to extract the specific range
                let parent_vec = parent.to_vec();
                vec.extend_from_slice(&parent_vec[*offset..*offset + *length]);
            }
            BinaryData::Concat { left, right, .. } => {
                left.write_to_vec(vec);
                right.write_to_vec(vec);
            }
            BinaryData::Tiled { unit, count } => {
                let unit_bytes = unit.to_vec();
                vec.reserve(unit_bytes.len() * count);
                for _ in 0..*count {
                    vec.extend_from_slice(&unit_bytes);
                }
            }
        }
    }

    /// Create an iterator over the bytes in this binary
    pub fn iter(&self) -> BinaryIterator<'_> {
        BinaryIterator {
            binary: self,
            index: 0,
        }
    }

    /// Find the index of the first occurrence of `byte` at or after `offset`.
    ///
    /// Walks the rope structure directly without materializing it, so a search from an
    /// advancing offset (e.g. scanning successive lines of a buffer) stays linear overall.
    pub fn find_byte(&self, byte: u8, offset: usize) -> Option<usize> {
        match self {
            BinaryData::Owned(bytes) => {
                if offset >= bytes.len() {
                    return None;
                }
                bytes[offset..]
                    .iter()
                    .position(|&b| b == byte)
                    .map(|p| p + offset)
            }
            BinaryData::Zeroed(len) => (byte == 0 && offset < *len).then_some(offset),
            BinaryData::Slice {
                parent,
                offset: slice_offset,
                length,
            } => {
                if offset >= *length {
                    return None;
                }
                parent
                    .find_byte(byte, slice_offset + offset)
                    .map(|abs| abs - slice_offset)
                    .filter(|&rel| rel < *length)
            }
            BinaryData::Concat { left, right, .. } => {
                let left_len = left.len();
                if offset < left_len {
                    if let Some(index) = left.find_byte(byte, offset) {
                        return Some(index);
                    }
                    right.find_byte(byte, 0).map(|index| index + left_len)
                } else {
                    right
                        .find_byte(byte, offset - left_len)
                        .map(|index| index + left_len)
                }
            }
            BinaryData::Tiled { unit, count } => {
                let unit_len = unit.len();
                if unit_len == 0 || offset >= unit_len * count {
                    return None;
                }
                let start_unit = offset / unit_len;
                // The (possibly partial) first unit, searched from the offset within it.
                if let Some(pos) = unit.find_byte(byte, offset % unit_len) {
                    return Some(start_unit * unit_len + pos);
                }
                // Every later unit is identical, so the first whole one after the partial
                // settles it (its hit, if any, is at the same in-unit position).
                if start_unit + 1 < *count
                    && let Some(pos) = unit.find_byte(byte, 0)
                {
                    return Some((start_unit + 1) * unit_len + pos);
                }
                None
            }
        }
    }

    /// Get the depth of the tree structure (useful for compaction heuristics later)
    pub fn depth(&self) -> usize {
        match self {
            BinaryData::Owned(_) | BinaryData::Zeroed(_) => 0,
            BinaryData::Slice { parent, .. } => parent.depth() + 1,
            BinaryData::Concat { left, right, .. } => left.depth().max(right.depth()) + 1,
            BinaryData::Tiled { unit, .. } => unit.depth() + 1,
        }
    }
}

/// Iterator over bytes in a BinaryData structure
pub struct BinaryIterator<'a> {
    binary: &'a BinaryData,
    index: usize,
}

impl<'a> Iterator for BinaryIterator<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let byte = self.binary.byte_at(self.index)?;
        self.index += 1;
        Some(byte)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.binary.len().saturating_sub(self.index);
        (remaining, Some(remaining))
    }
}

impl<'a> ExactSizeIterator for BinaryIterator<'a> {
    fn len(&self) -> usize {
        self.binary.len().saturating_sub(self.index)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_owned() {
        let data = BinaryData::new(vec![1, 2, 3]);
        assert_eq!(data.len(), 3);
        assert_eq!(data.byte_at(0), Some(1));
        assert_eq!(data.byte_at(1), Some(2));
        assert_eq!(data.byte_at(2), Some(3));
        assert_eq!(data.byte_at(3), None);
    }

    #[test]
    fn test_zeroed() {
        let data = BinaryData::zeroed(100);
        assert_eq!(data.len(), 100);
        assert_eq!(data.byte_at(0), Some(0));
        assert_eq!(data.byte_at(99), Some(0));
        assert_eq!(data.byte_at(100), None);
        assert_eq!(data.to_vec(), vec![0; 100]);
    }

    #[test]
    fn test_concat() {
        let left = Rc::new(BinaryData::new(vec![1, 2, 3]));
        let right = Rc::new(BinaryData::new(vec![4, 5, 6]));
        let concat = BinaryData::concat(left, right);

        assert_eq!(concat.len(), 6);
        assert_eq!(concat.byte_at(0), Some(1));
        assert_eq!(concat.byte_at(2), Some(3));
        assert_eq!(concat.byte_at(3), Some(4));
        assert_eq!(concat.byte_at(5), Some(6));
        assert_eq!(concat.to_vec(), vec![1, 2, 3, 4, 5, 6]);
    }

    #[test]
    fn test_slice() {
        let data = Rc::new(BinaryData::new(vec![1, 2, 3, 4, 5]));
        let slice = BinaryData::slice(data, 1, 3).unwrap();

        assert_eq!(slice.len(), 3);
        assert_eq!(slice.byte_at(0), Some(2));
        assert_eq!(slice.byte_at(1), Some(3));
        assert_eq!(slice.byte_at(2), Some(4));
        assert_eq!(slice.to_vec(), vec![2, 3, 4]);
    }

    #[test]
    fn test_slice_bounds() {
        let data = Rc::new(BinaryData::new(vec![1, 2, 3]));

        // Valid slices
        assert!(BinaryData::slice(data.clone(), 0, 3).is_some());
        assert!(BinaryData::slice(data.clone(), 1, 2).is_some());
        assert!(BinaryData::slice(data.clone(), 3, 0).is_some());

        // Invalid slices
        assert!(BinaryData::slice(data.clone(), 4, 0).is_none());
        assert!(BinaryData::slice(data.clone(), 0, 4).is_none());
        assert!(BinaryData::slice(data.clone(), 2, 2).is_none());
    }

    #[test]
    fn test_nested_operations() {
        // Create a complex structure: concat(slice(owned), zeroed)
        let owned = Rc::new(BinaryData::new(vec![1, 2, 3, 4, 5]));
        let sliced = Rc::new(BinaryData::slice(owned, 1, 3).unwrap()); // [2, 3, 4]
        let zeroed = Rc::new(BinaryData::zeroed(2)); // [0, 0]
        let concat = BinaryData::concat(sliced, zeroed); // [2, 3, 4, 0, 0]

        assert_eq!(concat.len(), 5);
        assert_eq!(concat.byte_at(0), Some(2));
        assert_eq!(concat.byte_at(2), Some(4));
        assert_eq!(concat.byte_at(3), Some(0));
        assert_eq!(concat.byte_at(4), Some(0));
        assert_eq!(concat.to_vec(), vec![2, 3, 4, 0, 0]);
    }

    #[test]
    fn test_iterator() {
        let data = BinaryData::new(vec![1, 2, 3]);
        let collected: Vec<u8> = data.iter().collect();
        assert_eq!(collected, vec![1, 2, 3]);
    }

    #[test]
    fn test_depth() {
        let owned = BinaryData::new(vec![1, 2, 3]);
        assert_eq!(owned.depth(), 0);

        let sliced = BinaryData::slice(Rc::new(owned), 0, 2).unwrap();
        assert_eq!(sliced.depth(), 1);

        let concat = BinaryData::concat(Rc::new(sliced), Rc::new(BinaryData::zeroed(2)));
        assert_eq!(concat.depth(), 2);
    }
}
