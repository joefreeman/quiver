use std::rc::Rc;
use std::sync::Arc;

/// Maximum size of a binary value in bytes (16MB)
pub const MAX_BINARY_SIZE: usize = 16 * 1024 * 1024;

/// Rope-like structure for efficient binary operations.
/// Supports O(1) slicing and concatenation through structural sharing.
///
/// Serialized as its flat bytes: the rope is an internal representation of *how* a binary was
/// built, not part of the value it denotes, so a round trip through disk or a JSON transport
/// yields a flat `Owned` with the same contents.
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryData {
    /// Raw bytes, refcounted so cloning a `BinaryData` is always O(1) — in particular
    /// `concat(Owned, _)` shares the buffer rather than copying it. The bytes are never mutated
    /// through this handle (values are immutable), so sharing is sound.
    ///
    /// `Arc<[u8]>`, not `Rc<Vec<u8>>`, for two reasons. It is one allocation rather than
    /// two: the old form was doubly indirect, an `Rc` box holding a `Vec` header that pointed
    /// at the bytes. And it is *atomic*, which is what lets a leaf cross a worker boundary —
    /// [`WireValue::Binary`](crate::wire::WireValue) carries this handle, so a send shares the
    /// bytes rather than copying them. The rope spine above it stays `Rc`: it is
    /// process-local and never crosses, so it pays no atomics.
    Owned(Arc<[u8]>),

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

impl serde::Serialize for BinaryData {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_bytes(&self.to_vec())
    }
}

impl<'de> serde::Deserialize<'de> for BinaryData {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let bytes = <Vec<u8> as serde::Deserialize>::deserialize(deserializer)?;
        Ok(BinaryData::new(bytes))
    }
}

impl BinaryData {
    /// Create a new owned binary from a Vec<u8>
    pub fn new(bytes: Vec<u8>) -> Self {
        BinaryData::Owned(bytes.into())
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
            return Some(BinaryData::Zeroed(0));
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
            return BinaryData::Zeroed(0);
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

    /// Helper to write bytes to a vector (used by to_vec).
    ///
    /// Iterative: a left-leaning `Concat` spine can be n deep (a vector built by repeated
    /// push), so recursing the spine would overflow the stack. We walk an explicit work-list
    /// instead, emitting nodes left-to-right. `Slice`/`Tiled` still flatten their (shallow)
    /// parent/unit via `to_vec`, which is itself iterative — the deep structure we create is
    /// always a `Concat` spine, never nested slices.
    fn write_to_vec(&self, out: &mut Vec<u8>) {
        let mut stack: Vec<&BinaryData> = vec![self];
        while let Some(node) = stack.pop() {
            match node {
                BinaryData::Owned(bytes) => out.extend_from_slice(bytes),
                BinaryData::Zeroed(len) => out.resize(out.len() + len, 0),
                BinaryData::Slice {
                    parent,
                    offset,
                    length,
                } => {
                    // Emit only the slice's range, descending into the parent's overlapping
                    // nodes — a small slice of a large binary must cost the slice, not the
                    // whole parent. (Materializing the parent here made `to_vec` O(parent)
                    // per call, so scanning with per-position slices — `str.find_index`,
                    // used by `contains?`/`index_of` — was O(n²).)
                    parent.append_range(out, *offset, *length);
                }
                BinaryData::Concat { left, right, .. } => {
                    // Emit left before right: push right first so left pops next.
                    stack.push(right);
                    stack.push(left);
                }
                BinaryData::Tiled { unit, count } => {
                    let unit_bytes = unit.to_vec();
                    out.reserve(unit_bytes.len() * count);
                    for _ in 0..*count {
                        out.extend_from_slice(&unit_bytes);
                    }
                }
            }
        }
    }

    /// Append bytes `[start, start + length)` of this binary to `out`, descending only into
    /// the rope nodes that overlap the range. Cost is proportional to the range (plus the
    /// depth traversed), not the whole binary — so extracting a small slice of a large binary
    /// is cheap. Iterative (an explicit work-list) so a deep `Concat` spine can't overflow the
    /// stack, mirroring [`write_to_vec`].
    fn append_range(&self, out: &mut Vec<u8>, start: usize, length: usize) {
        // Each work item is (node, start-within-node, len). For a `Concat` we push the right
        // child before the left so the left pops (and emits) first — preserving byte order.
        let mut stack: Vec<(&BinaryData, usize, usize)> = vec![(self, start, length)];
        while let Some((node, s, l)) = stack.pop() {
            if l == 0 {
                continue;
            }
            match node {
                BinaryData::Owned(bytes) => out.extend_from_slice(&bytes[s..s + l]),
                BinaryData::Zeroed(_) => out.resize(out.len() + l, 0),
                BinaryData::Slice { parent, offset, .. } => {
                    stack.push((parent, offset + s, l));
                }
                BinaryData::Concat { left, right, .. } => {
                    let left_len = left.len();
                    if s >= left_len {
                        stack.push((right, s - left_len, l));
                    } else if s + l <= left_len {
                        stack.push((left, s, l));
                    } else {
                        let left_take = left_len - s;
                        stack.push((right, 0, l - left_take));
                        stack.push((left, s, left_take));
                    }
                }
                BinaryData::Tiled { unit, .. } => {
                    // The unit is shallow; emit the requested range against the repeated
                    // sequence directly so this leaf stays in order.
                    let unit_bytes = unit.to_vec();
                    let unit_len = unit_bytes.len();
                    let mut pos = s;
                    let mut rem = l;
                    while rem > 0 {
                        let within = pos % unit_len;
                        let take = rem.min(unit_len - within);
                        out.extend_from_slice(&unit_bytes[within..within + take]);
                        pos += take;
                        rem -= take;
                    }
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

    /// The bytes as a shareable handle: the leaf itself when this is already flat (O(1), no
    /// copy — the handle a cross-worker send passes instead of the bytes), otherwise the rope
    /// realised into a fresh one.
    pub fn shared_bytes(&self) -> Arc<[u8]> {
        match self {
            BinaryData::Owned(bytes) => bytes.clone(),
            _ => self.to_vec().into(),
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

    /// Whether these bytes are shared with another holder: for a flat leaf, whether its `Arc`
    /// has more than one owner — another value on this worker, or one on another worker, since
    /// a send passes the handle. A rope answers `false`; the question is about whole buffers,
    /// and a rope's own node is what a value holds.
    pub fn bytes_shared(&self) -> bool {
        match self {
            BinaryData::Owned(bytes) => Arc::strong_count(bytes) > 1,
            _ => false,
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

thread_local! {
    /// A shared empty leaf used to vacate `Rc` child slots during iterative drop without
    /// allocating. Cloning it is a refcount bump; it holds no children, so it drops trivially.
    static EMPTY_RC: Rc<BinaryData> = Rc::new(BinaryData::Zeroed(0));
}

impl Drop for BinaryData {
    /// Drop iteratively. The default (recursive) drop would walk a left-leaning `Concat` spine
    /// — n deep for a vector built by repeated push — and overflow the stack. Instead we move
    /// owned children onto an explicit work-list and drop them one at a time, descending only
    /// into `Rc`s we uniquely own (a shared child must stay intact; its `Rc` just decrements).
    fn drop(&mut self) {
        // Fast path: leaves have no `Rc` children, so their fields drop trivially (an `Owned`
        // buffer's `Rc<Vec>` simply decrements). This keeps the common flat-binary drop cheap.
        match self {
            BinaryData::Owned(_) | BinaryData::Zeroed(_) => return,
            _ => {}
        }

        // `try_with`: a rope can now be dropped during thread teardown — a compile-time
        // module value owns its binaries, and those values live in a thread-local artifact
        // store — and TLS destruction order is unspecified. Falling back to a private empty
        // leaf costs one allocation on a path that runs at most once per thread exit.
        let empty = EMPTY_RC
            .try_with(Rc::clone)
            .unwrap_or_else(|_| Rc::new(BinaryData::Zeroed(0)));
        let mut stack: Vec<Rc<BinaryData>> = Vec::new();
        take_children(self, &empty, &mut stack);
        while let Some(rc) = stack.pop() {
            // Sole owner: take the inner node and vacate ITS children before it drops, so its
            // own (recursive) drop sees only empty leaves and terminates immediately. Shared:
            // dropping `rc` here just decrements, leaving the still-referenced subtree intact.
            if let Ok(mut inner) = Rc::try_unwrap(rc) {
                take_children(&mut inner, &empty, &mut stack);
            }
        }
    }
}

/// Replace each `Rc` child of `node` with the shared empty leaf, pushing the originals onto
/// `stack`. After this, `node`'s own drop is non-recursive (its children are leaves).
fn take_children(node: &mut BinaryData, empty: &Rc<BinaryData>, stack: &mut Vec<Rc<BinaryData>>) {
    match node {
        BinaryData::Concat { left, right, .. } => {
            stack.push(std::mem::replace(left, empty.clone()));
            stack.push(std::mem::replace(right, empty.clone()));
        }
        BinaryData::Slice { parent, .. } => {
            stack.push(std::mem::replace(parent, empty.clone()));
        }
        BinaryData::Tiled { unit, .. } => {
            stack.push(std::mem::replace(unit, empty.clone()));
        }
        BinaryData::Owned(_) | BinaryData::Zeroed(_) => {}
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
    fn deep_rope_read_and_drop_are_stack_safe() {
        // A left-leaning `Concat` spine of this depth would overflow a recursive `to_vec` or a
        // recursive `Drop`. Run on a deliberately small stack so the test only passes if both
        // are iterative — i.e. we don't silently rely on the worker's oversized stack.
        std::thread::Builder::new()
            .stack_size(256 * 1024)
            .spawn(|| {
                let depth = 100_000;
                let mut acc = Rc::new(BinaryData::new(vec![0u8]));
                for i in 1..depth {
                    let lane = Rc::new(BinaryData::new(vec![(i & 0xff) as u8]));
                    acc = Rc::new(BinaryData::concat(acc, lane));
                }
                // Read: iterative `write_to_vec` must not recurse the spine.
                assert_eq!(acc.to_vec().len(), depth);
                // Drop: leaving scope tears the deep rope down via the iterative `Drop`.
            })
            .unwrap()
            .join()
            .unwrap();
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
