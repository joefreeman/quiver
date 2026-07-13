use crate::types::{BuiltinInfo, TupleTypeInfo, Type, TypeLookup};
use num_bigint::BigInt;
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum Constant {
    // One form for all integers: the small/big split is a runtime-representation
    // concern, applied where a constant becomes a `Value` (`handle_constant`).
    #[serde(rename = "int")]
    Integer(BigInt),
    #[serde(rename = "bin")]
    Binary(Vec<u8>),
}

/// A concrete type that uniquely identifies a runtime value's type.
/// This is used for O(1) type compatibility checking at runtime.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ConcreteType {
    Integer,
    Binary,
    Reference,
    Tuple(usize),    // tuple_id
    Function(usize), // func_id
    Builtin(usize),  // builtin_id
    Process(usize),  // func_id (spawning function)
    Resource(usize), // resource_type_id
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct Function {
    pub instructions: Vec<Instruction>,
    pub captures: usize,
    /// Type ID referencing this function's callable type in the types vec
    pub type_id: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Bytecode {
    pub constants: Vec<Constant>,
    pub functions: Vec<Function>,
    /// Builtin information with type_ids
    pub builtins: Vec<BuiltinInfo>,
    pub entry: Option<usize>,
    /// Tuple type information with type_ids for fields
    pub tuples: Vec<TupleTypeInfo>,
    /// Types (referenced by type_ids throughout bytecode)
    pub types: Vec<Type>,
    /// Resource type names (index is resource_id, used for effect dispatch)
    pub resources: Vec<String>,
    /// Annotation key names (index is the key id carried by Annotate/GetAnnotation)
    #[serde(default)]
    pub annotation_keys: Vec<String>,
    /// Field names interned for `GetNamed` (index is the field-name id it carries). Used
    /// only at load time to build the name→offset tables; never consulted per-instruction.
    #[serde(default)]
    pub field_names: Vec<String>,
    /// Failure-provenance sites (debug builds only): `Stamp` instructions index into it.
    #[serde(default)]
    pub debug: Option<SiteTable>,
}

/// What kind of failure a provenance site marks — nil is a failure *positionally* (a nil
/// result short-circuits), so sites sit where an expression's value becomes a result.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SiteKind {
    /// A step whose chain matches (a binding or `=pattern` guard) yielded nil.
    NoMatch,
    /// A block's branches were exhausted (the fall-through nil).
    BlockExhausted,
    /// Any other nil result (a deliberate `[]`, a nil-returning call, ...).
    NilResult,
}

/// One failure-provenance site: a source position a debug build stamps onto fresh nil
/// results (under the `origin` annotation key, as a `Site[...]` tuple value).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Site {
    /// Constants-table index of the module/source display name (a binary).
    pub module_constant: usize,
    pub line: u32,
    pub column: u32,
    pub kind: SiteKind,
}

/// The crash-delivery table (both build modes): the ids the executor needs to build the
/// `:crash` / `:timeout` stamped nils that a never-lethal await delivers (see
/// docs/process-state.md). Not carried by `Bytecode` — the environment derives it on its
/// merged program (`Program::crash_table`) and ships it with each `ProgramUpdate`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CrashTable {
    /// Annotation key id of `crash`.
    pub crash_key: usize,
    /// Annotation key id of `timeout`.
    pub timeout_key: usize,
    /// Tuple id of `Error[pid: (@), message: Str['bin]]` (runtime errors).
    pub error_tuple: usize,
    /// Tuple id of `Panic[pid: (@), message: Str['bin]]` (`__panic__` aborts).
    pub panic_tuple: usize,
    /// Tuple id of `Killed` (empty; kill/link/teardown — step 4).
    pub killed_tuple: usize,
    /// Tuple id of `Str['bin]`.
    pub str_tuple: usize,
}

/// The failure-provenance table of a debug build: the sites plus the ids the executor
/// needs to prebuild each site's annotated-nil value at load time.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SiteTable {
    /// Annotation key id of `origin`.
    pub origin_key: usize,
    /// Tuple id of `Site[module: Str['bin], line: 'int, column: 'int, kind: ...]`.
    pub site_tuple: usize,
    /// Tuple id of `Str['bin]`.
    pub str_tuple: usize,
    /// Tuple id per `SiteKind` (empty named tuples: `NoMatch`, `BlockExhausted`, ...).
    pub kind_tuples: Vec<usize>,
    pub sites: Vec<Site>,
}

impl SiteKind {
    /// Every kind, in discriminant order — `ALL[k.index()] == k` (asserted in tests), so
    /// `kind_tuples` built by iterating `ALL` is indexed correctly by `index()`.
    pub const ALL: [SiteKind; 3] = [
        SiteKind::NoMatch,
        SiteKind::BlockExhausted,
        SiteKind::NilResult,
    ];

    /// The tuple name of this kind's marker value.
    pub fn name(self) -> &'static str {
        match self {
            SiteKind::NoMatch => "NoMatch",
            SiteKind::BlockExhausted => "BlockExhausted",
            SiteKind::NilResult => "NilResult",
        }
    }

    /// This kind's index into `kind_tuples` (its discriminant).
    pub fn index(self) -> usize {
        self as usize
    }
}

#[cfg(test)]
mod tests {
    use super::SiteKind;

    #[test]
    fn site_kind_all_matches_indices() {
        for (position, kind) in SiteKind::ALL.iter().enumerate() {
            assert_eq!(kind.index(), position);
        }
    }
}

impl TypeLookup for Bytecode {
    fn lookup_type(&self, type_id: usize) -> Option<&Type> {
        self.types.get(type_id)
    }

    fn lookup_tuple(&self, tuple_id: usize) -> Option<&TupleTypeInfo> {
        self.tuples.get(tuple_id)
    }

    fn lookup_annotation_key_name(&self, key: usize) -> Option<&str> {
        self.annotation_keys.get(key).map(|name| name.as_str())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum Instruction {
    Constant(usize),
    Pop,
    Duplicate,
    Pick(usize),
    Rotate(usize),
    Reset(usize),
    Load(usize),
    Store,
    Tuple(usize),
    /// Pop a tuple; push the field at the given position. Emitted where the static type
    /// pins the field's position (a concrete tuple, or a union agreeing on one).
    GetPositional(usize),
    /// Pop a tuple; push the field whose *name* is the given field-name id, resolved
    /// against the tuple's own type at runtime via the precomputed offset table. Emitted
    /// where the static type doesn't pin a position — a partial type's field, or a union
    /// whose members carry the field at different positions.
    GetNamed(usize),
    IsType(usize),
    Jump(isize),
    JumpIf(isize),
    Call,
    TailCall(bool),
    Function(usize),
    Builtin(usize),
    Equal(usize),
    Not,
    /// Pop an annotation value, then a tuple/function carrier; push the carrier with the
    /// annotation attached under the given key id (copy-on-annotate).
    Annotate(usize),
    /// Pop a carrier; push its annotation under the given key id, or nil. The optional
    /// type id is the checked form's expected shape (`x:('t)key`): a carried entry
    /// incompatible with it also answers nil, via the same table `IsType` consults.
    GetAnnotation(usize, Option<usize>),
    /// Debug builds only: if the top of the stack is a nil result not yet carrying an
    /// `origin` annotation, stamp it with this site's provenance (see `SiteTable`).
    /// Fresh-only, so a propagating failure keeps its original site. No-op otherwise.
    Stamp(usize),
    Spawn,
    Send,
    Self_,
    Select,
    Process(usize, usize), // (process_id, function_index)
    /// Sample a process's current state (`?p` — docs/process-state.md): pop a process
    /// value, push its state. No runtime test — the state type is statically known
    /// (inferred at spawns; enforced by strict state subtyping at declared boundaries).
    State,
}
