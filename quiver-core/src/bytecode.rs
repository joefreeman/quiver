use crate::types::{BuiltinInfo, TupleTypeInfo, Type, TypeLookup};
use num_bigint::BigInt;
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize, Clone)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
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

/// Id remap tables for transplanting bytecode between programs — the environment
/// merging a compiled line into its program, or the linker loading a module artifact
/// into a session. A missing entry means the id is unchanged.
#[derive(Debug, Default)]
pub struct IdRemaps {
    pub constants: std::collections::HashMap<usize, usize>,
    pub functions: std::collections::HashMap<usize, usize>,
    pub tuples: std::collections::HashMap<usize, usize>,
    pub types: std::collections::HashMap<usize, usize>,
    pub builtins: std::collections::HashMap<usize, usize>,
    pub annotation_keys: std::collections::HashMap<usize, usize>,
    pub field_names: std::collections::HashMap<usize, usize>,
    pub sites: std::collections::HashMap<usize, usize>,
}

impl IdRemaps {
    fn map(table: &std::collections::HashMap<usize, usize>, id: usize) -> usize {
        *table.get(&id).unwrap_or(&id)
    }

    /// The [`Id`]-typed twin of [`map`](Self::map), for instruction operands. The remap
    /// tables are keyed by `usize` because they also remap table entries, which are indexed
    /// that way; only the instruction operands are narrowed.
    fn map_id(table: &std::collections::HashMap<usize, usize>, id: Id) -> Id {
        Self::map(table, id as usize) as Id
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct Function {
    pub instructions: Vec<Instruction>,
    pub captures: usize,
    /// Type ID referencing this function's callable type in the types vec
    pub type_id: usize,
}

impl Function {
    /// The same function with every table reference rewritten through `remaps`.
    /// Instruction operands that are not table ids (stack slots, jump offsets,
    /// positions, arities) pass through untouched.
    pub fn remap_ids(self, remaps: &IdRemaps) -> Function {
        let instructions = self
            .instructions
            .into_iter()
            .map(|instruction| match instruction {
                Instruction::Constant(idx) => {
                    Instruction::Constant(IdRemaps::map_id(&remaps.constants, idx))
                }
                Instruction::Function(idx) => {
                    Instruction::Function(IdRemaps::map_id(&remaps.functions, idx))
                }
                Instruction::Builtin(idx, type_argument) => Instruction::Builtin(
                    IdRemaps::map_id(&remaps.builtins, idx),
                    // A type-consuming builtin's type argument is a type reference,
                    // like GetAnnotation's check.
                    type_argument.map(|type_id| IdRemaps::map_id(&remaps.types, type_id)),
                ),
                Instruction::Tuple(tuple_id) => {
                    Instruction::Tuple(IdRemaps::map_id(&remaps.tuples, tuple_id))
                }
                Instruction::IsType(type_id) => {
                    Instruction::IsType(IdRemaps::map_id(&remaps.types, type_id))
                }
                Instruction::GetNamed(name_id) => {
                    Instruction::GetNamed(IdRemaps::map_id(&remaps.field_names, name_id))
                }
                Instruction::Annotate(key) => {
                    Instruction::Annotate(IdRemaps::map_id(&remaps.annotation_keys, key))
                }
                Instruction::GetAnnotation(key, check) => Instruction::GetAnnotation(
                    IdRemaps::map_id(&remaps.annotation_keys, key),
                    check.map(|type_id| IdRemaps::map_id(&remaps.types, type_id)),
                ),
                Instruction::Stamp(site) => {
                    Instruction::Stamp(IdRemaps::map_id(&remaps.sites, site))
                }
                other => other,
            })
            .collect();

        Function {
            instructions,
            captures: self.captures,
            type_id: IdRemaps::map(&remaps.types, self.type_id),
        }
    }
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
/// `:crash` / `:timeout` stamped nils that a never-lethal await delivers.
/// Not carried by `Bytecode` — the environment derives it on its
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

/// The io-failure table: the ids the executor needs to build a failed effect's stamped nil.
/// Present only when the program's host declared the io vocabulary.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ErrorTable {
    /// Annotation key id of `error`.
    pub error_key: usize,
    /// Tuple id of `IoError[kind: …, message: Str['bin]]`.
    pub io_error_tuple: usize,
    /// Tuple ids of the nullary kind tags, indexed by `EffectError::kind_index`.
    pub kind_tuples: Vec<usize>,
    /// Tuple id of `Str['bin]`.
    pub str_tuple: usize,
}

/// One stream resource kind's event-tuple ids — how the executor turns a generic
/// [`crate::process::StreamEvent`] into the kind's declared tuples. Derived from the
/// registry's stream declarations for the resource kinds a program actually names
/// (see `Program::derive_stream_table`); a program touching no streams carries none.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StreamInfo {
    /// The bytes event's tuple (`[source, 'bin]`-shaped), when the kind yields bytes.
    pub data_tuple: Option<usize>,
    /// The fresh-resource event's tuple (`[source, produced]`-shaped) and the
    /// produced resource's type id, when the kind yields resources.
    pub resource_tuple: Option<(usize, usize)>,
    /// The end-of-stream event's tuple (`[source]`-shaped).
    pub end_tuple: usize,
}

/// Stream event vocabulary for the whole program, indexed by resource type id
/// (`None` for non-stream kinds).
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct StreamTable {
    pub streams: Vec<Option<StreamInfo>>,
}

/// Everything the runtime may deliver to this program, resolved to merged-program
/// ids from the host's [`crate::builtins::RuntimeDeclarations`] at each merge
/// (`Program::runtime_tables`) and shipped on `ProgramUpdate`. Each member is
/// demand-scoped: `crash` requires only the declaration (any process can crash),
/// `changed` additionally requires the program to reference its demanding builtin
/// (`track`), and `streams` covers the stream resource kinds the program names.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct RuntimeTables {
    pub crash: Option<CrashTable>,
    /// The `Changed` wakeup tuple id, when demanded.
    pub changed: Option<usize>,
    /// The io-failure payload ids, when the host declared io.
    pub error: Option<ErrorTable>,
    pub streams: StreamTable,
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

/// An instruction operand that indexes a table (constants, functions, types, tuples,
/// builtins, field names, annotation keys, sites) or names a stack/frame slot. `u32` rather
/// than `usize`: instructions are the largest static table a program carries — a live-view
/// app is ~60k of them, replicated per worker — and the two-word operands were what forced
/// `Instruction` to 32 bytes. No program comes close to 4 billion of anything, and a denser
/// instruction also means more of them per cache line in the dispatch loop.
pub type Id = u32;

/// A relative jump, in instructions. `i32` for the same reason as [`Id`].
pub type Offset = i32;

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum Instruction {
    Constant(Id),
    Pop,
    Duplicate,
    Pick(Id),
    Rotate(Id),
    Reset(Id),
    Load(Id),
    Store,
    Tuple(Id),
    /// Pop a tuple; push the field at the given position. Emitted where the static type
    /// pins the field's position (a concrete tuple, or a union agreeing on one).
    GetPositional(Id),
    /// Pop a tuple; push the field whose *name* is the given field-name id, resolved
    /// against the tuple's own type at runtime via the precomputed offset table. Emitted
    /// where the static type doesn't pin a position — a partial type's field, or a union
    /// whose members carry the field at different positions.
    GetNamed(Id),
    IsType(Id),
    Jump(Offset),
    JumpIf(Offset),
    Call,
    TailCall(bool),
    Function(Id),
    /// Push a builtin value by id. The optional type id is a type-consuming builtin's
    /// explicit type argument (`__data_decode__<'t>`), resolved to a concrete type at
    /// compile time and carried on the pushed value for the implementation to read.
    Builtin(Id, Option<Id>),
    Equal(Id),
    Not,
    /// Pop an annotation value, then a tuple/function carrier; push the carrier with the
    /// annotation attached under the given key id (copy-on-annotate).
    Annotate(Id),
    /// Pop a carrier; push its annotation under the given key id, or nil. The optional
    /// type id is the checked form's expected shape (`x:('t)key`): a carried entry
    /// incompatible with it also answers nil, via the same table `IsType` consults.
    GetAnnotation(Id, Option<Id>),
    /// Debug builds only: if the top of the stack is a nil result not yet carrying an
    /// `origin` annotation, stamp it with this site's provenance (see `SiteTable`).
    /// Fresh-only, so a propagating failure keeps its original site. No-op otherwise.
    Stamp(Id),
    Spawn,
    Send,
    Self_,
    Select,
    Process(Id, Id), // (process_id, function_index)
    /// Sample a process's current state (`?p`): pop a process
    /// value, push its state. No runtime test — the state type is statically known
    /// (inferred at spawns; enforced by strict state subtyping at declared boundaries).
    State,
}
