use crate::types::{BuiltinInfo, NIL, OK, TupleTypeInfo, Type, TypeLookup};
use num_bigint::BigInt;
use serde::{Deserialize, Serialize};

/// A value the program carries rather than builds: the compiler interns it once and the
/// bytecode names it by index, where it would otherwise emit instructions that rebuild it on
/// every evaluation.
///
/// Composites reference their children **by constant index** rather than nesting them. That
/// is what makes [`crate::program::Program::register_constant`]'s content interning exact —
/// children are registered first, so structurally identical subtrees get identical indices
/// and their parents then digest identically — and it is what lets the executor's
/// materialisation memo reproduce the compile-time graph's sharing instead of expanding it
/// into a tree. It also keeps remapping shallow: a slot rewrites a few ids, with no
/// recursive walk over value structure.
///
/// The graph is acyclic. Values are built bottom-up, and a module's members cannot reference
/// each other (aliases are positional — a definition precedes its uses), so nothing closes a
/// loop. Identity-bearing values — refs, pids, resources — have no constant form at all,
/// which is the same exclusion compile-time evaluation already imposes.
#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize, Clone)]
pub enum Constant {
    // One form for all integers: the small/big split is a runtime-representation
    // concern, applied where a constant becomes a `Value` (`handle_constant`).
    #[serde(rename = "int", with = "decimal_bigint")]
    Integer(BigInt),
    #[serde(rename = "bin", with = "base64_bytes")]
    Binary(Vec<u8>),
    /// `id` is the tuple id an `Opcode::Tuple` operand would carry — the same table, the
    /// same remap. A field-less tuple needs no constant (`Payload::shared` interns the
    /// empty payload, so building one is already a refcount bump).
    #[serde(rename = "tuple")]
    Tuple { id: usize, fields: Vec<usize> },
    /// A closure: `id` indexes the function table, `captures` are its capture values.
    #[serde(rename = "fn")]
    Function { id: usize, captures: Vec<usize> },
    /// A builtin, bare or instantiated — `register_builtin_instantiated` folds the type
    /// argument into the id, so materialisation reads it back off the builtin's table entry
    /// rather than storing it twice.
    #[serde(rename = "builtin")]
    Builtin { id: usize },
    /// Annotations over another constant. A separate node rather than a field on each
    /// carrier: one variant serves tuples, functions and builtins alike, and the
    /// *unannotated* carrier stays a slot of its own that other uses can share. `entries`
    /// is `(annotation key id, constant index)`, sorted by key id so that the interning
    /// digest does not depend on attach order.
    #[serde(rename = "anno")]
    Annotated {
        value: usize,
        entries: Vec<(usize, usize)>,
    },
}

impl Constant {
    /// The constant indices this one references. The edge set of the graph: reclamation
    /// closes over it, and the linker topologically sorts by it.
    pub fn children(&self) -> Vec<usize> {
        match self {
            Constant::Integer(_) | Constant::Binary(_) | Constant::Builtin { .. } => Vec::new(),
            Constant::Tuple { fields, .. } => fields.clone(),
            Constant::Function { captures, .. } => captures.clone(),
            Constant::Annotated { value, entries } => std::iter::once(*value)
                .chain(entries.iter().map(|(_, index)| *index))
                .collect(),
        }
    }

    /// The function index a closure constant names, for the linker's dependency edges.
    pub fn function(&self) -> Option<usize> {
        match self {
            Constant::Function { id, .. } => Some(*id),
            _ => None,
        }
    }

    /// The same constant with every table reference rewritten through `remaps`. Shallow —
    /// children are rewritten as indices, not walked.
    pub fn remap_ids(self, remaps: &IdRemaps) -> Constant {
        let map_child = |index: usize| IdRemaps::map(&remaps.constants, "constant", index);
        match self {
            Constant::Integer(_) | Constant::Binary(_) => self,
            Constant::Tuple { id, fields } => Constant::Tuple {
                id: IdRemaps::map(&remaps.tuples, "tuple", id),
                fields: fields.into_iter().map(map_child).collect(),
            },
            Constant::Function { id, captures } => Constant::Function {
                id: IdRemaps::map(&remaps.functions, "function", id),
                captures: captures.into_iter().map(map_child).collect(),
            },
            Constant::Builtin { id } => Constant::Builtin {
                id: IdRemaps::map(&remaps.builtins, "builtin", id),
            },
            Constant::Annotated { value, entries } => Constant::Annotated {
                value: map_child(value),
                // Key ids are remapped, so the sort that keeps the digest canonical has to
                // be re-established rather than assumed to survive.
                entries: {
                    let mut entries: Vec<(usize, usize)> = entries
                        .into_iter()
                        .map(|(key, index)| {
                            (
                                IdRemaps::map(&remaps.annotation_keys, "annotation key", key),
                                map_child(index),
                            )
                        })
                        .collect();
                    entries.sort_by_key(|(key, _)| *key);
                    entries
                },
            },
        }
    }
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
/// into a session.
///
/// Each table is **total** over the ids it will be asked about: a caller adds an entry per
/// row it links, and finishes filling a table before anything consults it. A miss is a
/// caller bug and [`IdRemaps::map`] says so.
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
    /// Rewrite one id through `table`.
    ///
    /// A remap is **total** over the ids it will be asked about: whoever builds one adds an
    /// entry per row it links, and extraction's `verify` checks that a unit references
    /// nothing outside its own tables. So a miss is a bug in the caller, and this says so
    /// rather than answering the id unchanged.
    ///
    /// Passing the id through was the old behaviour, and it is the worst available answer:
    /// an unmapped id is not an absent one, it is a *plausible* one naming whatever else
    /// happens to sit at that index — another module's function, another module's
    /// provenance site. That produces a program that links, runs, and is quietly wrong,
    /// which is exactly how a site-table ordering bug once made every failure report
    /// against another module's source while every payload stayed correct.
    pub(crate) fn map(
        table: &std::collections::HashMap<usize, usize>,
        label: &str,
        id: usize,
    ) -> usize {
        *table.get(&id).unwrap_or_else(|| {
            panic!(
                "no {label} remap for id {id}: a remap must cover every id it is asked \
                 about, so this means the table was consulted before it was filled"
            )
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Serialize, Deserialize, Clone)]
pub struct Function {
    #[serde(with = "instruction_stream")]
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
            .map(|instruction| {
                // Fixed-width packing is what lets this be an operand rewrite: the opcode
                // is untouched and nothing downstream shifts.
                let (table, label) = match instruction.opcode() {
                    Opcode::Constant => (&remaps.constants, "constant"),
                    Opcode::Function => (&remaps.functions, "function"),
                    Opcode::Builtin => (&remaps.builtins, "builtin"),
                    Opcode::Tuple => (&remaps.tuples, "tuple"),
                    Opcode::IsType => (&remaps.types, "type"),
                    Opcode::GetNamed => (&remaps.field_names, "field name"),
                    Opcode::Annotate | Opcode::GetAnnotation => {
                        (&remaps.annotation_keys, "annotation key")
                    }
                    Opcode::Stamp => (&remaps.sites, "site"),
                    // Stack slots, jump offsets, positions and arities are not table ids;
                    // `Process` names a function whose index is already session-space.
                    _ => return instruction,
                };
                instruction.with_operand(IdRemaps::map(
                    table,
                    label,
                    instruction.operand() as usize,
                ))
            })
            .collect();

        Function {
            instructions,
            captures: self.captures,
            type_id: IdRemaps::map(&remaps.types, "type", self.type_id),
        }
    }

    /// Collect the code this function's instructions reference statically: function and
    /// constant table indices. The code-reclamation sweep closes over these; everything
    /// else an instruction carries (types, tuples, builtins, field names, sites) belongs
    /// to tables that are never reclaimed. `Process` operands are deliberately not
    /// collected — a pid's root-function index is identity, and a reclaimed stub keeps
    /// what identity tests read.
    pub fn collect_code_refs(
        &self,
        functions: &mut std::collections::HashSet<usize>,
        constants: &mut std::collections::HashSet<usize>,
    ) {
        for instruction in &self.instructions {
            match instruction.opcode() {
                Opcode::Constant => {
                    constants.insert(instruction.operand() as usize);
                }
                Opcode::Function => {
                    functions.insert(instruction.operand() as usize);
                }
                _ => {}
            }
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
    use super::instruction_stream::{write_uleb, zigzag};
    use super::{BigInt, Constant, Function, Instruction, Opcode, SiteKind};
    use base64::Engine as _;
    use std::str::FromStr as _;

    const BASE64_TEST: base64::engine::general_purpose::GeneralPurpose =
        base64::engine::general_purpose::STANDARD;

    #[test]
    fn site_kind_all_matches_indices() {
        for (position, kind) in SiteKind::ALL.iter().enumerate() {
            assert_eq!(kind.index(), position);
        }
    }

    /// The decode is an indexed load into `Opcode::ALL`, which is only correct while the
    /// array is in discriminant order.
    #[test]
    fn opcode_all_matches_discriminants() {
        for (position, opcode) in Opcode::ALL.iter().enumerate() {
            assert_eq!(*opcode as usize, position);
        }
    }

    #[test]
    fn instruction_packs_into_a_word() {
        assert_eq!(std::mem::size_of::<Instruction>(), 4);
    }

    #[test]
    fn operands_round_trip() {
        assert_eq!(Instruction::load(0).operand(), 0);
        assert_eq!(
            Instruction::load(Instruction::OPERAND_MAX).operand() as usize,
            Instruction::OPERAND_MAX
        );
        assert_eq!(Instruction::constant(1234).opcode(), Opcode::Constant);
        assert_eq!(Instruction::constant(1234).operand(), 1234);
    }

    /// Jump offsets are signed and sign-extend out of the same 24 bits.
    #[test]
    fn offsets_round_trip() {
        for offset in [0, 1, -1, 930, -833, (1 << 23) - 1, -(1 << 23)] {
            assert_eq!(Instruction::jump(offset).offset(), offset);
            assert_eq!(Instruction::jump_if(offset).offset(), offset);
        }
    }

    /// Every opcode must survive the serialized encoding, including both operand
    /// extremes and both jump directions — the cases a hand-written codec gets wrong.
    #[test]
    fn instruction_stream_round_trips() {
        let mut instructions: Vec<Instruction> = Opcode::ALL
            .iter()
            .map(|opcode| match opcode.operand_kind() {
                super::OperandKind::None => Instruction::bare(*opcode),
                super::OperandKind::Id => Instruction::with_id(*opcode, 0),
                super::OperandKind::Offset => Instruction::with_offset(*opcode, 0),
            })
            .collect();
        for id in [1usize, 127, 128, 4095, Instruction::OPERAND_MAX] {
            instructions.push(Instruction::constant(id));
            instructions.push(Instruction::load(id));
        }
        for offset in [1, -1, 63, -64, 930, -833, (1 << 23) - 1, -(1 << 23)] {
            instructions.push(Instruction::jump(offset));
            instructions.push(Instruction::jump_if(offset));
        }

        let function = Function {
            instructions: instructions.clone(),
            captures: 3,
            type_id: 7,
        };
        let json = serde_json::to_string(&function).expect("serialize");
        let restored: Function = serde_json::from_str(&json).expect("deserialize");
        assert_eq!(restored.instructions, instructions);
        assert_eq!(restored.captures, 3);
        assert_eq!(restored.type_id, 7);
    }

    #[test]
    fn empty_instruction_stream_round_trips() {
        let function = Function {
            instructions: vec![],
            captures: 0,
            type_id: 0,
        };
        let json = serde_json::to_string(&function).expect("serialize");
        let restored: Function = serde_json::from_str(&json).expect("deserialize");
        assert!(restored.instructions.is_empty());
    }

    #[test]
    fn constants_round_trip() {
        let constants = vec![
            Constant::Integer(BigInt::from(0)),
            Constant::Integer(BigInt::from(-1)),
            Constant::Integer(BigInt::from(i64::MIN)),
            // Past 2^53, where a JSON number would silently lose precision.
            Constant::Integer(BigInt::from_str("123456789012345678901234567890").unwrap()),
            Constant::Binary(vec![]),
            Constant::Binary(vec![0x00, 0xff, 0x68, 0x69]),
            Constant::Binary((0..=255).collect()),
            Constant::Tuple {
                id: 4,
                fields: vec![0, 1],
            },
            Constant::Tuple {
                id: 0,
                fields: vec![],
            },
            Constant::Function {
                id: 12,
                captures: vec![7],
            },
            Constant::Builtin { id: 3 },
            Constant::Annotated {
                value: 7,
                entries: vec![(1, 4), (2, 5)],
            },
        ];
        let json = serde_json::to_string(&constants).expect("serialize");
        let restored: Vec<Constant> = serde_json::from_str(&json).expect("deserialize");
        assert_eq!(restored, constants);
    }

    /// Remapping is shallow: children move as indices, and annotation entries re-sort
    /// under their new key ids so the interning digest stays canonical.
    #[test]
    fn composite_constants_remap() {
        let mut remaps = super::IdRemaps::default();
        remaps
            .constants
            .extend([(0, 10), (1, 11), (4, 14), (5, 15)]);
        remaps.tuples.insert(4, 40);
        remaps.functions.insert(12, 120);
        remaps.builtins.insert(3, 30);
        remaps.annotation_keys.extend([(1, 9), (2, 8)]);

        assert_eq!(
            Constant::Tuple {
                id: 4,
                fields: vec![0, 1]
            }
            .remap_ids(&remaps),
            Constant::Tuple {
                id: 40,
                fields: vec![10, 11]
            }
        );
        assert_eq!(
            Constant::Function {
                id: 12,
                captures: vec![0]
            }
            .remap_ids(&remaps),
            Constant::Function {
                id: 120,
                captures: vec![10]
            }
        );
        assert_eq!(
            Constant::Builtin { id: 3 }.remap_ids(&remaps),
            Constant::Builtin { id: 30 }
        );
        assert_eq!(
            Constant::Annotated {
                value: 0,
                entries: vec![(1, 4), (2, 5)]
            }
            .remap_ids(&remaps),
            Constant::Annotated {
                value: 10,
                entries: vec![(8, 15), (9, 14)]
            }
        );
    }

    /// A corrupt stream must be a deserialization error, never a panic — the range checks
    /// in `with_id` and `with_offset` are assertions, so the decoder has to reject before
    /// constructing. This runs wherever a unit is deserialized, which for a host is
    /// *before* it can validate anything.
    #[test]
    fn malformed_instruction_streams_are_rejected() {
        let case = |encoded: &str| {
            serde_json::from_str::<Function>(&format!(
                r#"{{"instructions":"{encoded}","captures":0,"type_id":0}}"#
            ))
        };
        let stream = |bytes: &[u8]| BASE64_TEST.encode(bytes);
        let jump = Opcode::Jump as u8;
        assert!(case("!!!not base64!!!").is_err());
        // Opcode 200 does not exist.
        assert!(case(&stream(&[200u8])).is_err());
        // `Constant` (opcode 0) with its operand missing.
        assert!(case(&stream(&[0u8])).is_err());
        // An operand past the 24-bit field.
        assert!(case(&stream(&[0u8, 0x80, 0x80, 0x80, 0x80, 0x01])).is_err());
        // A jump with its offset missing.
        assert!(case(&stream(&[jump])).is_err());
        // Jump offsets past the field, in both directions (the encoding is zigzag, so
        // an odd value is a backward jump).
        let mut forward = vec![jump];
        write_uleb(zigzag(Instruction::OFFSET_MAX) + 2, &mut forward);
        assert!(case(&stream(&forward)).is_err());
        let mut backward = vec![jump];
        write_uleb(zigzag(Instruction::OFFSET_MIN) + 2, &mut backward);
        assert!(case(&stream(&backward)).is_err());
        // A zigzag value past 32 bits, which `unzigzag` would truncate into range.
        let mut truncating = vec![jump];
        write_uleb((1u64 << 33) | zigzag(4), &mut truncating);
        assert!(case(&stream(&truncating)).is_err());
        // The extremes themselves still decode.
        for offset in [Instruction::OFFSET_MIN, Instruction::OFFSET_MAX] {
            let mut bytes = vec![jump];
            write_uleb(zigzag(offset), &mut bytes);
            let function = case(&stream(&bytes)).expect("an in-range offset decodes");
            assert_eq!(function.instructions[0].offset(), offset);
        }
    }

    /// The two normalising constructors are what keep the encoding canonical.
    #[test]
    fn constructors_normalise() {
        assert_eq!(Instruction::pick(0), Instruction::duplicate());
        assert_eq!(Instruction::pick(1).opcode(), Opcode::Pick);
        assert_eq!(Instruction::tuple(super::NIL), Instruction::nil());
        assert_eq!(Instruction::tuple(super::OK), Instruction::ok());
        assert_eq!(Instruction::tuple(2).opcode(), Opcode::Tuple);
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
/// A table index (constants, functions, types, tuples, builtins, field names, annotation
/// keys, sites) or a stack/frame slot, as an [`Instruction`] carries it. Instructions are
/// the largest static table a program holds — a live-view app is ~60k of them, replicated
/// per worker — so an instruction is packed into a single `u32`, and 24 bits is what is
/// left for the operand once the opcode takes a byte. Real programs stay four orders of
/// magnitude below that ceiling (the largest operand across the example suite is ~3,000),
/// and a denser stream also means more instructions per cache line in the dispatch loop.
pub type Id = u32;

/// A relative jump, in instructions, counted from the instruction *after* the jump — so
/// `Jump(0)` is a no-op that falls through. Stored sign-extended in the same 24 bits as
/// an [`Id`], giving a range of ±8M instructions.
pub type Offset = i32;

/// What an [`Instruction`] does. The operand's meaning is per-opcode and documented on
/// each constructor; opcodes with no operand ignore it (and carry zero).
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Opcode {
    Constant,
    Pop,
    Duplicate,
    Pick,
    Rotate,
    Reset,
    Load,
    Store,
    Tuple,
    Nil,
    Ok,
    GetPositional,
    GetNamed,
    IsType,
    Jump,
    JumpIf,
    Call,
    TailCall,
    Recurse,
    Function,
    Builtin,
    Equal,
    Not,
    Annotate,
    GetAnnotation,
    Stamp,
    Spawn,
    Self_,
    Select,
    Process,
    State,
    Reclaimed,
    Squash,
}

/// What an opcode's operand field means — the one place that knows, so the disassembler
/// and the serialized encoding cannot disagree about it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperandKind {
    /// No operand; the field is zero.
    None,
    /// A table index or a stack/local slot.
    Id,
    /// A relative jump.
    Offset,
}

impl Opcode {
    /// What this opcode's operand field means.
    pub fn operand_kind(self) -> OperandKind {
        match self {
            Opcode::Jump | Opcode::JumpIf => OperandKind::Offset,
            Opcode::Pop
            | Opcode::Duplicate
            | Opcode::Store
            | Opcode::Nil
            | Opcode::Ok
            | Opcode::Call
            | Opcode::TailCall
            | Opcode::Recurse
            | Opcode::Equal
            | Opcode::Not
            | Opcode::Spawn
            | Opcode::Self_
            | Opcode::Select
            | Opcode::State
            | Opcode::Reclaimed => OperandKind::None,
            _ => OperandKind::Id,
        }
    }

    /// Every opcode, in discriminant order — `ALL[op as usize] == op` (asserted in tests),
    /// which is what makes the decode below a single indexed load.
    pub const ALL: [Opcode; 33] = [
        Opcode::Constant,
        Opcode::Pop,
        Opcode::Duplicate,
        Opcode::Pick,
        Opcode::Rotate,
        Opcode::Reset,
        Opcode::Load,
        Opcode::Store,
        Opcode::Tuple,
        Opcode::Nil,
        Opcode::Ok,
        Opcode::GetPositional,
        Opcode::GetNamed,
        Opcode::IsType,
        Opcode::Jump,
        Opcode::JumpIf,
        Opcode::Call,
        Opcode::TailCall,
        Opcode::Recurse,
        Opcode::Function,
        Opcode::Builtin,
        Opcode::Equal,
        Opcode::Not,
        Opcode::Annotate,
        Opcode::GetAnnotation,
        Opcode::Stamp,
        Opcode::Spawn,
        Opcode::Self_,
        Opcode::Select,
        Opcode::Process,
        Opcode::State,
        Opcode::Reclaimed,
        Opcode::Squash,
    ];
}

/// One instruction, packed into a word: an 8-bit opcode and a 24-bit operand.
///
/// Fixed width is deliberate. Every pass that rewrites table ids — the linker loading a
/// unit or a module artifact into a session — rewrites operands in place, which a
/// variable-length encoding would turn into a re-assembly of the whole stream.
///
/// Build one through the named constructors, never by hand: [`Instruction::tuple`] and
/// [`Instruction::pick`] normalise their operands, and the rest keep the debug-build
/// range check in one place.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Instruction(u32);

impl Instruction {
    const OPERAND_BITS: u32 = 24;
    const OPERAND_MASK: u32 = (1 << Self::OPERAND_BITS) - 1;

    /// The widest table id or slot an instruction can name.
    pub const OPERAND_MAX: usize = Self::OPERAND_MASK as usize;
    /// The widest relative jump an instruction can carry, the operand field reading as
    /// signed. A decoder checks a stream against these before constructing.
    pub const OFFSET_MIN: Offset = -(1 << (Self::OPERAND_BITS - 1));
    pub const OFFSET_MAX: Offset = (1 << (Self::OPERAND_BITS - 1)) - 1;

    fn new(opcode: Opcode, operand: u32) -> Instruction {
        Instruction(((opcode as u32) << Self::OPERAND_BITS) | (operand & Self::OPERAND_MASK))
    }

    /// An opcode that takes no operand.
    fn bare(opcode: Opcode) -> Instruction {
        Instruction::new(opcode, 0)
    }

    /// An opcode carrying a table id or slot.
    ///
    /// The range check is unconditional, not a `debug_assert`: overflowing the field
    /// would silently truncate to a *valid-looking* id pointing at the wrong table entry,
    /// and instructions are built once at compile time, never in the dispatch loop, so
    /// the check costs nothing that matters.
    fn with_id(opcode: Opcode, id: usize) -> Instruction {
        assert!(
            id <= Self::OPERAND_MAX,
            "instruction operand {id} exceeds {} bits — this program is too large to encode",
            Self::OPERAND_BITS
        );
        Instruction::new(opcode, id as u32)
    }

    /// An opcode carrying a relative jump. Range-checked like [`Self::with_id`].
    fn with_offset(opcode: Opcode, offset: Offset) -> Instruction {
        assert!(
            (Self::OFFSET_MIN..=Self::OFFSET_MAX).contains(&offset),
            "jump offset {offset} exceeds {} bits — this function is too large to encode",
            Self::OPERAND_BITS
        );
        Instruction::new(opcode, (offset as u32) & Self::OPERAND_MASK)
    }

    /// What this instruction does.
    pub fn opcode(self) -> Opcode {
        Opcode::ALL[(self.0 >> Self::OPERAND_BITS) as usize]
    }

    /// The table id or slot this instruction carries. Zero for an operand-less opcode.
    pub fn operand(self) -> Id {
        self.0 & Self::OPERAND_MASK
    }

    /// The relative jump this instruction carries, sign-extended from its 24 bits.
    pub fn offset(self) -> Offset {
        ((self.0 << (32 - Self::OPERAND_BITS)) as i32) >> (32 - Self::OPERAND_BITS)
    }

    /// The same instruction with its operand replaced — how the id-remapping passes
    /// rewrite a table reference without decoding the opcode.
    pub fn with_operand(self, id: usize) -> Instruction {
        Instruction::with_id(self.opcode(), id)
    }

    /// Push the constant at this index.
    pub fn constant(constant_id: usize) -> Instruction {
        Instruction::with_id(Opcode::Constant, constant_id)
    }

    /// Discard the top of the stack.
    pub fn pop() -> Instruction {
        Instruction::bare(Opcode::Pop)
    }

    /// Push a copy of the top of the stack.
    pub fn duplicate() -> Instruction {
        Instruction::bare(Opcode::Duplicate)
    }

    /// Push a copy of the value `depth` slots below the top. Depth zero is
    /// [`Instruction::duplicate`], which is much the commonest case and has its own
    /// opcode; normalising here is what keeps `Pick` non-zero.
    pub fn pick(depth: usize) -> Instruction {
        match depth {
            0 => Instruction::duplicate(),
            other => Instruction::with_id(Opcode::Pick, other),
        }
    }

    /// Move the value `count - 1` slots below the top to the top, sliding the rest down.
    /// `Rotate(2)` swaps the top pair.
    pub fn rotate(count: usize) -> Instruction {
        debug_assert!(count >= 2, "Rotate({count}) does nothing");
        Instruction::with_id(Opcode::Rotate, count)
    }

    /// Drop every local from `slot` upward, leaving the frame's earlier bindings — how a
    /// block's scope ends.
    pub fn reset(slot: usize) -> Instruction {
        Instruction::with_id(Opcode::Reset, slot)
    }

    /// Push the frame-relative local at this slot.
    pub fn load(slot: usize) -> Instruction {
        Instruction::with_id(Opcode::Load, slot)
    }

    /// Move the top of the stack into the frame's next local slot.
    pub fn store() -> Instruction {
        Instruction::bare(Opcode::Store)
    }

    /// Pop this tuple type's arity worth of fields (the topmost is its last field) and
    /// push the tuple. Nil and `Ok` are specialised to [`Instruction::nil`] and
    /// [`Instruction::ok`]: together they are the majority of all tuple construction, and
    /// neither pops anything.
    pub fn tuple(tuple_id: usize) -> Instruction {
        match tuple_id {
            NIL => Instruction::nil(),
            OK => Instruction::ok(),
            other => Instruction::with_id(Opcode::Tuple, other),
        }
    }

    /// Push nil.
    pub fn nil() -> Instruction {
        Instruction::bare(Opcode::Nil)
    }

    /// Push `Ok`.
    pub fn ok() -> Instruction {
        Instruction::bare(Opcode::Ok)
    }

    /// Pop a tuple; push the field at this position. Emitted where the static type pins
    /// the field's position (a concrete tuple, or a union agreeing on one).
    pub fn get_positional(index: usize) -> Instruction {
        Instruction::with_id(Opcode::GetPositional, index)
    }

    /// Pop a tuple; push the field with this name, its offset resolved against the
    /// tuple's own type at runtime. Emitted where the static type doesn't pin a position
    /// — a partial type's field, or a union whose members carry it at different offsets.
    pub fn get_named(name_id: usize) -> Instruction {
        Instruction::with_id(Opcode::GetNamed, name_id)
    }

    /// Pop a value; push `Ok` if it inhabits this type, nil otherwise.
    pub fn is_type(type_id: usize) -> Instruction {
        Instruction::with_id(Opcode::IsType, type_id)
    }

    /// Jump unconditionally.
    pub fn jump(offset: Offset) -> Instruction {
        Instruction::with_offset(Opcode::Jump, offset)
    }

    /// Pop a value; jump if it is not nil.
    pub fn jump_if(offset: Offset) -> Instruction {
        Instruction::with_offset(Opcode::JumpIf, offset)
    }

    /// Call the callable on top of the stack with the argument beneath it.
    pub fn call() -> Instruction {
        Instruction::bare(Opcode::Call)
    }

    /// Tail-call the callable on top of the stack with the argument beneath it (`^f`,
    /// `^~`), replacing the current frame.
    pub fn tail_call() -> Instruction {
        Instruction::bare(Opcode::TailCall)
    }

    /// Re-enter the current frame with the argument on top of the stack (`^`), keeping
    /// its function and captures. Shares no stack shape with [`Instruction::tail_call`],
    /// which is why the two are separate opcodes.
    pub fn recurse() -> Instruction {
        Instruction::bare(Opcode::Recurse)
    }

    /// Pop this function's captures; push the closure.
    pub fn function(function_id: usize) -> Instruction {
        Instruction::with_id(Opcode::Function, function_id)
    }

    /// Push a builtin value. A type-consuming builtin's explicit type argument
    /// (`__data_decode__<'t>`) belongs to the *entry* this names rather than to the
    /// instruction: each instantiation is its own builtin-table entry.
    pub fn builtin(builtin_id: usize) -> Instruction {
        Instruction::with_id(Opcode::Builtin, builtin_id)
    }

    /// Pop two values; push `Ok` if they are structurally equal, nil otherwise. The
    /// result is a truth flag rather than the compared value, so equal nils stay
    /// distinguishable from a failed comparison.
    pub fn equal() -> Instruction {
        Instruction::bare(Opcode::Equal)
    }

    /// Pop a value; push `Ok` if it is nil, nil otherwise.
    pub fn not() -> Instruction {
        Instruction::bare(Opcode::Not)
    }

    /// Pop an annotation value, then a tuple/function carrier; push the carrier with the
    /// annotation attached under this key (copy-on-annotate).
    pub fn annotate(key: usize) -> Instruction {
        Instruction::with_id(Opcode::Annotate, key)
    }

    /// Pop a carrier; push its annotation under this key, or nil. Total: a value that
    /// cannot carry annotations answers nil too. The checked form (`x:('t)key`) compiles
    /// to this followed by a test against the expected shape.
    pub fn get_annotation(key: usize) -> Instruction {
        Instruction::with_id(Opcode::GetAnnotation, key)
    }

    /// Debug builds only: if the top of the stack is a nil result not yet carrying an
    /// `origin` annotation, stamp it with this site's provenance. Fresh-only, so a
    /// propagating failure keeps its original site; a no-op on anything else.
    pub fn stamp(site_id: usize) -> Instruction {
        Instruction::with_id(Opcode::Stamp, site_id)
    }

    /// Pop a function value, then an argument; spawn a process and push its pid.
    pub fn spawn() -> Instruction {
        Instruction::bare(Opcode::Spawn)
    }

    /// Push the running process's own pid.
    pub fn self_() -> Instruction {
        Instruction::bare(Opcode::Self_)
    }

    /// Pop a source or tuple of sources; race them and push the winner's result.
    pub fn select() -> Instruction {
        Instruction::bare(Opcode::Select)
    }

    /// Pop an integer process id; push a process value with that id and this root
    /// function index. REPL-only: it is how a session names a pid it has already seen
    /// (`@1`). The id arrives as an ordinary constant rather than a second operand, so it
    /// rides the constants table that linking already remaps.
    pub fn process(function_id: usize) -> Instruction {
        Instruction::with_id(Opcode::Process, function_id)
    }

    /// Pop a process value; push its current state (`?p`). No runtime test — the state
    /// type is statically known.
    pub fn state() -> Instruction {
        Instruction::bare(Opcode::State)
    }

    /// A point execution never reaches, which aborts the process loudly if it does rather than
    /// return garbage: the body of a function whose code was reclaimed (liveness said nothing
    /// could call it; written into stubbed slots by `Program::reclaim_code`), or the point after
    /// a call that never returns (`__panic__`), which marks the flow as ending there.
    pub fn reclaimed() -> Instruction {
        Instruction::bare(Opcode::Reclaimed)
    }

    /// Keep the top of the stack and drop the `count` values beneath it — how a failure leaves
    /// a half-built expression (a tuple's earlier fields, a call's callee) for its step, with
    /// only the failing nil where the step's value belongs.
    pub fn squash(count: usize) -> Instruction {
        Instruction::with_id(Opcode::Squash, count)
    }
}

impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let opcode = self.opcode();
        match opcode.operand_kind() {
            OperandKind::None => write!(f, "{opcode:?}"),
            OperandKind::Id => write!(f, "{opcode:?}({})", self.operand()),
            OperandKind::Offset => write!(f, "{opcode:?}({})", self.offset()),
        }
    }
}

/// A function's instruction stream, serialized as base64 over a compact byte encoding:
/// one opcode byte, then the operand as LEB128 (zigzagged for a jump) where the opcode
/// takes one.
///
/// The *stored* form is variable-length even though the in-memory one is not. The reason
/// the in-memory representation is fixed-width — that every id-remapping pass rewrites
/// operands in place — does not apply here, because deserializing rebuilds the vector
/// anyway. So the wire is free to be as small as it likes.
mod instruction_stream {
    use super::{Instruction, Opcode, OperandKind};
    use base64::Engine as _;
    use serde::{Deserialize, Deserializer, Serializer, de::Error as _};

    const BASE64: base64::engine::general_purpose::GeneralPurpose =
        base64::engine::general_purpose::STANDARD;

    pub(super) fn write_uleb(mut value: u64, out: &mut Vec<u8>) {
        while value >= 0x80 {
            out.push((value as u8) | 0x80);
            value >>= 7;
        }
        out.push(value as u8);
    }

    fn read_uleb(bytes: &[u8], cursor: &mut usize) -> Option<u64> {
        let mut value: u64 = 0;
        for shift in (0..64).step_by(7) {
            let byte = *bytes.get(*cursor)?;
            *cursor += 1;
            value |= u64::from(byte & 0x7f) << shift;
            if byte & 0x80 == 0 {
                return Some(value);
            }
        }
        None
    }

    /// Jump offsets are small in both directions, so zigzag keeps a backward jump one byte.
    pub(super) fn zigzag(offset: i32) -> u64 {
        ((offset << 1) ^ (offset >> 31)) as u32 as u64
    }

    fn unzigzag(value: u64) -> i32 {
        ((value >> 1) as i32) ^ -((value & 1) as i32)
    }

    pub fn serialize<S: Serializer>(
        instructions: &[Instruction],
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        // Most instructions encode to one or two bytes; sizing for two avoids regrowth.
        let mut bytes = Vec::with_capacity(instructions.len() * 2);
        for instruction in instructions {
            let opcode = instruction.opcode();
            bytes.push(opcode as u8);
            match opcode.operand_kind() {
                OperandKind::None => {}
                OperandKind::Id => write_uleb(u64::from(instruction.operand()), &mut bytes),
                OperandKind::Offset => write_uleb(zigzag(instruction.offset()), &mut bytes),
            }
        }
        serializer.serialize_str(&BASE64.encode(&bytes))
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Vec<Instruction>, D::Error> {
        let encoded = <&str>::deserialize(deserializer)?;
        let bytes = BASE64.decode(encoded).map_err(D::Error::custom)?;

        let mut instructions = Vec::new();
        let mut cursor = 0;
        while cursor < bytes.len() {
            let byte = bytes[cursor];
            cursor += 1;
            let opcode = *Opcode::ALL
                .get(byte as usize)
                .ok_or_else(|| D::Error::custom(format!("unknown opcode {byte}")))?;
            let truncated = || D::Error::custom("instruction stream ends mid-operand");
            instructions.push(match opcode.operand_kind() {
                OperandKind::None => Instruction::bare(opcode),
                OperandKind::Id => {
                    let id = read_uleb(&bytes, &mut cursor).ok_or_else(truncated)?;
                    if id > Instruction::OPERAND_MAX as u64 {
                        return Err(D::Error::custom(format!("operand {id} out of range")));
                    }
                    Instruction::with_id(opcode, id as usize)
                }
                OperandKind::Offset => {
                    let encoded = read_uleb(&bytes, &mut cursor).ok_or_else(truncated)?;
                    let offset = unzigzag(encoded);
                    // `unzigzag` truncates past 32 bits and the field holds 24, so
                    // round-tripping the encoding is what catches both — the range check
                    // in `with_offset` is an assertion, and a corrupt stream must be a
                    // deserialization error rather than a panic.
                    if zigzag(offset) != encoded
                        || !(Instruction::OFFSET_MIN..=Instruction::OFFSET_MAX).contains(&offset)
                    {
                        return Err(D::Error::custom(format!(
                            "jump offset out of range (encoded {encoded})"
                        )));
                    }
                    Instruction::with_offset(opcode, offset)
                }
            });
        }
        Ok(instructions)
    }
}

/// A binary constant, serialized as base64 rather than a JSON array of byte numbers.
mod base64_bytes {
    use base64::Engine as _;
    use serde::{Deserialize, Deserializer, Serializer, de::Error as _};

    const BASE64: base64::engine::general_purpose::GeneralPurpose =
        base64::engine::general_purpose::STANDARD;

    pub fn serialize<S: Serializer>(bytes: &[u8], serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&BASE64.encode(bytes))
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Vec<u8>, D::Error> {
        let encoded = <&str>::deserialize(deserializer)?;
        BASE64.decode(encoded).map_err(D::Error::custom)
    }
}

/// An integer constant, serialized as a decimal string. Integers are arbitrary precision,
/// so a JSON number would not round-trip past 2^53; base64 over the magnitude bytes would
/// round-trip but costs *more* than decimal for the small values that dominate.
mod decimal_bigint {
    use num_bigint::BigInt;
    use serde::{Deserialize, Deserializer, Serializer, de::Error as _};
    use std::str::FromStr as _;

    pub fn serialize<S: Serializer>(value: &BigInt, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&value.to_string())
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<BigInt, D::Error> {
        let text = <&str>::deserialize(deserializer)?;
        BigInt::from_str(text).map_err(D::Error::custom)
    }
}
