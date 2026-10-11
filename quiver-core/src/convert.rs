//! Route planning for `%codec.convert<'t, 's>`: converting a value of `'s` to `'t` through
//! the functions it is given (its **edges**) and the structural conversions every type has.
//!
//! A plan is a graph of nodes, one per `(source, target)` type pair met, each holding the
//! **alternatives** for getting from one to the other, in the order they are tried:
//!
//! - **fit**: a value already belonging to the target passes through unchanged;
//! - **forward**: an edge whose parameter the value fits is applied, and its answer converted
//!   on to the target;
//! - **backward**: the value is first converted to an edge's parameter, so that the edge's
//!   answer reaches the target (how a tuple is reshaped into what an encoding function takes);
//! - **tuple**: a tuple converts to a target tuple of the same name, labels and positions,
//!   field by field;
//! - **shapes**: a tuple is read from and written as the neutral shapes every format shares — a
//!   record (`'%codec.record<'d>`) for labelled fields, a list for positional ones, text for a
//!   symbol, and its content for a newtype — told apart, among a union's variants, by a tag.
//!
//! The shapes are read and written as the conversion's options (`keys:`, `tag:`, `shapes:`, see
//! `options`) say. They apply only to the program's own tuples: one an edge takes or answers,
//! or holds inside, is the edges' — a format's document, or a type with a representation of its
//! own — and only the edges convert it.
//!
//! The executor walks the plan guided by the value, depth first: the first success of each
//! step is kept, and a failure anywhere in an alternative falls through to the next one. An
//! edge answering nil declines.
//!
//! The plan is made when the instantiation is registered — at compile time, and again at link
//! time from the same type arguments — because generic edges are instantiated against the
//! types they meet, and the runtime can't create types. Planning on types also prunes every
//! alternative that can never succeed, and finds a conversion with no route at all.
//!
//! Termination: an edge is never applied twice in a chain — the edges applied since the last
//! structural step — so chains are finite, and a structural step moves to the value's fields.
//! A generic edge can answer a type no node has yet (`#<'t>'t -> Box['t]`), so the plan keys a
//! node by the generic edges in its chain, and never plans one twice in a chain. A concrete
//! edge answers a type the plan already knows, so its applications share nodes, and the graph
//! may cycle through them: the walk keeps each node's chain at run time, and skips an edge
//! already in it. Keying nodes by every edge in the chain would plan each pair once per
//! subset of the edges applicable there — exponential in the edges supplied.

mod options;

use std::collections::{HashMap, HashSet};

use serde::{Deserialize, Serialize};

pub use options::{OptionPath, PathStep, Text};
use options::{Options, Style, Tag};

use crate::binders::close_against;
use crate::program::Program;
use crate::types::{TupleTypeInfo, Type, TypeLookup, is_compatible, types_overlap};

/// The most nodes a plan may have: past it, planning gives up rather than run away on
/// edges that keep producing new types.
const MAX_NODES: usize = 20_000;

/// A conversion's route graph, from its `root` node.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Plan {
    pub root: usize,
    pub nodes: Vec<Node>,
    /// Why the conversion can't be planned (no route at all, or a malformed edge tuple): a
    /// compile error at the call.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
}

/// One `(source, target)` pair: the target, which fit tests the value against, and the
/// alternatives that can reach it.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Node {
    pub target: usize,
    pub alternatives: Vec<Alternative>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Alternative {
    /// The value belongs to the node's target.
    Fit,
    /// The value belongs to `parameter` (the edge's, instantiated): apply the edge, and convert
    /// its answer with node `then`.
    Forward {
        edge: usize,
        parameter: usize,
        then: usize,
    },
    /// Convert the value with node `prepare` (to the edge's parameter), apply the edge, and
    /// convert its answer with node `then`.
    Backward {
        edge: usize,
        prepare: usize,
        then: usize,
    },
    /// The value is a tuple shaped like `label` (name, labels, arity): convert each field with
    /// its node, and build the result as `label`.
    Tuple { label: usize, fields: Vec<usize> },
    /// The value is a record: build `label`, a tuple of labelled fields, each from the entry
    /// under its key — when it carries `tag`, an entry naming the member.
    FromRecord {
        label: usize,
        fields: Vec<RecordField>,
        tag: Option<TagEntry>,
    },
    /// The value is a tuple shaped like `source`, of labelled fields: write it as a record, each
    /// field an entry under its key, after `tag`'s entry naming the member.
    ToRecord {
        source: usize,
        record: RecordShape,
        fields: Vec<(Text, usize)>,
        tag: Option<TagEntry>,
    },
    /// The value is a list as long as `label` has fields: build `label`, a tuple of positional
    /// fields, one from each element.
    FromList { label: usize, fields: Vec<usize> },
    /// The value is a tuple shaped like `source`, of positional fields: write it as a list.
    ToList {
        source: usize,
        list: ListShape,
        fields: Vec<usize>,
    },
    /// The value is the text `wire`: build `label`, a symbol (a named tuple without fields).
    FromText { label: usize, wire: Text },
    /// The value is the symbol `source`: write it as the text `wire`, a `text` tuple.
    ToText {
        source: usize,
        text: usize,
        wire: Text,
    },
    /// Convert the value with `inner` and wrap it in `label`, a tuple of one positional field:
    /// a newtype is its content. Wrapping leaves the value where it is, so it joins the chain,
    /// as an edge does, and is never repeated without a structural step between.
    Wrap { label: usize, inner: usize },
    /// The value is a tuple shaped like `source`, of one positional field: convert its content
    /// with `inner`.
    Unwrap { source: usize, inner: usize },
    /// The value is a record of one entry, under the member's name `wire`: convert the entry's
    /// value, the member's content, with `inner` (to `label`, the member).
    FromExternal {
        label: usize,
        wire: Text,
        inner: usize,
    },
    /// The value is a tuple shaped like `source`: write its content with `inner`, as a record of
    /// one entry under its name `wire`.
    ToExternal {
        source: usize,
        record: RecordShape,
        wire: Text,
        inner: usize,
    },
    /// The value is a record naming the member `wire` under the key `tag`: convert the entry
    /// under `content` with `inner` (to `label`, the member).
    FromAdjacent {
        label: usize,
        tag: TagEntry,
        content: Text,
        inner: usize,
    },
    /// The value is a tuple shaped like `source`: write it as a record naming it under `tag`,
    /// its content written with `inner` under `content`.
    ToAdjacent {
        source: usize,
        record: RecordShape,
        tag: TagEntry,
        content: Text,
        inner: usize,
    },
}

/// A tag's entry in a record: the member's name `wire` under `key`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TagEntry {
    pub key: Text,
    pub wire: Text,
}

/// A field a record is read into: its label's key, its node (from the entry's value), the
/// default it takes when the entry is absent, and whether it must otherwise be present — a field
/// whose type doesn't admit nil can't be absent.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RecordField {
    pub key: Text,
    pub node: usize,
    pub default: Option<OptionPath>,
    pub required: bool,
}

/// The tuples a list is built of: `Cons[element, tail]` and `Nil`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct ListShape {
    pub cons: usize,
    pub nil: usize,
}

/// The tuples a record (`'%codec.record<'d>`) is built of: `Record[entries]`, its list of
/// entries, each entry `[key, value]`, and the key's `Str`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct RecordShape {
    pub record: usize,
    pub list: ListShape,
    pub entry: usize,
    pub text: usize,
}

impl Alternative {
    /// The nodes the alternative continues with.
    fn successors(&self) -> Vec<usize> {
        match self {
            Alternative::Fit | Alternative::FromText { .. } | Alternative::ToText { .. } => vec![],
            Alternative::Forward { then, .. } => vec![*then],
            Alternative::Backward { prepare, then, .. } => vec![*prepare, *then],
            Alternative::Tuple { fields, .. }
            | Alternative::FromList { fields, .. }
            | Alternative::ToList { fields, .. } => fields.clone(),
            Alternative::FromRecord { fields, .. } => {
                fields.iter().map(|field| field.node).collect()
            }
            Alternative::ToRecord { fields, .. } => fields.iter().map(|&(_, node)| node).collect(),
            Alternative::Wrap { inner, .. }
            | Alternative::Unwrap { inner, .. }
            | Alternative::FromExternal { inner, .. }
            | Alternative::ToExternal { inner, .. }
            | Alternative::FromAdjacent { inner, .. }
            | Alternative::ToAdjacent { inner, .. } => vec![*inner],
        }
    }

    /// The alternative with each node it continues with renumbered.
    fn renumbered(&self, renumber: impl Fn(usize) -> usize) -> Alternative {
        let nodes = |nodes: &[usize]| nodes.iter().map(|&node| renumber(node)).collect();
        match self {
            Alternative::Fit => Alternative::Fit,
            &Alternative::Forward {
                edge,
                parameter,
                then,
            } => Alternative::Forward {
                edge,
                parameter,
                then: renumber(then),
            },
            &Alternative::Backward {
                edge,
                prepare,
                then,
            } => Alternative::Backward {
                edge,
                prepare: renumber(prepare),
                then: renumber(then),
            },
            Alternative::Tuple { label, fields } => Alternative::Tuple {
                label: *label,
                fields: nodes(fields),
            },
            Alternative::FromRecord { label, fields, tag } => Alternative::FromRecord {
                label: *label,
                fields: fields
                    .iter()
                    .map(|field| RecordField {
                        node: renumber(field.node),
                        ..field.clone()
                    })
                    .collect(),
                tag: tag.clone(),
            },
            Alternative::ToRecord {
                source,
                record,
                fields,
                tag,
            } => Alternative::ToRecord {
                source: *source,
                record: *record,
                fields: fields
                    .iter()
                    .map(|(key, node)| (key.clone(), renumber(*node)))
                    .collect(),
                tag: tag.clone(),
            },
            Alternative::FromList { label, fields } => Alternative::FromList {
                label: *label,
                fields: nodes(fields),
            },
            Alternative::ToList {
                source,
                list,
                fields,
            } => Alternative::ToList {
                source: *source,
                list: *list,
                fields: nodes(fields),
            },
            Alternative::FromText { .. } | Alternative::ToText { .. } => self.clone(),
            Alternative::FromExternal { label, wire, inner } => Alternative::FromExternal {
                label: *label,
                wire: wire.clone(),
                inner: renumber(*inner),
            },
            Alternative::ToExternal {
                source,
                record,
                wire,
                inner,
            } => Alternative::ToExternal {
                source: *source,
                record: *record,
                wire: wire.clone(),
                inner: renumber(*inner),
            },
            Alternative::FromAdjacent {
                label,
                tag,
                content,
                inner,
            } => Alternative::FromAdjacent {
                label: *label,
                tag: tag.clone(),
                content: content.clone(),
                inner: renumber(*inner),
            },
            Alternative::ToAdjacent {
                source,
                record,
                tag,
                content,
                inner,
            } => Alternative::ToAdjacent {
                source: *source,
                record: *record,
                tag: tag.clone(),
                content: content.clone(),
                inner: renumber(*inner),
            },
            &Alternative::Wrap { label, inner } => Alternative::Wrap {
                label,
                inner: renumber(inner),
            },
            &Alternative::Unwrap { source, inner } => Alternative::Unwrap {
                source,
                inner: renumber(inner),
            },
        }
    }
}

impl Plan {
    fn failed(error: String) -> Self {
        Plan {
            root: 0,
            nodes: vec![],
            error: Some(error),
        }
    }
}

/// An edge's type: its parameter and result, closed against the function type, and the ways
/// it may be used.
#[derive(Clone, Copy)]
struct Edge {
    parameter: usize,
    result: usize,
    generic: bool,
    use_: Use,
}

/// How an edge may be applied: to a value as it stands (an `Edge`'s `out`), aimed at a target
/// (its `in`), or either way when it is given bare. A generic edge is instantiated from the side
/// it is applied from — the value forward, the target backward — so an edge meant one way, such
/// as a decoder generic in the document it reads, would be instantiated nonsensically the other
/// way, and every such instantiation is a route the planner must explore. The direction is part
/// of the edges' type, so it survives wherever the edges are passed.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Use {
    Forward,
    Backward,
    Either,
}

/// An entry of a conversion's `using:`, by its type (see [`Planner::edge_functions`]).
enum Edges {
    Pair([usize; 2]),
    Nested(usize),
    Bare(usize),
}

/// Where an `Edge[out: f, in: g]` holds its two functions, by position: `out` takes a value of
/// the edge's type apart, and `in` builds one. The plan and the conversion it drives both read
/// pairs by this, so they agree on what is one — a tuple named `Edge` of exactly those labels.
pub(crate) fn edge_pair(info: &TupleTypeInfo) -> Option<[usize; 2]> {
    if info.name.as_deref() != Some("Edge") || info.fields.len() != 2 {
        return None;
    }
    let position = |label: &str| {
        info.fields
            .iter()
            .position(|(field, _)| field.as_deref() == Some(label))
    };
    Some([position("out")?, position("in")?])
}

/// Plan the conversion for type arguments `[target, source, edges]`, where `edges` is the type
/// of the tuple of functions supplied.
pub fn plan(type_arguments: &[usize], program: &mut Program) -> Plan {
    let &[target, source, edges, keys, tag, shapes] = type_arguments else {
        return Plan::failed(format!(
            "a conversion takes six type arguments (target, source, edges, keys, tag, shapes), \
             given {}",
            type_arguments.len()
        ));
    };
    let options = match Options::read(keys, tag, shapes, program) {
        Ok(options) => options,
        Err(error) => return Plan::failed(error),
    };
    let mut planner = Planner {
        program,
        edges: Vec::new(),
        claimed: Vec::new(),
        met: vec![false; options.shapes.len()],
        options,
        errors: Vec::new(),
        nodes: Vec::new(),
        memo: HashMap::new(),
        overflow: false,
    };
    if let Err(error) = planner.collect_edges(edges) {
        return Plan::failed(error);
    }
    let source = planner.strip(source);
    let target = planner.strip(target);
    let root = planner.node(source, target, &[]);
    if let Some(error) = planner.errors.first() {
        return Plan::failed(error.clone());
    }
    if let Some(unmet) = planner.met.iter().position(|met| !met) {
        return Plan::failed(format!(
            "`shapes:` configures {}, which the conversion never reads or writes",
            planner.options.shapes[unmet].name
        ));
    }
    if planner.overflow {
        return Plan::failed(format!(
            "the conversion from {} to {} needs more than {MAX_NODES} steps to plan",
            planner.describe(source),
            planner.describe(target)
        ));
    }
    let mut plan = planner.finish(root);
    if plan.nodes[plan.root].alternatives.is_empty() {
        plan.error = Some(format!(
            "no route from {} to {}",
            crate::format::format_type_by_id(&*program, source),
            crate::format::format_type_by_id(&*program, target)
        ));
    }
    plan
}

struct Planner<'a> {
    program: &'a mut Program,
    edges: Vec<Edge>,
    /// The named tuple types the edges take or answer, and those inside them, which are theirs to
    /// convert: a format's document (JSON's `Object[…]`, `True`, …), a type with a
    /// representation of its own (a dict, given its `entries` and `from`), or a type a program
    /// converts by hand. Each is held with the edges' type variables as `_`, so that it covers
    /// every instantiation. Structure is read from and written as the neutral shapes only for a
    /// tuple of none of these types.
    claimed: Vec<usize>,
    options: Options,
    /// Which of `options.shapes` the plan has met a tuple of.
    met: Vec<bool>,
    /// What is wrong with the options, found as the tuples they configure are met.
    errors: Vec<String>,
    nodes: Vec<Node>,
    /// Nodes by `(source, target, chain)`, the chain being the generic edges applied since the
    /// last structural step, sorted.
    memo: HashMap<(usize, usize, Vec<usize>), usize>,
    overflow: bool,
}

impl Planner<'_> {
    fn lookup(&self, type_id: usize) -> Type {
        self.program
            .lookup_type(type_id)
            .cloned()
            .expect("a planned type is in the table")
    }

    fn strip(&self, type_id: usize) -> usize {
        Type::strip_annotations(type_id, &*self.program)
    }

    fn describe(&self, type_id: usize) -> String {
        crate::format::format_type_by_id(&*self.program, type_id)
    }

    fn top(&mut self) -> usize {
        self.program.register_type(Type::Top)
    }

    fn collect_edges(&mut self, edges: usize) -> Result<(), String> {
        let edges = self.strip(edges);
        let Type::Tuple(tuple) = self.lookup(edges) else {
            return Err(format!(
                "a conversion's functions must be a tuple, found {}",
                self.describe(edges)
            ));
        };
        let fields = self
            .program
            .lookup_tuple(tuple)
            .expect("a tuple type's tuple is in the table")
            .fields
            .clone();
        for (_, field) in fields {
            match self.edge_functions(field) {
                Edges::Pair([out, into]) => {
                    let out = self.add_edge(out, Use::Forward)?;
                    let into = self.add_edge(into, Use::Backward)?;
                    self.check_pair(out, into)?;
                }
                Edges::Nested(edges) => self.collect_edges(edges)?,
                Edges::Bare(function) => {
                    self.add_edge(function, Use::Either)?;
                }
            }
        }
        Ok(())
    }

    /// What an entry of `using:` holds: an `Edge`'s two functions, `out` and `in`; an unnamed
    /// tuple of more entries (a format's edges beside a program's own); or one function, bare.
    fn edge_functions(&self, field: usize) -> Edges {
        let field = self.strip(field);
        if let Type::Tuple(tuple) = self.lookup(field) {
            let info = self
                .program
                .lookup_tuple(tuple)
                .expect("a tuple type's tuple is in the table");
            if let Some(positions) = edge_pair(info) {
                return Edges::Pair(positions.map(|position| self.strip(info.fields[position].1)));
            }
            if info.name.is_none() {
                return Edges::Nested(field);
            }
        }
        Edges::Bare(field)
    }

    /// Add the edge of type `callable`, used as `use_` says; its index among the edges.
    fn add_edge(&mut self, callable: usize, use_: Use) -> Result<usize, String> {
        let Type::Callable {
            parameter, result, ..
        } = self.lookup(callable)
        else {
            return Err(format!(
                "a conversion's functions must all be functions, bare or paired as \
                 Edge[out: f, in: g], found {}",
                self.describe(callable)
            ));
        };
        // The parameter and result sit inside the function type, a binder of their own.
        let parameter = close_against(parameter, &[callable], self.program);
        let result = close_against(result, &[callable], self.program);
        let generic = self.has_variables(callable);
        for side in [parameter, result] {
            self.claim(side, &mut HashSet::new());
        }
        self.edges.push(Edge {
            parameter: self.strip(parameter),
            result: self.strip(result),
            generic,
            use_,
        });
        Ok(self.edges.len() - 1)
    }

    /// That an `Edge`'s functions are a pair: `in` builds what `out` takes apart, from what `out`
    /// answers. Their type variables are each their own, so they are compared as `_`.
    fn check_pair(&mut self, out: usize, into: usize) -> Result<(), String> {
        let unbound = HashMap::new();
        let mut memo = HashMap::new();
        let mut open =
            |planner: &mut Self, type_id: usize| planner.substitute(type_id, &unbound, &mut memo);
        let (out, into) = (self.edges[out], self.edges[into]);
        let taken = open(self, out.parameter);
        let built = open(self, into.result);
        let answered = open(self, out.result);
        let given = open(self, into.parameter);
        let answered = self.answer(answered);
        let built = self.answer(built);
        let pairs = built.is_some_and(|built| types_overlap(taken, built, &*self.program))
            && answered.is_some_and(|answered| types_overlap(answered, given, &*self.program));
        if pairs {
            return Ok(());
        }
        Err(format!(
            "an Edge's `in` builds what its `out` takes apart, from what `out` answers, but `out` \
             is {} -> {}, and `in` {} -> {}",
            self.describe(out.parameter),
            self.describe(out.result),
            self.describe(into.parameter),
            self.describe(into.result)
        ))
    }

    /// Claim the named tuple types in `type_id` for the edges, and those in their fields.
    fn claim(&mut self, type_id: usize, seen: &mut HashSet<usize>) {
        for member in self.members(type_id) {
            if !seen.insert(member) {
                continue;
            }
            if let Type::Tuple(_) = self.lookup(member) {
                let (_, info) = self.tuple_info(member);
                for &(_, field) in &info.fields {
                    self.claim(field, seen);
                }
                // An unnamed tuple has no identity to claim it by: `[key, value]` in a format's
                // document is the same shape as a program's own pair.
                if info.name.is_some() {
                    let claimed = self.substitute(member, &HashMap::new(), &mut HashMap::new());
                    if !self.claimed.contains(&claimed) {
                        self.claimed.push(claimed);
                    }
                }
            }
        }
    }

    fn has_variables(&self, type_id: usize) -> bool {
        fn walk(type_id: usize, program: &Program, seen: &mut HashSet<usize>) -> bool {
            if !seen.insert(type_id) {
                return false;
            }
            let Some(typ) = program.lookup_type(type_id) else {
                return false;
            };
            matches!(typ, Type::Variable(_))
                || typ
                    .parts(program)
                    .into_iter()
                    .any(|part| walk(part, program, seen))
        }
        walk(type_id, self.program, &mut HashSet::new())
    }

    /// The node converting `source` to `target` after the edges in `chain`.
    fn node(&mut self, source: usize, target: usize, chain: &[usize]) -> usize {
        let source = self.strip(source);
        let target = self.strip(target);
        let key = (source, target, chain.to_vec());
        if let Some(&id) = self.memo.get(&key) {
            return id;
        }
        let id = self.nodes.len();
        self.nodes.push(Node {
            target,
            alternatives: Vec::new(),
        });
        self.memo.insert(key, id);
        if self.nodes.len() > MAX_NODES {
            self.overflow = true;
            return id;
        }

        let mut alternatives = Vec::new();
        if types_overlap(source, target, &*self.program) {
            alternatives.push(Alternative::Fit);
            // Every value fits: nothing else could be wanted.
            if is_compatible(source, target, &*self.program) {
                self.nodes[id].alternatives = alternatives;
                return id;
            }
        }
        for edge in 0..self.edges.len() {
            if !chain.contains(&edge) {
                self.edge_alternatives(edge, source, target, chain, &mut alternatives);
            }
        }
        // A tuple among several variants is told apart by its tag.
        let tagged = self.variants(target) > 1;
        for member in self.tuple_members(target) {
            if let Some(alternative) = self.tuple_alternative(source, target, member, chain) {
                alternatives.push(alternative);
            }
            if let Some(alternative) = self.read_alternative(source, member, chain, tagged) {
                alternatives.push(alternative);
            }
        }
        let tagged = self.variants(source) > 1;
        for member in self.tuple_members(source) {
            if let Some(alternative) = self.write_alternative(member, target, tagged) {
                alternatives.push(alternative);
            }
        }
        self.nodes[id].alternatives = alternatives;
        id
    }

    /// How many variants a type has: its members other than nil.
    fn variants(&mut self, type_id: usize) -> usize {
        self.members(type_id)
            .into_iter()
            .filter(|&member| !matches!(self.lookup(member), Type::Tuple(crate::types::NIL)))
            .count()
    }

    /// How the target tuple `member` is read from one of the neutral shapes the source may be:
    /// labelled fields from a record, positional ones from a list, a symbol from text, and a
    /// single positional field — a newtype — from whatever its content is read from. A variant
    /// (`tagged`) is first told apart by its tag, as the options say.
    fn read_alternative(
        &mut self,
        source: usize,
        member: usize,
        chain: &[usize],
        tagged: bool,
    ) -> Option<Alternative> {
        let (label, info) = self.tuple_info(member);
        if !self.structural(member, &info) {
            return None;
        }
        let config = self.config(&info, tagged)?;
        let layout = Layout::of(&info);
        // Content a tag wraps is read as the member, its tag aside.
        let content = |planner: &mut Self| -> Option<usize> {
            let (_, value) = planner.record_of(source)?;
            let present = planner.answer(value)?;
            Some(planner.node(present, member, &[]))
        };
        match (layout, config.tag.clone()) {
            (Layout::Other, _) => None,
            (Layout::Labelled, Tag::Untagged) => {
                self.reading_record(source, label, &info, &config, None)
            }
            (Layout::Labelled, Tag::Internal { key }) => {
                let tag = TagEntry {
                    key: Text::Given(key),
                    wire: config.wire.clone(),
                };
                self.reading_record(source, label, &info, &config, Some(tag))
            }
            (Layout::Symbol, Tag::Untagged | Tag::External) => {
                self.text_of(source)?;
                Some(Alternative::FromText {
                    label,
                    wire: config.wire,
                })
            }
            (Layout::Symbol, Tag::Internal { key: tag } | Tag::Adjacent { tag, .. }) => {
                let tag = TagEntry {
                    key: Text::Given(tag),
                    wire: config.wire.clone(),
                };
                self.reading_record(source, label, &info, &config, Some(tag))
            }
            (_, Tag::External) => Some(Alternative::FromExternal {
                label,
                wire: config.wire,
                inner: content(self)?,
            }),
            (_, Tag::Adjacent { tag, content: key }) => Some(Alternative::FromAdjacent {
                label,
                tag: TagEntry {
                    key: Text::Given(tag),
                    wire: config.wire,
                },
                content: Text::Given(key),
                inner: content(self)?,
            }),
            (Layout::Positional | Layout::Newtype, Tag::Internal { .. }) => {
                self.positional_internally(&info);
                None
            }
            (Layout::Positional, Tag::Untagged) => {
                let (_, element) = self.list_of(source)?;
                let fields = info
                    .fields
                    .iter()
                    .map(|&(_, field)| self.node(element, field, &[]))
                    .collect();
                Some(Alternative::FromList { label, fields })
            }
            (Layout::Newtype, Tag::Untagged) => {
                let inner = self.node(source, info.fields[0].1, chain);
                Some(Alternative::Wrap { label, inner })
            }
        }
    }

    /// How the source tuple `member` is written as one of the neutral shapes the target may
    /// be, the converse of [`Self::read_alternative`].
    fn write_alternative(
        &mut self,
        member: usize,
        target: usize,
        tagged: bool,
    ) -> Option<Alternative> {
        let (source, info) = self.tuple_info(member);
        if !self.structural(member, &info) {
            return None;
        }
        let config = self.config(&info, tagged)?;
        let layout = Layout::of(&info);
        // Content a tag wraps is written as the member, its tag aside.
        let content = |planner: &mut Self| -> Option<(RecordShape, usize)> {
            let (record, value) = planner.record_of(target)?;
            Some((record, planner.node(member, value, &[])))
        };
        match (layout, config.tag.clone()) {
            (Layout::Other, _) => None,
            (Layout::Labelled, Tag::Untagged) => {
                self.writing_record(source, target, &info, &config, None)
            }
            (Layout::Labelled, Tag::Internal { key }) => {
                let tag = TagEntry {
                    key: Text::Given(key),
                    wire: config.wire.clone(),
                };
                self.writing_record(source, target, &info, &config, Some(tag))
            }
            (Layout::Symbol, Tag::Untagged | Tag::External) => {
                let text = self.text_of(target)?;
                Some(Alternative::ToText {
                    source,
                    text,
                    wire: config.wire,
                })
            }
            (Layout::Symbol, Tag::Internal { key: tag } | Tag::Adjacent { tag, .. }) => {
                let tag = TagEntry {
                    key: Text::Given(tag),
                    wire: config.wire.clone(),
                };
                self.writing_record(source, target, &info, &config, Some(tag))
            }
            (_, Tag::External) => {
                let (record, inner) = content(self)?;
                Some(Alternative::ToExternal {
                    source,
                    record,
                    wire: config.wire,
                    inner,
                })
            }
            (_, Tag::Adjacent { tag, content: key }) => {
                let (record, inner) = content(self)?;
                Some(Alternative::ToAdjacent {
                    source,
                    record,
                    tag: TagEntry {
                        key: Text::Given(tag),
                        wire: config.wire,
                    },
                    content: Text::Given(key),
                    inner,
                })
            }
            (Layout::Positional | Layout::Newtype, Tag::Internal { .. }) => {
                self.positional_internally(&info);
                None
            }
            (Layout::Positional, Tag::Untagged) => {
                let (list, element) = self.list_of(target)?;
                let fields = info
                    .fields
                    .iter()
                    .map(|&(_, field)| self.node(field, element, &[]))
                    .collect();
                Some(Alternative::ToList {
                    source,
                    list,
                    fields,
                })
            }
            (Layout::Newtype, Tag::Untagged) => {
                let inner = self.node(info.fields[0].1, target, &[]);
                Some(Alternative::Unwrap { source, inner })
            }
        }
    }

    /// Reading the labelled tuple `label` (`info`) from a record, the source's.
    fn reading_record(
        &mut self,
        source: usize,
        label: usize,
        info: &TupleTypeInfo,
        config: &Config,
        tag: Option<TagEntry>,
    ) -> Option<Alternative> {
        let (_, value) = self.record_of(source)?;
        let present = self.answer(value)?;
        let fields = info
            .fields
            .iter()
            .zip(&config.keys)
            .map(|((field_label, field), key)| {
                let field_label = field_label
                    .as_deref()
                    .expect("a record's fields are labelled");
                let default = config
                    .defaults
                    .iter()
                    .find(|(defaulted, _)| defaulted == field_label)
                    .map(|(_, path)| path.clone());
                RecordField {
                    key: key.clone(),
                    node: self.node(present, *field, &[]),
                    required: default.is_none()
                        && !self.lookup(*field).contains_nil(&*self.program),
                    default,
                }
            })
            .collect();
        Some(Alternative::FromRecord { label, fields, tag })
    }

    /// Writing the labelled tuple `source` (`info`) as a record, the target's.
    fn writing_record(
        &mut self,
        source: usize,
        target: usize,
        info: &TupleTypeInfo,
        config: &Config,
        tag: Option<TagEntry>,
    ) -> Option<Alternative> {
        let (record, value) = self.record_of(target)?;
        let fields = info
            .fields
            .iter()
            .zip(&config.keys)
            .map(|(&(_, field), key)| (key.clone(), self.node(field, value, &[])))
            .collect();
        Some(Alternative::ToRecord {
            source,
            record,
            fields,
            tag,
        })
    }

    fn positional_internally(&mut self, info: &TupleTypeInfo) {
        self.errors.push(format!(
            "can't tag the positional variant {} internally: it has no labels to add the tag to",
            info.name.as_deref().unwrap_or("[…]")
        ));
    }

    /// How a tuple is read and written, as the options say: its keys, its defaults, its wire
    /// name, and — for a variant (`tagged`) — its tag. `None`, recording why, when the options
    /// say something of it that can't be.
    fn config(&mut self, info: &TupleTypeInfo, tagged: bool) -> Option<Config> {
        let shape = self
            .options
            .shape(info.name.as_deref())
            .map(|(index, shape)| (index, shape.clone()));
        let name = info.name.clone().unwrap_or_else(|| "[…]".to_string());
        if let Some((index, _)) = shape {
            self.met[index] = true;
        }
        let shape = shape.map(|(_, shape)| shape);
        let labels: Vec<&str> = info
            .fields
            .iter()
            .filter_map(|(label, _)| label.as_deref())
            .collect();
        let mut problem = None;
        if let Some(shape) = &shape {
            let configured = shape
                .rename
                .iter()
                .map(|(label, _)| label)
                .chain(shape.default.iter().map(|(label, _, _)| label));
            if !matches!(Layout::of(info), Layout::Labelled)
                && (shape.keys.is_some() || !shape.rename.is_empty() || !shape.default.is_empty())
            {
                problem = Some(format!(
                    "`shapes:` gives {name} keys, renames or defaults, which only labelled fields \
                     take"
                ));
            } else if let Some(stray) = configured
                .clone()
                .find(|label| !labels.contains(&label.as_str()))
            {
                problem = Some(format!(
                    "`shapes:` names {name}'s field `{stray}`, which {name} doesn't have"
                ));
            } else if let Some((label, _, default)) =
                shape.default.iter().find(|(label, _, default)| {
                    let field = info
                        .fields
                        .iter()
                        .find(|(field, _)| field.as_deref() == Some(label.as_str()))
                        .map(|&(_, field)| field)
                        .expect("checked among the labels");
                    !is_compatible(*default, field, &*self.program)
                })
            {
                let field = info
                    .fields
                    .iter()
                    .find(|(field, _)| field.as_deref() == Some(label.as_str()))
                    .map(|&(_, field)| field)
                    .expect("checked among the labels");
                problem = Some(format!(
                    "`shapes:` defaults {name}'s `{label}` to {}, which isn't a {}",
                    self.describe(*default),
                    self.describe(field)
                ));
            }
        }
        if let Some(problem) = problem {
            self.errors.push(problem);
            return None;
        }

        let style = shape
            .as_ref()
            .and_then(|shape| shape.keys)
            .or(self.options.keys)
            .unwrap_or(Style::Exact);
        let keys: Vec<Text> = labels
            .iter()
            .map(|label| {
                shape
                    .as_ref()
                    .and_then(|shape| shape.rename.iter().find(|(renamed, _)| renamed == label))
                    .map(|(_, path)| Text::Given(path.clone()))
                    .unwrap_or_else(|| Text::Fixed(style.key(label)))
            })
            .collect();
        let fixed: Vec<&str> = keys.iter().filter_map(Text::fixed).collect();
        if let Some(shared) = fixed
            .iter()
            .enumerate()
            .find(|(index, key)| fixed[..*index].contains(key))
            .map(|(_, key)| key)
        {
            self.errors.push(format!(
                "writes two fields of {name} under the key \"{shared}\""
            ));
            return None;
        }
        let tag = match tagged {
            true => shape
                .as_ref()
                .and_then(|shape| shape.tag.clone())
                .or_else(|| self.options.tag.clone())
                .unwrap_or(Tag::Untagged),
            false => Tag::Untagged,
        };
        Some(Config {
            keys,
            defaults: shape
                .as_ref()
                .map(|shape| {
                    shape
                        .default
                        .iter()
                        .map(|(label, path, _)| (label.clone(), path.clone()))
                        .collect()
                })
                .unwrap_or_default(),
            wire: match shape.and_then(|shape| shape.wire) {
                Some(path) => Text::Given(path),
                None => Text::Fixed(name),
            },
            tag,
        })
    }

    /// Whether the tuple `member` (`info`) is read from and written as the neutral shapes: it is
    /// neither one they are built of nor of a type an edge claims. A claim is by type, not by
    /// name, so a program's `Node[^, ^]` is its own beside a dict's `Node[…]`.
    fn structural(&self, member: usize, info: &TupleTypeInfo) -> bool {
        !is_neutral(info)
            && !self
                .claimed
                .iter()
                .any(|&claimed| is_compatible(member, claimed, &*self.program))
    }

    fn tuple_info(&self, member: usize) -> (usize, TupleTypeInfo) {
        let Type::Tuple(label) = self.lookup(member) else {
            unreachable!("a tuple member is a tuple type");
        };
        let info = self
            .program
            .lookup_tuple(label)
            .expect("a tuple type's tuple is in the table")
            .clone();
        (label, info)
    }

    /// The tuple `name` among a type's members, when it has one.
    fn named_member(&mut self, type_id: usize, name: &str) -> Option<(usize, TupleTypeInfo)> {
        self.tuple_members(type_id)
            .into_iter()
            .map(|member| self.tuple_info(member))
            .find(|(_, info)| info.name.as_deref() == Some(name))
    }

    /// When a type is a list, `Nil | Cons[element, ^]`: its tuples, and its element type.
    fn list_of(&mut self, type_id: usize) -> Option<(ListShape, usize)> {
        let (cons, cons_info) = self.named_member(type_id, "Cons")?;
        let (nil, nil_info) = self.named_member(type_id, "Nil")?;
        match (&cons_info.fields[..], &nil_info.fields[..]) {
            ([(None, element), (None, _)], []) => Some((ListShape { cons, nil }, *element)),
            _ => None,
        }
    }

    /// When a type is text, `Str['bin]`: its tuple.
    fn text_of(&mut self, type_id: usize) -> Option<usize> {
        let (text, info) = self.named_member(type_id, "Str")?;
        match info.fields[..] {
            [(None, bytes)] if matches!(self.lookup(bytes), Type::Binary) => Some(text),
            _ => None,
        }
    }

    /// When a type is a record, `'%codec.record<'d>` — `Record[list of [Str, value]]`: its
    /// tuples, and its entries' value type.
    fn record_of(&mut self, type_id: usize) -> Option<(RecordShape, usize)> {
        let (record, info) = self.named_member(type_id, "Record")?;
        let [(None, entries)] = info.fields[..] else {
            return None;
        };
        let (list, entry) = self.list_of(entries)?;
        let [entry] = self.tuple_members(entry)[..] else {
            return None;
        };
        let (entry, entry_info) = self.tuple_info(entry);
        let (None, [(None, key), (None, value)]) = (&entry_info.name, &entry_info.fields[..])
        else {
            return None;
        };
        let text = self.text_of(*key)?;
        Some((
            RecordShape {
                record,
                list,
                entry,
                text,
            },
            *value,
        ))
    }

    fn edge_alternatives(
        &mut self,
        edge: usize,
        source: usize,
        target: usize,
        chain: &[usize],
        alternatives: &mut Vec<Alternative>,
    ) {
        let Edge {
            parameter,
            result,
            generic,
            use_,
        } = self.edges[edge];
        let mut chained = chain.to_vec();
        if generic {
            chained.push(edge);
            chained.sort_unstable();
        }

        // A generic edge is instantiated twice: from the value (forward) and from the target
        // (backward). A plain one is the same both ways.
        let (forward_parameter, forward_result) = if generic {
            self.instantiate(parameter, result, parameter, source)
        } else {
            (parameter, result)
        };
        let (backward_parameter, backward_result) = if generic {
            self.instantiate(parameter, result, result, target)
        } else {
            (parameter, result)
        };
        let same = (forward_parameter, forward_result) == (backward_parameter, backward_result);

        // Goal first: the edge aimed at the target — its answer reaching it, once the value is
        // made what the edge takes — before the edge applied to the value as it stands, whose
        // answer may still be far from the target. Unless every value is already what the edge
        // takes, when preparing it would add nothing to applying it directly.
        let mut aimed = false;
        // (A backward-only edge has no forward application to leave it to.)
        let prepared = use_ == Use::Backward
            || !(same && is_compatible(source, backward_parameter, &*self.program));
        if use_ != Use::Forward
            && prepared
            && let Some(answer) = self.answer(backward_result)
            // Aimed only where the answer may already be the target. So it is never followed by
            // an edge applied forward: that is part of what `convert` means (its documentation
            // says so). A further edge that can be aimed is still found, aimed first with this one
            // in its preparation, but a forward-only one is not; following answers forward would
            // plan every encoder's answer on through every decoder — round trips between the
            // program's types. An answer merely shaped like the target is no route either: a
            // dict's `Node[…]` is not a program's tree's `Node[^, ^]`, however alike their names.
            && types_overlap(answer, target, &*self.program)
        {
            let prepare = self.node(source, backward_parameter, &chained);
            let then = self.node(answer, target, &chained);
            alternatives.push(Alternative::Backward {
                edge,
                prepare,
                then,
            });
            aimed = same;
        }

        // Forward: the value may already be what the edge takes, its answer converted on.
        // Covered already by the aimed alternative when it applies the same instantiation and
        // its answer needs nothing further.
        if use_ != Use::Backward
            && types_overlap(source, forward_parameter, &*self.program)
            && let Some(answer) = self.answer(forward_result)
            && !(aimed && is_compatible(answer, target, &*self.program))
        {
            let then = self.node(answer, target, &chained);
            alternatives.push(Alternative::Forward {
                edge,
                parameter: forward_parameter,
                then,
            });
        }
    }

    /// What an edge answers when it doesn't decline: its result without nil, or `None` when
    /// it can only decline.
    fn answer(&mut self, result: usize) -> Option<usize> {
        let typ = self.lookup(result);
        if !typ.contains_nil(&*self.program) {
            return Some(result);
        }
        let without = typ.without_nil(&*self.program);
        if without.is_never() {
            return None;
        }
        Some(match without {
            Type::Union(members) if members.len() == 1 => members[0],
            without => self.program.register_type(without),
        })
    }

    /// A generic edge's parameter and result, with its variables bound by matching `pattern`
    /// (its parameter or its result) against `concrete`. A variable left unbound is `_`.
    fn instantiate(
        &mut self,
        parameter: usize,
        result: usize,
        pattern: usize,
        concrete: usize,
    ) -> (usize, usize) {
        let mut bindings = HashMap::new();
        self.bind(pattern, concrete, &mut bindings, &mut HashSet::new());
        let mut memo = HashMap::new();
        let parameter = self.substitute(parameter, &bindings, &mut memo);
        let result = self.substitute(result, &bindings, &mut memo);
        (parameter, result)
    }

    fn bind(
        &mut self,
        pattern: usize,
        concrete: usize,
        bindings: &mut HashMap<String, usize>,
        seen: &mut HashSet<(usize, usize)>,
    ) {
        let pattern = self.strip(pattern);
        let concrete = self.strip(concrete);
        if !seen.insert((pattern, concrete)) {
            return;
        }
        match self.lookup(pattern) {
            Type::Variable(name) => {
                let bound = match bindings.get(&name) {
                    Some(&previous) if previous != concrete => self.union(vec![previous, concrete]),
                    _ => concrete,
                };
                bindings.insert(name, bound);
            }
            Type::Union(_) => {
                let patterns = self.members(pattern);
                let (variables, shaped): (Vec<usize>, Vec<usize>) = patterns
                    .into_iter()
                    .partition(|&member| matches!(self.lookup(member), Type::Variable(_)));
                for member in self.members(concrete) {
                    let matching: Vec<usize> = shaped
                        .iter()
                        .copied()
                        .filter(|&shape| self.same_head(shape, member))
                        .collect();
                    if matching.is_empty() {
                        for &variable in &variables {
                            self.bind(variable, member, bindings, seen);
                        }
                    }
                    for shape in matching {
                        self.bind(shape, member, bindings, seen);
                    }
                }
            }
            Type::Tuple(pattern_tuple) => {
                let pattern_fields = self.tuple_fields(pattern_tuple);
                for member in self.members(concrete) {
                    if !self.same_head(pattern, member) {
                        continue;
                    }
                    let Type::Tuple(concrete_tuple) = self.lookup(member) else {
                        continue;
                    };
                    let concrete_fields = self.tuple_fields(concrete_tuple);
                    for (&(_, pattern_field), &(_, concrete_field)) in
                        pattern_fields.iter().zip(&concrete_fields)
                    {
                        self.bind(pattern_field, concrete_field, bindings, seen);
                    }
                }
            }
            Type::Callable {
                parameter, result, ..
            } => {
                if let Type::Callable {
                    parameter: concrete_parameter,
                    result: concrete_result,
                    ..
                } = self.lookup(concrete)
                {
                    let pairs = [(parameter, concrete_parameter), (result, concrete_result)];
                    for (pattern_part, concrete_part) in pairs {
                        let pattern_part = close_against(pattern_part, &[pattern], self.program);
                        let concrete_part = close_against(concrete_part, &[concrete], self.program);
                        self.bind(pattern_part, concrete_part, bindings, seen);
                    }
                }
            }
            _ => {}
        }
    }

    /// Whether two types are tuples of the same shape, or alike in kind.
    fn same_head(&self, pattern: usize, concrete: usize) -> bool {
        match (self.lookup(pattern), self.lookup(concrete)) {
            (Type::Tuple(a), Type::Tuple(b)) => same_shape(
                self.program.lookup_tuple(a).expect("tuple in table"),
                self.program.lookup_tuple(b).expect("tuple in table"),
            ),
            (a, b) => std::mem::discriminant(&a) == std::mem::discriminant(&b),
        }
    }

    fn substitute(
        &mut self,
        type_id: usize,
        bindings: &HashMap<String, usize>,
        memo: &mut HashMap<usize, usize>,
    ) -> usize {
        if let Some(&done) = memo.get(&type_id) {
            return done;
        }
        let typ = self.lookup(type_id);
        let substituted = if let Type::Variable(name) = &typ {
            match bindings.get(name) {
                Some(&bound) => bound,
                None => self.top(),
            }
        } else {
            let parts = typ.parts(&*self.program);
            if parts.is_empty() {
                type_id
            } else {
                let parts: Vec<usize> = parts
                    .into_iter()
                    .map(|part| self.substitute(part, bindings, memo))
                    .collect();
                self.program.with_parts(type_id, &parts)
            }
        };
        memo.insert(type_id, substituted);
        substituted
    }

    /// A type's members, each closed against it: a union's, flattened, or the type itself.
    fn members(&mut self, type_id: usize) -> Vec<usize> {
        let type_id = self.strip(type_id);
        match self.lookup(type_id) {
            Type::Union(members) => members
                .into_iter()
                .flat_map(|member| {
                    let closed = close_against(member, &[type_id], self.program);
                    self.members(closed)
                })
                .collect(),
            _ => vec![type_id],
        }
    }

    fn tuple_members(&mut self, type_id: usize) -> Vec<usize> {
        self.members(type_id)
            .into_iter()
            .filter(|&member| matches!(self.lookup(member), Type::Tuple(_)))
            .collect()
    }

    fn tuple_fields(&self, tuple: usize) -> Vec<(Option<String>, usize)> {
        self.program
            .lookup_tuple(tuple)
            .expect("a tuple type's tuple is in the table")
            .fields
            .clone()
    }

    /// The structural conversion from `source` to the target tuple `member` (of `target`), when
    /// some value of `source` is a tuple shaped like it.
    ///
    /// A field converting the same pair as the node it is in (a list's tail, say) is that
    /// conversion again, one level down, so it continues the node's chain: otherwise the edges
    /// the node has spent would be tried afresh at every level, ahead of the structural step.
    fn tuple_alternative(
        &mut self,
        source: usize,
        target: usize,
        member: usize,
        chain: &[usize],
    ) -> Option<Alternative> {
        let Type::Tuple(label) = self.lookup(member) else {
            unreachable!("a tuple member is a tuple type");
        };
        let info = self
            .program
            .lookup_tuple(label)
            .expect("a tuple type's tuple is in the table")
            .clone();
        let sources = self.source_fields(source, &info)?;
        let fields = info
            .fields
            .iter()
            .zip(sources)
            .map(|(&(_, field), field_source)| {
                let recursion = self.strip(field_source) == source && self.strip(field) == target;
                self.node(field_source, field, if recursion { chain } else { &[] })
            })
            .collect();
        Some(Alternative::Tuple { label, fields })
    }

    /// The type of each field, for the values of `source` that are tuples shaped like `shape`;
    /// `None` when there are none.
    fn source_fields(&mut self, source: usize, shape: &TupleTypeInfo) -> Option<Vec<usize>> {
        let mut found: Option<Vec<Vec<usize>>> = None;
        for member in self.members(source) {
            let fields: Vec<usize> = match self.lookup(member) {
                Type::Top => vec![self.top(); shape.fields.len()],
                Type::Tuple(tuple) => {
                    let info = self.program.lookup_tuple(tuple).expect("tuple in table");
                    if !same_shape(info, shape) {
                        continue;
                    }
                    info.fields.iter().map(|&(_, field)| field).collect()
                }
                Type::Partial { name, fields, rest } => {
                    if name.is_some() && name != shape.name {
                        continue;
                    }
                    let top = self.top();
                    shape
                        .fields
                        .iter()
                        .map(|(label, _)| {
                            label
                                .as_ref()
                                .and_then(|label| {
                                    fields
                                        .iter()
                                        .find(|(listed, _)| listed == label)
                                        .map(|&(_, field)| field)
                                })
                                .or(rest)
                                .unwrap_or(top)
                        })
                        .collect()
                }
                _ => continue,
            };
            let found = found.get_or_insert_with(|| vec![Vec::new(); shape.fields.len()]);
            for (types, field) in found.iter_mut().zip(fields) {
                if !types.contains(&field) {
                    types.push(field);
                }
            }
        }
        Some(found?.into_iter().map(|types| self.union(types)).collect())
    }

    /// The union of `types`, flattened: a single member is itself, and `_` absorbs the rest.
    fn union(&mut self, types: Vec<usize>) -> usize {
        let mut members = Vec::new();
        for type_id in types {
            for member in self.members(type_id) {
                if matches!(self.lookup(member), Type::Top) {
                    return member;
                }
                if !members.contains(&member) {
                    members.push(member);
                }
            }
        }
        match members[..] {
            [single] => single,
            _ => self.program.register_type(Type::Union(members)),
        }
    }

    /// The plan from `root`: alternatives that can never succeed removed, and the nodes still
    /// reachable renumbered from 0.
    fn finish(self, root: usize) -> Plan {
        let mut nodes = self.nodes;
        let possible = possible_nodes(&nodes);
        for node in &mut nodes {
            node.alternatives
                .retain(|alternative| alternative_possible(alternative, &possible));
        }

        let mut renumbered: HashMap<usize, usize> = HashMap::new();
        let mut order = vec![root];
        renumbered.insert(root, 0);
        let mut index = 0;
        while index < order.len() {
            let node = order[index];
            index += 1;
            for alternative in &nodes[node].alternatives {
                for next in alternative.successors() {
                    if let std::collections::hash_map::Entry::Vacant(entry) = renumbered.entry(next)
                    {
                        entry.insert(order.len());
                        order.push(next);
                    }
                }
            }
        }
        let renumber = |id: usize| renumbered[&id];
        let nodes = order
            .iter()
            .map(|&node| Node {
                target: nodes[node].target,
                alternatives: nodes[node]
                    .alternatives
                    .iter()
                    .map(|alternative| alternative.renumbered(renumber))
                    .collect(),
            })
            .collect();
        Plan {
            root: 0,
            nodes,
            error: None,
        }
    }
}

/// How a tuple is read and written, as [`Planner::config`] works it out from the options.
struct Config {
    /// Each labelled field's key, in order.
    keys: Vec<Text>,
    /// Each defaulted field's label, and where its default is given.
    defaults: Vec<(String, OptionPath)>,
    /// The name it is written under, as a variant or a symbol.
    wire: Text,
    tag: Tag,
}

/// How a tuple is laid out, which says which neutral shape it is read from and written as.
enum Layout {
    /// Labelled fields, at least one: a record.
    Labelled,
    /// Two or more positional fields: a list.
    Positional,
    /// One positional field: its content.
    Newtype,
    /// A name and no fields: its name as text.
    Symbol,
    /// Nil, or a mix of labelled and positional fields: none.
    Other,
}

impl Layout {
    fn of(info: &TupleTypeInfo) -> Layout {
        let labelled = info
            .fields
            .iter()
            .filter(|(label, _)| label.is_some())
            .count();
        match (info.fields.len(), labelled) {
            (0, _) if info.name.is_some() => Layout::Symbol,
            (0, _) => Layout::Other,
            (n, l) if l == n => Layout::Labelled,
            (1, 0) => Layout::Newtype,
            (_, 0) => Layout::Positional,
            _ => Layout::Other,
        }
    }
}

/// Whether a tuple is one the neutral shapes are built of — text, a record, or a list's — and
/// so is never itself read from or written as one.
fn is_neutral(info: &TupleTypeInfo) -> bool {
    matches!(
        info.name.as_deref(),
        Some("Str" | "Record" | "Cons" | "Nil")
    )
}

/// Whether two tuples have the same name and the same labels in the same positions.
pub(crate) fn same_shape(a: &TupleTypeInfo, b: &TupleTypeInfo) -> bool {
    a.name == b.name
        && a.fields.len() == b.fields.len()
        && a.fields
            .iter()
            .zip(&b.fields)
            .all(|((a, _), (b, _))| a == b)
}

fn alternative_possible(alternative: &Alternative, possible: &[bool]) -> bool {
    alternative.successors().iter().all(|&next| possible[next])
}

/// Which nodes some value could get through: the least fixpoint, since a node is possible
/// only through an alternative whose every step is.
fn possible_nodes(nodes: &[Node]) -> Vec<bool> {
    let mut possible = vec![false; nodes.len()];
    loop {
        let mut changed = false;
        for (id, node) in nodes.iter().enumerate() {
            if !possible[id]
                && node
                    .alternatives
                    .iter()
                    .any(|alternative| alternative_possible(alternative, &possible))
            {
                possible[id] = true;
                changed = true;
            }
        }
        if !changed {
            return possible;
        }
    }
}
