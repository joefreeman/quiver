//! `%codec.convert<'t, 's> [value, using: edges]`: walking a conversion's plan (see
//! [`crate::convert`]) with a value.
//!
//! The walk is depth first, guided by the value: each node tries its alternatives in order,
//! the first to succeed is its result, and a failure anywhere inside an alternative moves on to
//! the next. Edges are Quiver functions, so applying one is a [`Completion::Call`] whose
//! return resumes the walk ([`Continuation::Convert`]); the walk's state is a stack of frames
//! rather than the native stack, so a long list converts in constant native stack.
//!
//! A failed conversion answers nil carrying `%data`'s `DecodeError` under `:error`, for the
//! failure that got furthest into the value. A nil value answers itself.

use std::rc::Rc;
use std::sync::Arc;

use super::data::FailureKind;
use super::{BuiltinContext, Completion, Continuation};
use crate::convert::{
    Alternative, ListShape, OptionPath, PathStep, Plan, RecordShape, TagEntry, Text, edge_pair,
    same_shape,
};
use crate::effects::Effect;
use crate::error::Error;
use crate::types::TypeLookup;
use crate::value::Value;

pub fn builtin_codec_convert<E: Effect>(
    arg: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Completion<E>, Error> {
    let plan = ctx.plan().ok_or_else(|| {
        Error::InvalidArgument(
            "__codec_convert__ called without type arguments — a bare reference carries no \
             instantiation; name it with one (`__codec_convert__<'t>`) where the types are \
             concrete"
                .to_string(),
        )
    })?;
    // Compiling and linking both reject an instantiation whose plan failed.
    if let Some(error) = &plan.error {
        return Err(Error::InvalidArgument(format!(
            "conversion without a plan reached the runtime: {error}"
        )));
    }
    let (value, edges) = split_argument(arg, ctx)?;
    // A nil value is a failure arriving — a document that didn't parse — so it answers itself,
    // its `:error` intact, as a failed step would.
    if value.is_nil() {
        return Ok(Completion::Value(value));
    }
    let machine = Box::new(Machine {
        plan,
        options: arg.clone(),
        edges,
        frames: Vec::new(),
        path: Vec::new(),
        failure: None,
    });
    let root = machine.plan.root;
    machine.drive(Signal::Enter(root, value, Chain::default()), ctx)
}

/// The converted value and the edge functions, from `[value, using: edges]`.
fn split_argument<E: Effect>(
    arg: &Value,
    ctx: &BuiltinContext<E>,
) -> Result<(Value, Vec<Value>), Error> {
    let malformed = || Error::TypeMismatch {
        expected: "[value, using: (functions…)]".to_string(),
        found: arg.type_name().to_string(),
    };
    let Value::Tuple(tuple, fields) = arg else {
        return Err(malformed());
    };
    let info = ctx.executor.lookup_tuple(*tuple).ok_or_else(malformed)?;
    let using = info
        .fields
        .iter()
        .position(|(label, _)| label.as_deref() == Some("using"))
        .ok_or_else(malformed)?;
    let value = info
        .fields
        .iter()
        .position(|(label, _)| label.is_none())
        .ok_or_else(malformed)?;
    if !matches!(fields[using], Value::Tuple(..)) {
        return Err(malformed());
    }
    let mut functions = Vec::new();
    edge_functions(&fields[using], ctx, &mut functions);
    let edges = functions;
    Ok((fields[value].clone(), edges))
}

/// The functions in a tuple of edges, in the order the plan numbers them: an
/// `Edge[out: f, in: g]` is two, `out` then `in`, and an unnamed tuple holds more edges.
fn edge_functions<E: Effect>(edges: &Value, ctx: &BuiltinContext<E>, functions: &mut Vec<Value>) {
    let Value::Tuple(_, entries) = edges else {
        unreachable!("the plan checked the edges are a tuple");
    };
    for entry in entries.iter() {
        let info = match entry {
            Value::Tuple(tuple, _) => ctx.executor.lookup_tuple(*tuple),
            _ => None,
        };
        match (
            info.and_then(edge_pair),
            info.map(|info| info.name.is_none()),
            entry,
        ) {
            (Some(positions), _, Value::Tuple(_, pair)) => {
                functions.extend(positions.map(|position| pair[position].clone()))
            }
            (None, Some(true), _) => edge_functions(entry, ctx, functions),
            _ => functions.push(entry.clone()),
        }
    }
}

/// A conversion in progress.
#[derive(Debug)]
pub struct Machine {
    plan: Arc<Plan>,
    /// The conversion's argument, which the plan's option paths read.
    options: Value,
    edges: Vec<Value>,
    frames: Vec<Frame>,
    /// Where the walk is in the value: per enclosing tuple step, the tuple being built and the
    /// field.
    path: Vec<(usize, usize)>,
    /// The failure that got furthest, for the `:error` payload.
    failure: Option<Failure>,
}

#[derive(Debug)]
struct Failure {
    path: Vec<(usize, usize)>,
    target: usize,
}

/// The steps since the last structural one that left the value where it was — edges applied,
/// and newtypes wrapped — which a node may not take again (see [`crate::convert`]'s
/// termination).
#[derive(Debug, Clone, Default)]
struct Chain(Rc<Vec<Link>>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Link {
    Edge(usize),
    Wrap(usize),
}

impl Chain {
    fn contains(&self, link: Link) -> bool {
        self.0.contains(&link)
    }

    fn with(&self, link: Link) -> Chain {
        let mut links = (*self.0).clone();
        links.push(link);
        Chain(Rc::new(links))
    }
}

#[derive(Debug)]
enum Frame {
    /// Trying a node's alternatives for `value`, from `next`.
    Node {
        node: usize,
        value: Value,
        next: usize,
        chain: Chain,
    },
    /// Converting the value an edge is to take: on success, apply the edge.
    Prepare {
        edge: usize,
        then: usize,
        chain: Chain,
    },
    /// Waiting for an edge's answer, to convert it with node `then`.
    Apply { then: usize, chain: Chain },
    /// Converting the parts of a value one by one, the ones done so far in `built`, to `make`
    /// the result from them. A part with no node is taken as it is.
    Build {
        make: Make,
        /// The tuple whose fields the parts are, for the failure's path.
        path: usize,
        parts: Vec<(Option<usize>, Value)>,
        built: Vec<Value>,
        /// The chain each part is converted after: none for a value's parts, which are values of
        /// their own, and the wrapping for a newtype's content.
        chain: Chain,
    },
}

/// What a [`Frame::Build`] makes of its converted parts.
#[derive(Debug)]
enum Make {
    /// A tuple of them.
    Tuple(usize),
    /// A record, each part the value of an entry under its key.
    Record(RecordShape, Vec<Vec<u8>>),
    /// A list of them.
    List(ListShape),
    /// The one part itself: a newtype's content.
    Content,
}

/// What the top frame is told: start converting a value with a node (after a chain of edges),
/// or the result of the step it started (`None`: failed).
enum Signal {
    Enter(usize, Value, Chain),
    Return(Option<Value>),
}

/// What a frame does next: signal the frame below (or itself), or apply an edge.
enum Step {
    Signal(Signal),
    Apply(usize, Value),
}

impl Machine {
    /// Resume after an edge answered.
    pub(super) fn resume<E: Effect>(
        mut self: Box<Self>,
        answer: Value,
        ctx: &mut BuiltinContext<E>,
    ) -> Result<Completion<E>, Error> {
        let Some(Frame::Apply { then, chain }) = self.frames.pop() else {
            unreachable!("a conversion resumes waiting for an edge's answer");
        };
        // An edge answering nil declines.
        let signal = if answer.is_nil() {
            Signal::Return(None)
        } else {
            Signal::Enter(then, answer, chain)
        };
        self.drive(signal, ctx)
    }

    fn drive<E: Effect>(
        mut self: Box<Self>,
        mut signal: Signal,
        ctx: &mut BuiltinContext<E>,
    ) -> Result<Completion<E>, Error> {
        loop {
            let step = match signal {
                Signal::Enter(node, value, chain) => {
                    self.frames.push(Frame::Node {
                        node,
                        value,
                        next: 0,
                        chain,
                    });
                    self.advance(ctx)?
                }
                Signal::Return(result) => match self.frames.last_mut() {
                    None => {
                        return Ok(Completion::Value(match result {
                            Some(value) => value,
                            None => self.stamp(ctx)?,
                        }));
                    }
                    Some(Frame::Node { .. }) => match result {
                        Some(value) => {
                            self.frames.pop();
                            Step::Signal(Signal::Return(Some(value)))
                        }
                        None => self.advance(ctx)?,
                    },
                    Some(Frame::Prepare { .. }) => {
                        let Some(Frame::Prepare { edge, then, chain }) = self.frames.pop() else {
                            unreachable!()
                        };
                        match result {
                            Some(prepared) => {
                                self.frames.push(Frame::Apply { then, chain });
                                Step::Apply(edge, prepared)
                            }
                            None => Step::Signal(Signal::Return(None)),
                        }
                    }
                    Some(Frame::Apply { .. }) => {
                        unreachable!("an edge's answer arrives by resuming")
                    }
                    Some(Frame::Build { .. }) => {
                        self.path.pop();
                        match result {
                            Some(value) => self.next_part(value, ctx)?,
                            None => {
                                self.frames.pop();
                                Step::Signal(Signal::Return(None))
                            }
                        }
                    }
                },
            };
            signal = match step {
                Step::Signal(next) => next,
                Step::Apply(edge, argument) => return self.apply(edge, argument),
            };
        }
    }

    /// Try the top node's next alternative.
    fn advance<E: Effect>(&mut self, ctx: &mut BuiltinContext<E>) -> Result<Step, Error> {
        let plan = Arc::clone(&self.plan);
        let Some(Frame::Node {
            node,
            value,
            next,
            chain,
        }) = self.frames.last()
        else {
            unreachable!("advancing a node frame");
        };
        let node = &plan.nodes[*node];
        let (value, chain, mut next) = (value.clone(), chain.clone(), *next);
        while let Some(alternative) = node.alternatives.get(next) {
            next += 1;
            let Some(Frame::Node { next: tried, .. }) = self.frames.last_mut() else {
                unreachable!("advancing a node frame");
            };
            *tried = next;
            let build =
                |make: Make, path: usize, parts: Vec<(Option<usize>, Value)>| Frame::Build {
                    make,
                    path,
                    built: Vec::with_capacity(parts.len()),
                    parts,
                    chain: Chain::default(),
                };
            let frame =
                match alternative {
                    Alternative::Fit => {
                        if ctx.executor.check_type_compatible(&value, node.target) {
                            self.frames.pop();
                            return Ok(Step::Signal(Signal::Return(Some(value))));
                        }
                        None
                    }
                    &Alternative::Forward {
                        edge,
                        parameter,
                        then,
                    } => {
                        if !chain.contains(Link::Edge(edge))
                            && ctx.executor.check_type_compatible(&value, parameter)
                        {
                            let chain = chain.with(Link::Edge(edge));
                            self.frames.push(Frame::Apply { then, chain });
                            return Ok(Step::Apply(edge, value));
                        }
                        None
                    }
                    &Alternative::Backward {
                        edge,
                        prepare,
                        then,
                    } => {
                        if !chain.contains(Link::Edge(edge)) {
                            let chain = chain.with(Link::Edge(edge));
                            self.frames.push(Frame::Prepare {
                                edge,
                                then,
                                chain: chain.clone(),
                            });
                            return Ok(Step::Signal(Signal::Enter(prepare, value, chain)));
                        }
                        None
                    }
                    Alternative::Tuple { label, fields } => shaped_payload(&value, *label, ctx)
                        .map(|source| {
                            let parts = fields.iter().map(|&node| Some(node)).zip(source).collect();
                            build(Make::Tuple(*label), *label, parts)
                        }),
                    Alternative::FromRecord { label, fields, tag } => {
                        match record_entries(&value, ctx)? {
                            Some(entries) => {
                                self.read_record(&entries, *label, fields, tag.as_ref(), ctx)?
                            }
                            None => None,
                        }
                    }
                    Alternative::ToRecord {
                        source,
                        record,
                        fields,
                        tag,
                    } => match shaped_payload(&value, *source, ctx) {
                        Some(payload) => {
                            let mut keys = Vec::with_capacity(fields.len() + 1);
                            let mut parts = Vec::with_capacity(fields.len() + 1);
                            if let Some(tag) = tag {
                                keys.push(self.text(&tag.key, ctx)?);
                                parts.push((None, self.text_value(&tag.wire, record.text, ctx)?));
                            }
                            for ((key, node), part) in fields.iter().zip(payload) {
                                keys.push(self.text(key, ctx)?);
                                parts.push((Some(*node), part));
                            }
                            Some(build(Make::Record(*record, keys), *source, parts))
                        }
                        None => None,
                    },
                    Alternative::FromList { label, fields } => match list_items(&value, ctx) {
                        Some(items) if items.len() == fields.len() => {
                            let parts = fields.iter().map(|&node| Some(node)).zip(items).collect();
                            Some(build(Make::Tuple(*label), *label, parts))
                        }
                        _ => None,
                    },
                    Alternative::ToList {
                        source,
                        list,
                        fields,
                    } => shaped_payload(&value, *source, ctx).map(|payload| {
                        let parts = fields.iter().map(|&node| Some(node)).zip(payload).collect();
                        build(Make::List(*list), *source, parts)
                    }),
                    Alternative::FromText { label, wire } => {
                        let wire = self.text(wire, ctx)?;
                        if text_bytes(&value, ctx)?.is_some_and(|text| text == wire) {
                            self.frames.pop();
                            let symbol = Value::tuple(*label, vec![]);
                            return Ok(Step::Signal(Signal::Return(Some(symbol))));
                        }
                        None
                    }
                    Alternative::ToText { source, text, wire } => {
                        if shaped_payload(&value, *source, ctx).is_some() {
                            let text = self.text_value(wire, *text, ctx)?;
                            self.frames.pop();
                            return Ok(Step::Signal(Signal::Return(Some(text))));
                        }
                        None
                    }
                    Alternative::FromExternal { label, wire, inner } => {
                        match record_entries(&value, ctx)?.as_deref() {
                            Some([(key, content)])
                                if !content.is_nil() && *key == self.text(wire, ctx)? =>
                            {
                                Some(build(
                                    Make::Content,
                                    *label,
                                    vec![(Some(*inner), content.clone())],
                                ))
                            }
                            _ => None,
                        }
                    }
                    Alternative::ToExternal {
                        source,
                        record,
                        wire,
                        inner,
                    } => match shaped_payload(&value, *source, ctx) {
                        Some(_) => {
                            let keys = vec![self.text(wire, ctx)?];
                            let parts = vec![(Some(*inner), value.clone())];
                            Some(build(Make::Record(*record, keys), *source, parts))
                        }
                        None => None,
                    },
                    Alternative::FromAdjacent {
                        label,
                        tag,
                        content,
                        inner,
                    } => match record_entries(&value, ctx)? {
                        Some(entries) if self.tagged(&entries, tag, ctx)? => {
                            let content = self.text(content, ctx)?;
                            entry(&entries, &content).map(|content| {
                                build(Make::Content, *label, vec![(Some(*inner), content)])
                            })
                        }
                        _ => None,
                    },
                    Alternative::ToAdjacent {
                        source,
                        record,
                        tag,
                        content,
                        inner,
                    } => match shaped_payload(&value, *source, ctx) {
                        Some(_) => {
                            let keys = vec![self.text(&tag.key, ctx)?, self.text(content, ctx)?];
                            let parts = vec![
                                (None, self.text_value(&tag.wire, record.text, ctx)?),
                                (Some(*inner), value.clone()),
                            ];
                            Some(build(Make::Record(*record, keys), *source, parts))
                        }
                        None => None,
                    },
                    &Alternative::Wrap { label, inner } => (!chain.contains(Link::Wrap(label)))
                        .then(|| Frame::Build {
                            make: Make::Tuple(label),
                            path: label,
                            parts: vec![(Some(inner), value.clone())],
                            built: Vec::with_capacity(1),
                            chain: chain.with(Link::Wrap(label)),
                        }),
                    &Alternative::Unwrap { source, inner } => shaped_payload(&value, source, ctx)
                        .map(|payload| {
                            build(
                                Make::Content,
                                source,
                                vec![(Some(inner), payload[0].clone())],
                            )
                        }),
                };
            if let Some(frame) = frame {
                self.frames.push(frame);
                return self.start_part(ctx);
            }
        }
        let target = node.target;
        self.frames.pop();
        self.record_failure(target);
        Ok(Step::Signal(Signal::Return(None)))
    }

    /// The frame reading the labelled tuple `label` from a record's entries — each field from
    /// the entry under its key, or its default — when the record has every field it requires
    /// and, for a tagged variant, names it.
    fn read_record<E: Effect>(
        &self,
        entries: &[(Vec<u8>, Value)],
        label: usize,
        fields: &[crate::convert::RecordField],
        tag: Option<&TagEntry>,
        ctx: &mut BuiltinContext<E>,
    ) -> Result<Option<Frame>, Error> {
        if let Some(tag) = tag
            && !self.tagged(entries, tag, ctx)?
        {
            return Ok(None);
        }
        let mut parts = Vec::with_capacity(fields.len());
        for field in fields {
            let key = self.text(&field.key, ctx)?;
            let part = match (entry(entries, &key), &field.default) {
                (Some(value), _) => (Some(field.node), value),
                (None, Some(default)) => (None, self.option(default, ctx)),
                (None, None) if field.required => return Ok(None),
                (None, None) => (None, Value::nil()),
            };
            parts.push(part);
        }
        Ok(Some(Frame::Build {
            make: Make::Tuple(label),
            path: label,
            built: Vec::with_capacity(parts.len()),
            parts,
            chain: Chain::default(),
        }))
    }

    /// Whether a record's entries name the variant as `tag` says.
    fn tagged<E: Effect>(
        &self,
        entries: &[(Vec<u8>, Value)],
        tag: &TagEntry,
        ctx: &mut BuiltinContext<E>,
    ) -> Result<bool, Error> {
        let key = self.text(&tag.key, ctx)?;
        let wire = self.text(&tag.wire, ctx)?;
        Ok(match entry(entries, &key) {
            Some(named) => text_bytes(&named, ctx)?.is_some_and(|named| named == wire),
            None => false,
        })
    }

    /// The value an option path leads to in the conversion's argument.
    fn option<E: Effect>(&self, path: &OptionPath, ctx: &BuiltinContext<E>) -> Value {
        let field = |value: &Value, label: &str| -> Value {
            let Value::Tuple(tuple, payload) = value else {
                unreachable!("the plan's option paths follow the options' types");
            };
            let index = ctx
                .executor
                .lookup_tuple(*tuple)
                .and_then(|info| {
                    info.fields
                        .iter()
                        .position(|(field, _)| field.as_deref() == Some(label))
                })
                .expect("the plan's option paths follow the options' types");
            payload[index].clone()
        };
        let mut value = field(&self.options, &path.option);
        for step in &path.steps {
            value = match step {
                PathStep::Label(label) => field(&value, label),
                PathStep::Index(index) => match &value {
                    Value::Tuple(_, payload) => payload[*index].clone(),
                    _ => unreachable!("the plan's option paths follow the options' types"),
                },
            };
        }
        value
    }

    /// The bytes of text the plan names.
    fn text<E: Effect>(&self, text: &Text, ctx: &mut BuiltinContext<E>) -> Result<Vec<u8>, Error> {
        match text {
            Text::Fixed(text) => Ok(text.as_bytes().to_vec()),
            Text::Given(path) => {
                let value = self.option(path, ctx);
                Ok(text_bytes(&value, ctx)?.expect("the planner checked the option is text"))
            }
        }
    }

    /// Text the plan names, as a `text` tuple.
    fn text_value<E: Effect>(
        &self,
        text: &Text,
        tuple: usize,
        ctx: &mut BuiltinContext<E>,
    ) -> Result<Value, Error> {
        let bytes = self.text(text, ctx)?;
        let binary = ctx.executor.allocate_binary(bytes)?;
        Ok(Value::tuple(tuple, vec![Value::Binary(binary)]))
    }

    /// Convert the top build frame's next part, or make its result once they are all done.
    fn start_part<E: Effect>(&mut self, ctx: &mut BuiltinContext<E>) -> Result<Step, Error> {
        let Some(Frame::Build {
            path,
            parts,
            built,
            chain,
            ..
        }) = self.frames.last_mut()
        else {
            unreachable!("a build frame");
        };
        let index = built.len();
        if index == parts.len() {
            let Some(Frame::Build { make, built, .. }) = self.frames.pop() else {
                unreachable!("a build frame");
            };
            let value = make.make(built, ctx)?;
            return Ok(Step::Signal(Signal::Return(Some(value))));
        }
        match &parts[index] {
            (Some(node), part) => {
                let (node, part, chain) = (*node, part.clone(), chain.clone());
                self.path.push((*path, index));
                Ok(Step::Signal(Signal::Enter(node, part, chain)))
            }
            (None, part) => {
                let part = part.clone();
                built.push(part);
                self.start_part(ctx)
            }
        }
    }

    fn next_part<E: Effect>(
        &mut self,
        value: Value,
        ctx: &mut BuiltinContext<E>,
    ) -> Result<Step, Error> {
        let Some(Frame::Build { built, .. }) = self.frames.last_mut() else {
            unreachable!("a build frame");
        };
        built.push(value);
        self.start_part(ctx)
    }

    fn apply<E: Effect>(
        self: Box<Self>,
        edge: usize,
        argument: Value,
    ) -> Result<Completion<E>, Error> {
        match &self.edges[edge] {
            Value::Function(function, captures) => Ok(Completion::Call {
                function: *function,
                captures: Rc::clone(captures),
                argument,
                then: Continuation::Convert(self),
            }),
            // TODO: builtins can't be called from a builtin yet; a function wrapping one can.
            Value::Builtin(..) => Err(Error::InvalidArgument(
                "a conversion's functions can't be builtins: wrap the builtin in a function"
                    .to_string(),
            )),
            other => Err(Error::TypeMismatch {
                expected: "function".to_string(),
                found: other.type_name().to_string(),
            }),
        }
    }

    fn record_failure(&mut self, target: usize) {
        if self
            .failure
            .as_ref()
            .is_none_or(|failure| self.path.len() > failure.path.len())
        {
            self.failure = Some(Failure {
                path: self.path.clone(),
                target,
            });
        }
    }

    /// The failed conversion's nil, carrying where it failed when the program has the decode
    /// failure vocabulary.
    fn stamp<E: Effect>(&self, ctx: &mut BuiltinContext<E>) -> Result<Value, Error> {
        let Some(failure) = &self.failure else {
            return Ok(Value::nil());
        };
        let Some(table) = ctx.executor.decode_error_table().cloned() else {
            return Ok(Value::nil());
        };
        let executor = &mut *ctx.executor;
        let target = crate::format::format_type_by_id(&*executor, failure.target);
        let path: Vec<Result<String, usize>> = failure
            .path
            .iter()
            .map(|&(tuple, index)| {
                executor
                    .lookup_tuple(tuple)
                    .and_then(|info| info.fields.get(index))
                    .and_then(|(label, _)| label.clone())
                    .ok_or(index)
            })
            .collect();
        let mut chain = Value::tuple(table.nil_tuple, vec![]);
        for part in path.into_iter().rev() {
            let element = match part {
                Ok(label) => {
                    let binary = executor.allocate_binary(label.into_bytes())?;
                    Value::tuple(table.str_tuple, vec![Value::Binary(binary)])
                }
                Err(index) => Value::int(index as i64),
            };
            chain = Value::tuple(table.cons_tuple, vec![element, chain]);
        }
        // The value has the wrong shape here.
        let kind = Value::tuple(table.kind_tuples[FailureKind::Mismatch as usize], vec![]);
        let binary = executor.allocate_binary(format!("no route to {target}").into_bytes())?;
        let message = Value::tuple(table.str_tuple, vec![Value::Binary(binary)]);
        let payload = Value::tuple(table.decode_error_tuple, vec![chain, kind, message]);
        Ok(Value::nil()
            .annotated(table.error_key, payload)
            .expect("a fresh nil carries no annotations"))
    }
}

impl Make {
    fn make<E: Effect>(
        self,
        parts: Vec<Value>,
        ctx: &mut BuiltinContext<E>,
    ) -> Result<Value, Error> {
        Ok(match self {
            Make::Tuple(label) => Value::tuple(label, parts),
            Make::Record(shape, keys) => {
                let mut entries = Vec::with_capacity(parts.len());
                for (key, value) in keys.into_iter().zip(parts) {
                    let binary = ctx.executor.allocate_binary(key)?;
                    let key = Value::tuple(shape.text, vec![Value::Binary(binary)]);
                    entries.push(Value::tuple(shape.entry, vec![key, value]));
                }
                Value::tuple(shape.record, vec![build_list(shape.list, entries)])
            }
            Make::List(shape) => build_list(shape, parts),
            Make::Content => parts
                .into_iter()
                .next()
                .expect("a newtype's content is its one part"),
        })
    }
}

fn build_list(shape: ListShape, items: Vec<Value>) -> Value {
    items
        .into_iter()
        .rev()
        .fold(Value::tuple(shape.nil, vec![]), |tail, item| {
            Value::tuple(shape.cons, vec![item, tail])
        })
}

/// The name of a tuple value, when it is one.
fn tuple_name<'a, E: Effect>(value: &Value, ctx: &'a BuiltinContext<E>) -> Option<&'a str> {
    let Value::Tuple(tuple, _) = value else {
        return None;
    };
    ctx.executor
        .lookup_tuple(*tuple)
        .and_then(|info| info.name.as_deref())
}

/// The elements of a list value, `Cons[element, tail]` down to `Nil`; `None` for any other.
fn list_items<E: Effect>(mut value: &Value, ctx: &BuiltinContext<E>) -> Option<Vec<Value>> {
    let mut items = Vec::new();
    loop {
        match (tuple_name(value, ctx), value) {
            (Some("Nil"), Value::Tuple(_, payload)) if payload.is_empty() => return Some(items),
            (Some("Cons"), Value::Tuple(_, payload)) if payload.len() == 2 => {
                items.push(payload[0].clone());
                value = &payload[1];
            }
            _ => return None,
        }
    }
}

/// The bytes of a text value, `Str[bytes]`; `None` for any other.
fn text_bytes<E: Effect>(
    value: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Option<Vec<u8>>, Error> {
    match (tuple_name(value, ctx), value) {
        (Some("Str"), Value::Tuple(_, payload)) => match &payload[..] {
            [Value::Binary(binary)] => Ok(Some(ctx.executor.get_binary_data(binary)?.to_vec())),
            _ => Ok(None),
        },
        _ => Ok(None),
    }
}

/// The entries of a record value, `Record[list of [key, value]]`, each key as bytes; `None`
/// for any other value.
fn record_entries<E: Effect>(
    value: &Value,
    ctx: &mut BuiltinContext<E>,
) -> Result<Option<Entries>, Error> {
    let Some(entries) = (match (tuple_name(value, ctx), value) {
        (Some("Record"), Value::Tuple(_, payload)) if payload.len() == 1 => {
            list_items(&payload[0], ctx)
        }
        _ => None,
    }) else {
        return Ok(None);
    };
    let mut read = Vec::with_capacity(entries.len());
    for entry in entries {
        let Value::Tuple(_, pair) = &entry else {
            return Ok(None);
        };
        let [key, value] = &pair[..] else {
            return Ok(None);
        };
        let Some(key) = text_bytes(key, ctx)? else {
            return Ok(None);
        };
        read.push((key, value.clone()));
    }
    Ok(Some(read))
}

/// A record's entries, each key as bytes.
type Entries = Vec<(Vec<u8>, Value)>;

/// The value of a record's first entry under `key` that has one (a nil value is no entry).
fn entry(entries: &[(Vec<u8>, Value)], key: &[u8]) -> Option<Value> {
    entries
        .iter()
        .find(|(entry, value)| entry == key && !value.is_nil())
        .map(|(_, value)| value.clone())
}

/// A value's fields, when it is a tuple shaped like `label`.
fn shaped_payload<E: Effect>(
    value: &Value,
    label: usize,
    ctx: &BuiltinContext<E>,
) -> Option<Vec<Value>> {
    match value {
        Value::Tuple(tuple, payload) if shaped_like(*tuple, label, ctx) => Some(payload.to_vec()),
        _ => None,
    }
}

/// Whether a value built as `tuple` has the name, labels and arity of `label`.
fn shaped_like<E: Effect>(tuple: usize, label: usize, ctx: &BuiltinContext<E>) -> bool {
    if tuple == label {
        return true;
    }
    match (
        ctx.executor.lookup_tuple(tuple),
        ctx.executor.lookup_tuple(label),
    ) {
        (Some(actual), Some(expected)) => same_shape(actual, expected),
        _ => false,
    }
}
