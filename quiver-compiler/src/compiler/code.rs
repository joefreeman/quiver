//! A dialect's content as Quiver code: its `'%meta.expr`, the `code` a dialect is given when it
//! asks for it.
//!
//! The content is parsed as Quiver and lowered, node by node, to the IR value (`std/meta.qv`)
//! that [`super::dialect::Splicer`] would turn back into the code written. The code's own names
//! are its own: a binder becomes `Bind` and its uses `Local`, `$` the parameter of a function it
//! writes (`Param`), and a type parameter one of its own functions declares (`Var`). Anything
//! else — a variable of the surrounding code, an alias in its scope — is not the code's, and can
//! only stand in it as a *hole*, `` `name `` or `` `(chain) ``, which becomes
//! `Hole[offset, length]`: the span of its code within the content, in an expression, pattern
//! or type position. A module named in the code (`%codec.field`, `'%str`) stays a name.
//!
//! A hole means nothing in code a dialect returns: it is how `%meta{ … }` quotes, lifting each
//! into an `Unquote` of its span — code evaluated where the quote is written, to the IR it
//! stands for.

use quiver_core::program::Program;
use quiver_core::value::Value;

use super::values::{absent, binary, binary_string, integer, list, node, string};
use crate::ast;
use crate::parser::SourceSpan;

/// Why content can't be lowered, at a content-relative span when the node has one.
pub struct CodeError {
    pub message: String,
    pub span: Option<SourceSpan>,
}

/// An IR value and its exact type.
type Ir = (Value, usize);

type Lowered = Result<Ir, CodeError>;

/// Lower content parsed as a sequence: one expression stands for itself, and several steps for
/// a block of them.
pub fn lower(content: &ast::Sequence, program: &mut Program) -> Lowered {
    let mut coder = Coder {
        program,
        scopes: vec![Vec::new()],
        functions: Vec::new(),
    };
    match content.single_chain() {
        Some(chain) => coder.chain(chain),
        None => {
            let condition = coder.sequence(content)?;
            let consequence = coder.absent();
            let branch = coder.node(
                "Branch",
                vec![
                    (Some("condition"), condition),
                    (Some("consequence"), consequence),
                ],
            );
            let branches = coder.list(vec![branch]);
            Ok(coder.node("Block", vec![(Some("branches"), branches)]))
        }
    }
}

struct Coder<'p> {
    program: &'p mut Program,
    /// The names the code's binders have bound, by enclosing scope, innermost last.
    scopes: Vec<Vec<String>>,
    /// The type parameters of the code's functions enclosing the node, innermost last: their
    /// count is how far `$` reaches.
    functions: Vec<Vec<String>>,
}

fn unsupported(what: &str, span: Option<SourceSpan>) -> CodeError {
    CodeError {
        message: format!("can't express {what} as code"),
        span,
    }
}

impl Coder<'_> {
    fn node(&mut self, name: &str, fields: Vec<(Option<&str>, Ir)>) -> Ir {
        node(self.program, name, fields)
    }

    fn list(&mut self, items: Vec<Ir>) -> Ir {
        list(self.program, items)
    }

    fn text(&mut self, text: &str) -> Ir {
        string(self.program, text)
    }

    fn int(&mut self, n: usize) -> Ir {
        integer(self.program, n)
    }

    fn absent(&mut self) -> Ir {
        absent(self.program)
    }

    fn ripple(&mut self) -> Ir {
        self.node("Ripple", vec![])
    }

    /// `Hole[offset, length]`: the span of the code inside a hole.
    fn hole(&mut self, chain: &ast::Chain) -> Lowered {
        let span = chain
            .span
            .get()
            .expect("the parser records the span of a hole's code");
        let offset = self.int(span.offset);
        let length = self.int(span.length);
        Ok(self.node(
            "Hole",
            vec![(Some("offset"), offset), (Some("length"), length)],
        ))
    }

    fn literal(&mut self, literal: &ast::Literal) -> Ir {
        match literal {
            ast::Literal::Integer(n) => (
                Value::integer(n.clone()),
                self.program
                    .register_type(quiver_core::types::Type::Integer),
            ),
            ast::Literal::Binary(literal) => binary(self.program, literal.bytes()),
        }
    }

    fn scoped<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.scopes.push(Vec::new());
        let result = f(self);
        self.scopes.pop();
        result
    }

    fn bound(&self, name: &str) -> bool {
        self.scopes
            .iter()
            .any(|scope| scope.iter().any(|n| n == name))
    }

    /// A sequence's steps, as a list of expressions.
    fn sequence(&mut self, sequence: &ast::Sequence) -> Lowered {
        let mut steps = Vec::with_capacity(sequence.steps.len());
        for step in &sequence.steps {
            match step {
                ast::Step::Chain(chain) => steps.push(self.chain(chain)?),
                ast::Step::TypeAlias { name_span, .. } => {
                    return Err(unsupported("a type alias", name_span.get()));
                }
            }
        }
        Ok(self.list(steps))
    }

    /// A chain: its first term, each later one piped into (or matching) what came before, and
    /// its binding, if it has one, matched last.
    fn chain(&mut self, chain: &ast::Chain) -> Lowered {
        if let Some(assertion) = chain.assertions.first() {
            return Err(unsupported("an assertion", assertion.span.get()));
        }
        let Some((first, rest)) = chain.terms.split_first() else {
            return Err(unsupported(
                "a step that is only an assertion",
                chain.span.get(),
            ));
        };
        let mut value = match first {
            // At the head of a chain, a match is against the flowing value.
            ast::Term::Match(pattern) => {
                let pattern = self.pattern(pattern)?;
                let ripple = self.ripple();
                self.node(
                    "Match",
                    vec![(Some("value"), ripple), (Some("pattern"), pattern)],
                )
            }
            term => self.term(term)?,
        };
        for term in rest {
            value = match term {
                ast::Term::Match(pattern) => {
                    let pattern = self.pattern(pattern)?;
                    self.node(
                        "Match",
                        vec![(Some("value"), value), (Some("pattern"), pattern)],
                    )
                }
                term => {
                    let into = self.term(term)?;
                    self.node("Pipe", vec![(Some("value"), value), (Some("into"), into)])
                }
            };
        }
        if let Some(binding) = &chain.binding {
            let pattern = self.pattern(binding)?;
            value = self.node(
                "Match",
                vec![(Some("value"), value), (Some("pattern"), pattern)],
            );
        }
        Ok(value)
    }

    fn term(&mut self, term: &ast::Term) -> Lowered {
        match term {
            ast::Term::Hole(chain) => self.hole(chain),
            ast::Term::Literal(literal) => Ok(self.literal(literal)),
            ast::Term::String(_, segments) => {
                let mut bytes = Vec::new();
                for segment in segments {
                    match segment {
                        ast::StrSegment::Text(text) => bytes.extend_from_slice(text),
                        ast::StrSegment::Hole(_) => {
                            return Err(unsupported(
                                "an interpolated string: build it with %str.concat",
                                None,
                            ));
                        }
                    }
                }
                Ok(binary_string(self.program, bytes))
            }
            ast::Term::Tuple(tuple) => self.tuple(tuple),
            ast::Term::Access(access) => self.access(access),
            ast::Term::Apply(callee, argument) => {
                let callee = self.access(callee)?;
                let argument = self.term(argument)?;
                Ok(self.node(
                    "Apply",
                    vec![(Some("callee"), callee), (Some("arg"), argument)],
                ))
            }
            ast::Term::Match(pattern) => {
                let pattern = self.pattern(pattern)?;
                let ripple = self.ripple();
                Ok(self.node(
                    "Match",
                    vec![(Some("value"), ripple), (Some("pattern"), pattern)],
                ))
            }
            ast::Term::Block(block) => self.block(block),
            ast::Term::Function(function) => self.function(function),
            ast::Term::Dialect(dialect) => Err(unsupported(
                &format!("the dialect %{}{{ … }}", dialect.path.join("/")),
                dialect.span.get(),
            )),
            ast::Term::Spawn(_, _, span) => Err(unsupported("a spawn", span.get())),
            ast::Term::Select(_, span) => Err(unsupported("a select", span.get())),
            ast::Term::State(_, span) => Err(unsupported("a state sample", span.get())),
            ast::Term::Process(_) => Err(unsupported("a process", None)),
        }
    }

    fn tuple(&mut self, tuple: &ast::Tuple) -> Lowered {
        let name = match &tuple.name {
            ast::TupleName::Anonymous => self.absent(),
            ast::TupleName::Named(name) => self.text(name),
            ast::TupleName::Inherit => {
                return Err(unsupported("a name-keeping update", tuple.span.get()));
            }
        };
        let mut fields = Vec::with_capacity(tuple.fields.len());
        for field in &tuple.fields {
            let value = match &field.value {
                ast::FieldValue::Chain(chain) => self.chain(chain)?,
                ast::FieldValue::Spread(Some(source)) => {
                    let source = self.access(source)?;
                    fields.push(self.node("Spread", vec![(Some("source"), source)]));
                    continue;
                }
                ast::FieldValue::Spread(None) => {
                    return Err(unsupported(
                        "a spread of the flowing value",
                        field.span.get(),
                    ));
                }
            };
            let field = match &field.name {
                Some(label) => {
                    let label = self.text(label);
                    self.node(
                        "Labeled",
                        vec![(Some("label"), label), (Some("value"), value)],
                    )
                }
                None => value,
            };
            fields.push(field);
        }
        let fields = self.list(fields);
        Ok(self.node(
            "Tuple",
            vec![(Some("name"), name), (Some("fields"), fields)],
        ))
    }

    /// A name: the code's own binding, parameter or flowing value, or a module's member — and
    /// the fields read off it.
    fn access(&mut self, access: &ast::Access) -> Lowered {
        let span = access.span.get();
        let mut accessors = access.accessors.iter();
        let mut value = match &access.source {
            Some(ast::AccessSource::Identifier(name)) => {
                if !self.bound(name) {
                    return Err(CodeError {
                        message: format!(
                            "names `{name}`, which the code doesn't bind: a value from outside \
                             it stands in it as a hole, `{name}"
                        ),
                        span,
                    });
                }
                let name = self.text(name);
                self.node("Local", vec![(None, name)])
            }
            Some(ast::AccessSource::Parameter { depth }) => {
                if *depth >= self.functions.len() {
                    return Err(CodeError {
                        message: format!(
                            "names `{}` outside a function of its own that deep: a parameter \
                             from outside the code stands in it as a hole",
                            ast::parameter_sigils(*depth)
                        ),
                        span,
                    });
                }
                let depth = self.int(*depth);
                self.node("Param", vec![(None, depth)])
            }
            // `~> .field` reads the flowing value's field.
            Some(ast::AccessSource::Ripple) | None => self.ripple(),
            Some(ast::AccessSource::Import(module, None)) => {
                // A module named alone is its own value, `member: []`.
                let member = match access.accessors.first() {
                    Some(ast::AccessPath::Field(member)) => {
                        accessors.next();
                        self.text(member)
                    }
                    _ => self.absent(),
                };
                let module = self.text(&module.join("/"));
                let arguments = access
                    .type_arguments
                    .iter()
                    .map(|argument| self.type_(argument))
                    .collect::<Result<_, _>>()?;
                let arguments = self.list(arguments);
                self.node(
                    "Member",
                    vec![
                        (Some("module"), module),
                        (Some("member"), member),
                        (Some("arguments"), arguments),
                    ],
                )
            }
            _ => return Err(unsupported("this name", span)),
        };
        if !access.type_arguments.is_empty()
            && !matches!(access.source, Some(ast::AccessSource::Import(..)))
        {
            return Err(unsupported(
                "type arguments on anything but a module member",
                span,
            ));
        }
        for accessor in accessors {
            let field = match accessor {
                ast::AccessPath::Field(field) => self.text(field),
                ast::AccessPath::Index(index) => self.int(*index),
                ast::AccessPath::Annotation(..) => {
                    return Err(unsupported("an annotation read", span));
                }
            };
            value = self.node("Get", vec![(Some("of"), value), (Some("field"), field)]);
        }
        Ok(value)
    }

    fn block(&mut self, block: &ast::Block) -> Lowered {
        if let Some(annotation) = block.annotations.first() {
            return Err(unsupported(
                "a block's annotations: only a function's carry them",
                annotation.span.get(),
            ));
        }
        let branches = block
            .branches
            .iter()
            .map(|branch| self.branch(branch))
            .collect::<Result<_, _>>()?;
        let branches = self.list(branches);
        Ok(self.node("Block", vec![(Some("branches"), branches)]))
    }

    /// A branch, in a scope of its own: its condition's bindings reach its consequence.
    fn branch(&mut self, branch: &ast::Branch) -> Lowered {
        self.scoped(|coder| {
            let condition = coder.sequence(&branch.condition)?;
            let consequence = match &branch.consequence {
                Some(consequence) => coder.sequence(consequence)?,
                None => coder.absent(),
            };
            Ok(coder.node(
                "Branch",
                vec![
                    (Some("condition"), condition),
                    (Some("consequence"), consequence),
                ],
            ))
        })
    }

    fn function(&mut self, function: &ast::Function) -> Lowered {
        let span = function.span.get();
        let mut parameters = Vec::with_capacity(function.type_parameters.len());
        for parameter in &function.type_parameters {
            if parameter.bound.is_some() {
                return Err(unsupported("a bounded type parameter", span));
            }
            parameters.push(parameter.name.clone());
        }
        let Some(parameter_type) = &function.parameter_type else {
            return Err(unsupported(
                "a function whose parameter type is inferred: state it",
                span,
            ));
        };
        // An annotation's value is evaluated where the closure is made, outside the function.
        let annotations = match &function.body {
            Some(body) => body
                .annotations
                .iter()
                .map(|annotation| {
                    let value = self.chain(&annotation.value)?;
                    let key = self.text(&annotation.name);
                    Ok(self.node(
                        "Annotation",
                        vec![(Some("key"), key), (Some("value"), value)],
                    ))
                })
                .collect::<Result<Vec<_>, _>>()?,
            None => Vec::new(),
        };
        self.functions.push(parameters.clone());
        let lowered = self.scoped(|coder| {
            let parameter = coder.type_(parameter_type)?;
            let result = function
                .return_type
                .as_ref()
                .map(|result| coder.type_(result))
                .transpose()?;
            let body = match &function.body {
                // The function's own braces, unless they hold a single expression.
                Some(body) => match body.branches.as_slice() {
                    [branch] if branch.consequence.is_none() => {
                        match branch.condition.single_chain() {
                            Some(chain) => coder.chain(chain)?,
                            None => coder.block(&ast::Block {
                                annotations: Vec::new(),
                                branches: body.branches.clone(),
                            })?,
                        }
                    }
                    _ => coder.block(&ast::Block {
                        annotations: Vec::new(),
                        branches: body.branches.clone(),
                    })?,
                },
                // A function with no body is the identity.
                None => {
                    let depth = coder.int(0);
                    coder.node("Param", vec![(None, depth)])
                }
            };
            Ok((parameter, result, body))
        });
        self.functions.pop();
        let (parameter, result, body) = lowered?;
        let parameters = parameters.iter().map(|name| self.text(name)).collect();
        let parameters = self.list(parameters);
        let result = match result {
            Some(result) => result,
            None => self.absent(),
        };
        let annotations = self.list(annotations);
        Ok(self.node(
            "Function",
            vec![
                (Some("parameters"), parameters),
                (Some("parameter"), parameter),
                (Some("result"), result),
                (Some("annotations"), annotations),
                (Some("body"), body),
            ],
        ))
    }

    fn pattern(&mut self, pattern: &ast::Match) -> Lowered {
        match pattern {
            ast::Match::Hole(chain) => self.hole(chain),
            ast::Match::Identifier(name, _) => {
                self.scopes
                    .last_mut()
                    .expect("code has a scope")
                    .push(name.clone());
                let name = self.text(name);
                Ok(self.node("Bind", vec![(None, name)]))
            }
            ast::Match::Literal(literal) => Ok(self.literal(literal)),
            ast::Match::String(_, bytes) => Ok(binary_string(self.program, bytes.clone())),
            ast::Match::Tuple(tuple) => {
                let name = match &tuple.name {
                    Some(name) => self.text(name),
                    None => self.absent(),
                };
                let fields = tuple
                    .fields
                    .iter()
                    .map(|field| {
                        let pattern = self.pattern(&field.pattern)?;
                        Ok(match &field.name {
                            Some(label) => {
                                let label = self.text(label);
                                self.node(
                                    "Labeled",
                                    vec![(Some("label"), label), (Some("value"), pattern)],
                                )
                            }
                            None => pattern,
                        })
                    })
                    .collect::<Result<_, _>>()?;
                let fields = self.list(fields);
                Ok(self.node(
                    "Tuple",
                    vec![(Some("name"), name), (Some("fields"), fields)],
                ))
            }
            ast::Match::Placeholder => Ok(self.node("Any", vec![])),
            ast::Match::Ripple => Ok(self.ripple()),
            ast::Match::Type(type_def) => {
                let type_def = self.type_(type_def)?;
                Ok(self.node("Is", vec![(None, type_def)]))
            }
            ast::Match::Not(inner) => {
                let inner = self.pattern(inner)?;
                Ok(self.node("Not", vec![(None, inner)]))
            }
            ast::Match::And(parts) | ast::Match::Or(parts) => {
                let parts = parts
                    .iter()
                    .map(|part| self.pattern(part))
                    .collect::<Result<_, _>>()?;
                let parts = self.list(parts);
                let name = if matches!(pattern, ast::Match::And(_)) {
                    "And"
                } else {
                    "Or"
                };
                Ok(self.node(name, vec![(None, parts)]))
            }
            ast::Match::Partial(partial) => {
                let name = match &partial.name {
                    Some(name) => self.text(name),
                    None => self.absent(),
                };
                let fields = partial
                    .fields
                    .iter()
                    .map(|field| {
                        // A pun, `(x)`, binds the field under its own label.
                        let pattern = match &field.pattern {
                            Some(pattern) => self.pattern(pattern)?,
                            None => self.pattern(&ast::Match::Identifier(
                                field.name.clone(),
                                field.name_span,
                            ))?,
                        };
                        let label = self.text(&field.name);
                        Ok(self.node(
                            "Labeled",
                            vec![(Some("label"), label), (Some("value"), pattern)],
                        ))
                    })
                    .collect::<Result<_, _>>()?;
                let fields = self.list(fields);
                Ok(self.node(
                    "Partial",
                    vec![(Some("name"), name), (Some("fields"), fields)],
                ))
            }
            // Binding every named field binds names the code doesn't write, which only code
            // that knows the value's type could read.
            ast::Match::Star(_) => Err(unsupported("a star pattern", None)),
            ast::Match::Pin(target) => {
                let target = self.access(target)?;
                Ok(self.node("Pin", vec![(None, target)]))
            }
        }
    }

    fn type_(&mut self, type_def: &ast::Type) -> Lowered {
        match type_def {
            ast::Type::Hole(chain) => self.hole(chain),
            ast::Type::Primitive(ast::PrimitiveType::Int) => Ok(self.node("Int", vec![])),
            ast::Type::Primitive(ast::PrimitiveType::Bin) => Ok(self.node("Bin", vec![])),
            ast::Type::Primitive(ast::PrimitiveType::Ref) => Ok(self.node("Ref", vec![])),
            ast::Type::Top => Ok(self.node("Top", vec![])),
            ast::Type::Identifier { name, arguments }
                if arguments.is_empty()
                    && self
                        .functions
                        .iter()
                        .any(|parameters| parameters.contains(name)) =>
            {
                let name = self.text(name);
                Ok(self.node("Var", vec![(None, name)]))
            }
            ast::Type::Identifier { name, .. } => Err(CodeError {
                message: format!(
                    "names the type '{name}, which the code doesn't declare: code names a type \
                     structurally, by a module ('%mod.name), or as a hole"
                ),
                span: None,
            }),
            ast::Type::ModuleType {
                module,
                member,
                arguments,
                package: None,
            } => {
                let arguments = arguments
                    .iter()
                    .map(|argument| self.type_(argument))
                    .collect::<Result<_, _>>()?;
                let arguments = self.list(arguments);
                let module = self.text(&module.join("/"));
                let member = match member {
                    Some(member) => self.text(member),
                    None => self.absent(),
                };
                Ok(self.node(
                    "Named",
                    vec![
                        (Some("module"), module),
                        (Some("member"), member),
                        (Some("arguments"), arguments),
                    ],
                ))
            }
            ast::Type::ModuleType { .. } => unreachable!("a parsed module type has no package"),
            ast::Type::Tuple(tuple) => {
                let name = match &tuple.name {
                    Some(name) => self.text(name),
                    None => self.absent(),
                };
                let mut fields = Vec::with_capacity(tuple.fields.len());
                for field in &tuple.fields {
                    let ast::FieldType::Field {
                        name,
                        omittable: false,
                        type_def: Some(type_def),
                        default: None,
                        ..
                    } = field
                    else {
                        return Err(unsupported(
                            "a field type that isn't a label and a type (an omittable label, a \
                             default, or a spread)",
                            None,
                        ));
                    };
                    let type_def = self.type_(type_def)?;
                    fields.push(match name {
                        Some(name) => {
                            let label = self.text(name);
                            self.node(
                                "Labeled",
                                vec![(Some("label"), label), (Some("value"), type_def)],
                            )
                        }
                        None => type_def,
                    });
                }
                let fields = self.list(fields);
                if tuple.is_partial {
                    let rest = match &tuple.rest {
                        Some(rest) => self.type_(rest)?,
                        None => self.absent(),
                    };
                    Ok(self.node(
                        "Partial",
                        vec![
                            (Some("name"), name),
                            (Some("fields"), fields),
                            (Some("rest"), rest),
                        ],
                    ))
                } else {
                    Ok(self.node(
                        "Tuple",
                        vec![(Some("name"), name), (Some("fields"), fields)],
                    ))
                }
            }
            ast::Type::Union(union) => {
                let members = union
                    .types
                    .iter()
                    .map(|member| self.type_(member))
                    .collect::<Result<_, _>>()?;
                let members = self.list(members);
                Ok(self.node("Union", vec![(None, members)]))
            }
            ast::Type::Intersection(members) => {
                let members = members
                    .iter()
                    .map(|member| self.type_(member))
                    .collect::<Result<_, _>>()?;
                let members = self.list(members);
                Ok(self.node("Intersection", vec![(None, members)]))
            }
            ast::Type::Function(function) => {
                if function.receive.is_some() || function.states.is_some() {
                    return Err(unsupported("a function type's `!` or `?` clause", None));
                }
                let parameter = self.type_(&function.input)?;
                let result = self.type_(&function.output)?;
                Ok(self.node(
                    "Function",
                    vec![(Some("parameter"), parameter), (Some("result"), result)],
                ))
            }
            ast::Type::Process(process) => {
                let grant = |coder: &mut Self, part: &Option<Box<ast::Type>>| match part {
                    Some(part) => coder.type_(part),
                    None => Ok(coder.absent()),
                };
                let send = grant(self, &process.receive_type)?;
                let awaited = grant(self, &process.return_type)?;
                let state = grant(self, &process.state_type)?;
                Ok(self.node(
                    "Process",
                    vec![
                        (Some("send"), send),
                        (Some("await"), awaited),
                        (Some("state"), state),
                    ],
                ))
            }
            ast::Type::Cycle(depth) => {
                let depth = self.int(depth.unwrap_or(0));
                Ok(self.node("Cycle", vec![(None, depth)]))
            }
            ast::Type::Resource(name) => {
                let name = self.text(name);
                Ok(self.node("Resource", vec![(None, name)]))
            }
            ast::Type::SelfDefault { .. } => Err(unsupported(
                "a module's own default type: name it as '%mod",
                None,
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::dialect::{Content, Expander, Splicer, local_prefix};

    /// `ast` as text without what only says where or how the code was written — its spans,
    /// the line breaks of its chains — or where an expansion's names come from: the binding
    /// prefix and type parameter suffix on its own names, and the package its modules resolve
    /// in. What is left is what the code says.
    fn normalised(ast: &impl std::fmt::Debug) -> String {
        let mut text = format!("{ast:?}");
        while let Some(start) = text.find("Some(SourceSpan {") {
            let end = start + text[start..].find("})").expect("a span closes") + 2;
            text.replace_range(start..end, "None");
        }
        let mut text = text
            .replace(&local_prefix(0), "")
            .replace("#meta", "")
            .replace("Some(Memory)", "None")
            .replace(
                "Continuation { end: Spanned(None), pipe: Spanned(None) }",
                "",
            );
        while text.contains("[, ") || text.contains(", ]") {
            text = text.replace("[, ", "[").replace(", ]", "]");
        }
        text
    }

    /// Lower `source` to its IR and splice that back, expecting the code parsed from it: what
    /// `code` gives a dialect and what a dialect's code splices as are each other's
    /// inverse, over the code the IR expresses, written in its canonical spelling (a binding
    /// as a match, `1 ~> =x`, a function with its body, and a partial pattern's fields with
    /// theirs, `(x: x)`).
    #[track_caller]
    fn round_trips(source: &str) {
        let parsed = crate::parser::parse(source).expect("the source parses");
        let mut program = Program::new();
        let (ir, _) = lower(
            &crate::parser::parse_quoted(source).expect("the source parses quoted"),
            &mut program,
        )
        .unwrap_or_else(|error| panic!("{source}: {}", error.message));
        let expander = Expander {
            name: "%test".to_string(),
            package: crate::resolver::PackageId::Memory,
            span: ast::Spanned::default(),
        };
        // The code's own, holding no `Unquote`, so the content is never read.
        let dialect = ast::Dialect {
            path: vec!["test".to_string()],
            raw: source.to_string(),
            span: ast::Spanned::default(),
            content_span: ast::Spanned::default(),
        };
        let content = Content {
            dialect: &dialect,
            text: source,
            escapes: &[],
        };
        let splicer = Splicer::new(&program, &expander, content, local_prefix(0));
        let spliced = splicer
            .value_to_chain(&ir)
            .unwrap_or_else(|error| panic!("{source}: {error}"));
        // Several steps lower as a block of them.
        let expected = match parsed.single_chain() {
            Some(chain) => chain.clone(),
            None => ast::Chain {
                binding: None,
                binding_span: ast::Spanned::default(),
                span: ast::Spanned::default(),
                terms: vec![ast::Term::Block(ast::Block {
                    annotations: vec![],
                    branches: vec![ast::Branch {
                        condition: parsed,
                        consequence: None,
                    }],
                })],
                continuations: vec![],
                assertions: vec![],
            },
        };
        assert_eq!(normalised(&spliced), normalised(&expected), "{source}");
    }

    #[test]
    fn literals_and_tuples() {
        for source in [
            "1",
            "<0a1b>",
            "\"text\"",
            "[]",
            "[1, x: 2]",
            "Point[x: 1, y: 2]",
            "Blue",
        ] {
            round_trips(source);
        }
    }

    #[test]
    fn names_calls_and_chains() {
        for source in [
            "%num.add [1, 2]",
            "[1, 2] ~> %num.add",
            "1 ~> %num.add [~, 2]",
            "%codec.convert<'int> [1, using: []]",
            "1 ~> =x; x",
            "[x: 1] ~> =p; p.x",
            "[x: 1] ~> =p; [...p, y: 2]",
            "[x: 1] ~> ~.x",
        ] {
            round_trips(source);
        }
    }

    #[test]
    fn blocks_and_functions() {
        for source in [
            "{ | =1 => 2 | 3 }",
            "#'int { $ }",
            "#'int -> 'int { %num.add [$, 1] }",
            "#<'t>'t { $ }",
            "#['int, 'bin] { $0 }",
            "#'int { #'int { $$ } }",
            "#'int { :doc \"d\"\n $ }",
        ] {
            round_trips(source);
        }
    }

    #[test]
    fn patterns() {
        for source in [
            "[1, 2] ~> =[a, _]; a",
            "1 ~> =(1 | 2)",
            "1 ~> =\\[]",
            "1 ~> =('int & n); n",
            "{ | =1 => Ok | =_ => No }",
            "P[x: 1] ~> =P[x: 'int]",
            "[1] ~> =[~]",
            "\"a\" ~> =\"a\"",
            "P[x: 1] ~> =P(x: n); n",
            "[x: 1, y: 2] ~> =(x: x, y: 'int); x",
            "1 ~> =y; 1 ~> =^y",
            "[x: [y: 1]] ~> =p; 1 ~> =^p.x.y",
            "#[x: 'int, y: 'int] { $y ~> =^$x }",
            "#'int { #'int { =^$$ } }",
        ] {
            round_trips(source);
        }
    }

    #[test]
    fn types() {
        for source in [
            "#(x: 'int, *'bin) { $ }",
            "#('int | []) { $ }",
            "#('int & 'bin) { $ }",
            "#(#'int -> 'bin) { $ }",
            "#(@'int !'bin ?'int) { $ }",
            "#'%str { $ }",
            "#'%list<'int> { $ }",
            "#(Nil | Cons['int, ^]) { $ }",
            "#+File { $ }",
            "#_ { $ }",
            "#'ref { $ }",
        ] {
            round_trips(source);
        }
    }
}
