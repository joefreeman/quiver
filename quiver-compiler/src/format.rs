//! An AST pretty-printer that renders a parsed [`Program`] back to canonical Quiver source.
//!
//! Its defining transformation is **argument-first** lowering: a function application written as
//! juxtaposition (`f [args]`, `f x`) is rendered with the argument as a *preceding* chain term and
//! the callable following — `[args] f`, `x f`. The one exception is the ripple family
//! (`~`, `~.f`, `^~`, `@~`), where the flowing value *is* the callable, so the argument genuinely
//! cannot precede it; those stay juxtaposed.
//!
//! Because this lowering deliberately rewrites the AST on first print (an `Apply` node becomes two
//! ordinary chain terms), the printer is a *stable fixpoint* rather than AST-preserving: printing
//! its own output yields identical text. That idempotence is the property the tests assert.

use crate::ast::*;

/// Render a whole program to argument-first canonical source.
pub fn format_program(program: &Program) -> String {
    program
        .statements
        .iter()
        .map(render_statement)
        .collect::<Vec<_>>()
        .join("\n")
}

fn render_statement(statement: &Statement) -> String {
    match statement {
        Statement::TypeAlias {
            name,
            type_parameters,
            type_definition,
            ..
        } => {
            let mut out = String::from("'");
            if let Some(name) = name {
                out.push_str(name);
            }
            out.push_str(&render_type_parameters(type_parameters));
            out.push_str(" = ");
            out.push_str(&render_type(type_definition));
            out
        }
        Statement::Expression(sequence) => render_sequence(sequence),
    }
}

/// `<'a, 'b>` for a non-empty parameter list, otherwise empty. Type parameters are stored without
/// their `'` prefix, so it is re-added here.
fn render_type_parameters(params: &[String]) -> String {
    if params.is_empty() {
        return String::new();
    }
    format!(
        "<{}>",
        params
            .iter()
            .map(|p| format!("'{}", p))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn render_expression(expression: &Expression) -> String {
    expression
        .branches
        .iter()
        .map(render_branch)
        .collect::<Vec<_>>()
        .join(" | ")
}

fn render_branch(branch: &Branch) -> String {
    let condition = render_sequence(&branch.condition);
    match &branch.consequence {
        Some(consequence) => format!("{} => {}", condition, render_sequence(consequence)),
        None => condition,
    }
}

fn render_sequence(sequence: &Sequence) -> String {
    sequence
        .chains
        .iter()
        .map(render_chain)
        .collect::<Vec<_>>()
        .join(", ")
}

fn render_chain(chain: &Chain) -> String {
    let mut out = String::new();
    if let Some(pattern) = &chain.match_pattern {
        out.push_str(&render_match(pattern));
        out.push_str(" = ");
    }
    out.push_str(&render_terms(&chain.terms));
    out
}

/// Render the terms of a chain joined by ` `. Each term is a single chain element: application
/// is argument-first (`[args] f` is two terms — the tuple and the callable), so a term never
/// expands into several elements.
fn render_terms(terms: &[Term]) -> String {
    terms.iter().map(render_term).collect::<Vec<_>>().join(" ")
}

/// Render a single chain term.
fn render_term(term: &Term) -> String {
    match term {
        Term::Literal(literal) => render_literal(literal),
        Term::Tuple(tuple) => render_tuple(tuple),
        Term::Match(pattern) => format!("={}", render_match(pattern)),
        Term::Block(expression) => format!("{{ {} }}", render_expression(expression)),
        Term::Function(function) => render_function(function),
        Term::Access(access) => render_access(access),
        Term::Equality => "==".to_string(),
        Term::Not => "<>".to_string(),
        Term::Self_ => ".".to_string(),
        Term::Select(sources, _) => render_select(sources),
        Term::Process(index) => format!("@{}", index),
        Term::Reference(access) => format!("&{}", render_access(access)),
        Term::Spawn(func, _) => render_spawn_head(func),
    }
}

/// Render the head of a spawn (`@f`, `@~`, `@{ … }`, `@('int) { … }`). An inline spawned function
/// must use the `@`-sugar forms — the parser does not accept `@#…` — so a function head with a
/// parameter type is always emitted as `@(type) { body }` (the parenthesised arm accepts any type).
fn render_spawn_head(func: &Term) -> String {
    match func {
        Term::Function(function) => {
            let body = function
                .body
                .as_ref()
                .map(render_expression)
                .unwrap_or_default();
            match &function.parameter_type {
                None => format!("@{{ {} }}", body),
                Some(parameter_type) => {
                    format!("@({}) {{ {} }}", render_type(parameter_type), body)
                }
            }
        }
        other => format!("@{}", render_term(other)),
    }
}

fn render_select(sources: &Option<Vec<Chain>>) -> String {
    match sources {
        None => "!".to_string(),
        // The general form (with the required space after `!`) always re-parses, so all the
        // single-source sugars are emitted in this canonical shape.
        Some(chains) => format!(
            "! [{}]",
            chains
                .iter()
                .map(render_chain)
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

fn render_literal(literal: &Literal) -> String {
    match literal {
        Literal::Integer(value) => value.to_string(),
        Literal::Binary(bytes) => format!("0x{}", hex::encode(bytes)),
    }
}

fn render_tuple(tuple: &Tuple) -> String {
    let fields = tuple
        .fields
        .iter()
        .map(render_field)
        .collect::<Vec<_>>()
        .join(", ");
    match &tuple.name {
        TupleName::Anonymous => format!("[{}]", fields),
        TupleName::Named(name) if tuple.fields.is_empty() => name.clone(),
        TupleName::Named(name) => format!("{}[{}]", name, fields),
        // An inherited spread-update always re-parses via the `~`-headed form: with a `~` source
        // the parser leaves every spread field untouched, so the AST round-trips exactly whether
        // the original head was `~` or a variable.
        TupleName::Inherit => format!("~[{}]", fields),
    }
}

fn render_field(field: &TupleField) -> String {
    match &field.value {
        FieldValue::Spread(None) => "...".to_string(),
        FieldValue::Spread(Some(name)) => format!("...{}", name),
        FieldValue::Chain(chain) => match &field.name {
            Some(name) => format!("{}: {}", name, render_chain(chain)),
            None => render_chain(chain),
        },
    }
}

fn render_function(function: &Function) -> String {
    let mut out = String::from("#");
    out.push_str(&render_type_parameters(&function.type_parameters));
    if let Some(parameter_type) = &function.parameter_type {
        // The parameter type sits in a `function_input_type` position, which does not accept a bare
        // union/intersection/function — wrap those in parentheses.
        out.push_str(&render_type_atom(parameter_type));
    }
    if let Some(return_type) = &function.return_type {
        out.push_str(" -> ");
        out.push_str(&render_type_atom(return_type));
    }
    if let Some(body) = &function.body {
        out.push_str(" { ");
        out.push_str(&render_expression(body));
        out.push_str(" }");
    }
    out
}

fn render_access(access: &Access) -> String {
    let mut out = String::new();
    match &access.source {
        None => {}
        Some(AccessSource::Identifier(name)) => out.push_str(name),
        Some(AccessSource::Parameter) => out.push('$'),
        Some(AccessSource::Ripple) => out.push('~'),
        Some(AccessSource::Import(parts)) => {
            out.push('%');
            out.push_str(&parts.join("/"));
        }
        Some(AccessSource::Self_) => out.push('.'),
        Some(AccessSource::Builtin(name)) => {
            out.push_str("__");
            out.push_str(name);
            out.push_str("__");
        }
        Some(AccessSource::TailCall(None)) => out.push('^'),
        Some(AccessSource::TailCall(Some(name))) => {
            out.push('^');
            out.push_str(name);
        }
        Some(AccessSource::TailCallRipple) => out.push_str("^~"),
    }
    for accessor in &access.accessors {
        match accessor {
            AccessPath::Field(field) => {
                out.push('.');
                out.push_str(field);
            }
            AccessPath::Index(index) => {
                out.push('.');
                out.push_str(&index.to_string());
            }
        }
    }
    out
}

// ---------------------------------------------------------------------------
// Patterns
// ---------------------------------------------------------------------------

fn render_match(pattern: &Match) -> String {
    match pattern {
        Match::Identifier(name, _) => name.clone(),
        Match::Literal(literal) => render_literal(literal),
        Match::Tuple(tuple) => render_match_tuple(tuple),
        Match::Partial(partial) => render_partial_pattern(partial),
        Match::Star(None) => "*".to_string(),
        Match::Star(Some(name)) => format!("{}*", name),
        Match::Placeholder => "_".to_string(),
        Match::Reference(name, _) => format!("&{}", name),
        Match::Type(type_def) => render_match_type(type_def),
        Match::Or(alternatives) => format!(
            "({})",
            alternatives
                .iter()
                .map(render_match)
                .collect::<Vec<_>>()
                .join(" | ")
        ),
        // A type-ascribed binding always parenthesises its type: the parser requires `('(' type ')'`
        // immediately followed by the binder.
        Match::As(type_def, name, _) => format!("({}){}", render_type(type_def), name),
    }
}

fn render_match_tuple(tuple: &MatchTuple) -> String {
    let fields = tuple
        .fields
        .iter()
        .map(render_match_field)
        .collect::<Vec<_>>()
        .join(", ");
    match &tuple.name {
        Some(name) if tuple.fields.is_empty() => name.clone(),
        Some(name) => format!("{}[{}]", name, fields),
        None => format!("[{}]", fields),
    }
}

fn render_match_field(field: &MatchField) -> String {
    match &field.name {
        Some(name) => format!("{}: {}", name, render_match(&field.pattern)),
        None => render_match(&field.pattern),
    }
}

fn render_partial_pattern(partial: &PartialPattern) -> String {
    let fields = partial
        .fields
        .iter()
        .map(|field| match &field.pattern {
            None => field.name.clone(),
            Some(pattern) => format!("{}: {}", field.name, render_match(pattern)),
        })
        .collect::<Vec<_>>()
        .join(", ");
    format!("{}({})", partial.name.clone().unwrap_or_default(), fields)
}

/// Render a type used as a pattern. A pattern's type position only accepts the bare forms recognised
/// by `inline_type_expression` (a type name/module/self-default reference or a partial type);
/// anything else (unions, intersections, functions, non-partial tuples, cycles, …) must be wrapped
/// in parentheses so it re-parses as a `Match::Type` rather than, say, a structural tuple pattern.
fn render_match_type(type_def: &Type) -> String {
    let bare = match type_def {
        Type::Primitive(_)
        | Type::Identifier { .. }
        | Type::ModuleType { .. }
        | Type::SelfDefault { .. } => true,
        Type::Tuple(tuple_type) => tuple_type.is_partial,
        _ => false,
    };
    if bare {
        render_type(type_def)
    } else {
        format!("({})", render_type(type_def))
    }
}

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

fn render_type(type_def: &Type) -> String {
    match type_def {
        Type::Primitive(PrimitiveType::Int) => "'int".to_string(),
        Type::Primitive(PrimitiveType::Bin) => "'bin".to_string(),
        Type::Primitive(PrimitiveType::Ref) => "'ref".to_string(),
        Type::Identifier { name, arguments } => {
            format!("'{}{}", name, render_type_arguments(arguments))
        }
        Type::Tuple(tuple_type) => render_tuple_type(tuple_type),
        Type::Function(function_type) => format!(
            "#{} -> {}",
            render_type_atom(&function_type.input),
            render_type_atom(&function_type.output)
        ),
        Type::Union(union_type) => union_type
            .types
            .iter()
            .map(render_union_member)
            .collect::<Vec<_>>()
            .join(" | "),
        Type::Intersection(types) => types
            .iter()
            .map(render_type_atom)
            .collect::<Vec<_>>()
            .join(" & "),
        Type::Cycle(None) => "^".to_string(),
        Type::Cycle(Some(level)) => format!("^{}", level),
        Type::Process(process_type) => render_process_type(process_type),
        Type::Resource(name) => format!("\\{}", name),
        Type::ModuleType {
            module,
            member,
            arguments,
        } => {
            let mut out = format!("'%{}", module.join("/"));
            if let Some(member) = member {
                out.push('.');
                out.push_str(member);
            }
            out.push_str(&render_type_arguments(arguments));
            out
        }
        Type::SelfDefault { arguments } => format!("'{}", render_type_arguments(arguments)),
    }
}

/// Render a type where the grammar expects a `base_type`/atom (intersection members, process
/// receive/return, function input/output): wrap a union, intersection, or function in parentheses.
fn render_type_atom(type_def: &Type) -> String {
    match type_def {
        Type::Union(_) | Type::Intersection(_) | Type::Function(_) => {
            format!("({})", render_type(type_def))
        }
        _ => render_type(type_def),
    }
}

/// A union member is an intersection-level type, so it never needs wrapping except for a function
/// type (which only appears as a member when originally parenthesised).
fn render_union_member(type_def: &Type) -> String {
    match type_def {
        Type::Function(_) => format!("({})", render_type(type_def)),
        _ => render_type(type_def),
    }
}

fn render_type_arguments(arguments: &[Type]) -> String {
    if arguments.is_empty() {
        return String::new();
    }
    format!(
        "<{}>",
        arguments
            .iter()
            .map(render_type)
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn render_tuple_type(tuple_type: &TupleType) -> String {
    let name = tuple_type.name.clone().unwrap_or_default();
    if tuple_type.fields.is_empty() {
        return if tuple_type.is_partial {
            format!("{}()", name)
        } else if tuple_type.name.is_some() {
            name
        } else {
            "[]".to_string()
        };
    }
    let fields = tuple_type
        .fields
        .iter()
        .map(render_field_type)
        .collect::<Vec<_>>()
        .join(", ");
    let (open, close) = if tuple_type.is_partial {
        ("(", ")")
    } else {
        ("[", "]")
    };
    format!("{}{}{}{}", name, open, fields, close)
}

fn render_field_type(field_type: &FieldType) -> String {
    match field_type {
        FieldType::Field {
            name: Some(name),
            type_def,
        } => format!("{}: {}", name, render_type(type_def)),
        FieldType::Field {
            name: None,
            type_def,
        } => render_type(type_def),
        FieldType::Spread {
            identifier: None, ..
        } => "...".to_string(),
        FieldType::Spread {
            identifier: Some(identifier),
            type_arguments,
        } => format!(
            "...'{}{}",
            identifier,
            render_type_arguments(type_arguments)
        ),
    }
}

fn render_process_type(process_type: &ProcessType) -> String {
    match (&process_type.receive_type, &process_type.return_type) {
        (Some(receive), None) => format!("@{}", render_type_atom(receive)),
        (None, None) => "@".to_string(),
        (None, Some(ret)) => format!("(@-> {})", render_type_atom(ret)),
        (Some(receive), Some(ret)) => {
            format!(
                "(@{} -> {})",
                render_type_atom(receive),
                render_type_atom(ret)
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;
    use std::path::PathBuf;

    /// Assert that `source` parses, and that printing is an idempotent fixpoint: the second print
    /// equals the first, and the once-printed text re-parses cleanly.
    fn assert_idempotent(source: &str, label: &str) {
        let ast = parse(source).unwrap_or_else(|e| panic!("{label}: source must parse: {e:?}"));
        let printed = format_program(&ast);
        let reparsed = parse(&printed).unwrap_or_else(|e| {
            panic!("{label}: formatted output must parse: {e:?}\n--- output ---\n{printed}")
        });
        let printed2 = format_program(&reparsed);
        assert_eq!(printed, printed2, "{label}: format must be idempotent");
    }

    fn std_dir() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("compiler crate has a parent (repo root)")
            .join("std")
    }

    #[test]
    fn idempotent_over_std() {
        let dir = std_dir();
        let mut files: Vec<_> = std::fs::read_dir(&dir)
            .unwrap_or_else(|e| panic!("read std dir {dir:?}: {e:?}"))
            .map(|entry| entry.unwrap().path())
            .filter(|path| path.extension().is_some_and(|ext| ext == "qv"))
            .collect();
        files.sort();
        assert!(!files.is_empty(), "expected std/*.qv files in {dir:?}");
        for path in files {
            let source = std::fs::read_to_string(&path).unwrap();
            let label = path.file_name().unwrap().to_string_lossy().to_string();
            assert_idempotent(&source, &label);
        }
    }

    #[test]
    fn idempotent_over_corpus() {
        // A snippet corpus exercising every AST variant the printer must handle. Each must parse
        // and reach a print fixpoint.
        let corpus: &[&str] = &[
            // --- argument-first application ---
            "[3, 4] add [~, 2] mul",
            "x f",
            "[[x] g, y] f",
            "[3, 4] __integer_add__",
            "&g f",
            "5 f",
            // --- bare ripple (flowing-value) terms: no juxtaposed argument ---
            "5 ~",
            "num ~.add",
            "&g ^~",
            "&g @~",
            // --- argument-first tail calls ---
            "[a, b] ^",
            "[x] ^foo",
            // --- spawn ---
            "@f",
            "@{ 5 }",
            "@'int { $ }",
            "@('int | 'bin) { $ }",
            "x @counter",
            // --- select / process / references ---
            "!'int",
            "!'int { =0 => Ok | [] }",
            "! [p, 1000]",
            "!p",
            "! []",
            "&f",
            "&.",
            "&__integer_add__",
            "42 pid",
            "@3",
            // --- ripple / spread values ---
            "5 [~, 1]",
            "0 Point[x: ~, y: ~]",
            "a[..., y: 3]",
            "~[..., y: 3]",
            "A[x: 1] B[...]",
            "[...a, ...b]",
            "[w: 0, ...a]",
            // --- field access / self / operators ---
            "point.x .name",
            "$ $.x $.0",
            "5 ==",
            "[] <>",
            "42 .",
            // --- match forms ---
            "=Point[x, y]",
            "=(x: 'int)",
            "=Config*",
            "=*",
            "=_",
            "=&y",
            "=('int)n",
            "=([a] | [b])",
            "='int",
            "=Circle[radius: r]",
            "=\"hello\"",
            "=42",
            "=0x0a1b",
            "Point[x, y] = p",
            "x = 5",
            "(a, b) = p",
            "[x: a, y: b] = p",
            "Config(host, port) = c",
            // --- blocks / branches / consequence ---
            "v { =0 => \"zero\" | \"neg\" }",
            "item { is_valid? process | [] show_error }",
            "{ =Square[x] [x, 10] num.gt? => \"large\" | \"small\" }",
            // --- functions ---
            "xs [~, #{ $0 }, Nil] map",
            "#['int, 'int] { =[a, b] => [b, a] }",
            "#<'t>'t { $ }",
            "#'int",
            "#'int -> 'bin { $ }",
            // --- multi-chain sequences & control flow ---
            "tag = %ref, [tag, 42] =[&tag, x], x",
            "[], 5",
            // --- type aliases: unions, intersections, partials, modules, recursion ---
            "'bool = True | False",
            "'shape = Circle[radius: 'int] | Rectangle[width: 'int, height: 'int]",
            "'rw = 'readable & 'writable",
            "'inter = 'a & 'b | 'c",
            "'list<'t> = Nil | Cons['t, ^]",
            "'tree<'t> = Leaf['t] | Node[^, ^]",
            "'json = Null | 'bool | 'int | Str['bin] | Array[(Nil | Cons[^, ^1])]",
            "'pair<'a, 'b> = Pair[first: 'a, second: 'b]",
            "'adder = #'int -> 'int",
            "'writer = (write: (#'bin -> Ok))",
            "'np = Point(x: 'int)",
            "'ep = ()",
            "'enp = Point()",
            "'nil = []",
            "'mt = '%list<'int>",
            "'mn = '%shapes.circle",
            "'recv = @'int",
            "'both = (@'int -> 'bin)",
            "'ret = (@-> 'bin)",
            "'res = \\File",
            "'post = Post[...'entity, title: Str['bin], ...'updateable]",
            "' = Str['bin]",
            "'<'t> = Nil | Cons['t, ^]",
            "'selfapp = '<'int>",
            "'fnfield = [f: (#'int -> 'bin) | Nil]",
        ];
        for snippet in corpus {
            assert_idempotent(snippet, snippet);
        }
    }
}
