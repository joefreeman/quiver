use crate::ast;
use std::collections::HashSet;

/// Where a captured value comes from in the creation scope.
#[derive(Debug, Clone, PartialEq)]
pub enum CaptureSource {
    /// A variable in the enclosing scope, by name.
    Variable(String),
    /// A function parameter `levels` above the capturing function: 1 is its parent's own
    /// parameter; deeper levels resolve to the parent's own outer-parameter capture, one
    /// level shallower, so a chain of closures relays the value inward.
    OuterParameter(usize),
}

impl CaptureSource {
    /// The base name this capture registers and resolves under in the string-keyed scope
    /// map. A private encoding — produced only here, never parsed back. An outer parameter
    /// borrows the written sigil spelling purely because identifiers can never start with
    /// `$`, so the key cannot collide with a variable capture.
    pub fn scope_name(&self) -> String {
        match self {
            CaptureSource::Variable(name) => name.clone(),
            CaptureSource::OuterParameter(levels) => ast::parameter_sigils(*levels),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Capture {
    pub source: CaptureSource,
    pub accessors: Vec<ast::AccessPath>,
    /// The (first) referencing site — where diagnostics about this capture should point,
    /// since captures are materialised far from the reference, at closure creation.
    pub span: ast::Spanned,
}

/// Collect free variables (captures) from a function body.
/// Returns captures in deterministic order (order of first occurrence in AST traversal).
pub fn collect_free_variables(
    body: Option<&ast::Block>,
    function_parameters: &HashSet<String>,
    defined_variables: &dyn Fn(&str, &[ast::AccessPath]) -> bool,
) -> Vec<Capture> {
    let Some(body) = body else {
        // Identity function has no captures
        return Vec::new();
    };

    let mut collector = FreeVariableCollector {
        function_parameters,
        defined_variables,
        captures: Vec::new(),
        function_depth: 0,
    };
    collector.visit_block(body);
    collector.captures
}

struct FreeVariableCollector<'a> {
    function_parameters: &'a HashSet<String>,
    defined_variables: &'a dyn Fn(&str, &[ast::AccessPath]) -> bool,
    /// Captures in order of first occurrence (deterministic ordering)
    captures: Vec<Capture>,
    /// Function-literal nesting below the function being collected: 0 in its direct body,
    /// incremented inside each nested literal. Outer-parameter references (`$$`, `$$$`) are
    /// free exactly when they reach above this depth.
    function_depth: usize,
}

impl<'a> FreeVariableCollector<'a> {
    fn visit_block(&mut self, block: &ast::Block) {
        for branch in &block.branches {
            self.visit_sequence(&branch.condition);
            if let Some(ref consequence) = branch.consequence {
                self.visit_sequence(consequence);
            }
        }
    }

    fn visit_sequence(&mut self, sequence: &ast::Sequence) {
        for chain in sequence.chains() {
            self.visit_chain(chain);
        }
    }

    fn visit_chain(&mut self, chain: &ast::Chain) {
        for term in &chain.terms {
            self.visit_term(term);
        }
    }

    fn visit_term(&mut self, term: &ast::Term) {
        match term {
            ast::Term::Literal(_) => {}
            ast::Term::Tuple(tuple) => {
                for field in &tuple.fields {
                    match &field.value {
                        ast::FieldValue::Chain(chain) => self.visit_chain(chain),
                        // A sourced spread (`...a.b`, `...$$conn`, including the source of an
                        // `a[..., y]` spread-update) reads its access, so a closure captures
                        // exactly what the equivalent expression access would.
                        ast::FieldValue::Spread(Some(access)) => self.visit_access_capture(access),
                        // A bare spread (`...`) is the chained value, not a variable.
                        ast::FieldValue::Spread(None) => {}
                    }
                }
            }
            ast::Term::String(_, segments) => {
                // Each hole is an expression that may reference (and so must capture) variables.
                for segment in segments {
                    if let ast::StrSegment::Hole(block) = segment {
                        self.visit_block(block);
                    }
                }
            }
            ast::Term::Match(pattern) => {
                // Match patterns can define variables or reference them (via &)
                // We need to traverse the pattern to find pin nodes
                self.visit_match(pattern);
            }
            ast::Term::Block(block) => {
                self.visit_block(block);
            }
            ast::Term::Function(func) => {
                // The nested literal's body is one function level deeper: its own `$` is not
                // ours, and an `$$` inside it refers to *this* function's parameter (bound
                // here, captured by the literal itself), so only deeper runs are free.
                if let Some(body) = &func.body {
                    self.function_depth += 1;
                    self.visit_block(body);
                    self.function_depth -= 1;
                }
            }
            ast::Term::Access(access) => {
                // A variable reference or a named tail call (`^f`) captures its identifier; `$`,
                // imports, builtins, and ripples don't.
                self.visit_access_capture(access);
            }
            ast::Term::Spawn(function, argument, _) => {
                self.visit_term(function);
                if let Some(argument) = argument {
                    self.visit_term(argument);
                }
            }
            ast::Term::Apply(access, argument) => {
                self.visit_access_capture(access);
                self.visit_term(argument);
            }
            ast::Term::Self_ => {}
            ast::Term::Process(_) => {}
            ast::Term::Select(sources, _) => {
                // Visit all source chains (if explicit sources provided)
                if let Some(sources) = sources {
                    for source in sources {
                        self.visit_chain(source);
                    }
                }
            }
            ast::Term::State(access, _) => {
                // `?('t)p` names its target without calling it — the same capture as any name.
                self.visit_access_capture(access);
            }
            // Dialects are expanded before capture collection (`compile_function`), so an
            // unexpanded invocation here has no variable references to collect yet.
            ast::Term::Dialect(_) => {}
        }
    }

    /// Capture the value an access refers to: a plain identifier (`f`, `f.x`), a named tail
    /// call (`^f`), or an outer-parameter run (`$$`, `$$$x`). Own `$`, imports, builtins,
    /// ripples, and self tail calls (`^`) capture nothing.
    fn visit_access_capture(&mut self, access: &ast::Access) {
        match &access.source {
            Some(ast::AccessSource::Identifier(name) | ast::AccessSource::TailCall(Some(name))) => {
                self.visit_identifier(name, access.accessors.clone(), access.base_span);
            }
            Some(ast::AccessSource::Parameter { depth }) => {
                self.visit_parameter(*depth, access.accessors.clone(), access.base_span);
            }
            // A module reference needs no capture: it is a compile-time-known value, so
            // the body emits the single constant that names it, wherever it appears.
            _ => {}
        }
    }

    /// Record an outer-parameter reference (`$$…`) as a capture when it reaches above the
    /// function being collected. `depth` is levels-up from the reference's own innermost
    /// function; re-rooted here, the capture holds levels-up from *this* function. Shallower
    /// references are some nested literal's concern — its own collection pass captures them
    /// from us. Validity (enough enclosing functions) is enforced with a pointed error where
    /// the capture is materialised, since only the creation scope knows the nesting.
    fn visit_parameter(
        &mut self,
        depth: usize,
        accessors: Vec<ast::AccessPath>,
        span: ast::Spanned,
    ) {
        if depth <= self.function_depth {
            return;
        }
        self.add_capture(Capture {
            source: CaptureSource::OuterParameter(depth - self.function_depth),
            accessors,
            span,
        });
    }

    /// Add a capture unless one for the same source and path exists (preserving
    /// first-occurrence order — and the first occurrence's span with it).
    fn add_capture(&mut self, capture: Capture) {
        if !self
            .captures
            .iter()
            .any(|c| c.source == capture.source && c.accessors == capture.accessors)
        {
            self.captures.push(capture);
        }
    }

    fn visit_identifier(
        &mut self,
        identifier: &str,
        accessors: Vec<ast::AccessPath>,
        span: ast::Spanned,
    ) {
        if !self.function_parameters.contains(identifier)
            && (self.defined_variables)(identifier, &accessors)
        {
            // Only added if not already present (preserves first-occurrence order)
            self.add_capture(Capture {
                source: CaptureSource::Variable(identifier.to_string()),
                accessors,
                span,
            });
        }
    }

    fn visit_match(&mut self, pattern: &ast::Match) {
        match pattern {
            ast::Match::Pin(target) => {
                // `&name` / `&name.field` reference an existing variable (with its access path,
                // so a closure captures exactly what expression accesses would); `&$$x` is an
                // outer-parameter reference and captures like the expression `$$x`. An own-`$`
                // pin reads the enclosing parameter and captures nothing.
                match &target.root {
                    ast::PinRoot::Variable(name) => {
                        self.visit_identifier(name, target.accessors.clone(), target.base_span);
                    }
                    ast::PinRoot::Parameter { depth } => {
                        self.visit_parameter(*depth, target.accessors.clone(), target.base_span);
                    }
                }
            }
            ast::Match::Tuple(tuple) => {
                // Recursively visit fields in tuple patterns
                for field in &tuple.fields {
                    self.visit_match(&field.pattern);
                }
            }
            ast::Match::Partial(partial) => {
                // Visit nested patterns in partial pattern fields
                for field in &partial.fields {
                    if let Some(nested_pattern) = &field.pattern {
                        self.visit_match(nested_pattern);
                    }
                }
            }
            ast::Match::Or(alternatives) => {
                // Visit each alternative's nested patterns and references
                for alternative in alternatives {
                    self.visit_match(alternative);
                }
            }
            // These don't contain nested patterns or variable references (the as-binder's
            // parenthesised part is a type, which binds nothing and names no variable).
            ast::Match::As(_, _, _)
            | ast::Match::Identifier(_, _)
            | ast::Match::Literal(_)
            | ast::Match::String(_, _)
            | ast::Match::Star(_)
            | ast::Match::Placeholder
            | ast::Match::Type(_) => {}
        }
    }
}
