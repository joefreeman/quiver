use crate::ast;
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Capture {
    pub base: String,
    pub accessors: Vec<ast::AccessPath>,
}

/// Collect free variables (captures) from a function body.
/// Returns captures in deterministic order (order of first occurrence in AST traversal).
pub fn collect_free_variables(
    body: Option<&ast::Expression>,
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
    };
    collector.visit_expression(body);
    collector.captures
}

struct FreeVariableCollector<'a> {
    function_parameters: &'a HashSet<String>,
    defined_variables: &'a dyn Fn(&str, &[ast::AccessPath]) -> bool,
    /// Captures in order of first occurrence (deterministic ordering)
    captures: Vec<Capture>,
}

impl<'a> FreeVariableCollector<'a> {
    fn visit_expression(&mut self, expression: &ast::Expression) {
        for branch in &expression.branches {
            self.visit_sequence(&branch.condition);
            if let Some(ref consequence) = branch.consequence {
                self.visit_sequence(consequence);
            }
        }
    }

    fn visit_sequence(&mut self, sequence: &ast::Sequence) {
        for chain in &sequence.chains {
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
                        // A named spread (`...a`, including the `a` of an `a[..., y]` spread-
                        // update) references the variable `a`, so a closure must capture it.
                        ast::FieldValue::Spread(Some(name)) => self.visit_identifier(name, vec![]),
                        // A bare spread (`...`) is the chained value, not a variable.
                        ast::FieldValue::Spread(None) => {}
                    }
                }
            }
            ast::Term::Match(pattern) => {
                // Match patterns can define variables or reference them (via &)
                // We need to traverse the pattern to find Match::Reference nodes
                self.visit_match(pattern);
            }
            ast::Term::Block(block) => {
                self.visit_expression(block);
            }
            ast::Term::Function(func) => {
                if let Some(body) = &func.body {
                    self.visit_expression(body);
                }
            }
            ast::Term::Access(access) => {
                // A variable reference or a named tail call (`^f`) captures its identifier; `$`,
                // imports, builtins, and ripples don't.
                self.visit_access_capture(access);
            }
            ast::Term::Equality => {}
            ast::Term::Not => {}
            ast::Term::Spawn(function, _) => {
                self.visit_term(function);
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
            ast::Term::Reference(access) => {
                // Reference to a value - same variable capture as Access
                self.visit_access_capture(access);
            }
        }
    }

    /// Capture the variable an access refers to: a plain identifier (`f`, `f.x`) or a named tail
    /// call (`^f`). `$`, imports, builtins, ripples, and self tail calls (`^`) capture nothing.
    fn visit_access_capture(&mut self, access: &ast::Access) {
        if let Some(ast::AccessSource::Identifier(name) | ast::AccessSource::TailCall(Some(name))) =
            &access.source
        {
            self.visit_identifier(name, access.accessors.clone());
        }
    }

    fn visit_identifier(&mut self, identifier: &str, accessors: Vec<ast::AccessPath>) {
        if !self.function_parameters.contains(identifier)
            && (self.defined_variables)(identifier, &accessors)
        {
            let capture = Capture {
                base: identifier.to_string(),
                accessors,
            };
            // Only add if not already present (preserves first-occurrence order)
            if !self.captures.contains(&capture) {
                self.captures.push(capture);
            }
        }
    }

    fn visit_match(&mut self, pattern: &ast::Match) {
        match pattern {
            ast::Match::Reference(name, _) => {
                // `&name` references an existing variable.
                self.visit_identifier(name, vec![]);
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
            | ast::Match::Star(_)
            | ast::Match::Placeholder
            | ast::Match::Type(_) => {}
        }
    }
}
