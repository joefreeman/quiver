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
    block: Option<&ast::Block>,
    function_parameters: &HashSet<String>,
    defined_variables: &dyn Fn(&str, &[ast::AccessPath]) -> bool,
) -> Vec<Capture> {
    let Some(block) = block else {
        // Identity function has no captures
        return Vec::new();
    };

    let mut collector = FreeVariableCollector {
        function_parameters,
        defined_variables,
        captures: Vec::new(),
    };
    collector.visit_block(block);
    collector.captures
}

struct FreeVariableCollector<'a> {
    function_parameters: &'a HashSet<String>,
    defined_variables: &'a dyn Fn(&str, &[ast::AccessPath]) -> bool,
    /// Captures in order of first occurrence (deterministic ordering)
    captures: Vec<Capture>,
}

impl<'a> FreeVariableCollector<'a> {
    fn visit_block(&mut self, expression: &ast::Block) {
        for branch in &expression.branches {
            self.visit_expression(&branch.condition);
            if let Some(ref consequence) = branch.consequence {
                self.visit_expression(consequence);
            }
        }
    }

    fn visit_expression(&mut self, expression: &ast::Expression) {
        for chain in &expression.chains {
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
                    if let ast::FieldValue::Chain(chain) = &field.value {
                        self.visit_chain(chain);
                    }
                    // Ripple doesn't introduce variables
                }
            }
            ast::Term::Match(pattern) => {
                // Match patterns can define variables or reference them (via &)
                // We need to traverse the pattern to find Match::Reference nodes
                self.visit_match(pattern);
            }
            ast::Term::Block(block) => {
                self.visit_block(block);
            }
            ast::Term::Function(func) => {
                if let Some(body) = &func.body {
                    self.visit_block(body);
                }
            }
            ast::Term::Access(access) => {
                if let Some(ast::AccessSource::Identifier(name)) = &access.source {
                    self.visit_identifier(name, access.accessors.clone());
                }
                // $ doesn't capture variables, so skip AccessSource::Parameter
                if let Some(argument) = &access.argument {
                    for field in argument {
                        if let ast::FieldValue::Chain(chain) = &field.value {
                            self.visit_chain(chain);
                        }
                    }
                }
            }
            ast::Term::Builtin(_) => {}
            ast::Term::TailCall(tail_call) => {
                if let Some(name) = &tail_call.identifier {
                    self.visit_identifier(name, tail_call.accessors.clone());
                }
                if let Some(argument) = &tail_call.argument {
                    for field in argument {
                        if let ast::FieldValue::Chain(chain) = &field.value {
                            self.visit_chain(chain);
                        }
                    }
                }
            }
            ast::Term::Equality => {}
            ast::Term::Not => {}
            ast::Term::Spawn(term) => {
                self.visit_term(term);
            }
            ast::Term::Self_ => {}
            ast::Term::Process(_) => {}
            ast::Term::Select(sources) => {
                // Visit all source chains (if explicit sources provided)
                if let Some(sources) = sources {
                    for source in sources {
                        self.visit_chain(source);
                    }
                }
            }
            ast::Term::Reference(access) => {
                // Reference to a value - same variable capture as Access
                if let Some(ast::AccessSource::Identifier(name)) = &access.source {
                    self.visit_identifier(name, access.accessors.clone());
                }
                if let Some(argument) = &access.argument {
                    for field in argument {
                        if let ast::FieldValue::Chain(chain) = &field.value {
                            self.visit_chain(chain);
                        }
                    }
                }
            }
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
            ast::Match::Reference(ty) => {
                // Check if this is a bare identifier reference (could be a variable)
                if let ast::Type::Identifier { name, arguments } = ty
                    && arguments.is_empty()
                {
                    // This could be a variable reference like &x
                    self.visit_identifier(name, vec![]);
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
                for (_, nested_pattern) in &partial.fields {
                    if let Some(nested_pattern) = nested_pattern {
                        self.visit_match(nested_pattern);
                    }
                }
            }
            // These don't contain nested patterns or references
            ast::Match::Identifier(_)
            | ast::Match::Literal(_)
            | ast::Match::Star
            | ast::Match::Placeholder
            | ast::Match::Type(_) => {}
        }
    }
}
