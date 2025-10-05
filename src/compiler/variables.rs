use crate::ast;
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Capture {
    pub base: String,
    pub accessors: Vec<ast::AccessPath>,
}

pub fn collect_free_variables(
    block: &ast::Block,
    function_parameters: &HashSet<String>,
    defined_variables: &dyn Fn(&str, &[ast::AccessPath]) -> bool,
) -> Vec<Capture> {
    let mut collector = FreeVariableCollector {
        function_parameters,
        defined_variables,
        captures: HashSet::new(),
    };
    collector.visit_block(block);
    collector.captures.into_iter().collect()
}

struct FreeVariableCollector<'a> {
    function_parameters: &'a HashSet<String>,
    defined_variables: &'a dyn Fn(&str, &[ast::AccessPath]) -> bool,
    captures: HashSet<Capture>,
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
            ast::Term::BindMatch(_) => {
                // Bind matches don't reference variables, they define them
            }
            ast::Term::PinMatch(pattern) => {
                // Pin matches may reference variables (for pins) and define variables (for binds)
                // Visit the pattern to find any pinned variable references
                self.visit_match_pattern(pattern);
            }
            ast::Term::Block(block) => {
                self.visit_block(&block);
            }
            ast::Term::Function(func) => {
                self.visit_block(&func.body);
            }
            ast::Term::Access(access) => {
                if let Some(name) = &access.identifier {
                    self.visit_identifier(name, access.accessors.clone());
                }
                if let Some(argument) = &access.argument {
                    for field in &argument.fields {
                        if let ast::FieldValue::Chain(chain) = &field.value {
                            self.visit_chain(chain);
                        }
                    }
                }
            }
            ast::Term::Import(_) => {}
            ast::Term::Builtin(_) => {}
            ast::Term::TailCall(tail_call) => {
                if let Some(name) = &tail_call.identifier {
                    self.visit_identifier(name, tail_call.accessors.clone());
                }
                if let Some(argument) = &tail_call.argument {
                    for field in &argument.fields {
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
            ast::Term::Receive(receive) => {
                if let Some(block) = &receive.block {
                    self.visit_block(block);
                }
            }
            ast::Term::Await => {}
        }
    }

    fn visit_match_pattern(&mut self, pattern: &ast::Match) {
        match pattern {
            ast::Match::Identifier(_) => {
                // Regular identifiers in matches define variables, not reference them
            }
            ast::Match::Literal(_) => {}
            ast::Match::Tuple(tuple) => {
                for field in &tuple.fields {
                    self.visit_match_pattern(&field.pattern);
                }
            }
            ast::Match::Partial(_) => {
                // Partial patterns define variables, not reference them
            }
            ast::Match::Star => {}
            ast::Match::Placeholder => {}
            ast::Match::Pin(inner) => {
                // Pin patterns reference existing variables
                self.visit_pin_pattern(inner);
            }
            ast::Match::Bind(inner) => {
                // Bind patterns define variables (explicitly switch back to bind mode)
                self.visit_match_pattern(inner);
            }
        }
    }

    fn visit_pin_pattern(&mut self, pattern: &ast::Match) {
        match pattern {
            ast::Match::Identifier(name) => {
                // Pinned identifiers reference variables
                self.visit_identifier(name, vec![]);
            }
            ast::Match::Literal(_) => {}
            ast::Match::Tuple(tuple) => {
                // In pin mode, recurse with pin semantics
                for field in &tuple.fields {
                    self.visit_pin_pattern(&field.pattern);
                }
            }
            ast::Match::Partial(_) => {
                // Partial patterns in pin mode still reference fields, not the base
            }
            ast::Match::Star => {}
            ast::Match::Placeholder => {}
            ast::Match::Pin(inner) => {
                // Nested pin - continue with pin semantics
                self.visit_pin_pattern(inner);
            }
            ast::Match::Bind(inner) => {
                // Explicit bind in pin context - switch back to bind mode
                self.visit_match_pattern(inner);
            }
        }
    }

    fn visit_identifier(&mut self, identifier: &str, accessors: Vec<ast::AccessPath>) {
        if !self.function_parameters.contains(identifier)
            && (self.defined_variables)(identifier, &accessors)
        {
            self.captures.insert(Capture {
                base: identifier.to_string(),
                accessors,
            });
        }
    }
}
