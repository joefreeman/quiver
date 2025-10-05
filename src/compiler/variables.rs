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
            ast::Term::Identifier(name) => {
                self.visit_identifier(name, vec![]);
            }
            ast::Term::Tuple(tuple) => {
                for field in &tuple.fields {
                    if let ast::FieldValue::Chain(chain) = &field.value {
                        self.visit_chain(chain);
                    }
                    // Ripple doesn't introduce variables
                }
            }
            ast::Term::Assignment(_) => {
                // Assignments don't reference variables, they define them
            }
            ast::Term::Block(block) => {
                self.visit_block(&block);
            }
            ast::Term::FunctionDefinition(func) => {
                self.visit_block(&func.body);
            }
            ast::Term::FunctionCall(target) => {
                match target {
                    ast::FunctionCall::Identifier { name, accessors } => {
                        self.visit_identifier(name, accessors.clone());
                    }
                    ast::FunctionCall::Builtin(_) => {
                        // Builtins don't reference variables
                    }
                }
            }
            ast::Term::MemberAccess(member_access) => {
                if let Some(name) = &member_access.identifier {
                    self.visit_identifier(name, member_access.accessors.clone());
                }
            }
            ast::Term::Import(_) => {}
            ast::Term::Builtin(_) => {}
            ast::Term::TailCall(tail_call) => {
                if let Some(name) = &tail_call.identifier {
                    self.visit_identifier(name, tail_call.accessors.clone());
                }
            }
            ast::Term::Equality => {}
            ast::Term::Not => {}
            ast::Term::Spawn(term) => {
                self.visit_term(term);
            }
            ast::Term::Send(send_call) => {
                self.visit_identifier(&send_call.name, send_call.accessors.clone());
            }
            ast::Term::Self_ => {}
            ast::Term::Receive(receive) => {
                if let Some(block) = &receive.block {
                    self.visit_block(block);
                }
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
