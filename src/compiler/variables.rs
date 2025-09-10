use crate::ast;
use std::collections::HashSet;

pub fn collect_free_variables(
    block: &ast::Block,
    function_parameters: &HashSet<String>,
    defined_variables: &dyn Fn(&str) -> bool,
) -> HashSet<String> {
    let mut collector = FreeVariableCollector {
        function_parameters,
        defined_variables,
        captures: HashSet::new(),
    };
    collector.visit_block(block);
    collector.captures
}

struct FreeVariableCollector<'a> {
    function_parameters: &'a HashSet<String>,
    defined_variables: &'a dyn Fn(&str) -> bool,
    captures: HashSet<String>,
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
        for term in &expression.terms {
            self.visit_term(term);
        }
    }

    fn visit_term(&mut self, term: &ast::Term) {
        match term {
            ast::Term::Assignment { pattern: _, value } => {
                self.visit_chain(value);
            }
            ast::Term::Chain(chain) => {
                self.visit_chain(chain);
            }
        }
    }

    fn visit_chain(&mut self, chain: &ast::Chain) {
        self.visit_value(&chain.value);
        for operation in &chain.operations {
            self.visit_operation(operation);
        }
    }

    fn visit_value(&mut self, value: &ast::Value) {
        match value {
            ast::Value::Literal(_) => {}
            ast::Value::Tuple(tuple) => {
                for field in &tuple.fields {
                    self.visit_chain(&field.value);
                }
            }
            ast::Value::FunctionDefinition(func) => {
                self.visit_block(&func.body);
            }
            ast::Value::Block(block) => {
                self.visit_block(&block);
            }
            ast::Value::Parameter(_) => {}
            ast::Value::MemberAccess(member_access) => {
                self.visit_identifier(&member_access.target);
            }
            ast::Value::Import(_) => {}
        }
    }

    fn visit_operation(&mut self, operation: &ast::Operation) {
        match operation {
            ast::Operation::Tuple(tuple) => {
                for field in &tuple.fields {
                    if let ast::OperationTupleFieldValue::Chain(chain) = &field.value {
                        self.visit_chain(chain);
                    }
                }
            }
            ast::Operation::Block(block) => {
                self.visit_block(&block);
            }
            ast::Operation::Parameter(_) => {}
            ast::Operation::MemberAccess(member_access) => {
                self.visit_identifier(&member_access.target);
            }
            ast::Operation::FieldAccess(_) => {}
            ast::Operation::PositionalAccess(_) => {}
            ast::Operation::TailCall(identifier) => {
                self.visit_identifier(identifier);
            }
            ast::Operation::Builtin(_) => {}
            ast::Operation::Equality => {}
            ast::Operation::Not => {}
        }
    }

    fn visit_identifier(&mut self, identifier: &str) {
        if !self.function_parameters.contains(identifier) && (self.defined_variables)(identifier) {
            self.captures.insert(identifier.to_string());
        }
    }
}
