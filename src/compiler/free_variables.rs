use crate::ast;
use std::collections::HashSet;

pub fn collect_free_variables(
    expression: &ast::Expression,
    function_parameters: &HashSet<String>,
    defined_variables: &dyn Fn(&str) -> bool,
) -> HashSet<String> {
    let mut collector = FreeVariableCollector {
        function_parameters,
        defined_variables,
        captures: HashSet::new(),
    };
    collector.visit_expression(expression);
    collector.captures
}

struct FreeVariableCollector<'a> {
    function_parameters: &'a HashSet<String>,
    defined_variables: &'a dyn Fn(&str) -> bool,
    captures: HashSet<String>,
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
        for term in &sequence.terms {
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
                self.visit_expression(&func.body.expression);
            }
            ast::Value::Block(block) => {
                self.visit_expression(&block.expression);
            }
            ast::Value::Parameter(_) => {}
            ast::Value::MemberAccess(member_access) => {
                self.visit_identifier(&member_access.target);
            }
            ast::Value::Import(_) => {}
            ast::Value::Parenthesized(expr) => {
                self.visit_expression(expr);
            }
        }
    }

    fn visit_operation(&mut self, operation: &ast::Operation) {
        match operation {
            ast::Operation::Operator(_) => {}
            ast::Operation::Tuple(tuple) => {
                for field in &tuple.fields {
                    if let ast::OperationTupleFieldValue::Chain(chain) = &field.value {
                        self.visit_chain(chain);
                    }
                }
            }
            ast::Operation::Block(block) => {
                self.visit_expression(&block.expression);
            }
            ast::Operation::MemberAccess(member_access) => {
                self.visit_identifier(&member_access.target);
            }
            ast::Operation::FieldAccess(_) => {}
            ast::Operation::PositionalAccess(_) => {}
            ast::Operation::TailCall(identifier) => {
                self.visit_identifier(identifier);
            }
        }
    }

    fn visit_identifier(&mut self, identifier: &str) {
        if !self.function_parameters.contains(identifier) && (self.defined_variables)(identifier) {
            self.captures.insert(identifier.to_string());
        }
    }
}
