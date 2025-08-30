pub mod ast;
pub mod bytecode;
pub mod compiler;
pub mod parser;
pub mod vm;

use compiler::Compiler;
use vm::{VM, Value};

pub struct Quiver {
    vm: VM,
    compiler: Compiler,
}

impl Quiver {
    pub fn new() -> Self {
        Self {
            vm: VM::new(),
            compiler: Compiler::new(),
        }
    }

    pub fn evaluate(&mut self, _source: &str) -> Result<Option<Value>, Error> {
        // let program = parser::parse(source).map_err(Error::ParseError);
        // let result = self.compiler.evaluate(&program, &mut self.vm).map_err(QuiverError::CompileError)
        // TODO
        Ok(Some(Value::Integer(0)))
    }
}

#[derive(Debug)]
pub enum Error {
    ParseError(parser::Error),
    RuntimeError(vm::Error),
    CompileError(compiler::Error),
}
