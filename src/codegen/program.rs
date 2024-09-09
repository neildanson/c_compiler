use super::function::Function;
use crate::tacky;
use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub struct Program {
    function: Function,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.function)
    }
}

impl From<tacky::Program> for Program {
    fn from(ast: tacky::Program) -> Self {
        let function = ast.function.into();
        Program { function }
    }
}
