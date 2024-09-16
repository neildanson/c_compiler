use super::Function;
use crate::{error::CompilerError, tacky};
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

impl TryFrom<tacky::Program> for Program {
    type Error = CompilerError;
    fn try_from(ast: tacky::Program) -> Result<Self, Self::Error> {
        let function = ast.function.try_into()?;
        Ok(Program { function })
    }
}
