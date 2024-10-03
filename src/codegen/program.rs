use super::Function;
use crate::{error::CompilerError, tacky};
use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub struct Program {
    functions: Vec<Function>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for function in &self.functions {
            write!(f, "{}", function)?;
        }
        Ok(())
    }
}

impl TryFrom<tacky::Program> for Program {
    type Error = CompilerError;
    fn try_from(ast: tacky::Program) -> Result<Self, Self::Error> {
        let functions = ast
            .functions
            .into_iter()
            .map(Function::try_from)
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Program { functions })
    }
}
