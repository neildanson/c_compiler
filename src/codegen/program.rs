use super::{Function, StaticVariable, TopLevel};
use crate::{error::CompilerError, tacky};
use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub struct Program {
    top_level: Vec<TopLevel>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for top_level in &self.top_level {
            write!(f, "{}", top_level)?;
            writeln!(f)?;
        }
        if cfg!(target_os = "linux") {
            writeln!(f, ".section .note.GNU-stack,\"\",@progbits")?;
        }

        Ok(())
    }
}

impl TryFrom<tacky::Program> for Program {
    type Error = CompilerError;
    fn try_from(ast: tacky::Program) -> Result<Self, Self::Error> {
        let mut top_level = Vec::new();
        for tl in ast.top_level {
            match tl {
                tacky::TopLevel::Function(f) => {
                    top_level.push(TopLevel::Function(Function::try_from(f, &ast.symbols)?));
                }
                tacky::TopLevel::StaticVariable(s) => {
                    top_level.push(TopLevel::StaticVariable(StaticVariable::try_from(s)?));
                }
            }
        }


        Ok(Program { top_level })
    }
}
