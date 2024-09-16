use std::fmt::{Display, Formatter, Result};

use crate::tacky;

use super::*;

#[derive(Debug, PartialEq)]
pub struct Function {
    name: String,
    body: Vec<Instruction>,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "\t.globl {}", self.name)?;
        writeln!(f, "{}:", self.name)?;
        writeln!(f, "\t# Function Preamble")?;
        writeln!(f, "\tpushq %rbp")?;
        writeln!(f, "\tmovq %rsp, %rbp")?;
        writeln!(f)?;
        for instruction in &self.body {
            writeln!(f, "{}", instruction)?;
        }
        Ok(())
    }
}

impl From<tacky::Function> for Function {
    fn from(ast: tacky::Function) -> Self {
        let mut body = Vec::new();
        for statement in ast.body {
            let mut instructions: Vec<_> = statement.into();
            body.append(&mut instructions);
        }

        let (mut body, stack_size) = rewrite_pseudo_with_stack(body);
        body.insert(0, Instruction::AllocateStack(stack_size));
        let body = fixup_stack_operations(body);
        Function {
            name: ast.name,
            body,
        }
    }
}
