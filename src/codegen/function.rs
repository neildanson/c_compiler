use std::fmt::{Display, Formatter};

use crate::{error::CompilerError, tacky};

use super::*;

#[derive(Debug, PartialEq)]
pub struct Function {
    name: String,
    body: Option<Vec<Instruction>>,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        if let Some(body) = &self.body {       
            writeln!(f, "\t.globl {}", format_fn_call(&self.name))?;
            writeln!(f, "{}:", format_fn_call(&self.name))?;
            writeln!(f, "\t# Function Preamble")?;
            writeln!(f, "\tpushq %rbp")?;
            writeln!(f, "\tmovq %rsp, %rbp")?;
            writeln!(f)?;
            for instruction in body {
                writeln!(f, "{}", instruction)?;
            }
        }
        Ok(())
    }
}

impl TryFrom<tacky::Function> for Function {
    type Error = CompilerError;
    fn try_from(ast: tacky::Function) -> Result<Self, Self::Error> {
        if let Some(body_ast) = ast.body {
            let mut body = Vec::new();

            for parameter in ast.params {
                body.insert(0, Instruction::Mov { src : Operand::Register(Reg::DI), dst : Operand::Pseudo(parameter) });
            }

            for statement in body_ast {
                let mut instructions: Vec<_> = statement.try_into()?;
                body.append(&mut instructions);
            }
            let (mut body, stack_size) = rewrite_pseudo_with_stack(body);
            body.insert(0, Instruction::AllocateStack(stack_size));
            let body = fixup_stack_operations(body);
            Ok(Function {
                name: ast.name,
                body: Some(body),
            })
        }
        else {
            return Ok(Function {
                name: ast.name,
                body:None,
            });
        }
    }
}
