use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

use crate::{error::CompilerError, tacky, validate::Symbol};

use super::*;

#[derive(Debug, PartialEq)]
pub struct Function {
    name: String,
    global: bool,
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

impl Function {
    pub fn try_from(
        ast: tacky::Function,
        symbols: &HashMap<String, Symbol>,
    ) -> Result<Function, CompilerError> {
        if let Some(body_ast) = ast.body {
            let mut body = Vec::new();

            for (i, parameter) in ast.params.into_iter().enumerate().rev() {
                body.insert(
                    0,
                    Instruction::Mov {
                        src: Operand::arg(i),
                        dst: Operand::Pseudo(parameter),
                    },
                );
            }

            for statement in body_ast {
                let mut instructions: Vec<_> = statement.try_into()?;
                body.append(&mut instructions);
            }

            let (mut body, stack_size) = rewrite_pseudo_with_stack(body, symbols);
            let size = ((stack_size * 4) + 15) & !15;
            body.insert(0, Instruction::AllocateStack(size)); //* 4 as ints are 4 bytes */
            let body = fixup_stack_operations(body);
            Ok(Function {
                name: ast.name,
                global: ast.global,
                body: Some(body),
            })
        } else {
            Ok(Function {
                name: ast.name,
                global: ast.global,
                body: None,
            })
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct StaticVariable {
    identfiier: String,
    global: bool,
    value: i32,
}

//TODO properly: Implement TryFrom for StaticVariable
impl Display for StaticVariable {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        if self.global {
            writeln!(f, "\t.globl {}", self.identfiier)?;
        }
        writeln!(f, "{}:", self.identfiier)?;
        writeln!(f, "\t.long {}", self.value)
    }
}

//From?
impl TryFrom<tacky::StaticVariable> for StaticVariable {
    type Error = CompilerError;
    fn try_from(ast: tacky::StaticVariable) -> Result<Self, Self::Error> {
        Ok(StaticVariable {
            identfiier: ast.identifier,
            global: ast.global,
            value: ast.init,
        })
    }
}

#[derive(Debug, PartialEq)]
pub enum TopLevel {
    Function(Function),
    StaticVariable(StaticVariable),
}

impl Display for TopLevel {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            TopLevel::Function(function) => write!(f, "{}", function),
            TopLevel::StaticVariable(static_variable) => write!(f, "{}", static_variable),
        }
    }
}
