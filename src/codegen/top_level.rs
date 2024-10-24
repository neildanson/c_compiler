use std::{collections::HashMap, fmt::{Display, Formatter}};

use crate::{error::CompilerError, tacky, validate::StaticAttr};

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
            //if self.global || self.name == "main" {
                writeln!(f, "\t.globl {}", format_fn_call(&self.name))?;
            //}
            writeln!(f, "\t.text")?;
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
    fn try_from(
        ast: tacky::Function,
    ) -> Result<Self, Self::Error> {
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

            //TODO how to handle statics without a symbol_table?
            
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

impl Function { 
    pub fn fixup(&mut self, static_variables: &HashMap<String, StaticAttr>) {
        if let Some(body) = &self.body {
            let (mut body, stack_size) = rewrite_pseudo_with_stack(body.clone(), static_variables);
            let size = ((stack_size * 4) + 15) & !15;
            body.insert(0, Instruction::AllocateStack(size)); //* 4 as ints are 4 bytes */
            self.body = Some(fixup_stack_operations(body));
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
        if self.value == 0 {
            writeln!(f, "\t.bss")?;
            writeln!(f, "\t.align 4")?;
            writeln!(f, "{}:", self.identfiier)?;
            writeln!(f, "\t.zero 4")
        } else {
            writeln!(f, "\t.data")?;
            writeln!(f, "\t.align 4")?;
            writeln!(f, "{}:", self.identfiier)?;
            writeln!(f, "\t.long {}", self.value)
        }
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
