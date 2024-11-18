use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

use crate::{error::CompilerError, parse::Type, tacky, validate::StaticInit};

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
            if self.global || self.name == "main" {
                writeln!(f, "\t.globl {}", format_fn_call(&self.name))?;
            }
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
    fn try_from(ast: tacky::Function) -> Result<Self, Self::Error> {
        if let Some(body_ast) = ast.body {
            let mut body = Vec::new();

            for (i, (ty, parameter)) in ast.params.into_iter().enumerate().rev() {
                body.insert(
                    0,
                    Instruction::Mov {
                        assembly_type: (&ty).into(),
                        src: Operand::arg(i),
                        dst: Operand::Pseudo(parameter),
                    },
                );
            }

            for statement in body_ast {
                let mut instructions: Vec<_> = statement.try_into()?;
                body.append(&mut instructions);
            }

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
    pub fn fixup(&mut self, symbol_table: &HashMap<String, self::AsmSymTabEntry>) {
        if let Some(body) = &self.body {
            let (body, stack_size) = rewrite_pseudo_with_stack(body.clone(), symbol_table);
            let mut body = fixup_stack_operations(&body);
            let size = ((stack_size * 4) + 15) & !15;

            body.insert(
                0,
                Instruction::Binary {
                    op: BinaryOp::Sub,
                    assembly_type: AssemblyType::QuadWord,
                    src2: Operand::Immediate { imm: size as i64 },
                    dst: Operand::Register(Reg::SP),
                },
            ); //* 4 as ints are 4 bytes */
            self.body = Some(body);
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct StaticVariable {
    identfiier: String,
    global: bool,
    alignment: i32,
    value: StaticInit,
}

impl Display for StaticVariable {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        if self.global {
            if cfg!(target_os = "macos") {
                writeln!(f, "\t.globl _{}", self.identfiier)?;
            } else {
                writeln!(f, "\t.globl {}", self.identfiier)?;
            }
        }
        if self.value == StaticInit::IntInit(0) {
            writeln!(f, "\t.balign {}", self.alignment)?;
            writeln!(f, "\t.bss")?;
            if cfg!(target_os = "macos") {
                writeln!(f, "_{}:", self.identfiier)?;
            } else {
                writeln!(f, "{}:", self.identfiier)?;
            }
            writeln!(f, "\t.zero {}", self.alignment)
        } else {
            writeln!(f, "\t.data")?;
            writeln!(f, "\t.balign {}", self.alignment)?;
            if cfg!(target_os = "macos") {
                writeln!(f, "_{}:", self.identfiier)?;
            } else {
                writeln!(f, "{}:", self.identfiier)?;
            }

            match self.value {
                StaticInit::IntInit(value) => writeln!(f, "\t.long {}", value),
                StaticInit::LongInit(value) => writeln!(f, "\t.quad {}", value),
            }
        }
    }
}

impl TryFrom<tacky::StaticVariable> for StaticVariable {
    type Error = CompilerError;
    fn try_from(ast: tacky::StaticVariable) -> Result<Self, Self::Error> {
        let alignment = match ast.ty {
            Type::Int => 4,
            Type::Long => 8,
            _ => panic!("Unsupported type"),
        };
        Ok(StaticVariable {
            identfiier: ast.identifier,
            global: ast.global,
            alignment,
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
