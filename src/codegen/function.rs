use std::collections::HashMap;
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
        writeln!(f, "\tpushq %rbp")?;
        writeln!(f, "\tmovq %rsp, %rbp")?;
        for instruction in &self.body {
            writeln!(f, "{}", instruction)?;
        }
        Ok(())
    }
}

fn fixup_pseudo(name: String, stack: &mut HashMap<String, i32>) -> Operand {
    if let Some(offset) = stack.get(&name) {
        Operand::Stack(*offset)
    } else {
        let offset = (stack.len() + 1) as i32 * 4;
        stack.insert(name, offset);
        Operand::Stack(offset)
    }
}

fn replace_pseudo_with_stack(body: Vec<Instruction>) -> (Vec<Instruction>, usize) {
    let mut stack = HashMap::new();
    let mut new_body = Vec::new();
    for instruction in body {
        match instruction {
            Instruction::Mov { src, dst } => {
                let src = match src {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack),
                    _ => src,
                };
                let dst = match dst {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack),
                    _ => dst,
                };
                new_body.push(Instruction::Mov { src, dst });
            }
            Instruction::Unary { op, dst } => {
                let dst = match dst {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack),
                    _ => dst,
                };
                new_body.push(Instruction::Unary { op, dst });
            }
            Instruction::Binary { op, src2, dst } => {
                let src2 = match src2 {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack),
                    _ => src2,
                };
                let dst = match dst {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack),
                    _ => dst,
                };
                new_body.push(Instruction::Binary { op, src2, dst });
            }
            any_other => new_body.push(any_other),
        }
    }
    (new_body, stack.len())
}

fn fixup_stack_operations(body: Vec<Instruction>) -> Vec<Instruction> {
    let mut new_body = Vec::new();
    for instruction in body {
        match instruction.clone() {
            Instruction::Mov { src, dst } => {
                if let Operand::Stack(_) = src {
                    if let Operand::Stack(_) = dst {
                        new_body.push(Instruction::Mov {
                            src,
                            dst: Operand::Register(Reg::R10),
                        });
                        new_body.push(Instruction::Mov {
                            src: Operand::Register(Reg::R10),
                            dst,
                        });
                        continue;
                    }
                }
                new_body.push(instruction.clone());
            }
            Instruction::Binary { op, src2, dst } => {
                if let Operand::Stack(_) = dst {
                    if let Operand::Stack(_) = src2 {
                        match op {
                            BinaryOp::Add | BinaryOp::Sub => {
                                new_body.push(Instruction::Mov {
                                    src: src2,
                                    dst: Operand::Register(Reg::R10),
                                });
                                new_body.push(Instruction::Binary {
                                    op,
                                    src2: Operand::Register(Reg::R10),
                                    dst,
                                });
                                continue;
                            }

                            _ => continue,
                        }
                    } else {
                        if op == BinaryOp::Mult {
                            new_body.push(Instruction::Mov {
                                src: dst.clone(),
                                dst: Operand::Register(Reg::R11),
                            });
                            new_body.push(Instruction::Binary {
                                op,
                                src2,
                                dst: Operand::Register(Reg::R11),
                            });
                            new_body.push(Instruction::Mov {
                                src: Operand::Register(Reg::R11),
                                dst,
                            });
                            continue;
                        }
                    }
                }
                new_body.push(instruction.clone());
            }
            _ => new_body.push(instruction),
        }
    }
    new_body
}

impl From<tacky::Function> for Function {
    fn from(ast: tacky::Function) -> Self {
        let mut body = Vec::new();
        for statement in ast.body {
            let mut instructions: Vec<_> = statement.into();
            body.append(&mut instructions);
        }

        let (mut body, stack_size) = replace_pseudo_with_stack(body);
        body.insert(0, Instruction::AllocateStack(stack_size));
        let body = fixup_stack_operations(body);
        Function {
            name: ast.name,
            body,
        }
    }
}