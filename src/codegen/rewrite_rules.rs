use std::collections::HashMap;

use super::*;

fn fixup_pseudo(
    name: String,
    stack: &mut HashMap<String, i32>,
    parameter: bool,
) -> Operand {
    if let Some(offset) = stack.get(&name) {
        Operand::Stack(*offset)
    } else {
        //if let Some(symbol) = symbols.get(&name) {
        //    if symbol.attributes.is_static() {
        //        return Operand::Data(name);
        //    }
        //}

        if parameter {
            let offset = (stack.len() + 1) as i32 * 8;
            stack.insert(name, -offset);
            Operand::local(offset)
        } else {
            let offset = (stack.len() + 1) as i32 * 4;
            stack.insert(name, -offset);
            Operand::local(offset)
        }
    }
}

pub(crate) fn rewrite_pseudo_with_stack(
    body: Vec<Instruction>,
) -> (Vec<Instruction>, usize) {
    let mut stack = HashMap::new();
    let mut new_body = Vec::new();
    for instruction in body {
        match instruction {
            Instruction::Mov { src, dst } => {
                let src = match src {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack, false),
                    _ => src,
                };
                let dst = match dst {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack, false),
                    _ => dst,
                };
                new_body.push(Instruction::Mov { src, dst });
            }
            Instruction::Unary { op, dst } => {
                let dst = match dst {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack, false),
                    _ => dst,
                };
                new_body.push(Instruction::Unary { op, dst });
            }
            Instruction::Binary { op, src2, dst } => {
                let src2 = match src2 {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack, false),
                    _ => src2,
                };
                let dst = match dst {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack, false),
                    _ => dst,
                };
                new_body.push(Instruction::Binary { op, src2, dst });
            }
            Instruction::Cmp(lhs, rhs) => {
                let lhs = match lhs {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack, false),
                    _ => lhs,
                };
                let rhs = match rhs {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack, false),
                    _ => rhs,
                };
                new_body.push(Instruction::Cmp(lhs, rhs));
            }
            Instruction::SetCC(cond, operand) => {
                let operand = match operand {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack, false),
                    _ => operand,
                };
                new_body.push(Instruction::SetCC(cond, operand));
            }
            Instruction::Push(operand) => {
                let operand = match operand {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack, true),
                    _ => operand,
                };
                new_body.push(Instruction::Push(operand));
            }
            Instruction::Idiv { src } => new_body.push(Instruction::Idiv {
                src: match src {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack, false),
                    _ => src,
                },
            }),
            any_other => new_body.push(any_other),
        }
    }

    (new_body, stack.len())
}

pub(crate) fn fixup_stack_operations(body: Vec<Instruction>) -> Vec<Instruction> {
    let mut new_body = Vec::new();
    for instruction in body {
        match instruction.clone() {
            Instruction::Mov { src, dst } => {
                if let Operand::Stack(_) | Operand::Data(_) = src {
                    if let Operand::Stack(_) | Operand::Data(_) = dst {
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
            Instruction::Cmp(lhs, rhs) => {
                if let Operand::Stack(_) | Operand::Data(_) = lhs {
                    if let Operand::Stack(_) | Operand::Data(_) = rhs {
                        new_body.push(Instruction::Mov {
                            src: lhs,
                            dst: Operand::Register(Reg::R10),
                        });
                        new_body.push(Instruction::Cmp(Operand::Register(Reg::R10), rhs));
                        continue;
                    }
                }
                if let Operand::Immediate { imm: _ } = rhs {
                    new_body.push(Instruction::Mov {
                        src: rhs,
                        dst: Operand::Register(Reg::R10),
                    });
                    new_body.push(Instruction::Cmp(lhs, Operand::Register(Reg::R10)));
                    continue;
                }
                new_body.push(instruction.clone());
            }
            Instruction::Binary { op, src2, dst } if op == BinaryOp::Mult => {
                if let Operand::Stack(_) = dst {
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
                new_body.push(instruction.clone());
            }

            Instruction::Binary { op, src2, dst } => {
                if let Operand::Stack(_) | Operand::Data(_) = dst {
                    if let Operand::Stack(_) | Operand::Data(_) = src2 {
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
                    }
                }
                new_body.push(instruction.clone());
            }
            Instruction::Idiv { src } => {
                if let Operand::Immediate { imm: _ } = src {
                    new_body.push(Instruction::Mov {
                        src,
                        dst: Operand::Register(Reg::R10),
                    });
                    new_body.push(Instruction::Idiv {
                        src: Operand::Register(Reg::R10),
                    });
                    continue;
                }
                new_body.push(instruction.clone());
            }
            _ => new_body.push(instruction),
        }
    }
    new_body
}
