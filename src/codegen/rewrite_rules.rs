use std::collections::HashMap;
use super::*;

fn fixup_pseudo(
    name: String,
    stack: &mut HashMap<String, i32>,
    parameter: bool,
    symbols: &HashMap<String, AsmSymTabEntry>,
) -> Operand {
    if let Some(offset) = stack.get(&name) {
        Operand::Stack(*offset)
    } else {
        let symbol = symbols.get(&name);
        //TODO do this bit better as it's O(n)
        let stack_pos = stack.iter().map(|(_k, v)| -v).max().unwrap_or(0);

        match (symbol, parameter) {
            (Some(AsmSymTabEntry::ObjEntry(_, true)), _) => {
                Operand::Data(name)
            }
            (Some(AsmSymTabEntry::ObjEntry(ty, false)), _) => {
                let offset = stack_pos + (ty.size() as i32);
                stack.insert(name, -offset);
                Operand::local(offset)
            }
            (_, true) => {
                let offset = stack_pos + (AssemblyType::QuadWord.size() as i32);
                stack.insert(name, -offset);
                Operand::local(offset)
            }
            _ => panic!("Symbol not found {} {:?}", name, symbol),
        }
    }
}

pub(crate) fn rewrite_pseudo_with_stack(
    body: Vec<Instruction>,
    static_variables: &HashMap<String, AsmSymTabEntry>,
) -> (Vec<Instruction>, usize) {
    let mut stack = HashMap::new();
    let mut new_body = Vec::new();
    for instruction in body {
        match instruction {
            Instruction::Mov {
                assembly_type,
                src,
                dst,
            } => {
                let src = match src {
                    Operand::Pseudo(name) => {
                        fixup_pseudo(name, &mut stack, false, static_variables)
                    }
                    _ => src,
                };
                let dst = match dst {
                    Operand::Pseudo(name) => {
                        fixup_pseudo(name, &mut stack, false, static_variables)
                    }
                    _ => dst,
                };
                new_body.push(Instruction::Mov {
                    assembly_type,
                    src,
                    dst,
                });
            }
            Instruction::Unary {
                op,
                assembly_type,
                dst,
            } => {
                let dst = match dst {
                    Operand::Pseudo(name) => {
                        fixup_pseudo(name, &mut stack, false, static_variables)
                    }
                    _ => dst,
                };
                new_body.push(Instruction::Unary {
                    op,
                    assembly_type,
                    dst,
                });
            }
            Instruction::Binary {
                op,
                assembly_type,
                src2,
                dst,
            } => {
                let src2 = match src2 {
                    Operand::Pseudo(name) => {
                        fixup_pseudo(name, &mut stack, false, static_variables)
                    }
                    _ => src2,
                };
                let dst = match dst {
                    Operand::Pseudo(name) => {
                        fixup_pseudo(name, &mut stack, false, static_variables)
                    }
                    _ => dst,
                };
                new_body.push(Instruction::Binary {
                    op,
                    assembly_type,
                    src2,
                    dst,
                });
            }
            Instruction::Cmp(assembly_type, lhs, rhs) => {
                let lhs = match lhs {
                    Operand::Pseudo(name) => {
                        fixup_pseudo(name, &mut stack, false, static_variables)
                    }
                    _ => lhs,
                };
                let rhs = match rhs {
                    Operand::Pseudo(name) => {
                        fixup_pseudo(name, &mut stack, false, static_variables)
                    }
                    _ => rhs,
                };
                new_body.push(Instruction::Cmp(assembly_type, lhs, rhs));
            }
            Instruction::SetCC(cond, operand) => {
                let operand = match operand {
                    Operand::Pseudo(name) => {
                        fixup_pseudo(name, &mut stack, false, static_variables)
                    }
                    _ => operand,
                };
                new_body.push(Instruction::SetCC(cond, operand));
            }
            Instruction::Push(operand) => {
                let operand = match operand {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack, true, static_variables),
                    _ => operand,
                };
                new_body.push(Instruction::Push(operand));
            }
            Instruction::Idiv { assembly_type, src } => new_body.push(Instruction::Idiv {
                assembly_type,
                src: match src {
                    Operand::Pseudo(name) => {
                        fixup_pseudo(name, &mut stack, false, static_variables)
                    }
                    _ => src,
                },
            }),
            Instruction::Movsx { src, dst } => new_body.push(Instruction::Movsx {
                src: match src {
                    Operand::Pseudo(name) => {
                        fixup_pseudo(name, &mut stack, false, static_variables)
                    }
                    _ => src,
                },
                dst: match dst {
                    Operand::Pseudo(name) => {
                        fixup_pseudo(name, &mut stack, false, static_variables)
                    }
                    _ => dst,
                },
            }),
            any_other => new_body.push(any_other),
        }
    }
    let stack_pos : i32 = stack.iter().map(|(_k, v)| -v).max().unwrap_or(0);
    (new_body, stack_pos.abs() as usize)
}

pub(crate) fn fixup_stack_operations(body: Vec<Instruction>) -> Vec<Instruction> {
    let mut new_body = Vec::new();
    for instruction in body {
        match instruction.clone() {
            Instruction::Mov {
                assembly_type: AssemblyType::LongWord,
                src,
                dst,
            } => {
                if let Operand::Stack(_) | Operand::Data(_) = src {
                    if let Operand::Stack(_) | Operand::Data(_) = dst {
                        new_body.push(Instruction::Mov {
                            assembly_type: AssemblyType::LongWord,
                            src,
                            dst: Operand::Register(Reg::R10),
                        });
                        new_body.push(Instruction::Mov {
                            assembly_type: AssemblyType::LongWord,
                            src: Operand::Register(Reg::R10),
                            dst,
                        });
                        continue;
                    }
                }
                new_body.push(instruction.clone());
            }
            Instruction::Mov {
                assembly_type: AssemblyType::QuadWord,
                src,
                dst,
            } => {
                if let Operand::Stack(_) | Operand::Data(_) = src {
                    if let Operand::Stack(_) | Operand::Data(_) = dst {
                        new_body.push(Instruction::Mov {
                            assembly_type: AssemblyType::QuadWord,
                            src,
                            dst: Operand::Register(Reg::R10),
                        });
                        new_body.push(Instruction::Mov {
                            assembly_type: AssemblyType::QuadWord,
                            src: Operand::Register(Reg::R10),
                            dst,
                        });
                        continue;
                    }
                }
                if let Operand::Immediate { imm: _ } = src {
                    if let Operand::Stack(_) = dst {
                        new_body.push(Instruction::Mov {
                            assembly_type: AssemblyType::QuadWord,
                            src,
                            dst: Operand::Register(Reg::R10),
                        });
                        new_body.push(Instruction::Mov {
                            assembly_type: AssemblyType::QuadWord,
                            src: Operand::Register(Reg::R10),
                            dst,
                        });
                        continue;
                    }
                }
                new_body.push(instruction.clone());
            }
            Instruction::Movsx {
                //TODO this is wrong
                src,
                dst,
            } => {
                if let Operand::Stack(_) | Operand::Data(_) = src {
                    if let Operand::Stack(_) | Operand::Data(_) = dst {
                        new_body.push(Instruction::Mov {
                            assembly_type: AssemblyType::QuadWord,
                            src,
                            dst: Operand::Register(Reg::R10),
                        });
                        new_body.push(Instruction::Mov {
                            assembly_type: AssemblyType::QuadWord,
                            src: Operand::Register(Reg::R10),
                            dst,
                        });
                        continue;
                    }
                }
                if let Operand::Immediate { imm: _ } = src {
                    if let Operand::Stack(_) = dst {
                        new_body.push(Instruction::Mov {
                            assembly_type: AssemblyType::QuadWord,
                            src,
                            dst: Operand::Register(Reg::R10),
                        });
                        new_body.push(Instruction::Mov {
                            assembly_type: AssemblyType::QuadWord,
                            src: Operand::Register(Reg::R10),
                            dst,
                        });
                        continue;
                    }
                }

                if let Operand::Immediate { imm: _ } = src {
                    if let Operand::Stack(s) = dst {
                        new_body.push(Instruction::Mov {
                            assembly_type: AssemblyType::LongWord, //Should use type from imm
                            src,
                            dst: Operand::Register(Reg::R10),
                        });

                        new_body.push(Instruction::Movsx {
                            src: Operand::Register(Reg::R10),
                            dst: Operand::Register(Reg::R11),
                        });
                        new_body.push(Instruction::Mov {
                            assembly_type: AssemblyType::QuadWord,
                            src: Operand::Register(Reg::R11),
                            dst: Operand::Stack(s),
                        });
                        continue;
                    }
                }
                new_body.push(instruction.clone());
            }
            Instruction::Cmp(assembly_type, lhs, rhs) => {
                if let Operand::Stack(_) | Operand::Data(_) = lhs {
                    if let Operand::Stack(_) | Operand::Data(_) = rhs {
                        new_body.push(Instruction::Mov {
                            assembly_type,
                            src: lhs,
                            dst: Operand::Register(Reg::R10),
                        });
                        new_body.push(Instruction::Cmp(
                            assembly_type,
                            Operand::Register(Reg::R10),
                            rhs,
                        ));
                        continue;
                    }
                }
                if let Operand::Immediate { imm: _ } = rhs {
                    new_body.push(Instruction::Mov {
                        assembly_type,
                        src: rhs,
                        dst: Operand::Register(Reg::R10),
                    });
                    new_body.push(Instruction::Cmp(
                        assembly_type,
                        lhs,
                        Operand::Register(Reg::R10),
                    ));
                    continue;
                }
                new_body.push(instruction.clone());
            }
            Instruction::Binary {
                op,
                assembly_type,
                src2,
                dst,
            } if op == BinaryOp::Mult => {
                if let Operand::Stack(_) = dst {
                    new_body.push(Instruction::Mov {
                        assembly_type,
                        src: dst.clone(),
                        dst: Operand::Register(Reg::R11),
                    });
                    new_body.push(Instruction::Binary {
                        op,
                        assembly_type,
                        src2,
                        dst: Operand::Register(Reg::R11),
                    });
                    new_body.push(Instruction::Mov {
                        assembly_type,
                        src: Operand::Register(Reg::R11),
                        dst,
                    });
                    continue;
                }
                new_body.push(instruction.clone());
            }

            Instruction::Binary {
                op,
                assembly_type,
                src2,
                dst,
            } => {
                if let Operand::Stack(_) | Operand::Data(_) = dst {
                    if let Operand::Stack(_) | Operand::Data(_) = src2 {
                        match op {
                            BinaryOp::Add | BinaryOp::Sub => {
                                new_body.push(Instruction::Mov {
                                    assembly_type,
                                    src: src2,
                                    dst: Operand::Register(Reg::R10),
                                });
                                new_body.push(Instruction::Binary {
                                    op,
                                    assembly_type,
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
            Instruction::Idiv { assembly_type, src } => {
                if let Operand::Immediate { imm: _ } = src {
                    new_body.push(Instruction::Mov {
                        assembly_type,
                        src,
                        dst: Operand::Register(Reg::R10),
                    });
                    new_body.push(Instruction::Idiv {
                        assembly_type,
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
