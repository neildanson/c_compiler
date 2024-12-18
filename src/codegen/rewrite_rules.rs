use super::*;
use std::collections::HashMap;

fn valid_stack_location(stack_pos: i32, ty: &AssemblyType) -> i32 {
    match ty {
        AssemblyType::LongWord => stack_pos,
        AssemblyType::QuadWord | AssemblyType::Double => {
            if stack_pos % 8 == 0 {
                stack_pos
            } else {
                stack_pos + 8 - (stack_pos % 8)
            }
        } //AssemblyType::Double => unimplemented!(),
    }
}

fn fixup_large_operand(operand: Operand, reg: Reg, instructions: &mut Vec<Instruction>) -> Operand {
    match operand {
        Operand::Immediate { imm } if imm > i32::MAX as i128 => {
            instructions.push(Instruction::Mov {
                assembly_type: AssemblyType::QuadWord,
                src: Operand::Immediate { imm },
                dst: Operand::Register(reg),
            });
            Operand::Register(reg)
        }
        _ => operand,
    }
}

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
        let stack_pos = stack.values().map(|v| -v).max().unwrap_or(0);

        match (symbol, parameter) {
            (Some(AsmSymTabEntry::ObjEntry(_, true)), _) => Operand::Data(name),
            (Some(AsmSymTabEntry::ObjEntry(ty, false)), _) => {
                let offset = stack_pos + (ty.size() as i32);
                let offset = valid_stack_location(offset, ty);
                stack.insert(name, -offset);
                Operand::local(offset)
            }
            (_, true) => {
                let offset = stack_pos + (AssemblyType::QuadWord.size() as i32);
                let offset = valid_stack_location(offset, &AssemblyType::QuadWord);
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
            Instruction::Div { assembly_type, src } => {
                new_body.push(Instruction::Div {
                    assembly_type,
                    src: match src {
                        Operand::Pseudo(name) => {
                            fixup_pseudo(name, &mut stack, false, static_variables)
                        }
                        _ => src,
                    },
                });
            }
            Instruction::MovZeroExtend { src, dst } => {
                new_body.push(Instruction::MovZeroExtend {
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
                });
            }
            Instruction::Cvttsd2si(assembly_type, src, dst) => {
                new_body.push(Instruction::Cvttsd2si(
                    assembly_type,
                    match src {
                        Operand::Pseudo(name) => {
                            fixup_pseudo(name, &mut stack, false, static_variables)
                        }
                        _ => src,
                    },
                    match dst {
                        Operand::Pseudo(name) => {
                            fixup_pseudo(name, &mut stack, false, static_variables)
                        }
                        _ => dst,
                    },
                ));
            }
            any_other => new_body.push(any_other),
        }
    }
    let stack_pos: i32 = stack.values().map(|v| -v).max().unwrap_or(0);
    (new_body, stack_pos.unsigned_abs() as usize)
}

pub(crate) fn fixup_stack_operations(body: &[Instruction]) -> Vec<Instruction> {
    let mut new_body = Vec::new();
    for instruction in body {
        match instruction.clone() {
            Instruction::Mov {
                assembly_type: AssemblyType::LongWord,
                src,
                dst,
            } => {
                let assembly_type = AssemblyType::LongWord;
                if let Operand::Stack(_) | Operand::Data(_) = src {
                    if let Operand::Stack(_) | Operand::Data(_) = dst {
                        new_body.push(Instruction::Mov {
                            assembly_type,
                            src,
                            dst: Operand::Register(Reg::R10),
                        });
                        new_body.push(Instruction::Mov {
                            assembly_type,
                            src: Operand::Register(Reg::R10),
                            dst,
                        });
                        continue;
                    }
                }
                if let Operand::Immediate { imm } = src
                    && imm > i32::MAX as i128
                {
                    let src = Operand::Immediate {
                        imm: imm as i32 as i128, //Weird?
                    };
                    new_body.push(Instruction::Mov {
                        assembly_type,
                        src,
                        dst,
                    });
                    continue;
                }
                new_body.push(instruction.clone());
            }
            Instruction::Mov {
                assembly_type: AssemblyType::Double,
                src,
                dst,
            } => {
                let assembly_type = AssemblyType::Double;
                if let Operand::Stack(_) | Operand::Data(_) = src {
                    if let Operand::Stack(_) | Operand::Data(_) = dst {
                        new_body.push(Instruction::Mov {
                            assembly_type,
                            src,
                            dst: Operand::Register(Reg::XMM0),
                        });
                        new_body.push(Instruction::Mov {
                            assembly_type,
                            src: Operand::Register(Reg::XMM0),
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
                let assembly_type = AssemblyType::QuadWord;
                if let Operand::Stack(_) | Operand::Data(_) = src {
                    if let Operand::Stack(_) | Operand::Data(_) = dst {
                        new_body.push(Instruction::Mov {
                            assembly_type,
                            src,
                            dst: Operand::Register(Reg::R10),
                        });
                        new_body.push(Instruction::Mov {
                            assembly_type,
                            src: Operand::Register(Reg::R10),
                            dst,
                        });
                        continue;
                    }
                }
                if let Operand::Immediate { imm: _ } = src {
                    new_body.push(Instruction::Mov {
                        assembly_type,
                        src,
                        dst: Operand::Register(Reg::R10),
                    });
                    new_body.push(Instruction::Mov {
                        assembly_type,
                        src: Operand::Register(Reg::R10),
                        dst,
                    });
                    continue;
                }
                new_body.push(instruction.clone());
            }
            Instruction::Movsx { src, dst } => {
                if let Operand::Immediate { imm: _ } = src {
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
                        dst,
                    });
                    continue;
                }

                if let Operand::Stack(_) | Operand::Data(_) = src {
                    if let Operand::Stack(_) | Operand::Data(_) = dst {
                        new_body.push(Instruction::Mov {
                            assembly_type: AssemblyType::LongWord,
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

                if let Operand::Immediate { imm: _ } = src {
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

                new_body.push(instruction.clone());
            }
            Instruction::Cmp(AssemblyType::QuadWord, lhs, rhs) => {
                let assembly_type = AssemblyType::QuadWord;
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
                let lhs = fixup_large_operand(lhs, Reg::R11, &mut new_body);
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

                new_body.push(Instruction::Cmp(assembly_type, lhs, rhs));
            }
            Instruction::Cmp(assembly_type, lhs, rhs) if assembly_type == AssemblyType::LongWord || assembly_type == AssemblyType::QuadWord => {
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
            Instruction::Cmp(AssemblyType::Double, lhs, rhs) => {
                let assembly_type = AssemblyType::Double;
                if let Operand::Stack(_) | Operand::Data(_) = lhs {
                    if let Operand::Stack(_) | Operand::Data(_) = rhs {
                        new_body.push(Instruction::Mov {
                            assembly_type,
                            src: lhs,
                            dst: Operand::Register(Reg::XMM0),
                        });
                        new_body.push(Instruction::Cmp(
                            assembly_type,
                            Operand::Register(Reg::XMM0),
                            rhs,
                        ));
                        continue;
                    }
                }
                if let Operand::Immediate { imm: _ } = rhs {
                    new_body.push(Instruction::Mov {
                        assembly_type,
                        src: rhs,
                        dst: Operand::Register(Reg::XMM0),
                    });
                    new_body.push(Instruction::Cmp(
                        assembly_type,
                        lhs,
                        Operand::Register(Reg::XMM0),
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
                let src2 = fixup_large_operand(src2, Reg::R10, &mut new_body);

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
            } => match op {
                BinaryOp::Add | BinaryOp::Sub => {
                    let src2 = fixup_large_operand(src2, Reg::R11, &mut new_body);
                    if let Operand::Stack(_) | Operand::Data(_) = dst {
                        if let Operand::Stack(_) | Operand::Data(_) = src2 {
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
                    }
                    new_body.push(Instruction::Binary {
                        op,
                        assembly_type,
                        src2,
                        dst,
                    });
                }
                _ => continue,
            },
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
            Instruction::Div { assembly_type, src } => {
                if let Operand::Immediate { imm: _ } = src {
                    new_body.push(Instruction::Mov {
                        assembly_type,
                        src,
                        dst: Operand::Register(Reg::R10),
                    });
                    new_body.push(Instruction::Div {
                        assembly_type,
                        src: Operand::Register(Reg::R10),
                    });
                    continue;
                }
                new_body.push(instruction.clone());
            }
            Instruction::Push(operand) => {
                let operand = fixup_large_operand(operand, Reg::R10, &mut new_body);
                new_body.push(Instruction::Push(operand));
            }
            Instruction::MovZeroExtend { src, dst } => {
                if let Operand::Register(_) = dst {
                    new_body.push(Instruction::Mov {
                        assembly_type: AssemblyType::LongWord,
                        src,
                        dst,
                    });

                    continue;
                } else {
                    new_body.push(Instruction::Mov {
                        assembly_type: AssemblyType::LongWord,
                        src,
                        dst: Operand::Register(Reg::R11),
                    });
                    new_body.push(Instruction::Mov {
                        assembly_type: AssemblyType::QuadWord,
                        src: Operand::Register(Reg::R11),
                        dst,
                    });
                    continue;
                }
            }
            Instruction::Cvttsd2si(assembly_type, src, dst) => {
                //Dst cannot be a stack location
                if let Operand::Stack(_) | Operand::Data(_) = src {
                    if let Operand::Stack(_) | Operand::Data(_) = dst {
                        new_body.push(Instruction::Mov {
                            assembly_type: AssemblyType::Double,
                            src,
                            dst: Operand::Register(Reg::XMM0),
                        });
                        new_body.push(Instruction::Cvttsd2si(
                            assembly_type,
                            Operand::Register(Reg::XMM0),
                            Operand::Register(Reg::R10),
                        ));
                        new_body.push(Instruction::Mov {
                            assembly_type,
                            src: Operand::Register(Reg::R10),
                            dst,
                        });
                        continue;
                    }
                    continue;
                }
                new_body.push(instruction.clone());
            }
            _ => new_body.push(instruction.clone()),
        }
    }
    new_body
}
