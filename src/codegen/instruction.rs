use crate::{error::CompilerError, tacky};
use std::fmt::{Display, Formatter};

use super::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Instruction {
    Mov {
        src: Operand,
        dst: Operand,
    },
    Unary {
        op: UnaryOp,
        dst: Operand,
    },
    Binary {
        op: BinaryOp,
        src2: Operand,
        dst: Operand,
    },
    Idiv {
        src: Operand,
    },
    Cdq,
    AllocateStack(usize),
    DeallocateStack(usize),
    Ret,
    Cmp(Operand, Operand),
    Jmp(String),
    JmpCC(ConditionCode, String),
    SetCC(ConditionCode, Operand),
    Label(String),
    Push(Operand),
    Pop(Reg), //Defined for pop rdi, but will be introduced later in one of last chapters !
    Call(String),
}

fn format_label(label: &str) -> String {
    if cfg!(target_os = "macos") {
        format!("L{}", label)
    } else {
        format!(".L{}", label)
    }
}

pub fn format_fn_call(name: &str) -> String {
    if cfg!(target_os = "macos") {
        format!("_{}", name)
    } else {
        name.to_string()
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Instruction::Mov { src, dst } => {
                write!(f, "\tmovl {}, {}", src, dst)
            }
            Instruction::Ret => {
                writeln!(f, "\tmovq %rbp, %rsp")?;
                writeln!(f, "\tpopq %rbp")?;
                write!(f, "\tret")
            }

            Instruction::Unary { op, dst } => {
                write!(f, "\t{} {}", op, dst)
            }

            Instruction::AllocateStack(size) => {
                writeln!(f, "\t# Allocating stack of size {}", size)?;
                //round size to newarest multiple of 16
                writeln!(f, "\tsubq ${}, %rsp", size)
            }
            Instruction::DeallocateStack(size) => {
                write!(f, "\taddq ${}, %rsp", size)
            }
            Instruction::Idiv { src } => {
                write!(f, "\tidivl {}", src)
            }
            Instruction::Cdq => {
                write!(f, "\tcdq")
            }
            Instruction::Binary { op, src2, dst } => {
                write!(f, "\t{} {}, {}", op, src2, dst)
            }
            Instruction::Cmp(src1, src2) => {
                write!(f, "\tcmpl {}, {}", src1, src2)
            }
            Instruction::Jmp(target) => {
                write!(f, "\tjmp {}", format_label(target))
            }
            Instruction::JmpCC(cc, target) => {
                write!(f, "\tj{} {}", cc, format_label(target))
            }
            Instruction::SetCC(cc, dst) => {
                let dst = match dst {
                    Operand::Register(register) => format!("{:.1}", register),
                    d => format!("{}", d),
                };

                write!(f, "\tset{} {}", cc, dst)
            }
            Instruction::Label(name) => {
                write!(f, "{}:", format_label(name))
            }
            Instruction::Push(operand) => {
                let operand = match operand {
                    Operand::Register(register) => format!("{:.8}", register),
                    d => format!("{}", d),
                };
                write!(f, "\tpushq {}", operand)
            }
            Instruction::Call(name) => {
                if cfg!(target_os = "macos") {
                    write!(f, "\tcall _{}", name)
                } else {
                    write!(f, "\tcall {}@PLT", format_fn_call(name)) //In principal we dont need the @PLT for defined functions by us
                }
            }
            Instruction::Pop(register) => {
                write!(f, "\tpopq {:.8}", register)
            } //instruction => unimplemented!("Instruction {}", instruction), //Add the rest of the instructions
        }
    }
}

fn convert_function_call(
    name: String,
    args: Vec<tacky::Value>,
    dst: tacky::Value,
) -> Result<Vec<Instruction>, CompilerError> {
    let register_args: Vec<_> = args.iter().take(6).collect();
    let stack_args: Vec<_> = args.iter().skip(6).collect();

    let stack_padding = //if length stack args is odd, then pad
        if stack_args.len() % 2 == 1 {
            8
        } else {
            0
        };
    let mut instructions = vec![];

    if stack_padding > 0 {
        instructions.push(Instruction::Push(Operand::Register(Reg::DI)));
        instructions.push(Instruction::AllocateStack(stack_padding));
    }

    for (i, arg) in register_args.iter().enumerate() {
        let assembly_arg = (*arg).clone().into();
        instructions.push(Instruction::Mov {
            src: assembly_arg,
            dst: Operand::arg(i),
        });
    }

    for arg in stack_args.iter().rev() {
        let assembly_arg = (*arg).clone().into();
        match assembly_arg {
            Operand::Register(_) | Operand::Immediate { imm: _ } => {
                instructions.push(Instruction::Push(assembly_arg));
            }
            _ => {
                instructions.push(Instruction::Mov {
                    src: assembly_arg,
                    dst: Operand::Register(Reg::AX),
                });
                instructions.push(Instruction::Push(Operand::Register(Reg::AX)));
            }
        }
    }

    instructions.push(Instruction::Call(name));
    let bytes_to_remove = 8 * stack_args.len() + stack_padding;
    if bytes_to_remove > 0 {
        instructions.push(Instruction::DeallocateStack(bytes_to_remove));
        instructions.push(Instruction::Pop(Reg::DI));
    }
    instructions.push(Instruction::Mov {
        src: Operand::Register(Reg::AX),
        dst: dst.into(),
    });

    Ok(instructions)
}

impl TryFrom<tacky::Instruction> for Vec<Instruction> {
    type Error = CompilerError;
    fn try_from(instruction: tacky::Instruction) -> Result<Self, Self::Error> {
        match instruction {
            tacky::Instruction::Return(value) => {
                let src = value.into();
                let dst = Operand::Register(Reg::AX);
                Ok(vec![Instruction::Mov { src, dst }, Instruction::Ret])
            }
            tacky::Instruction::Unary {
                op: tacky::UnaryOp::Not,
                src,
                dst,
            } => {
                let src = src.into();
                let dst: Operand = dst.into();
                Ok(vec![
                    Instruction::Cmp(Operand::Immediate { imm: 0 }, src),
                    Instruction::Mov {
                        src: Operand::Immediate { imm: 0 },
                        dst: dst.clone(),
                    },
                    Instruction::SetCC(ConditionCode::E, dst),
                ])
            }
            tacky::Instruction::Unary { op, src, dst } => {
                let src = src.into();
                let dst: Operand = dst.into();
                Ok(vec![
                    Instruction::Mov {
                        src,
                        dst: dst.clone(),
                    },
                    Instruction::Unary { op: op.into(), dst },
                ])
            }
            tacky::Instruction::Binary {
                op: tacky::BinaryOp::Divide,
                src1,
                src2,
                dst,
            } => {
                let src1 = src1.into();
                let src2 = src2.into();
                let dst = dst.into();
                Ok(vec![
                    Instruction::Mov {
                        src: src1,
                        dst: Operand::Register(Reg::AX),
                    },
                    Instruction::Cdq,
                    Instruction::Idiv { src: src2 },
                    Instruction::Mov {
                        src: Operand::Register(Reg::AX),
                        dst,
                    },
                ])
            }
            tacky::Instruction::Binary {
                op: tacky::BinaryOp::Remainder,
                src1,
                src2,
                dst,
            } => {
                let src1 = src1.into();
                let src2 = src2.into();
                let dst = dst.into();
                Ok(vec![
                    Instruction::Mov {
                        src: src1,
                        dst: Operand::Register(Reg::AX),
                    },
                    Instruction::Cdq,
                    Instruction::Idiv { src: src2 },
                    Instruction::Mov {
                        src: Operand::Register(Reg::DX),
                        dst,
                    },
                ])
            }
            tacky::Instruction::Binary {
                op,
                src1,
                src2,
                dst,
            } if let Ok(cc) = op.clone().try_into() => {
                let src1 = src1.into();
                let src2 = src2.into();
                let dst: Operand = dst.into();
                Ok(vec![
                    Instruction::Cmp(src2, src1),
                    Instruction::Mov {
                        src: Operand::Immediate { imm: 0 },
                        dst: dst.clone(),
                    },
                    Instruction::SetCC(cc, dst),
                ])
            }

            tacky::Instruction::Binary {
                op,
                src1,
                src2,
                dst,
            } => {
                let src1 = src1.into();
                let src2 = src2.into();
                let dst: Operand = dst.into();
                let op = op.try_into()?;
                Ok(vec![
                    Instruction::Mov {
                        src: src1,
                        dst: dst.clone(),
                    },
                    Instruction::Binary { op, src2, dst },
                ])
            }
            tacky::Instruction::JumpIfZero { condition, target } => Ok(vec![
                Instruction::Cmp(Operand::Immediate { imm: 0 }, condition.into()),
                Instruction::JmpCC(ConditionCode::E, target),
            ]),
            tacky::Instruction::JumpIfNotZero { condition, target } => Ok(vec![
                Instruction::Cmp(Operand::Immediate { imm: 0 }, condition.into()),
                Instruction::JmpCC(ConditionCode::NE, target),
            ]),
            tacky::Instruction::Copy { src, dst } => Ok(vec![Instruction::Mov {
                src: src.into(),
                dst: dst.into(),
            }]),
            tacky::Instruction::Label { name } => Ok(vec![Instruction::Label(name)]),
            tacky::Instruction::Jump { target } => Ok(vec![Instruction::Jmp(target)]),
            tacky::Instruction::FunCall { name, args, dst } => {
                convert_function_call(name, args, dst)
            }
        }
    }
}
