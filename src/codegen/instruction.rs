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
    Ret,
    Cmp(Operand, Operand),
    Jmp(String),
    JmpCC(ConditionCode, String),
    SetCC(ConditionCode, Operand),
    Label(String),
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
                writeln!(f, "\tsubq ${}, %rsp", size * 4)
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
                write!(f, "\tjmp .L{}", target)
            }
            Instruction::JmpCC(cc, target) => {
                write!(f, "\tj{} .L{}", cc, target)
            }
            Instruction::SetCC(cc, dst) => {
                let dst = match dst {
                    Operand::Register(Reg::AX) => "al",
                    Operand::Register(Reg::DX) => "dl",
                    Operand::Register(Reg::R10) => "r10b",
                    Operand::Register(Reg::R11) => "r11b",
                    d => &format!("{}", d),
                };

                write!(f, "\tset{} {}", cc, dst)
            }
            Instruction::Label(name) => {
                write!(f, ".L{}:", name)
            } //instruction => unimplemented!("Instruction {}", instruction), //Add the rest of the instructions
        }
    }
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
            tacky::Instruction::Unary { op, src, dst } if op == tacky::UnaryOp::Not => {
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
                op,
                src1,
                src2,
                dst,
            } if op == tacky::BinaryOp::Divide => {
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
                op,
                src1,
                src2,
                dst,
            } if op == tacky::BinaryOp::Remainder => {
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
            //x => unimplemented!("{:?}", x),
        }
    }
}
