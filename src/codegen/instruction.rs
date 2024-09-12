use crate::tacky;
use std::fmt::{Display, Formatter, Result};

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
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Instruction::Mov { src, dst } => {
                writeln!(f, "#Mov")?;
                writeln!(f, "\tmovl {}, {}", src, dst)
            }
            Instruction::Ret => {
                writeln!(f, "#Return")?;
                writeln!(f, "\tmovq %rbp, %rsp")?;
                writeln!(f, "\tpopq %rbp")?;
                writeln!(f, "\tret")
            }

            Instruction::Unary { op, dst } => {
                writeln!(f, "#Unary")?;
                writeln!(f, "\t{} {}", op, dst)
            }

            Instruction::AllocateStack(size) => {
                writeln!(f, "#Allocate Stack")?;
                writeln!(f, "\tsubq ${}, %rsp", size * 4)
            }
            Instruction::Idiv { src } => {
                writeln!(f, "#Idiv")?;
                writeln!(f, "\tidivl {}", src)
            }
            Instruction::Cdq => {
                writeln!(f, "#Cdq")?;
                writeln!(f, "\tcdq")
            }
            Instruction::Binary { op, src2, dst } => {
                writeln!(f, "#Binary")?;
                writeln!(f, "\t{} {}, {}", op, src2, dst)
            } //_ => unimplemented!("Instruction Display not implemented"),
        }
    }
}

impl From<tacky::Instruction> for Vec<Instruction> {
    fn from(ast: tacky::Instruction) -> Self {
        match ast {
            tacky::Instruction::Return(value) => {
                let src = value.into();
                let dst = Operand::Register(Reg::AX);
                vec![Instruction::Mov { src, dst }, Instruction::Ret]
            }
            tacky::Instruction::Unary { op, src, dst } => {
                let src = src.into();
                let dst: Operand = dst.into();
                vec![
                    Instruction::Mov {
                        src,
                        dst: dst.clone(),
                    },
                    Instruction::Unary { op: op.into(), dst },
                ]
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
                vec![
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
                ]
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
                vec![
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
                ]
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
                let op = op.into();
                vec![
                    Instruction::Mov {
                        src: src1,
                        dst: dst.clone(),
                    },
                    Instruction::Binary { op, src2, dst },
                ]
            }
            _ => unimplemented!(),
        }
    }
}
