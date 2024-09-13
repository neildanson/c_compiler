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
    Cmp(Operand, Operand),
    Jmp(String),
    JmpCC(ConditionCode, String),
    SetCC(ConditionCode, Operand),
    Label(String),
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
            }
            Instruction::Cmp(src1, src2) => {
                writeln!(f, "#Cmp")?;
                writeln!(f, "\tcmpl {}, {}", src1, src2)
            }
            Instruction::Jmp(target) => {
                writeln!(f, "#Jmp")?;
                writeln!(f, "\tjmp .L{}", target)
            }
            Instruction::JmpCC(cc, target) => {
                writeln!(f, "#JmpCC")?;
                writeln!(f, "\tj{} .L{}", cc, target)
            }
            Instruction::SetCC(cc, dst) => {
                writeln!(f, "#SetCC")?;
                writeln!(f, "\tset{} {}", cc, dst)
            }
            Instruction::Label(name) => {
                writeln!(f, "#Label")?;
                writeln!(f, "\t.L{}:", name)
            }
            //instruction => unimplemented!("Instruction {}", instruction), //Add the rest of the instructions
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
            } if let Ok(cc) = op.clone().try_into() => {
                //TODO Extract to function passing in condition code
                let src1 = src1.into();
                let src2 = src2.into();
                let dst : Operand = dst.into();
                vec![
                    
                    Instruction::Cmp(src2, src1),
                    Instruction::Mov {
                        src: Operand::Immediate { imm: 0 },
                        dst: dst.clone(),
                    },
                    Instruction::SetCC(cc, dst),
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
            tacky::Instruction::JumpIfZero { condition, target } => 
                vec![
                    Instruction::Cmp(Operand::Immediate { imm: 0 } , condition.into()),
                    Instruction::JmpCC(ConditionCode::E, target),
                ],
            tacky::Instruction::JumpIfNotZero { condition, target } => 
                vec![
                    Instruction::Cmp(Operand::Immediate { imm: 0 } , condition.into()),
                    Instruction::JmpCC(ConditionCode::NE, target),
                ],
            tacky::Instruction::Copy { src, dst } => 
                vec![
                    Instruction::Mov { src: src.into(), dst: dst.into() },
                ],
            tacky::Instruction::Label { name } => 
                vec![
                    Instruction::Label(name),
                ],
            tacky::Instruction::Jump { target } => 
                vec![
                    Instruction::Jmp(target),
                ],
            //x => unimplemented!("{:?}", x),

        }
    }
}
