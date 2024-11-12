use crate::{
    error::CompilerError,
    parse::Constant,
    tacky::{self, Value},
};
use std::fmt::{Display, Formatter};

use super::*;

#[derive(Debug, PartialEq, Clone)]
pub enum AssemblyType {
    LongWord, //32 bit
    QuadWord, //64 bit
}

impl Display for AssemblyType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            AssemblyType::LongWord => write!(f, "l"),
            AssemblyType::QuadWord => write!(f, "q"),
        }
    }
}

impl Value {
    pub fn assembly_type(&self) -> AssemblyType {
        match self {
            Value::Constant(Constant::Int(_)) => AssemblyType::LongWord,
            Value::Constant(Constant::Long(_)) => AssemblyType::QuadWord,
            Value::Var(variable) => {
                println!("Variable: {:?}", variable);
                AssemblyType::LongWord
            } //panic!("Unsupported assembly type for value {:?}", self),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Instruction {
    Mov {
        assembly_type: AssemblyType,
        src: Operand,
        dst: Operand,
    },
    Movsx {
        src: Operand,
        dst: Operand,
    },
    Unary {
        op: UnaryOp,
        assembly_type: AssemblyType,
        dst: Operand,
    },
    Binary {
        op: BinaryOp,
        assembly_type: AssemblyType,
        src2: Operand,
        dst: Operand,
    },
    Idiv {
        assembly_type: AssemblyType,
        src: Operand,
    },
    Cdq(AssemblyType),
    Ret,
    Cmp(AssemblyType, Operand, Operand),
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
            Instruction::Mov {
                assembly_type,
                src,
                dst,
            } => {
                write!(f, "\tmov{} {}, {}", assembly_type, src, dst)
            }
            Instruction::Ret => {
                writeln!(f, "\tmovq %rbp, %rsp")?;
                writeln!(f, "\tpopq %rbp")?;
                write!(f, "\tret")
            }

            Instruction::Unary {
                assembly_type,
                op,
                dst,
            } => {
                write!(f, "\t{}{} {}", op, assembly_type, dst)
            }
            Instruction::Idiv { assembly_type, src } => {
                write!(f, "\tidiv{} {}", assembly_type, src)
            }
            Instruction::Cdq(AssemblyType::LongWord) => {
                write!(f, "\tcdq")
            }
            Instruction::Cdq(AssemblyType::QuadWord) => {
                write!(f, "\tcqo")
            }
            Instruction::Binary {
                op,
                assembly_type,
                src2,
                dst,
            } => {
                write!(f, "\t{}{} {}, {}", op, assembly_type, src2, dst)
            }
            Instruction::Cmp(assembly_type, src1, src2) => {
                write!(f, "\tcmp{} {}, {}", assembly_type, src1, src2)
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
            }
            Instruction::Movsx { src, dst } => {
                write!(f, "\tmovsxl {}, {}", src, dst)
            }
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

    let stack_padding : usize = //if length stack args is odd, then pad
        if stack_args.len() % 2 == 1 {
            8
        } else {
            0
        };
    let mut instructions = vec![];

    if stack_padding > 0 {
        instructions.push(Instruction::Push(Operand::Register(Reg::DI)));
        instructions.push(Instruction::Binary {
            op: BinaryOp::Sub,
            assembly_type: AssemblyType::QuadWord,
            src2: Operand::Immediate {
                imm: stack_padding as i64,
            },
            dst: Operand::Register(Reg::SP),
        });
    }

    for (i, arg) in register_args.iter().enumerate() {
        let assembly_arg = (*arg).clone().into();
        instructions.push(Instruction::Mov {
            assembly_type: AssemblyType::LongWord, //All arguments are 64 bit?
            src: assembly_arg,
            dst: Operand::arg(i),
        });
    }

    for arg in stack_args.iter().rev() {
        let assembly_arg = (*arg).clone().into();
        if arg.assembly_type() == AssemblyType::QuadWord {
            //TODO this doesnt really work well as we dont have the assembly type here whena variable
            instructions.push(Instruction::Push(assembly_arg));
        } else {
            match assembly_arg {
                Operand::Register(_) | Operand::Immediate { imm: _ } => {
                    instructions.push(Instruction::Push(assembly_arg));
                }
                _ => {
                    instructions.push(Instruction::Mov {
                        assembly_type: AssemblyType::LongWord,
                        src: assembly_arg,
                        dst: Operand::Register(Reg::AX),
                    });
                    instructions.push(Instruction::Push(Operand::Register(Reg::AX)));
                }
            }
        }
    }

    instructions.push(Instruction::Call(name));
    let bytes_to_remove = 8 * stack_args.len() + stack_padding;
    if bytes_to_remove > 0 {
        instructions.push(Instruction::Binary {
            op: BinaryOp::Add,
            assembly_type: AssemblyType::QuadWord,
            src2: Operand::Immediate {
                imm: bytes_to_remove as i64,
            },
            dst: Operand::Register(Reg::SP),
        });
        instructions.push(Instruction::Pop(Reg::DI));
    }
    let assembly_type = AssemblyType::LongWord; //TODO: Check if this is correct
    instructions.push(Instruction::Mov {
        assembly_type,
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
                let assembly_type = AssemblyType::LongWord; //TODO: Check if this is correct
                Ok(vec![
                    Instruction::Mov {
                        assembly_type,
                        src,
                        dst,
                    },
                    Instruction::Ret,
                ])
            }
            tacky::Instruction::Unary {
                op: tacky::UnaryOp::Not,
                src,
                dst,
            } => {
                let src = src.into();
                let dst: Operand = dst.into();
                Ok(vec![
                    Instruction::Cmp(AssemblyType::LongWord, Operand::Immediate { imm: 0 }, src),
                    Instruction::Mov {
                        assembly_type: AssemblyType::LongWord, //TODO: Check if this is correct
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
                        assembly_type: AssemblyType::LongWord, //TODO: Check if this is correct
                        src,
                        dst: dst.clone(),
                    },
                    Instruction::Unary {
                        assembly_type: AssemblyType::LongWord,
                        op: op.into(),
                        dst,
                    },
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
                let assembly_type = AssemblyType::LongWord; //TODO: Check if this is correct
                Ok(vec![
                    Instruction::Mov {
                        assembly_type: assembly_type.clone(),
                        src: src1,
                        dst: Operand::Register(Reg::AX),
                    },
                    Instruction::Cdq(assembly_type.clone()),
                    Instruction::Idiv {
                        assembly_type: assembly_type.clone(),
                        src: src2,
                    },
                    Instruction::Mov {
                        assembly_type,
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
                let assembly_type = AssemblyType::LongWord; //TODO: Check if this is correct
                Ok(vec![
                    Instruction::Mov {
                        assembly_type: assembly_type.clone(),
                        src: src1,
                        dst: Operand::Register(Reg::AX),
                    },
                    Instruction::Cdq(assembly_type.clone()),
                    Instruction::Idiv {
                        assembly_type: assembly_type.clone(),
                        src: src2,
                    },
                    Instruction::Mov {
                        assembly_type,
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
                let assembly_type = AssemblyType::LongWord; //TODO: Check if this is correct
                Ok(vec![
                    Instruction::Cmp(assembly_type.clone(), src2, src1),
                    Instruction::Mov {
                        assembly_type,
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
                let assembly_type = AssemblyType::LongWord; //TODO: Check if this is correct
                Ok(vec![
                    Instruction::Mov {
                        assembly_type: assembly_type.clone(),
                        src: src1,
                        dst: dst.clone(),
                    },
                    Instruction::Binary {
                        op,
                        assembly_type,
                        src2,
                        dst,
                    },
                ])
            }
            tacky::Instruction::JumpIfZero { condition, target } => Ok(vec![
                Instruction::Cmp(
                    AssemblyType::LongWord,
                    Operand::Immediate { imm: 0 },
                    condition.into(),
                ),
                Instruction::JmpCC(ConditionCode::E, target),
            ]),
            tacky::Instruction::JumpIfNotZero { condition, target } => Ok(vec![
                Instruction::Cmp(
                    AssemblyType::LongWord,
                    Operand::Immediate { imm: 0 },
                    condition.into(),
                ),
                Instruction::JmpCC(ConditionCode::NE, target),
            ]),
            tacky::Instruction::Copy { src, dst } => Ok(vec![Instruction::Mov {
                assembly_type: AssemblyType::LongWord, //TODO: Check if this is correct
                src: src.into(),
                dst: dst.into(),
            }]),
            tacky::Instruction::Label { name } => Ok(vec![Instruction::Label(name)]),
            tacky::Instruction::Jump { target } => Ok(vec![Instruction::Jmp(target)]),
            tacky::Instruction::FunCall { name, args, dst } => {
                convert_function_call(name, args, dst)
            }
            tacky::Instruction::SignExtend { src, dst } => {
                let src = src.into();
                let dst = dst.into();
                Ok(vec![Instruction::Movsx { src, dst }]) //TODO
            }
            tacky::Instruction::Truncate { src, dst } => {
                let src = src.into();
                let dst = dst.into();
                Ok(vec![Instruction::Mov {
                    assembly_type: AssemblyType::LongWord,
                    src,
                    dst,
                }]) //TODO
            }
        }
    }
}
