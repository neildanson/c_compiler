use crate::{
    ast::{Constant, Type},
    error::CompilerError,
    tacky::{self, Value},
};
use std::fmt::{Display, Formatter};

use super::*;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum AssemblyType {
    LongWord, //32 bit
    QuadWord, //64 bit
    Double,   //64 bit
}

impl AssemblyType {
    pub fn size(&self) -> usize {
        match self {
            AssemblyType::LongWord => 4,
            AssemblyType::QuadWord => 8,
            AssemblyType::Double => 8,
        }
    }
}

impl From<&Type> for AssemblyType {
    fn from(ty: &Type) -> Self {
        match ty {
            Type::Int => AssemblyType::LongWord,
            Type::Long => AssemblyType::QuadWord,
            Type::UInt => AssemblyType::LongWord,
            Type::ULong => AssemblyType::QuadWord,
            Type::Double => AssemblyType::Double,
            _ => panic!("Unsupported assembly type for type {:?}", ty),
        }
    }
}

impl Display for AssemblyType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            AssemblyType::LongWord => write!(f, "l"),
            AssemblyType::QuadWord => write!(f, "q"),
            AssemblyType::Double => write!(f, "sd"), //Check
        }
    }
}

impl Value {
    pub fn assembly_type(&self) -> AssemblyType {
        match self {
            Value::Constant(Constant::Int(_)) => AssemblyType::LongWord,
            Value::Constant(Constant::Long(_)) => AssemblyType::QuadWord,
            Value::Constant(Constant::UnsignedInt(_)) => AssemblyType::LongWord,
            Value::Constant(Constant::UnsignedLong(_)) => AssemblyType::QuadWord,
            Value::Var(_, ty) => ty.into(),
            Value::Constant(Constant::Double(_)) => AssemblyType::Double,
        }
    }
    pub fn parse_type(&self) -> Type {
        match self {
            Value::Constant(Constant::Int(_)) => Type::Int,
            Value::Constant(Constant::Long(_)) => Type::Long,
            Value::Constant(Constant::UnsignedInt(_)) => Type::UInt,
            Value::Constant(Constant::UnsignedLong(_)) => Type::ULong,
            Value::Constant(Constant::Double(_)) => Type::Double,
            Value::Var(_, ty) => ty.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Instruction {
    Comment(String),
    Mov {
        assembly_type: AssemblyType,
        src: Operand,
        dst: Operand,
    },
    Movsx {
        src: Operand,
        dst: Operand,
    },
    MovZeroExtend {
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
    Div {
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
    Cvttsd2si(AssemblyType, Operand, Operand), //Double to Signed Int
    Cvtsi2sd(AssemblyType, Operand, Operand),  //Signed Int to Double
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
            Instruction::Comment(comment) => {
                write!(f, "/* {} */", comment)
            }
            Instruction::Mov {
                assembly_type,
                src,
                dst,
            } => {
                write!(
                    f,
                    "\tmov{} {}, {}",
                    assembly_type,
                    src.asm(*assembly_type),
                    dst.asm(*assembly_type)
                )
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
                write!(f, "\t{}{} {}", op, assembly_type, dst.asm(*assembly_type))
            }
            Instruction::Idiv { assembly_type, src } => {
                write!(f, "\tidiv{} {}", assembly_type, src.asm(*assembly_type))
            }
            Instruction::Div { assembly_type, src } => {
                write!(f, "\tdiv{} {}", assembly_type, src.asm(*assembly_type))
            }
            Instruction::Cdq(AssemblyType::LongWord) => {
                write!(f, "\tcdq")
            }
            Instruction::Cdq(AssemblyType::QuadWord) => {
                write!(f, "\tcqo")
            }
            Instruction::Cdq(AssemblyType::Double) => {
                panic!("Cdq not implemented for double")
            }
            Instruction::Binary {
                op,
                assembly_type,
                src2,
                dst,
            } => {
                write!(
                    f,
                    "\t{}{} {}, {}",
                    op,
                    assembly_type,
                    src2.asm(*assembly_type),
                    dst.asm(*assembly_type)
                )
            }
            Instruction::Cmp(assembly_type, src1, src2) => {
                write!(
                    f,
                    "\tcmp{} {}, {}",
                    assembly_type,
                    src1.asm(*assembly_type),
                    src2.asm(*assembly_type)
                )
            }
            Instruction::Jmp(target) => {
                write!(f, "\tjmp {}", format_label(target))
            }
            Instruction::JmpCC(cc, target) => {
                write!(f, "\tj{} {}", cc, format_label(target))
            }
            Instruction::SetCC(cc, dst) => {
                let dst = match dst {
                    Operand::Register(register) => register.asm(None),
                    d => d.asm(AssemblyType::LongWord),
                };

                write!(f, "\tset{} {}", cc, dst)
            }
            Instruction::Label(name) => {
                write!(f, "{}:", format_label(name))
            }
            Instruction::Push(operand) => {
                let operand = match operand {
                    Operand::Register(register) => register.asm(Some(AssemblyType::QuadWord)),
                    d => d.asm(AssemblyType::QuadWord),
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
                write!(f, "\tpopq {}", register.asm(Some(AssemblyType::QuadWord)))
            }
            Instruction::Movsx { src, dst } => {
                write!(
                    f,
                    "\tmovslq {}, {}",
                    src.asm(AssemblyType::LongWord),
                    dst.asm(AssemblyType::QuadWord)
                )
            }
            Instruction::MovZeroExtend {
                src: _src,
                dst: _dst,
            } => {
                panic!("MovZeroExtend not implemented")
            }
            Instruction::Cvttsd2si(assembly_type, src, dst) => {
                write!(
                    f,
                    "\tcvttsd2si{} {}, {}",
                    assembly_type,
                    src.asm(AssemblyType::Double),
                    dst.asm(*assembly_type)
                )
            }
            Instruction::Cvtsi2sd(assembly_type, src, dst) => {
                write!(
                    f,
                    "\tcvtsi2sd{} {}, {}",
                    assembly_type,
                    src.asm(*assembly_type),
                    dst.asm(*assembly_type)
                )
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
                imm: stack_padding as i128,
            },
            dst: Operand::Register(Reg::SP),
        });
    }

    for (i, arg) in register_args.iter().enumerate() {
        let assembly_arg = (*arg).clone().try_into()?;
        instructions.push(Instruction::Mov {
            assembly_type: arg.assembly_type(),
            src: assembly_arg,
            dst: Operand::arg(i),
        });
    }

    for arg in stack_args.iter().rev() {
        let assembly_arg = (*arg).clone().try_into()?;
        if arg.assembly_type() == AssemblyType::QuadWord {
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
                imm: bytes_to_remove as i128,
            },
            dst: Operand::Register(Reg::SP),
        });
        instructions.push(Instruction::Pop(Reg::DI));
    }
    let assembly_type = dst.assembly_type();
    instructions.push(Instruction::Mov {
        assembly_type,
        src: Operand::Register(Reg::AX),
        dst: dst.try_into()?,
    });

    Ok(instructions)
}

fn make_double_const(constant: &Constant, static_constants: &mut Vec<StaticConstant>) -> Operand {
    let identifier = format!(".LC{}", static_constants.len()); //TODO - get method name to ensure unique
    static_constants.push(StaticConstant {
        identifier: identifier.clone(),
        init: constant.clone().into(),
        ty: Type::Double,
    });
    Operand::Data(identifier)
}

//Double check that we really need this in all places we call it.
fn value_to_operand(value: &Value, static_constants: &mut Vec<StaticConstant>) -> Operand {
    value.try_into().unwrap_or_else(|_| match value {
        Value::Constant(c) => make_double_const(c, static_constants),
        _ => panic!("Value not supported"),
    })
}

pub fn convert_tacky_instruction_to_codegen_instruction(
    instruction: tacky::Instruction,
    static_constants: &mut Vec<StaticConstant>,
) -> Result<Vec<Instruction>, CompilerError> {
    match instruction {
        tacky::Instruction::Comment(comment) => Ok(vec![Instruction::Comment(comment)]),
        tacky::Instruction::Return(value) => {
            let assembly_type = value.assembly_type(); //TODO: This is incorrect
            let src = value.try_into()?;
            let dst = Operand::Register(Reg::AX);
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
            let assembly_type = src.assembly_type();
            let src = src.try_into()?;
            let dst: Operand = dst.try_into()?;
            Ok(vec![
                Instruction::Cmp(assembly_type, Operand::Immediate { imm: 0 }, src),
                Instruction::Mov {
                    assembly_type,
                    src: Operand::Immediate { imm: 0 },
                    dst: dst.clone(),
                },
                Instruction::SetCC(ConditionCode::E, dst),
            ])
        }
        tacky::Instruction::Unary { op, src, dst } => {
            let assembly_type = dst.assembly_type();
            let src = src.try_into()?;
            let dst: Operand = dst.try_into()?;
            Ok(vec![
                Instruction::Mov {
                    assembly_type,
                    src,
                    dst: dst.clone(),
                },
                Instruction::Unary {
                    assembly_type,
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
            let assembly_type = src1.assembly_type();
            if src1.parse_type().is_signed() {
                let src1 = src1.try_into()?;
                let src2 = src2.try_into()?;
                let dst = dst.try_into()?;
                Ok(vec![
                    Instruction::Mov {
                        assembly_type,
                        src: src1,
                        dst: Operand::Register(Reg::AX),
                    },
                    Instruction::Cdq(assembly_type),
                    Instruction::Idiv {
                        assembly_type,
                        src: src2,
                    },
                    Instruction::Mov {
                        assembly_type,
                        src: Operand::Register(Reg::AX),
                        dst,
                    },
                ])
            } else {
                let src1 = src1.try_into()?;
                let src2 = src2.try_into()?;
                let dst = dst.try_into()?;
                Ok(vec![
                    Instruction::Mov {
                        assembly_type,
                        src: src1,
                        dst: Operand::Register(Reg::AX),
                    },
                    Instruction::Mov {
                        assembly_type,
                        src: Operand::Immediate { imm: 0 },
                        dst: Operand::Register(Reg::DX),
                    },
                    Instruction::Div {
                        assembly_type,
                        src: src2,
                    },
                    Instruction::Mov {
                        assembly_type,
                        src: Operand::Register(Reg::AX),
                        dst,
                    },
                ])
            }
        }
        tacky::Instruction::Binary {
            op: tacky::BinaryOp::Remainder,
            src1,
            src2,
            dst,
        } => {
            if src2.parse_type().is_signed() {
                let assembly_type = src1.assembly_type();
                let src1 = src1.try_into()?;
                let src2 = src2.try_into()?;
                let dst = dst.try_into()?;
                Ok(vec![
                    Instruction::Mov {
                        assembly_type,
                        src: src1,
                        dst: Operand::Register(Reg::AX),
                    },
                    Instruction::Cdq(assembly_type),
                    Instruction::Idiv {
                        assembly_type,
                        src: src2,
                    },
                    Instruction::Mov {
                        assembly_type,
                        src: Operand::Register(Reg::DX),
                        dst,
                    },
                ])
            } else {
                let assembly_type = src1.assembly_type();
                let src1 = src1.try_into()?;
                let src2 = src2.try_into()?;
                let dst = dst.try_into()?;
                Ok(vec![
                    Instruction::Mov {
                        assembly_type,
                        src: src1,
                        dst: Operand::Register(Reg::AX),
                    },
                    Instruction::Mov {
                        assembly_type,
                        src: Operand::Immediate { imm: 0 },
                        dst: Operand::Register(Reg::DX),
                    },
                    Instruction::Div {
                        assembly_type,
                        src: src2,
                    },
                    Instruction::Mov {
                        assembly_type,
                        src: Operand::Register(Reg::DX),
                        dst,
                    },
                ])
            }
        }
        tacky::Instruction::Binary {
            op,
            src1,
            src2,
            dst,
        } if let Ok(cc) = ConditionCode::try_from(op.clone(), src1.parse_type()) => {
            let assembly_type = src1.assembly_type();
            let src1 = src1.try_into()?;
            let src2 = src2.try_into()?;
            let dst: Operand = dst.try_into()?;
            Ok(vec![
                Instruction::Cmp(assembly_type, src2, src1),
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
            let assembly_type = src1.assembly_type();
            let src1 = src1.try_into()?;
            let src2 = src2.try_into()?;
            let dst: Operand = dst.try_into()?;
            let op = op.try_into()?;
            Ok(vec![
                Instruction::Mov {
                    assembly_type,
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
                condition.assembly_type(),
                Operand::Immediate { imm: 0 },
                condition.try_into()?,
            ),
            Instruction::JmpCC(ConditionCode::E, target),
        ]),
        tacky::Instruction::JumpIfNotZero { condition, target } => Ok(vec![
            Instruction::Cmp(
                condition.assembly_type(),
                Operand::Immediate { imm: 0 },
                condition.try_into()?,
            ),
            Instruction::JmpCC(ConditionCode::NE, target),
        ]),
        tacky::Instruction::Copy { src, dst } => Ok(vec![Instruction::Mov {
            assembly_type: dst.assembly_type(),
            src: value_to_operand(&src, static_constants),
            dst: dst.try_into()?,
        }]),
        tacky::Instruction::Label { name } => Ok(vec![Instruction::Label(name)]),
        tacky::Instruction::Jump { target } => Ok(vec![Instruction::Jmp(target)]),
        tacky::Instruction::FunCall { name, args, dst } => convert_function_call(name, args, dst),
        tacky::Instruction::SignExtend { src, dst } => {
            let src = src.try_into()?;
            let dst = dst.try_into()?;
            Ok(vec![Instruction::Movsx { src, dst }])
        }
        tacky::Instruction::Truncate { src, dst } => {
            let src = src.try_into()?;
            let dst = dst.try_into()?;
            Ok(vec![Instruction::Mov {
                assembly_type: AssemblyType::LongWord,
                src,
                dst,
            }])
        }
        tacky::Instruction::ZeroExtend { src, dst } => {
            let src = src.try_into()?;
            let dst = dst.try_into()?;
            Ok(vec![Instruction::MovZeroExtend { src, dst }])
        }
        tacky::Instruction::DoubleToInt { src, dst } => {
            let src = src.try_into()?;
            let dst = dst.try_into()?;
            Ok(vec![Instruction::Cvttsd2si(
                AssemblyType::LongWord,
                src,
                dst,
            )])
        }
        tacky::Instruction::DoubleToUInt { src: _, dst: _ }
        | tacky::Instruction::IntToDouble { src: _, dst: _ }
        | tacky::Instruction::UIntToDouble { src: _, dst: _ } => {
            unimplemented!("DoubleToInt, DoubleToUInt, IntToDouble, UIntToDouble")
        }
    }
}
