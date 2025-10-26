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
}

impl AssemblyType {
    pub fn size(&self) -> usize {
        match self {
            AssemblyType::LongWord => 4,
            AssemblyType::QuadWord => 8,
        }
    }
}

impl From<&Type> for AssemblyType {
    fn from(ty: &Type) -> Self {
        match ty {
            Type::Int => AssemblyType::LongWord,
            Type::Long => AssemblyType::QuadWord,
            Type::UInt => AssemblyType::LongWord, //TODO: Check if this is correct
            Type::ULong => AssemblyType::QuadWord, //TODO: Check if this is correct
            Type::Float => AssemblyType::LongWord,
            Type::Double => AssemblyType::QuadWord,
            _ => panic!("Unsupported assembly type for type {:?}", ty),
        }
    }
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
            Value::Constant(Constant::UnsignedInt(_)) => AssemblyType::LongWord, //TODO: Check if this is correct
            Value::Constant(Constant::UnsignedLong(_)) => AssemblyType::QuadWord, //TODO: Check if this is correct
            Value::Constant(Constant::Float(_)) => AssemblyType::LongWord,
            Value::Constant(Constant::Double(_)) => AssemblyType::QuadWord,
            Value::Var(_, ty) => ty.into(),
        }
    }
    pub fn parse_type(&self) -> Type {
        match self {
            Value::Constant(Constant::Int(_)) => Type::Int,
            Value::Constant(Constant::Long(_)) => Type::Long,
            Value::Constant(Constant::UnsignedInt(_)) => Type::UInt,
            Value::Constant(Constant::UnsignedLong(_)) => Type::ULong,
            Value::Constant(Constant::Float(_)) => Type::Float,
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
    // SSE instructions for floating point
    Movss {
        src: Operand,
        dst: Operand,
    },
    Movsd {
        src: Operand,
        dst: Operand,
    },
    Cvtsi2ss {
        assembly_type: AssemblyType,
        src: Operand,
        dst: Operand,
    },
    Cvtsi2sd {
        assembly_type: AssemblyType,
        src: Operand,
        dst: Operand,
    },
    Cvttss2si {
        assembly_type: AssemblyType,
        src: Operand,
        dst: Operand,
    },
    Cvttsd2si {
        assembly_type: AssemblyType,
        src: Operand,
        dst: Operand,
    },
    Cvtss2sd {
        src: Operand,
        dst: Operand,
    },
    Cvtsd2ss {
        src: Operand,
        dst: Operand,
    },
    Addss {
        src: Operand,
        dst: Operand,
    },
    Addsd {
        src: Operand,
        dst: Operand,
    },
    Subss {
        src: Operand,
        dst: Operand,
    },
    Subsd {
        src: Operand,
        dst: Operand,
    },
    Mulss {
        src: Operand,
        dst: Operand,
    },
    Mulsd {
        src: Operand,
        dst: Operand,
    },
    Divss {
        src: Operand,
        dst: Operand,
    },
    Divsd {
        src: Operand,
        dst: Operand,
    },
    Xorps {
        src: Operand,
        dst: Operand,
    },
    Xorpd {
        src: Operand,
        dst: Operand,
    },
    Ucomiss {
        src1: Operand,
        src2: Operand,
    },
    Ucomisd {
        src1: Operand,
        src2: Operand,
    },
    Movd {
        src: Operand,
        dst: Operand,
    },
    Movq {
        src: Operand,
        dst: Operand,
    },
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
                    d => d.asm(AssemblyType::LongWord), //TODO: Check if this is correct
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
            // SSE floating point instructions
            Instruction::Movss { src, dst } => {
                write!(f, "\tmovss {}, {}", src.asm(AssemblyType::LongWord), dst.asm(AssemblyType::LongWord))
            }
            Instruction::Movsd { src, dst } => {
                write!(f, "\tmovsd {}, {}", src.asm(AssemblyType::QuadWord), dst.asm(AssemblyType::QuadWord))
            }
            Instruction::Cvtsi2ss { assembly_type, src, dst } => {
                write!(f, "\tcvtsi2ss{} {}, {}", assembly_type, src.asm(*assembly_type), dst.asm(AssemblyType::LongWord))
            }
            Instruction::Cvtsi2sd { assembly_type, src, dst } => {
                write!(f, "\tcvtsi2sd{} {}, {}", assembly_type, src.asm(*assembly_type), dst.asm(AssemblyType::QuadWord))
            }
            Instruction::Cvttss2si { assembly_type, src, dst } => {
                write!(f, "\tcvttss2si{} {}, {}", assembly_type, src.asm(AssemblyType::LongWord), dst.asm(*assembly_type))
            }
            Instruction::Cvttsd2si { assembly_type, src, dst } => {
                write!(f, "\tcvttsd2si{} {}, {}", assembly_type, src.asm(AssemblyType::QuadWord), dst.asm(*assembly_type))
            }
            Instruction::Cvtss2sd { src, dst } => {
                write!(f, "\tcvtss2sd {}, {}", src.asm(AssemblyType::LongWord), dst.asm(AssemblyType::QuadWord))
            }
            Instruction::Cvtsd2ss { src, dst } => {
                write!(f, "\tcvtsd2ss {}, {}", src.asm(AssemblyType::QuadWord), dst.asm(AssemblyType::LongWord))
            }
            Instruction::Addss { src, dst } => {
                write!(f, "\taddss {}, {}", src.asm(AssemblyType::LongWord), dst.asm(AssemblyType::LongWord))
            }
            Instruction::Addsd { src, dst } => {
                write!(f, "\taddsd {}, {}", src.asm(AssemblyType::QuadWord), dst.asm(AssemblyType::QuadWord))
            }
            Instruction::Subss { src, dst } => {
                write!(f, "\tsubss {}, {}", src.asm(AssemblyType::LongWord), dst.asm(AssemblyType::LongWord))
            }
            Instruction::Subsd { src, dst } => {
                write!(f, "\tsubsd {}, {}", src.asm(AssemblyType::QuadWord), dst.asm(AssemblyType::QuadWord))
            }
            Instruction::Mulss { src, dst } => {
                write!(f, "\tmulss {}, {}", src.asm(AssemblyType::LongWord), dst.asm(AssemblyType::LongWord))
            }
            Instruction::Mulsd { src, dst } => {
                write!(f, "\tmulsd {}, {}", src.asm(AssemblyType::QuadWord), dst.asm(AssemblyType::QuadWord))
            }
            Instruction::Divss { src, dst } => {
                write!(f, "\tdivss {}, {}", src.asm(AssemblyType::LongWord), dst.asm(AssemblyType::LongWord))
            }
            Instruction::Divsd { src, dst } => {
                write!(f, "\tdivsd {}, {}", src.asm(AssemblyType::QuadWord), dst.asm(AssemblyType::QuadWord))
            }
            Instruction::Xorps { src, dst } => {
                write!(f, "\txorps {}, {}", src.asm(AssemblyType::LongWord), dst.asm(AssemblyType::LongWord))
            }
            Instruction::Xorpd { src, dst } => {
                write!(f, "\txorpd {}, {}", src.asm(AssemblyType::QuadWord), dst.asm(AssemblyType::QuadWord))
            }
            Instruction::Ucomiss { src1, src2 } => {
                write!(f, "\tucomiss {}, {}", src1.asm(AssemblyType::LongWord), src2.asm(AssemblyType::LongWord))
            }
            Instruction::Ucomisd { src1, src2 } => {
                write!(f, "\tucomisd {}, {}", src1.asm(AssemblyType::QuadWord), src2.asm(AssemblyType::QuadWord))
            }
            Instruction::Movd { src, dst } => {
                write!(f, "\tmovd {}, {}", src.asm(AssemblyType::LongWord), dst.asm(AssemblyType::LongWord))
            }
            Instruction::Movq { src, dst } => {
                write!(f, "\tmovq {}, {}", src.asm(AssemblyType::QuadWord), dst.asm(AssemblyType::QuadWord))
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
        let assembly_arg = (*arg).clone().into();
        instructions.push(Instruction::Mov {
            assembly_type: arg.assembly_type(),
            src: assembly_arg,
            dst: Operand::arg(i),
        });
    }

    for arg in stack_args.iter().rev() {
        let assembly_arg = (*arg).clone().into();
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
    let ty = dst.parse_type();
    let assembly_type = dst.assembly_type();
    
    // Return values: XMM0 for float/double, RAX for integers
    if ty == Type::Float {
        instructions.push(Instruction::Movss {
            src: Operand::Register(Reg::XMM0),
            dst: dst.into(),
        });
    } else if ty == Type::Double {
        instructions.push(Instruction::Movsd {
            src: Operand::Register(Reg::XMM0),
            dst: dst.into(),
        });
    } else {
        instructions.push(Instruction::Mov {
            assembly_type,
            src: Operand::Register(Reg::AX),
            dst: dst.into(),
        });
    }

    Ok(instructions)
}

// Helper function to load a float/double value into an XMM register
// Handles immediate values by loading through an integer register first
fn load_float_to_xmm(src: Operand, dst: Reg, ty: Type) -> Vec<Instruction> {
    if matches!(src, Operand::Immediate { .. }) {
        // For immediates, load through integer register first
        let assembly_type = if ty == Type::Float {
            AssemblyType::LongWord
        } else {
            AssemblyType::QuadWord
        };
        
        let temp_reg = Reg::R10;
        let mut insts = vec![
            Instruction::Mov {
                assembly_type,
                src: src.clone(),
                dst: Operand::Register(temp_reg),
            }
        ];
        
        if ty == Type::Float {
            insts.push(Instruction::Movd {
                src: Operand::Register(temp_reg),
                dst: Operand::Register(dst),
            });
        } else {
            insts.push(Instruction::Movq {
                src: Operand::Register(temp_reg),
                dst: Operand::Register(dst),
            });
        }
        insts
    } else {
        // For non-immediates, can load directly
        if ty == Type::Float {
            vec![Instruction::Movss {
                src,
                dst: Operand::Register(dst),
            }]
        } else {
            vec![Instruction::Movsd {
                src,
                dst: Operand::Register(dst),
            }]
        }
    }
}

impl TryFrom<tacky::Instruction> for Vec<Instruction> {
    type Error = CompilerError;
    fn try_from(instruction: tacky::Instruction) -> Result<Self, Self::Error> {
        match instruction {
            tacky::Instruction::Comment(comment) => Ok(vec![Instruction::Comment(comment)]),
            tacky::Instruction::Return(value) => {
                let ty = value.parse_type();
                let assembly_type = value.assembly_type();
                let src: Operand = value.clone().into();
                
                // Use XMM0 for float/double return values, RAX for integer types
                if ty == Type::Float || ty == Type::Double {
                    let mut insts = load_float_to_xmm(src, Reg::XMM0, ty);
                    insts.push(Instruction::Ret);
                    Ok(insts)
                } else {
                    Ok(vec![
                        Instruction::Mov {
                            assembly_type,
                            src,
                            dst: Operand::Register(Reg::AX),
                        },
                        Instruction::Ret,
                    ])
                }
            }
            tacky::Instruction::Unary {
                op: tacky::UnaryOp::Not,
                src,
                dst,
            } => {
                let assembly_type = src.assembly_type();
                let src = src.into();
                let dst: Operand = dst.into();
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
                let ty = dst.parse_type();
                let assembly_type = dst.assembly_type();
                let src_op = src.into();
                let dst_op: Operand = dst.into();
                
                // Handle floating point negation
                if matches!(op, tacky::UnaryOp::Negate) && ty == Type::Float {
                    // XOR with sign bit to negate float
                    Ok(vec![
                        Instruction::Movss { src: src_op, dst: dst_op.clone() },
                        Instruction::Mov {
                            assembly_type: AssemblyType::LongWord,
                            src: Operand::Immediate { imm: 0x80000000 },
                            dst: Operand::Register(Reg::R10),
                        },
                        Instruction::Movss {
                            src: Operand::Register(Reg::R10),
                            dst: Operand::Register(Reg::XMM1),
                        },
                        Instruction::Xorps {
                            src: Operand::Register(Reg::XMM1),
                            dst: dst_op,
                        },
                    ])
                } else if matches!(op, tacky::UnaryOp::Negate) && ty == Type::Double {
                    // XOR with sign bit to negate double
                    Ok(vec![
                        Instruction::Movsd { src: src_op, dst: dst_op.clone() },
                        Instruction::Mov {
                            assembly_type: AssemblyType::QuadWord,
                            src: Operand::Immediate { imm: 0x8000000000000000u64 as i128 },
                            dst: Operand::Register(Reg::R10),
                        },
                        Instruction::Movsd {
                            src: Operand::Register(Reg::R10),
                            dst: Operand::Register(Reg::XMM1),
                        },
                        Instruction::Xorpd {
                            src: Operand::Register(Reg::XMM1),
                            dst: dst_op,
                        },
                    ])
                } else {
                    // Integer unary operations
                    Ok(vec![
                        Instruction::Mov {
                            assembly_type,
                            src: src_op,
                            dst: dst_op.clone(),
                        },
                        Instruction::Unary {
                            assembly_type,
                            op: op.into(),
                            dst: dst_op,
                        },
                    ])
                }
            }
            tacky::Instruction::Binary {
                op: tacky::BinaryOp::Divide,
                src1,
                src2,
                dst,
            } => {
                let ty = src1.parse_type();
                let assembly_type = src1.assembly_type();
                
                // Handle floating point division
                if ty == Type::Float {
                    let src1: Operand = src1.into();
                    let src2: Operand = src2.into();
                    let dst: Operand = dst.into();
                    Ok(vec![
                        Instruction::Movss { src: src1, dst: dst.clone() },
                        Instruction::Divss { src: src2, dst },
                    ])
                } else if ty == Type::Double {
                    let src1: Operand = src1.into();
                    let src2: Operand = src2.into();
                    let dst: Operand = dst.into();
                    Ok(vec![
                        Instruction::Movsd { src: src1, dst: dst.clone() },
                        Instruction::Divsd { src: src2, dst },
                    ])
                } else if ty.is_signed() {
                    //TODO: Check if this is correct
                    let src1: Operand = src1.into();
                    let src2: Operand = src2.into();
                    let dst: Operand = dst.into();
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
                    //TODO: Check if this is correct
                    let src1 = src1.into();
                    let src2 = src2.into();
                    let dst = dst.into();
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
                    let assembly_type = src1.assembly_type(); //TODO: Check if this is correct
                    let src1 = src1.into();
                    let src2 = src2.into();
                    let dst = dst.into();
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
                    let assembly_type = src1.assembly_type(); //TODO: Check if this is correct
                    let src1 = src1.into();
                    let src2 = src2.into();
                    let dst = dst.into();
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
                let ty = src1.parse_type();
                let assembly_type = src1.assembly_type();
                let src1: Operand = src1.into();
                let src2: Operand = src2.into();
                let dst: Operand = dst.into();
                
                // Handle floating point comparisons
                if ty == Type::Float {
                    Ok(vec![
                        Instruction::Ucomiss { src1: src2.clone(), src2: src1.clone() },
                        Instruction::Mov {
                            assembly_type: AssemblyType::LongWord,
                            src: Operand::Immediate { imm: 0 },
                            dst: dst.clone(),
                        },
                        Instruction::SetCC(cc, dst),
                    ])
                } else if ty == Type::Double {
                    Ok(vec![
                        Instruction::Ucomisd { src1: src2.clone(), src2: src1.clone() },
                        Instruction::Mov {
                            assembly_type: AssemblyType::QuadWord,
                            src: Operand::Immediate { imm: 0 },
                            dst: dst.clone(),
                        },
                        Instruction::SetCC(cc, dst),
                    ])
                } else {
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
            }

            tacky::Instruction::Binary {
                op,
                src1,
                src2,
                dst,
            } => {
                let ty = src1.parse_type();
                let assembly_type = src1.assembly_type(); //TODO: Check if this is correct
                let src1_op = src1.into();
                let src2_op = src2.into();
                let dst_op: Operand = dst.into();
                
                // Handle floating point operations
                match (ty, &op) {
                    (Type::Float, tacky::BinaryOp::Add) => {
                        Ok(vec![
                            Instruction::Movss { src: src1_op, dst: dst_op.clone() },
                            Instruction::Addss { src: src2_op, dst: dst_op },
                        ])
                    }
                    (Type::Double, tacky::BinaryOp::Add) => {
                        Ok(vec![
                            Instruction::Movsd { src: src1_op, dst: dst_op.clone() },
                            Instruction::Addsd { src: src2_op, dst: dst_op },
                        ])
                    }
                    (Type::Float, tacky::BinaryOp::Subtract) => {
                        Ok(vec![
                            Instruction::Movss { src: src1_op, dst: dst_op.clone() },
                            Instruction::Subss { src: src2_op, dst: dst_op },
                        ])
                    }
                    (Type::Double, tacky::BinaryOp::Subtract) => {
                        Ok(vec![
                            Instruction::Movsd { src: src1_op, dst: dst_op.clone() },
                            Instruction::Subsd { src: src2_op, dst: dst_op },
                        ])
                    }
                    (Type::Float, tacky::BinaryOp::Multiply) => {
                        Ok(vec![
                            Instruction::Movss { src: src1_op, dst: dst_op.clone() },
                            Instruction::Mulss { src: src2_op, dst: dst_op },
                        ])
                    }
                    (Type::Double, tacky::BinaryOp::Multiply) => {
                        Ok(vec![
                            Instruction::Movsd { src: src1_op, dst: dst_op.clone() },
                            Instruction::Mulsd { src: src2_op, dst: dst_op },
                        ])
                    }
                    // Integer operations
                    _ => {
                        let op = op.try_into()?;
                        Ok(vec![
                            Instruction::Mov {
                                assembly_type,
                                src: src1_op,
                                dst: dst_op.clone(),
                            },
                            Instruction::Binary {
                                op,
                                assembly_type,
                                src2: src2_op,
                                dst: dst_op,
                            },
                        ])
                    }
                }
            }
            tacky::Instruction::JumpIfZero { condition, target } => {
                let ty = condition.parse_type();
                let cond_op = condition.clone().into();
                
                if ty == Type::Float {
                    Ok(vec![
                        Instruction::Xorps {
                            src: Operand::Register(Reg::XMM1),
                            dst: Operand::Register(Reg::XMM1),
                        },
                        Instruction::Ucomiss { src1: cond_op, src2: Operand::Register(Reg::XMM1) },
                        Instruction::JmpCC(ConditionCode::E, target),
                    ])
                } else if ty == Type::Double {
                    Ok(vec![
                        Instruction::Xorpd {
                            src: Operand::Register(Reg::XMM1),
                            dst: Operand::Register(Reg::XMM1),
                        },
                        Instruction::Ucomisd { src1: cond_op, src2: Operand::Register(Reg::XMM1) },
                        Instruction::JmpCC(ConditionCode::E, target),
                    ])
                } else {
                    Ok(vec![
                        Instruction::Cmp(
                            condition.assembly_type(),
                            Operand::Immediate { imm: 0 },
                            cond_op,
                        ),
                        Instruction::JmpCC(ConditionCode::E, target),
                    ])
                }
            }
            tacky::Instruction::JumpIfNotZero { condition, target } => {
                let ty = condition.parse_type();
                let cond_op = condition.clone().into();
                
                if ty == Type::Float {
                    Ok(vec![
                        Instruction::Xorps {
                            src: Operand::Register(Reg::XMM1),
                            dst: Operand::Register(Reg::XMM1),
                        },
                        Instruction::Ucomiss { src1: cond_op, src2: Operand::Register(Reg::XMM1) },
                        Instruction::JmpCC(ConditionCode::NE, target),
                    ])
                } else if ty == Type::Double {
                    Ok(vec![
                        Instruction::Xorpd {
                            src: Operand::Register(Reg::XMM1),
                            dst: Operand::Register(Reg::XMM1),
                        },
                        Instruction::Ucomisd { src1: cond_op, src2: Operand::Register(Reg::XMM1) },
                        Instruction::JmpCC(ConditionCode::NE, target),
                    ])
                } else {
                    Ok(vec![
                        Instruction::Cmp(
                            condition.assembly_type(),
                            Operand::Immediate { imm: 0 },
                            cond_op,
                        ),
                        Instruction::JmpCC(ConditionCode::NE, target),
                    ])
                }
            }
            tacky::Instruction::Copy { src, dst } => {
                let ty = dst.parse_type();
                let src_op = src.into();
                let dst_op = dst.into();
                
                if ty == Type::Float {
                    Ok(vec![Instruction::Movss { src: src_op, dst: dst_op }])
                } else if ty == Type::Double {
                    Ok(vec![Instruction::Movsd { src: src_op, dst: dst_op }])
                } else {
                    Ok(vec![Instruction::Mov {
                        assembly_type: AssemblyType::from(&ty),
                        src: src_op,
                        dst: dst_op,
                    }])
                }
            }
            tacky::Instruction::Label { name } => Ok(vec![Instruction::Label(name)]),
            tacky::Instruction::Jump { target } => Ok(vec![Instruction::Jmp(target)]),
            tacky::Instruction::FunCall { name, args, dst } => {
                convert_function_call(name, args, dst)
            }
            tacky::Instruction::SignExtend { src, dst } => {
                let src = src.into();
                let dst = dst.into();
                Ok(vec![Instruction::Movsx { src, dst }])
            }
            tacky::Instruction::Truncate { src, dst } => {
                let src = src.into();
                let dst = dst.into();
                Ok(vec![Instruction::Mov {
                    assembly_type: AssemblyType::LongWord,
                    src,
                    dst,
                }])
            }
            tacky::Instruction::ZeroExtend { src, dst } => {
                let src = src.into();
                let dst = dst.into();
                Ok(vec![Instruction::MovZeroExtend { src, dst }])
            }
            tacky::Instruction::IntToFloat { src, dst } => {
                let assembly_type = src.assembly_type();
                let src = src.into();
                let dst = dst.into();
                Ok(vec![Instruction::Cvtsi2ss {
                    assembly_type,
                    src,
                    dst,
                }])
            }
            tacky::Instruction::IntToDouble { src, dst } => {
                let assembly_type = src.assembly_type();
                let src = src.into();
                let dst = dst.into();
                Ok(vec![Instruction::Cvtsi2sd {
                    assembly_type,
                    src,
                    dst,
                }])
            }
            tacky::Instruction::FloatToInt { src, dst } => {
                let assembly_type = dst.assembly_type();
                let src = src.into();
                let dst = dst.into();
                Ok(vec![Instruction::Cvttss2si {
                    assembly_type,
                    src,
                    dst,
                }])
            }
            tacky::Instruction::DoubleToInt { src, dst } => {
                let assembly_type = dst.assembly_type();
                let src = src.into();
                let dst = dst.into();
                Ok(vec![Instruction::Cvttsd2si {
                    assembly_type,
                    src,
                    dst,
                }])
            }
            tacky::Instruction::FloatToDouble { src, dst } => {
                let src = src.into();
                let dst = dst.into();
                Ok(vec![Instruction::Cvtss2sd { src, dst }])
            }
            tacky::Instruction::DoubleToFloat { src, dst } => {
                let src = src.into();
                let dst = dst.into();
                Ok(vec![Instruction::Cvtsd2ss { src, dst }])
            }
        }
    }
}
