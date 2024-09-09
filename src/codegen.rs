use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result};

use crate::*;
//use crate::error::CompilerError;

#[derive(Debug, PartialEq, Clone)]
enum Reg {
    AX,
    DX,
    R10,
    R11,
}

impl Display for Reg {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Reg::AX => write!(f, "%eax"),
            Reg::DX => write!(f, "%edx"),
            Reg::R10 => write!(f, "%r10d"),
            Reg::R11 => write!(f, "%r11d"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Operand {
    Register(Reg),
    Immediate { imm: i32 },
    Pseudo(String),
    Stack(i32),
}

impl Display for Operand {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Operand::Register(reg) => write!(f, "{}", reg),
            Operand::Immediate { imm } => write!(f, "${}", imm),
            Operand::Stack(offset) => write!(f, "-{}(%rbp)", offset),
            _ => unimplemented!("Operand Display not implemented"),
        }
    }
}

impl From<tacky::Value> for Operand {
    fn from(value: tacky::Value) -> Self {
        match value {
            tacky::Value::Constant(imm) => Operand::Immediate { imm },
            tacky::Value::Var(name) => Operand::Pseudo(name),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum UnaryOp {
    Neg,
    Not,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            UnaryOp::Neg => write!(f, "negl"),
            UnaryOp::Not => write!(f, "notl"),
        }
    }
}

impl From<tacky::UnaryOp> for UnaryOp {
    fn from(op: tacky::UnaryOp) -> Self {
        match op {
            tacky::UnaryOp::Negate => UnaryOp::Neg,
            tacky::UnaryOp::Complement => UnaryOp::Not,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum BinaryOp {
    Add,
    Sub,
    Mult,
    Div,
    Rem,
}

impl Display for BinaryOp {
    //TODO
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            BinaryOp::Add => write!(f, "addl"),
            BinaryOp::Sub => write!(f, "subl"),
            BinaryOp::Mult => write!(f, "imull"),
            BinaryOp::Div => write!(f, "idivl"),
            BinaryOp::Rem => write!(f, "idivl"),
        }
    }
}

impl From<tacky::BinaryOp> for BinaryOp {
    fn from(op: tacky::BinaryOp) -> Self {
        match op {
            tacky::BinaryOp::Add => BinaryOp::Add,
            tacky::BinaryOp::Subtract => BinaryOp::Sub,
            tacky::BinaryOp::Multiply => BinaryOp::Mult,
            tacky::BinaryOp::Divide => BinaryOp::Div,
            tacky::BinaryOp::Remainder => BinaryOp::Rem,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Instruction {
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
                let dst = dst.into();
                let op = op.into();
                vec![
                    Instruction::Mov {
                        src: src1,
                        dst: Operand::Register(Reg::AX),
                    },
                    Instruction::Binary { op, src2, dst },
                ]
            }
        }
    }
}

#[derive(Debug, PartialEq)]
struct Function {
    name: String,
    body: Vec<Instruction>,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter) -> Result {
        writeln!(f, "\t.globl {}", self.name)?;
        writeln!(f, "{}:", self.name)?;
        writeln!(f, "\tpushq %rbp")?;
        writeln!(f, "\tmovq %rsp, %rbp")?;
        for instruction in &self.body {
            writeln!(f, "{}", instruction)?;
        }
        Ok(())
    }
}

fn fixup_pseudo(name: String, stack: &mut HashMap<String, i32>) -> Operand {
    if let Some(offset) = stack.get(&name) {
        Operand::Stack(*offset)
    } else {
        let offset = (stack.len() + 1) as i32 * 4;
        stack.insert(name, offset);
        Operand::Stack(offset)
    }
}

fn replace_pseudo_with_stack(body: Vec<Instruction>) -> (Vec<Instruction>, usize) {
    let mut stack = HashMap::new();
    let mut new_body = Vec::new();
    for instruction in body {
        match instruction {
            Instruction::Mov { src, dst } => {
                let src = match src {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack),
                    _ => src,
                };
                let dst = match dst {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack),
                    _ => dst,
                };
                new_body.push(Instruction::Mov { src, dst });
            }
            Instruction::Unary { op, dst } => {
                let dst = match dst {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack),
                    _ => dst,
                };
                new_body.push(Instruction::Unary { op, dst });
            }
            Instruction::Binary { op, src2, dst } => {
                let src2 = match src2 {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack),
                    _ => src2,
                };
                let dst = match dst {
                    Operand::Pseudo(name) => fixup_pseudo(name, &mut stack),
                    _ => dst,
                };
                new_body.push(Instruction::Binary { op, src2, dst });
            }
            any_other => new_body.push(any_other),
        }
    }
    (new_body, stack.len())
}

fn fixup_stack_operations(body: Vec<Instruction>) -> Vec<Instruction> {
    let mut new_body = Vec::new();
    for instruction in body {
        match instruction.clone() {
            Instruction::Mov { src, dst } => {
                if let Operand::Stack(_) = src {
                    if let Operand::Stack(_) = dst {
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
            Instruction::Binary { op, src2, dst } => {
                if let Operand::Stack(_) = dst {
                    if let Operand::Stack(_) = src2 {
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
                    } else {
                        if op == BinaryOp::Mult {
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
                    }
                }
                new_body.push(instruction.clone());
            }
            _ => new_body.push(instruction),
        }
    }
    new_body
}

impl From<tacky::Function> for Function {
    fn from(ast: tacky::Function) -> Self {
        let mut body = Vec::new();
        for statement in ast.body {
            let mut instructions: Vec<_> = statement.into();
            body.append(&mut instructions);
        }

        let (mut body, stack_size) = replace_pseudo_with_stack(body);
        body.insert(0, Instruction::AllocateStack(stack_size));
        let body = fixup_stack_operations(body);
        Function {
            name: ast.name,
            body,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    function: Function,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.function)
    }
}

impl From<tacky::Program> for Program {
    fn from(ast: tacky::Program) -> Self {
        let function = ast.function.into();
        Program { function }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_replace_pseudo_with_stack() {
        let body = vec![
            Instruction::Mov {
                src: Operand::Pseudo("a".to_string()),
                dst: Operand::Register(Reg::AX),
            },
            Instruction::Mov {
                src: Operand::Pseudo("b".to_string()),
                dst: Operand::Register(Reg::AX),
            },
            Instruction::Mov {
                src: Operand::Pseudo("a".to_string()),
                dst: Operand::Register(Reg::AX),
            },
        ];
        let (new_body, stack_size) = replace_pseudo_with_stack(body);
        assert_eq!(
            new_body,
            vec![
                Instruction::Mov {
                    src: Operand::Stack(4),
                    dst: Operand::Register(Reg::AX)
                },
                Instruction::Mov {
                    src: Operand::Stack(8),
                    dst: Operand::Register(Reg::AX)
                },
                Instruction::Mov {
                    src: Operand::Stack(4),
                    dst: Operand::Register(Reg::AX)
                },
            ]
        );
        assert_eq!(stack_size, 2);
    }

    #[test]
    fn fixup_binary_pseudo_with_stack() {
        let body = vec![Instruction::Binary {
            op: BinaryOp::Add,
            src2: Operand::Pseudo("a".to_string()),
            dst: Operand::Pseudo("b".to_string()),
        }];
        let (new_body, _) = replace_pseudo_with_stack(body);
        assert_eq!(
            new_body,
            vec![Instruction::Binary {
                op: BinaryOp::Add,
                src2: Operand::Stack(4),
                dst: Operand::Stack(8)
            }]
        );
    }

    #[test]
    fn fixup_idiv_stack() {
        let body = vec![Instruction::Idiv {
            src: Operand::Immediate { imm: 3 },
        }];
        let new_body = fixup_stack_operations(body);
        assert_eq!(
            new_body,
            vec![
                Instruction::Mov {
                    src: Operand::Immediate { imm: 3 },
                    dst: Operand::Register(Reg::R10)
                },
                Instruction::Idiv {
                    src: Operand::Register(Reg::R10)
                }
            ]
        );
    }

    #[test]
    fn fixup_mul_stack() {
        let body = vec![Instruction::Binary {
            op: BinaryOp::Mult,
            src2: Operand::Immediate { imm: 3 },
            dst: Operand::Stack(4),
        }];
        let new_body = fixup_stack_operations(body);
        assert_eq!(
            new_body,
            vec![
                Instruction::Mov {
                    src: Operand::Stack(4),
                    dst: Operand::Register(Reg::R11)
                },
                Instruction::Binary {
                    op: BinaryOp::Mult,
                    src2: Operand::Immediate { imm: 3 },
                    dst: Operand::Register(Reg::R11)
                },
                Instruction::Mov {
                    src: Operand::Register(Reg::R11),
                    dst: Operand::Stack(4)
                }
            ]
        );
    }

    #[test]
    fn fixup_add_stack() {
        let body = vec![Instruction::Binary {
            op: BinaryOp::Add,
            src2: Operand::Stack(4),
            dst: Operand::Stack(8),
        }];
        let new_body = fixup_stack_operations(body);
        assert_eq!(
            new_body,
            vec![
                Instruction::Mov {
                    src: Operand::Stack(4),
                    dst: Operand::Register(Reg::R10)
                },
                Instruction::Binary {
                    op: BinaryOp::Add,
                    src2: Operand::Register(Reg::R10),
                    dst: Operand::Stack(8)
                },
            ]
        );
    }

    #[test]
    fn fixup_sub_stack() {
        let body = vec![Instruction::Binary {
            op: BinaryOp::Sub,
            src2: Operand::Stack(4),
            dst: Operand::Stack(8),
        }];
        let new_body = fixup_stack_operations(body);
        assert_eq!(
            new_body,
            vec![
                Instruction::Mov {
                    src: Operand::Stack(4),
                    dst: Operand::Register(Reg::R10)
                },
                Instruction::Binary {
                    op: BinaryOp::Sub,
                    src2: Operand::Register(Reg::R10),
                    dst: Operand::Stack(8)
                },
            ]
        );
    }

    #[test]
    fn test_fixup_invalid_instructions() {
        let body = vec![Instruction::Mov {
            src: Operand::Stack(0),
            dst: Operand::Stack(1),
        }];
        let new_body = fixup_stack_operations(body);
        assert_eq!(
            new_body,
            vec![
                Instruction::Mov {
                    src: Operand::Stack(0),
                    dst: Operand::Register(Reg::R10)
                },
                Instruction::Mov {
                    src: Operand::Register(Reg::R10),
                    dst: Operand::Stack(1)
                },
            ]
        );
    }
}
