use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result};

use crate::*;
use crate::error::CompilerError;

#[derive(Debug, PartialEq, Clone)]
enum Reg {
    AX, R10
}

impl Display for Reg {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Reg::AX => write!(f, "%eax"),
            Reg::R10 => write!(f, "%r10"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Operand {
    Register(Reg), 
    Immediate { imm: i32 },
    Pseudo(String),
    Stack(i32)
}

impl Display for Operand {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Operand::Register(reg) => write!(f, "{}", reg),
            Operand::Immediate { imm } => write!(f, "${}", imm),
            Operand::Stack(offset) => write!(f, "{}(%rbp)", offset),
            _ => unimplemented!(),
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
enum Instruction {
    Mov { src: Operand, dst: Operand },
    Unary { op: UnaryOp, dst: Operand },
    AllocateStack(usize),
    Ret,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Instruction::Mov { src, dst } => {
                writeln!(f, "#Mov")?;
                writeln!(f, "\tmovl {}, {}", src, dst)
            },
            Instruction::Ret => {
                writeln!(f, "#Return")?;
                writeln!(f, "\tmovq %rbp, %rsp")?;
                writeln!(f, "\tpopq %rbp")?;
                writeln!(f, "\tret")
            },


            Instruction::Unary { op, dst } => {
                writeln!(f, "#Unary")?;
                writeln!(f, "\t{} {}", op, dst)
            },
            Instruction::AllocateStack(size) => {
                writeln!(f, "#Allocate Stack")?;
                writeln!(f, "\tsubq ${}, %rsp", size * 4)
            },
        }
    }
}

impl From <tacky::Instruction> for Vec<Instruction> {
    
    fn from(ast: tacky::Instruction) -> Self {
        match ast {
            tacky::Instruction::Return(value) => {
                let src = value.into();
                let dst = Operand::Register(Reg::AX);
                vec![
                    Instruction::Mov { src, dst },
                    Instruction::Ret
                ]
            },
            tacky::Instruction::Unary { op, src, dst } => {
                let src = src.into();
                let dst :Operand = dst.into();
                vec![
                    Instruction::Mov { src, dst: dst.clone() },
                    Instruction::Unary { op: op.into(), dst }
                ]
            },
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
        writeln!(f, "\tpushq {}", "%rbp")?;
        writeln!(f, "\tmovq {}, {}", "%rsp", "%rbp")?;
        for instruction in &self.body {
            writeln!(f, "{}", instruction)?;
        }
        Ok(())
    }
}


fn replace_pseudo_with_stack(body: Vec<Instruction>) -> (Vec<Instruction>, usize) {
    let mut stack = HashMap::new();
    let mut new_body = Vec::new();
    for instruction in body {
        match instruction {
            Instruction::Mov { src, dst } => {
                let src = match src {
                    Operand::Pseudo(name) => {
                        if let Some(offset) = stack.get(&name) {
                            Operand::Stack(*offset)
                        } else {
                            let offset = stack.len() as i32 * 4;
                            stack.insert(name, offset);
                            Operand::Stack(offset)
                        }
                    },
                    _ => src,
                };
                let dst = match dst {
                    Operand::Pseudo(name) => {
                        if let Some(offset) = stack.get(&name) {
                            Operand::Stack(*offset)
                        } else {
                            let offset = stack.len() as i32 * 4;
                            stack.insert(name, offset);
                            Operand::Stack(offset)
                        }
                    },
                    _ => dst,
                };
                new_body.push(Instruction::Mov { src, dst });
            },
            Instruction::Unary { op, dst } => {
                let dst = match dst {
                    Operand::Pseudo(name) => {
                        if let Some(offset) = stack.get(&name) {
                            Operand::Stack(*offset)
                        } else {
                            let offset = stack.len() as i32 * 4;
                            stack.insert(name, offset);
                            Operand::Stack(offset)
                        }
                    },
                    _ => dst,
                };
                new_body.push(Instruction::Unary { op,  dst });
            },
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
                        new_body.push(Instruction::Mov { src, dst: Operand::Register(Reg::R10) });
                        new_body.push(Instruction::Mov { src: Operand::Register(Reg::R10), dst });
                        continue;
                    }
                }
                new_body.push(instruction.clone());
            },
            _ => new_body.push(instruction),
        }
    }
    new_body
}


impl From<tacky::Function> for Function {
    fn from(ast: tacky::Function) -> Self {
        let mut body = Vec::new();
        for statement in ast.body {
            let mut instructions :Vec<_> = statement.into();
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

//Todo write tests for psuedo to stack

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_replace_pseudo_with_stack() {
        let body = vec![
            Instruction::Mov { src: Operand::Pseudo("a".to_string()), dst: Operand::Register(Reg::AX) },
            Instruction::Mov { src: Operand::Pseudo("b".to_string()), dst: Operand::Register(Reg::AX) },
            Instruction::Mov { src: Operand::Pseudo("a".to_string()), dst: Operand::Register(Reg::AX) },
        ];
        let (new_body, stack_size) = replace_pseudo_with_stack(body);
        assert_eq!(new_body, vec![
            Instruction::Mov { src: Operand::Stack(0), dst: Operand::Register(Reg::AX) },
            Instruction::Mov { src: Operand::Stack(4), dst: Operand::Register(Reg::AX) },
            Instruction::Mov { src: Operand::Stack(0), dst: Operand::Register(Reg::AX) },
        ]);
        assert_eq!(stack_size, 2);
    }

    #[test]
    fn test_fixup_invalid_instructions() {
        let body = vec![
            Instruction::Mov { src: Operand::Stack (0), dst: Operand::Stack (1) },
        ];
        let new_body = fixup_stack_operations(body);
        assert_eq!(new_body, vec![
            Instruction::Mov { src: Operand::Stack (0), dst: Operand::Register(Reg::R10) },
            Instruction::Mov { src: Operand::Register (Reg::R10), dst: Operand::Stack (1) },
        ]);
    }

    
}