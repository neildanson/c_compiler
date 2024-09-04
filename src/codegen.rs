use std::collections::HashMap;
use std::fmt::Display;

use crate::*;
use crate::error::CompilerError;

#[derive(Debug)]
enum Reg {
    AX, R10
}

#[derive(Debug)]
enum Operand {
    Register(Reg), 
    Immediate { imm: i32 },
    Pseudo(String),
    Stack(i32)
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Operand::Register(_) => write!(f, "%eax"),
            Operand::Immediate { imm } => write!(f, "${}", imm),
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

#[derive(Debug)]
enum UnaryOp {
    Neg,
    Not,
}

impl From<tacky::UnaryOp> for UnaryOp {
    fn from(op: tacky::UnaryOp) -> Self {
        match op {
            tacky::UnaryOp::Negate => UnaryOp::Neg,
            tacky::UnaryOp::Complement => UnaryOp::Not,
        }
    }
}

#[derive(Debug)]
enum Instruction {
    Mov { src: Operand, dst: Operand },
    Unary { op: UnaryOp, src: Operand, dst: Operand },
    Ret,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Instruction::Mov { src, dst } => write!(f, "movl {}, {}", src, dst),
            Instruction::Ret => write!(f, "ret"),
            _ => unimplemented!(),
        }
    }
}

impl From <tacky::Instruction> for Instruction {
    
    fn from(ast: tacky::Instruction) -> Self {
        match ast {
            tacky::Instruction::Return(value) => {
                let src = value.into();
                let dst = Operand::Register(Reg::AX);
                Instruction::Mov { src, dst }
            },
            tacky::Instruction::Unary { op, src, dst } => {
                let src = src.into();
                let dst = dst.into();
                Instruction::Unary { op: op.into(), src, dst }
            },
        }
    }
}

#[derive(Debug)]
struct Function {
    name: String,
    body: Vec<Instruction>,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "   .globl {}", self.name)?;
        writeln!(f, "{}:", self.name)?;
        for instruction in &self.body {
            writeln!(f, "    {}", instruction)?;
        }
        Ok(())
    }
}


fn replace_pseudo_with_stack(body: Vec<Instruction>) -> Vec<Instruction> {
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
            Instruction::Unary { op, src, dst } => {
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
                new_body.push(Instruction::Unary { op, src, dst });
            },
            any_other => new_body.push(any_other),
        }
    }
    new_body
}


impl From<tacky::Function> for Function {
    fn from(ast: tacky::Function) -> Self {
        let mut body = Vec::new();
        for statement in ast.body {
            let instruction = statement.into();
            body.push(instruction);
        }
        
        let body = replace_pseudo_with_stack(body);

        Function {
            name: ast.name,
            body,
        }
    }
}

#[derive(Debug)]
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
