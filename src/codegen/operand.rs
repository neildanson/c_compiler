use crate::tacky;
use std::fmt::{Display, Formatter, Result};

use super::Reg;

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Register(Reg),
    Immediate { imm: i32 },
    Pseudo(String),
    Stack(i32),
}

//https://doc.rust-lang.org/std/fmt/struct.Formatter.html
impl Display for Operand {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Operand::Register(reg) => write!(f, "{:.4}", reg), 
            Operand::Immediate { imm } => write!(f, "${}", imm),
            Operand::Stack(offset) => write!(f, "-{}(%rbp)", offset),
            op => unimplemented!("Operand Display not implemented for {:?}", op),
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
