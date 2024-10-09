use crate::tacky;
use std::fmt::{Display, Formatter, Result};

use super::Reg;

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Register(Reg),
    Immediate { imm: i32 },
    Pseudo(String),
    Stack(usize),
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

impl From<usize> for Operand {
    fn from(offset: usize) -> Self {
        let arg_registers = [Reg::DI, Reg::SI, Reg::DX, Reg::CX, Reg::R8, Reg::R9];

        if offset < arg_registers.len() {
            Operand::Register(arg_registers[offset].clone())
        } else {
            Operand::Stack(offset * 8) //I think this is bogus
        }
    }
}
