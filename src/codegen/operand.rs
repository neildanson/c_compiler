use crate::tacky;
use std::fmt::{Display, Formatter, Result};

use super::Reg;

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Register(Reg),
    Immediate { imm: i32 },
    Pseudo(String),
    Stack(i32),
    Data(String),
}

impl Display for Operand {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Operand::Register(reg) => write!(f, "{:.4}", reg),
            Operand::Immediate { imm } => write!(f, "${}", imm),
            Operand::Stack(offset) => write!(f, "{}(%rbp)", offset),
            Operand::Data(data) => {
                if cfg!(target_os = "macos") {
                    write!(f, "_{}(%rip)", data)
                } else {
                    write!(f, "{}(%rip)", data)
                }
            }
            op => unimplemented!("Operand Display not implemented for {:?}", op),
        }
    }
}

impl From<tacky::Value> for Operand {
    fn from(value: tacky::Value) -> Self {
        match value {
            tacky::Value::Constant(imm) => Operand::Immediate { imm : imm.i32() },
            tacky::Value::Var(name) => Operand::Pseudo(name),
        }
    }
}

const ARG_REGISTERS: [Reg; 6] = [Reg::DI, Reg::SI, Reg::DX, Reg::CX, Reg::R8, Reg::R9];

impl Operand {
    pub fn local(offset: i32) -> Self {
        Operand::Stack(-offset)
    }

    pub fn arg(offset: usize) -> Self {
        if offset < ARG_REGISTERS.len() {
            Operand::Register(ARG_REGISTERS[offset].clone())
        } else {
            Operand::Stack(((offset - ARG_REGISTERS.len() + 2) * 8) as i32) // 8 bytes per argument
        }
    }
}
