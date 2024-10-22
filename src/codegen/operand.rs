use crate::{tacky, validate::{IdentifierAttributes, Symbol}};
use std::{collections::HashMap, fmt::{Display, Formatter, Result}};

use super::Reg;

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Register(Reg),
    Immediate { imm: i32 },
    Pseudo(String),
    Stack(i32),
    Data(String)
}

impl Display for Operand {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Operand::Register(reg) => write!(f, "{:.4}", reg),
            Operand::Immediate { imm } => write!(f, "${}", imm),
            Operand::Stack(offset) => write!(f, "{}(%rbp)", offset),
            op => unimplemented!("Operand Display not implemented for {:?}", op),
        }
    }
}

impl Operand {
    pub fn from(value: tacky::Value, symbols : &HashMap<String, Symbol>) -> Self {
        match value {
            tacky::Value::Constant(imm) => Operand::Immediate { imm },
            tacky::Value::Var(name) => 
                if let Some(symbol) = symbols.get(&name) {
                    if let IdentifierAttributes::Static { init : _init, global : _global } = &symbol.attributes {
                         Operand::Data(name)
                    }
                    else {
                        Operand::Pseudo(name)
                    }
                } else {
                    Operand::Pseudo(name)
                },
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
