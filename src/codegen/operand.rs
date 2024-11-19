use crate::tacky;

use super::{AssemblyType, Reg};

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Register(Reg),
    Immediate { imm: i64 },
    Pseudo(String),
    Stack(i32),
    Data(String),
}

impl Operand {
    pub fn asm(&self, assembly_type: AssemblyType) -> String {
        match self {
            Operand::Register(reg) => reg.asm(Some(assembly_type)),
            Operand::Immediate { imm } => format!("${}", imm),
            Operand::Stack(offset) => format!("{}(%rbp)", offset),
            Operand::Data(data) => {
                if cfg!(target_os = "macos") {
                    format!("_{}(%rip)", data)
                } else {
                    format!("{}(%rip)", data)
                }
            }
            op => unimplemented!("Operand Display not implemented for {:?}", op),
        }
    }
}

impl From<tacky::Value> for Operand {
    fn from(value: tacky::Value) -> Self {
        match value {
            tacky::Value::Constant(imm) => Operand::Immediate { imm: imm.as_i64() },
            tacky::Value::Var(name, _) => Operand::Pseudo(name), 
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
            Operand::Stack(
                ((offset - ARG_REGISTERS.len() + 2) * AssemblyType::QuadWord.size()) as i32,
            ) // 8 bytes per argument
        }
    }
}
