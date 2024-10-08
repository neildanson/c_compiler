use std::fmt::{Display, Formatter, Result};

#[derive(Debug, PartialEq, Clone)]
pub enum Reg {
    AX,
    CX,
    DX,
    DI,
    SI,
    R8,
    R9,
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
            Reg::R8 => write!(f, "%r8d"),
            Reg::R9 => write!(f, "%r9d"),
            Reg::CX => write!(f, "%ecx"),
            Reg::DI => write!(f, "%edi"),
            Reg::SI => write!(f, "%esi"),
        }
    }
}
