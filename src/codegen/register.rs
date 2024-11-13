use super::AssemblyType;

pub enum RegisterSize {
    OneByte,
    FourByte,
    EightByte,
}

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
    SP,
}

impl Reg {
    pub fn asm(&self, assembly_type: Option<AssemblyType>) -> String {
        match assembly_type {
            None => match self {
                Reg::AX => "%al".to_string(),
                Reg::CX => "%cl".to_string(),
                Reg::DX => "%dl".to_string(),
                Reg::DI => "%dil".to_string(),
                Reg::SI => "%sil".to_string(),
                Reg::R8 => "%r8b".to_string(),
                Reg::R9 => "%r9b".to_string(),
                Reg::R10 => "%r10b".to_string(),
                Reg::R11 => "%r11b".to_string(),
                Reg::SP => "%rsp".to_string(),
            },
            Some(AssemblyType::LongWord) => match self {
                Reg::AX => "%eax".to_string(),
                Reg::CX => "%ecx".to_string(),
                Reg::DX => "%edx".to_string(),
                Reg::DI => "%edi".to_string(),
                Reg::SI => "%esi".to_string(),
                Reg::R8 => "%r8d".to_string(),
                Reg::R9 => "%r9d".to_string(),
                Reg::R10 => "%r10d".to_string(),
                Reg::R11 => "%r11d".to_string(),
                Reg::SP => "%rsp".to_string(),
            },
            Some(AssemblyType::QuadWord) => match self {
                Reg::AX => "%rax".to_string(),
                Reg::CX => "%rcx".to_string(),
                Reg::DX => "%rdx".to_string(),
                Reg::DI => "%rdi".to_string(),
                Reg::SI => "%rsi".to_string(),
                Reg::R8 => "%r8".to_string(),
                Reg::R9 => "%r9".to_string(),
                Reg::R10 => "%r10".to_string(),
                Reg::R11 => "%r11".to_string(),
                Reg::SP => "%rsp".to_string(),
            },
        }
    }
}
