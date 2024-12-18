use super::AssemblyType;

pub enum RegisterSize {
    OneByte,
    FourByte,
    EightByte,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Reg {
    //General Purpose
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
    //SSE
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM14,
    XMM15,
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
                reg => panic!("Invalid register size {:?} for {:?}", assembly_type, reg),
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
                reg => panic!("Invalid register size {:?} for {:?}", assembly_type, reg),
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
                reg => panic!("Invalid register size {:?} for {:?}", assembly_type, reg),
            },
            Some(AssemblyType::Double) => match self {
                Reg::XMM0 => "%xmm0".to_string(),
                Reg::XMM1 => "%xmm1".to_string(),
                Reg::XMM2 => "%xmm2".to_string(),
                Reg::XMM3 => "%xmm3".to_string(),
                Reg::XMM4 => "%xmm4".to_string(),
                Reg::XMM5 => "%xmm5".to_string(),
                Reg::XMM6 => "%xmm6".to_string(),
                Reg::XMM7 => "%xmm7".to_string(),
                Reg::XMM14 => "%xmm14".to_string(),
                Reg::XMM15 => "%xmm15".to_string(),
                reg => panic!("Invalid register size {:?} for {:?}", assembly_type, reg),
            },
        }
    }
}
