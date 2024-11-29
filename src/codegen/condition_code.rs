use std::fmt::{Display, Formatter};

use crate::{error::CompilerError, tacky};

use super::error::CodeGenError;

#[derive(Debug, PartialEq, Clone)]
pub enum ConditionCode {
    E,  // Signed Equal (ZF=1)
    NE, // Signed Not Equal (ZF=0)
    L,  // Signed Less
    LE, // Signed Less or Equal
    G,  // Signed Greater
    GE, // Signed Greater or Equal
    A,  // Unsigned Greater
    AE, // Unsigned Greater or Equal
    B,  // Unsigned Less
    BE,  //CF=1 or ZF=1
}

impl Display for ConditionCode {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            ConditionCode::E => write!(f, "e"),
            ConditionCode::NE => write!(f, "ne"),
            ConditionCode::L => write!(f, "l"),
            ConditionCode::LE => write!(f, "le"),
            ConditionCode::G => write!(f, "g"),
            ConditionCode::GE => write!(f, "ge"),
            ConditionCode::A => write!(f, "a"),
            ConditionCode::AE => write!(f, "ae"),
            ConditionCode::B => write!(f, "b"),
            ConditionCode::BE => write!(f, "be"),
        }
    }
}

impl TryFrom<tacky::BinaryOp> for ConditionCode {
    type Error = CompilerError;
    fn try_from(cc: tacky::BinaryOp) -> Result<Self, Self::Error> {
        match cc {
            tacky::BinaryOp::Equal => Ok(ConditionCode::E),
            tacky::BinaryOp::NotEqual => Ok(ConditionCode::NE),
            tacky::BinaryOp::LessThan => Ok(ConditionCode::L),
            tacky::BinaryOp::LessThanOrEqual => Ok(ConditionCode::LE),
            tacky::BinaryOp::GreaterThan => Ok(ConditionCode::G),
            tacky::BinaryOp::GreaterThanOrEqual => Ok(ConditionCode::GE),
            _ => Err(CompilerError::CodeGen(CodeGenError::InvalidConditionCode)),
        }
    }
}
