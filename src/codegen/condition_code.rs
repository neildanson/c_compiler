use std::fmt::{Display, Formatter};

use crate::{ast::Type, error::CompilerError, tacky};

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
    BE, //Unsigned Less or Equal
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

impl ConditionCode {
    //TODO : Add more condition codes for unsigned
    pub fn try_from(cc: tacky::BinaryOp, ty: Type) -> Result<Self, CompilerError> {
        match (cc, ty.is_signed()) {
            (tacky::BinaryOp::Equal, _) => Ok(ConditionCode::E),
            (tacky::BinaryOp::NotEqual, _) => Ok(ConditionCode::NE),
            (tacky::BinaryOp::LessThan, true) => Ok(ConditionCode::L),
            (tacky::BinaryOp::LessThan, false) => Ok(ConditionCode::B),
            (tacky::BinaryOp::LessThanOrEqual, true) => Ok(ConditionCode::LE),
            (tacky::BinaryOp::LessThanOrEqual, false) => Ok(ConditionCode::BE),
            (tacky::BinaryOp::GreaterThan, true) => Ok(ConditionCode::G),
            (tacky::BinaryOp::GreaterThan, false) => Ok(ConditionCode::A),
            (tacky::BinaryOp::GreaterThanOrEqual, true) => Ok(ConditionCode::GE),
            (tacky::BinaryOp::GreaterThanOrEqual, false) => Ok(ConditionCode::AE),
            _ => Err(CompilerError::CodeGen(CodeGenError::InvalidConditionCode)),
        }
    }
}
