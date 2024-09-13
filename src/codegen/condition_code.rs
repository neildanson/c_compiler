use crate::{error::CompilerError, tacky};

use super::*;

#[derive(Debug, PartialEq, Clone)]
pub enum ConditionCode {
    E, // Equal
    NE, // Not Equal
    L, // Less
    LE, // Less or Equal
    G, // Greater
    GE, // Greater or Equal
}

//Try from?
impl TryFrom <tacky::BinaryOp> for ConditionCode {
    type Error = CompilerError;
    fn try_from(cc: tacky::BinaryOp) -> Result<ConditionCode, Self::Error> {
        match cc {
            tacky::BinaryOp::Equal => Ok(ConditionCode::E),
            tacky::BinaryOp::NotEqual => Ok(ConditionCode::NE),
            tacky::BinaryOp::LessThan => Ok(ConditionCode::L),
            tacky::BinaryOp::LessThanOrEqual => Ok(ConditionCode::LE),
            tacky::BinaryOp::GreaterThan => Ok(ConditionCode::G),
            tacky::BinaryOp::GreaterThanOrEqual => Ok(ConditionCode::GE),
            _ => Err(CompilerError::CodeGen)
        }
    }
}