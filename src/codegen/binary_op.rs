use crate::{error::CompilerError, tacky};
use std::fmt::{Display, Formatter};

use super::error::CodeGenError;

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mult,
    ShiftLeft,
    ShiftRight,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "add"),
            BinaryOp::Sub => write!(f, "sub"),
            BinaryOp::Mult => write!(f, "imul"),
            BinaryOp::ShiftLeft => write!(f, "shl"),
            BinaryOp::ShiftRight => write!(f, "shr"),
            BinaryOp::BitwiseAnd => write!(f, "and"),
            BinaryOp::BitwiseOr => write!(f, "or"),
            BinaryOp::BitwiseXor => write!(f, "xor"),
        }
    }
}

impl TryFrom<tacky::BinaryOp> for BinaryOp {
    type Error = CompilerError;
    fn try_from(op: tacky::BinaryOp) -> Result<Self, Self::Error> {
        match op {
            tacky::BinaryOp::Add => Ok(BinaryOp::Add),
            tacky::BinaryOp::Subtract => Ok(BinaryOp::Sub),
            tacky::BinaryOp::Multiply => Ok(BinaryOp::Mult),
            tacky::BinaryOp::ShiftLeft => Ok(BinaryOp::ShiftLeft),
            tacky::BinaryOp::ShiftRight => Ok(BinaryOp::ShiftRight),
            tacky::BinaryOp::BitwiseAnd => Ok(BinaryOp::BitwiseAnd),
            tacky::BinaryOp::BitwiseOr => Ok(BinaryOp::BitwiseOr),
            tacky::BinaryOp::BitwiseXor => Ok(BinaryOp::BitwiseXor),
            _ => Err(CompilerError::CodeGen(CodeGenError::InvalidBinaryOp)),
        }
    }
}
