use crate::tacky;
use std::fmt::{Display, Formatter, Result};

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
    //TODO
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            BinaryOp::Add => write!(f, "addl"),
            BinaryOp::Sub => write!(f, "subl"),
            BinaryOp::Mult => write!(f, "imull"),
            BinaryOp::ShiftLeft => write!(f, "shl"), //TODO
            BinaryOp::ShiftRight => write!(f, "shr"), //TODO
            BinaryOp::BitwiseAnd => write!(f, "andl"), //TODO
            BinaryOp::BitwiseOr => write!(f, "orl"), //TODO
            BinaryOp::BitwiseXor => write!(f, "xorl"), //TODO
        }
    }
}

impl From<tacky::BinaryOp> for BinaryOp {
    fn from(op: tacky::BinaryOp) -> Self {
        match op {
            tacky::BinaryOp::Add => BinaryOp::Add,
            tacky::BinaryOp::Subtract => BinaryOp::Sub,
            tacky::BinaryOp::Multiply => BinaryOp::Mult,
            tacky::BinaryOp::Divide => unimplemented!("BinaryOp From not implemented"),
            tacky::BinaryOp::Remainder => unimplemented!("BinaryOp From not implemented"),
            tacky::BinaryOp::ShiftLeft => BinaryOp::ShiftLeft,
            tacky::BinaryOp::ShiftRight => BinaryOp::ShiftRight,
            tacky::BinaryOp::BitwiseAnd => BinaryOp::BitwiseAnd,
            tacky::BinaryOp::BitwiseOr => BinaryOp::BitwiseOr,
            tacky::BinaryOp::BitwiseXor => BinaryOp::BitwiseXor,
        }
    }
}
