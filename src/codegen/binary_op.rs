use crate::tacky;
use std::fmt::{Display, Formatter, Result};

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mult,
}

impl Display for BinaryOp {
    //TODO
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            BinaryOp::Add => write!(f, "addl"),
            BinaryOp::Sub => write!(f, "subl"),
            BinaryOp::Mult => write!(f, "imull"),
        }
    }
}

impl From<tacky::BinaryOp> for BinaryOp {
    fn from(op: tacky::BinaryOp) -> Self {
        match op {
            tacky::BinaryOp::Add => BinaryOp::Add,
            tacky::BinaryOp::Subtract => BinaryOp::Sub,
            tacky::BinaryOp::Multiply => BinaryOp::Mult,
            _ => unimplemented!("BinaryOp From not implemented"),
        }
    }
}
