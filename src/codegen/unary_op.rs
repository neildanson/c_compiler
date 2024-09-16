use std::fmt::{Display, Formatter, Result};

use crate::tacky;

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            UnaryOp::Neg => write!(f, "negl"),
            op => unimplemented!("UnaryOp Display not implemented for {:?}", op),
        }
    }
}

impl From<tacky::UnaryOp> for UnaryOp {
    fn from(op: tacky::UnaryOp) -> Self {
        match op {
            tacky::UnaryOp::Negate => UnaryOp::Neg,
            tacky::UnaryOp::Complement => UnaryOp::Not,
            tacky::UnaryOp::Not => UnaryOp::Not,
            //x => unimplemented!("UnaryOp not implemented for {:?}", x),
        }
    }
}
