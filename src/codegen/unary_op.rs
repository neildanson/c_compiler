use std::fmt::{Display, Formatter, Result};

use crate::tacky;

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Neg,
    Complement,
    Shr,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            UnaryOp::Neg => write!(f, "neg"),
            UnaryOp::Complement => write!(f, "not"),
            op => unimplemented!("UnaryOp Display not implemented for {:?}", op),
        }
    }
}

impl From<tacky::UnaryOp> for UnaryOp {
    fn from(op: tacky::UnaryOp) -> Self {
        match op {
            tacky::UnaryOp::Negate => UnaryOp::Neg,
            tacky::UnaryOp::Complement => UnaryOp::Complement,
            x => unimplemented!("UnaryOp not implemented for {:?}", x),
        }
    }
}
