use std::fmt::{Display, Formatter, Result};
use crate::tacky;


#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mult,
    Div,
    Rem,
}

impl Display for BinaryOp {
    //TODO
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            BinaryOp::Add => write!(f, "addl"),
            BinaryOp::Sub => write!(f, "subl"),
            BinaryOp::Mult => write!(f, "imull"),
            BinaryOp::Div => write!(f, "idivl"),
            BinaryOp::Rem => write!(f, "idivl"),
        }
    }
}

impl From<tacky::BinaryOp> for BinaryOp {
    fn from(op: tacky::BinaryOp) -> Self {
        match op {
            tacky::BinaryOp::Add => BinaryOp::Add,
            tacky::BinaryOp::Subtract => BinaryOp::Sub,
            tacky::BinaryOp::Multiply => BinaryOp::Mult,
            tacky::BinaryOp::Divide => BinaryOp::Div,
            tacky::BinaryOp::Remainder => BinaryOp::Rem,
        }
    }
}