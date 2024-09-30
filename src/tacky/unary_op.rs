use crate::parse;

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Complement,
    Negate,
    Not,
}

impl From<&parse::UnaryOperator> for UnaryOp {
    fn from(op: &parse::UnaryOperator) -> Self {
        match op {
            parse::UnaryOperator::Tilde => UnaryOp::Complement,
            parse::UnaryOperator::Negation => UnaryOp::Negate,
            parse::UnaryOperator::Not => UnaryOp::Not,
            _ => unimplemented!("UnaryOp not implemented for {:?}", op),
        }
    }
}
