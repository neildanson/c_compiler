use crate::ast;
#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    Complement,
    Negate,
    Not,
}

impl From<&ast::UnaryOperator> for UnaryOp {
    fn from(op: &ast::UnaryOperator) -> Self {
        match op {
            ast::UnaryOperator::Tilde => UnaryOp::Complement,
            ast::UnaryOperator::Negation => UnaryOp::Negate,
            ast::UnaryOperator::Not => UnaryOp::Not,
            _ => unimplemented!("UnaryOp not implemented for {:?}", op),
        }
    }
}
