use crate::ast;
use crate::error::{CodeGenError, CompilerError};

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    ShiftLeft,
    ShiftRight,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

impl TryFrom<&ast::BinaryOperator> for BinaryOp {
    type Error = CompilerError;
    fn try_from(op: &ast::BinaryOperator) -> Result<Self, Self::Error> {
        match op {
            ast::BinaryOperator::Add => Ok(BinaryOp::Add),
            ast::BinaryOperator::Sub => Ok(BinaryOp::Subtract),
            ast::BinaryOperator::Mul => Ok(BinaryOp::Multiply),
            ast::BinaryOperator::Div => Ok(BinaryOp::Divide),
            ast::BinaryOperator::Mod => Ok(BinaryOp::Remainder),
            ast::BinaryOperator::ShiftLeft => Ok(BinaryOp::ShiftLeft),
            ast::BinaryOperator::ShiftRight => Ok(BinaryOp::ShiftRight),
            ast::BinaryOperator::BitwiseAnd => Ok(BinaryOp::BitwiseAnd),
            ast::BinaryOperator::BitwiseOr => Ok(BinaryOp::BitwiseOr),
            ast::BinaryOperator::BitwiseXor => Ok(BinaryOp::BitwiseXor),
            ast::BinaryOperator::Equal => Ok(BinaryOp::Equal),
            ast::BinaryOperator::NotEqual => Ok(BinaryOp::NotEqual),
            ast::BinaryOperator::LessThan => Ok(BinaryOp::LessThan),
            ast::BinaryOperator::LessThanOrEqual => Ok(BinaryOp::LessThanOrEqual),
            ast::BinaryOperator::GreaterThan => Ok(BinaryOp::GreaterThan),
            ast::BinaryOperator::GreaterThanOrEqual => Ok(BinaryOp::GreaterThanOrEqual),
            _ => Err(CompilerError::CodeGen(CodeGenError::InvalidBinaryOp)),
        }
    }
}
