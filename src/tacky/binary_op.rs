use crate::error::{CodeGenError, CompilerError};
use crate::parse;

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

impl TryFrom<&parse::BinaryOperator> for BinaryOp {
    type Error = CompilerError;
    fn try_from(op: &parse::BinaryOperator) -> Result<Self, Self::Error> {
        match op {
            parse::BinaryOperator::Add => Ok(BinaryOp::Add),
            parse::BinaryOperator::Sub => Ok(BinaryOp::Subtract),
            parse::BinaryOperator::Mul => Ok(BinaryOp::Multiply),
            parse::BinaryOperator::Div => Ok(BinaryOp::Divide),
            parse::BinaryOperator::Mod => Ok(BinaryOp::Remainder),
            parse::BinaryOperator::ShiftLeft => Ok(BinaryOp::ShiftLeft),
            parse::BinaryOperator::ShiftRight => Ok(BinaryOp::ShiftRight),
            parse::BinaryOperator::BitwiseAnd => Ok(BinaryOp::BitwiseAnd),
            parse::BinaryOperator::BitwiseOr => Ok(BinaryOp::BitwiseOr),
            parse::BinaryOperator::BitwiseXor => Ok(BinaryOp::BitwiseXor),
            parse::BinaryOperator::Equal => Ok(BinaryOp::Equal),
            parse::BinaryOperator::NotEqual => Ok(BinaryOp::NotEqual),
            parse::BinaryOperator::LessThan => Ok(BinaryOp::LessThan),
            parse::BinaryOperator::LessThanOrEqual => Ok(BinaryOp::LessThanOrEqual),
            parse::BinaryOperator::GreaterThan => Ok(BinaryOp::GreaterThan),
            parse::BinaryOperator::GreaterThanOrEqual => Ok(BinaryOp::GreaterThanOrEqual),
            _ => Err(CompilerError::CodeGen(CodeGenError::InvalidBinaryOp)),
        }
    }
}

/*
fn convert_binop(op: &parse::BinaryOperator) -> BinaryOp {
    match op {
        parse::BinaryOperator::Add => BinaryOp::Add,
        parse::BinaryOperator::Sub => BinaryOp::Subtract,
        parse::BinaryOperator::Mul => BinaryOp::Multiply,
        parse::BinaryOperator::Div => BinaryOp::Divide,
        parse::BinaryOperator::Mod => BinaryOp::Remainder,
        parse::BinaryOperator::ShiftLeft => BinaryOp::ShiftLeft,
        parse::BinaryOperator::ShiftRight => BinaryOp::ShiftRight,
        parse::BinaryOperator::BitwiseAnd => BinaryOp::BitwiseAnd,
        parse::BinaryOperator::BitwiseOr => BinaryOp::BitwiseOr,
        parse::BinaryOperator::BitwiseXor => BinaryOp::BitwiseXor,
        parse::BinaryOperator::Equal => BinaryOp::Equal,
        parse::BinaryOperator::NotEqual => BinaryOp::NotEqual,
        parse::BinaryOperator::LessThan => BinaryOp::LessThan,
        parse::BinaryOperator::LessThanOrEqual => BinaryOp::LessThanOrEqual,
        parse::BinaryOperator::GreaterThan => BinaryOp::GreaterThan,
        parse::BinaryOperator::GreaterThanOrEqual => BinaryOp::GreaterThanOrEqual,
        op => unimplemented!("Unimplemented Tacky binary operator {:?}", op),
    }
} */
