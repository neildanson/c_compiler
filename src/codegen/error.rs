use std::fmt::Display;

#[derive(Debug)]
pub enum CodeGenError {
    InvalidConditionCode,
    InvalidBinaryOp,
    InvalidDoubleConstant,
}

impl Display for CodeGenError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CodeGenError::InvalidConditionCode => write!(f, "Invalid Condition Code"),
            CodeGenError::InvalidBinaryOp => write!(f, "Invalid Binary Operation"),
            CodeGenError::InvalidDoubleConstant => write!(f, "Invalid Double Constant"),
        }
    }
}
