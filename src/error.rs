use std::{error::Error, fmt::Display};

#[derive(Debug)]
pub enum CodeGenError {
    InvalidConditionCode,
    InvalidBinaryOp,
}

impl Display for CodeGenError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CodeGenError::InvalidConditionCode => write!(f, "Invalid Condition Code"),
            CodeGenError::InvalidBinaryOp => write!(f, "Invalid Binary Operation"),
        }
    }
}

#[derive(Debug)]
pub enum SemanticAnalysisError {
    VariableAlreadyDeclared(String),
    VariableNotDeclared(String),
    InvalidLValue,
    NotImplemented,
    InvalidBreak,
    InvalidContinue,
    InvalidBlockItem,
    FunctionNotDeclared(String),
}

impl Display for SemanticAnalysisError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SemanticAnalysisError::VariableAlreadyDeclared(s) => {
                write!(f, "Variable {} already declared", s)
            }
            SemanticAnalysisError::VariableNotDeclared(s) => {
                write!(f, "Variable {} not declared", s)
            }
            SemanticAnalysisError::InvalidLValue => write!(f, "Invalid LValue"),
            SemanticAnalysisError::NotImplemented => write!(f, "Not Implemented"),
            SemanticAnalysisError::InvalidBreak => write!(f, "Invalid Break"),
            SemanticAnalysisError::InvalidContinue => write!(f, "Invalid Continue"),
            SemanticAnalysisError::InvalidBlockItem => write!(f, "Invalid Block Item"),
            SemanticAnalysisError::FunctionNotDeclared(s) => {
                write!(f, "Function {} not declared", s)
            }
        }
    }
}

#[derive(Debug)]
pub enum CompilerError {
    IO(std::io::Error),
    Lex,
    Parse(String),
    SemanticAnalysis(SemanticAnalysisError),
    CodeGen(CodeGenError),
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CompilerError::IO(err) => write!(f, "{}", err),
            CompilerError::Lex => write!(f, "Lexing Error"),
            CompilerError::Parse(s) => write!(f, "Parsing Error : {}", s),
            CompilerError::SemanticAnalysis(s) => write!(f, "Semantic Analysis Error : {}", s),
            CompilerError::CodeGen(c) => write!(f, "Code Generation Error : {}", c),
        }
    }
}

impl Error for CompilerError {}
