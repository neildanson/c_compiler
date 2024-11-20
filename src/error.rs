use std::{error::Error, fmt::Display};

use crate::{ast::Type, codegen::error::CodeGenError, validate::error::SemanticAnalysisError};

#[derive(Debug)]
pub enum CompilerError {
    IO(std::io::Error),
    Lex,
    Parse(String),
    SemanticAnalysis(SemanticAnalysisError),
    CodeGen(CodeGenError),
    InvalidCast(Type, Type),
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CompilerError::IO(err) => write!(f, "{}", err),
            CompilerError::Lex => write!(f, "Lexing Error"),
            CompilerError::Parse(s) => write!(f, "Parsing Error : {}", s),
            CompilerError::SemanticAnalysis(s) => write!(f, "Semantic Analysis Error : {}", s),
            CompilerError::CodeGen(c) => write!(f, "Code Generation Error : {}", c),
            CompilerError::InvalidCast(from, to) => {
                write!(f, "Invalid cast from {:?} to {:?}", from, to)
            }
        }
    }
}

impl Error for CompilerError {}
