use std::{error::Error, fmt::Display};

#[derive(Debug)]
pub enum CompilerError {
    IO(std::io::Error),
    Lex,
    Parse(String),
    CodeGen,
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CompilerError::IO(err) => write!(f, "{}", err),
            CompilerError::Lex => write!(f, "Lexing Error"),
            CompilerError::Parse(s) => write!(f, "Parsing Error : {}", s),
            CompilerError::CodeGen => write!(f, "Code Generation Error"),
        }
    }
}

impl Error for CompilerError {}
