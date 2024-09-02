use std::{error::Error, fmt::Display};

#[derive(Debug)]
pub enum CompilerError {
    IO(std::io::Error),
    Lex,
    Parse,
    CodeGen,
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CompilerError::IO(err) => write!(f, "{}", err.to_string()),
            CompilerError::Lex => write!(f, "Lexing Error"),
            CompilerError::Parse => write!(f, "Parsing Error"),
            CompilerError::CodeGen => write!(f, "Code Generation Error"),
        }
    }
}

impl Error for CompilerError {
    fn description(&self) -> &str {
        match self {
            CompilerError::IO(_) => "File Things",
            CompilerError::Lex => "Lexing Error",
            CompilerError::Parse => "Parsing Error",
            CompilerError::CodeGen => "Code Generation Error",
        }
    }
}
