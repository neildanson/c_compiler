use crate::token::Token;
use std::io::{ErrorKind, Result};

#[derive(Debug, PartialEq)]
pub struct Program {
    pub functions: Function,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub body: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Int(i32),
    Identifier(String),
}

fn parse_statement(tokens: &[Token]) -> Result<(Statement, &[Token])> {
    let (statement, tokens) = match tokens {
        [Token::Return, Token::Constant(c), Token::SemiColon, rest @ ..] => {
            (Statement::Return(Expression::Int(c.parse().unwrap())), rest)
        }
        [Token::Return, Token::Identifier(id), Token::SemiColon, rest @ ..] => {
            (Statement::Return(Expression::Identifier(id.clone())), rest)
        }
        _ => return Err(ErrorKind::InvalidInput.into()),
    };
    Ok((statement, tokens))
}



#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Tokenizer;

    #[test]
    fn test_parse_statement() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("return 42;").unwrap();
        let (statement, rest) = parse_statement(&tokens).unwrap();
        assert_eq!(statement, Statement::Return(Expression::Int(42)));
        assert!(rest.is_empty());
    }

    #[test]
    fn test_parse_statement_identifier() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("return x;").unwrap();
        let (statement, rest) = parse_statement(&tokens).unwrap();
        assert_eq!(statement, Statement::Return(Expression::Identifier("x".to_string())));
        assert!(rest.is_empty());
    }
}
