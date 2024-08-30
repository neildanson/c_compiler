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

fn parse_expression(tokens: &[Token]) -> Result<(Expression, &[Token])> {
    let (expression, tokens) = match tokens {
        [Token::Constant(c), rest @ ..] => (Expression::Int(c.parse().unwrap()), rest),
        [Token::Identifier(id), rest @ ..] => (Expression::Identifier(id.clone()), rest),
        _ => return Err(ErrorKind::InvalidInput.into()),
    };
    Ok((expression, tokens))
}

fn parse_statement(tokens: &[Token]) -> Result<(Statement, &[Token])> {
    let (statement, tokens) = match tokens {
        [Token::Return, rest @ ..] => {
            let (expression, rest) = parse_expression(rest)?;
            let rest = match rest {
                [Token::SemiColon, rest @ ..] => rest,
                _ => return Err(ErrorKind::InvalidInput.into()),
            };
            (Statement::Return(expression), rest)
        }
        _ => return Err(ErrorKind::InvalidInput.into()),
    };
    Ok((statement, tokens))
}

pub fn parse_function(tokens: &[Token]) -> Result<(Function, &[Token])> {
    let (function, tokens) = match tokens {
        [Token::Int, Token::Identifier(name), Token::LParen, Token::RParen, Token::LBrace, rest @ ..] => {
            let mut statements = Vec::new();
            let mut rest = rest;
            while let [Token::Return, ..] = rest {
                let (statement, new_rest) = parse_statement(rest)?;
                statements.push(statement);
                rest = new_rest;
            }
            let rest = match rest {
                [Token::RBrace, rest @ ..] => rest,
                _ => return Err(ErrorKind::InvalidInput.into()),
            };
            (
                Function {
                    name: name.clone(),
                    body: statements,
                },
                rest,
            )
        }
        _ => return Err(ErrorKind::InvalidInput.into()),
    };
    Ok((function, tokens))
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
        assert_eq!(
            statement,
            Statement::Return(Expression::Identifier("x".to_string()))
        );
        assert!(rest.is_empty());
    }

    #[test]
    fn test_parse_expression() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("42").unwrap();
        let (expression, rest) = parse_expression(&tokens).unwrap();
        assert_eq!(expression, Expression::Int(42));
        assert!(rest.is_empty());
    }

    #[test]
    fn test_parse_expression_identifier() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("x").unwrap();
        let (expression, rest) = parse_expression(&tokens).unwrap();
        assert_eq!(expression, Expression::Identifier("x".to_string()));
        assert!(rest.is_empty());
    }

    #[test]
    fn test_parse_function() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("int main() { return 42; }").unwrap();
        let (function, rest) = parse_function(&tokens).unwrap();
        assert_eq!(
            function,
            Function {
                name: "main".to_string(),
                body: vec![Statement::Return(Expression::Int(42))]
            }
        );
        assert!(rest.is_empty());
    }
}
