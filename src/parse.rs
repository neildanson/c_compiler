use crate::{error::CompilerError, lex::Token};
use anyhow::Result;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub function: Function,
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
pub enum UnaryOperator {
    Negation,
    Tilde,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Constant(i32),
    //Identifier(String),
    Unary(UnaryOperator, Box<Expression>),
}

fn parse_expression(tokens: &[Token]) -> Result<(Expression, &[Token])> {
    let (expression, tokens) = match tokens {
        [Token::Constant(c), rest @ ..] => (Expression::Constant(c.parse().unwrap()), rest),
        //[Token::Identifier(id), rest @ ..] => (Expression::Identifier(id.clone()), rest),
        [Token::Negation, rest @ ..] => {
            let (expression, rest) = parse_expression(rest)?;
            (
                Expression::Unary(UnaryOperator::Negation, Box::new(expression)),
                rest,
            )
        }
        [Token::Tilde, rest @ ..] => {
            let (expression, rest) = parse_expression(rest)?;
            (
                Expression::Unary(UnaryOperator::Tilde, Box::new(expression)),
                rest,
            )
        }
        [Token::LParen, rest @ ..] => {
            let (expression, rest) = parse_expression(rest)?;
            let rest = match rest {
                [Token::RParen, rest @ ..] => rest,
                _ => return Err(CompilerError::Parse.into()),
            };
            (expression, rest)
        }
        _ => return Err(CompilerError::Parse.into()),
    };
    Ok((expression, tokens))
}

fn parse_statement(tokens: &[Token]) -> Result<(Statement, &[Token])> {
    let (statement, tokens) = match tokens {
        [Token::Return, rest @ ..] => {
            let (expression, rest) = parse_expression(rest)?;
            let rest = match rest {
                [Token::SemiColon, rest @ ..] => rest,
                _ => return Err(CompilerError::Parse.into()),
            };
            (Statement::Return(expression), rest)
        }
        _ => return Err(CompilerError::Parse.into()),
    };
    Ok((statement, tokens))
}

fn parse_function(tokens: &[Token]) -> Result<(Function, &[Token])> {
    let (function, tokens) = match tokens {
        [Token::Int, Token::Identifier(name), Token::LParen, Token::Void, Token::RParen, Token::LBrace, rest @ ..] =>
        {
            let mut statements = Vec::new();
            let mut rest = rest;
            while let [Token::Return, ..] = rest {
                let (statement, new_rest) = parse_statement(rest)?;
                statements.push(statement);
                rest = new_rest;
            }
            let rest = match rest {
                [Token::RBrace, rest @ ..] => rest,
                _ => return Err(CompilerError::Parse.into()),
            };
            (
                Function {
                    name: name.clone(),
                    body: statements,
                },
                rest,
            )
        }
        _ => return Err(CompilerError::Parse.into()),
    };
    Ok((function, tokens))
}

pub fn parse_program(tokens: &[Token]) -> Result<Program> {
    let (function, rest) = parse_function(tokens)?;
    if !rest.is_empty() {
        return Err(CompilerError::Parse.into());
    }
    Ok(Program { function })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex::Tokenizer;

    #[test]
    fn test_parse_statement() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("return 42;").unwrap();
        let (statement, rest) = parse_statement(&tokens).unwrap();
        assert_eq!(statement, Statement::Return(Expression::Constant(42)));
        assert!(rest.is_empty());
    }

    /*
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
     */

    #[test]
    fn test_parse_expression() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("42").unwrap();
        let (expression, rest) = parse_expression(&tokens).unwrap();
        assert_eq!(expression, Expression::Constant(42));
        assert!(rest.is_empty());
    }

    /*
    #[test]
    fn test_parse_expression_identifier() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("x").unwrap();
        let (expression, rest) = parse_expression(&tokens).unwrap();
        assert_eq!(expression, Expression::Identifier("x".to_string()));
        assert!(rest.is_empty());
    }
    */

    #[test]
    fn test_parse_function() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("int main(void) { return 42; }").unwrap();
        let (function, rest) = parse_function(&tokens).unwrap();
        assert_eq!(
            function,
            Function {
                name: "main".to_string(),
                body: vec![Statement::Return(Expression::Constant(42))]
            }
        );
        assert!(rest.is_empty());
    }

    #[test]
    fn test_parse_function_identifier() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer
            .tokenize(
                "
int main(void) {
    // test case w/ multi-digit constant
    return 100;
}",
            )
            .unwrap();
        let (function, rest) = parse_function(&tokens).unwrap();
        assert_eq!(
            function,
            Function {
                name: "main".to_string(),
                body: vec![Statement::Return(Expression::Constant(100))]
            }
        );
        assert!(rest.is_empty());
    }
}
