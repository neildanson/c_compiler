use crate::{error::CompilerError, lex::Token};
use anyhow::Result;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub function: Function,
}

#[derive(Debug, PartialEq)]
pub struct Declaration {
    pub name: String,
    pub value: Expression,
}

#[derive(Debug, PartialEq)]
pub enum BlockItem {
    Declaration(Declaration),
    Statement(Statement),
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub body: Vec<BlockItem>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    Null,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    Negation,
    Tilde,
    Not,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Var(String),
    Factor(Factor),
    BinOp(BinaryOperator, Box<Expression>, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
}

#[derive(Debug, PartialEq)]
pub enum Factor {
    Int(i32),
    Unary(UnaryOperator, Box<Factor>),
    Expression(Box<Expression>),
    Identifier(String),
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    ShiftLeft,
    ShiftRight,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

fn precedence(tok: &Token) -> u16 {
    match tok {
        Token::ShiftLeft | Token::ShiftRight => 1,
        Token::BitwiseAnd | Token::BitwiseOr | Token::BitwiseXor => 1,
        Token::Assignment => 1,
        Token::Or => 5,
        Token::And => 10,
        Token::Equal | Token::NotEqual => 30,
        Token::LessThan
        | Token::GreaterThan
        | Token::LessThanOrEqual
        | Token::GreaterThanOrEqual => 35,
        Token::Plus | Token::Minus => 45,
        Token::Asterisk | Token::Slash | Token::Percent => 50,
        _ => 0,
    }
}

fn is_binop(tok: &Token) -> bool {
    match tok {
        Token::Plus
        | Token::Minus
        | Token::Asterisk
        | Token::Slash
        | Token::Percent
        | Token::ShiftLeft
        | Token::ShiftRight
        | Token::BitwiseAnd
        | Token::BitwiseOr
        | Token::BitwiseXor
        | Token::And
        | Token::Or
        | Token::Equal
        | Token::NotEqual
        | Token::LessThan
        | Token::GreaterThan
        | Token::LessThanOrEqual
        | Token::GreaterThanOrEqual => true,
        _ => false,
    }
}

fn parse_factor(tokens: &[Token]) -> Result<(Factor, &[Token])> {
    let (factor, tokens) = match tokens {
        [Token::Constant(c), rest @ ..] => (Factor::Int(c.parse().unwrap()), rest),
        [Token::Minus, rest @ ..] => {
            let (factor, rest) = parse_factor(rest)?;
            (
                Factor::Unary(UnaryOperator::Negation, Box::new(factor)),
                rest,
            )
        }
        [Token::Not, rest @ ..] => {
            let (factor, rest) = parse_factor(rest)?;
            (Factor::Unary(UnaryOperator::Not, Box::new(factor)), rest)
        }
        [Token::Tilde, rest @ ..] => {
            let (factor, rest) = parse_factor(rest)?;
            (Factor::Unary(UnaryOperator::Tilde, Box::new(factor)), rest)
        }
        [Token::LParen, rest @ ..] => {
            let (expression, rest) = parse_expression(rest, 0)?;
            let rest = match rest {
                [Token::RParen, rest @ ..] => rest,
                _ => return Err(CompilerError::Parse("LParen".to_string()).into()),
            };
            (Factor::Expression(Box::new(expression)), rest)
        }
        [Token::Identifier(name), rest @ ..] => (Factor::Identifier(name.clone()), rest),
        toks => {
            return Err(CompilerError::Parse(format!("Factor Unexpected Tokens {:?}", toks)).into())
        }
    };
    Ok((factor, tokens))
}

fn parse_binop(tokens: &[Token]) -> Result<(BinaryOperator, &[Token])> {
    let (binop, tokens) = match tokens {
        [Token::Plus, rest @ ..] => (BinaryOperator::Add, rest),
        [Token::Minus, rest @ ..] => (BinaryOperator::Sub, rest),
        [Token::Asterisk, rest @ ..] => (BinaryOperator::Mul, rest),
        [Token::Slash, rest @ ..] => (BinaryOperator::Div, rest),
        [Token::Percent, rest @ ..] => (BinaryOperator::Mod, rest),
        [Token::ShiftLeft, rest @ ..] => (BinaryOperator::ShiftLeft, rest),
        [Token::ShiftRight, rest @ ..] => (BinaryOperator::ShiftRight, rest),
        [Token::BitwiseAnd, rest @ ..] => (BinaryOperator::BitwiseAnd, rest),
        [Token::BitwiseOr, rest @ ..] => (BinaryOperator::BitwiseOr, rest),
        [Token::BitwiseXor, rest @ ..] => (BinaryOperator::BitwiseXor, rest),
        [Token::And, rest @ ..] => (BinaryOperator::And, rest),
        [Token::Or, rest @ ..] => (BinaryOperator::Or, rest),
        [Token::Equal, rest @ ..] => (BinaryOperator::Equal, rest),
        [Token::NotEqual, rest @ ..] => (BinaryOperator::NotEqual, rest),
        [Token::LessThan, rest @ ..] => (BinaryOperator::LessThan, rest),
        [Token::GreaterThan, rest @ ..] => (BinaryOperator::GreaterThan, rest),
        [Token::LessThanOrEqual, rest @ ..] => (BinaryOperator::LessThanOrEqual, rest),
        [Token::GreaterThanOrEqual, rest @ ..] => (BinaryOperator::GreaterThanOrEqual, rest),
        toks => {
            return Err(CompilerError::Parse(
                format!("BinOp Unexpected Tokens {:?}", toks).to_string(),
            )
            .into())
        }
    };
    Ok((binop, tokens))
}

fn parse_expression(tokens: &[Token], min_precedence: u16) -> Result<(Expression, &[Token])> {
    let left = parse_factor(tokens)?;
    let (left, mut tokens) = left;
    let mut left_expr = Expression::Factor(left);
    //Perhaps just use shunting algorithm here?
    while let Some(next_token) = tokens.iter().next() {
        if next_token == &Token::Assignment {
            let (right_expr, new_tokens) = parse_expression(&tokens[1..], precedence(next_token))?;
            tokens = new_tokens;
            left_expr = Expression::Assignment(Box::new(left_expr), Box::new(right_expr));
            continue;
        }

        if !is_binop(next_token) || precedence(next_token) < min_precedence {
            break;
        }
        let (binop, rest) = parse_binop(tokens)?;

        let (right_expr, new_tokens) = parse_expression(rest, precedence(next_token) + 1)?;
        tokens = new_tokens;
        left_expr = Expression::BinOp(binop, Box::new(left_expr), Box::new(right_expr));
    }
    let result = (left_expr, tokens);
    Ok(result)
}

fn parse_statement(tokens: &[Token]) -> Result<(Statement, &[Token])> {
    let (statement, tokens) = match tokens {
        [Token::Return, rest @ ..] => {
            let (expression, rest) = parse_expression(rest, 0)?;
            let rest = match rest {
                [Token::SemiColon, rest @ ..] => rest,
                _ => return Err(CompilerError::Parse("Expected SemiColon".to_string()).into()),
            };
            (Statement::Return(expression), rest)
        }
        tok => {
            return Err(
                CompilerError::Parse(format!("Statement Unexpected Tokens {:?}", tok)).into(),
            )
        }
    };
    Ok((statement, tokens))
}

fn parse_declaration(tokens : &[Token]) -> Result<(Declaration, &[Token])> {
    let (declaration, tokens) = match tokens {
        [Token::Int, Token::Identifier(name), Token::Assignment, rest @ ..] => {
            let (expression, rest) = parse_expression(rest, 0)?;
            let rest = match rest {
                [Token::SemiColon, rest @ ..] => rest,
                _ => return Err(CompilerError::Parse("Expected SemiColon".to_string()).into()),
            };
            (
                Declaration {
                    name: name.clone(),
                    value: expression,
                },
                rest,
            )
        }
        toks => {
            return Err(
                CompilerError::Parse(format!("Declaration Unexpected Tokens {:?}", toks)).into(),
            )
        }
    };
    Ok((declaration, tokens))
}

fn parse_block_item(tokens: &[Token]) -> Result<(BlockItem, &[Token])> {

    let declaration = parse_declaration(tokens);
    if declaration.is_ok() {
        let (declaration, tokens) = declaration.unwrap();
        return Ok((BlockItem::Declaration(declaration), tokens));
    }

    let statement = parse_statement(tokens);
    if statement.is_ok() {
        let (statement, tokens) = statement.unwrap();
        return Ok((BlockItem::Statement(statement), tokens));
    }
    println!("Failed to parse block item {:?}", tokens);
    Err(CompilerError::Parse("Unexpected tokens".to_string()).into())
}

fn parse_function(tokens: &[Token]) -> Result<(Function, &[Token])> {
    let (function, tokens) = match tokens {
        [Token::Int, Token::Identifier(name), Token::LParen, Token::Void, Token::RParen, Token::LBrace, rest @ ..] =>
        {
            let mut statements = Vec::new();
            let mut rest = rest;
            let mut is_ok = true;
            while is_ok {
                let result = parse_block_item(rest);
                match result {
                    Ok((block_item, new_rest)) => {
                        statements.push(block_item);
                        rest = new_rest;
                    }
                    Err(_) => {
                        is_ok = false;
                    }
                }
            }
            let rest = match rest {
                [Token::RBrace, rest @ ..] => rest,
                rest => return Err(CompilerError::Parse(format!("Expected RBrace, got {:?}", rest)).into()),
            };
            (
                Function {
                    name: name.clone(),
                    body: statements,
                },
                rest,
            )
        }
        toks => {
            return Err(
                CompilerError::Parse(format!("Function Unexpected Tokens {:?}", toks)).into(),
            )
        }
    };
    Ok((function, tokens))
}

pub fn parse_program(tokens: &[Token]) -> Result<Program> {
    let (function, rest) = parse_function(tokens)?;
    if !rest.is_empty() {
        return Err(CompilerError::Parse("Garbage found after function".to_string()).into());
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
        assert_eq!(
            statement,
            Statement::Return(Expression::Factor(Factor::Int(42)))
        );
        assert!(rest.is_empty());
    }
     

    #[test]
    fn test_parse_expression() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("42").unwrap();
        let (expression, rest) = parse_expression(&tokens, 0).unwrap();
        assert_eq!(expression, Expression::Factor(Factor::Int(42)));
        assert!(rest.is_empty());
    }

    #[test]
    fn test_parse_expression_unary() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("-42").unwrap();
        let (expression, rest) = parse_expression(&tokens, 0).unwrap();
        assert_eq!(
            expression,
            Expression::Factor(Factor::Unary(
                UnaryOperator::Negation,
                Box::new(Factor::Int(42))
            ))
        );
        assert!(rest.is_empty());
    }

    #[test]
    fn test_parse_function() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("int main(void) { return 42; }").unwrap();
        let (function, rest) = parse_function(&tokens).unwrap();
        assert_eq!(
            function,
            Function {
                name: "main".to_string(),
                body: vec![BlockItem::Statement(Statement::Return(Expression::Factor(
                    Factor::Int(42)
                )))]
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
                body: vec![BlockItem::Statement(Statement::Return(Expression::Factor(
                    Factor::Int(100)
                )))]
            }
        );
        assert!(rest.is_empty());
    }

    #[test]
    fn test_parse_function_with_addition() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer
            .tokenize("int main(void) { return 42 + 12; }")
            .unwrap();
        let (function, rest) = parse_function(&tokens).unwrap();
        assert_eq!(
            function,
            Function {
                name: "main".to_string(),
                body: vec![BlockItem::Statement(Statement::Return(Expression::BinOp(
                    BinaryOperator::Add,
                    Box::new(Expression::Factor(Factor::Int(42))),
                    Box::new(Expression::Factor(Factor::Int(12)))
                )))]
            }
        );
        assert!(rest.is_empty());
    }
}
