use super::ast::*;
use crate::{error::CompilerError, lex::Token};
use anyhow::Result;

fn precedence(tok: &Token) -> u16 {
    match tok {
        Token::ShiftLeft | Token::ShiftRight => 1,
        Token::BitwiseAnd | Token::BitwiseOr | Token::BitwiseXor => 1,
        Token::Assignment => 1,
        Token::QuestionMark => 3,
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
    matches!(
        tok,
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
            | Token::GreaterThanOrEqual
            | Token::Assignment
            | Token::QuestionMark
    )
}

fn swallow_one(exp: Token, tokens: &[Token]) -> Result<&[Token]> {
    match tokens {
        [tok, rest @ ..] if *tok == exp => Ok(rest),
        rest => Err(CompilerError::Parse(format!("Expected {:?}, got {:?}", exp, rest)).into()),
    }
}

fn swallow_semicolon(tokens: &[Token]) -> Result<&[Token]> {
    swallow_one(Token::SemiColon, tokens)
}

fn optional<T, F>(parser: F, tokens: &[Token]) -> Result<(Option<T>, &[Token])>
where
    F: Fn(&[Token]) -> Result<(T, &[Token])>,
{
    let result = parser(tokens);
    match result {
        Ok((value, rest)) => Ok((Some(value), rest)),
        _ => Ok((None, tokens)),
    }
}

fn parse_parameter_list(tokens: &[Token]) -> Result<(Vec<Identifier>, &[Token])> {
    let (parameters, rest) = match tokens {
        [Token::Void, rest @ ..] => (Vec::new(), rest),
        [Token::Int, Token::Identifier(name), rest @ ..] => {
            let mut parameters = vec![name.clone()];
            let mut rest = rest;
            while let [Token::Comma, Token::Int, Token::Identifier(name), new_rest @ ..] = rest {
                parameters.push(name.clone());
                rest = new_rest;
            }
            (parameters, rest)
        }
        toks => {
            return Err(CompilerError::Parse(format!(
                "Parameter List Unexpected Tokens {:?}",
                toks
            ))
            .into())
        }
    };
    Ok((parameters, rest))
}

fn parse_argument_list(tokens: &[Token]) -> Result<(Vec<Expression>, &[Token])> {
    match tokens {
        [Token::RParen, rest @ ..] => Ok((Vec::new(), rest)),
        _ => {
            let (expression, rest) = parse_expression(tokens, 0)?;
            let mut arguments = vec![expression];
            let mut rest = rest;
            while let [Token::Comma, new_rest @ ..] = rest {
                let (expression, new_rest) = parse_expression(new_rest, 0)?;
                arguments.push(expression);
                rest = new_rest;
            }
            let rest = swallow_one(Token::RParen, rest)?;
            Ok((arguments, rest))
        }
    }
}

fn parse_factor(tokens: &[Token]) -> Result<(Expression, &[Token])> {
    let (factor, tokens) = match tokens {
        [Token::Constant(c), rest @ ..] => (Expression::Constant(c.parse().unwrap()), rest),
        [Token::Minus, rest @ ..] => {
            let (factor, rest) = parse_factor(rest)?;
            (
                Expression::Unary(UnaryOperator::Negation, Box::new(factor)),
                rest,
            )
        }
        [Token::Not, rest @ ..] => {
            let (factor, rest) = parse_factor(rest)?;
            (
                Expression::Unary(UnaryOperator::Not, Box::new(factor)),
                rest,
            )
        }
        [Token::Tilde, rest @ ..] => {
            let (factor, rest) = parse_factor(rest)?;
            (
                Expression::Unary(UnaryOperator::Tilde, Box::new(factor)),
                rest,
            )
        }
        [Token::DoublePlus, rest @ ..] => {
            let (factor, rest) = parse_factor(rest)?;
            (
                Expression::Unary(UnaryOperator::PreIncrement, Box::new(factor)),
                rest,
            )
        }
        [Token::DoubleMinus, rest @ ..] => {
            let (factor, rest) = parse_factor(rest)?;
            (
                Expression::Unary(UnaryOperator::PreDecrement, Box::new(factor)),
                rest,
            )
        }
        [Token::Identifier(name), Token::DoublePlus, rest @ ..] => (
            Expression::Unary(
                UnaryOperator::PostIncrement,
                Box::new(Expression::Var(name.clone())),
            ),
            rest,
        ),
        [Token::Identifier(name), Token::DoubleMinus, rest @ ..] => (
            Expression::Unary(
                UnaryOperator::PostDecrement,
                Box::new(Expression::Var(name.clone())),
            ),
            rest,
        ),
        [Token::LParen, rest @ ..] => {
            let (expression, rest) = parse_expression(rest, 0)?;
            let rest = swallow_one(Token::RParen, rest)?;
            (expression, rest)
        }
        [Token::Identifier(name), Token::LParen, rest @ ..] => {
            let (arguments, rest) = parse_argument_list(rest)?;
            (Expression::FunctionCall(name.clone(), arguments), rest)
        }
        [Token::Identifier(name), rest @ ..] => (Expression::Var(name.clone()), rest),
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

fn parse_conditional_middle(tokens: &[Token]) -> Result<(Expression, &[Token])> {
    let (expression, tokens) = parse_expression(tokens, 0)?;
    let rest = swallow_one(Token::Colon, tokens)?;
    Ok((expression, rest))
}

fn parse_expression(tokens: &[Token], min_precedence: u16) -> Result<(Expression, &[Token])> {
    let left = parse_factor(tokens)?;
    let (mut left_expr, mut tokens) = left;
    while let Some(next_token) = tokens.iter().next() {
        if !is_binop(next_token) || precedence(next_token) < min_precedence {
            break;
        }

        if next_token == &Token::Assignment {
            let (right_expr, new_tokens) = parse_expression(&tokens[1..], precedence(next_token))?;
            tokens = new_tokens;
            left_expr = Expression::Assignment(Box::new(left_expr), Box::new(right_expr));
            continue;
        }

        if next_token == &Token::QuestionMark {
            let (then_expr, rest) = parse_conditional_middle(&tokens[1..])?;
            let (else_expr, new_tokens) = parse_expression(rest, precedence(next_token))?;
            tokens = new_tokens;
            left_expr = Expression::Conditional(
                Box::new(left_expr),
                Box::new(then_expr),
                Box::new(else_expr),
            );
            continue;
        }

        let (binop, rest) = parse_binop(tokens)?;

        let (right_expr, new_tokens) = parse_expression(rest, precedence(next_token) + 1)?;
        tokens = new_tokens;
        left_expr = Expression::BinOp(binop, Box::new(left_expr), Box::new(right_expr));
    }

    let result = (left_expr, tokens);
    Ok(result)
}

fn parse_optional_expression(tokens: &[Token]) -> Result<(Option<Expression>, &[Token])> {
    optional(|tokens| parse_expression(tokens, 0), tokens)
}

fn parse_for_init(tokens: &[Token]) -> Result<(ForInit, &[Token])> {
    let decl = parse_variable_declaration(tokens);

    match decl {
        Ok((decl, rest)) => Ok((ForInit::InitDeclaration(decl), rest)),
        _ => {
            let (expression, rest) = parse_optional_expression(tokens)?;
            let rest = swallow_semicolon(rest)?;
            Ok((ForInit::InitExpression(expression), rest))
        }
    }
}

fn parse_statement(tokens: &[Token]) -> Result<(Statement, &[Token])> {
    let (statement, tokens) = match tokens {
        [Token::Return, rest @ ..] => {
            let (expression, rest) = parse_expression(rest, 0)?;
            let rest = swallow_semicolon(rest)?;
            (Statement::Return(expression), rest)
        }
        [Token::SemiColon, rest @ ..] => (Statement::Null, rest),
        [Token::If, Token::LParen, rest @ ..] => {
            let (expression, rest) = parse_expression(rest, 0)?;
            let rest = swallow_one(Token::RParen, rest)?;
            let (then, rest) = parse_statement(rest)?;
            let (els, rest) = match rest {
                [Token::Else, rest @ ..] => {
                    let (els, rest) = parse_statement(rest)?;
                    (Some(Box::new(els)), rest)
                }
                rest => (None, rest),
            };
            (Statement::If(expression, Box::new(then), els), rest)
        }
        [Token::LBrace, rest @ ..] => {
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
                    Err(_) => { //Combine the two error cases
                        is_ok = false;
                    }
                }
            }
            let rest = swallow_one(Token::RBrace, rest)?;
            (Statement::Compound(statements), rest)
        }
        [Token::Do, rest @ ..] => {
            let (statement, rest) = parse_statement(rest)?;
            let rest = swallow_one(Token::While, rest)?;
            let (expression, rest) = parse_expression(rest, 0)?;
            let rest = swallow_semicolon(rest)?;
            (
                Statement::DoWhile(Box::new(statement), expression, None),
                rest,
            )
        }
        [Token::While, Token::LParen, rest @ ..] => {
            let (expression, rest) = parse_expression(rest, 0)?;
            let rest = swallow_one(Token::RParen, rest)?;
            let (statement, rest) = parse_statement(rest)?;
            (
                Statement::While(expression, Box::new(statement), None),
                rest,
            )
        }
        [Token::Break, rest @ ..] => {
            let rest = swallow_semicolon(rest)?;
            (Statement::Break(None), rest)
        }
        [Token::Continue, rest @ ..] => {
            let rest = swallow_semicolon(rest)?;
            (Statement::Continue(None), rest)
        }
        [Token::For, Token::LParen, rest @ ..] => {
            let (init, rest) = parse_for_init(rest)?;
            let (condition, rest) = parse_optional_expression(rest)?;
            let rest = swallow_semicolon(rest)?;
            let (post, rest) = parse_optional_expression(rest)?;
            let rest = swallow_one(Token::RParen, rest)?;
            let (statement, rest) = parse_statement(rest)?;
            (
                Statement::For(init, condition, post, Box::new(statement), None),
                rest,
            )
        }
        tok => {
            let statement = parse_expression(tok, 0)?;
            let (expression, rest) = statement;
            let rest = swallow_semicolon(rest)?;
            (Statement::Expression(expression), rest)
        }
    };
    Ok((statement, tokens))
}

fn parse_variable_declaration(tokens: &[Token]) -> Result<(VariableDeclaration, &[Token])> {
    let (declaration, rest) = match tokens {
        [Token::Int, Token::Identifier(name), Token::Assignment, rest @ ..] => {
            let (expression, rest) = parse_expression(rest, 0)?;
            (
                VariableDeclaration {
                    name: name.clone(),
                    value: Some(expression),
                },
                rest,
            )
        }
        [Token::Int, Token::Identifier(name), rest @ ..] => (
            VariableDeclaration {
                name: name.clone(),
                value: None,
            },
            rest,
        ),

        toks => {
            return Err(
                CompilerError::Parse(format!("Declaration Unexpected Tokens {:?}", toks)).into(),
            )
        }
    };
    let rest = swallow_semicolon(rest)?;
    Ok((declaration, rest))
}

fn parse_declaration(tokens: &[Token]) -> Result<(Declaration, &[Token])> {
    let declaration = parse_variable_declaration(tokens);
    if let Ok((declaration, rest)) = declaration {
        return Ok((Declaration::Variable(declaration), rest));
    }

    let function = parse_function_definition(tokens);
    if let Ok((function, rest)) = function {
        return Ok((Declaration::Function(function), rest));
    }
    Err(CompilerError::Parse("Unexpected tokens".to_string()).into())
}

fn parse_block_item(tokens: &[Token]) -> Result<(BlockItem, &[Token])> {
    let declaration = parse_declaration(tokens);
    if let Ok((declaration, rest)) = declaration {
        return Ok((BlockItem::Declaration(declaration), rest));
    }

    let statement = parse_statement(tokens);
    if let Ok((statement, rest)) = statement {
        return Ok((BlockItem::Statement(statement), rest));
    }
    Err(CompilerError::Parse("Unexpected tokens".to_string()).into())
}

fn parse_function_body(tokens: &[Token]) -> Result<(Vec<BlockItem>, &[Token])> {
    let mut statements = Vec::new();
    let rest = tokens;
    let mut is_ok = true;
    let mut rest = swallow_one(Token::LBrace, rest)?;
    while is_ok {
        let result = parse_block_item(rest);
        match result {
            Ok((block_item, new_rest)) => {
                statements.push(block_item);
                rest = new_rest;
            }
            Err(_) => { //Combine the two error cases
                is_ok = false;
            }
        }
    }
    let rest = swallow_one(Token::RBrace, rest)?;
    Ok((statements, rest))
}

fn parse_function_definition(tokens: &[Token]) -> Result<(FunctionDefinition, &[Token])> {
    let (function, tokens) = match tokens {
        [Token::Int, Token::Identifier(name), Token::LParen, rest @ ..] => {
            let (params, rest) = parse_parameter_list(rest)?;
            let rest = swallow_one(Token::RParen, rest)?;

            let (statements, rest) = optional(parse_function_body, rest)?;
            let rest = match statements {
                Some(_) => rest,
                None => swallow_semicolon(rest)?,
            };

            (
                FunctionDefinition {
                    name: name.clone(),
                    parameters: params,
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
    let mut tokens = tokens;
    let mut functions = Vec::new();
    while let Ok((function, rest)) = parse_function_definition(tokens) {
        tokens = rest;
        functions.push(function);
    }

    if !tokens.is_empty() {
        return Err(CompilerError::Parse(format!(
            "Garbage found after function {:?}, parsed \n\n{:?}",
            tokens, functions
        ))
        .into());
    }
    Ok(Program { functions })
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

    #[test]
    fn test_parse_expression() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("42").unwrap();
        let (expression, rest) = parse_expression(&tokens, 0).unwrap();
        assert_eq!(expression, Expression::Constant(42));
        assert!(rest.is_empty());
    }

    #[test]
    fn test_parse_expression_unary() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("-42").unwrap();
        let (expression, rest) = parse_expression(&tokens, 0).unwrap();
        assert_eq!(
            expression,
            Expression::Unary(UnaryOperator::Negation, Box::new(Expression::Constant(42)))
        );
        assert!(rest.is_empty());
    }

    #[test]
    fn test_parse_function() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("int main(void) { return 42; }").unwrap();
        let (function, rest) = parse_function_definition(&tokens).unwrap();
        assert_eq!(
            function,
            FunctionDefinition {
                name: "main".to_string(),
                parameters: vec![],
                body: Some(vec![BlockItem::Statement(Statement::Return(
                    Expression::Constant(42)
                ))])
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
        let (function, rest) = parse_function_definition(&tokens).unwrap();
        assert_eq!(
            function,
            FunctionDefinition {
                name: "main".to_string(),
                parameters: vec![],
                body: Some(vec![BlockItem::Statement(Statement::Return(
                    Expression::Constant(100)
                ))])
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
        let (function, rest) = parse_function_definition(&tokens).unwrap();
        assert_eq!(
            function,
            FunctionDefinition {
                name: "main".to_string(),
                parameters: vec![],
                body: Some(vec![BlockItem::Statement(Statement::Return(
                    Expression::BinOp(
                        BinaryOperator::Add,
                        Box::new(Expression::Constant(42)),
                        Box::new(Expression::Constant(12))
                    )
                ))])
            }
        );
        assert!(rest.is_empty());
    }
}
