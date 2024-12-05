use std::collections::HashSet;

use super::{Expression, Statement};
use crate::{ast::*, error::CompilerError, lex::Token};
use anyhow::Result;

type ParameterListResult<'a> = (Vec<(Type, Identifier)>, &'a [Token]);
type DeclarationResult<'a> = (Declaration<Statement<Expression>, Expression>, &'a [Token]);
type FunctionBodyResult<'a> = (
    Vec<BlockItem<Statement<Expression>, Expression>>,
    &'a [Token],
);
type FunctionDeclarationResult<'a> = (
    FunctionDeclaration<Statement<Expression>, Expression>,
    &'a [Token],
);
type BlockItemResult<'a> = (BlockItem<Statement<Expression>, Expression>, &'a [Token]);

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

fn parse_nth_parameter(tokens: &[Token]) -> Result<(Identifier, Type, &[Token])> {
    let rest = swallow_one(Token::Comma, tokens)?;
    let (ty, _, rest) = parse_type_and_storage(rest, false)?;
    match rest {
        [Token::Identifier(name), rest @ ..] => Ok((name.clone(), ty, rest)),
        _ => Err(CompilerError::Parse(format!("Parameter Unexpected Tokens {:?}", rest)).into()),
    }
}

fn parse_parameter_list(tokens: &[Token]) -> Result<ParameterListResult> {
    let (parameters, rest) = match tokens {
        [Token::Void, rest @ ..] => (Vec::new(), rest),
        rest => {
            let (ty, _, rest) = parse_type_and_storage(rest, false)?;
            match rest {
                [Token::Identifier(name), rest @ ..] => {
                    let mut parameters = vec![(ty, name.clone())];
                    let mut rest = rest;
                    while let Ok((name, ty, new_rest)) = parse_nth_parameter(rest) {
                        parameters.push((ty, name.clone()));
                        rest = new_rest;
                    }
                    return Ok((parameters, rest));
                }
                _ => {
                    return Err(CompilerError::Parse(format!(
                        "Parameter List Unexpected Tokens {:?}",
                        rest
                    ))
                    .into())
                }
            }
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

fn parse_cast(tokens: &[Token]) -> Result<(Expression, &[Token])> {
    let (ty, _, rest) = parse_type_and_storage(tokens, false)?;
    let rest = swallow_one(Token::RParen, rest)?;
    let (expression, rest) = parse_factor(rest)?;
    let expression = Expression::Cast(ty, Box::new(expression));
    Ok((expression, rest))
}

fn parse_factor(tokens: &[Token]) -> Result<(Expression, &[Token])> {
    let (factor, tokens) = match tokens {
        [Token::LParen, rest @ ..] => {
            let cast = parse_cast(rest);
            match cast {
                Ok((expression, rest)) => (expression, rest),
                Err(_) => {
                    let (expression, rest) = parse_expression(rest, 0)?;
                    let rest = swallow_one(Token::RParen, rest)?;
                    (expression, rest)
                }
            }
        }
        [Token::Constant(c), rest @ ..] => {
            let constant = parse_constant(c)?;
            (Expression::Constant(constant), rest)
        }
        [Token::LongConstant(c), rest @ ..] => {
            let constant = parse_constant(c)?;
            (Expression::Constant(constant), rest)
        }
        [Token::UnsignedIntConstant(c), rest @ ..] => {
            let constant = parse_unsigned_constant(c)?;
            (Expression::Constant(constant), rest)
        }
        [Token::UnsignedLongConstant(c), rest @ ..] => {
            let constant = parse_unsigned_constant(c)?;
            (Expression::Constant(constant), rest)
        }
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

fn parse_for_init(tokens: &[Token]) -> Result<(ForInit<Expression>, &[Token])> {
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

fn parse_statement(tokens: &[Token]) -> Result<(Statement<Expression>, &[Token])> {
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
            let mut error = None;
            while error.is_none() {
                let result = parse_block_item(rest);
                match result {
                    Ok((block_item, new_rest)) => {
                        statements.push(block_item);
                        rest = new_rest;
                    }
                    Err(err) => {
                        error = Some(err);
                    }
                }
            }
            let rest = swallow_one(Token::RBrace, rest);
            match (rest, error) {
                (Ok(rest), _) => (Statement::Compound(statements), rest),
                (_, Some(err)) => return Err(err),
                (Err(err), _) => return Err(err),
            }
        }
        [Token::Do, rest @ ..] => {
            let (statement, rest) = parse_statement(rest)?;
            let rest = swallow_one(Token::While, rest)?;
            let (expression, rest) = parse_expression(rest, 0)?;
            let rest = swallow_semicolon(rest)?;
            (Statement::DoWhile(Box::new(statement), expression), rest)
        }
        [Token::While, Token::LParen, rest @ ..] => {
            let (expression, rest) = parse_expression(rest, 0)?;
            let rest = swallow_one(Token::RParen, rest)?;
            let (statement, rest) = parse_statement(rest)?;
            (Statement::While(expression, Box::new(statement)), rest)
        }
        [Token::Break, rest @ ..] => {
            let rest = swallow_semicolon(rest)?;
            (Statement::Break, rest)
        }
        [Token::Continue, rest @ ..] => {
            let rest = swallow_semicolon(rest)?;
            (Statement::Continue, rest)
        }
        [Token::For, Token::LParen, rest @ ..] => {
            let (init, rest) = parse_for_init(rest)?;
            let (condition, rest) = parse_optional_expression(rest)?;
            let rest = swallow_semicolon(rest)?;
            let (post, rest) = parse_optional_expression(rest)?;
            let rest = swallow_one(Token::RParen, rest)?;
            let (statement, rest) = parse_statement(rest)?;
            (
                Statement::For(init, condition, post, Box::new(statement)),
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

fn parse_storage_class(token: &Token) -> Option<StorageClass> {
    match token {
        Token::Static => Some(StorageClass::Static),
        Token::Extern => Some(StorageClass::Extern),
        _ => None,
    }
}

fn parse_type(token: &[&Token]) -> Result<Type> {
    if token.is_empty() {
        return Err(CompilerError::Parse("Invalid type specifier".to_string()).into());
    }
    let mut unique_tokens: HashSet<&Token> = token.iter().cloned().collect();
    if token.len() != unique_tokens.len() {
        return Err(CompilerError::Parse("Duplicate type specifier".to_string()).into());
    }
    if unique_tokens.contains(&Token::Signed) && unique_tokens.contains(&Token::Unsigned) {
        return Err(CompilerError::Parse("Conflicting type specifier".to_string()).into());
    }

    if unique_tokens.contains(&Token::Unsigned) && unique_tokens.contains(&Token::Long) {
        return Ok(Type::ULong);
    }
    if unique_tokens.contains(&Token::Unsigned) {
        return Ok(Type::UInt);
    }
    if unique_tokens.contains(&Token::Long) {
        return Ok(Type::Long);
    }
    if unique_tokens.contains(&Token::Int) || unique_tokens.remove(&Token::Signed) {
        return Ok(Type::Int);
    }
    Err(CompilerError::Parse(format!(
        "Invalid type specifier {:?} {:?}",
        unique_tokens, token
    ))
    .into())
}

fn parse_type_and_storage(
    specifier_list: &[Token],
    allow_storage: bool,
) -> Result<(Type, Option<StorageClass>, &[Token])> {
    let mut types = Vec::new();
    let mut storage_classes = Vec::new();
    for specifier in specifier_list {
        if specifier == &Token::Int
            || specifier == &Token::Long
            || specifier == &Token::Unsigned
            || specifier == &Token::Signed
        {
            types.push(specifier);
        } else {
            match specifier {
                Token::Static | Token::Extern => {
                    storage_classes.push(specifier);
                }
                _ => break,
            }
        }
    }

    if types.is_empty() {
        return Err(CompilerError::Parse("Invalid type specifier".to_string()).into());
    }
    if storage_classes.len() > 1 {
        return Err(CompilerError::Parse("Invalid storage class specifier".to_string()).into());
    }

    let ty = parse_type(&types)?;

    let storage_class = match storage_classes.first() {
        Some(storage_class) => parse_storage_class(storage_class),
        None => None,
    };

    if !allow_storage && storage_class.is_some() {
        return Err(CompilerError::Parse("Invalid storage class specifier".to_string()).into());
    }

    Ok((
        ty,
        storage_class,
        &specifier_list[types.len() + storage_classes.len()..],
    ))
}

fn parse_variable_declaration(
    tokens: &[Token],
) -> Result<(VariableDeclaration<Expression>, &[Token])> {
    let (var_type, storage_class, rest) = parse_type_and_storage(tokens, true)?;
    let (declaration, rest) = match rest {
        [Token::Identifier(name), Token::Assignment, rest @ ..] => {
            let (expression, rest) = parse_expression(rest, 0)?;
            (
                VariableDeclaration {
                    name: name.clone(),
                    init: Some(expression),
                    var_type,
                    storage_class,
                },
                rest,
            )
        }
        [Token::Identifier(name), rest @ ..] => (
            VariableDeclaration {
                name: name.clone(),
                init: None,
                var_type,
                storage_class,
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

fn parse_declaration(tokens: &[Token]) -> Result<DeclarationResult> {
    let declaration = parse_variable_declaration(tokens);
    if let Ok((declaration, rest)) = declaration {
        return Ok((Declaration::Variable(declaration), rest));
    }

    let function = parse_function_declaration(tokens);
    if let Ok((function, rest)) = function {
        return Ok((Declaration::Function(function), rest));
    }

    match (declaration, function) {
        (Err(e1), Err(e2)) => {
            let e1 = e1.to_string();
            let e2 = e2.to_string();
            let err = format!("{}\n or\n {}", e1, e2);
            Err(CompilerError::Parse(err).into())
        }
        (Err(e1), _) => Err(e1),
        (_, Err(e2)) => Err(e2),
        _ => unreachable!(),
    }
}

fn parse_block_item(tokens: &[Token]) -> Result<BlockItemResult> {
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

fn parse_constant(constant: &str) -> Result<Constant> {
    let is_long = constant.contains("L") || constant.contains("l");
    let constant = constant.replace("L", "").replace("l", "");

    let v = constant.parse::<u64>()?;
    if v > i64::MAX as u64 {
        return Err(CompilerError::Parse("Constant out of range".to_string()).into());
    }

    if !is_long && v < (i32::MAX as u64) {
        Ok(Constant::Int(v as i32))
    } else {
        Ok(Constant::Long(v as i64))
    }
}

fn parse_unsigned_constant(constant: &str) -> Result<Constant> {
    let is_long = constant.contains("L") || constant.contains("l");
    let constant = constant
        .replace("U", "")
        .replace("u", "")
        .replace("L", "")
        .replace("l", "");

    let v = constant.parse::<u64>()?;

    if !is_long && v < (u32::MAX as u64) {
        Ok(Constant::UnsignedInt(v as u32))
    } else {
        Ok(Constant::UnsignedLong(v))
    }
}

fn parse_function_body(tokens: &[Token]) -> Result<FunctionBodyResult> {
    let mut statements = Vec::new();
    let rest = tokens;
    let mut error = None;
    let mut rest = swallow_one(Token::LBrace, rest)?;
    while error.is_none() {
        let result = parse_block_item(rest);
        match result {
            Ok((block_item, new_rest)) => {
                statements.push(block_item);
                rest = new_rest;
            }
            Err(err) => {
                error = Some(err);
            }
        }
    }

    let rest = swallow_one(Token::RBrace, rest);
    match (rest, error) {
        (Ok(rest), _) => Ok((statements, rest)),
        (_, Some(err)) => Err(err),
        (Err(err), _) => Err(err),
    }
}

fn parse_function_declaration(tokens: &[Token]) -> Result<FunctionDeclarationResult> {
    let (fun_type, storage_class, rest) = parse_type_and_storage(tokens, true)?;
    let (function, rest) = match rest {
        [Token::Identifier(name), Token::LParen, rest @ ..] => {
            let (params, rest) = parse_parameter_list(rest)?;
            let rest = swallow_one(Token::RParen, rest)?;

            let (statements, rest) = optional(parse_function_body, rest)?;
            let rest = match statements {
                Some(_) => rest,
                None => swallow_semicolon(rest)?,
            };

            (
                FunctionDeclaration::new(name.clone(), params, statements, fun_type, storage_class),
                rest,
            )
        }

        toks => {
            return Err(
                CompilerError::Parse(format!("Function Unexpected Tokens {:?}", toks)).into(),
            )
        }
    };
    Ok((function, rest))
}

pub fn parse_program(tokens: &[Token]) -> Result<Program<Statement<Expression>, Expression>> {
    let mut tokens = tokens;
    let mut declarations = Vec::new();
    let mut error = None;
    while error.is_none() {
        let definition = parse_declaration(tokens);
        match definition {
            Ok((declaration, rest)) => {
                declarations.push(declaration);
                tokens = rest;
            }
            Err(err) => {
                error = Some(err);
            }
        }
    }

    if let Some(err) = error {
        if !tokens.is_empty() {
            return Err(err);
        }
    }
    Ok(Program { declarations })
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
            Statement::Return(Expression::Constant(Constant::Int(42)))
        );
        assert!(rest.is_empty());
    }

    #[test]
    fn test_parse_expression() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("42;").unwrap();
        let (expression, rest) = parse_expression(&tokens, 0).unwrap();
        assert_eq!(expression, Expression::Constant(Constant::Int(42)));
        assert!(rest.is_empty());
    }

    #[test]
    fn test_parse_expression_unary() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("-42;").unwrap();
        let (expression, rest) = parse_expression(&tokens, 0).unwrap();
        assert_eq!(
            expression,
            Expression::Unary(
                UnaryOperator::Negation,
                Box::new(Expression::Constant(Constant::Int(42))),
            )
        );
        assert!(rest.is_empty());
    }

    #[test]
    fn test_parse_function() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("int main(void) { return 42; }").unwrap();
        let (function, rest) = parse_function_declaration(&tokens).unwrap();
        assert_eq!(
            function,
            FunctionDeclaration::new(
                "main".to_string(),
                vec![],
                Some(vec![BlockItem::Statement(Statement::Return(
                    Expression::Constant(Constant::Int(42))
                ))]),
                Type::Int,
                None
            )
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
        let (function, rest) = parse_function_declaration(&tokens).unwrap();
        assert_eq!(
            function,
            FunctionDeclaration::new(
                "main".to_string(),
                vec![],
                Some(vec![BlockItem::Statement(Statement::Return(
                    Expression::Constant(Constant::Int(100))
                ))]),
                Type::Int,
                None
            )
        );
        assert!(rest.is_empty());
    }

    #[test]
    fn test_parse_function_with_addition() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer
            .tokenize("int main(void) { return 42 + 12; }")
            .unwrap();
        let (function, rest) = parse_function_declaration(&tokens).unwrap();
        assert_eq!(
            function,
            FunctionDeclaration::new(
                "main".to_string(),
                vec![],
                Some(vec![BlockItem::Statement(Statement::Return(
                    Expression::BinOp(
                        BinaryOperator::Add,
                        Box::new(Expression::Constant(Constant::Int(42))),
                        Box::new(Expression::Constant(Constant::Int(12))),
                    )
                ))]),
                Type::Int,
                None
            )
        );

        assert!(rest.is_empty());
    }
}
