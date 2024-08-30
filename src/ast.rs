use crate::token::Token;

#[derive(Debug)]
pub struct Ast;


pub fn parse(tokens : Vec<Token>) -> Ast {
    match tokens.as_slice() {
        &[] => Ast,
        &[Token::Int, Token::Identifier(_), Token::LParen, Token::RParen, Token::LBrace, Token::RBrace] => Ast,
        _ => panic!("Failed to parse {:?}", tokens),
    }

}