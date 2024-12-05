use anyhow::Result;
use regex::Regex;

use crate::error::CompilerError;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Token {
    Comment(String),
    PreProcessorDirective(String), //#[a-zA-Z_]\w*\b
    Identifier(String),            //[a-zA-Z_]\w*\b
    Constant(String),              //[0-9]+\b
    LongConstant(String),          //[0-9]+[lL]\b
    UnsignedIntConstant(String),   //[0-9]+[uU]\b
    UnsignedLongConstant(String),  //[0-9]+[lL][uU]|[uU]|[lL]\b
    Int,                           //int\b
    Long,                          //long\b
    Void,                          //void\b
    Return,                        //return\b
    Static,                        //static\b
    Extern,                        //extern\b
    If,                            //if\b
    Else,                          //else\b
    LParen,                        //\(
    RParen,                        //\)
    LBrace,                        //{
    RBrace,                        //}
    SemiColon,                     //;
    Tilde,                         //~
    DoubleMinus,                   //--
    DoublePlus,                    //++
    Plus,                          //+
    Minus,                         //-
    Asterisk,                      //*
    Slash,                         // /
    Percent,                       //%
    ShiftLeft,                     //<<
    ShiftRight,                    //>>
    BitwiseXor,                    //^
    BitwiseOr,                     //^|
    BitwiseAnd,                    //^&
    Not,                           //^!
    And,                           //^&&
    Or,                            //^||
    Equal,                         //^==
    NotEqual,                      //^!=
    LessThan,                      //<
    GreaterThan,                   //>
    LessThanOrEqual,               //<=
    GreaterThanOrEqual,            //>=
    Assignment,                    //=
    QuestionMark,                  //?
    Colon,                         //:
    Do,                            //do\b
    While,                         //while\b
    For,                           //for\b
    Break,                         //break\b
    Continue,                      //continue\b
    Comma,                         //,
    Signed,                        //signed\b
    Unsigned,                      //unsigned\b
}

pub struct Tokenizer {
    token_mappers: Vec<TokenMapper>,
}

impl Default for Tokenizer {
    fn default() -> Self {
        Self::new()
    }
}

struct TokenMapper {
    regex: Regex,
    token: Box<dyn Fn(String) -> Token>,
    back_one : bool,
}

impl TokenMapper {
    fn new(regex: &str, token: Box<dyn Fn(String) -> Token>, back_one:bool) -> Self {
        TokenMapper {
            regex: Regex::new(regex).unwrap(),
            token,
            back_one
        }
    }

    fn map<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        if let Some(mat) = self.regex.find(input) {
            let s = mat.as_str();
            let s = if self.back_one {
                &s[..s.len()-1]
            } else {
                s
            };
            Some(((self.token)(s.to_string()), &input[s.len()..]))
        } else {
            None
        }
    }
}

impl Tokenizer {
    pub fn new() -> Self {
        let token_mappers = vec![
            TokenMapper::new(r"^/[*]([^*]|([*][^/]))*[*]/", Box::new(Token::Comment), false),
            TokenMapper::new(r"^//(.*)", Box::new(Token::Comment), false),
            TokenMapper::new(r"^#(.*)", Box::new(Token::PreProcessorDirective), false),
            TokenMapper::new(r"^void\b", Box::new(|_| Token::Void), false),
            TokenMapper::new(r"^return\b", Box::new(|_| Token::Return), false),
            TokenMapper::new(r"^static\b", Box::new(|_| Token::Static), false),
            TokenMapper::new(r"^extern\b", Box::new(|_| Token::Extern), false),
            TokenMapper::new(r"^int\b", Box::new(|_| Token::Int), false),
            TokenMapper::new(r"^long\b", Box::new(|_| Token::Long), false),
            TokenMapper::new(r"^if\b", Box::new(|_| Token::If), false),
            TokenMapper::new(r"^else\b", Box::new(|_| Token::Else), false),
            TokenMapper::new(r"^do\b", Box::new(|_| Token::Do), false),
            TokenMapper::new(r"^while\b", Box::new(|_| Token::While), false),
            TokenMapper::new(r"^for\b", Box::new(|_| Token::For), false),
            TokenMapper::new(r"^break\b", Box::new(|_| Token::Break), false),
            TokenMapper::new(r"^continue\b", Box::new(|_| Token::Continue), false),
            TokenMapper::new(r"^signed\b", Box::new(|_| Token::Signed), false),
            TokenMapper::new(r"^unsigned\b", Box::new(|_| Token::Unsigned), false),
            TokenMapper::new(r"^\(", Box::new(|_| Token::LParen), false),
            TokenMapper::new(r"^\)", Box::new(|_| Token::RParen), false),
            TokenMapper::new(r"^\{", Box::new(|_| Token::LBrace), false),
            TokenMapper::new(r"^\}", Box::new(|_| Token::RBrace), false),
            TokenMapper::new(r"^;", Box::new(|_| Token::SemiColon), false),
            TokenMapper::new(r"^[a-zA-Z_]\w*\b", Box::new(Token::Identifier), false),
            TokenMapper::new(r"^([0-9]+)[^\w.]", Box::new(Token::Constant), true),
            TokenMapper::new(r"^([0-9]+[lL])[^\w.]", Box::new(Token::LongConstant), true),
            TokenMapper::new(r"^([0-9]+[uU])[^\w.]", Box::new(Token::UnsignedIntConstant), true),
            TokenMapper::new(
                r"^([0-9]+([lL][uU]|[uU][lL]))[^\w.]",
                Box::new(Token::UnsignedLongConstant),
                true,
            ),
            TokenMapper::new(r"^~", Box::new(|_| Token::Tilde), false),
            TokenMapper::new(r"^--", Box::new(|_| Token::DoubleMinus), false),
            TokenMapper::new(r"^\+\+", Box::new(|_| Token::DoublePlus), false),
            TokenMapper::new(r"^<=", Box::new(|_| Token::LessThanOrEqual), false),
            TokenMapper::new(r"^>=", Box::new(|_| Token::GreaterThanOrEqual), false),
            TokenMapper::new(r"^<<", Box::new(|_| Token::ShiftLeft), false),
            TokenMapper::new(r"^>>", Box::new(|_| Token::ShiftRight), false),
            TokenMapper::new(r"^==", Box::new(|_| Token::Equal), false),
            TokenMapper::new(r"^&&", Box::new(|_| Token::And), false),
            TokenMapper::new(r"^!=", Box::new(|_| Token::NotEqual), false),
            TokenMapper::new(r"^\|\|", Box::new(|_| Token::Or), false),
            TokenMapper::new(r"^\+", Box::new(|_| Token::Plus), false),
            TokenMapper::new(r"^-", Box::new(|_| Token::Minus), false),
            TokenMapper::new(r"^\*", Box::new(|_| Token::Asterisk), false),
            TokenMapper::new(r"^/", Box::new(|_| Token::Slash), false),
            TokenMapper::new(r"^%", Box::new(|_| Token::Percent), false),
            TokenMapper::new(r"^\^", Box::new(|_| Token::BitwiseXor), false),
            TokenMapper::new(r"^\|", Box::new(|_| Token::BitwiseOr), false),
            TokenMapper::new(r"^&", Box::new(|_| Token::BitwiseAnd), false),
            TokenMapper::new(r"^!", Box::new(|_| Token::Not), false),
            TokenMapper::new(r"^<", Box::new(|_| Token::LessThan), false),
            TokenMapper::new(r"^>", Box::new(|_| Token::GreaterThan), false),
            TokenMapper::new(r"^=", Box::new(|_| Token::Assignment), false),
            TokenMapper::new(r"^\?", Box::new(|_| Token::QuestionMark), false),
            TokenMapper::new(r"^:", Box::new(|_| Token::Colon), false),
            TokenMapper::new(r"^,", Box::new(|_| Token::Comma), false),
        ];

        Tokenizer { token_mappers }
    }

    pub fn tokenize(&self, input: &str) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();
        let mut input = input;
        while !input.is_empty() {
            if input.starts_with(" ")
                || input.starts_with("\n")
                || input.starts_with("\r")
                || input.starts_with("\t")
            {
                input = &input[1..];
            } else {
                let mut found = false;
                for token_mapper in &self.token_mappers {
                    if let Some((token, rest)) = token_mapper.map(input) {
                        tokens.push(token);
                        input = rest;
                        found = true;
                        break;
                    }
                }
                if !found {
                    println!("Failed to lex {}", input);
                    return Err(CompilerError::Lex.into());
                }
            }
        }
        let tokens = tokens
            .into_iter()
            .filter(|t| !matches!(t, Token::Comment(_) | Token::PreProcessorDirective(_)))
            .collect();
        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_simple_return() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("int main() { return 42; }").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Int,
                Token::Identifier("main".to_string()),
                Token::LParen,
                Token::RParen,
                Token::LBrace,
                Token::Return,
                Token::Constant("42".to_string()),
                Token::SemiColon,
                Token::RBrace
            ]
        );
    }

    #[test]
    fn test_double_negation() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("--42;").unwrap();
        assert_eq!(
            tokens,
            vec![Token::DoubleMinus, Token::Constant("42".to_string()), Token::SemiColon]
        );
    }

    #[test]
    fn test_tilde() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("~42;").unwrap();
        assert_eq!(
            tokens,
            vec![Token::Tilde, Token::Constant("42".to_string()), Token::SemiColon]
        );
    }

    #[test]
    fn test_plus() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("+42;").unwrap();
        assert_eq!(tokens, vec![Token::Plus, Token::Constant("42".to_string()), Token::SemiColon]);
    }

    #[test]
    fn test_minus() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("-42;").unwrap();
        assert_eq!(
            tokens,
            vec![Token::Minus, Token::Constant("42".to_string()), Token::SemiColon]
        );
    }

    #[test]
    fn test_star() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("*42;").unwrap();
        assert_eq!(
            tokens,
            vec![Token::Asterisk, Token::Constant("42".to_string()), Token::SemiColon]
        );
    }
}
