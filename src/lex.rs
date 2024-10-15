use anyhow::Result;
use regex::Regex;

use crate::error::CompilerError;

#[derive(Debug, PartialEq)]
pub enum Token {
    Comment(String),
    PreProcessorDirective(String), //#[a-zA-Z_]\w*\b
    Identifier(String),            //[a-zA-Z_]\w*\b
    Constant(String),              //[0-9]+\b
    Int,                           //int\b
    Void,                          //void\b
    Return,                        //return\b
    Static,                        //static\b
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
}

impl TokenMapper {
    fn new(regex: &str, token: Box<dyn Fn(String) -> Token>) -> Self {
        TokenMapper {
            regex: Regex::new(regex).unwrap(),
            token,
        }
    }

    fn map<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        if let Some(mat) = self.regex.find(input) {
            let s = mat.as_str();
            Some(((self.token)(s.to_string()), &input[s.len()..]))
        } else {
            None
        }
    }
}

impl Tokenizer {
    pub fn new() -> Self {
        let token_mappers = vec![
            TokenMapper::new(r"^/[*]([^*]|([*][^/]))*[*]/", Box::new(Token::Comment)),
            TokenMapper::new(r"^//(.*)", Box::new(Token::Comment)),
            TokenMapper::new(r"^#(.*)", Box::new(Token::PreProcessorDirective)),
            TokenMapper::new(r"^void\b", Box::new(|_| Token::Void)),
            TokenMapper::new(r"^return\b", Box::new(|_| Token::Return)),
            TokenMapper::new(r"^static\b", Box::new(|_| Token::Static)),
            TokenMapper::new(r"^int\b", Box::new(|_| Token::Int)),
            TokenMapper::new(r"^if\b", Box::new(|_| Token::If)),
            TokenMapper::new(r"^else\b", Box::new(|_| Token::Else)),
            TokenMapper::new(r"^do\b", Box::new(|_| Token::Do)),
            TokenMapper::new(r"^while\b", Box::new(|_| Token::While)),
            TokenMapper::new(r"^for\b", Box::new(|_| Token::For)),
            TokenMapper::new(r"^break\b", Box::new(|_| Token::Break)),
            TokenMapper::new(r"^continue\b", Box::new(|_| Token::Continue)),
            TokenMapper::new(r"^\(", Box::new(|_| Token::LParen)),
            TokenMapper::new(r"^\)", Box::new(|_| Token::RParen)),
            TokenMapper::new(r"^\{", Box::new(|_| Token::LBrace)),
            TokenMapper::new(r"^\}", Box::new(|_| Token::RBrace)),
            TokenMapper::new(r"^;", Box::new(|_| Token::SemiColon)),
            TokenMapper::new(r"^[a-zA-Z_]\w*\b", Box::new(Token::Identifier)),
            TokenMapper::new(r"^[0-9]+\b", Box::new(Token::Constant)),
            TokenMapper::new(r"^~", Box::new(|_| Token::Tilde)),
            TokenMapper::new(r"^--", Box::new(|_| Token::DoubleMinus)),
            TokenMapper::new(r"^\+\+", Box::new(|_| Token::DoublePlus)),
            TokenMapper::new(r"^<=", Box::new(|_| Token::LessThanOrEqual)),
            TokenMapper::new(r"^>=", Box::new(|_| Token::GreaterThanOrEqual)),
            TokenMapper::new(r"^<<", Box::new(|_| Token::ShiftLeft)),
            TokenMapper::new(r"^>>", Box::new(|_| Token::ShiftRight)),
            TokenMapper::new(r"^==", Box::new(|_| Token::Equal)),
            TokenMapper::new(r"^&&", Box::new(|_| Token::And)),
            TokenMapper::new(r"^!=", Box::new(|_| Token::NotEqual)),
            TokenMapper::new(r"^\|\|", Box::new(|_| Token::Or)),
            TokenMapper::new(r"^\+", Box::new(|_| Token::Plus)),
            TokenMapper::new(r"^-", Box::new(|_| Token::Minus)),
            TokenMapper::new(r"^\*", Box::new(|_| Token::Asterisk)),
            TokenMapper::new(r"^/", Box::new(|_| Token::Slash)),
            TokenMapper::new(r"^%", Box::new(|_| Token::Percent)),
            TokenMapper::new(r"^\^", Box::new(|_| Token::BitwiseXor)),
            TokenMapper::new(r"^\|", Box::new(|_| Token::BitwiseOr)),
            TokenMapper::new(r"^&", Box::new(|_| Token::BitwiseAnd)),
            TokenMapper::new(r"^!", Box::new(|_| Token::Not)),
            TokenMapper::new(r"^<", Box::new(|_| Token::LessThan)),
            TokenMapper::new(r"^>", Box::new(|_| Token::GreaterThan)),
            TokenMapper::new(r"^=", Box::new(|_| Token::Assignment)),
            TokenMapper::new(r"^\?", Box::new(|_| Token::QuestionMark)),
            TokenMapper::new(r"^:", Box::new(|_| Token::Colon)),
            TokenMapper::new(r"^,", Box::new(|_| Token::Comma)),
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
        let tokens = tokenizer.tokenize("--42").unwrap();
        assert_eq!(
            tokens,
            vec![Token::DoubleMinus, Token::Constant("42".to_string())]
        );
    }

    #[test]
    fn test_tilde() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("~42").unwrap();
        assert_eq!(
            tokens,
            vec![Token::Tilde, Token::Constant("42".to_string())]
        );
    }

    #[test]
    fn test_plus() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("+42").unwrap();
        assert_eq!(tokens, vec![Token::Plus, Token::Constant("42".to_string())]);
    }

    #[test]
    fn test_minus() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("-42").unwrap();
        assert_eq!(
            tokens,
            vec![Token::Minus, Token::Constant("42".to_string())]
        );
    }

    #[test]
    fn test_star() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("*42").unwrap();
        assert_eq!(
            tokens,
            vec![Token::Asterisk, Token::Constant("42".to_string())]
        );
    }
}
