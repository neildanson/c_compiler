use anyhow::Result;
use regex::Regex;

use crate::error::CompilerError;

#[derive(Debug, PartialEq)]
pub enum Token {
    Comment(String),
    PreProcessorDirective(String), //#[a-zA-Z_]\w*\b
    Identifier(String), //[a-zA-Z_]\w*\b
    Constant(String),   //[0-9]+\b
    Int,                //int\b
    Void,               //void\b
    Return,             //return\b
    LParen,             //\(
    RParen,             //\)
    LBrace,             //{
    RBrace,             //}
    SemiColon,          //;
    Tilde,              //~
    DoubleNegation,     //--
    Plus,               //+
    Minus,              //-
    Asterisk,           //*
    Slash,              // /
    Percent,            //%
    ShiftLeft,          //<<
    ShiftRight,         //>>
    BitwiseXor,         //^
    BitwiseOr,          //|
    BitwiseAnd,         //&
}

pub struct Tokenizer {
    multi_line_comment: Regex,
    single_line_comment: Regex,
    preprocessor_directive: Regex,
    constant: Regex,
    identifier: Regex,
    int: Regex,
    void: Regex,
    return_: Regex,
    lparen: Regex,
    rparen: Regex,
    lbrace: Regex,
    rbrace: Regex,
    semicolon: Regex,
    tilde: Regex,
    double_negation: Regex,
    plus: Regex,
    minus: Regex,
    star: Regex,
    slash: Regex,
    percent: Regex,
    shift_left: Regex,
    shift_right: Regex,
    bitwise_xor: Regex,
    bitwise_or: Regex,
    bitwise_and: Regex,
}

impl Default for Tokenizer {
    fn default() -> Self {
        Self::new()
    }
}

impl Tokenizer {
    pub fn new() -> Self {
        Tokenizer {
            multi_line_comment: Regex::new(r"^/[*]([^*]|([*][^/]))*[*]/").unwrap(),
            single_line_comment: Regex::new(r"^//(.*)").unwrap(),
            preprocessor_directive: Regex::new(r"^#(.*)").unwrap(),
            constant: Regex::new(r"^[0-9]+\b").unwrap(),
            identifier: Regex::new(r"^[a-zA-Z_]\w*\b").unwrap(),
            int: Regex::new(r"^int\b").unwrap(),
            void: Regex::new(r"^void\b").unwrap(),
            return_: Regex::new(r"^return\b").unwrap(),
            lparen: Regex::new(r"^\(").unwrap(),
            rparen: Regex::new(r"^\)").unwrap(),
            lbrace: Regex::new(r"^\{").unwrap(),
            rbrace: Regex::new(r"^\}").unwrap(),
            semicolon: Regex::new(r"^;").unwrap(),
            tilde: Regex::new(r"^~").unwrap(),
            double_negation: Regex::new(r"^--").unwrap(),
            plus: Regex::new(r"^\+").unwrap(),
            minus: Regex::new(r"^-").unwrap(),
            star: Regex::new(r"^\*").unwrap(),
            slash: Regex::new(r"^/").unwrap(),
            percent: Regex::new(r"^%").unwrap(),
            shift_left: Regex::new(r"^<<").unwrap(),
            shift_right: Regex::new(r"^>>").unwrap(),
            bitwise_xor: Regex::new(r"^\^").unwrap(),
            bitwise_or: Regex::new(r"^\|").unwrap(),
            bitwise_and: Regex::new(r"^&").unwrap(),
        }
    }

    pub fn tokenize(&self, input: &str) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();
        let tokenizer = Tokenizer::new();
        let mut input = input;
        while !input.is_empty() {
            if input.starts_with(" ")
                || input.starts_with("\n")
                || input.starts_with("\r")
                || input.starts_with("\t")
            {
                input = &input[1..];
            } else if let Some((_token, rest)) = tokenizer.multi_line_comment(input) {
                //tokens.push(token);
                input = rest;
            } else if let Some((_token, rest)) = tokenizer.single_line_comment(input) {
                //tokens.push(token);
                input = rest;
            } else if let Some((_token, rest)) = tokenizer.preprocessor_directive(input) {
                //tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.void(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.return_(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.int(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.lparen(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.rparen(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.lbrace(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.rbrace(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.semicolon(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.identifier(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.constant(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.tilde(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.double_negation(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.plus(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.minus(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.star(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.slash(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.percent(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.shift_left(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.shift_right(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.bitwise_xor(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.bitwise_or(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.bitwise_and(input) {
                tokens.push(token);
                input = rest;
            } else {
                println!("Failed to lex {}", input);
                return Err(CompilerError::Lex.into());
            }
        }
        Ok(tokens)
    }

    fn parse<'a, F>(input: &'a str, re: &Regex, token: F) -> Option<(Token, &'a str)>
    where
        F: Fn(&str) -> Token,
    {
        if let Some(mat) = re.find(input) {
            let s = mat.as_str();
            Some((token(s), &input[s.len()..]))
        } else {
            None
        }
    }

    fn multi_line_comment<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.multi_line_comment, |s| {
            Token::Comment(s.to_string())
        })
    }

    fn single_line_comment<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.single_line_comment, |s| {
            Token::Comment(s.to_string())
        })
    }

    fn preprocessor_directive<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.preprocessor_directive, |s| {
            Token::PreProcessorDirective(s.to_string())
        })
    }

    fn constant<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.constant, |s| Token::Constant(s.to_string()))
    }

    fn identifier<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.identifier, |s| {
            Token::Identifier(s.to_string())
        })
    }

    fn int<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.int, |_| Token::Int)
    }

    fn void<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.void, |_| Token::Void)
    }

    fn return_<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.return_, |_| Token::Return)
    }

    fn lparen<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.lparen, |_| Token::LParen)
    }

    fn rparen<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.rparen, |_| Token::RParen)
    }

    fn lbrace<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.lbrace, |_| Token::LBrace)
    }

    fn rbrace<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.rbrace, |_| Token::RBrace)
    }

    fn semicolon<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.semicolon, |_| Token::SemiColon)
    }

    fn tilde<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.tilde, |_| Token::Tilde)
    }

    fn double_negation<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.double_negation, |_| Token::DoubleNegation)
    }

    fn plus<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.plus, |_| Token::Plus)
    }

    fn minus<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.minus, |_| Token::Minus)
    }

    fn star<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.star, |_| Token::Asterisk)
    }

    fn slash<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.slash, |_| Token::Slash)
    }

    fn percent<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.percent, |_| Token::Percent)
    }

    fn shift_left<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.shift_left, |_| Token::ShiftLeft)
    }

    fn shift_right<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.shift_right, |_| Token::ShiftRight)
    }

    fn bitwise_xor<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.bitwise_xor, |_| Token::BitwiseXor)
    }

    fn bitwise_or<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.bitwise_or, |_| Token::BitwiseOr)
    }

    fn bitwise_and<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.bitwise_and, |_| Token::BitwiseAnd)
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
            vec![Token::DoubleNegation, Token::Constant("42".to_string())]
        );
    }

    #[test]
    fn test_tilde() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("~42").unwrap();
        assert_eq!(tokens, vec![Token::Tilde, Token::Constant("42".to_string())]);
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
        assert_eq!(tokens, vec![Token::Minus, Token::Constant("42".to_string())]);
    }

    #[test]
    fn test_star() {
        let tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize("*42").unwrap();
        assert_eq!(tokens, vec![Token::Asterisk, Token::Constant("42".to_string())]);
    }
}
