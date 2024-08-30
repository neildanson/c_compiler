use regex::Regex;
use std::io::*;

#[derive(Debug)]
pub enum Token {
    Comment(String),
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
}

pub struct Tokenizer {
    multi_line_comment: Regex,
    single_line_comment: Regex,
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
}

impl Tokenizer {
    pub fn new() -> Self {
        Tokenizer {
            multi_line_comment: Regex::new(r"^/[*]([^*]|([*][^/]))*[*]/").unwrap(),
            single_line_comment: Regex::new(r"^//.*").unwrap(),
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
            } else if let Some((token, rest)) = tokenizer.multi_line_comment(input) {
                tokens.push(token);
                input = rest;
            } else if let Some((token, rest)) = tokenizer.single_line_comment(input) {
                tokens.push(token);
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
            } else {
                println!("Failed to lex {}", input);
                return Err(Error::new(ErrorKind::InvalidInput, "Failed to lex"));
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
}
