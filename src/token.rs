use regex::Regex;

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

pub struct Tokenizer{
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
    pub fn new() -> Self{
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

    fn parse<'a, F>(input: &'a str, re: &Regex, token: F) -> Option<(Token, &'a str)> where  F: Fn(&str) -> Token {
        if let Some(mat) = re.find(input) {
            let s = mat.as_str();
            Some((token(s), &input[s.len()..]))
        } else {
            None
        }
    }

    pub fn multi_line_comment<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.multi_line_comment, |s| Token::Comment(s.to_string()))
    }

    pub fn single_line_comment<'a>(&self,input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.single_line_comment, |s| Token::Comment(s.to_string()))
    }

    pub fn constant<'a>(&self,input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.constant, |s| Token::Constant(s.to_string()))
    }

    pub fn identifier<'a>(&self,input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.identifier, |s| Token::Identifier(s.to_string()))
    }

    pub fn int<'a>(&self,input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.int, |_| Token::Int)
    }

    pub fn void<'a>(&self,input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.void, |_| Token::Void)
    }

    pub fn return_<'a>(&self,input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.return_, |_| Token::Return)
    }

    pub fn lparen<'a>(&self, input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.lparen, |_| Token::LParen)
    }

    pub fn rparen<'a>(&self,input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.rparen, |_| Token::RParen)
    }

    pub fn lbrace<'a>(&self,input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.lbrace, |_| Token::LBrace)
    }

    pub fn rbrace<'a>(&self,input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.rbrace, |_| Token::RBrace)
    }

    pub fn semicolon<'a>(&self,input: &'a str) -> Option<(Token, &'a str)> {
        Self::parse(input, &self.semicolon, |_| Token::SemiColon)
    }
}
