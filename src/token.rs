use regex::Regex;

#[derive(Debug)]
pub enum Token {
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

impl Token {
    pub fn constant(input: &str) -> Option<(Token, &str)> {
        let re = Regex::new(r"\d+\b").unwrap();
        if let Some(mat) = re.find(input) {
            let s = mat.as_str();
            Some((Token::Constant(s.to_string()), &input[s.len()..]))
        } else {
            None
        }
    }

    pub fn identifier(input: &str) -> Option<(Token, &str)> {
        let re = Regex::new(r"^[a-zA-Z_]\w*\b").unwrap();
        if let Some(mat) = re.find(input) {
            let s = mat.as_str();
            Some((Token::Identifier(s.to_string()), &input[s.len()..]))
        } else {
            None
        }
    }

    pub fn int(input: &str) -> Option<(Token, &str)> {
        let re = Regex::new(r"^int\b").unwrap();
        if let Some(mat) = re.find(input) {
            let s = mat.as_str();
            Some((Token::Int, &input[s.len()..]))
        } else {
            None
        }
    }

    pub fn void(input: &str) -> Option<(Token, &str)> {
        let re = Regex::new(r"^void\b").unwrap();
        if let Some(mat) = re.find(input) {
            let s = mat.as_str();
            Some((Token::Void, &input[s.len()..]))
        } else {
            None
        }
    }

    pub fn return_(input: &str) -> Option<(Token, &str)> {
        let re = Regex::new(r"^return\b").unwrap();
        if let Some(mat) = re.find(input) {
            let s = mat.as_str();
            Some((Token::Return, &input[s.len()..]))
        } else {
            None
        }
    }

    fn parse_single_char<'a>(input: &'a str, c: &str, token: Token) -> Option<(Token, &'a str)> {
        let re = Regex::new(&format!(r"^{}", c)).unwrap();
        if re.find(input).is_some() {
            Some((token, &input[1..]))
        } else {
            None
        }
    }

    pub fn lparen(input: &str) -> Option<(Token, &str)> {
        Self::parse_single_char(input, r"\(", Token::LParen)
    }

    pub fn rparen(input: &str) -> Option<(Token, &str)> {
        Self::parse_single_char(input, r"\)", Token::RParen)
    }

    pub fn lbrace(input: &str) -> Option<(Token, &str)> {
        Self::parse_single_char(input, r"\{", Token::LBrace)
    }

    pub fn rbrace(input: &str) -> Option<(Token, &str)> {
        Self::parse_single_char(input, r"\}", Token::RBrace)
    }

    pub fn semicolon(input: &str) -> Option<(Token, &str)> {
        Self::parse_single_char(input, r";", Token::SemiColon)
    }
}
