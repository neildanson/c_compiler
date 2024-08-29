use regex::Regex;

#[derive(Debug)]
pub enum Token {
    Identifier(String), //[a-zA-Z_]\w*\b
    Constant(String), //[0-9]+\b
    Int(i32), //int\b
    Void, //void\b
    Return, //return\b
    LParen, //\(
    RParen, //\)
    LBrace, //{
    RBrace, //}
    SemiColon //;
}

impl Token {
    pub fn constant(input : &str) -> Option<(Token, &str)> {
        let re = Regex::new(r"\d+\b").unwrap();
        if let Some(mat) = re.find(input) {
            let s = mat.as_str();
            Some((Token::Constant(s.to_string()), &input[s.len()..]))
        } else {
            None
        }
    }

    pub fn identifier(input : &str) -> Option<(Token, &str)> {
        let re = Regex::new(r"[a-zA-Z_]\w*\b").unwrap();
        if let Some(mat) = re.find(input) {
            let s = mat.as_str();
            Some((Token::Identifier(s.to_string()), &input[s.len()..]))
        } else {
            None
        }
    }

    pub fn int(input : &str) -> Option<(Token, &str)> {
        let re = Regex::new(r"\A[0-9]+\b").unwrap();
        if let Some(mat) = re.find(input) {
            let s = mat.as_str();
            let i = s.parse::<i32>().expect("Failed to parse int - this is probably a bug");
            Some((Token::Int(i), &input[s.len()..]))
        } else {
            None
        }
    }
}