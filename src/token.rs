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

pub fn identifier(input : &str) -> Option<(Token, usize)> {
    let re = Regex::new(r"^[a-zA-Z_]\w*\b").unwrap();
    if let Some(mat) = re.find(input) {
        let s = mat.as_str();
        Some((Token::Identifier(s.to_string()), s.len()))
    } else {
        None
    }
}