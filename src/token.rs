pub enum Token {
    Identifier(String),
    Constant(String),
    Int(i32),
    Void,
    Return,
    LParen,
    RParen,
    LBrace,
    RBrace,
    SemiColon

}