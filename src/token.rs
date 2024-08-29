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