use crate::token::{Token, Tokenizer};
use std::io::*;

fn read_file(filename: &str) -> std::io::Result<String> {
    let file = std::fs::File::open(filename)?;
    let mut buff = std::io::BufReader::new(file);
    let mut contents = String::new();
    buff.read_to_string(&mut contents)?;
    Ok(contents)
}

pub fn lex(input: &str) -> Result<Vec<Token>> {
    let content = read_file(input)?;
    let tokenizer = Tokenizer::new();
    tokenizer.tokenize(&content)
}
