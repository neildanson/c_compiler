use crate::token::{Token, Tokenizer};
use std::io::*;

fn read_file(filename: &str) -> std::io::Result<String> {
    let file = std::fs::File::open(filename)?;
    let mut buff = std::io::BufReader::new(file);
    let mut contents = String::new();
    buff.read_to_string(&mut contents)?;
    Ok(contents)
}

fn lex_internal(input: &str) -> Result<Vec<Token>> {
    let mut tokens = Vec::new();
    let  tokenizer = Tokenizer::new();
    let mut input = input;
    while !input.is_empty() {
        if input.starts_with(" ") || input.starts_with("\n") || input.starts_with("\r") || input.starts_with("\t") {
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

pub fn lex(input: &str) -> Result<Vec<Token>> {
    let content = read_file(input)?;
    lex_internal(&content)
}
