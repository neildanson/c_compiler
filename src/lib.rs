#![feature(if_let_guard)]
pub mod codegen;
pub mod error;
pub mod lex;
pub mod parse;
pub mod tacky;

use crate::lex::*;
use crate::parse::parse_program;
use crate::tacky::Tacky;
use anyhow::Result;
use parse::Analysis;
use std::io::{Read, Write};

pub fn read_file(filename: &str) -> Result<String> {
    let file = std::fs::File::open(filename)?;
    let mut buff = std::io::BufReader::new(file);
    let mut contents = String::new();
    buff.read_to_string(&mut contents)?;
    Ok(contents)
}

pub fn write_file(filename: &str, contents: &str) -> Result<()> {
    let file = std::fs::File::create(filename)?;
    let mut buff = std::io::BufWriter::new(file);
    buff.write_all(contents.as_bytes())?;
    buff.flush()?;
    Ok(())
}

pub fn lex(filename: &str) -> Result<Vec<Token>> {
    let tokenizer = Tokenizer::new();
    let input = read_file(filename)?;
    let tokens = tokenizer.tokenize(&input)?;
    Ok(tokens)
}

pub fn parse(filename: &str) -> Result<parse::Program> {
    let tokens = lex(filename)?;
    let ast = parse_program(&tokens)?;
    Ok(ast)
}

pub fn validate(filename: &str) -> Result<parse::Program> {
    let program = parse(filename)?;
    let mut analysis = Analysis::default();
    let program = analysis.semantic_validation(program)?;
    Ok(program)
}

pub fn tacky(filename: &str) -> Result<tacky::Program> {
    let ast = validate(filename)?;
    let mut tacky = Tacky::default();
    let tacky = tacky.emit_tacky(ast)?;
    Ok(tacky)
}

pub fn codegen(filename: &str) -> Result<codegen::Program> {
    let ast = tacky(filename)?;
    let asm = ast.try_into()?;
    Ok(asm)
}

pub fn write_asm(filename: &str, asm: &codegen::Program) -> Result<()> {
    let asm = asm.to_string();
    write_file(filename, &asm)?;
    Ok(())
}
