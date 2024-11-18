#![feature(if_let_guard)]
#![feature(let_chains)]
pub mod codegen;
pub mod error;
pub mod lex;
pub mod parse;
pub mod tacky;
pub mod validate;

use crate::lex::*;
use crate::parse::parse_program;
use crate::tacky::Tacky;
use anyhow::Result;
use parse::{Expression, Statement};
use std::io::{Read, Write};
use tacky::TackyResult;
use validate::{semantic_analysis::SemanticAnalysis, ValidateResult};

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

pub fn parse(filename: &str) -> Result<parse::Program<Statement<Expression>, Expression>> {
    let tokens = lex(filename)?;
    let ast = parse_program(&tokens)?;
    Ok(ast)
}

pub fn validate(filename: &str) -> Result<ValidateResult> {
    let program = parse(filename)?;
    let program = SemanticAnalysis::semantic_validation(program)?;
    Ok(program)
}

pub fn tacky(filename: &str) -> Result<TackyResult> {
    let validate_result = validate(filename)?;
    let mut tacky = Tacky::new(validate_result);
    let tacky = tacky.emit_tacky()?;
    Ok(tacky)
}

pub fn codegen(filename: &str) -> Result<codegen::Program> {
    let tacky = tacky(filename)?;
    let mut asm: codegen::Program = tacky.program.try_into()?;
    asm.fixup(&tacky.symbols);
    Ok(asm)
}

pub fn write_asm(filename: &str, asm: &codegen::Program) -> Result<()> {
    let asm = asm.to_string();
    write_file(filename, &asm)?;
    Ok(())
}

pub fn pre_process_c(filename: &str) -> Result<String> {
    let filename = if filename.ends_with(".c") {
        filename
    } else {
        return Err(anyhow::anyhow!("Invalid file extension"));
    };

    let dir = tempfile::tempdir()?;
    let temp_file = format!(
        "temp/{}.c",
        dir.path().file_name().unwrap().to_str().unwrap()
    );

    let output = std::process::Command::new("gcc")
        .args(vec!["-E", "-P", filename, "-o", &temp_file])
        .output()?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(anyhow::anyhow!("gcc failed: {}", stderr));
    }

    Ok(temp_file)
}

pub fn compile(filename: &str, original_filename: &str, c_flag: bool) -> Result<()> {
    let filename = if filename.ends_with(".c") {
        filename
    } else {
        return Err(anyhow::anyhow!("Invalid file extension"));
    };
    let s_file = filename.replace(".c", ".s");
    let out_file = original_filename.replace(".c", "");
    let o_file = original_filename.replace(".c", ".o");

    let asm = codegen(filename)?;
    write_asm(&s_file, &asm)?;

    let args = if c_flag {
        vec!["-c", &s_file, "-o", &o_file]
    } else {
        vec![&s_file, "-o", &out_file]
    };

    let output = std::process::Command::new("gcc").args(args).output()?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(anyhow::anyhow!("gcc failed: {}", stderr));
    }
    Ok(())
}
