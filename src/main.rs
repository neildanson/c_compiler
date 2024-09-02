use std::io::{Read, Write};

use anyhow::Result;
use clap::{arg, Command};

use c_compiler::parse::parse_program;
use c_compiler::parse::Program;
use c_compiler::codegen::AsmProgram;
use c_compiler::lex::*;

fn read_file(filename: &str) -> Result<String> {
    let file = std::fs::File::open(filename)?;
    let mut buff = std::io::BufReader::new(file);
    let mut contents = String::new();
    buff.read_to_string(&mut contents)?;
    Ok(contents)
}

fn write_file(filename: &str, contents: &str) -> Result<()> {
    let file = std::fs::File::create(filename)?;
    let mut buff = std::io::BufWriter::new(file);
    buff.write_all(contents.as_bytes())?;
    Ok(())
}

fn lex(filename: &str) -> Result<Vec<Token>> {
    let tokenizer = Tokenizer::new();
    let input = read_file(filename)?;
    let tokens = tokenizer.tokenize(&input)?;
    Ok(tokens)
}

fn parse(filename: &str) -> Result<Program> {
    let tokens = lex(filename)?;
    let ast = parse_program(&tokens)?;
    Ok(ast)
}

fn codegen(filename: &str) -> Result<AsmProgram> {
    let ast = parse(filename)?;
    let asm = ast.try_into()?;
    Ok(asm)
}

fn write_asm(filename: &str, asm: AsmProgram) -> Result<()> {
    let asm = asm.to_string();
    write_file(filename, &asm)?;
    Ok(())
}

//gcc asm.s
//./a.out
//echo $?

//../writing-a-c-compiler-tests/test_compiler ./target/debug/c_compiler --chapter 1 --stage parse

//Usage : cargo run -- --codegen a.s
//Usage : cargo run -- --lex main.c
//Usage : cargo run -- --parse main.c
fn main() -> Result<()> {
    let matches = Command::new("C Compiler")
        .version("0.01")
        .about("Does awesome things")
        .arg(arg!(--lex <VALUE>).required(false))
        .arg(arg!(--parse <VALUE>).required(false))
        .arg(arg!(--codegen <VALUE>).required(false))
        .arg(arg!(--S).required(false))
        .get_matches();

    let lex_file = matches.get_one::<String>("lex");
    let parse_file = matches.get_one::<String>("parse");
    let codegen_file = matches.get_one::<String>("codegen");
    let s_flag = matches.get_flag("S");

    if let Some(filename) = lex_file {
        let tokens = lex(filename)?;
        println!("{:#?}", tokens);
    }

    if let Some(filename) = parse_file {
        let ast = parse(filename)?;
        println!("{:#?}", ast);
    }

    if let Some(filename) = codegen_file {
        let asm = codegen(filename)?;
        if s_flag {
            write_asm("a.s", asm)?;
        }
    }

    Ok(())
}
