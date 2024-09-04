use std::io::{Read, Write};

use anyhow::Result;
use clap::{arg, Command};

use c_compiler::*;
use c_compiler::lex::*;
use c_compiler::parse::parse_program;
use c_compiler::tacky::Tacky;

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

fn parse(filename: &str) -> Result<parse::Program> {
    let tokens = lex(filename)?;
    let ast = parse_program(&tokens)?;
    Ok(ast)
}

fn tacky(filename: &str) -> Result<tacky::Program> {
    let ast = parse(filename)?;
    let mut tacky = Tacky::new();
    let tacky = tacky.emit_tacky(ast);
    Ok(tacky)
}

fn codegen(filename: &str) -> Result<codegen::Program> {
    let ast = tacky(filename)?;
    let asm = ast.into();
    Ok(asm)
}

fn write_asm(filename: &str, asm: &codegen::Program) -> Result<()> {
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
//Usage : cargo run -- --lex main.c --parse main.c --tacky main.c --codegen main.c --verbose
fn main() -> Result<()> {
    let matches = Command::new("C Compiler")
        .version("0.01")
        .about("Does awesome things")
        .arg(arg!(--lex <VALUE>).required(false))
        .arg(arg!(--parse <VALUE>).required(false))
        .arg(arg!(--tacky <VALUE>).required(false))
        .arg(arg!(--codegen <VALUE>).required(false))
        .arg(arg!(--validate <VALUE>).required(false)) //unused
        .arg(arg!(--run <VALUE>).required(false)) //unused
        .arg(arg!(--S).required(false))
        .arg(arg!(--verbose).required(false))
        .get_matches();

    let lex_file = matches.get_one::<String>("lex");
    let parse_file = matches.get_one::<String>("parse");
    let tacky_file = matches.get_one::<String>("tacky");
    let codegen_file = matches.get_one::<String>("codegen");
    let s_flag = matches.get_flag("S");
    let verbose_flag = matches.get_flag("verbose");

    if let Some(filename) = lex_file {
        let tokens = lex(filename)?;
        if verbose_flag {
            println!("{:#?}", tokens);
        }
    }

    if let Some(filename) = parse_file {
        let ast = parse(filename)?;
        if verbose_flag {
            println!("{:#?}", ast);
        }
    }

    if let Some(filename) = tacky_file {
        let ast = tacky(filename)?;
        if verbose_flag {
            println!("{:#?}", ast);
        }
    }

    if let Some(filename) = codegen_file {
        let asm = codegen(filename)?;
        if s_flag {
            write_asm("a.s", &asm)?;
        }
        if verbose_flag {
            println!("{:#?}", asm);
        }
    }

    Ok(())
}
