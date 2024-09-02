use c_compiler::codegen;
use c_compiler::token::*;
use c_compiler::ast::*;
use clap::{arg, Command};
use std::io::*;


fn read_file(filename: &str) -> std::io::Result<String> {
    let file = std::fs::File::open(filename)?;
    let mut buff = std::io::BufReader::new(file);
    let mut contents = String::new();
    buff.read_to_string(&mut contents)?;
    Ok(contents)
}

fn write_file(filename: &str, contents: &str) -> std::io::Result<()> {
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

fn codegen(filename: &str) -> Result<()> {
    let ast = parse(filename)?;
    let asm : codegen::Program = ast.try_into().unwrap();
    write_file("out.s", &asm.to_string())?;
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
        .get_matches();

    let lex_file = matches.get_one::<String>("lex");
    let parse_file = matches.get_one::<String>("parse");
    let codegen_file = matches.get_one::<String>("codegen");

    if let Some(filename) = lex_file {
        let tokens = lex(filename)?;
        println!("{:#?}", tokens);
    }

    if let Some(filename) = parse_file {
        let ast = parse(filename)?;
        println!("{:#?}", ast);
    }

    if let Some(filename) = codegen_file {
        codegen(filename)?;
        
    } 
    Ok(())
}
