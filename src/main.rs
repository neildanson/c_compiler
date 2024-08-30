use clap::{arg, Command};
use token::Tokenizer;
use std::io::*;
mod ast;
mod token;

//gcc asm.s
//./a.out
//echo $?

//Usage : cargo run -- --codegen a.s
//Usage : cargo run -- --lex main.c
//Usage : cargo run -- --parse main.c
fn read_file(filename: &str) -> std::io::Result<String> {
    let file = std::fs::File::open(filename)?;
    let mut buff = std::io::BufReader::new(file);
    let mut contents = String::new();
    buff.read_to_string(&mut contents)?;
    Ok(contents)
}

fn main() -> Result<()> {
    //let file = std::fs::File::create("contents.txt")?;
    //let mut buff = BufWriter::new(file);
    //write!(buff, "{}", "filename")?;
    let matches = Command::new("C Compiler")
        .version("0.01")
        .about("Does awesome things")
        .arg(arg!(--lex <VALUE>).required(false))
        .arg(arg!(--parse <VALUE>).required(false))
        .arg(arg!(--codegen <VALUE>).required(false))
        .get_matches();

    let lex = matches.get_one::<String>("lex");
    let parse = matches.get_one::<String>("parse");
    let codegen = matches.get_one::<String>("codegen");

    let tokenizer = Tokenizer::new();
    
    if let Some(filename) = lex {
        let input = read_file(filename)?;
        let tokens = tokenizer.tokenize(&input)?;
        println!("{:#?}", tokens);
    }

    if let Some(filename) = parse {
        let input = read_file(filename)?;
        let tokens = tokenizer.tokenize(&input)?;
        //let ast = ast::parse(&tokens);
        //println!("{:#?}", ast);
    }

    if let Some(filename) = codegen {
        let input = read_file(filename)?;
        let tokens = tokenizer.tokenize(&input)?;
        //let _ast = ast::parse(&tokens); //Turn Ast to Asm

        let file = std::fs::File::create(filename)?;
        let mut buff = BufWriter::new(file);
        write!(
            buff,
            ".globl main
    main:
        movl $2, %eax
        ret
    "
        )?;
    }
    Ok(())
}
