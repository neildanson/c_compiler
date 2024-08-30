use clap::{arg, Command};
use std::io::{BufWriter, Write, Result};
mod ast;
mod lex;
mod token;

//gcc asm.s
//./a.out
//echo $?

//Usage : cargo run -- --codegen a.s
//Usage : cargo run -- --lex main.c
//Usage : cargo run -- --parse main.c
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

    if let Some(filename) = lex {
        let tokens = lex::lex(filename)?;
        println!("{:#?}", tokens);
    }

    if let Some(filename) = parse {
        let tokens = lex::lex(filename)?;
        let ast = ast::parse(&tokens);
        println!("{:#?}", ast);

    }

    if let Some(filename) = codegen {
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
