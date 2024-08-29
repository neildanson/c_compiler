use std::io::{BufWriter, Write};
use clap::{arg, Command};

//gcc asm.s
//./a.out
//echo $?


//Usage : cargo run -- --codegen a.s
fn main() -> std::io::Result<()> {
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


    if let Some (_) = lex {

    }

    if let Some(_) = parse  {
        
    }

    if let Some (filename) = codegen {
        let file = std::fs::File::create(filename)?;
        let mut buff = BufWriter::new(file);
        write!(buff, ".globl main
    main:
        movl $2, %eax
        ret
    ")?;
    }
    Ok(())
}
