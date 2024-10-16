use clap::{arg, Command};

use anyhow::Result;
use c_compiler::*;

//gcc asm.s
//./a.out
//echo $?

//../writing-a-c-compiler-tests/test_compiler ./target/debug/c_compiler --chapter 1 --stage parse

//Usage : cargo run -- --codegen a.s
//Usage : cargo run -- --lex main.c
//Usage : cargo run -- --parse main.c
//Goal : cargo run -- main.c --lex --parse --tacky --codegen  --verbose
fn main() -> Result<()> {
    let matches = Command::new("C Compiler")
        .version("0.01")
        .about("Does awesome things")
        .arg(arg!([VALUE]).required(true))
        .arg(arg!(--lex).required(false))
        .arg(arg!(--parse).required(false))
        .arg(arg!(--tacky).required(false))
        .arg(arg!(--codegen).required(false))
        .arg(arg!(--validate).required(false)) //unused
        .arg(arg!(--run).required(false)) //unused
        .arg(arg!(--c).required(false).short('c'))
        //.arg(arg!(--eliminate-unreachable-code).required(false)) //unused
        //.arg(arg!(--eliminate-dead-stores).required(false)) //unused
        //.arg(arg!(--fold-constants).required(false)) //unused
        .arg(arg!(--optimize).required(false)) //unused
        .arg(arg!(--S).required(false).short('S'))
        .arg(arg!(--verbose).required(false))
        .get_matches();

    let filename = matches.get_one::<String>("VALUE").unwrap();
    let lex_file = matches.get_flag("lex");
    let parse_file = matches.get_flag("parse");
    let tacky_file = matches.get_flag("tacky");
    let codegen_file = matches.get_flag("codegen");
    let validate_file = matches.get_flag("validate");
    let s_flag = matches.get_flag("S");
    let verbose_flag = matches.get_flag("verbose");
    let c_flag = matches.get_flag("c");

    let processed_filename = pre_process_c(filename)?;

    if lex_file {
        let tokens = lex(&processed_filename)?;
        if verbose_flag {
            println!("{:#?}", tokens);
        }
    } else if parse_file {
        let ast = parse(&processed_filename)?;
        if verbose_flag {
            println!("{:#?}", ast);
        }
    } else if validate_file {
        let asm = validate(&processed_filename)?;
        if verbose_flag {
            println!("{:#?}", asm);
        }
    } else if tacky_file {
        let ast = tacky(&processed_filename)?;
        if verbose_flag {
            println!("{:#?}", ast);
        }
    } else if codegen_file {
        let asm = codegen(&processed_filename)?;
        if verbose_flag {
            println!("{:#?}", asm);
        }

        if s_flag {
            write_asm("a.s", &asm)?;
        }
    } else {
        compile(&processed_filename, filename, c_flag)?;
    }

    Ok(())
}
