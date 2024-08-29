use std::io::{BufWriter, Write};

//gcc asm.s
//./a.out
//echo $?

fn main() -> std::io::Result<()> {
    let file = std::fs::File::create("asm.s")?;
    let mut buff = BufWriter::new(file);
    write!(buff, ".globl main
main:
    movl $2, %eax
    ret
")?;
    Ok(())
}
