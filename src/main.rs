use std::io::{BufWriter, Write};

fn main() -> std::io::Result<()> {
    //write a file with string contents
    let file = std::fs::File::create("asm.s")?;
    let mut buff = BufWriter::new(file);
    write!(buff, "Hello world")?;
    Ok(())
}
