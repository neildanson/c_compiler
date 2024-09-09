# A Rust implementation of Writing a C Compiler by Nora Sandler

## Notes

Works on MacOS/Linux

Test Suite here 

`git clone https://github.com/nlsandler/writing-a-c-compiler-tests.git`

## Running Test Suite

`../writing-a-c-compiler-tests/test_compiler --check-setup`

`../writing-a-c-compiler-tests/test_compiler ./target/debug/c_compiler --chapter 1 --stage lex`

## Example commands (WIP)

Run Lexer (turn text into Tokens)

`cargo run -- --lex main.c`

Run Parser (turn text into Tokens, parse into AST)

`cargo run -- --parse main.c`

Generate Code

`cargo run -- --codegen main.c --S`

Verbose 

`cargo run -- --lex main.c --parse main.c --tacky main.c --codegen main.c --verbose --S`

## Other commands

Compile ASM to Executable code using gcc

```
gcc asm.s
./a.out
echo $?
```

A Convenience script called `compile_and_run.sh` will compile `a.s` into `a.out` and run and echo the result. 

`chmod u+x compile_and_run.sh`
