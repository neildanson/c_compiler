# A Rust implementation of Writing a C Compiler by Nora Sandler

## Notes

Works on MacOS/Linux

Test Suite here 

`git clone https://github.com/nlsandler/writing-a-c-compiler-tests.git`

## Example commands (WIP)

Run Lexer (turn text into Tokens)

`cargo run -- --lex main.c`

Run Parser (turn text into Tokens, parse into AST)

`cargo run -- --parse main.c`

Generate Code

`cargo run -- --codegen a.s`

## Other commands

Compile ASM to Executable code using gcc

```
gcc asm.s
./a.out
echo $?```