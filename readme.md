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

Assembly reference

### mov

`mov src, dst`

Limits

Cannot move from rbp (local variable) to rbp must move 1 rbp to register 1st. In our case we use `scratch` register r10.


# add, sub, imul

`add src, dst`

eg 

`add 2, %eax`

will add 2 to eax and store in place. 

src can be a an immediate value, register or memory address. 
dst can be a register or memory address. 
src & dst cannot _both_ be memory addresses. 

# idiv & cdq

idiv computes both (integer) division and remainder

`9 / 2` & `9 % 2` both emit same assembler, but read result from different place

eg

```
movl $2, -4(%rdp)   # move 2 into local
movl $9, %eax       # mov 9 into eax
cdq                 # sign extend eax into edx
idiv -4(%rdp)       # eax div by local. result in 
                    # divide result in eax
                    # mod result in edx
```


# MacOS

TODO (Done?)

* arch -x86_64 zsh ✅
* Labels shouldn't have . prefix ✅
* Method names Begin with _ ✅
* External methods should not be marked with _ ✅
* Some weirdness with @PLT

accept -c as parameter and pass into gcc as 

gcc -c ASSEMBLY_FILE -o OUTPUT_FILE 

where ASSEMBLY_FILE == OUTPUT_FILE with .o suffix