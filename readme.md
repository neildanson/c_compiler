# A Rust implementation of Writing a C Compiler by Nora Sandler

## Notes

Works on MacOS/Linux

Test Suite here into folder _next to_ this repo. 

`git clone https://github.com/nlsandler/writing-a-c-compiler-tests.git`

## Running Test Suite

`../writing-a-c-compiler-tests/test_compiler --check-setup`

`../writing-a-c-compiler-tests/test_compiler ./target/debug/c_compiler --chapter 1 --stage lex`

## Example commands (WIP)

Run Lexer (turn text into Tokens)

`cargo run -- main.c --lex `

Run Parser (turn text into Tokens, parse into AST)

`cargo run -- main.c --parse`

Generate Code

`cargo run -- main.c --codegen --S`

Verbose 

`cargo run -- main.c --codegen --verbose --S`

## Other commands

Compile ASM to Executable code using gcc

```
gcc asm.s
./a.out
echo $?
```

A Convenience script called `compile_and_run.sh` will run the test suite, compile `a.s` into `a.out` and run and echo the result as well as run gcc on the c file to compare the output. 

`chmod u+x compile_and_run.sh`

A Convenience script called `compile_asm.sh` will compile _just_ the `a.s` file, so if you need to make manual adjustments you can quickly test without a fill build pass. 

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


# MacOS notes

* arch -x86_64 zsh ✅
* Labels shouldn't have . prefix ✅
* Method names Begin with _ ✅
* External methods should not be marked with _ ✅

# Helpful notes

Copilot is pretty good at explaining why asm code segfaults. Use it. 

[godbolt](https://godbolt.org/) is a super useful resource. Dont forget to turn off Intel asm syntax.


# TODOs

100% Implemented up to chapter 9 (excluding extras)

Chapter 10 notes

* Lexer complete
* Parser complete
  - Implement code p226  (parse_type_and_storage_specifier) and figure out where to integrate. 
* Codegen is done
* Tacky from parse is wonky
  * I dont like passing back a seperate symbol table - feels like I can just extract from the parse ast during tacky generation
  * Plan!
    * Generate a Tasky AST
    * Visit the tree, populating statics and rewriting the tree. 
    * All the information about global and linkage is part of parse ast? Might need to consider.
    * Dont have a tryfrom tacky:Function -> cogegen:Function. It's too constrictive. 
    * I dont have a plan for resolving symbols that are static in `rewrite_pseudo_with_stack`. 