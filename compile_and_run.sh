#!/bin/bash
cargo test
cargo run -- --codegen main.c --S
cargo build

../writing-a-c-compiler-tests/test_compiler ./target/debug/c_compiler --chapter 3 --stage codegen --bitwise

gcc a.s
./a.out
echo "My Code : $?"

gcc -o b.out main.c
./b.out
echo "GCC Code : $?"