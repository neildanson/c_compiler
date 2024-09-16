#!/bin/bash
cargo test
cargo build

../writing-a-c-compiler-tests/test_compiler ./target/debug/c_compiler --chapter 3 --stage validate
../writing-a-c-compiler-tests/test_compiler ./target/debug/c_compiler --chapter 4 

gcc -E -P main.c -o a.c
cargo run -- --codegen a.c --S

gcc a.s
time ./a.out
echo "My Code : $?"

gcc -o b.out main.c
time ./b.out
echo "GCC Code : $?"