#!/bin/bash
cargo test
cargo build

../writing-a-c-compiler-tests/test_compiler ./target/debug/c_compiler --chapter 4 --stage validate
../writing-a-c-compiler-tests/test_compiler ./target/debug/c_compiler --chapter 5 --stage parse 

gcc -E -P main.c -o a.c
cargo run -- --parse a.c

gcc a.s
time ./a.out
echo "My Code : $?"

gcc -o b.out main.c
time ./b.out
echo "GCC Code : $?"