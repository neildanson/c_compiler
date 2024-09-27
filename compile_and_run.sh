#!/bin/bash
cargo test --release
cargo build --release

../writing-a-c-compiler-tests/test_compiler ./target/release/c_compiler --chapter 7 --stage codegen
../writing-a-c-compiler-tests/test_compiler ./target/release/c_compiler --chapter 8 --stage codegen
 

gcc -E -P main.c -o a.c
cargo run --release -- --codegen a.c --S

gcc a.s
time ./a.out
echo "My Code : $?"

gcc -o b.out main.c
time ./b.out
echo "GCC Code : $?"