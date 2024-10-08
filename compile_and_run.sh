#!/bin/bash
cargo test --release
cargo build --release

../writing-a-c-compiler-tests/test_compiler ./target/release/c_compiler --chapter 9 --stage codegen
 

gcc -E -P main.c -o a.c
cargo run --release -- --codegen a.c --S

echo "Running the code"


gcc -o b.out main.c
time ./b.out
echo "GCC Code : $?"

gcc a.s
time ./a.out
echo "My Code : $?"

