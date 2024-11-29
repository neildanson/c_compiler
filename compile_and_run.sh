#!/bin/bash
cargo test --release
cargo build --release


../writing-a-c-compiler-tests/test_compiler ./target/release/c_compiler --chapter 11
../writing-a-c-compiler-tests/test_compiler ./target/release/c_compiler --chapter 12 --stage validate

rm -rf temp
mkdir temp

cargo run --release -- --codegen main.c --S

echo "Running the code"


gcc -o b.out main.c
time ./b.out
echo "GCC Code : $?"

gcc a.s
time ./a.out
echo "My Code : $?"

