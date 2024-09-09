#!/bin/bash

cargo run -- --codegen main.c --S

../writing-a-c-compiler-tests/test_compiler ./target/debug/c_compiler --chapter 3 --stage codegen

gcc a.s
./a.out
echo $?