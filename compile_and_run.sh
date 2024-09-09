#!/bin/bash

cargo run -- --codegen main.c --S
gcc a.s
./a.out
echo $?