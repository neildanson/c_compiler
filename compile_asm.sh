#!/bin/bash
#cargo run -- main.c --tacky --verbose
#cargo run -- main.c --codegen --S
gcc a.s
./a.out
echo "My Code : $?"