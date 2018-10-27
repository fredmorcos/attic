#!/bin/sh

echo "=== Building Hallelujah Compiler ==="
cd src

echo "=== Generating Lexical Analyzer ==="
flex --header-file=lexer.h lexer.flex

echo "=== Generating Parser ==="

echo "=== Compiling hllc ==="
cd ..
gcc src/*.c -lfl -o hllc
