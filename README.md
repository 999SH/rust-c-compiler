# rust-c-compiler

My C compiler, built in rust. Compiles to x84-64 Assembly.

Currently supports: Most unary and binary operations, Function calls, referencing/dereferencing. 

Compiler runs the C code found in code.c when running the command: make all 

Manual compilation can be done with 
cargo build
cargo run ./testcode/code.c 
gcc -o testcode/executable testcode/output.s
