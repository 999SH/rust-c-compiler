.PHONY: all clean

all: build run compile

build:
	cargo build

run:
	cargo run ./testcode/code.c

compile: testcode/output.s
	gcc -o testcode/executable testcode/output.s

gcc_asm: testcode/code.c
	gcc -S -o testcode/gcc_asm.s testcode/code.c

clean:
	cargo clean
	rm -f testcode/output.s
	rm testcode/executable

gcc_clean: 
	rm testcode/gcc_asm.s
