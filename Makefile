.PHONY: all clean

all: build run compile link

build:
	cargo build

run:
	cargo run ./testcode/code.c

compile:
	nasm -f elf64 -g -F dwarf ./testcode/output.asm -o ./testcode/out.o

link:
	ld  ./testcode/out.o -o ./testcode/executable

gcc_asm: testcode/code.c
	gcc -S -o testcode/gcc_asm.s testcode/code.c

clean:
	cargo clean
	rm -f ./testcode/output.asm
	rm ./testcode/executable
	rm ./testcode/out.o

gcc_clean: 
	rm testcode/gcc_asm.s

full:
	make clean
	make all