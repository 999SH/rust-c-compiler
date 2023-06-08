.intel_syntax noprefix
.global _start
_start:
call adder_entry
mov eax, 0x60
xor edi, edi
syscall
adder_entry:
push rbp
mov rbp, rsp
mov [rbp-8], rdi
mov [rbp-16], rsi
mov rax, [-24]
push rax
mov rax, 5
pop rcx
add rax, rcx
mov rsp, rbp
pop rbp
ret
main_entry:
push rbp
mov rbp, rsp
mov rax, 0
mov rsp, rbp
pop rbp
ret
