.data
UnNom:
    .long 43
    .long 54
    .long 23
    .long 32
    .long 76
    .string "hello world"
    .float 3.14
UnAutre:
    .space 4

.text

.globl _start

_start:
    xor %rbx, %rbx
    movq $5, %rax
    movq $0, %rbx
    movq $UnNom, %rcx

top:
    addq (%rcx), %rbx
    addq $4, %rcx
    decq %rax
    jnz top

done:
    movq $0, %rbx
    movq $1, %rax
    int $0x80
