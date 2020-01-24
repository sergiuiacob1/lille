.data
    star:
        .ascii "*"
    newline:
        .ascii "\n"
    count:
        .long 4

.text

.globl _start

_start:
    xor %rcx, %rcx
    mov $count, %ecx
    jmp lin

print_rsi:
    mov $1, %rax
    mov $1, %rdi
    mov $1, %rdx
    syscall
    ret

lin:
    dec %rcx
    cmp $0, %rcx
    jl fin
    mov $5, %rdx

col:
    dec %rdx
    cmp $0, %rdx
    jl col_fin

    mov $star, %rsi
    push %rcx
    push %rdx
    call print_rsi
    pop %rdx
    pop %rcx

    jmp col

col_fin:
    mov $newline, %rsi
    push %rcx
    push %rdx
    call print_rsi
    pop %rdx
    pop %rcx

    jmp lin
    
fin:
    movl $0, %ebx
    movl $1, %eax
    int $0x80
