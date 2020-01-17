.data
    star:
        .ascii "*"
    newline:
        .ascii "\n"

.text

.globl _start

_start:
    mov $5, %rcx
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
    cmp %rcx, %rdx
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
    
