.global _add
.global _sub
.global _multiplication
.global _division

_add:
    push %rbp
    movq %rsp, %rbp

    mov %rdi, %rax
    mov %rsi, %rbx
    add %rbx, %rax

    movq %rbp, %rsp
    pop %rbp
    ret

_sub:
    push %rbp
    movq %rsp, %rbp

    mov %rdi, %rbx
    mov %rsi, %rax
    sub %rbx, %rax

    movq %rbp, %rsp
    pop %rbp
    ret


_multiplication:
    push %rbp
    movq %rsp, %rbp

    mov %rdi, %rax
    mov %rsi, %rbx
    imul %rbx, %rax

    movq %rbp, %rsp
    pop %rbp
    ret

_division:
    push %rbp
    movq %rsp, %rbp

    mov %rdi, %rbx
    mov %rsi, %rax
    xor %rdx, %rdx
    cmp $0, %rax
    jg _do_division
    sub $1, %rdx

    _do_division:
        idiv %rbx

    movq %rbp, %rsp
    pop %rbp
    ret
