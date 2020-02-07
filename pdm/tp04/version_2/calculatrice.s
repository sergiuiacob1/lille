.data
    BUFFER:
        .space 20
    INVERSED:
        .space 20
    DMAX:
        .long 20

.text

.global main

main:
    push $3214
    call _print
    jmp _fin

_read:
    push %rbp
    movq %rsp, %rbp

    mov $3, %eax
    mov $0, %ebx
    mov $BUFFER, %ecx
    mov $DMAX, %edx
    int $0x80
    
    movq %rbp, %rsp
    pop %rbp
    ret

_print:
    // print the top of the previous stack
    push %rbp
    movq %rsp, %rbp

    mov 16(%rbp), %rax
    xor %rdx, %rdx

    // remember the sign
    mov $1, %r14
    cmp $0, %rax
    jg _is_positive
    mov $-1, %r14
    imul $-1, %rax 

    _is_positive:
    // get the number's digits
    mov $INVERSED, %rbx
    xor %rcx, %rcx
    mov $10, %r10
    _reverse_number:
        xor %rdx, %rdx
        idiv %r10, %rax
        movb %dl, (%rbx)
        inc %rcx
        inc %rbx

        cmp $0, %rax
        jnz _reverse_number

    // put the digits in the BUFFER, but reverse them

    // total number of characters to be written:
    mov %rcx, %r15
    mov $BUFFER, %rbx

    // check the sign
    cmp $-1, %r14
    jne _do_not_adjust_sign
    movb $'-', (%rbx)
    inc %rbx
    inc %r15

    _do_not_adjust_sign:
    mov $INVERSED, %rax
    
    // for rbx, go to the end
    add %rcx, %rbx
    // put the newline at the end
    movb $'\n', (%rbx)
    dec %rbx

    _build_correct_number:
        mov (%rax), %r13
        movb %r13b, (%rbx)
        addb $48, (%rbx)

        inc %rax
        dec %rbx

        loop _build_correct_number

    // one more character (the newline)
    inc %r15
    mov $4, %rax
    mov $1, %rbx
    mov $BUFFER, %rcx
    mov %r15, %rdx
    int $0x80

    movq %rbp, %rsp
    pop %rbp
    ret

_fin:
    mov $1, %rax
    mov $0, %rbx
    int $0x80
