.data
    BUFFER:
        .space 20

    DMAX:
        .long 10

.text

.global _start

_start:
    xor %rcx, %rcx
    call _read
    // save how many bytes we read
    mov %rax, %rcx
    // it also contains the newline, so remove that
    dec %rcx
    
    // if I read nothing
    cmp $0, %rcx
    je _fin

    mov $BUFFER, %r10
    _parse:
        // check the current value
        _test_add:
            cmpb $'+', (%r10)
            jne _test_sub
            call _add
            add $16, %rsp
            push %rax
        _test_sub:
            cmpb $'-', (%r10)
            jne _test_multiplication
            call _sub
            add $16, %rsp
            push %rax
        _test_multiplication:
            cmpb $'*', (%r10)
            jne _test_division
            call _multiplication
            add $16, %rsp
            push %rax
        _test_division:
            cmpb $'/', (%r10)
            jne _test_digit
            call _division
            add $16, %rsp
            push %rax

        _test_digit:
            cmpb $'0', (%r10)
            jng _continue_parse
            cmpb $'9', (%r10)
            jg _continue_parse
            // if I passed these checks, I have a digit here, so push it on the stack

            xor %rbx, %rbx
            movb (%r10), %bl
            mov $48, %r12
            sub %r12, %rbx

            // push this value to the stack
            push %rbx

    _continue_parse:
        inc %r10
        loop _parse

    call _print
    jmp _fin

_add:
    push %rbp
    movq %rsp, %rbp

    mov 16(%rbp), %rax
    mov 24(%rbp), %rbx
    add %rbx, %rax

    movq %rbp, %rsp
    pop %rbp
    ret

_sub:
    push %rbp
    movq %rsp, %rbp

    mov 16(%rbp), %rbx
    mov 24(%rbp), %rax
    sub %rbx, %rax

    movq %rbp, %rsp
    pop %rbp
    ret


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

_multiplication:
    push %rbp
    movq %rsp, %rbp

    mov 16(%rbp), %rax
    mov 24(%rbp), %rbx
    imul %rbx, %rax

    movq %rbp, %rsp
    pop %rbp
    ret

_division:
    push %rbp
    movq %rsp, %rbp

    mov 16(%rbp), %rbx
    mov 24(%rbp), %rax
    idiv %rbx, %rax

    movq %rbp, %rsp
    pop %rbp
    ret

_print:
    // print the top of the stack
    push %rbp
    movq %rsp, %rbp

    mov 16(%rbp), %r10

    mov $4, %eax
    mov $1, %ebx
    mov $BUFFER, %ecx
    movb $'8', %cl
    mov $1, %edx
    int $0x80

    movq %rbp, %rsp
    pop %rbp
    ret

_fin:
    mov $1, %rax
    mov $0, %rbx
    int $0x80
