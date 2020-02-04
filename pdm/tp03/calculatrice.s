.data
    BUFFER:
        .space 20

    DMAX:
        .long 20

.text

.global _start

_start:
    _read_expression:
        xor %rcx, %rcx
        call _read
        // save how many bytes we read
        mov %rax, %rcx
        // it also contains the newline, so remove that
        dec %rcx
        
        // if I read nothing
        cmp $0, %rcx
        je _read_done

        mov $BUFFER, %r11
        _test_add:
            cmpb $'+', (%r11)
            jne _test_sub
            call _add
            add $16, %rsp
            push %rax
            jmp _read_expression
        _test_sub:
            cmpb $'-', (%r11)
            jne _test_multiplication
            call _sub
            add $16, %rsp
            push %rax
            jmp _read_expression
        _test_multiplication:
            cmpb $'*', (%r11)
            jne _test_division
            call _multiplication
            add $16, %rsp
            push %rax
            jmp _read_expression
        _test_division:
            cmpb $'/', (%r11)
            jne _is_a_number
            call _division
            add $16, %rsp
            push %rax
            jmp _read_expression

        _is_a_number:
            // if I reached this point, then I have a number
            // I form the number in r12
            xor %r12, %r12
            mov $10, %r10
            _build_number:
                movb (%r11), %bl
                mov $48, %r13
                // digit = char - '0'
                sub %r13, %rbx

                imul %r10, %r12
                add %rbx, %r12

                // next digit
                inc %r11
                loop _build_number
        
            // parse is done
            push %r12
            xor %r12, %r12
            jmp _read_expression

    _read_done:
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
    xor %rdx, %rdx
    idiv %rbx, %rax

    movq %rbp, %rsp
    pop %rbp
    ret

_print:
    // print the top of the previous stack
    push %rbp
    movq %rsp, %rbp

    mov 16(%rbp), %rax
    xor %rdx, %rdx
    xor %rcx, %rcx

    mov $10, %r10

    xor %r11, %r11
    _invert_number:
        xor %rdx, %rdx
        idiv %r10, %rax

        imul %r10, %r11
        add %rdx, %r11

        cmp $0, %rax
        jnz _invert_number

    mov %r11, %rax

    mov $BUFFER, %rbx
    xor %rcx, %rcx
    _parse_number:
        // modulo is in %rdx
        xor %rdx, %rdx
        idiv %r10, %rax
        movb %dl, (%rbx)
        addb $'0', (%rbx)
        inc %rbx
        inc %rcx

        cmp $0, %rax
        jnz _parse_number

    movb $'\n', (%rbx)
    inc %rcx

    mov $4, %rax
    mov $1, %rbx
    mov %rcx, %rdx
    mov $BUFFER, %rcx
    int $0x80

    movq %rbp, %rsp
    pop %rbp
    ret

_fin:
    mov $1, %rax
    mov $0, %rbx
    int $0x80
