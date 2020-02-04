.data
    BUFFER:
        .space 20
    INVERSED:
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

        // if I read at least 2 characters, I surely have a number
        cmp $1, %rcx
        jg _is_a_number

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

            // if the first character is '-' or '+', adjust the sign
            // and skip that character

            // use this for the sign
            mov $1, %r14

            _test_first_sign:
                cmpb $'+', (%r11)
                jne _test_minus
                // I have a '+' at the beginning, skip
                inc %r11
                dec %rcx
                jmp _build_number

            _test_minus:
                cmpb $'-', (%r11)
                jne _build_number
                // I have a '-' at the beginning, so make r12 negative
                mov $-1, %r14
                inc %r11
                dec %rcx
                jmp _build_number

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
            // multiply with the sign
            mov %r12, %rax
            imul %r14, %rax
            push %rax
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
