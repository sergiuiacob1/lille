.data
    BUFFER:
        .space 256
    INVERSED:
        .space 256
    DMAX:
        .long 255

.text

.global main

main:
    // read the whole expression in BUFFER
    call _read
    // check how much I read
    mov %rax, %rcx
    // decrease 1 (newline)
    sub $1, %rcx
    cmp $0, %rcx
    // if I read nothing, finish program
    jng _fin

    // parse expression
    // build numbers in r11
    // r12 for the sign
    xor %r11, %r11
    mov $1, %r12
    mov $10, %r10
    mov $BUFFER, %rbx

    _parse:
        // if I have a space and the character before was a digit
        // then push number to stack
        // otherwise, continue parse

        cmpb $' ', (%rbx)
        jne _not_space

            // if the character before was a digit, push number to stack
            dec %rbx
            xor %rax, %rax
            movb (%rbx), %al
            push %rax
            inc %rbx
            call _test_if_digit
            cmp $1, %rax
            // if it wasn't a digit, just continue
            jne _continue_parse

            // it was a digit, so push current number to stack
            // multiply it by the sign
            mov %r11, %rax
            imul %r12, %rax
            push %rax

            // reset for next number
            xor %r11, %r11
            // positive by default
            mov $1, %r12
            
            jmp _continue_parse

        _not_space:
            // if I have '+' or '-' followed by a digit
            // then this is the sign for the next number
            cmpb $'+', (%rbx)
            jne _maybe_minus_sign
                // ok I have '+', is the next char a digit?
                xor %rax, %rax
                movb 1(%rbx), %al
                push %rax
                call _test_if_digit
                cmp $1, %rax
                // if not a digit, then it is not a sign for the number
                jne _not_sign_for_number
                    // it is a + followed by a digit
                    // set positive sign
                    mov $1, %r12
                    jmp _continue_parse

            _maybe_minus_sign:
                cmpb $'-', (%rbx)
                jne _not_sign_for_number
                xor %rax, %rax
                movb 1(%rbx), %al
                push %rax
                call _test_if_digit
                cmp $1, %rax
                jne _not_sign_for_number
                    // it is a minus
                    mov $-1, %r12
                    jmp _continue_parse


            _not_sign_for_number:
                // do I have a digit right now?
                xor %rax, %rax
                movb (%rbx), %al
                push %rax
                call _test_if_digit
                cmp $1, %rax
                jne _maybe_add
                    // yes, it is a digit
                    xor %rdx, %rdx
                    movb (%rbx), %dl
                    mov $48, %r14
                    // digit = char - '0'
                    sub %r14, %rdx

                    imul %r10, %r11
                    add %rdx, %r11
                    jmp _continue_parse

                _maybe_add:
                    cmpb $'+', (%rbx)
                    jne _maybe_minus
                        // call add
                        pop %rdi
                        pop %rsi
                        call _add
                        push %rax
                        jmp _continue_parse
                _maybe_minus:
                    cmpb $'-', (%rbx)
                    jne _maybe_multiplication
                        // call sub
                        pop %rdi
                        pop %rsi
                        call _sub
                        push %rax
                        jmp _continue_parse
                _maybe_multiplication:
                    cmpb $'*', (%rbx)
                    jne _maybe_division
                        // call multiplication
                        pop %rdi
                        pop %rsi
                        call _multiplication
                        push %rax
                        jmp _continue_parse
                _maybe_division:
                    cmpb $'/', (%rbx)
                    jne _continue_parse
                        // call division
                        pop %rdi
                        pop %rsi
                        call _division
                        push %rax
                        jmp _continue_parse                                                


        _continue_parse:
            inc %rbx
            loop _parse

    call _print
    jmp _fin

_test_if_digit:
    // returns 1 in rax if the character received as parameter is a digit
    // 0 otherwise
    push %rbp
    movq %rsp, %rbp

    // consider it is not a digit by default
    xor %rax, %rax

    // check
    mov 16(%rbp), %r15
    // it has to be between '0' and '9' in order for it to be a digit
    cmpb $'0', %r15b
    jl _test_if_digit_return
    cmpb $'9', %r15b
    jg _test_if_digit_return

    // if I passed the tests above, it is a digit
    mov $1, %rax

    _test_if_digit_return:
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
