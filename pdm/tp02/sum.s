.data
    N: .byte 4
    BUFFER: .long 4

.text

.global _start


_start:
    call _read_in_buffer
    mov $BUFFER, %rax
    push (%rax)
    call _read_in_buffer
    pop %rax
    addl %eax, BUFFER
    call _write_buffer
    jmp _done

_read_in_buffer:
    mov $3, %eax
    mov $0, %ebx
    mov $BUFFER, %ecx
    mov $N, %edx
    int $0x80
    ret

_write_buffer:
    mov $4, %eax
    mov $1, %ebx
    mov $BUFFER, %ecx
    mov $4, %edx
    int $0x80
    ret

_done:
    mov $0, %ebx
    mov $1, %eax
    int $0x80
