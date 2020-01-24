.data
    N:
        .byte 4

.bss
    BUFF:
        .space 4

.text

.global _start

_start:
    call _read_int
    xor %ecx, %ecx
    mov $BUFF, %ecx
    push %rcx
    call _read_int
    pop %rcx
    jmp _done
    add $BUFF, %rcx
    jmp _done

_print_res:
    mov %eax, %ecx
    mov $4, %edx
    mov $4, %eax
    mov $1, %ebx
    int $0x80
    ret

_read_int:
    mov $3, %eax
    mov $0, %ebx
    mov $BUFF, %ecx
    mov $N, %edx
    int $0x80
    ret
    
_done:
    mov %ecx, %eax
    call _print_res
    mov $1, %eax
    mov $0, %ebx
    int $0x80
