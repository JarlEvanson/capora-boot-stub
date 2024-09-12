[BITS 64]

handler:
    mov rax, 0x00000000
    mov rdi, qword [rel data]
    mov rcx, qword [rel data + 8]
    rep stosd

    mov r15, qword [rel data]
    mov r14, qword [rel data + 16]
    mov r13, 128
    mov r12, 0

    mov rdx, cr2

print_loop: ; 16 digits
    cmp r13, 0
    jz end
    sub r13, 8

    mov rax, rdx
    and rax, 0xF
    shr rdx, 4

    jmp draw_digit

end:
    jmp $

; rax = digit
; r15 = buffer_ptr
; r14 = stride
; r13 = x
; r12 = y
draw_digit:
    lea rsi, [rel data + 24]
    shl rax, 4
    add rsi, rax

    mov rbx, 16

y_loop:
    mov al, byte [rsi]
    mov rcx, 8

x_loop:
    mov r11, 0x00D0D0D0
    mov r10, 0x00000000
    test al, 0x1
    cmovz r11, r10

    lea r9, [r13 + rcx] ; x position
    lea r10, [r15 + 4 * r9]

    mov rdi, 16
    sub rdi, rbx
    lea r9, [r12 + rdi]
    imul r9, r14
    lea r10, [r10 + 4 * r9]

    mov dword [r10], r11d
    shr al, 1
    loop x_loop

    inc rsi
    dec rbx
    jnz y_loop

    jmp print_loop

data:
