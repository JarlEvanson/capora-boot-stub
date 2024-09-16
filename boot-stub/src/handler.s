[BITS 64]

handler:
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    push rbp

    push r8
    push r9
    push r10
    push r11
    push r12
    push r13
    push r14
    push r15

    mov rax, 0x00000000
    mov rdi, qword [rel data]
    mov rcx, qword [rel data + 8]
    rep stosd

    mov r15, qword [rel data]
    mov r14, qword [rel data + 16]

    mov r8, 6
    mov r12, 0

.frame_loop:
    mov r13, 120
    mov rdx, [rsp + 112 + r8 * 8]
    call print_u64
    add r12, 16
    dec r8
    jnz .frame_loop

    mov r13, 120
    add r12, 16
    mov rdx, cr2
    call print_u64

    mov r8, 15
    add r12, 32
.registers_loop:
    mov r13, 120
    mov rdx, [rsp - 8 + r8 * 8]
    call print_u64
    add r12, 16
    dec r8
    jnz .registers_loop

    jmp $

; Arguments
; rdx = value
; r15 = buffer_ptr
; r14 = stride
; r13 = x
; r12 = y
; Clobbers
; rax, rbx, rcx, rdx, rsi, rdi, rdbp, r9, r10, r11
print_u64:
    mov rbp, 16

.print_loop:
    mov rax, rdx
    and rax, 0xF
    shr rdx, 4

    call draw_digit
    sub r13, 8
    dec rbp
    jnz .print_loop

    ret

; Arguments:
; rax = digit
; r15 = buffer_ptr
; r14 = stride
; r13 = x
; r12 = y
; Clobbers:
; rax, rbx, rcx, rsi, rdi, r9, r10, r11
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

    ret

data:
