format ELF64

section '.text' executable

extrn _exit
extrn printf
extrn InitWindow
extrn SetExitKey
extrn EndDrawing
extrn CloseWindow
extrn IsKeyPressed
extrn BeginDrawing
extrn SetTargetFPS
extrn DrawRectangle
extrn GetScreenWidth
extrn SetConfigFlags
extrn ClearBackground
extrn WindowShouldClose

define KEY_Q 81
define KEY_SPACE 32
define WINDOW_WIDTH 800
define WINDOW_HEIGHT 200
define RECT_COLOR 0xffffffff
define BLACKGROUND_COLOR 0xff181818

public _start
_start:
    mov rdi, 0x00000040
    or rdi, 0x00001000
    call SetConfigFlags    

    mov rdi, WINDOW_WIDTH
    mov rsi, WINDOW_HEIGHT
    mov rdx, title
    call InitWindow

    mov rdi, KEY_Q
    call SetExitKey

.loop:
    call WindowShouldClose
    test rax, rax
    jnz .ret

    call BeginDrawing
    mov rdi, BLACKGROUND_COLOR
    call ClearBackground

    mov edi, [x]
    mov esi, 0
    mov rdx, 200
    mov rcx, 200
    mov r8, RECT_COLOR
    call DrawRectangle
    call EndDrawing

    mov rdi, KEY_SPACE
    call IsKeyPressed

    test rax, rax
    jz .no_key

    cmp byte [freeze], 0
    je .set_freeze
    mov byte [freeze], 0
    jmp .check_freeze

.set_freeze:
    mov byte [freeze], 1

.no_key:
.check_freeze:
    cmp byte [freeze], 1
    je .skip

    add dword [x], 10

    test rax, rax
    jnz .skip

.skip:
    call GetScreenWidth
    cmp [x], rax
    jle .continue
    mov dword [x], 0

.continue:
    jmp .loop

.ret:
    call CloseWindow
    mov rdi, 0
    call _exit

section '.data' writeable
x: dd 0
y: dd 0
title: db "funny flying rectangle", 0
freeze: db 0
