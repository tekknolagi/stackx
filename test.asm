global _start

section .data

    align 2
    str:    db 'Hello world!',0xA
    strLen: equ $-str

section .bss

section .text

    _start:

    mov edx, strLen ; length to write
    mov ecx, str ; string address
    mov ebx, 1 ; stdout
    mov eax, 4 ; write
    int 0x80 ; syscall


    mov ebx, 0 ; status
    mov eax, 1 ; exit
    int 0x80
