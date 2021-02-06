section .text
    global _start

_start:
    call display
    mov eax, 1
    int 0x80

display:
    mov ecx, 80

next:
    ; note x86 stack is word based, you either push a word or dword.
    ; which implies, a char [10] will need to occupy twice of the
    ; space it should occupy.
    push ecx,
    mov eax, 4
    mov ebx, 1
    mov ecx, achar
    mov edx, 1
    int 80h

    pop ecx
    mov dx, [achar]
    cmp byte [achar], 0dh
    inc byte [achar]
    loop next
    ret

section .data
achar db '0'
ascii_range db 80
