; tiny.s from http://www.muppetlabs.com/~breadbox/software/tiny/teensy.html
; 
; compile with
;   nasm -f elf tiny.s
;   ld -s tiny.o
BITS 32
GLOBAL _start
SECTION .text
_start:
        mov     eax, 1
        mov     ebx, 42  
        int     0x80
