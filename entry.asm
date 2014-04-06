[bits 16]
[cpu 8086]

org 0x100
	
section .text

entry:
	mov ah, 0x0f
	int 0x10     ; get video mode
	mov [mode], ax     ; store vid mode
	
	xor ah, ah
	mov al, 0x13
	int 0x10     ; set to graphics mode
	
	call main
	
terminate:
	mov ax, [mode]      ; restore vid mode
	xor ah, ah
	int 0x10
	
	mov ah, 0x4c
	int 0x21 ; exit

section .bss
	mode: resw 1

%include "gfx.asm"
%include "main.asm"
