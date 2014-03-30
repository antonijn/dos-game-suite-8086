	[bits 16]
	[cpu 8086]
	
	org 0x100
entry:
	mov ah, 0x0f
	int 0x10     ; get video mode
	push ax     ; store vid mode
	
	mov ah, 0x00
	mov al, 0x13
	int 0x10     ; set to graphics mode
	
	call main
	
	pop ax      ; restore vid mode
	mov ah, 0x00
	int 0x10
	
	mov ah, 0x4c
	int 0x21 ; exit
	
%include "main.asm"
%include "gfx.asm"
