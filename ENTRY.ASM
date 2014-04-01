	[bits 16]
	[cpu 8086]
	
	org 0x100
entry:
	mov ah, 0x0f
	int 0x10     ; get video mode
	mov [mode], ax     ; store vid mode
	
	mov ah, 0x00
	mov al, 0x13
	int 0x10     ; set to graphics mode
	
	call main
	
terminate:
	mov ax, [mode]      ; restore vid mode
	mov ah, 0x00
	int 0x10
	
	mov ah, 0x4c
	int 0x21 ; exit
	
	mode dw 0

%include "GFX.ASM"
%include "MAIN.ASM"
