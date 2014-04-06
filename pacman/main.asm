[bits 16]
[cpu 8086]
org 0x100

WORLD_WIDTH: equ 19
WORLD_HEIGHT: equ 19
TILE_WIDTH: equ 10
TILE_HEIGHT: equ 10
WORLD_XOFFS: equ (320-TILE_WIDTH*WORLD_WIDTH)/2
WORLD_YOFFS: equ (200-TILE_HEIGHT*WORLD_HEIGHT)/2

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
	
%include "../gfx.asm"

section .bss
	mode: resw 1

section .data
	world: db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,\
	          1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,\
	          1,0,1,1,0,1,1,1,0,1,0,1,1,1,0,1,1,0,1,\
	          1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,\
	          1,0,1,1,0,1,0,1,1,1,1,1,0,1,0,1,1,0,1,\
	          1,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,1,\
	          1,1,1,1,0,1,1,1,0,1,0,1,1,1,0,1,1,1,1,\
	          0,0,0,1,0,1,0,0,0,0,0,0,0,1,0,1,0,0,0,\
	          1,1,1,1,0,1,0,1,1,0,1,1,0,1,0,1,1,1,1,\
	          0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,\
	          1,1,1,1,0,1,0,1,1,0,1,1,0,1,0,1,1,1,1,\
	          0,0,0,1,0,1,0,0,0,0,0,0,0,1,0,1,0,0,0,\
	          1,1,1,1,0,1,1,1,0,1,0,1,1,1,0,1,1,1,1,\
	          1,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,1,\
	          1,0,1,1,0,1,0,1,1,1,1,1,0,1,0,1,1,0,1,\
	          1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,\
	          1,0,1,1,0,1,1,1,0,1,0,1,1,1,0,1,1,0,1,\
	          1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,\
	          1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	
section .text
main:
	call renderworld
	
.gameloop:
	jmp .gameloop

; Gets the pixel position of a tile.
;
; [bp + 4] contains the tile pos
;
; Returns x in ax
; Returns y in bx
get_tile_pix:
	push bp
	mov bp, sp
	
	xor bx, bx
	xor cx, cx
	
	mov bl, [bp + 4] ;x
	mov cl, [bp + 5] ;y
	
	mov ax, TILE_WIDTH
	mul bx
	add ax, WORLD_XOFFS
	mov bx, ax         ; bx = x*tw + xoffs
	
	mov ax, TILE_HEIGHT
	mul cx
	add ax, WORLD_YOFFS
	mov cx, ax         ; cx = y*th + yoffs
	
	mov ax, bx
	mov bx, cx
	
	pop bp
	ret 2

; Gets index from position.
;
; pos [bp + 4]
;
; Returns in ax
get_tile_idx:
	push bp
	mov bp, sp
	
	mov cx, [bp + 4]
	xor bx, bx
	mov bl, ch ;y
	mov ax, WORLD_WIDTH
	mul bx
	
	xor bx, bx
	mov bl, cl ;x
	add ax, bx
	
	pop bp
	ret 2
	
; Determines whether tile is empty or not.
;
; pos [bp + 4]
;
; Zero flag is set if empty
is_tile_empty:
	push bp
	mov bp, sp
	
	push word [bp + 4]
	call get_tile_idx ;rets in ax
	mov bx, ax
	mov al, [world + bx]
	test al, al
	
	pop bp
	ret 2

; Sets tile to a colour.
;
; posy [bp + 7]
; posx [bp + 6]
; col [bp + 4]
rendertile:
	push bp
	mov bp, sp
	
	push word [bp + 6]
	call get_tile_pix ; ax = x, bx = y
	
	push ax
	push bx
	mov ax, TILE_WIDTH
	push ax
	mov ax, TILE_HEIGHT
	push ax
	push word [bp + 4]
	call fillrect
	
	pop bp
	ret 4
	
renderworld:
	mov al, WORLD_WIDTH-1 ;w

.xloop:
	test al, al
	js .xbreak
	
	mov ah, WORLD_HEIGHT-1 ;y

.yloop:
	test ah, ah
	js .ybreak
	
	push ax ;save
	
	push ax
	call is_tile_empty
	jz .ycontinue
	
	pop ax ;restore
	push ax ;save
	
	push ax
	mov ax, WHITE
	push ax
	call rendertile
	
	pop ax ;restore
	dec ah
	
	jmp .yloop
	
.ycontinue:
	pop ax
	dec ah
	jmp .yloop

.ybreak:
	dec al
	jmp .xloop
	
.xbreak:
	ret
	