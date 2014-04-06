[bits 16]
[cpu 8086]
org 0x100

WORLD_WIDTH: equ 19
WORLD_HEIGHT: equ 19
TILE_WIDTH: equ 10
TILE_HEIGHT: equ 10
WORLD_XOFFS: equ (320-TILE_WIDTH*WORLD_WIDTH)/2
WORLD_YOFFS: equ (200-TILE_HEIGHT*WORLD_HEIGHT)/2
SPEED: equ 2

WALL: equ 0
COIN: equ 1
EMPTY: equ 2
BALL: equ 3

section .text

entry:
	mov ah, 0x0f
	int 0x10     ; get video mode
	mov [mode], ax     ; store vid mode
	
	xor ah, ah
	mov al, 0x13
	int 0x10     ; set to graphics mode
	
	jmp main
	
terminate:
	mov ax, [mode]      ; restore vid mode
	xor ah, ah
	int 0x10
	
	mov ah, 0x4c
	int 0x21 ; exit
	
%include "../gfx.asm"

section .bss
	mode: resw 1

struc ghost
	g_pos_x: resb 1
	g_pos_y: resb 1
	g_dir_x: resb 1
	g_dir_y: resb 1
	g_colour: resb 1
endstruc
	
section .data
	ghosts: dw ghost0, ghost1, ghost2, 0
	ghost0: istruc ghost
	        at g_pos_x, db 8
	        at g_pos_y, db 9
	        at g_dir_x, db 0
	        at g_dir_y, db 0
	        at g_colour, db RED
	        iend
	ghost1: istruc ghost
	        at g_pos_x, db 9
	        at g_pos_y, db 9
	        at g_dir_x, db 0
	        at g_dir_y, db 0
	        at g_colour, db CYAN
	        iend
	ghost2: istruc ghost
	        at g_pos_x, db 10
	        at g_pos_y, db 9
	        at g_dir_x, db 0
	        at g_dir_y, db 0
	        at g_colour, db LIGHT_GREEN
	        iend

	pos_x: dw 160-TILE_WIDTH/2
	pos_y: dw 100-TILE_HEIGHT/2
	dir_x: dw 0
	dir_y: dw 0
	world: db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,\
	          0,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,\
	          0,1,0,0,1,0,0,0,1,0,1,0,0,0,1,0,0,1,0,\
	          0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,\
	          0,1,0,0,1,0,1,0,0,0,0,0,1,0,1,0,0,1,0,\
	          0,1,1,1,1,0,1,1,1,0,1,1,1,0,1,1,1,1,0,\
	          0,0,0,0,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,\
	          2,2,2,0,1,0,1,1,1,1,1,1,1,0,1,0,2,2,2,\
	          0,0,0,0,1,0,1,0,0,2,0,0,1,0,1,0,0,0,0,\
	          1,1,1,1,1,1,1,0,2,2,2,0,1,1,1,1,1,1,1,\
	          0,0,0,0,1,0,1,0,0,0,0,0,1,0,1,0,0,0,0,\
	          2,2,2,0,1,0,1,1,1,1,1,1,1,0,1,0,2,2,2,\
	          0,0,0,0,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,\
	          0,1,1,1,1,0,1,1,1,0,1,1,1,0,1,1,1,1,0,\
	          0,1,0,0,1,0,1,0,0,0,0,0,1,0,1,0,0,1,0,\
	          0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,\
	          0,1,0,0,1,0,0,0,1,0,1,0,0,0,1,0,0,1,0,\
	          0,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,\
	          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	
section .text
main:
	call renderworld
	
.gameloop:
	;sleep
	mov ah, 0x86
	xor dx, dx     ; lsw
	mov cx, 0x0001 ; msw: little endian
	int 0x15    ; microseconds
	
	call input
	; registers destroyed
	
	; blacken
	cmp word [dir_x], SPEED
	jne .dxnespeed
	push word [pos_x]
	push word [pos_y]
	mov ax, SPEED
	push ax
	mov ax, TILE_HEIGHT
	push ax
	mov ax, BLACK
	push ax
	call fillrect

.dxnespeed:
	cmp word [dir_x], -SPEED
	jne .dxnemspeed
	mov ax, [pos_x]
	add ax, TILE_WIDTH-SPEED
	push ax
	push word [pos_y]
	mov ax, SPEED
	push ax
	mov ax, TILE_HEIGHT
	push ax
	mov ax, BLACK
	push ax
	call fillrect
	
.dxnemspeed:
	cmp word [dir_y], SPEED
	jne .dynespeed
	push word [pos_x]
	push word [pos_y]
	mov ax, TILE_WIDTH
	push ax
	mov ax, SPEED
	push ax
	mov ax, BLACK
	push ax
	call fillrect
	
.dynespeed:
	cmp word [dir_y], -SPEED
	jne .dynemspeed
	push word [pos_x]
	mov ax, [pos_y]
	add ax, TILE_HEIGHT-SPEED
	push ax
	mov ax, TILE_WIDTH
	push ax
	mov ax, SPEED
	push ax
	mov ax, BLACK
	push ax
	call fillrect
	
.dynemspeed:
	mov ax, [dir_x]
	add [pos_x], ax
	mov ax, [dir_y]
	add [pos_y], ax
	
	push word [pos_x]
	push word [pos_y]
	mov ax, TILE_WIDTH
	push ax
	mov ax, TILE_HEIGHT
	push ax
	mov ax, YELLOW
	push ax
	call fillrect

	jmp .gameloop
	
input:
	xor cx, cx
	xor ax, ax

	mov ah, 0x01
	int 0x16

	jnz .handlekey ; no key, exit
    ret
    
.handlekey:
    xor ah, ah
	int 0x16

	cmp ah, 0x4b ; left arrow scan code
	jne .leftbreak
	mov word [dir_x], -SPEED
	mov word [dir_y], 0
	jmp input

.leftbreak:
	cmp ah, 0x50 ; down arrow scan code
	jne .downbreak
	mov word [dir_x], 0
	mov word [dir_y], SPEED
	jmp input
	
.downbreak:
	cmp ah, 0x4d ; right arrow scan code
	jne .rightbreak
	mov word [dir_x], SPEED
	mov word [dir_y], 0
	jmp input
	
.rightbreak:
	cmp ah, 0x48 ; up arrow scan code
	jne input ;repeat
	mov word [dir_x], 0
	mov word [dir_y], -SPEED
	jmp input ;repeat

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
is_tile_wall:
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
	call get_tile_idx
	mov bx, ax
	mov al, [world + bx]
	test al, al ; zf if wall
	jz .wall
	
	cmp al, 2
	je .ycontinue ;continue if no coin
	
	; draw coin
	pop ax ;restore
	push ax ;save
	
	push ax
	call get_tile_pix
	add ax, TILE_WIDTH/2-1
	add bx, TILE_HEIGHT/2-1
	push ax
	push bx
	mov ax, 2
	push ax
	push ax
	mov ax, YELLOW
	push ax
	call fillrect
	
	jmp .ycontinue
	
.wall:
	pop ax ;restore
	push ax ;save
	
	push ax
	mov ax, BLUE
	push ax
	call rendertile
	
.ycontinue:
	pop ax ;restore
	dec ah
	jmp .yloop

.ybreak:
	dec al
	jmp .xloop
	
.xbreak:
	ret
	