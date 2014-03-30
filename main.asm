%include "gfx.asm"

%define GRID_WIDTH  300
%define GRID_HEIGHT 150
%define TILE_WIDTH  10
%define TILE_HEIGHT 10
%define GRID_XOFFS  10
%define GRID_YOFFS  25
%define NUM_XTILES GRID_WIDTH/TILE_WIDTH
%define NUM_YTILES GRID_HEIGHT/TILE_HEIGHT
%define MAX_SNAKE_LEN 100

snake times MAX_SNAKE_LEN db 0,0
snake_startidx dw 0
snake_length dw 0

dir_x db 0
dir_y db 0

main:
	; initialise grid
	call initgrid
	
	; make the snake
	mov [snake], word 0x0a0a
	inc word [snake_length]
	call snake_endpos
	call snake_endposcol
	
	mov [snake+2], word 0x0b0a
	inc word [snake_length]
	call snake_endpos
	call snake_endposcol
	
	mov [snake+4], word 0x0c0a
	inc word [snake_length]
	call snake_endpos
	call snake_endposcol
	
.gameloop:
	call update
	jmp .gameloop

; Gets the snake start pos.
; note: leaves registers intact
;
; Returns x in al
; Returns y in ah
snake_startpos:
	push bx
	mov ax, [snake_startidx]
	mov bx, 2
	mul bx
	mov bx, ax
	mov ax, [snake + bx]
	pop bx
	retn

; Gets the snake end pos.
; note: leaves registers intact
;
; Returns x in al
; Returns y in ah
snake_endpos:
	push bx
	push dx
	
	mov ax, [snake_length]
	add ax, [snake_startidx]
	dec ax
	;mov bx, word MAX_SNAKE_LEN
	;div bx
	mov dx, ax
	
	mov ax, dx
	mov bx, 2
	mul bx
	mov bx, ax
	
	mov ax, [snake + bx]
	
	pop dx
	pop bx
	retn

; Sets the snake end pos.
; note: leaves registers intact
;
; al contains x
; ah contains y
snake_setendpos:
	push bx
	push dx
	push ax
	
	mov ax, word [snake_length]
	mov bx, word MAX_SNAKE_LEN
	div bx
	add dx, [snake_startidx]
	mov bx, dx
	pop ax
	mov [snake + bx], ax
	
	pop dx
	pop bx
	retn

; Colours the end position of the snake.
;
; al contains the x
; ah contains the y
snake_endposcol:
	xor bx, bx
	xor cx, cx
	
	mov bl, al ;x
	mov cl, ah ;y
	
	mov ax, TILE_WIDTH
	mul bx
	add ax, GRID_XOFFS
	inc ax
	mov bx, ax         ; bx = x*tw + xoffs + 1
	
	mov ax, TILE_HEIGHT
	mul cx
	add ax, GRID_YOFFS
	inc ax
	mov cx, ax         ; cx = y*th + yoffs + 1
	
	push bx
	push cx
	push word TILE_WIDTH-1
	push word TILE_HEIGHT-1
	push word 0
	call fillrect
	
	retn

; Moves the snake.
move_snake:
	call snake_startpos ;get start pos
	add al, [dir_x]
	add ah, [dir_y]
	;inc word [snake_startidx] ;move snake along
	;call snake_setendpos ;move head
	call snake_endposcol ;colourise
	
	retn
	
update:
	
	call move_snake
	
	retn
	
initgrid:
	
	; background
	push word GRID_XOFFS
	push word GRID_YOFFS
	push word GRID_WIDTH
	push word GRID_HEIGHT
	push word 0x7
	call fillrect
	
	; grid vertical stripes
	mov cx, GRID_XOFFS
.gridxloop:
	cmp cx, GRID_WIDTH+GRID_XOFFS
	jge .gridxbreak
	
	push cx
	push cx
	push word GRID_YOFFS
	push word GRID_HEIGHT
	push word 8
	call renderlinev
	; registers destroyed
	pop cx
	
	add cx, TILE_WIDTH
	jmp .gridxloop
.gridxbreak:

	; grid horizontal stripes
	mov cx, GRID_YOFFS
.gridyloop:
	cmp cx, GRID_HEIGHT+GRID_YOFFS
	jge .gridybreak
	
	push cx
	push word GRID_XOFFS
	push cx
	push word GRID_WIDTH
	push word 8
	call renderlineh
	; registers destroyed
	pop cx
	
	add cx, TILE_HEIGHT
	jmp .gridyloop
.gridybreak:

	; grid outline
	push word GRID_XOFFS
	push word GRID_YOFFS
	push word GRID_WIDTH
	push word 9
	call renderlineh
	
	push word GRID_XOFFS
	push word GRID_YOFFS+GRID_HEIGHT
	push word GRID_WIDTH
	push word 9
	call renderlineh
	
	push word GRID_XOFFS
	push word GRID_YOFFS
	push word GRID_HEIGHT
	push word 9
	call renderlinev
	
	push word GRID_XOFFS+GRID_WIDTH
	push word GRID_YOFFS
	push word GRID_HEIGHT
	push word 9
	call renderlinev

	retn