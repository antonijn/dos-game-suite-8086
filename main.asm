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

dir_x db 1
dir_y db 0

counter dw 0

main:
	; initialise grid
	call initgrid
	
	; make the snake
	mov ax, word 0x0101
	mov [snake], ax
	push ax ; pos
	xor ax, ax
	push ax ; col (0)
	call snake_boxcol
	inc word [snake_length]
	
	mov ax, word 0x0102
	mov [snake+2], ax
	push ax ; pos
	xor ax, ax
	push ax ; col (0)
	call snake_boxcol
	inc word [snake_length]
	
	mov ax, word 0x0103
	mov [snake+4], ax
	push ax ; pos
	xor ax, ax
	push ax ; col (0)
	call snake_boxcol
	inc word [snake_length]
	
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
	push dx
	
	mov ax, 2
	mul word [snake_startidx]
	mov bx, ax
	mov ax, [snake + bx]
	
	pop dx
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
	mov bx, word MAX_SNAKE_LEN
	div bx
	;mov dx, ax
	
	mov ax, 2
	mul dx
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
	add ax, [snake_startidx]
	dec ax
	mov bx, word MAX_SNAKE_LEN
	div bx
	
	mov ax, 2
	mul dx
	mov bx, ax
	
	pop ax
	mov [snake + bx], ax
	
	pop dx
	pop bx
	retn

; Sets the colour of a box.
;
; [bp + 6] contains the pos
; [bp + 4] contains the colour
snake_boxcol:
	push bp
	mov bp, sp
	
	xor bx, bx
	xor cx, cx
	
	mov bl, byte [bp + 6] ;x
	mov cl, byte [bp + 7] ;y
	
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
	mov ax, word TILE_WIDTH-1
	push ax ; TILE_WIDTH-1
	mov ax, word TILE_HEIGHT-1
	push ax ; TILE_HEIGHT-1
	mov ax, word [bp + 4]
	push ax
	call fillrect
	
	pop bp
	retn 4

; Moves the snake.
move_snake:
	;sleep ~.5 seconds
	mov ah, 0x86
	mov cx, 6   ; high word (0x0004)
	xor dx, dx  ; low word  (0x0000)
	int 0x15    ; microseconds

	call snake_startpos
	; registers intact, result in ax
	
	push ax
	mov ax, 7
	push ax
	call snake_boxcol
	; registers destroyed
	
	call snake_endpos ;get end pos
	; registers intact, result in ax
	
	inc word [snake_startidx] ;move snake along
	
	cmp al, NUM_XTILES-2
	jl .skip1
	mov byte [dir_x], 0
	mov byte [dir_y], 1
.skip1:
	cmp ah, NUM_YTILES-2
	jl .skip2
	mov byte [dir_x], -1
	mov byte [dir_y], 0
.skip2:
	cmp al, 1
	jge .skip3
	mov byte [dir_x], 0
	mov byte [dir_y], -1
.skip3:
	cmp ah, 1
	jge .skip4
	mov byte [dir_x], 1
	mov byte [dir_y], 0
.skip4:
	
	add al, [dir_x]
	add ah, [dir_y]
	
	call snake_setendpos ;move head
	; registers intact
	
	push ax
	xor ax, ax
	push ax
	call snake_boxcol ;colourise
	; registers destroyed
	retn
	
update:
	
	; tail call for now, 'cause why the hell not
	jmp move_snake
	
	;retn
	
initgrid:
	
	; background
	mov ax, GRID_XOFFS
	push ax
	mov ax, GRID_YOFFS
	push ax
	mov ax, GRID_WIDTH
	push ax
	mov ax, GRID_HEIGHT
	push ax
	mov ax, 0x7
	push ax
	call fillrect
	
	; grid vertical stripes
	mov cx, GRID_XOFFS
.gridxloop:
	cmp cx, GRID_WIDTH+GRID_XOFFS
	jge .gridxbreak
	
	push cx
	push cx
	mov ax, GRID_YOFFS
	push ax
	mov ax, GRID_HEIGHT
	push ax
	mov ax, 8
	push ax
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
	mov ax, GRID_XOFFS
	push ax
	push cx
	mov ax, GRID_WIDTH
	push ax
	mov ax, 8
	push ax
	call renderlineh
	; registers destroyed
	pop cx
	
	add cx, TILE_HEIGHT
	jmp .gridyloop
.gridybreak:

	; grid outline
	mov ax, GRID_XOFFS
	push ax
	mov ax, GRID_YOFFS
	push ax
	mov ax, GRID_WIDTH
	push ax
	mov ax, 9
	push ax
	call renderlineh
	
	mov ax, GRID_XOFFS
	push ax
	mov ax, GRID_YOFFS+GRID_HEIGHT
	push ax
	mov ax, GRID_WIDTH
	push ax
	mov ax, 9
	push ax
	call renderlineh
	
	mov ax, GRID_XOFFS
	push ax
	mov ax, GRID_YOFFS
	push ax
	mov ax, GRID_HEIGHT
	push ax
	mov ax, 9
	push ax
	call renderlinev
	
	mov ax, GRID_XOFFS+GRID_WIDTH
	push ax
	mov ax, GRID_YOFFS
	push ax
	mov ax, GRID_HEIGHT
	push ax
	mov ax, 9
	push ax
	call renderlinev

	retn