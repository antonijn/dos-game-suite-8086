[bits 16]
[cpu 8086]
org 0x100

GRID_WIDTH: equ 300
GRID_HEIGHT: equ 150
TILE_WIDTH: equ 10
TILE_HEIGHT: equ 10
GRID_XOFFS: equ 10
GRID_YOFFS: equ 25
NUM_XTILES: equ GRID_WIDTH/TILE_WIDTH
NUM_YTILES: equ GRID_HEIGHT/TILE_HEIGHT
MAX_SNAKE_LEN: equ 0x80
START_SNAKE_LEN: equ 5
ACCELERATION: equ 2000

BORDER_COLOUR: equ LIGHT_GREEN
POINT_COLOUR: equ WHITE
GRID_BACKG_COLOUR: equ LIGHT_GRAY
GRID_LINE_COLOUR: equ DARK_GRAY
SNAKE_COLOUR: equ GREEN

RND_A: equ 5555
RND_B: equ 444

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
	rnd_seed: resw 1
	snake: resb 2*MAX_SNAKE_LEN

	pnt_x: resb 1
	pnt_y: resb 1

section .data
	snake_startidx: dw 0
	snake_length: dw 0
	dir_x: db 1
	dir_y: db 0
	dir_prev_x: db 1
	dir_prev_y: db 0
	
	score: dw 0
	
	interval: dd 0x30000

	msg0: db "YOU SCORED "
.len: equ $-msg0
	msg1: db " POINTS!"
.len: equ $-msg1
	msg2: db "PRESS ANY KEY TO QUIT"
.len: equ $-msg2

section .text

main:
	xor ah, ah
	int 0x1a ; get time
	mov [rnd_seed], dx

	call renderscore
	call initgrid
	call initsnake
	call newpoint
	
.gameloop:
	;sleep
	mov ah, 0x86
	mov dx, [interval]     ; lsw
	mov cx, [interval + 2] ; msw: little endian
	int 0x15    ; microseconds
	
	call snake_endpos ;get end pos
	; registers intact, result in ax
	
	push ax
	
	mov al, [dir_x]
	mov [dir_prev_x], al
	mov al, [dir_y]
	mov [dir_prev_y], al
	call input
	
	pop ax ;pop endpos back
	add al, [dir_x]
	add ah, [dir_y]
	
	cmp al, -1
	jne .xneminusone
	mov al, NUM_XTILES-1
	
.xneminusone:
	cmp ah, -1
	jne .yneminusone
	mov ah, NUM_YTILES-1
	
.yneminusone:
	cmp al, NUM_XTILES
	jne .xnentiles
	xor al, al
	
.xnentiles:
	cmp ah, NUM_YTILES
	jne .ynentiles
	xor ah, ah
	
.ynentiles:
	call snake_setnextpos ;move head
	; registers intact
	
	push ax ;store before is_tile_empty
	
	push ax
	call is_tile_empty
	jnz .continue ; continue if empty
	
	jmp die
	
.continue:
	pop ax ;restore
	push ax; save before boxcol
	
	push ax
	mov ax, SNAKE_COLOUR
	push ax
	call snake_boxcol ;colourise
	; registers destroyed
	
	pop ax ;restore
	
	cmp al, [pnt_x] ; x == pnt_x
	jne .nepnt
	cmp ah, [pnt_y] ; && y == pnt_y
	jne .nepnt
	
	; hit point
	cmp word [interval + 2], 0
	je .skip_speedup
	
	; speed up
	sub word [interval], ACCELERATION
	sbb word [interval + 2], 0
	
.skip_speedup:
	add word [score], 10
	call renderscore
	call newpoint
	
	inc word [snake_length]
	jmp .gameloop
	
.nepnt:
	call snake_startpos
	; registers intact, result in ax
	
	push ax
	mov ax, GRID_BACKG_COLOUR
	push ax
	call snake_boxcol
	; registers destroyed
	
	xor dx, dx
	mov ax, [snake_startidx]
	inc ax
	mov bx, MAX_SNAKE_LEN
	div bx
	mov [snake_startidx], dx
	
	jmp .gameloop

; Gets a random number
;
; Returns in ax
rand:
	mov ax, RND_A
	mul word [rnd_seed]
	add ax, RND_B
	mov [rnd_seed], ax
	ret

clearkbbuf:
	xor cx, cx
	xor ax, ax

	mov ah, 0x01
	int 0x16

	jz .return ; no key, exit
        
	xor ah, ah
	int 0x16
	jmp clearkbbuf
	
.return:
	ret
	
die:
	xor ax, ax
	push ax
	push ax
	mov ax, 320
	push ax
	mov ax, 200
	push ax
	mov ax, BLACK
	push ax
	call fillrect
	
	mov ah, 0x86
	xor dx, dx     ; lsw
	mov cx, 0x0006 ; msw: little endian
	int 0x15    ; sleep microseconds
	
	mov ax, msg0 ; YOU SCORED 
	push ax
	mov ax, msg0.len
	push ax
	mov ax, 10
	push ax
	push ax
	mov ax, WHITE
	push ax
	call renderstring
	
	mov ax, [score]
	push ax
	mov ax, 10
	push ax
	mov ax, 10+5*msg0.len
	push ax
	mov ax, 10
	push ax
	mov ax, YELLOW
	push ax
	call renderint
	; ax contains chars written
	
	mov bx, msg1 ; POINTS!
	push bx
	mov bx, msg1.len
	push bx
	mov bx, 5
	mul bx
	add ax, 10+5*msg0.len
	push ax
	mov ax, 10
	push ax
	mov ax, WHITE
	push ax
	call renderstring
	
	mov ax, msg2 ; PRESS ANY KEY TO QUIT
	push ax
	mov ax, msg2.len
	push ax
	mov ax, 10
	push ax
	mov ax, 17
	push ax
	mov ax, WHITE
	push ax
	call renderstring
	
	call clearkbbuf
	
	xor ah, ah
	int 0x16 ; get key
	
	jmp terminate
	
renderscore:
	push word [score]
	mov ax, 10
	push ax
	push ax
	push ax
	mov ax, WHITE
	push ax
	call renderint
	
	ret
	
; Checks if given tile is not a snake.
;
; [bp + 4] is tile pos
;
; zf is 0 if no snake occupies tile
is_tile_empty:
	push bp
	mov bp, sp
	
	mov ax, 0xa000
	mov es, ax
	
	push word [bp + 4]
	call get_tile_pix
	
	xchg ax, bx ;ax now has y
	mov cx, 320
	mul cx
	add ax, bx ; y*320 + x
	mov bx, ax
	
	xor ax, ax
	mov al, [es:bx]
	
	pop bp
	
	sub al, SNAKE_COLOUR
	test al, al
	ret 2
	
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
	ret

; Gets the snake end pos.
; note: leaves registers intact
;
; Returns x in al
; Returns y in ah
snake_endpos:
	push bx
	push dx
	
	xor dx, dx
	mov ax, [snake_length]
	add ax, [snake_startidx]
	dec ax
	mov bx, MAX_SNAKE_LEN
	div bx
	
	mov ax, 2
	mul dx
	mov bx, ax
	
	mov ax, [snake + bx]
	
	pop dx
	pop bx
	ret

; Sets the snake end pos.
; note: leaves registers intact
;
; al contains x
; ah contains y
snake_setnextpos:
	push bx
	push dx
	push ax
	
	xor dx, dx
	mov ax, [snake_length]
	add ax, [snake_startidx]
	mov bx, MAX_SNAKE_LEN
	div bx
	
	mov ax, 2
	mul dx
	mov bx, ax
	
	pop ax
	mov [snake + bx], ax
	
	pop dx
	pop bx
	ret

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
	add ax, GRID_XOFFS
	inc ax
	mov bx, ax         ; bx = x*tw + xoffs + 1
	
	mov ax, TILE_HEIGHT
	mul cx
	add ax, GRID_YOFFS
	inc ax
	mov cx, ax         ; cx = y*th + yoffs + 1
	
	mov ax, bx
	mov bx, cx
	
	pop bp
	ret 2

; Sets the colour of a box.
;
; [bp + 6] contains the pos
; [bp + 4] contains the colour
snake_boxcol:
	push bp
	mov bp, sp
	
	push word [bp + 6]
	call get_tile_pix
	
	push ax
	push bx
	mov ax, TILE_WIDTH-1
	push ax ; TILE_WIDTH-1
	mov ax, TILE_HEIGHT-1
	push ax ; TILE_HEIGHT-1
	mov ax, [bp + 4]
	push ax
	call fillrect
	
	pop bp
	ret 4

; Get input
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

	cmp byte [dir_prev_x], 1
	je .leftbreak
	cmp ah, 0x4b ; left arrow scan code
	jne .leftbreak
	mov byte [dir_x], -1
	mov byte [dir_y], 0
	jmp input

.leftbreak:
	cmp byte [dir_prev_y], -1
	je .downbreak
	cmp ah, 0x50 ; down arrow scan code
	jne .downbreak
	mov byte [dir_x], 0
	mov byte [dir_y], 1
	jmp input
	
.downbreak:
	cmp byte [dir_prev_x], -1
	je .rightbreak
	cmp ah, 0x4d ; right arrow scan code
	jne .rightbreak
	mov byte [dir_x], 1
	mov byte [dir_y], 0
	jmp input
	
.rightbreak:
	cmp byte [dir_prev_y], 1
	je input ;repeat
	cmp ah, 0x48 ; up arrow scan code
	jne input ;repeat
	mov byte [dir_x], 0
	mov byte [dir_y], -1
	jmp input ;repeat
	
newpoint:
	call rand ; ax
	xor dx, dx
	mov bx, NUM_YTILES
	div bx
	mov [pnt_y], dl
	mov cl, dl ;y
	
	call rand ; ax
	xor dx, dx
	mov bx, NUM_XTILES ;x
	div bx
	mov [pnt_x], dl
	
	mov dh, cl
	
	push dx
	
	push dx
	call is_tile_empty
	jnz .break ;break if empty
	
	pop dx
	jz newpoint ;loop if not empty
	
.break:
	pop dx
	
	push dx
	mov ax, POINT_COLOUR
	push ax
	call snake_boxcol
	
	ret
	
initsnake:
	xor cx, cx
	
.loop:
	cmp cx, START_SNAKE_LEN
	jb .continue
	ret
	
.continue:
	mov ax, 2
	mul cx     ; snake[2 * i]
	mov bx, ax
	
	mov al, cl ;x
	mov ah, 2  ;y
	add al, 2
	
	mov [snake + bx], ax
	
	push cx
	
	push ax ;pos
	mov ax, SNAKE_COLOUR
	push ax ;colour
	call snake_boxcol
	; registers destroyed
	
	inc word [snake_length]
	
	pop cx
	inc cx
	jmp .loop
	
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
	mov ax, GRID_BACKG_COLOUR
	push ax
	call fillrect
	
	; grid vertical stripes
	mov cx, GRID_XOFFS
	
.xloop:
	cmp cx, GRID_WIDTH+GRID_XOFFS
	jae .xbreak
	
	push cx
	push cx
	mov ax, GRID_YOFFS
	push ax
	mov ax, GRID_HEIGHT
	push ax
	mov ax, GRID_LINE_COLOUR
	push ax
	call renderlinev
	; registers destroyed
	pop cx
	
	add cx, TILE_WIDTH
	jmp .xloop
	
.xbreak:
	; grid horizontal stripes
	mov cx, GRID_YOFFS
	
.yloop:
	cmp cx, GRID_HEIGHT+GRID_YOFFS
	jae .ybreak
	
	push cx
	mov ax, GRID_XOFFS
	push ax
	push cx
	mov ax, GRID_WIDTH
	push ax
	mov ax, GRID_LINE_COLOUR
	push ax
	call renderlineh
	; registers destroyed
	pop cx
	
	add cx, TILE_HEIGHT
	jmp .yloop
	
.ybreak:
	; grid outline
	mov ax, GRID_XOFFS
	push ax
	mov ax, GRID_YOFFS
	push ax
	mov ax, GRID_WIDTH+1
	push ax
	mov ax, BORDER_COLOUR
	push ax
	call renderlineh
	
	mov ax, GRID_XOFFS
	push ax
	mov ax, GRID_YOFFS+GRID_HEIGHT
	push ax
	mov ax, GRID_WIDTH+1
	push ax
	mov ax, BORDER_COLOUR
	push ax
	call renderlineh
	
	mov ax, GRID_XOFFS
	push ax
	mov ax, GRID_YOFFS+1
	push ax
	mov ax, GRID_HEIGHT-1
	push ax
	mov ax, BORDER_COLOUR
	push ax
	call renderlinev
	
	mov ax, GRID_XOFFS+GRID_WIDTH
	push ax
	mov ax, GRID_YOFFS+1
	push ax
	mov ax, GRID_HEIGHT-1
	push ax
	mov ax, BORDER_COLOUR
	push ax
	call renderlinev

	ret
