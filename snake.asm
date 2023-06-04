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

STATE_EMPTY: equ 0b00000000
STATE_SNAKE: equ 0b01010101
STATE_POINT: equ 0b10101010
STATE_ANY:   equ 0b11111111

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

	jmp main

terminate:
	mov ax, [mode]      ; restore vid mode
	xor ah, ah
	int 0x10

	mov ah, 0x4c
	int 0x21 ; exit

%include "gfx.asm"

section .bss
	mode: resw 1
	rnd_seed: resw 1
	snake: resb 2*MAX_SNAKE_LEN

	; Each byte contains four two-bit values corresponding to
	; STATE_EMPTY, STATE_SNAKE or STATE_POINT.
	tile_states: resb (NUM_XTILES*NUM_YTILES+3)/4

section .data
	snake_tail_idx: dw 0
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
	; sprite:
	; incbin "sprite.raw"
; .len: equ $-sprite

section .text

main:
	xor ah, ah
	int 0x1a ; get time
	mov [rnd_seed], dx

	call render_score
	call init_grid
	call init_snake
	call new_random_point

.gameloop:
	;sleep
	mov ah, 0x86
	mov dx, [interval]     ; lsw
	mov cx, [interval + 2] ; msw: little endian
	int 0x15    ; microseconds

	call input
	call next_head_pos

	push ax ; save

	push ax
	call get_tile_state
	test al, STATE_SNAKE
	jnz die ; die if there is a snake
	test al, STATE_POINT

	pop ax ; restore
	jz .no_point

	; hit point
	cmp word [interval + 2], 0
	je .max_speed_reached

	; speed up
	sub word [interval], ACCELERATION
	sbb word [interval + 2], 0

.max_speed_reached:
	push ax ; save
	add word [score], 10
	call render_score
	; registers destroyed
	pop ax ; restore

	push ax
	call snake_move_head

	inc word [snake_length]

	call new_random_point
	jmp .gameloop

.no_point:
	push ax ; save next_head_pos

	call snake_tail_pos
	; registers intact, result in ax

	push ax
	mov bx, STATE_EMPTY
	push bx
	call set_tile_state

	push ax
	mov ax, GRID_BACKG_COLOUR
	push ax
	call fill_tile
	; registers destroyed

	pop ax ; restore next_head_pos
	push ax
	call snake_move_head

	xor dx, dx
	mov ax, [snake_tail_idx]
	inc ax
	mov bx, MAX_SNAKE_LEN
	div bx
	mov [snake_tail_idx], dx

	jmp .gameloop

; Get next position for snake head
; note: leaves registers intact
;
; Returns:
; next x in al
; next y in ah
next_head_pos:
	call snake_head_pos ;get end pos
	; registers intact, result in ax

	add al, [dir_x]
	add ah, [dir_y]

	cmp al, -1
	jne .nowrap_left
	mov al, NUM_XTILES-1

.nowrap_left:
	cmp ah, -1
	jne .nowrap_up
	mov ah, NUM_YTILES-1

.nowrap_up:
	cmp al, NUM_XTILES
	jne .nowrap_right
	xor al, al

.nowrap_right:
	cmp ah, NUM_YTILES
	jne .nowrap_down
	xor ah, ah

.nowrap_down:
	ret

; Gets a random number
;
; Returns in ax
rand:
	mov ax, RND_A
	mul word [rnd_seed]
	add ax, RND_B
	mov [rnd_seed], ax
	ret

clear_kb_buf:
	xor cx, cx
	xor ax, ax

	mov ah, 0x01
	int 0x16

	jz .return ; no key, exit

	xor ah, ah
	int 0x16
	jmp clear_kb_buf

.return:
	ret

; Display game over message
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

	call clear_kb_buf

	xor ah, ah
	int 0x16 ; get key

	jmp terminate

render_score:
	push word [score]
	mov ax, 10
	push ax
	push ax
	push ax
	mov ax, WHITE
	push ax
	call renderint

	ret

; Get pointer to byte containing state of given tile, and a mask
; indicating the specific relevant bits.
;
; [bp + 4] is tile pos
;
; Return mask in al
; Return pointer in bx
state_ptr_and_mask:
	push bp
	mov bp, sp
	push cx
	push dx

	xor bx, bx
	mov bl, [bp + 4] ; x
	xor cx, cx
	mov cl, [bp + 5] ; y

	mov ax, NUM_XTILES
	mul cx
	add ax, bx

	mov cx, 4
	div cx

	; ptr = tile_states + offset / 4
	add ax, tile_states
	mov bx, ax

	; mask = 0b11 << (offset % 4) * 2
	mov cx, dx
	shl cx, 1

	mov ax, 0b11
	shl ax, cl

	pop dx
	pop cx
	pop bp
	ret 2

; Get tile state.
; note: leaves registers intact
; [bp + 4] is tile pos
;
; Returns:
; al contains tile state bits
get_tile_state:
	push bp
	mov bp, sp
	push bx

	push word [bp + 4]
	call state_ptr_and_mask

	and al, [tile_states + bx]

	pop bx
	pop bp
	ret 2

; Set tile state.
; note: leaves registers intact
; [bp + 6] is tile pos
; [bp + 4] is tile state
set_tile_state:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx

	push word [bp + 6]
	call state_ptr_and_mask

	mov cl, [tile_states + bx]

	not al
	and cl, al

	not al
	and al, [bp + 4]
	or cl, al

	mov [tile_states + bx], cl

	pop cx
	pop bx
	pop ax
	pop bp
	ret 4

; Gets the snake start pos.
; note: leaves registers intact
;
; Returns x in al
; Returns y in ah
snake_tail_pos:
	push bx

	mov bx, [snake_tail_idx]
	shl bx, 1
	mov ax, [snake + bx]

	pop bx
	ret

; Gets the snake end pos.
; note: leaves registers intact
;
; Returns x in al
; Returns y in ah
snake_head_pos:
	push bx
	push dx

	xor dx, dx
	mov ax, [snake_tail_idx]
	add ax, [snake_length]
	dec ax
	mov bx, MAX_SNAKE_LEN
	div bx

	mov bx, dx
	shl bx, 1
	mov ax, [snake + bx]

	pop dx
	pop bx
	ret

; Sets the snake head pos.
; note: leaves registers intact
;
; [bp + 4] contains new pos
snake_move_head:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx

	mov ax, [snake_length]
	add ax, [snake_tail_idx]
	mov bx, MAX_SNAKE_LEN
	div bx

	mov ax, [bp + 4]

	mov bx, dx
	shl bx, 1
	mov [snake + bx], ax

	push ax
	mov ax, SNAKE_COLOUR
	push ax
	call fill_tile ; colourise
	; registers destroyed

	push word [bp + 4]
	mov bx, STATE_SNAKE
	push bx
	call set_tile_state

	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2

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
fill_tile:
	push bp
	mov bp, sp

	push word [bp + 6]
	call get_tile_pix

	push ax
	push bx
	mov ax, TILE_WIDTH-1
	push ax
	mov ax, TILE_HEIGHT-1
	push ax
	mov ax, [bp + 4]
	push ax
	call fillrect

	pop bp
	ret 4

; Handle input
input:
	mov al, [dir_x]
	mov [dir_prev_x], al
	mov al, [dir_y]
	mov [dir_prev_y], al

.repeat:
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
	jmp .repeat

.leftbreak:
	cmp byte [dir_prev_y], -1
	je .downbreak
	cmp ah, 0x50 ; down arrow scan code
	jne .downbreak
	mov byte [dir_x], 0
	mov byte [dir_y], 1
	jmp .repeat

.downbreak:
	cmp byte [dir_prev_x], -1
	je .rightbreak
	cmp ah, 0x4d ; right arrow scan code
	jne .rightbreak
	mov byte [dir_x], 1
	mov byte [dir_y], 0
	jmp .repeat

.rightbreak:
	cmp byte [dir_prev_y], 1
	je input ;repeat
	cmp ah, 0x48 ; up arrow scan code
	jne input ;repeat
	mov byte [dir_x], 0
	mov byte [dir_y], -1
	jmp .repeat

new_random_point:
	call rand ; ax
	xor dx, dx
	mov bx, NUM_YTILES
	div bx
	mov cl, dl ;y

	call rand ; ax
	xor dx, dx
	mov bx, NUM_XTILES ;x
	div bx

	mov dh, cl

	push dx
	call get_tile_state
	test al, STATE_ANY
	jnz new_random_point ;loop if not empty

	push dx
	mov ax, STATE_POINT
	push ax
	call set_tile_state

	push dx
	mov ax, POINT_COLOUR
	push ax
	call fill_tile
	ret

init_snake:
	mov cx, START_SNAKE_LEN
	mov [snake_length], cx

.loop:
	; x
	mov al, cl
	inc al

	; y
	mov ah, 2

	mov bx, cx
	dec bx
	shl bx, 1

	mov [snake + bx], ax

	push ax
	mov bx, STATE_SNAKE
	push bx
	call set_tile_state
	; registers preserved

	push cx

	push ax ; pos
	mov ax, SNAKE_COLOUR
	push ax ; colour
	call fill_tile
	; registers destroyed

	pop cx
	loop .loop
	ret

init_grid:
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
