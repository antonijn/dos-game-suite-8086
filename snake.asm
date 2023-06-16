[bits 16]
[cpu 8086]
org 0x100

GRID_WIDTH: equ 297
GRID_HEIGHT: equ 154
TILE_WIDTH: equ 11
TILE_HEIGHT: equ 11
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

NORTH: equ 0
EAST:  equ 1
SOUTH: equ 2
WEST:  equ 3

section .text

entry:
	mov bx, main
	push bx
	call enter_13h

	mov ah, 0x4c
	int 0x21 ; exit

%include "lib/gfx.asm"

section .bss
	mode: resw 1
	rnd_seed: resw 1
	snake: resb 2*MAX_SNAKE_LEN

	; Each byte contains four two-bit values corresponding to
	; STATE_EMPTY, STATE_SNAKE or STATE_POINT.
	tile_states: resb (NUM_XTILES*NUM_YTILES+3)/4

section .data
	init_snake_images:
		dw img_tail          + TILE_WIDTH*TILE_HEIGHT
		dw img_body_straight + TILE_WIDTH*TILE_HEIGHT
		dw img_body_straight + TILE_WIDTH*TILE_HEIGHT
		dw img_body_straight + TILE_WIDTH*TILE_HEIGHT
		dw img_head          + TILE_WIDTH*TILE_HEIGHT

	snake_tail_idx: dw 0
	snake_length: dw 0
	dir_x: db 1
	dir_y: db 0
	dir_prev_x: db 1
	dir_prev_y: db 0
	cardinal_dir: db EAST
	prev_cardinal_dir: db EAST

	score: dw 0

	interval: dd 0x30000

	msg0: db "YOU SCORED "
.len: equ $-msg0
	msg1: db " POINTS!"
.len: equ $-msg1
	msg2: db "PRESS ANY KEY TO QUIT"
.len: equ $-msg2

%include "assets/snake.asm"

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

	; cx = next head pos
	mov cx, ax

	push ax
	call get_tile_state
	test al, STATE_SNAKE
	jz .no_snake
	call die
	ret

.no_snake:
	test al, STATE_POINT

	jz .no_point

	; hit point
	cmp word [interval + 2], 0
	je .max_speed_reached

	; speed up
	sub word [interval], ACCELERATION
	sbb word [interval + 2], 0

.max_speed_reached:
	add word [score], 10
	call render_score

	push cx
	call snake_move_head

	inc word [snake_length]

	call new_random_point
	jmp .gameloop

.no_point:
	call snake_move_tail

	push cx
	call snake_move_head

	mov ax, [snake_tail_idx]
	inc ax
	and ax, MAX_SNAKE_LEN-1
	mov [snake_tail_idx], ax

	jmp .gameloop

; Get next position for snake head
; note: leaves registers intact
;
; Returns:
; next x in al
; next y in ah
next_head_pos:
	call snake_head_pos
	; result in ax

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
; Courtesy of https://lemire.me/blog/2019/07/03/a-fast-16-bit-random-number-generator/
;
; Returns in ax
rand:
	push dx

	mov ax, [rnd_seed]
	add ax, 0xFC15
	mov [rnd_seed], ax

	mov dx, 0x2AB
	mul dx

	; we use both high and low word
	xor ax, dx

	pop dx
	ret

; note: Caller-saved registers
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
	call fill_rect

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
	call render_string

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
	call render_int
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
	call render_string

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
	call render_string

	call clear_kb_buf

	xor ah, ah
	int 0x16 ; get key

	ret

render_score:
	push ax

	; clear background
	mov ax, 10
	push ax
	push ax
	mov ax, 5*TILE_WIDTH
	push ax
	mov ax, TILE_HEIGHT
	push ax
	mov ax, BLACK
	push ax
	call fill_rect

	push word [score]
	mov ax, 10
	push ax
	push ax
	push ax
	mov ax, WHITE
	push ax
	call render_int

	pop ax
	ret

; [bp + 4] x, y delta
;
; Output in ax
cardinal:
	push bp
	mov bp, sp
	push bx

	mov bx, [bp + 4]
	xor ax, ax

	; x
	test bl, bl
	js .west
	jnz .east

	; y
	test bh, bh
	js .north
	jmp .south
.west:
	inc ax
.south:
	inc ax
.east:
	inc ax
.north:
	pop bx
	pop bp
	ret 2

; [bp + 6] Initial
; [bp + 4] Final
delta_pos:
	push bp
	mov bp, sp
	push bx
	push cx

	mov ax, [bp + 4]
	mov bx, [bp + 6]

	; cl = delta y
	mov cl, ah
	sub cl, bh

	; al = delta x
	sub al, bl

	mov bl, NUM_XTILES/2
	mov bh, NUM_XTILES
	cmp al, bl
	jg .correct_x
	neg bl
	neg bh
	cmp al, bl
	jl .correct_x
.check_y:
	mov bl, NUM_YTILES/2
	mov bh, NUM_YTILES
	cmp cl, bl
	jg .correct_y
	neg bl
	neg bh
	cmp cl, bl
	jl .correct_y
	jmp .corrected

.correct_x:
	sub al, bh
	jmp .check_y
.correct_y:
	sub cl, bh

.corrected:
	mov ah, cl

	pop cx
	pop bx
	pop bp
	ret 4

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
	xor dx, dx
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
;
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

; Gets the snake end pos.
;
; Returns x in al
; Returns y in ah
snake_head_pos:
	push bx

	mov bx, [snake_tail_idx]
	add bx, [snake_length]
	dec bx
	and bx, MAX_SNAKE_LEN-1
	shl bx, 1

	mov ax, [snake + bx]

	pop bx
	ret

; Sets the snake head pos.
;
; [bp + 4] contains new pos
snake_move_head:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push si
	push di

	mov ax, [snake_length]
	add ax, [snake_tail_idx]
	mov bx, MAX_SNAKE_LEN
	xor dx, dx
	div bx

	; cx = new snake pos
	mov cx, [bp + 4]

	mov di, dx
	shl di, 1
	mov [snake + di], cx

	mov ax, TILE_WIDTH*TILE_HEIGHT
	xor bx, bx
	mov bl, [cardinal_dir]
	mul bx
	mov bx, ax

	push cx ; pos
	lea ax, [img_head + bx]
	push ax
	call blit_tile
	; registers preserved

	sub di, 2
	jns .positive_di
	add di, MAX_SNAKE_LEN*2
.positive_di:
	xor bx, bx
	mov bl, [cardinal_dir]
	xor dx, dx
	mov dl, [prev_cardinal_dir]
	mov ax, bx
	sub ax, dx
	jns .positive_al
	add al, 4
.positive_al:
	test ax, 1
	jz .straight

	shr ax, 1
	add dx, ax
	and dx, 3

	mov bx, img_body_bent
	jmp .draw_prev_snake

.straight:
	mov dx, bx
	and dx, 1
	mov bx, img_body_straight

.draw_prev_snake:
	mov ax, TILE_WIDTH*TILE_HEIGHT
	mul dx
	mov si, ax

	; prev snake pos
	push word [snake + di]
	lea ax, [si + bx]
	push ax
	call blit_tile

	push word [bp + 4]
	mov bx, STATE_SNAKE
	push bx
	call set_tile_state

	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2

snake_move_tail:
	push ax
	push bx
	push cx
	push dx

	; ax = old tail pos
	mov bx, [snake_tail_idx]
	shl bx, 1
	mov ax, [snake + bx]

	push ax
	mov cx, STATE_EMPTY
	push cx
	call set_tile_state

	push ax
	mov cx, GRID_BACKG_COLOUR
	push cx
	call fill_tile

	; cx = new tail pos
	add bx, 2
	and bx, (2*MAX_SNAKE_LEN)-1
	mov cx, [snake + bx]

	; dx = next tail pos
	add bx, 2
	and bx, (2*MAX_SNAKE_LEN)-1
	mov dx, [snake + bx]

	push cx
	push dx
	call delta_pos

	push ax
	call cardinal

	mov dx, TILE_WIDTH*TILE_HEIGHT
	mul dx

	push cx
	add ax, img_tail
	push ax
	call blit_tile

	pop dx
	pop cx
	pop bx
	pop ax
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
	push cx
	push dx

	xor bx, bx
	xor cx, cx

	mov bl, [bp + 4] ;x
	mov cl, [bp + 5] ;y

	mov ax, TILE_WIDTH
	mul bx
	add ax, GRID_XOFFS
	mov bx, ax         ; bx = x*tw + xoffs

	mov ax, TILE_HEIGHT
	mul cx
	add ax, GRID_YOFFS
	mov cx, ax         ; cx = y*th + yoffs

	mov ax, bx
	mov bx, cx

	pop dx
	pop cx
	pop bp
	ret 2

; Sets the colour of a box.
;
; [bp + 6] contains the pos
; [bp + 4] contains the colour
fill_tile:
	push bp
	mov bp, sp
	push ax
	push bx

	push word [bp + 6]
	call get_tile_pix

	push ax
	push bx
	mov ax, TILE_WIDTH
	push ax
	mov ax, TILE_HEIGHT
	push ax
	mov ax, [bp + 4]
	push ax
	call fill_rect

	pop bx
	pop ax
	pop bp
	ret 4

; Draw texture to a tile.
;
; [bp + 6] Pos
; [bp + 4] Texture
blit_tile:
	push bp
	mov bp, sp
	push ax
	push bx

	push word [bp + 6]
	call get_tile_pix

	push ax
	push bx
	mov ax, TILE_WIDTH
	push ax
	push ax
	push word [bp + 4]
	call blit

	pop bx
	pop ax
	pop bp
	ret 4

; Handle input
input:
	mov al, [cardinal_dir]
	mov [prev_cardinal_dir], al
	mov al, [dir_x]
	mov [dir_prev_x], al
	mov al, [dir_y]
	mov [dir_prev_y], al

.repeat:
	xor cx, cx
	xor ax, ax

	mov ah, 0x01
	int 0x16
	jz .end

	xor ah, ah
	int 0x16

	cmp byte [dir_prev_x], 1
	je .left_break
	cmp ah, 0x4b ; left arrow scan code
	jne .left_break
	mov byte [dir_x], -1
	mov byte [dir_y], 0
	jmp .repeat

.left_break:
	cmp byte [dir_prev_y], -1
	je .down_break
	cmp ah, 0x50 ; down arrow scan code
	jne .down_break
	mov byte [dir_x], 0
	mov byte [dir_y], 1
	jmp .repeat

.down_break:
	cmp byte [dir_prev_x], -1
	je .right_break
	cmp ah, 0x4d ; right arrow scan code
	jne .right_break
	mov byte [dir_x], 1
	mov byte [dir_y], 0
	jmp .repeat

.right_break:
	cmp byte [dir_prev_y], 1
	je input ;repeat
	cmp ah, 0x48 ; up arrow scan code
	jne input ;repeat
	mov byte [dir_x], 0
	mov byte [dir_y], -1
	jmp .repeat

.end:
	push word [dir_x]
	call cardinal
	mov [cardinal_dir], al
	ret

new_random_point:
	push ax
	push bx
	push cx
	push dx

.try:
	call rand ; ax
	mov bx, NUM_YTILES
	xor dx, dx
	div bx
	mov cl, dl ;y

	call rand ; ax
	mov bx, NUM_XTILES ;x
	xor dx, dx
	div bx

	mov dh, cl

	push dx
	call get_tile_state
	test al, STATE_ANY
	jnz .try

	push dx
	mov ax, STATE_POINT
	push ax
	call set_tile_state

	push dx ; pos
	mov ax, img_star
	push ax
	call blit_tile

	pop dx
	pop cx
	pop bx
	pop ax
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
	mov dx, STATE_SNAKE
	push dx
	call set_tile_state

	push ax ; pos
	push word [init_snake_images + bx]
	call blit_tile

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
	call fill_rect

	; grid outline
	mov ax, GRID_XOFFS-1
	push ax
	mov ax, GRID_YOFFS-1
	push ax
	mov ax, GRID_WIDTH+2
	push ax
	mov ax, BORDER_COLOUR
	push ax
	call render_line_h

	mov ax, GRID_XOFFS-1
	push ax
	mov ax, GRID_YOFFS+GRID_HEIGHT
	push ax
	mov ax, GRID_WIDTH+2
	push ax
	mov ax, BORDER_COLOUR
	push ax
	call render_line_h

	mov ax, GRID_XOFFS-1
	push ax
	mov ax, GRID_YOFFS
	push ax
	mov ax, GRID_HEIGHT
	push ax
	mov ax, BORDER_COLOUR
	push ax
	call render_line_v

	mov ax, GRID_XOFFS+GRID_WIDTH
	push ax
	mov ax, GRID_YOFFS
	push ax
	mov ax, GRID_HEIGHT
	push ax
	mov ax, BORDER_COLOUR
	push ax
	call render_line_v

	ret
