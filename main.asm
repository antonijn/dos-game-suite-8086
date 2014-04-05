GRID_WIDTH  equ 300
GRID_HEIGHT equ 150
TILE_WIDTH  equ 10
TILE_HEIGHT equ 10
GRID_XOFFS  equ 10
GRID_YOFFS  equ 25
NUM_XTILES equ GRID_WIDTH/TILE_WIDTH
NUM_YTILES equ GRID_HEIGHT/TILE_HEIGHT
MAX_SNAKE_LEN equ 100
START_SNAKE_LEN equ 5
ACCELERATION equ 2000

BORDER_COLOUR equ LIGHT_GREEN
POINT_COLOUR equ WHITE
GRID_BACKG_COLOUR equ LIGHT_GRAY
GRID_LINE_COLOUR equ DARK_GRAY
SNAKE_COLOUR equ GREEN

snake times MAX_SNAKE_LEN db 0,0
snake_startidx dw 0
snake_length dw 0

pnt_x db 0
pnt_y db 0

dir_x db 1
dir_y db 0
dir_prev_x db 1
dir_prev_y db 0

rnd_seed dw 1
rnd_a dw 5555
rnd_b dw 444

score dw 0

interval dd 0x30000

msg0 db "YOU SCORED "
msg0len equ $-msg0
msg1 db " POINTS!"
msg1len equ $-msg1
msg2 db "PRESS ANY KEY TO QUIT"
msg2len equ $-msg2

main:
	mov ah, 0x00
	int 0x1a
	mov [rnd_seed], dx

	call renderscore
	call initgrid
	call initsnake
	call newpoint
	
.gameloop:
	call update
	jmp .gameloop

; Gets a random number
;
; Returns in ax
rand:
	mov ax, [rnd_a]
	mul word [rnd_seed]
	add ax, [rnd_b]
	mov word [rnd_seed], ax
	ret

clearkbbuf:
	xor cx, cx
	xor ax, ax

	mov ah, 0x01
	int 0x16

	jz .return ; no key, exit
        
	mov ah, 0x00
	int 0x16
	
	jmp clearkbbuf
.return:
	ret
	
exit:
	call clearkbbuf
	
	mov ax, 0
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
	int 0x15    ; microseconds
	
	call clearkbbuf
	
	mov ax, msg0 ; YOU SCORED 
	push ax
	mov ax, msg0len
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
	mov ax, 10+5*msg0len
	push ax
	mov ax, 10
	push ax
	mov ax, YELLOW
	push ax
	call renderint
	; ax contains chars written
	
	mov bx, msg1 ; POINTS!
	push bx
	mov bx, msg1len
	push bx
	mov bx, 5
	mul bx
	add ax, 10+5*msg0len
	push ax
	mov ax, 10
	push ax
	mov ax, WHITE
	push ax
	call renderstring
	
	mov ax, msg2 ; PRESS ANY KEY TO QUIT
	push ax
	mov ax, msg2len
	push ax
	mov ax, 10
	push ax
	mov ax, 17
	push ax
	mov ax, WHITE
	push ax
	call renderstring
	
	mov ah, 0x00
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
	
; Checks if given tile is snake.
;
; [bp + 4] is tile pos
;
; ax is not 0 if tile is snake
is_tile_snake:
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
	mov al, byte [es:bx]
	
	pop bp
	
	cmp al, SNAKE_COLOUR
	je .issnake
	
	xor ax, ax
	ret 2
	
.issnake:
	
	mov ax, 1
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
	mov bx, word MAX_SNAKE_LEN
	div bx
	;mov dx, ax
	
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
	mov ax, word [snake_length]
	add ax, [snake_startidx]
	;dec ax
	mov bx, word MAX_SNAKE_LEN
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
	
	mov bl, byte [bp + 4] ;x
	mov cl, byte [bp + 5] ;y
	
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
	mov ax, word TILE_WIDTH-1
	push ax ; TILE_WIDTH-1
	mov ax, word TILE_HEIGHT-1
	push ax ; TILE_HEIGHT-1
	mov ax, word [bp + 4]
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

	jz .return ; no key, exit
        
	mov ah, 0x00
	int 0x16

.handlekey:
	cmp byte [dir_prev_x], 1
	je .ab
	cmp al, 'a'
	jne .ab
	mov byte [dir_x], -1
	mov byte [dir_y], 0
	jmp input
.ab:
	
	cmp byte [dir_prev_y], -1
	je .sb
	cmp al, 's'
	jne .sb
	mov byte [dir_x], 0
	mov byte [dir_y], 1
	jmp input
.sb:

	cmp byte [dir_prev_x], -1
	je .db
	cmp al, 'd'
	jne .db
	mov byte [dir_x], 1
	mov byte [dir_y], 0
	jmp input
.db:
	
	cmp byte [dir_prev_y], 1
	je .wd
	cmp al, 'w'
	jne .wd
	mov byte [dir_x], 0
	mov byte [dir_y], -1
.wd:
	jmp input

.return:
	ret
	
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
	call is_tile_snake
	test ax, ax
	jz .break
	
	pop dx
	jnz newpoint
.break:
	
	pop dx
	
	push dx
	mov ax, POINT_COLOUR
	push ax
	call snake_boxcol
	
	ret
	
; Moves the snake.
move_snake:

	;sleep ~.5 seconds
	mov ah, 0x86
	mov dx, word [interval]     ; lsw
	mov cx, word [interval + 2] ; msw: little endian
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
	mov al, 0
.xnentiles:
	cmp ah, NUM_YTILES
	jne .ynentiles
	mov ah, 0
.ynentiles:
	
	call snake_setnextpos ;move head
	; registers intact
	
	push ax ;store before is_tile_snake
	
	push ax
	call is_tile_snake
	test ax, ax
	jz .continue
	
	jmp exit
	
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
	jmp .ret
	
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
.ret:
	ret
	
update:
	
	; tail call for now, 'cause why the hell not
	jmp move_snake
	
	;ret
	
initsnake:
	
	xor cx, cx
.isloop:
	cmp cx, START_SNAKE_LEN
	jae .isbreak
	
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
	jmp .isloop
.isbreak:
	ret
	
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
.gridxloop:
	cmp cx, GRID_WIDTH+GRID_XOFFS
	jae .gridxbreak
	
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
	jmp .gridxloop
.gridxbreak:

	; grid horizontal stripes
	mov cx, GRID_YOFFS
.gridyloop:
	cmp cx, GRID_HEIGHT+GRID_YOFFS
	jae .gridybreak
	
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
	jmp .gridyloop
.gridybreak:

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
