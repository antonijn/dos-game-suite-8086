%define GRID_WIDTH  300
%define GRID_HEIGHT 150
%define TILE_WIDTH  10
%define TILE_HEIGHT 10
%define GRID_XOFFS  10
%define GRID_YOFFS  25
%define NUM_XTILES GRID_WIDTH/TILE_WIDTH
%define NUM_YTILES GRID_HEIGHT/TILE_HEIGHT
%define MAX_SNAKE_LEN 100
%define START_SNAKE_LEN 5

snake times MAX_SNAKE_LEN db 0,0
snake_startidx dw 0
snake_length dw 0

pnt_x db 0
pnt_y db 0

dir_x db 1
dir_y db 0

rnd_seed dw 1337
rnd_a dw 5555
rnd_b dw 444

counter dw 0

main:
	; initialise grid
	call initgrid
	
	; make the snake
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
	retn
	
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
	retn

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

; Get input
input:
	xor cx, cx

.reproc:
	mov ah, 0x01
	int 0x16
	jnz .processkey
	
	; if no key
	cmp cx, 0
	jne .handlekey ;handle if not first time
	retn ;return if first time
.processkey:
	;remove from buffer
	mov ah, 0x00
	int 0x16
	mov cx, 1
	; keep going until buffer is empty
	jmp .reproc
.handlekey:
	
	cmp al, 'a'
	jne .ab
	mov byte [dir_x], -1
	mov byte [dir_y], 0
	retn
.ab:
	
	cmp al, 's'
	jne .sb
	mov byte [dir_x], 0
	mov byte [dir_y], 1
	retn
.sb:

	cmp al, 'd'
	jne .db
	mov byte [dir_x], 1
	mov byte [dir_y], 0
	retn
.db:
	
	cmp al, 'w'
	jne .wd
	mov byte [dir_x], 0
	mov byte [dir_y], -1
.wd:
	retn
	
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
	mov ax, CYAN
	push ax
	call snake_boxcol
	
	retn
	
; Moves the snake.
move_snake:

	;sleep ~.5 seconds
	mov ah, 0x86
	mov cx, 3   ; high word (0x0003)
	xor dx, dx  ; low word  (0x0000)
	int 0x15    ; microseconds
	
	call snake_endpos ;get end pos
	; registers intact, result in ax
	
	push ax
	
	call input
	
	pop ax ;pop endpos back
	add al, [dir_x]
	add ah, [dir_y]
	
	call snake_setnextpos ;move head
	; registers intact
	
	push ax ;store before boxcol
	
	push ax
	mov ax, BLACK
	push ax
	call snake_boxcol ;colourise
	; registers destroyed
	
	pop ax ;restore
	
	cmp al, [pnt_x] ; x == pnt_x
	jne .nepnt_y
	cmp ah, [pnt_y] ; && y == pnt_y
	jne .nepnt_y
	
	call newpoint
	
	inc word [snake_length]
	jmp .inclen
	
.nepnt_y:
	call snake_startpos
	; registers intact, result in ax
	
	push ax
	mov ax, LIGHT_GRAY
	push ax
	call snake_boxcol
	; registers destroyed
	
	xor dx, dx
	mov ax, [snake_startidx]
	inc ax
	mov bx, MAX_SNAKE_LEN
	div bx
	mov [snake_startidx], dx
.inclen:
	retn
	
update:
	
	; tail call for now, 'cause why the hell not
	jmp move_snake
	
	;retn
	
initsnake:
	
	xor cx, cx
.isloop:
	cmp cx, START_SNAKE_LEN
	jge .isbreak
	
	mov ax, 2
	mul cx     ; snake[2 * i]
	mov bx, ax
	
	mov al, cl ;x
	mov ah, 2  ;y
	add al, 2
	
	mov [snake + bx], ax
	
	push cx
	
	push ax ;pos
	mov ax, BLACK
	push ax ;colour
	call snake_boxcol
	; registers destroyed
	
	inc word [snake_length]
	
	pop cx
	inc cx
	jmp .isloop
.isbreak:
	retn
	
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
	mov ax, LIGHT_GRAY
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
	mov ax, DARK_GRAY
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
	mov ax, DARK_GRAY
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
	mov ax, CYAN
	push ax
	call renderlineh
	
	mov ax, GRID_XOFFS
	push ax
	mov ax, GRID_YOFFS+GRID_HEIGHT
	push ax
	mov ax, GRID_WIDTH
	push ax
	mov ax, CYAN
	push ax
	call renderlineh
	
	mov ax, GRID_XOFFS
	push ax
	mov ax, GRID_YOFFS
	push ax
	mov ax, GRID_HEIGHT
	push ax
	mov ax, CYAN
	push ax
	call renderlinev
	
	mov ax, GRID_XOFFS+GRID_WIDTH
	push ax
	mov ax, GRID_YOFFS
	push ax
	mov ax, GRID_HEIGHT
	push ax
	mov ax, CYAN
	push ax
	call renderlinev

	retn