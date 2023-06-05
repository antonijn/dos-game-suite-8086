BLACK: equ 0x0
BLUE: equ 0x1
GREEN: equ 0x2
CYAN: equ 0x3
RED: equ 0x4
MAGENTA: equ 0x5
BROWN: equ 0x6
LIGHT_GRAY: equ 0x7
DARK_GRAY: equ 0x8
LIGHT_BLUE: equ 0x9
LIGHT_GREEN: equ 0xa
LIGHT_CYAN: equ 0xb
LIGHT_RED: equ 0xc
LIGHT_MAGENTA: equ 0xd
YELLOW: equ 0xe
WHITE: equ 0xf

GLYPH_WIDTH:  equ 4
GLYPH_HEIGHT: equ 5

section .text

; Blit simple square texture to screen
; ax, bx, cx and dx are preserved, di and si are not.
; [bp + 12] display_x
; [bp + 10] display_y
; [bp + 8]  tex_width
; [bp + 6]  tex_stride
; [bp + 4]  buffer
blit:
	push bp
	mov bp, sp
	push si
	push di
	push ax
	push bx
	push cx
	push dx

	mov ax, 0xa000
	mov es, ax

	mov ax, 320
	mul word [bp + 10]
	add ax, [bp + 12]
	mov di, ax

	mov si, [bp + 4]
	mov bx, [bp + 6]
	mov dx, [bp + 8]
	test dx, dx
	jz .ret

	sub bx, dx
	mov ax, dx
	cld
.line_loop:
	mov cx, dx
	rep movsb

	add si, bx
	add di, 320
	sub di, dx
	dec ax
	jnz .line_loop

.ret:
	pop dx
	pop cx
	pop bx
	pop ax
	pop di
	pop si
	pop bp
	ret 10

; Writes an int to the screen
;
; i     12[bp]
; base  10[bp]
; xoffs  8[bp]
; yoffs  6[bp]
; col    4[bp]
;
; Returns number of digits written in ax
render_int:
	push bp
	mov bp, sp
	push si
	push di
	push bx
	push cx
	push dx

	xor cx, cx
	mov ax, [bp + 12]
	test ax, ax
	jnz .calcloop

	; i is zero
	push cx
	inc cx

.calcloop:
	test ax, ax
	jz .calcleave   ; if (!i) break

	xor dx,dx
	div word [bp + 10] ; i /= base, dx has remainder
	push dx ; store rem on stack
	inc cx ; inc digit count
	jmp .calcloop

.calcleave:
	push cx     ;numDigits
	xor ax, ax
	push ax     ;numDigit

.renderloop:
	;render digits in reverse order
	pop di ;numDigit
	pop cx ;numDigits
	cmp di, cx
	jae .return

	pop bx ;pop the previously pushed remainder (digit) (first arg)

	push cx
	push di ;save numDigit, numDigits

	push bx ;digit: first arg
	mov ax, 5
	mul di
	add ax, [bp + 8]
	push ax            ;xoffsnew = xoffs + 5*numDigit
	push word [bp + 6] ;yoffs

	push word [bp + 4] ;col
	call render_digit
	; all regs destroyed

	pop bx
	inc bx
	push bx ;++numDigit (numDigit is at top of stack)

	jmp .renderloop

.return: ;numDigits in cx
	mov ax, cx

	pop dx
	pop cx
	pop bx
	pop di
	pop si
	pop bp
	ret 10

; Write digit to the screen
;
; digit 10[bp]
; xoffs  8[bp]
; yoffs  6[bp]
; col    4[bp]
render_digit:
	push bp
	mov bp, sp
	sub sp, 2
	push bx

	mov bl, [bp + 10]
	add bl, '0'
	lea si, [bp - 2]
	mov [si], bl
	call ascii_glyph

	push word [bp + 8]
	push word [bp + 6]
	push bx
	push word [bp + 4]
	call render_glyph

	pop bx
	add sp, 2
	pop bp
	ret 8

; Fills a rectangle with a colour.
;
; xoffs 12[bp]
; yoffs 10[bp]
; width  8[bp]
; height 6[bp]
; colour 4[bp]
fill_rect:
	push bp
	mov bp, sp
	push si
	push di
	push ax
	push bx
	push cx
	push dx

	mov ax, 0xa000
	mov es, ax

	mov ax, 320
	mul word [bp + 10]
	add ax, [bp + 12]
	mov si, ax

	mov dx, [bp + 8]
	mov bx, [bp + 6]
	test bx, bx
	jz .end

	mov al, [bp + 4]
	cld
.line_loop:
	mov di, si
	mov cx, dx
	rep stosb

	add si, 320
	dec bx
	jnz .line_loop

.end:
	pop dx
	pop cx
	pop bx
	pop ax
	pop di
	pop si
	pop bp
	ret 10

; Render a vertical line.
;
; xoffs 10[bp]
; yoffs  8[bp]
; height 6[bp]
; colour 4[bp]
renderlinev:
	push bp
	mov bp, sp

	push word [bp + 10]
	push word [bp + 8]
	mov ax, 1
	push ax
	push word [bp + 6]
	push word [bp + 4]
	call fill_rect

	pop bp
	ret 8

; Render a horizontal line.
;
; xoffs 10[bp]
; yoffs  8[bp]
; width  6[bp]
; colour 4[bp]
renderlineh:
	push bp
	mov bp, sp

	push word [bp + 10]
	push word [bp + 8]
	push word [bp + 6]
	mov ax, 1
	push ax
	push word [bp + 4]
	call fill_rect

	pop bp
	ret 8

; Writes a glyph to the screen
;
; [bp + 10] x coord
; [bp + 8]  y coord
; [bp + 6]  glyph address
; [bp + 4]  color
render_glyph:
	push bp
	mov bp, sp
	push si
	push di
	push ax
	push bx
	push cx
	push dx

	mov ax, 0xa000
	mov es, ax

	mov ax, 320
	mul word [bp + 8]
	add ax, [bp + 10]
	mov di, ax

	; bl = nibble mask
	mov bl, 0xF0
	mov si, [bp + 6]

	; bh = color
	mov bh, [bp + 4]

	; nibble count
	mov cx, GLYPH_HEIGHT
.loop:
	mov al, [si]
	and al, bl
	test bl, 0xF0
	jz .lower_nibble
	shr al, 1
	shr al, 1
	shr al, 1
	shr al, 1

.lower_nibble:
	mov dl, 0x08

.row_loop:
	test al, dl
	jz .skip_pixel
	mov [es:di], bh

.skip_pixel:
	inc di
	shr dl, 1
	jnz .row_loop

	test bl, 0x0F
	jz .same_byte
	inc si

.same_byte:
	; to next row
	add di, 320-4
	not bl
	loop .loop

	pop dx
	pop cx
	pop bx
	pop ax
	pop di
	pop si
	pop bp
	ret 8

; Get glyph corresponding to ASCII character.
; note: Preserves registers intact.
;
; Arguments:
; byte [si] input ASCII character
;
; Returns:
; Glyph address in bx.
ascii_glyph:
	push dx

	xor bx, bx
	xor dx, dx
	mov dl, [si]

	cmp dl, 'A'
	jl .less_a

	cmp dl, 'Z'
	jg .skip

	; is alpha
	sub dl, 'A'
	mov bx, texA
	jmp .finished

.less_a:
	cmp dl, '0'
	jl .less_zero

	cmp dl, '9'
	jg .skip

	; is digit
	sub dl, '0'
	mov bx, tex0
	jmp .finished

.less_zero:
	cmp dl, 0x21
	jne .skip

	; is exclamation mark
	mov dl, 0
	mov bx, texEM

.finished:
	add bx, dx
	add bx, dx
	add bx, dx

.skip:
	pop dx
	ret

; Writes a character to the screen
;
; ch    10[bp]
; xoffs  8[bp]
; yoffs  6[bp]
; col    4[bp]
render_char:
	push bp
	mov bp, sp
	push si
	push bx

	lea si, [bp + 10]
	call ascii_glyph
	test bx, bx
	jz .ret

	push word [bp + 8]
	push word [bp + 6]
	push bx
	push word [bp + 4]
	call render_glyph

.ret:
	pop bx
	pop si
	pop bp
	ret 8

; Writes a string to the screen
;
; str   12[bp]
; len   10[bp]
; xoffs  8[bp]
; yoffs  6[bp]
; col    4[bp]
;
; Returns x pos where it left off in ax
render_string:
	push bp
	mov bp, sp
	push si
	push di
	push bx
	push cx
	push dx

	mov di, [bp + 4]
	mov dx, [bp + 6]
	mov ax, [bp + 8]
	mov cx, [bp + 10]
	mov si, [bp + 12]
.loop:
	call ascii_glyph
	test bx, bx
	jz .skip

	push ax
	push dx
	push bx
	push di
	call render_glyph

.skip:
	add ax, GLYPH_WIDTH+1
	inc si
	loop .loop

.break:
	pop dx
	pop cx
	pop bx
	pop di
	pop si
	pop bp
	ret 10

section .data
tex0:	db 0b01101001, 0b10011001, 0b01100000
tex1:	db 0b00100010, 0b00100010, 0b00100000
tex2:	db 0b01101001, 0b00100100, 0b11110000
tex3:	db 0b11100001, 0b01100001, 0b11100000
tex4:	db 0b10011001, 0b11110001, 0b00010000
tex5:	db 0b11111000, 0b11100001, 0b11100000
tex6:	db 0b01111000, 0b11111001, 0b11110000
tex7:	db 0b11110001, 0b00100100, 0b10000000
tex8:	db 0b01101001, 0b01101001, 0b01100000
tex9:	db 0b11111001, 0b11110001, 0b11100000
texA:	db 0b01101001, 0b11111001, 0b10010000
texB:	db 0b11101001, 0b11111001, 0b11110000
texC:	db 0b01111000, 0b10001000, 0b01110000
texD:	db 0b11101001, 0b10011001, 0b11110000
texE:	db 0b11111000, 0b11101000, 0b11110000
texF:	db 0b11111000, 0b11101000, 0b10000000
texG:	db 0b11111000, 0b10011001, 0b11110000
texH:	db 0b10011001, 0b11111001, 0b10010000
texI:	db 0b11100100, 0b01000100, 0b11100000
texJ:	db 0b11100100, 0b01000100, 0b11000000
texK:	db 0b10011010, 0b11001010, 0b10010000
texL:	db 0b10001000, 0b10001000, 0b11110000
texM:	db 0b10011111, 0b10011001, 0b10010000
texN:	db 0b11011011, 0b10011001, 0b10010000
texO:	db 0b11111001, 0b10011001, 0b11110000
texP:	db 0b11101001, 0b11101000, 0b10000000
texQ:	db 0b11111001, 0b10011011, 0b11110000
texR:	db 0b11101001, 0b11101010, 0b10010000
texS:	db 0b11111000, 0b11110001, 0b11110000
texT:	db 0b11110010, 0b00100010, 0b00100000
texU:	db 0b10011001, 0b10011001, 0b01100000
texV:	db 0b10011001, 0b01010011, 0b00010000
texW:	db 0b10011001, 0b10011111, 0b10010000
texX:	db 0b10011001, 0b01101001, 0b10010000
texY:	db 0b10011001, 0b11110010, 0b00100000
texZ:	db 0b11110001, 0b00100100, 0b11110000
texEM:	db 0b00100010, 0b00100000, 0b00100000
