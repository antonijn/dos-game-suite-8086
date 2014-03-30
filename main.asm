%include "gfx.asm"

%define GRID_WIDTH  300
%define GRID_HEIGHT 150
%define TILE_WIDTH  10
%define TILE_HEIGHT 10
%define GRID_XOFFS  10
%define GRID_YOFFS  25

main:
	call initgrid
	
.l:
	jmp .l

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
	push word 0
	call renderlinev
	; registers destroyed
	pop cx
	
	add cx, TILE_WIDTH
	jmp .gridxloop
.gridxbreak:

	mov cx, GRID_YOFFS
.gridyloop:
	cmp cx, GRID_HEIGHT+GRID_YOFFS
	jge .gridybreak
	
	push cx
	push word GRID_XOFFS
	push cx
	push word GRID_WIDTH
	push word 0
	call renderlineh
	; registers destroyed
	pop cx
	
	add cx, TILE_HEIGHT
	jmp .gridyloop
.gridybreak:

	retn