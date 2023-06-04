snake.com: snake.asm gfx.asm
	nasm -f bin -o $@ snake.asm

clean:
	rm -f *.com
