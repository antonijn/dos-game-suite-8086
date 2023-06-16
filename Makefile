snake.com: snake.asm lib/gfx.asm
	nasm -f bin -o $@ snake.asm

clean:
	rm -f *.com
