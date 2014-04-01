build:
	nasm -f bin -o SNAKE.COM entry.asm
	dosbox ./SNAKE.COM

