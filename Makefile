build:
	nasm -f bin -o SNAKE.COM ENTRY.ASM
	dosbox ./SNAKE.COM

