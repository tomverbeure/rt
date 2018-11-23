	.section .text
	.global start
	.global main

start:
	/* set stack pointer */
	lui sp,(4*1024)>>12

	/* jump to main C code */
	jal ra,main

	/* trap */
	ebreak

