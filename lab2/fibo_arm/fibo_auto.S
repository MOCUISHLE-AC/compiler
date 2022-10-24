	.arch armv7-a
	.eabi_attribute 28, 1
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 2
	.eabi_attribute 30, 6
	.eabi_attribute 34, 1
	.eabi_attribute 18, 4
	.file	"myfibo.c"
	.text
	.comm	n,4,4
	.comm	fibo,400,4
	.section	.rodata
	.align	2
.LC0:
	.ascii	"%d \000"
	.text
	.align	1
	.global	myfibo
	.arch armv7-a
	.syntax unified
	.thumb
	.thumb_func
	.fpu vfpv3-d16
	.type	myfibo, %function
myfibo:
	@ args = 0, pretend = 0, frame = 16
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{r4, r7, lr}
	sub	sp, sp, #20
	add	r7, sp, #0
	str	r0, [r7, #4]
	ldr	r4, .L7
.LPIC0:
	add	r4, pc
	ldr	r3, .L7+4
	ldr	r3, [r4, r3]
	mov	r2, r3
	movs	r3, #1
	str	r3, [r2]
	ldr	r3, .L7+4
	ldr	r3, [r4, r3]
	mov	r2, r3
	movs	r3, #1
	str	r3, [r2, #4]
	movs	r3, #0
	str	r3, [r7, #12]
	b	.L2
.L6:
	ldr	r3, [r7, #12]
	cmp	r3, #0
	beq	.L3
	ldr	r3, [r7, #12]
	cmp	r3, #1
	bne	.L4
.L3:
	ldr	r3, .L7+4
	ldr	r3, [r4, r3]
	mov	r2, r3
	ldr	r3, [r7, #12]
	ldr	r3, [r2, r3, lsl #2]
	mov	r1, r3
	ldr	r3, .L7+8
.LPIC1:
	add	r3, pc
	mov	r0, r3
	bl	printf(PLT)
	b	.L5
.L4:
	ldr	r3, [r7, #12]
	subs	r3, r3, #1
	ldr	r2, .L7+4
	ldr	r2, [r4, r2]
	ldr	r2, [r2, r3, lsl #2]
	ldr	r3, [r7, #12]
	subs	r3, r3, #2
	ldr	r1, .L7+4
	ldr	r1, [r4, r1]
	ldr	r3, [r1, r3, lsl #2]
	add	r2, r2, r3
	ldr	r3, .L7+4
	ldr	r3, [r4, r3]
	mov	r1, r3
	ldr	r3, [r7, #12]
	str	r2, [r1, r3, lsl #2]
	ldr	r3, .L7+4
	ldr	r3, [r4, r3]
	mov	r2, r3
	ldr	r3, [r7, #12]
	ldr	r3, [r2, r3, lsl #2]
	mov	r1, r3
	ldr	r3, .L7+12
.LPIC2:
	add	r3, pc
	mov	r0, r3
	bl	printf(PLT)
.L5:
	ldr	r3, [r7, #12]
	adds	r3, r3, #1
	str	r3, [r7, #12]
.L2:
	ldr	r2, [r7, #12]
	ldr	r3, [r7, #4]
	cmp	r2, r3
	blt	.L6
	nop
	nop
	adds	r7, r7, #20
	mov	sp, r7
	@ sp needed
	pop	{r4, r7, pc}
.L8:
	.align	2
.L7:
	.word	_GLOBAL_OFFSET_TABLE_-(.LPIC0+4)
	.word	fibo(GOT)
	.word	.LC0-(.LPIC1+4)
	.word	.LC0-(.LPIC2+4)
	.size	myfibo, .-myfibo
	.section	.rodata
	.align	2
.LC1:
	.ascii	"%d\000"
	.align	2
.LC2:
	.ascii	"\345\206\205\345\255\230erro\357\274\201\357\274\201"
	.ascii	"\357\274\201\000"
	.text
	.align	1
	.global	main
	.syntax unified
	.thumb
	.thumb_func
	.fpu vfpv3-d16
	.type	main, %function
main:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{r3, r4, r7, lr}
	add	r7, sp, #0
	ldr	r4, .L13
.LPIC3:
	add	r4, pc
	ldr	r3, .L13+4
	ldr	r3, [r4, r3]
	mov	r1, r3
	ldr	r3, .L13+8
.LPIC4:
	add	r3, pc
	mov	r0, r3
	bl	__isoc99_scanf(PLT)
	ldr	r3, .L13+4
	ldr	r3, [r4, r3]
	ldr	r3, [r3]
	cmp	r3, #0
	ble	.L10
	ldr	r3, .L13+4
	ldr	r3, [r4, r3]
	ldr	r3, [r3]
	cmp	r3, #100
	ble	.L11
.L10:
	ldr	r3, .L13+12
.LPIC5:
	add	r3, pc
	mov	r0, r3
	bl	printf(PLT)
	movs	r3, #0
	b	.L12
.L11:
	ldr	r3, .L13+4
	ldr	r3, [r4, r3]
	ldr	r3, [r3]
	mov	r0, r3
	bl	myfibo(PLT)
	movs	r3, #0
.L12:
	mov	r0, r3
	pop	{r3, r4, r7, pc}
.L14:
	.align	2
.L13:
	.word	_GLOBAL_OFFSET_TABLE_-(.LPIC3+4)
	.word	n(GOT)
	.word	.LC1-(.LPIC4+4)
	.word	.LC2-(.LPIC5+4)
	.size	main, .-main
	.ident	"GCC: (Ubuntu 9.4.0-1ubuntu1~20.04.1) 9.4.0"
	.section	.note.GNU-stack,"",%progbits
