.data 
n:
    .word   0
	.text
    .global fibo
    .bss
    .align 2
    .size fibo, 400
@fibo   int[100]
fibo:
    .space 400

@str输入输出的.ascii 
    .section .rodata
    .align 2
str0: 
    .ascii "%d,"
	.align 2

str1:
	.ascii	"%d"
str2:
    .ascii	"\345\206\205\345\255\230erro\357\274\201\357\274\201\357\274\201\000"
    .global main 
    .global myfibo
    .text 

main:  
    push {lr}
    ldr r1,=n
	ldr r0,=str1
    @scanf("%d",&n)
	bl  __isoc99_scanf
    @r10 用于存储n
	ldr r10,=n
    ldr r10,[r10]
    cmp r10,#100
    @n>100超出内存
    bgt Erro
    cmp r10,#0
    @n<=0 访问错误
    bls Erro
    bl myfibo
    pop {pc}

myfibo: @void myfibo(int n);
    push {lr}
    @r11存fibo的地址
    ldr r11,_bridge+8
    @r4 int i=0
    mov r4,#0
    @r5 中间变量计算结果 r5=1
    mov r5,#1
    @fib[0]=1,fib[1]=1
    str r5,[r11,r4,LSL #2]
    @ i=1
    add r4,r4,#1
    str r5,[r11,r4,LSL #2]
    mov r4,#0


LOOP:
    mov r9,#1
    @i=0时直接输出，fibo[0]
    cmp r4,#0
    beq L1
    @i=1时直接输出，fibo[1]
    cmp r4,#1
    beq L1
    @r5=i-1,r6=i-2
    sub r5,r4,#1
    sub r6,r4,#2
    ldr r7,[r11,r5,LSL #2]
    ldr r8,[r11,r6,LSL #2]
    @r9=fibo[i-1]+fibo[i-2]
    add r9,r7,r8
    str r9,[r11,r4,LSL #2]
    @打印   

L1:
    ldr r0,_bridge+4
    @printf("%d,"@fib[i])
    mov r1,r9
    bl printf
    @i++
    add r4,r4,#1
    @判断i<n
    cmp r4,r10
    beq END
    b LOOP
Erro:
    ldr r0,_bridge+16
    bl printf

END:
    pop {pc}

_bridge:
    .word   n
    .word   str0
    .word   fibo
	.word 	str1
    .word   str2