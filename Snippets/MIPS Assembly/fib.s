.globl __start

__start:
	addi $t0, $0, 0x0A   # n, the input
	addi $t1, $0, 0x03   # i, the counter
	addi $t2, $0, 0x01   # a
	addi $t3, $0, 0x01   # b
	add  $t4, $0, $0     # sum
	add  $t5, $0, $0     # result

fibonacci:
	sub  $t6, $t0, $t1   # t6 = n - i
	blez $t6, finish     # if t6 neg (i > n) finish
	add  $t4, $t2, $t3   # sum = a + b
	andi $t6, $t1, 0x01  # t6 = i & 1
	beq  $t6, $0, fib_ie # goto fib_ie if i is even
	add  $t2, $t4, $0    # a = sum
	j    fib_inc
	
fib_ie:
	add  $t3, $t4, $0    # b = sum
	
fib_inc:
	addi $t1, $t1, 0x01  # increment i
	j    fibonacci

finish:
	add  $t5, $t2, $t3
	nop