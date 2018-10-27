.globl __start

__start:
	addi $t0, $0, 0x14	# t0 = 20
	addi $t1, $0, 0x02	# t1 = 2
	add  $t2, $0, $0    # t2 = 0, will be the result
	add  $t3, $0, $t0	  # t3 = t0, will be the remainder

division:
	sub  $t4, $t3, $t1   # t4 = t3 - t1
	bltz $t4, finish     # if t4 is negative or zero then finish
	add  $t3, $0, $t4	   # set t3 to the remainder
	addi $t2, $t2, 0x01  # increment the result
	j    division
	
finish:
	nop
