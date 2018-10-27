# some conventions
# 	s3 is matrix size
	
# 	s0 is i counter
# 	s1 is j counter
# 	s2 is k counter

	.data
mat1:	.word 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16
mat2:	.word 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16
mat3:	.word 0,0,0,0,0,0,0,0,0, 0, 0, 0, 0, 0, 0, 0
	
	.text
	.globl __start

__start:
	li    $s3, 4		    # s3 = matrix size
	addi  $a3, $s3, -1  # a3 = matrix size - 1
	li    $s0, -1		    # i = -1
	li    $s1, -1		    # j = -1
	li    $s2, -1		    # k = -1

	la    $t1, mat1
	la    $t2, mat2
	la    $t3, mat3

	li    $a1, 4

s_mul:
	sub   $s4, $s0, $a3	   # s4 = i - (matsize - 1)
	beq   $s4, $0, finish	 # if i - (matsize - 1) == 0 -> finish

	addi  $s0, $s0, 1	 # i++
	li    $s1, -1		   # j = -1
	
i_loop:
	sub   $s4, $s1, $a3	  # s4 = j - (matsize - 1)
	beq   $s4, $0, s_mul	# if j - (matsize - 1) == 0 -> loop i

	addi  $s1, $s1, 1	  # j++
	li    $s2, -1	      # k = -1

j_loop_body:
	sub   $s4, $s2, $a3	   # s4 = k - (matsize - 1)
	beq   $s4, $0, i_loop	 # if k - (matsize - 1) == 0 -> loop j

	addi  $s2, $s2, 1	 # k++

body:
	mul   $s5, $s2, $s3	 # s5 = k * matsize
	add   $s5, $s5, $s1	 # s5 = k * matsize + j
	mul   $s5, $s5, $a1	 # s5 *= 4 (to have it in words)

	add   $t0, $s5, $t1 	# t0 = index in mat1
	lw    $t4, 0($t0)	    # t4 = value of index in mat1

	mul   $s5, $s0, $s3	 # s5 = i * matsize
	add   $s5, $s5, $s2	 # s5 = i * matsize + k
	mul   $s5, $s5, $a1	 # s5 *= 4 (to have it in words)

	add   $t0, $s5, $t2	 # t0 = index in mat2
	lw    $t5, 0($t0)	   # t5 = value of index in mat2

	mul   $a0, $s0, $s3	 # a0 = i * matsize
	add   $a0, $a0, $s1	 # a0 = i * matsize + j
	mul   $a0, $a0, $a1	 # a0 *= 4 (to have it in words)

	add   $a0, $a0, $t3	 # a0 = index in mat3
	lw    $t6, 0($a0)	   # t6 = value of index in mat3

	mul   $t4, $t4, $t5	 # t4 *= t5
	add   $t6, $t6, $t4	 # t6 += t4

	sw    $t6, 0($a0)	   # save back
	j     j_loop_body
	
finish:
	nop
