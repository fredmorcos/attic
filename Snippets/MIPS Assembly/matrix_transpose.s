	.data
	 # matrix width = height = 4
mat:	.word 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16
	
	.text
	.globl __start

__start:
	j    trans

trans:
	addi $s0, $0, 0               # i = 0
i_loop:			             
	addi $s4, $s0, -4             # finished height?
	bgez $s4, finish              # yes -> finish
			             
	add  $s1, $s0, 1              # j = i + 1
j_loop:			             
	addi $s4, $s1, -4             # finished width?
	bltz $s4, body                # yes -> i loop
	addi $s0, $s0, 1              # i++
	j    i_loop

body:
	add  $a0, $0, $s0             # set i = loop_i (s0)
	add  $a1, $0, $s1             # set j = loop_j (s1)
	jal  get_val_at_index         # call get_val_at_index(i, j)
	add  $s2, $0, $v0             # s2 = ret val
			             
	add  $a0, $0, $s1             # set i = loop_j (s1)
	add  $a1, $0, $s0             # set j = loop_i (s0)
	jal  get_val_at_index         # call get_val_at_index(j, i)
	add  $s3, $0, $v0             # s3 = ret val
			             
	# START OF SWAP	             
			             
	add  $a0, $0, $s0             # set i = loop_i (s0)
	add  $a1, $0, $s1             # set j = loop_j (s1)
	add  $a2, $0, $s3            
	jal  set_val_at_index         # call set_val_at_index(i, j)
			             
	add  $a0, $0, $s1             # set i = loop_j (s1)
	add  $a1, $0, $s0             # set j = loop_i (s0)
	add  $a2, $0, $s2            
	jal  set_val_at_index         # call get_val_at_index(j, i)
			             
	# END OF SWAP	             
			             
	addi $s1, $s1, 1              # j++
	j    j_loop	             
			             
	j    finish                   # should never get here
			             
set_val_at_index:	             
	addi $sp, $sp, -16            # alloc stack frame
	sw   $ra, 0($sp)              # push return address
	sw   $a0, 4($sp)              # push argument 0 (i)
	sw   $a1, 8($sp)              # push argument 1 (j)
	sw   $a2, 12($sp)             # push argument 2 (val)
			             
	addi $t0, $0, 4               # t0 = matrix width
	mul  $t1, $t0, $a0            # t1 = i * width
	add  $t1, $t1, $a1            # t1 = i * width + j
			             
	la   $t2, mat                 # t2 = matrix addr
	addi $t3, $0, 4               # mul by 4, to have it in words
	mul  $t0, $t1, $t3           
	add  $t0, $t0, $t2            # t0 = addr of index
	sw   $a2, 0($t0)              # put value at mat[i, j]
			             
	lw   $a2, 12($sp)             # pop arg 2 (val)
	lw   $a1, 8($sp)              # pop arg 1 (j)
	lw   $a0, 4($sp)              # pop arg 0 (i)
	lw   $ra, 0($sp)              # pop return address
	addi $sp, $sp, 16             # dealloc stack frame
	jr   $ra                      # return
			             
get_val_at_index:	             
	addi $sp, $sp, -12            # alloc stack frame
	sw   $ra, 0($sp)              # push return address
	sw   $a0, 4($sp)              # push argument 0 (i)
	sw   $a1, 8($sp)              # push argument 1 (j)
			             
	addi $t0, $0, 4               # t0 = matrix width
	mul  $t1, $t0, $a0            # t1 = i * width
	add  $t1, $t1, $a1            # t1 = i * width + j
			             
	la   $t2, mat                 # t2 = matrix addr
	addi $t3, $0, 4               # mul by 4, to have it in words
	mul  $t0, $t1, $t3           
	add  $t0, $t0, $t2            # t0 = addr of index
	lw   $v0, 0($t0)              # retval = value at addr t0
			             
	lw   $a1, 8($sp)              # pop arg 1
	lw   $a0, 4($sp)              # pop arg 0
	lw   $ra, 0($sp)              # pop return address
	addi $sp, $sp, 12             # dealloc stack frame
	jr   $ra                      # return

finish:
	nop
