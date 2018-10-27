.globl __start

__start:
	addi $a0, $0, 0x0A  # fac(10)
	jal  factorial      # result should be in $v0
	j    finish
	
factorial:
	addi $sp, $sp, -8   # allocate stack (downwards)
	sw   $ra, 0($sp)    # push return addr
	sw   $a0, 4($sp)    # push argument

	addi $t0, $0, 0x02  # t0 = 2
	slt  $t0, $a0, $t0  # t0 = 1 if a0 < t0
	beq  $t0, $0, facb  # go to fac body if n >= 2
	addi $sp, $sp, 8
	addi $v0, $0, 0x01  # put 1 in return value
	jr   $ra
	
facb:	
	addi $a0, $a0, -1   # n - 1
	jal  factorial
	
	lw   $a0, 4($sp)    # pop argument
	lw   $ra, 0($sp)    # pop return addr
	addi $sp, $sp, 8    # deallocate stack (upwards)

	# mult $a0, $v0       # n * fac(n - 1)
	# mflo $v0            # mult result in LO reg, this is buggy

	mul  $v0, $v0, $a0

	jr   $ra            # return

finish:
	nop