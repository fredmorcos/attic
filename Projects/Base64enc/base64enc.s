# Fred Morcos <fred.morcos@gmail.com>

.section .data

table:
	.ascii "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="

.equ FILE_OPEN, 5
.equ FILE_WRITE, 4
.equ FILE_READ, 3
.equ FILE_CLOSE, 6

.equ FILE_READ_ONLY, 0
.equ FILE_CREATE_WRITE_TRUNCATE, 03101

.equ FILE_EOF, 0
.equ FILE_PERM, 0666

.equ SYSCALL, 0x80
.equ SYSCALL_EXIT, 1

.equ STACK_SIZE_RESERVE, 8
.equ STACK_FILE_IN, 0
.equ STACK_FILE_OUT, 4
.equ STACK_ARGC, 8
.equ STACK_ARGV_0_PROGRAM_NAME, 12
.equ STACK_ARGV_1_INPUT_FILE, 16
.equ STACK_ARGV_2_OUTPUT_FILE, 20

.equ PROGRAM_SUCCESS, 0
.equ PROGRAM_ERR_ARGS, 1
.equ PROGRAM_ERR_READ, 2
.equ PROGRAM_ERR_WRITE, 3
.equ PROGRAM_ERR_UNKNOWN, 4

.section .bss
.equ IBUFFER_SIZE, 3
.lcomm IBUFFER, IBUFFER_SIZE

.equ OBUFFER_SIZE, 4
.lcomm OBUFFER, OBUFFER_SIZE

.section .text
.globl _start

_start:
# reserve space on the stack for
# the file descriptors
	subl $STACK_SIZE_RESERVE, %esp
	movl %esp, %ebp

# check the arguments
	movl STACK_ARGC(%ebp), %edi
	cmpl $3, %edi
	je main_loop

# exit(1) if argv != 3
	movl $PROGRAM_ERR_ARGS, %ebx
	jmp exit

main_loop:
# open the input file for reading
	movl STACK_ARGV_1_INPUT_FILE(%ebp), %ebx
	movl $FILE_READ_ONLY, %ecx
	movl $FILE_PERM, %edx
	movl $FILE_OPEN, %eax
	int $SYSCALL

# check that %eax > 0
	cmpl $0, %eax
	jg save_and_open_output

# close input file and exit(2) if %eax < 0
	jmp read_error

save_and_open_output:
# save file descriptor in stack
	movl %eax, STACK_FILE_IN(%ebp)

# open the output file for writing
	movl STACK_ARGV_2_OUTPUT_FILE(%ebp), %ebx
	movl $FILE_CREATE_WRITE_TRUNCATE, %ecx
	movl $FILE_PERM, %edx
	movl $FILE_OPEN, %eax
	int $SYSCALL

# check that %eax > 0
	cmpl $0, %eax
	jg save_and_read_input

# close input and output file and
# exit(3) if %eax < 0
	jmp write_error

save_and_read_input:
# save file descriptor in stack
	movl %eax, STACK_FILE_OUT(%ebp)

start_reading_and_writing:
# start reading from file
	movl STACK_FILE_IN(%ebp), %ebx
	movl $IBUFFER, %ecx
	movl $IBUFFER_SIZE, %edx
	movl $FILE_READ, %eax
	int $SYSCALL

# if nothing was read then finish
	cmpl $FILE_EOF, %eax
	je done_with_everything
	jl read_error

encode:
# insert padding bits of zeros
	cmpl $2, %eax
	jg start_encode

	cmpl $1, %eax
	jg pad_2_bytes

# pad 1 byte
	movl $2, %edi
	movb $0, IBUFFER(, %edi, 1)
	movl $1, %edi
	movb $0, IBUFFER(, %edi, 1)

pad_2_bytes:
	movl $2, %edi
	movb $0, IBUFFER(, %edi, 1)
	jmp start_encode

start_encode:
# put what's in the input buffer in %ecx
	movl $0, %edi
	movb IBUFFER(, %edi, 1), %ch
	movl $1, %edi
	movb IBUFFER(, %edi, 1), %cl
	shll $8, %ecx
	movl $2, %edi
	movb IBUFFER(, %edi, 1), %cl

# encode all the 6-bits
	movl %ecx, %ebx
	andl $0b00000000111111000000000000000000, %ebx
	shrl $18, %ebx
	andl $0b00000000000000000000000000111111, %ebx
	movb table(, %ebx, 1), %dl
	movl $0, %edi
	movb %dl, OBUFFER(, %edi, 1)

	movl %ecx, %ebx
	andl $0b00000000000000111111000000000000, %ebx
	shrl $12, %ebx
	andl $0b00000000000000000000000000111111, %ebx
	movb table(, %ebx, 1), %dl
	movl $1, %edi
	movb %dl, OBUFFER(, %edi, 1)

	movl %ecx, %ebx
	andl $0b00000000000000000000111111000000, %ebx
	shrl $6, %ebx
	andl $0b00000000000000000000000000111111, %ebx
	movb table(, %ebx, 1), %dl
	movl $2, %edi
	movb %dl, OBUFFER(, %edi, 1)

	movl %ecx, %ebx
	andl $0b00000000000000000000000000111111, %ebx
	shrl $0, %ebx
	andl $0b00000000000000000000000000111111, %ebx
	movb table(, %ebx, 1), %dl
	movl $3, %edi
	movb %dl, OBUFFER(, %edi, 1)

# insert padding bytes of '='
	cmpl $2, %eax
	jg write_to_buffer

	cmpl $1, %eax
	jg pad_2_bytes_2

# pad 1 byte
	movl $3, %edi
	movl $64, %edx
	movb table(, %edx, 1), %dl
	movb %dl, OBUFFER(, %edi, 1)
	movl $2, %edi
	movl $64, %edx
	movb table(, %edx, 1), %dl
	movb %dl, OBUFFER(, %edi, 1)
	jmp write_to_buffer

pad_2_bytes_2:
	movl $3, %edi
	movl $64, %edx
	movb table(, %edx, 1), %dl
	movb %dl, OBUFFER(, %edi, 1)

write_to_buffer:
	movl $OBUFFER_SIZE, %edx
	call flush_buffer_to_file

	addl $4, %esi

	cmpl $76, %esi
	jne start_reading_and_writing

# write the end of line character to the end of the current line
	movl $0, %edi
	movb $10, OBUFFER(, %edi, 1)
	movl $1, %edx
	call flush_buffer_to_file

	movl $0, %esi
	jmp start_reading_and_writing

done_with_everything:
# write the end of line character to the end of the file
# only if the last line is less than 76 chars long
	cmpl $0, %esi	# compare with 0 because we reset
					# the line length counter
	je exit_success

	movl $0, %edi
	movb $10, OBUFFER(, %edi, 1)
	movl $1, %edx		# size from buffer to write
	call flush_buffer_to_file
	jmp exit_success

.type flush_buffer_to_file, @function
flush_buffer_to_file:
	movl STACK_FILE_OUT(%ebp), %ebx
	movl $OBUFFER, %ecx
	movl $FILE_WRITE, %eax
	int $SYSCALL

	cmpl $0, %eax
	jl write_error
	ret

.type close_input_file, @function
close_input_file:
	movl $FILE_CLOSE, %eax
	movl STACK_ARGV_1_INPUT_FILE(%ebp), %ebx
	int $SYSCALL
	ret

.type close_output_file, @function
close_output_file:
	movl $FILE_CLOSE, %eax
	movl STACK_ARGV_2_OUTPUT_FILE(%ebp), %ebx
	int $SYSCALL
	ret

read_error:
	call close_input_file
	movl $PROGRAM_ERR_READ, %ebx
	jmp exit

write_error:
	call close_input_file
	call close_output_file
	movl $PROGRAM_ERR_WRITE, %ebx
	jmp exit

exit_success:
	call close_input_file
	call close_output_file
	movl $PROGRAM_SUCCESS, %ebx
	jmp exit

unrecoverable_error:
	call close_input_file
	call close_output_file
	movl $PROGRAM_ERR_UNKNOWN, %ebx
	jmp exit

exit:
	movl $SYSCALL_EXIT, %eax
	int $SYSCALL
