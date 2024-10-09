
	.globl simple
simple:
	# Function Preamble
	pushq %rbp
	movq %rsp, %rbp

	# Allocating stack of size 12
	subq $48, %rsp

	movl %edi, -4(%rbp)
	movl %esi, -8(%rbp)
	movl %edx, -12(%rbp)
	movl %ecx, -16(%rbp)
	movl %r8d, -20(%rbp)
	movl %r9d, -24(%rbp)
	movl -4(%rbp), %r10d
	movl %r10d, -28(%rbp)
	movl -8(%rbp), %r10d
	subl %r10d, -28(%rbp)
	movl -28(%rbp), %r10d
	movl %r10d, -32(%rbp)
	movl -12(%rbp), %r10d
	subl %r10d, -32(%rbp)
	movl -32(%rbp), %r10d
	movl %r10d, -36(%rbp)
	movl -16(%rbp), %r10d
	subl %r10d, -36(%rbp)
	movl -36(%rbp), %r10d
	movl %r10d, -40(%rbp)
	movl -20(%rbp), %r10d
	subl %r10d, -40(%rbp)
	movl -40(%rbp), %r10d
	movl %r10d, -44(%rbp)
	movl -24(%rbp), %r10d
	subl %r10d, -44(%rbp)
	movl -44(%rbp), %eax
	movq %rbp, %rsp
	popq %rbp
	ret

	.globl main
main:
	# Function Preamble
	pushq %rbp
	movq %rsp, %rbp

	# Allocating stack of size 4
	subq $16, %rsp

	movl $30, %edi
	movl $6, %esi
	movl $5, %edx
	movl $4, %ecx
	movl $3, %r8d
	movl $2, %r9d
	call simple@PLT
	movl %eax, -4(%rbp)
	movl -4(%rbp), %eax
	movq %rbp, %rsp
	popq %rbp
	ret

.section .note.GNU-stack,"",@progbits
