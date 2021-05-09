	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 10, 15
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	pushq	%r15
	.cfi_def_cfa_offset 24
	pushq	%r14
	.cfi_def_cfa_offset 32
	pushq	%rbx
	.cfi_def_cfa_offset 40
	pushq	%rax
	.cfi_def_cfa_offset 48
	.cfi_offset %rbx, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	.cfi_offset %rbp, -16
	movl	$1, %edi
	movl	$10, %esi
	callq	_range
	movl	(%rax), %ebp
	movq	8(%rax), %r15
	movl	$0, (%rsp)
	leaq	L_intfmt(%rip), %r14
	cmpl	%ebp, (%rsp)
	jge	LBB0_3
	.p2align	4, 0x90
LBB0_2:                                 ## %for_body
                                        ## =>This Inner Loop Header: Depth=1
	movslq	(%rsp), %rbx
	movl	(%r15,%rbx,4), %esi
	movl	%esi, 4(%rsp)
	movq	%r14, %rdi
	xorl	%eax, %eax
	callq	_printf
	leal	1(%rbx), %eax
	movl	%eax, (%rsp)
	cmpl	%ebp, (%rsp)
	jl	LBB0_2
LBB0_3:                                 ## %merge
	xorl	%eax, %eax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_concat                         ## -- Begin function concat
	.p2align	4, 0x90
_concat:                                ## @concat
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	pushq	%rax
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	movq	%rsi, %r15
	movq	%rdi, %r12
	movl	$16, %edi
	callq	_malloc
	movq	%rax, %r14
	movl	(%r12), %ebx
	movl	(%r15), %r13d
	movq	8(%r12), %r12
	movq	8(%r15), %r15
	leal	(%rbx,%r13), %eax
	movl	%eax, -48(%rbp)                 ## 4-byte Spill
	movl	%eax, (%r14)
	leal	1(%rbx,%r13), %edi
	callq	_malloc
	movl	$0, -44(%rbp)
	cmpl	%ebx, -44(%rbp)
	jge	LBB1_3
	.p2align	4, 0x90
LBB1_2:                                 ## %for_body
                                        ## =>This Inner Loop Header: Depth=1
	movslq	-44(%rbp), %rcx
	movzbl	(%r12,%rcx), %edx
	movb	%dl, (%rax,%rcx)
	incl	%ecx
	movl	%ecx, -44(%rbp)
	cmpl	%ebx, -44(%rbp)
	jl	LBB1_2
LBB1_3:                                 ## %merge
	movq	%rsp, %rdx
	leaq	-16(%rdx), %rcx
	movq	%rcx, %rsp
	movl	$0, -16(%rdx)
	cmpl	%r13d, (%rcx)
	jge	LBB1_6
	.p2align	4, 0x90
LBB1_5:                                 ## %for_body12
                                        ## =>This Inner Loop Header: Depth=1
	movslq	-44(%rbp), %rdx
	movslq	(%rcx), %rsi
	movzbl	(%r15,%rsi), %ebx
	movb	%bl, (%rax,%rdx)
	incl	%edx
	movl	%edx, -44(%rbp)
	leal	1(%rsi), %edx
	movl	%edx, (%rcx)
	cmpl	%r13d, (%rcx)
	jl	LBB1_5
LBB1_6:                                 ## %merge22
	movslq	-48(%rbp), %rcx                 ## 4-byte Folded Reload
	movb	$0, (%rax,%rcx)
	movq	%rax, 8(%r14)
	movq	%r14, %rax
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_range                          ## -- Begin function range
	.p2align	4, 0x90
_range:                                 ## @range
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r14
	pushq	%rbx
	subq	$16, %rsp
	.cfi_offset %rbx, -32
	.cfi_offset %r14, -24
	movl	%esi, %ebx
	movl	%edi, %r14d
	movl	$16, %edi
	callq	_malloc
	subl	%r14d, %ebx
	incl	%ebx
	leaq	15(,%rbx,4), %rdx
	andq	$-16, %rdx
	movq	%rsp, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rsp
	movl	$0, -20(%rbp)
	movl	%r14d, -24(%rbp)
	cmpl	%ebx, -20(%rbp)
	jge	LBB2_3
	.p2align	4, 0x90
LBB2_2:                                 ## %while_body
                                        ## =>This Inner Loop Header: Depth=1
	movslq	-20(%rbp), %rdx
	movl	-24(%rbp), %esi
	movl	%esi, (%rcx,%rdx,4)
	incl	%esi
	movl	%esi, -24(%rbp)
	incl	%edx
	movl	%edx, -20(%rbp)
	cmpl	%ebx, -20(%rbp)
	jl	LBB2_2
LBB2_3:                                 ## %merge
	movl	%ebx, (%rax)
	movq	%rcx, 8(%rax)
	leaq	-16(%rbp), %rsp
	popq	%rbx
	popq	%r14
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
L_strfmt:                               ## @strfmt
	.asciz	"%s\n"

L_intfmt:                               ## @intfmt
	.asciz	"%d\n"

L_charfmt:                              ## @charfmt
	.asciz	"%c\n"

.subsections_via_symbols
