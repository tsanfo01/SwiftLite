	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 10, 15
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movl	$5, %edi
	movl	$3, %esi
	callq	_Fraction0
	movq	%rax, %rbx
	movl	$6, %edi
	movl	$3, %esi
	callq	_Fraction0
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_addFrac
	movq	%rax, %rbx
	movq	%rax, _x(%rip)
	movl	$1, %edi
	movl	$1, %esi
	callq	_Fraction0
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_subFrac
	movq	%rax, _y(%rip)
	movq	_x(%rip), %rdi
	callq	*56(%rdi)
	movq	_y(%rip), %rdi
	callq	*56(%rdi)
	xorl	%eax, %eax
	popq	%rbx
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
	movl	%esi, %ebx
	movl	%edi, %r15d
	movl	$16, %edi
	callq	_malloc
	movq	%rax, %r14
	subl	%r15d, %ebx
	movl	%ebx, %ebp
	incl	%ebp
	leal	4(,%rbx,4), %edi
	callq	_malloc
	movl	$0, (%rsp)
	movl	%r15d, 4(%rsp)
	cmpl	%ebp, (%rsp)
	jge	LBB2_3
	.p2align	4, 0x90
LBB2_2:                                 ## %while_body
                                        ## =>This Inner Loop Header: Depth=1
	movslq	(%rsp), %rcx
	movl	4(%rsp), %edx
	movl	%edx, (%rax,%rcx,4)
	incl	%edx
	movl	%edx, 4(%rsp)
	incl	%ecx
	movl	%ecx, (%rsp)
	cmpl	%ebp, (%rsp)
	jl	LBB2_2
LBB2_3:                                 ## %merge
	movl	%ebp, (%r14)
	movq	%rax, 8(%r14)
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_gcd                            ## -- Begin function gcd
	.p2align	4, 0x90
_gcd:                                   ## @gcd
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movl	%edi, -8(%rbp)
	movl	%esi, -4(%rbp)
	testl	%esi, %esi
	je	LBB3_1
## %bb.3:                               ## %else
	movl	-8(%rbp), %eax
	xorl	%edx, %edx
	divl	-4(%rbp)
	movq	%rsp, %rax
	leaq	-16(%rax), %rsp
	movl	%edx, -16(%rax)
	movl	-4(%rbp), %edi
	movl	%edx, %esi
	callq	_gcd
	jmp	LBB3_2
LBB3_1:                                 ## %then
	movl	-8(%rbp), %eax
LBB3_2:                                 ## %then
	movq	%rbp, %rsp
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_lcm                            ## -- Begin function lcm
	.p2align	4, 0x90
_lcm:                                   ## @lcm
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -16
	movl	%edi, %ebx
	movl	%edi, 12(%rsp)
	movl	%esi, 8(%rsp)
	callq	_gcd
	movl	%eax, %ecx
	movl	%ebx, %eax
	cltd
	idivl	%ecx
	imull	8(%rsp), %eax
	addq	$16, %rsp
	popq	%rbx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_setNum                         ## -- Begin function setNum
	.p2align	4, 0x90
_setNum:                                ## @setNum
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	movl	%esi, -12(%rsp)
	movl	%esi, (%rdi)
	xorl	%eax, %eax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_setDen                         ## -- Begin function setDen
	.p2align	4, 0x90
_setDen:                                ## @setDen
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	movl	%esi, -12(%rsp)
	movl	%esi, 4(%rdi)
	xorl	%eax, %eax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_getNum                         ## -- Begin function getNum
	.p2align	4, 0x90
_getNum:                                ## @getNum
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	movl	(%rdi), %eax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_getDen                         ## -- Begin function getDen
	.p2align	4, 0x90
_getDen:                                ## @getDen
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	movl	4(%rdi), %eax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_negate                         ## -- Begin function negate
	.p2align	4, 0x90
_negate:                                ## @negate
	.cfi_startproc
## %bb.0:                               ## %entry
	movq	%rdi, -8(%rsp)
	negl	(%rdi)
	movq	-8(%rsp), %rax
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_reduce                         ## -- Begin function reduce
	.p2align	4, 0x90
_reduce:                                ## @reduce
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	pushq	%rax
	.cfi_offset %rbx, -24
	movq	%rdi, -16(%rbp)
	cmpl	$0, (%rdi)
	je	LBB10_1
## %bb.2:                               ## %else
	movq	%rdi, %rbx
	movl	(%rdi), %edi
	movl	4(%rbx), %esi
	callq	_gcd
	movl	%eax, %ecx
	movq	%rsp, %rsi
	leaq	-16(%rsi), %rsp
	movl	%eax, -16(%rsi)
	movl	(%rbx), %eax
	cltd
	idivl	%ecx
	movq	-16(%rbp), %rcx
	movl	%eax, (%rcx)
	movl	4(%rbx), %eax
	cltd
	idivl	-16(%rsi)
	movq	-16(%rbp), %rcx
	movl	%eax, 4(%rcx)
	jmp	LBB10_3
LBB10_1:                                ## %then
	movq	-16(%rbp), %rax
	movl	$1, 4(%rax)
LBB10_3:                                ## %merge
	movq	-16(%rbp), %rax
	leaq	-8(%rbp), %rsp
	popq	%rbx
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_print                          ## -- Begin function print
	.p2align	4, 0x90
_print:                                 ## @print
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%r14
	.cfi_def_cfa_offset 16
	pushq	%rbx
	.cfi_def_cfa_offset 24
	pushq	%rax
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -24
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	%rdi, (%rsp)
	movl	(%rdi), %esi
	leaq	L_intfmt(%rip), %r14
	movq	%r14, %rdi
	xorl	%eax, %eax
	callq	_printf
	movl	$16, %edi
	callq	_malloc
	movl	$1, (%rax)
	leaq	L_lit(%rip), %rsi
	movq	%rsi, 8(%rax)
	leaq	L_strfmt(%rip), %rdi
	xorl	%eax, %eax
	callq	_printf
	movl	4(%rbx), %esi
	movq	%r14, %rdi
	xorl	%eax, %eax
	callq	_printf
	xorl	%eax, %eax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_Fraction0                      ## -- Begin function Fraction0
	.p2align	4, 0x90
_Fraction0:                             ## @Fraction0
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -16
	movl	%edi, 12(%rsp)
	movl	%esi, 8(%rsp)
	movl	$64, %edi
	callq	_malloc
	movq	%rax, %rbx
	movq	$0, (%rax)
	leaq	_setNum(%rip), %rax
	movq	%rax, 8(%rbx)
	leaq	_setDen(%rip), %rax
	movq	%rax, 16(%rbx)
	leaq	_getNum(%rip), %rax
	movq	%rax, 24(%rbx)
	leaq	_getDen(%rip), %rax
	movq	%rax, 32(%rbx)
	leaq	_negate(%rip), %rax
	movq	%rax, 40(%rbx)
	leaq	_reduce(%rip), %rax
	movq	%rax, 48(%rbx)
	leaq	_print(%rip), %rax
	movq	%rax, 56(%rbx)
	movl	$8, %edi
	callq	_malloc
	movq	%rbx, (%rax)
	cmpl	$0, 8(%rsp)
	js	LBB12_1
## %bb.2:                               ## %else
	movl	12(%rsp), %ecx
	movq	(%rax), %rdx
	movl	%ecx, (%rdx)
	movl	8(%rsp), %ecx
	movq	(%rax), %rax
	movl	%ecx, 4(%rax)
	jmp	LBB12_3
LBB12_1:                                ## %then
	xorl	%ecx, %ecx
	xorl	%edx, %edx
	subl	8(%rsp), %edx
	movq	(%rax), %rsi
	movl	%edx, 4(%rsi)
	subl	12(%rsp), %ecx
	movq	(%rax), %rax
	movl	%ecx, (%rax)
LBB12_3:                                ## %merge
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_addFrac                        ## -- Begin function addFrac
	.p2align	4, 0x90
_addFrac:                               ## @addFrac
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
	movq	%rdi, 32(%rsp)
	movq	%rsi, 24(%rsp)
	callq	*24(%rdi)
	movl	%eax, 20(%rsp)
	movq	32(%rsp), %rdi
	callq	*32(%rdi)
	movl	%eax, 8(%rsp)
	movq	24(%rsp), %rdi
	callq	*24(%rdi)
	movl	%eax, 16(%rsp)
	movq	24(%rsp), %rdi
	callq	*32(%rdi)
	movl	%eax, 12(%rsp)
	movl	8(%rsp), %esi
	movl	%eax, %edi
	callq	_lcm
	movl	%eax, %esi
	movl	%eax, 44(%rsp)
	cltd
	idivl	12(%rsp)
	movl	%eax, %ecx
	imull	16(%rsp), %ecx
	movl	%esi, %eax
	cltd
	idivl	8(%rsp)
	imull	20(%rsp), %eax
	addl	%ecx, %eax
	movl	%eax, 40(%rsp)
	movl	%eax, %edi
	callq	_Fraction0
	movq	%rax, 48(%rsp)
	movq	%rax, %rdi
	callq	*48(%rax)
	addq	$56, %rsp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_subFrac                        ## -- Begin function subFrac
	.p2align	4, 0x90
_subFrac:                               ## @subFrac
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	%rdi, 8(%rsp)
	movq	%rsi, (%rsp)
	movq	%rsi, %rdi
	callq	*40(%rsi)
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_addFrac
	addq	$16, %rsp
	popq	%rbx
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

L_lit:                                  ## @lit
	.asciz	"/"

	.globl	_x                              ## @x
.zerofill __DATA,__common,_x,8,3
	.globl	_y                              ## @y
.zerofill __DATA,__common,_y,8,3
.subsections_via_symbols
