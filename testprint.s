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
	leaq	L_fmt(%rip), %rbx
	leaq	L_lit(%rip), %rsi
	movq	%rbx, %rdi
	xorl	%eax, %eax
	callq	_printf
	leaq	L_lit.1(%rip), %rsi
	movq	%rbx, %rdi
	xorl	%eax, %eax
	callq	_printf
	leaq	L_lit.2(%rip), %rsi
	movq	%rbx, %rdi
	xorl	%eax, %eax
	callq	_printf
	xorl	%eax, %eax
	popq	%rbx
	retq
	.cfi_endproc
                                        ## -- End function
	.section	__TEXT,__cstring,cstring_literals
L_fmt:                                  ## @fmt
	.asciz	"%s\n"

L_lit:                                  ## @lit
	.asciz	"Hello_world"

L_lit.1:                                ## @lit.1
	.asciz	"Hello_world"

L_lit.2:                                ## @lit.2
	.asciz	"Hello_world"

.subsections_via_symbols
