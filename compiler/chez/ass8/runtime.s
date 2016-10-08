	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 11
	.globl	_print
	.align	4, 0x90
_print:                                 ## @print
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp0:
	.cfi_def_cfa_offset 16
Ltmp1:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp2:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	leaq	L_.str(%rip), %rax
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rsi
	movq	%rax, %rdi
	movb	$0, %al
	callq	_printf
	movl	%eax, -12(%rbp)         ## 4-byte Spill
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp3:
	.cfi_def_cfa_offset 16
Ltmp4:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp5:
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movl	$0, -4(%rbp)
	movl	%edi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	cmpl	$1, -8(%rbp)
	je	LBB1_2
## BB#1:
	leaq	L_.str.1(%rip), %rsi
	movq	___stderrp@GOTPCREL(%rip), %rax
	movq	(%rax), %rdi
	movq	-16(%rbp), %rax
	movq	(%rax), %rdx
	movb	$0, %al
	callq	_fprintf
	movl	$1, %edi
	movl	%eax, -36(%rbp)         ## 4-byte Spill
	callq	_exit
LBB1_2:
	movl	$8192, %eax             ## imm = 0x2000
	movl	%eax, %edi
	callq	_malloc
	movl	$8192, %ecx             ## imm = 0x2000
	movl	%ecx, %edi
	movq	%rax, -24(%rbp)
	callq	_malloc
	movq	%rax, -32(%rbp)
	movq	-24(%rbp), %rdi
	movq	-32(%rbp), %rsi
	callq	_scheme_entry
	movq	%rax, %rdi
	callq	_print
	movq	-24(%rbp), %rdi
	callq	_free
	movq	-32(%rbp), %rdi
	callq	_free
	xorl	%eax, %eax
	addq	$48, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"%ld\n"

L_.str.1:                               ## @.str.1
	.asciz	"usage: %s\n"


.subsections_via_symbols
