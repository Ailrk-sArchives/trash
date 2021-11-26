	.file	"b.cc"
	.text
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC2:
	.string	"%d "
	.text
	.p2align 4
	.globl	_Z3fooPcS_
	.type	_Z3fooPcS_, @function
_Z3fooPcS_:
.LFB12:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsi, %rdx
	movl	$2312, %r8d
	movq	%rdi, %rax
	pushq	%rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	leaq	7(%rdi), %rcx
	subq	$8, %rsp
	.cfi_def_cfa_offset 32
	movq	.LC0(%rip), %rsi
	movw	%r8w, 8(%rdi)
	leaq	1(%rdi), %r8
	movq	%rsi, (%rdi)
	movq	%rdx, %rdi
	subq	%r8, %rdi
	cmpq	$6, %rdi
	jbe	.L2
	movq	%rsi, (%rdx)
	movzbl	8(%rax), %esi
	subq	%rdx, %rcx
	movb	%sil, 8(%rdx)
	movzbl	9(%rax), %esi
	movb	%sil, 9(%rdx)
	cmpq	$14, %rcx
	jbe	.L4
.L10:
	movq	.LC1(%rip), %xmm1
	movq	(%rax), %xmm0
	paddb	%xmm1, %xmm0
	movq	%xmm0, (%rax)
	movq	(%rdx), %xmm0
	paddb	%xmm1, %xmm0
	movq	%xmm0, (%rdx)
	addb	$1, 8(%rax)
	addb	$1, 8(%rdx)
	addb	$1, 9(%rax)
	addb	$1, 9(%rdx)
.L5:
	xorl	%ebx, %ebx
	leaq	.LC2(%rip), %rbp
	.p2align 4,,10
	.p2align 3
.L6:
	movl	%ebx, %esi
	movq	%rbp, %rdi
	xorl	%eax, %eax
	addl	$1, %ebx
	call	printf@PLT
	cmpl	$10, %ebx
	jne	.L6
	addq	$8, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L2:
	.cfi_restore_state
	movb	$0, (%rdx)
	movzbl	1(%rax), %esi
	subq	%rdx, %rcx
	movb	%sil, 1(%rdx)
	movzbl	2(%rax), %esi
	movb	%sil, 2(%rdx)
	movzbl	3(%rax), %esi
	movb	%sil, 3(%rdx)
	movzbl	4(%rax), %esi
	movb	%sil, 4(%rdx)
	movzbl	5(%rax), %esi
	movb	%sil, 5(%rdx)
	movzbl	6(%rax), %esi
	movb	%sil, 6(%rdx)
	movzbl	7(%rax), %esi
	movb	%sil, 7(%rdx)
	movzbl	8(%rax), %esi
	movb	%sil, 8(%rdx)
	movzbl	9(%rax), %esi
	movb	%sil, 9(%rdx)
	cmpq	$14, %rcx
	ja	.L10
	.p2align 4,,10
	.p2align 3
.L4:
	addb	$1, (%rax)
	addb	$1, (%rdx)
	addb	$1, 1(%rax)
	addb	$1, 1(%rdx)
	addb	$1, 2(%rax)
	addb	$1, 2(%rdx)
	addb	$1, 3(%rax)
	addb	$1, 3(%rdx)
	addb	$1, 4(%rax)
	addb	$1, 4(%rdx)
	addb	$1, 5(%rax)
	addb	$1, 5(%rdx)
	addb	$1, 6(%rax)
	addb	$1, 6(%rdx)
	addb	$1, 7(%rax)
	addb	$1, 7(%rdx)
	addb	$1, 8(%rax)
	addb	$1, 8(%rdx)
	addb	$1, 9(%rax)
	addb	$1, 9(%rdx)
	jmp	.L5
	.cfi_endproc
.LFE12:
	.size	_Z3fooPcS_, .-_Z3fooPcS_
	.section	.text.startup,"ax",@progbits
	.p2align 4
	.globl	main
	.type	main, @function
main:
.LFB13:
	.cfi_startproc
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%fs:40, %rax
	movq	%rax, 24(%rsp)
	xorl	%eax, %eax
	leaq	14(%rsp), %rsi
	leaq	4(%rsp), %rdi
	call	_Z3fooPcS_
	movq	24(%rsp), %rax
	subq	%fs:40, %rax
	jne	.L14
	xorl	%eax, %eax
	addq	$40, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	ret
.L14:
	.cfi_restore_state
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE13:
	.size	main, .-main
	.section	.rodata.cst8,"aM",@progbits,8
	.align 8
.LC0:
	.byte	0
	.byte	1
	.byte	2
	.byte	3
	.byte	4
	.byte	5
	.byte	6
	.byte	7
	.align 8
.LC1:
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.ident	"GCC: (GNU) 11.1.0"
	.section	.note.GNU-stack,"",@progbits
