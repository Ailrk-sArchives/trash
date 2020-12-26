	.file	"pass_functions.cc"
	.text
	.section	.text._ZnwmPv,"axG",@progbits,_ZnwmPv,comdat
	.weak	_ZnwmPv
	.type	_ZnwmPv, @function
_ZnwmPv:
.LFB177:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE177:
	.size	_ZnwmPv, .-_ZnwmPv
	.section	.rodata
	.type	_ZStL19piecewise_construct, @object
	.size	_ZStL19piecewise_construct, 1
_ZStL19piecewise_construct:
	.zero	1
	.type	_ZStL13allocator_arg, @object
	.size	_ZStL13allocator_arg, 1
_ZStL13allocator_arg:
	.zero	1
	.type	_ZStL6ignore, @object
	.size	_ZStL6ignore, 1
_ZStL6ignore:
	.zero	1
	.section	.text._ZNSt9_Any_data9_M_accessEv,"axG",@progbits,_ZNSt9_Any_data9_M_accessEv,comdat
	.align 2
	.weak	_ZNSt9_Any_data9_M_accessEv
	.type	_ZNSt9_Any_data9_M_accessEv, @function
_ZNSt9_Any_data9_M_accessEv:
.LFB826:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE826:
	.size	_ZNSt9_Any_data9_M_accessEv, .-_ZNSt9_Any_data9_M_accessEv
	.section	.text._ZNKSt9_Any_data9_M_accessEv,"axG",@progbits,_ZNKSt9_Any_data9_M_accessEv,comdat
	.align 2
	.weak	_ZNKSt9_Any_data9_M_accessEv
	.type	_ZNKSt9_Any_data9_M_accessEv, @function
_ZNKSt9_Any_data9_M_accessEv:
.LFB827:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE827:
	.size	_ZNKSt9_Any_data9_M_accessEv, .-_ZNKSt9_Any_data9_M_accessEv
	.section	.text._ZNSt14_Function_baseC2Ev,"axG",@progbits,_ZNSt14_Function_baseC5Ev,comdat
	.align 2
	.weak	_ZNSt14_Function_baseC2Ev
	.type	_ZNSt14_Function_baseC2Ev, @function
_ZNSt14_Function_baseC2Ev:
.LFB844:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	$0, 16(%rax)
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE844:
	.size	_ZNSt14_Function_baseC2Ev, .-_ZNSt14_Function_baseC2Ev
	.weak	_ZNSt14_Function_baseC1Ev
	.set	_ZNSt14_Function_baseC1Ev,_ZNSt14_Function_baseC2Ev
	.section	.text._ZNSt14_Function_baseD2Ev,"axG",@progbits,_ZNSt14_Function_baseD5Ev,comdat
	.align 2
	.weak	_ZNSt14_Function_baseD2Ev
	.type	_ZNSt14_Function_baseD2Ev, @function
_ZNSt14_Function_baseD2Ev:
.LFB847:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA847
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.L10
	movq	-8(%rbp), %rax
	movq	16(%rax), %r8
	movq	-8(%rbp), %rcx
	movq	-8(%rbp), %rax
	movl	$3, %edx
	movq	%rcx, %rsi
	movq	%rax, %rdi
	call	*%r8
.L10:
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE847:
	.globl	__gxx_personality_v0
	.section	.gcc_except_table._ZNSt14_Function_baseD2Ev,"aG",@progbits,_ZNSt14_Function_baseD5Ev,comdat
.LLSDA847:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE847-.LLSDACSB847
.LLSDACSB847:
.LLSDACSE847:
	.section	.text._ZNSt14_Function_baseD2Ev,"axG",@progbits,_ZNSt14_Function_baseD5Ev,comdat
	.size	_ZNSt14_Function_baseD2Ev, .-_ZNSt14_Function_baseD2Ev
	.weak	_ZNSt14_Function_baseD1Ev
	.set	_ZNSt14_Function_baseD1Ev,_ZNSt14_Function_baseD2Ev
	.section	.text._ZNKSt14_Function_base8_M_emptyEv,"axG",@progbits,_ZNKSt14_Function_base8_M_emptyEv,comdat
	.align 2
	.weak	_ZNKSt14_Function_base8_M_emptyEv
	.type	_ZNKSt14_Function_base8_M_emptyEv, @function
_ZNKSt14_Function_base8_M_emptyEv:
.LFB849:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	sete	%al
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE849:
	.size	_ZNKSt14_Function_base8_M_emptyEv, .-_ZNKSt14_Function_base8_M_emptyEv
	.local	_ZStL8__ioinit
	.comm	_ZStL8__ioinit,1,1
	.text
	.globl	_Z8functioni
	.type	_Z8functioni, @function
_Z8functioni:
.LFB1930:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	addl	$1, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1930:
	.size	_Z8functioni, .-_Z8functioni
	.section	.text._ZN14FunctionObjectC2Ei,"axG",@progbits,_ZN14FunctionObjectC5Ei,comdat
	.align 2
	.weak	_ZN14FunctionObjectC2Ei
	.type	_ZN14FunctionObjectC2Ei, @function
_ZN14FunctionObjectC2Ei:
.LFB1932:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rax
	movl	-12(%rbp), %edx
	movl	%edx, (%rax)
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1932:
	.size	_ZN14FunctionObjectC2Ei, .-_ZN14FunctionObjectC2Ei
	.weak	_ZN14FunctionObjectC1Ei
	.set	_ZN14FunctionObjectC1Ei,_ZN14FunctionObjectC2Ei
	.section	.text._ZN14FunctionObjectclEi,"axG",@progbits,_ZN14FunctionObjectclEi,comdat
	.align 2
	.weak	_ZN14FunctionObjectclEi
	.type	_ZN14FunctionObjectclEi, @function
_ZN14FunctionObjectclEi:
.LFB1934:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rax
	movl	(%rax), %edx
	movl	-12(%rbp), %eax
	addl	%edx, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1934:
	.size	_ZN14FunctionObjectclEi, .-_ZN14FunctionObjectclEi
	.text
	.globl	_Z15function_callerSt8functionIFiiEEi
	.type	_Z15function_callerSt8functionIFiiEEi, @function
_Z15function_callerSt8functionIFiiEEi:
.LFB1936:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	-12(%rbp), %edx
	movq	-8(%rbp), %rax
	movl	%edx, %esi
	movq	%rax, %rdi
	call	_ZNKSt8functionIFiiEEclEi
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1936:
	.size	_Z15function_callerSt8functionIFiiEEi, .-_Z15function_callerSt8functionIFiiEEi
	.align 2
	.type	_ZZ4mainENKUliE_clEi, @function
_ZZ4mainENKUliE_clEi:
.LFB1938:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	-8(%rbp), %rax
	movl	(%rax), %edx
	movl	-12(%rbp), %eax
	addl	%edx, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1938:
	.size	_ZZ4mainENKUliE_clEi, .-_ZZ4mainENKUliE_clEi
	.section	.text._ZNSt8functionIFiiEED2Ev,"axG",@progbits,_ZNSt8functionIFiiEED5Ev,comdat
	.align 2
	.weak	_ZNSt8functionIFiiEED2Ev
	.type	_ZNSt8functionIFiiEED2Ev, @function
_ZNSt8functionIFiiEED2Ev:
.LFB1940:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_baseD2Ev
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1940:
	.size	_ZNSt8functionIFiiEED2Ev, .-_ZNSt8functionIFiiEED2Ev
	.weak	_ZNSt8functionIFiiEED1Ev
	.set	_ZNSt8functionIFiiEED1Ev,_ZNSt8functionIFiiEED2Ev
	.text
	.globl	main
	.type	main, @function
main:
.LFB1937:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA1937
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$72, %rsp
	.cfi_offset 3, -24
	movq	%fs:40, %rax
	movq	%rax, -24(%rbp)
	xorl	%eax, %eax
	movl	$3, -68(%rbp)
	movl	-68(%rbp), %edx
	leaq	-76(%rbp), %rax
	movl	%edx, %esi
	movq	%rax, %rdi
	call	_ZN14FunctionObjectC1Ei
	movl	-68(%rbp), %eax
	movl	%eax, -72(%rbp)
	movl	$5, %esi
	leaq	_Z8functioni(%rip), %rdi
.LEHB0:
	call	_Z15template_callerIPFiiEEiT_i
	movl	%eax, -80(%rbp)
	movl	-76(%rbp), %eax
	movl	$5, %esi
	movl	%eax, %edi
	call	_Z15template_callerI14FunctionObjectEiT_i
	movl	%eax, -80(%rbp)
	movl	-72(%rbp), %eax
	movl	$5, %esi
	movl	%eax, %edi
	call	_Z15template_callerIZ4mainEUliE_EiT_i
	movl	%eax, -80(%rbp)
	leaq	-64(%rbp), %rax
	leaq	_Z8functioni(%rip), %rsi
	movq	%rax, %rdi
	call	_ZNSt8functionIFiiEEC1IPS0_vvEET_
.LEHE0:
	leaq	-64(%rbp), %rax
	movl	$5, %esi
	movq	%rax, %rdi
.LEHB1:
	call	_Z15function_callerSt8functionIFiiEEi
.LEHE1:
	movl	%eax, -80(%rbp)
	leaq	-64(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt8functionIFiiEED1Ev
	movl	-76(%rbp), %edx
	leaq	-64(%rbp), %rax
	movl	%edx, %esi
	movq	%rax, %rdi
.LEHB2:
	call	_ZNSt8functionIFiiEEC1I14FunctionObjectvvEET_
.LEHE2:
	leaq	-64(%rbp), %rax
	movl	$5, %esi
	movq	%rax, %rdi
.LEHB3:
	call	_Z15function_callerSt8functionIFiiEEi
.LEHE3:
	movl	%eax, -80(%rbp)
	leaq	-64(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt8functionIFiiEED1Ev
	movl	-72(%rbp), %edx
	leaq	-64(%rbp), %rax
	movl	%edx, %esi
	movq	%rax, %rdi
.LEHB4:
	call	_ZNSt8functionIFiiEEC1IZ4mainEUliE_vvEET_
.LEHE4:
	leaq	-64(%rbp), %rax
	movl	$5, %esi
	movq	%rax, %rdi
.LEHB5:
	call	_Z15function_callerSt8functionIFiiEEi
.LEHE5:
	movl	%eax, -80(%rbp)
	leaq	-64(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt8functionIFiiEED1Ev
	movl	$0, %eax
	movq	-24(%rbp), %rcx
	subq	%fs:40, %rcx
	je	.L28
	jmp	.L32
.L29:
	movq	%rax, %rbx
	leaq	-64(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt8functionIFiiEED1Ev
	movq	%rbx, %rax
	movq	%rax, %rdi
.LEHB6:
	call	_Unwind_Resume@PLT
.L30:
	movq	%rax, %rbx
	leaq	-64(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt8functionIFiiEED1Ev
	movq	%rbx, %rax
	movq	%rax, %rdi
	call	_Unwind_Resume@PLT
.L31:
	movq	%rax, %rbx
	leaq	-64(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt8functionIFiiEED1Ev
	movq	%rbx, %rax
	movq	%rax, %rdi
	call	_Unwind_Resume@PLT
.LEHE6:
.L32:
	call	__stack_chk_fail@PLT
.L28:
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1937:
	.section	.gcc_except_table,"a",@progbits
.LLSDA1937:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE1937-.LLSDACSB1937
.LLSDACSB1937:
	.uleb128 .LEHB0-.LFB1937
	.uleb128 .LEHE0-.LEHB0
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB1-.LFB1937
	.uleb128 .LEHE1-.LEHB1
	.uleb128 .L29-.LFB1937
	.uleb128 0
	.uleb128 .LEHB2-.LFB1937
	.uleb128 .LEHE2-.LEHB2
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB3-.LFB1937
	.uleb128 .LEHE3-.LEHB3
	.uleb128 .L30-.LFB1937
	.uleb128 0
	.uleb128 .LEHB4-.LFB1937
	.uleb128 .LEHE4-.LEHB4
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB5-.LFB1937
	.uleb128 .LEHE5-.LEHB5
	.uleb128 .L31-.LFB1937
	.uleb128 0
	.uleb128 .LEHB6-.LFB1937
	.uleb128 .LEHE6-.LEHB6
	.uleb128 0
	.uleb128 0
.LLSDACSE1937:
	.text
	.size	main, .-main
	.section	.text._ZNKSt8functionIFiiEEclEi,"axG",@progbits,_ZNKSt8functionIFiiEEclEi,comdat
	.align 2
	.weak	_ZNKSt8functionIFiiEEclEi
	.type	_ZNKSt8functionIFiiEEclEi, @function
_ZNKSt8functionIFiiEEclEi:
.LFB2186:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt14_Function_base8_M_emptyEv
	testb	%al, %al
	je	.L34
	call	_ZSt25__throw_bad_function_callv@PLT
.L34:
	movq	-24(%rbp), %rax
	movq	24(%rax), %rbx
	leaq	-28(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt7forwardIiEOT_RNSt16remove_referenceIS0_E4typeE
	movq	%rax, %rdx
	movq	-24(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	*%rbx
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2186:
	.size	_ZNKSt8functionIFiiEEclEi, .-_ZNKSt8functionIFiiEEclEi
	.section	.text._Z15template_callerIPFiiEEiT_i,"axG",@progbits,_Z15template_callerIPFiiEEiT_i,comdat
	.weak	_Z15template_callerIPFiiEEiT_i
	.type	_Z15template_callerIPFiiEEiT_i, @function
_Z15template_callerIPFiiEEiT_i:
.LFB2187:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	-12(%rbp), %eax
	movq	-8(%rbp), %rdx
	movl	%eax, %edi
	call	*%rdx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2187:
	.size	_Z15template_callerIPFiiEEiT_i, .-_Z15template_callerIPFiiEEiT_i
	.section	.text._Z15template_callerI14FunctionObjectEiT_i,"axG",@progbits,_Z15template_callerI14FunctionObjectEiT_i,comdat
	.weak	_Z15template_callerI14FunctionObjectEiT_i
	.type	_Z15template_callerI14FunctionObjectEiT_i, @function
_Z15template_callerI14FunctionObjectEiT_i:
.LFB2188:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-8(%rbp), %edx
	leaq	-4(%rbp), %rax
	movl	%edx, %esi
	movq	%rax, %rdi
	call	_ZN14FunctionObjectclEi
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2188:
	.size	_Z15template_callerI14FunctionObjectEiT_i, .-_Z15template_callerI14FunctionObjectEiT_i
	.text
	.type	_Z15template_callerIZ4mainEUliE_EiT_i, @function
_Z15template_callerIZ4mainEUliE_EiT_i:
.LFB2189:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$8, %rsp
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	movl	-8(%rbp), %edx
	leaq	-4(%rbp), %rax
	movl	%edx, %esi
	movq	%rax, %rdi
	call	_ZZ4mainENKUliE_clEi
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2189:
	.size	_Z15template_callerIZ4mainEUliE_EiT_i, .-_Z15template_callerIZ4mainEUliE_EiT_i
	.section	.text._ZNSt8functionIFiiEEC2IPS0_vvEET_,"axG",@progbits,_ZNSt8functionIFiiEEC5IPS0_vvEET_,comdat
	.align 2
	.weak	_ZNSt8functionIFiiEEC2IPS0_vvEET_
	.type	_ZNSt8functionIFiiEEC2IPS0_vvEET_, @function
_ZNSt8functionIFiiEEC2IPS0_vvEET_:
.LFB2192:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA2192
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_baseC2Ev
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerIPFiiEE21_M_not_empty_functionIS1_EEbPT_
	testb	%al, %al
	je	.L46
	leaq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt4moveIRPFiiEEONSt16remove_referenceIT_E4typeEOS4_
	movq	%rax, %rdx
	movq	-24(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
.LEHB7:
	call	_ZNSt14_Function_base13_Base_managerIPFiiEE15_M_init_functorERSt9_Any_dataOS2_
.LEHE7:
	movq	-24(%rbp), %rax
	leaq	_ZNSt17_Function_handlerIFiiEPS0_E9_M_invokeERKSt9_Any_dataOi(%rip), %rdx
	movq	%rdx, 24(%rax)
	movq	-24(%rbp), %rax
	leaq	_ZNSt17_Function_handlerIFiiEPS0_E10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation(%rip), %rdx
	movq	%rdx, 16(%rax)
	jmp	.L46
.L45:
	movq	%rax, %rbx
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_baseD2Ev
	movq	%rbx, %rax
	movq	%rax, %rdi
.LEHB8:
	call	_Unwind_Resume@PLT
.LEHE8:
.L46:
	nop
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2192:
	.section	.gcc_except_table
.LLSDA2192:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE2192-.LLSDACSB2192
.LLSDACSB2192:
	.uleb128 .LEHB7-.LFB2192
	.uleb128 .LEHE7-.LEHB7
	.uleb128 .L45-.LFB2192
	.uleb128 0
	.uleb128 .LEHB8-.LFB2192
	.uleb128 .LEHE8-.LEHB8
	.uleb128 0
	.uleb128 0
.LLSDACSE2192:
	.section	.text._ZNSt8functionIFiiEEC2IPS0_vvEET_,"axG",@progbits,_ZNSt8functionIFiiEEC5IPS0_vvEET_,comdat
	.size	_ZNSt8functionIFiiEEC2IPS0_vvEET_, .-_ZNSt8functionIFiiEEC2IPS0_vvEET_
	.weak	_ZNSt8functionIFiiEEC1IPS0_vvEET_
	.set	_ZNSt8functionIFiiEEC1IPS0_vvEET_,_ZNSt8functionIFiiEEC2IPS0_vvEET_
	.section	.text._ZNSt8functionIFiiEEC2I14FunctionObjectvvEET_,"axG",@progbits,_ZNSt8functionIFiiEEC5I14FunctionObjectvvEET_,comdat
	.align 2
	.weak	_ZNSt8functionIFiiEEC2I14FunctionObjectvvEET_
	.type	_ZNSt8functionIFiiEEC2I14FunctionObjectvvEET_, @function
_ZNSt8functionIFiiEEC2I14FunctionObjectvvEET_:
.LFB2199:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA2199
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_baseC2Ev
	leaq	-28(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE21_M_not_empty_functionIS1_EEbRKT_
	testb	%al, %al
	je	.L51
	leaq	-28(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt4moveIR14FunctionObjectEONSt16remove_referenceIT_E4typeEOS3_
	movq	%rax, %rdx
	movq	-24(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
.LEHB9:
	call	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE15_M_init_functorERSt9_Any_dataOS1_
.LEHE9:
	movq	-24(%rbp), %rax
	leaq	_ZNSt17_Function_handlerIFiiE14FunctionObjectE9_M_invokeERKSt9_Any_dataOi(%rip), %rdx
	movq	%rdx, 24(%rax)
	movq	-24(%rbp), %rax
	leaq	_ZNSt17_Function_handlerIFiiE14FunctionObjectE10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation(%rip), %rdx
	movq	%rdx, 16(%rax)
	jmp	.L51
.L50:
	movq	%rax, %rbx
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_baseD2Ev
	movq	%rbx, %rax
	movq	%rax, %rdi
.LEHB10:
	call	_Unwind_Resume@PLT
.LEHE10:
.L51:
	nop
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2199:
	.section	.gcc_except_table
.LLSDA2199:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE2199-.LLSDACSB2199
.LLSDACSB2199:
	.uleb128 .LEHB9-.LFB2199
	.uleb128 .LEHE9-.LEHB9
	.uleb128 .L50-.LFB2199
	.uleb128 0
	.uleb128 .LEHB10-.LFB2199
	.uleb128 .LEHE10-.LEHB10
	.uleb128 0
	.uleb128 0
.LLSDACSE2199:
	.section	.text._ZNSt8functionIFiiEEC2I14FunctionObjectvvEET_,"axG",@progbits,_ZNSt8functionIFiiEEC5I14FunctionObjectvvEET_,comdat
	.size	_ZNSt8functionIFiiEEC2I14FunctionObjectvvEET_, .-_ZNSt8functionIFiiEEC2I14FunctionObjectvvEET_
	.weak	_ZNSt8functionIFiiEEC1I14FunctionObjectvvEET_
	.set	_ZNSt8functionIFiiEEC1I14FunctionObjectvvEET_,_ZNSt8functionIFiiEEC2I14FunctionObjectvvEET_
	.text
	.align 2
	.type	_ZNSt8functionIFiiEEC2IZ4mainEUliE_vvEET_, @function
_ZNSt8functionIFiiEEC2IZ4mainEUliE_vvEET_:
.LFB2203:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA2203
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_baseC2Ev
	leaq	-28(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E21_M_not_empty_functionIS1_EEbRKT_
	testb	%al, %al
	je	.L56
	leaq	-28(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt4moveIRZ4mainEUliE_EONSt16remove_referenceIT_E4typeEOS3_
	movq	%rax, %rdx
	movq	-24(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
.LEHB11:
	call	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E15_M_init_functorERSt9_Any_dataOS1_
.LEHE11:
	movq	-24(%rbp), %rax
	leaq	_ZNSt17_Function_handlerIFiiEZ4mainEUliE_E9_M_invokeERKSt9_Any_dataOi(%rip), %rdx
	movq	%rdx, 24(%rax)
	movq	-24(%rbp), %rax
	leaq	_ZNSt17_Function_handlerIFiiEZ4mainEUliE_E10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation(%rip), %rdx
	movq	%rdx, 16(%rax)
	jmp	.L56
.L55:
	movq	%rax, %rbx
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_baseD2Ev
	movq	%rbx, %rax
	movq	%rax, %rdi
.LEHB12:
	call	_Unwind_Resume@PLT
.LEHE12:
.L56:
	nop
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2203:
	.section	.gcc_except_table
.LLSDA2203:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE2203-.LLSDACSB2203
.LLSDACSB2203:
	.uleb128 .LEHB11-.LFB2203
	.uleb128 .LEHE11-.LEHB11
	.uleb128 .L55-.LFB2203
	.uleb128 0
	.uleb128 .LEHB12-.LFB2203
	.uleb128 .LEHE12-.LEHB12
	.uleb128 0
	.uleb128 0
.LLSDACSE2203:
	.text
	.size	_ZNSt8functionIFiiEEC2IZ4mainEUliE_vvEET_, .-_ZNSt8functionIFiiEEC2IZ4mainEUliE_vvEET_
	.set	_ZNSt8functionIFiiEEC1IZ4mainEUliE_vvEET_,_ZNSt8functionIFiiEEC2IZ4mainEUliE_vvEET_
	.section	.text._ZSt7forwardIiEOT_RNSt16remove_referenceIS0_E4typeE,"axG",@progbits,_ZSt7forwardIiEOT_RNSt16remove_referenceIS0_E4typeE,comdat
	.weak	_ZSt7forwardIiEOT_RNSt16remove_referenceIS0_E4typeE
	.type	_ZSt7forwardIiEOT_RNSt16remove_referenceIS0_E4typeE, @function
_ZSt7forwardIiEOT_RNSt16remove_referenceIS0_E4typeE:
.LFB2314:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2314:
	.size	_ZSt7forwardIiEOT_RNSt16remove_referenceIS0_E4typeE, .-_ZSt7forwardIiEOT_RNSt16remove_referenceIS0_E4typeE
	.section	.text._ZNSt14_Function_base13_Base_managerIPFiiEE21_M_not_empty_functionIS1_EEbPT_,"axG",@progbits,_ZNSt14_Function_base13_Base_managerIPFiiEE21_M_not_empty_functionIS1_EEbPT_,comdat
	.weak	_ZNSt14_Function_base13_Base_managerIPFiiEE21_M_not_empty_functionIS1_EEbPT_
	.type	_ZNSt14_Function_base13_Base_managerIPFiiEE21_M_not_empty_functionIS1_EEbPT_, @function
_ZNSt14_Function_base13_Base_managerIPFiiEE21_M_not_empty_functionIS1_EEbPT_:
.LFB2315:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	cmpq	$0, -8(%rbp)
	setne	%al
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2315:
	.size	_ZNSt14_Function_base13_Base_managerIPFiiEE21_M_not_empty_functionIS1_EEbPT_, .-_ZNSt14_Function_base13_Base_managerIPFiiEE21_M_not_empty_functionIS1_EEbPT_
	.section	.text._ZSt4moveIRPFiiEEONSt16remove_referenceIT_E4typeEOS4_,"axG",@progbits,_ZSt4moveIRPFiiEEONSt16remove_referenceIT_E4typeEOS4_,comdat
	.weak	_ZSt4moveIRPFiiEEONSt16remove_referenceIT_E4typeEOS4_
	.type	_ZSt4moveIRPFiiEEONSt16remove_referenceIT_E4typeEOS4_, @function
_ZSt4moveIRPFiiEEONSt16remove_referenceIT_E4typeEOS4_:
.LFB2316:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2316:
	.size	_ZSt4moveIRPFiiEEONSt16remove_referenceIT_E4typeEOS4_, .-_ZSt4moveIRPFiiEEONSt16remove_referenceIT_E4typeEOS4_
	.section	.text._ZNSt14_Function_base13_Base_managerIPFiiEE15_M_init_functorERSt9_Any_dataOS2_,"axG",@progbits,_ZNSt14_Function_base13_Base_managerIPFiiEE15_M_init_functorERSt9_Any_dataOS2_,comdat
	.weak	_ZNSt14_Function_base13_Base_managerIPFiiEE15_M_init_functorERSt9_Any_dataOS2_
	.type	_ZNSt14_Function_base13_Base_managerIPFiiEE15_M_init_functorERSt9_Any_dataOS2_, @function
_ZNSt14_Function_base13_Base_managerIPFiiEE15_M_init_functorERSt9_Any_dataOS2_:
.LFB2317:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt4moveIRPFiiEEONSt16remove_referenceIT_E4typeEOS4_
	movq	%rax, %rdx
	movq	-8(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerIPFiiEE15_M_init_functorERSt9_Any_dataOS2_St17integral_constantIbLb1EE
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2317:
	.size	_ZNSt14_Function_base13_Base_managerIPFiiEE15_M_init_functorERSt9_Any_dataOS2_, .-_ZNSt14_Function_base13_Base_managerIPFiiEE15_M_init_functorERSt9_Any_dataOS2_
	.section	.text._ZNSt17_Function_handlerIFiiEPS0_E9_M_invokeERKSt9_Any_dataOi,"axG",@progbits,_ZNSt17_Function_handlerIFiiEPS0_E9_M_invokeERKSt9_Any_dataOi,comdat
	.weak	_ZNSt17_Function_handlerIFiiEPS0_E9_M_invokeERKSt9_Any_dataOi
	.type	_ZNSt17_Function_handlerIFiiEPS0_E9_M_invokeERKSt9_Any_dataOi, @function
_ZNSt17_Function_handlerIFiiEPS0_E9_M_invokeERKSt9_Any_dataOi:
.LFB2318:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt7forwardIiEOT_RNSt16remove_referenceIS0_E4typeE
	movq	%rax, %rbx
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerIPFiiEE14_M_get_pointerERKSt9_Any_data
	movq	%rbx, %rsi
	movq	%rax, %rdi
	call	_ZSt10__invoke_rIiRPFiiEJiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES7_EEE5valueES7_E4typeEOSC_DpOSD_
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2318:
	.size	_ZNSt17_Function_handlerIFiiEPS0_E9_M_invokeERKSt9_Any_dataOi, .-_ZNSt17_Function_handlerIFiiEPS0_E9_M_invokeERKSt9_Any_dataOi
	.section	.text._ZNSt17_Function_handlerIFiiEPS0_E10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation,"axG",@progbits,_ZNSt17_Function_handlerIFiiEPS0_E10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation,comdat
	.weak	_ZNSt17_Function_handlerIFiiEPS0_E10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation
	.type	_ZNSt17_Function_handlerIFiiEPS0_E10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation, @function
_ZNSt17_Function_handlerIFiiEPS0_E10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation:
.LFB2319:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$40, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movl	%edx, -36(%rbp)
	movl	-36(%rbp), %eax
	testl	%eax, %eax
	je	.L67
	cmpl	$1, %eax
	je	.L68
	jmp	.L72
.L67:
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessIPKSt9type_infoEERT_v
	leaq	_ZTIPFiiE(%rip), %rdx
	movq	%rdx, (%rax)
	jmp	.L70
.L68:
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessIPPFiiEEERT_v
	movq	%rax, %rbx
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerIPFiiEE14_M_get_pointerERKSt9_Any_data
	movq	%rax, (%rbx)
	jmp	.L70
.L72:
	movl	-36(%rbp), %edx
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rcx, %rsi
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerIPFiiEE10_M_managerERSt9_Any_dataRKS4_St18_Manager_operation
.L70:
	movl	$0, %eax
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2319:
	.size	_ZNSt17_Function_handlerIFiiEPS0_E10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation, .-_ZNSt17_Function_handlerIFiiEPS0_E10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation
	.section	.text._ZNSt14_Function_base13_Base_managerI14FunctionObjectE21_M_not_empty_functionIS1_EEbRKT_,"axG",@progbits,_ZNSt14_Function_base13_Base_managerI14FunctionObjectE21_M_not_empty_functionIS1_EEbRKT_,comdat
	.weak	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE21_M_not_empty_functionIS1_EEbRKT_
	.type	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE21_M_not_empty_functionIS1_EEbRKT_, @function
_ZNSt14_Function_base13_Base_managerI14FunctionObjectE21_M_not_empty_functionIS1_EEbRKT_:
.LFB2322:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movl	$1, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2322:
	.size	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE21_M_not_empty_functionIS1_EEbRKT_, .-_ZNSt14_Function_base13_Base_managerI14FunctionObjectE21_M_not_empty_functionIS1_EEbRKT_
	.section	.text._ZSt4moveIR14FunctionObjectEONSt16remove_referenceIT_E4typeEOS3_,"axG",@progbits,_ZSt4moveIR14FunctionObjectEONSt16remove_referenceIT_E4typeEOS3_,comdat
	.weak	_ZSt4moveIR14FunctionObjectEONSt16remove_referenceIT_E4typeEOS3_
	.type	_ZSt4moveIR14FunctionObjectEONSt16remove_referenceIT_E4typeEOS3_, @function
_ZSt4moveIR14FunctionObjectEONSt16remove_referenceIT_E4typeEOS3_:
.LFB2323:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2323:
	.size	_ZSt4moveIR14FunctionObjectEONSt16remove_referenceIT_E4typeEOS3_, .-_ZSt4moveIR14FunctionObjectEONSt16remove_referenceIT_E4typeEOS3_
	.section	.text._ZNSt14_Function_base13_Base_managerI14FunctionObjectE15_M_init_functorERSt9_Any_dataOS1_,"axG",@progbits,_ZNSt14_Function_base13_Base_managerI14FunctionObjectE15_M_init_functorERSt9_Any_dataOS1_,comdat
	.weak	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE15_M_init_functorERSt9_Any_dataOS1_
	.type	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE15_M_init_functorERSt9_Any_dataOS1_, @function
_ZNSt14_Function_base13_Base_managerI14FunctionObjectE15_M_init_functorERSt9_Any_dataOS1_:
.LFB2324:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt4moveIR14FunctionObjectEONSt16remove_referenceIT_E4typeEOS3_
	movq	%rax, %rdx
	movq	-8(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE15_M_init_functorERSt9_Any_dataOS1_St17integral_constantIbLb1EE
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2324:
	.size	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE15_M_init_functorERSt9_Any_dataOS1_, .-_ZNSt14_Function_base13_Base_managerI14FunctionObjectE15_M_init_functorERSt9_Any_dataOS1_
	.section	.text._ZNSt17_Function_handlerIFiiE14FunctionObjectE9_M_invokeERKSt9_Any_dataOi,"axG",@progbits,_ZNSt17_Function_handlerIFiiE14FunctionObjectE9_M_invokeERKSt9_Any_dataOi,comdat
	.weak	_ZNSt17_Function_handlerIFiiE14FunctionObjectE9_M_invokeERKSt9_Any_dataOi
	.type	_ZNSt17_Function_handlerIFiiE14FunctionObjectE9_M_invokeERKSt9_Any_dataOi, @function
_ZNSt17_Function_handlerIFiiE14FunctionObjectE9_M_invokeERKSt9_Any_dataOi:
.LFB2325:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt7forwardIiEOT_RNSt16remove_referenceIS0_E4typeE
	movq	%rax, %rbx
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE14_M_get_pointerERKSt9_Any_data
	movq	%rbx, %rsi
	movq	%rax, %rdi
	call	_ZSt10__invoke_rIiR14FunctionObjectJiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES6_EEE5valueES6_E4typeEOSB_DpOSC_
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2325:
	.size	_ZNSt17_Function_handlerIFiiE14FunctionObjectE9_M_invokeERKSt9_Any_dataOi, .-_ZNSt17_Function_handlerIFiiE14FunctionObjectE9_M_invokeERKSt9_Any_dataOi
	.section	.text._ZNSt17_Function_handlerIFiiE14FunctionObjectE10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation,"axG",@progbits,_ZNSt17_Function_handlerIFiiE14FunctionObjectE10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation,comdat
	.weak	_ZNSt17_Function_handlerIFiiE14FunctionObjectE10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation
	.type	_ZNSt17_Function_handlerIFiiE14FunctionObjectE10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation, @function
_ZNSt17_Function_handlerIFiiE14FunctionObjectE10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation:
.LFB2326:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$40, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movl	%edx, -36(%rbp)
	movl	-36(%rbp), %eax
	testl	%eax, %eax
	je	.L81
	cmpl	$1, %eax
	je	.L82
	jmp	.L86
.L81:
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessIPKSt9type_infoEERT_v
	leaq	_ZTI14FunctionObject(%rip), %rdx
	movq	%rdx, (%rax)
	jmp	.L84
.L82:
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessIP14FunctionObjectEERT_v
	movq	%rax, %rbx
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE14_M_get_pointerERKSt9_Any_data
	movq	%rax, (%rbx)
	jmp	.L84
.L86:
	movl	-36(%rbp), %edx
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rcx, %rsi
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation
.L84:
	movl	$0, %eax
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2326:
	.size	_ZNSt17_Function_handlerIFiiE14FunctionObjectE10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation, .-_ZNSt17_Function_handlerIFiiE14FunctionObjectE10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation
	.text
	.type	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E21_M_not_empty_functionIS1_EEbRKT_, @function
_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E21_M_not_empty_functionIS1_EEbRKT_:
.LFB2327:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movl	$1, %eax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2327:
	.size	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E21_M_not_empty_functionIS1_EEbRKT_, .-_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E21_M_not_empty_functionIS1_EEbRKT_
	.type	_ZSt4moveIRZ4mainEUliE_EONSt16remove_referenceIT_E4typeEOS3_, @function
_ZSt4moveIRZ4mainEUliE_EONSt16remove_referenceIT_E4typeEOS3_:
.LFB2328:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2328:
	.size	_ZSt4moveIRZ4mainEUliE_EONSt16remove_referenceIT_E4typeEOS3_, .-_ZSt4moveIRZ4mainEUliE_EONSt16remove_referenceIT_E4typeEOS3_
	.type	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E15_M_init_functorERSt9_Any_dataOS1_, @function
_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E15_M_init_functorERSt9_Any_dataOS1_:
.LFB2329:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt4moveIRZ4mainEUliE_EONSt16remove_referenceIT_E4typeEOS3_
	movq	%rax, %rdx
	movq	-8(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E15_M_init_functorERSt9_Any_dataOS1_St17integral_constantIbLb1EE
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2329:
	.size	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E15_M_init_functorERSt9_Any_dataOS1_, .-_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E15_M_init_functorERSt9_Any_dataOS1_
	.type	_ZNSt17_Function_handlerIFiiEZ4mainEUliE_E9_M_invokeERKSt9_Any_dataOi, @function
_ZNSt17_Function_handlerIFiiEZ4mainEUliE_E9_M_invokeERKSt9_Any_dataOi:
.LFB2330:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt7forwardIiEOT_RNSt16remove_referenceIS0_E4typeE
	movq	%rax, %rbx
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E14_M_get_pointerERKSt9_Any_data
	movq	%rbx, %rsi
	movq	%rax, %rdi
	call	_ZSt10__invoke_rIiRZ4mainEUliE_JiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES6_EEE5valueES6_E4typeEOSB_DpOSC_
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2330:
	.size	_ZNSt17_Function_handlerIFiiEZ4mainEUliE_E9_M_invokeERKSt9_Any_dataOi, .-_ZNSt17_Function_handlerIFiiEZ4mainEUliE_E9_M_invokeERKSt9_Any_dataOi
	.type	_ZNSt17_Function_handlerIFiiEZ4mainEUliE_E10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation, @function
_ZNSt17_Function_handlerIFiiEZ4mainEUliE_E10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation:
.LFB2331:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$40, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movl	%edx, -36(%rbp)
	movl	-36(%rbp), %eax
	testl	%eax, %eax
	je	.L95
	cmpl	$1, %eax
	je	.L96
	jmp	.L100
.L95:
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessIPKSt9type_infoEERT_v
	leaq	_ZTIZ4mainEUliE_(%rip), %rdx
	movq	%rdx, (%rax)
	jmp	.L98
.L96:
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessIPZ4mainEUliE_EERT_v
	movq	%rax, %rbx
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E14_M_get_pointerERKSt9_Any_data
	movq	%rax, (%rbx)
	jmp	.L98
.L100:
	movl	-36(%rbp), %edx
	movq	-32(%rbp), %rcx
	movq	-24(%rbp), %rax
	movq	%rcx, %rsi
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation
.L98:
	movl	$0, %eax
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2331:
	.size	_ZNSt17_Function_handlerIFiiEZ4mainEUliE_E10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation, .-_ZNSt17_Function_handlerIFiiEZ4mainEUliE_E10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation
	.section	.text._ZNSt14_Function_base13_Base_managerIPFiiEE15_M_init_functorERSt9_Any_dataOS2_St17integral_constantIbLb1EE,"axG",@progbits,_ZNSt14_Function_base13_Base_managerIPFiiEE15_M_init_functorERSt9_Any_dataOS2_St17integral_constantIbLb1EE,comdat
	.weak	_ZNSt14_Function_base13_Base_managerIPFiiEE15_M_init_functorERSt9_Any_dataOS2_St17integral_constantIbLb1EE
	.type	_ZNSt14_Function_base13_Base_managerIPFiiEE15_M_init_functorERSt9_Any_dataOS2_St17integral_constantIbLb1EE, @function
_ZNSt14_Function_base13_Base_managerIPFiiEE15_M_init_functorERSt9_Any_dataOS2_St17integral_constantIbLb1EE:
.LFB2366:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt4moveIRPFiiEEONSt16remove_referenceIT_E4typeEOS4_
	movq	(%rax), %rbx
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessEv
	movq	%rax, %rsi
	movl	$8, %edi
	call	_ZnwmPv
	movq	%rbx, (%rax)
	nop
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2366:
	.size	_ZNSt14_Function_base13_Base_managerIPFiiEE15_M_init_functorERSt9_Any_dataOS2_St17integral_constantIbLb1EE, .-_ZNSt14_Function_base13_Base_managerIPFiiEE15_M_init_functorERSt9_Any_dataOS2_St17integral_constantIbLb1EE
	.section	.text._ZNSt14_Function_base13_Base_managerIPFiiEE14_M_get_pointerERKSt9_Any_data,"axG",@progbits,_ZNSt14_Function_base13_Base_managerIPFiiEE14_M_get_pointerERKSt9_Any_data,comdat
	.weak	_ZNSt14_Function_base13_Base_managerIPFiiEE14_M_get_pointerERKSt9_Any_data
	.type	_ZNSt14_Function_base13_Base_managerIPFiiEE14_M_get_pointerERKSt9_Any_data, @function
_ZNSt14_Function_base13_Base_managerIPFiiEE14_M_get_pointerERKSt9_Any_data:
.LFB2367:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt9_Any_data9_M_accessIPFiiEEERKT_v
	movq	%rax, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt11__addressofIKPFiiEEPT_RS3_
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2367:
	.size	_ZNSt14_Function_base13_Base_managerIPFiiEE14_M_get_pointerERKSt9_Any_data, .-_ZNSt14_Function_base13_Base_managerIPFiiEE14_M_get_pointerERKSt9_Any_data
	.section	.text._ZSt10__invoke_rIiRPFiiEJiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES7_EEE5valueES7_E4typeEOSC_DpOSD_,"axG",@progbits,_ZSt10__invoke_rIiRPFiiEJiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES7_EEE5valueES7_E4typeEOSC_DpOSD_,comdat
	.weak	_ZSt10__invoke_rIiRPFiiEJiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES7_EEE5valueES7_E4typeEOSC_DpOSD_
	.type	_ZSt10__invoke_rIiRPFiiEJiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES7_EEE5valueES7_E4typeEOSC_DpOSD_, @function
_ZSt10__invoke_rIiRPFiiEJiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES7_EEE5valueES7_E4typeEOSC_DpOSD_:
.LFB2368:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt7forwardIiEOT_RNSt16remove_referenceIS0_E4typeE
	movq	%rax, %rbx
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt7forwardIRPFiiEEOT_RNSt16remove_referenceIS3_E4typeE
	movq	%rbx, %rsi
	movq	%rax, %rdi
	call	_ZSt13__invoke_implIiRPFiiEJiEET_St14__invoke_otherOT0_DpOT1_
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2368:
	.size	_ZSt10__invoke_rIiRPFiiEJiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES7_EEE5valueES7_E4typeEOSC_DpOSD_, .-_ZSt10__invoke_rIiRPFiiEJiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES7_EEE5valueES7_E4typeEOSC_DpOSD_
	.section	.text._ZNSt9_Any_data9_M_accessIPKSt9type_infoEERT_v,"axG",@progbits,_ZNSt9_Any_data9_M_accessIPKSt9type_infoEERT_v,comdat
	.align 2
	.weak	_ZNSt9_Any_data9_M_accessIPKSt9type_infoEERT_v
	.type	_ZNSt9_Any_data9_M_accessIPKSt9type_infoEERT_v, @function
_ZNSt9_Any_data9_M_accessIPKSt9type_infoEERT_v:
.LFB2369:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessEv
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2369:
	.size	_ZNSt9_Any_data9_M_accessIPKSt9type_infoEERT_v, .-_ZNSt9_Any_data9_M_accessIPKSt9type_infoEERT_v
	.section	.text._ZNSt9_Any_data9_M_accessIPPFiiEEERT_v,"axG",@progbits,_ZNSt9_Any_data9_M_accessIPPFiiEEERT_v,comdat
	.align 2
	.weak	_ZNSt9_Any_data9_M_accessIPPFiiEEERT_v
	.type	_ZNSt9_Any_data9_M_accessIPPFiiEEERT_v, @function
_ZNSt9_Any_data9_M_accessIPPFiiEEERT_v:
.LFB2370:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessEv
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2370:
	.size	_ZNSt9_Any_data9_M_accessIPPFiiEEERT_v, .-_ZNSt9_Any_data9_M_accessIPPFiiEEERT_v
	.section	.text._ZNSt14_Function_base13_Base_managerIPFiiEE10_M_managerERSt9_Any_dataRKS4_St18_Manager_operation,"axG",@progbits,_ZNSt14_Function_base13_Base_managerIPFiiEE10_M_managerERSt9_Any_dataRKS4_St18_Manager_operation,comdat
	.weak	_ZNSt14_Function_base13_Base_managerIPFiiEE10_M_managerERSt9_Any_dataRKS4_St18_Manager_operation
	.type	_ZNSt14_Function_base13_Base_managerIPFiiEE10_M_managerERSt9_Any_dataRKS4_St18_Manager_operation, @function
_ZNSt14_Function_base13_Base_managerIPFiiEE10_M_managerERSt9_Any_dataRKS4_St18_Manager_operation:
.LFB2371:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$40, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movl	%edx, -36(%rbp)
	movl	-36(%rbp), %eax
	cmpl	$3, %eax
	je	.L111
	cmpl	$3, %eax
	jg	.L112
	cmpl	$2, %eax
	je	.L113
	cmpl	$2, %eax
	jg	.L112
	testl	%eax, %eax
	je	.L114
	cmpl	$1, %eax
	je	.L115
	jmp	.L112
.L114:
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessIPKSt9type_infoEERT_v
	leaq	_ZTIPFiiE(%rip), %rdx
	movq	%rdx, (%rax)
	jmp	.L112
.L115:
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessIPPFiiEEERT_v
	movq	%rax, %rbx
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerIPFiiEE14_M_get_pointerERKSt9_Any_data
	movq	%rax, (%rbx)
	jmp	.L112
.L113:
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerIPFiiEE8_M_cloneERSt9_Any_dataRKS4_St17integral_constantIbLb1EE
	jmp	.L112
.L111:
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerIPFiiEE10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE
	nop
.L112:
	movl	$0, %eax
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2371:
	.size	_ZNSt14_Function_base13_Base_managerIPFiiEE10_M_managerERSt9_Any_dataRKS4_St18_Manager_operation, .-_ZNSt14_Function_base13_Base_managerIPFiiEE10_M_managerERSt9_Any_dataRKS4_St18_Manager_operation
	.section	.text._ZNSt14_Function_base13_Base_managerI14FunctionObjectE15_M_init_functorERSt9_Any_dataOS1_St17integral_constantIbLb1EE,"axG",@progbits,_ZNSt14_Function_base13_Base_managerI14FunctionObjectE15_M_init_functorERSt9_Any_dataOS1_St17integral_constantIbLb1EE,comdat
	.weak	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE15_M_init_functorERSt9_Any_dataOS1_St17integral_constantIbLb1EE
	.type	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE15_M_init_functorERSt9_Any_dataOS1_St17integral_constantIbLb1EE, @function
_ZNSt14_Function_base13_Base_managerI14FunctionObjectE15_M_init_functorERSt9_Any_dataOS1_St17integral_constantIbLb1EE:
.LFB2378:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt4moveIR14FunctionObjectEONSt16remove_referenceIT_E4typeEOS3_
	movq	%rax, %rbx
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessEv
	movq	%rax, %rsi
	movl	$4, %edi
	call	_ZnwmPv
	movl	(%rbx), %edx
	movl	%edx, (%rax)
	nop
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2378:
	.size	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE15_M_init_functorERSt9_Any_dataOS1_St17integral_constantIbLb1EE, .-_ZNSt14_Function_base13_Base_managerI14FunctionObjectE15_M_init_functorERSt9_Any_dataOS1_St17integral_constantIbLb1EE
	.section	.text._ZNSt14_Function_base13_Base_managerI14FunctionObjectE14_M_get_pointerERKSt9_Any_data,"axG",@progbits,_ZNSt14_Function_base13_Base_managerI14FunctionObjectE14_M_get_pointerERKSt9_Any_data,comdat
	.weak	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE14_M_get_pointerERKSt9_Any_data
	.type	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE14_M_get_pointerERKSt9_Any_data, @function
_ZNSt14_Function_base13_Base_managerI14FunctionObjectE14_M_get_pointerERKSt9_Any_data:
.LFB2379:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt9_Any_data9_M_accessI14FunctionObjectEERKT_v
	movq	%rax, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt11__addressofIK14FunctionObjectEPT_RS2_
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2379:
	.size	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE14_M_get_pointerERKSt9_Any_data, .-_ZNSt14_Function_base13_Base_managerI14FunctionObjectE14_M_get_pointerERKSt9_Any_data
	.section	.text._ZSt10__invoke_rIiR14FunctionObjectJiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES6_EEE5valueES6_E4typeEOSB_DpOSC_,"axG",@progbits,_ZSt10__invoke_rIiR14FunctionObjectJiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES6_EEE5valueES6_E4typeEOSB_DpOSC_,comdat
	.weak	_ZSt10__invoke_rIiR14FunctionObjectJiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES6_EEE5valueES6_E4typeEOSB_DpOSC_
	.type	_ZSt10__invoke_rIiR14FunctionObjectJiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES6_EEE5valueES6_E4typeEOSB_DpOSC_, @function
_ZSt10__invoke_rIiR14FunctionObjectJiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES6_EEE5valueES6_E4typeEOSB_DpOSC_:
.LFB2380:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt7forwardIiEOT_RNSt16remove_referenceIS0_E4typeE
	movq	%rax, %rbx
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt7forwardIR14FunctionObjectEOT_RNSt16remove_referenceIS2_E4typeE
	movq	%rbx, %rsi
	movq	%rax, %rdi
	call	_ZSt13__invoke_implIiR14FunctionObjectJiEET_St14__invoke_otherOT0_DpOT1_
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2380:
	.size	_ZSt10__invoke_rIiR14FunctionObjectJiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES6_EEE5valueES6_E4typeEOSB_DpOSC_, .-_ZSt10__invoke_rIiR14FunctionObjectJiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES6_EEE5valueES6_E4typeEOSB_DpOSC_
	.section	.text._ZNSt9_Any_data9_M_accessIP14FunctionObjectEERT_v,"axG",@progbits,_ZNSt9_Any_data9_M_accessIP14FunctionObjectEERT_v,comdat
	.align 2
	.weak	_ZNSt9_Any_data9_M_accessIP14FunctionObjectEERT_v
	.type	_ZNSt9_Any_data9_M_accessIP14FunctionObjectEERT_v, @function
_ZNSt9_Any_data9_M_accessIP14FunctionObjectEERT_v:
.LFB2381:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessEv
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2381:
	.size	_ZNSt9_Any_data9_M_accessIP14FunctionObjectEERT_v, .-_ZNSt9_Any_data9_M_accessIP14FunctionObjectEERT_v
	.section	.text._ZNSt14_Function_base13_Base_managerI14FunctionObjectE10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation,"axG",@progbits,_ZNSt14_Function_base13_Base_managerI14FunctionObjectE10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation,comdat
	.weak	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation
	.type	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation, @function
_ZNSt14_Function_base13_Base_managerI14FunctionObjectE10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation:
.LFB2382:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$40, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movl	%edx, -36(%rbp)
	movl	-36(%rbp), %eax
	cmpl	$3, %eax
	je	.L125
	cmpl	$3, %eax
	jg	.L126
	cmpl	$2, %eax
	je	.L127
	cmpl	$2, %eax
	jg	.L126
	testl	%eax, %eax
	je	.L128
	cmpl	$1, %eax
	je	.L129
	jmp	.L126
.L128:
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessIPKSt9type_infoEERT_v
	leaq	_ZTI14FunctionObject(%rip), %rdx
	movq	%rdx, (%rax)
	jmp	.L126
.L129:
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessIP14FunctionObjectEERT_v
	movq	%rax, %rbx
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE14_M_get_pointerERKSt9_Any_data
	movq	%rax, (%rbx)
	jmp	.L126
.L127:
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE8_M_cloneERSt9_Any_dataRKS3_St17integral_constantIbLb1EE
	jmp	.L126
.L125:
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE
	nop
.L126:
	movl	$0, %eax
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2382:
	.size	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation, .-_ZNSt14_Function_base13_Base_managerI14FunctionObjectE10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation
	.text
	.type	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E15_M_init_functorERSt9_Any_dataOS1_St17integral_constantIbLb1EE, @function
_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E15_M_init_functorERSt9_Any_dataOS1_St17integral_constantIbLb1EE:
.LFB2383:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt4moveIRZ4mainEUliE_EONSt16remove_referenceIT_E4typeEOS3_
	movq	%rax, %rbx
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessEv
	movq	%rax, %rsi
	movl	$4, %edi
	call	_ZnwmPv
	movl	(%rbx), %edx
	movl	%edx, (%rax)
	nop
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2383:
	.size	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E15_M_init_functorERSt9_Any_dataOS1_St17integral_constantIbLb1EE, .-_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E15_M_init_functorERSt9_Any_dataOS1_St17integral_constantIbLb1EE
	.type	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E14_M_get_pointerERKSt9_Any_data, @function
_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E14_M_get_pointerERKSt9_Any_data:
.LFB2384:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$32, %rsp
	movq	%rdi, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt9_Any_data9_M_accessIZ4mainEUliE_EERKT_v
	movq	%rax, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt11__addressofIKZ4mainEUliE_EPT_RS2_
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2384:
	.size	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E14_M_get_pointerERKSt9_Any_data, .-_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E14_M_get_pointerERKSt9_Any_data
	.type	_ZSt10__invoke_rIiRZ4mainEUliE_JiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES6_EEE5valueES6_E4typeEOSB_DpOSC_, @function
_ZSt10__invoke_rIiRZ4mainEUliE_JiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES6_EEE5valueES6_E4typeEOSB_DpOSC_:
.LFB2385:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt7forwardIiEOT_RNSt16remove_referenceIS0_E4typeE
	movq	%rax, %rbx
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt7forwardIRZ4mainEUliE_EOT_RNSt16remove_referenceIS2_E4typeE
	movq	%rbx, %rsi
	movq	%rax, %rdi
	call	_ZSt13__invoke_implIiRZ4mainEUliE_JiEET_St14__invoke_otherOT0_DpOT1_
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2385:
	.size	_ZSt10__invoke_rIiRZ4mainEUliE_JiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES6_EEE5valueES6_E4typeEOSB_DpOSC_, .-_ZSt10__invoke_rIiRZ4mainEUliE_JiEENSt9enable_ifIXsrSt6__and_IJSt6__not_ISt7is_voidIT_EESt14is_convertibleINSt15__invoke_resultIT0_JDpT1_EE4typeES6_EEE5valueES6_E4typeEOSB_DpOSC_
	.align 2
	.type	_ZNSt9_Any_data9_M_accessIPZ4mainEUliE_EERT_v, @function
_ZNSt9_Any_data9_M_accessIPZ4mainEUliE_EERT_v:
.LFB2386:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessEv
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2386:
	.size	_ZNSt9_Any_data9_M_accessIPZ4mainEUliE_EERT_v, .-_ZNSt9_Any_data9_M_accessIPZ4mainEUliE_EERT_v
	.type	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation, @function
_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation:
.LFB2387:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$40, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movl	%edx, -36(%rbp)
	movl	-36(%rbp), %eax
	cmpl	$3, %eax
	je	.L139
	cmpl	$3, %eax
	jg	.L140
	cmpl	$2, %eax
	je	.L141
	cmpl	$2, %eax
	jg	.L140
	testl	%eax, %eax
	je	.L142
	cmpl	$1, %eax
	je	.L143
	jmp	.L140
.L142:
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessIPKSt9type_infoEERT_v
	leaq	_ZTIZ4mainEUliE_(%rip), %rdx
	movq	%rdx, (%rax)
	jmp	.L140
.L143:
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessIPZ4mainEUliE_EERT_v
	movq	%rax, %rbx
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E14_M_get_pointerERKSt9_Any_data
	movq	%rax, (%rbx)
	jmp	.L140
.L141:
	movq	-32(%rbp), %rdx
	movq	-24(%rbp), %rax
	movq	%rdx, %rsi
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E8_M_cloneERSt9_Any_dataRKS3_St17integral_constantIbLb1EE
	jmp	.L140
.L139:
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE
	nop
.L140:
	movl	$0, %eax
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2387:
	.size	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation, .-_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E10_M_managerERSt9_Any_dataRKS3_St18_Manager_operation
	.section	.text._ZNKSt9_Any_data9_M_accessIPFiiEEERKT_v,"axG",@progbits,_ZNKSt9_Any_data9_M_accessIPFiiEEERKT_v,comdat
	.align 2
	.weak	_ZNKSt9_Any_data9_M_accessIPFiiEEERKT_v
	.type	_ZNKSt9_Any_data9_M_accessIPFiiEEERKT_v, @function
_ZNKSt9_Any_data9_M_accessIPFiiEEERKT_v:
.LFB2422:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt9_Any_data9_M_accessEv
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2422:
	.size	_ZNKSt9_Any_data9_M_accessIPFiiEEERKT_v, .-_ZNKSt9_Any_data9_M_accessIPFiiEEERKT_v
	.section	.text._ZSt11__addressofIKPFiiEEPT_RS3_,"axG",@progbits,_ZSt11__addressofIKPFiiEEPT_RS3_,comdat
	.weak	_ZSt11__addressofIKPFiiEEPT_RS3_
	.type	_ZSt11__addressofIKPFiiEEPT_RS3_, @function
_ZSt11__addressofIKPFiiEEPT_RS3_:
.LFB2423:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2423:
	.size	_ZSt11__addressofIKPFiiEEPT_RS3_, .-_ZSt11__addressofIKPFiiEEPT_RS3_
	.section	.text._ZSt7forwardIRPFiiEEOT_RNSt16remove_referenceIS3_E4typeE,"axG",@progbits,_ZSt7forwardIRPFiiEEOT_RNSt16remove_referenceIS3_E4typeE,comdat
	.weak	_ZSt7forwardIRPFiiEEOT_RNSt16remove_referenceIS3_E4typeE
	.type	_ZSt7forwardIRPFiiEEOT_RNSt16remove_referenceIS3_E4typeE, @function
_ZSt7forwardIRPFiiEEOT_RNSt16remove_referenceIS3_E4typeE:
.LFB2425:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2425:
	.size	_ZSt7forwardIRPFiiEEOT_RNSt16remove_referenceIS3_E4typeE, .-_ZSt7forwardIRPFiiEEOT_RNSt16remove_referenceIS3_E4typeE
	.section	.text._ZSt13__invoke_implIiRPFiiEJiEET_St14__invoke_otherOT0_DpOT1_,"axG",@progbits,_ZSt13__invoke_implIiRPFiiEJiEET_St14__invoke_otherOT0_DpOT1_,comdat
	.weak	_ZSt13__invoke_implIiRPFiiEJiEET_St14__invoke_otherOT0_DpOT1_
	.type	_ZSt13__invoke_implIiRPFiiEJiEET_St14__invoke_otherOT0_DpOT1_, @function
_ZSt13__invoke_implIiRPFiiEJiEET_St14__invoke_otherOT0_DpOT1_:
.LFB2426:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt7forwardIRPFiiEEOT_RNSt16remove_referenceIS3_E4typeE
	movq	(%rax), %rbx
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt7forwardIiEOT_RNSt16remove_referenceIS0_E4typeE
	movl	(%rax), %eax
	movl	%eax, %edi
	call	*%rbx
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2426:
	.size	_ZSt13__invoke_implIiRPFiiEJiEET_St14__invoke_otherOT0_DpOT1_, .-_ZSt13__invoke_implIiRPFiiEJiEET_St14__invoke_otherOT0_DpOT1_
	.section	.text._ZNSt14_Function_base13_Base_managerIPFiiEE8_M_cloneERSt9_Any_dataRKS4_St17integral_constantIbLb1EE,"axG",@progbits,_ZNSt14_Function_base13_Base_managerIPFiiEE8_M_cloneERSt9_Any_dataRKS4_St17integral_constantIbLb1EE,comdat
	.weak	_ZNSt14_Function_base13_Base_managerIPFiiEE8_M_cloneERSt9_Any_dataRKS4_St17integral_constantIbLb1EE
	.type	_ZNSt14_Function_base13_Base_managerIPFiiEE8_M_cloneERSt9_Any_dataRKS4_St17integral_constantIbLb1EE, @function
_ZNSt14_Function_base13_Base_managerIPFiiEE8_M_cloneERSt9_Any_dataRKS4_St17integral_constantIbLb1EE:
.LFB2427:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt9_Any_data9_M_accessIPFiiEEERKT_v
	movq	(%rax), %rbx
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessEv
	movq	%rax, %rsi
	movl	$8, %edi
	call	_ZnwmPv
	movq	%rbx, (%rax)
	nop
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2427:
	.size	_ZNSt14_Function_base13_Base_managerIPFiiEE8_M_cloneERSt9_Any_dataRKS4_St17integral_constantIbLb1EE, .-_ZNSt14_Function_base13_Base_managerIPFiiEE8_M_cloneERSt9_Any_dataRKS4_St17integral_constantIbLb1EE
	.section	.text._ZNSt14_Function_base13_Base_managerIPFiiEE10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE,"axG",@progbits,_ZNSt14_Function_base13_Base_managerIPFiiEE10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE,comdat
	.weak	_ZNSt14_Function_base13_Base_managerIPFiiEE10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE
	.type	_ZNSt14_Function_base13_Base_managerIPFiiEE10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE, @function
_ZNSt14_Function_base13_Base_managerIPFiiEE10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE:
.LFB2428:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessIPFiiEEERT_v
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2428:
	.size	_ZNSt14_Function_base13_Base_managerIPFiiEE10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE, .-_ZNSt14_Function_base13_Base_managerIPFiiEE10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE
	.section	.text._ZNKSt9_Any_data9_M_accessI14FunctionObjectEERKT_v,"axG",@progbits,_ZNKSt9_Any_data9_M_accessI14FunctionObjectEERKT_v,comdat
	.align 2
	.weak	_ZNKSt9_Any_data9_M_accessI14FunctionObjectEERKT_v
	.type	_ZNKSt9_Any_data9_M_accessI14FunctionObjectEERKT_v, @function
_ZNKSt9_Any_data9_M_accessI14FunctionObjectEERKT_v:
.LFB2429:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt9_Any_data9_M_accessEv
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2429:
	.size	_ZNKSt9_Any_data9_M_accessI14FunctionObjectEERKT_v, .-_ZNKSt9_Any_data9_M_accessI14FunctionObjectEERKT_v
	.section	.text._ZSt11__addressofIK14FunctionObjectEPT_RS2_,"axG",@progbits,_ZSt11__addressofIK14FunctionObjectEPT_RS2_,comdat
	.weak	_ZSt11__addressofIK14FunctionObjectEPT_RS2_
	.type	_ZSt11__addressofIK14FunctionObjectEPT_RS2_, @function
_ZSt11__addressofIK14FunctionObjectEPT_RS2_:
.LFB2430:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2430:
	.size	_ZSt11__addressofIK14FunctionObjectEPT_RS2_, .-_ZSt11__addressofIK14FunctionObjectEPT_RS2_
	.section	.text._ZSt7forwardIR14FunctionObjectEOT_RNSt16remove_referenceIS2_E4typeE,"axG",@progbits,_ZSt7forwardIR14FunctionObjectEOT_RNSt16remove_referenceIS2_E4typeE,comdat
	.weak	_ZSt7forwardIR14FunctionObjectEOT_RNSt16remove_referenceIS2_E4typeE
	.type	_ZSt7forwardIR14FunctionObjectEOT_RNSt16remove_referenceIS2_E4typeE, @function
_ZSt7forwardIR14FunctionObjectEOT_RNSt16remove_referenceIS2_E4typeE:
.LFB2432:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2432:
	.size	_ZSt7forwardIR14FunctionObjectEOT_RNSt16remove_referenceIS2_E4typeE, .-_ZSt7forwardIR14FunctionObjectEOT_RNSt16remove_referenceIS2_E4typeE
	.section	.text._ZSt13__invoke_implIiR14FunctionObjectJiEET_St14__invoke_otherOT0_DpOT1_,"axG",@progbits,_ZSt13__invoke_implIiR14FunctionObjectJiEET_St14__invoke_otherOT0_DpOT1_,comdat
	.weak	_ZSt13__invoke_implIiR14FunctionObjectJiEET_St14__invoke_otherOT0_DpOT1_
	.type	_ZSt13__invoke_implIiR14FunctionObjectJiEET_St14__invoke_otherOT0_DpOT1_, @function
_ZSt13__invoke_implIiR14FunctionObjectJiEET_St14__invoke_otherOT0_DpOT1_:
.LFB2433:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt7forwardIR14FunctionObjectEOT_RNSt16remove_referenceIS2_E4typeE
	movq	%rax, %rbx
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt7forwardIiEOT_RNSt16remove_referenceIS0_E4typeE
	movl	(%rax), %eax
	movl	%eax, %esi
	movq	%rbx, %rdi
	call	_ZN14FunctionObjectclEi
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2433:
	.size	_ZSt13__invoke_implIiR14FunctionObjectJiEET_St14__invoke_otherOT0_DpOT1_, .-_ZSt13__invoke_implIiR14FunctionObjectJiEET_St14__invoke_otherOT0_DpOT1_
	.section	.text._ZNSt14_Function_base13_Base_managerI14FunctionObjectE8_M_cloneERSt9_Any_dataRKS3_St17integral_constantIbLb1EE,"axG",@progbits,_ZNSt14_Function_base13_Base_managerI14FunctionObjectE8_M_cloneERSt9_Any_dataRKS3_St17integral_constantIbLb1EE,comdat
	.weak	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE8_M_cloneERSt9_Any_dataRKS3_St17integral_constantIbLb1EE
	.type	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE8_M_cloneERSt9_Any_dataRKS3_St17integral_constantIbLb1EE, @function
_ZNSt14_Function_base13_Base_managerI14FunctionObjectE8_M_cloneERSt9_Any_dataRKS3_St17integral_constantIbLb1EE:
.LFB2434:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt9_Any_data9_M_accessI14FunctionObjectEERKT_v
	movq	%rax, %rbx
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessEv
	movq	%rax, %rsi
	movl	$4, %edi
	call	_ZnwmPv
	movl	(%rbx), %edx
	movl	%edx, (%rax)
	nop
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2434:
	.size	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE8_M_cloneERSt9_Any_dataRKS3_St17integral_constantIbLb1EE, .-_ZNSt14_Function_base13_Base_managerI14FunctionObjectE8_M_cloneERSt9_Any_dataRKS3_St17integral_constantIbLb1EE
	.section	.text._ZNSt14_Function_base13_Base_managerI14FunctionObjectE10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE,"axG",@progbits,_ZNSt14_Function_base13_Base_managerI14FunctionObjectE10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE,comdat
	.weak	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE
	.type	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE, @function
_ZNSt14_Function_base13_Base_managerI14FunctionObjectE10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE:
.LFB2435:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessI14FunctionObjectEERT_v
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2435:
	.size	_ZNSt14_Function_base13_Base_managerI14FunctionObjectE10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE, .-_ZNSt14_Function_base13_Base_managerI14FunctionObjectE10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE
	.text
	.align 2
	.type	_ZNKSt9_Any_data9_M_accessIZ4mainEUliE_EERKT_v, @function
_ZNKSt9_Any_data9_M_accessIZ4mainEUliE_EERKT_v:
.LFB2436:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt9_Any_data9_M_accessEv
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2436:
	.size	_ZNKSt9_Any_data9_M_accessIZ4mainEUliE_EERKT_v, .-_ZNKSt9_Any_data9_M_accessIZ4mainEUliE_EERKT_v
	.type	_ZSt11__addressofIKZ4mainEUliE_EPT_RS2_, @function
_ZSt11__addressofIKZ4mainEUliE_EPT_RS2_:
.LFB2437:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2437:
	.size	_ZSt11__addressofIKZ4mainEUliE_EPT_RS2_, .-_ZSt11__addressofIKZ4mainEUliE_EPT_RS2_
	.type	_ZSt7forwardIRZ4mainEUliE_EOT_RNSt16remove_referenceIS2_E4typeE, @function
_ZSt7forwardIRZ4mainEUliE_EOT_RNSt16remove_referenceIS2_E4typeE:
.LFB2439:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2439:
	.size	_ZSt7forwardIRZ4mainEUliE_EOT_RNSt16remove_referenceIS2_E4typeE, .-_ZSt7forwardIRZ4mainEUliE_EOT_RNSt16remove_referenceIS2_E4typeE
	.type	_ZSt13__invoke_implIiRZ4mainEUliE_JiEET_St14__invoke_otherOT0_DpOT1_, @function
_ZSt13__invoke_implIiRZ4mainEUliE_JiEET_St14__invoke_otherOT0_DpOT1_:
.LFB2440:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt7forwardIRZ4mainEUliE_EOT_RNSt16remove_referenceIS2_E4typeE
	movq	%rax, %rbx
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZSt7forwardIiEOT_RNSt16remove_referenceIS0_E4typeE
	movl	(%rax), %eax
	movl	%eax, %esi
	movq	%rbx, %rdi
	call	_ZZ4mainENKUliE_clEi
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2440:
	.size	_ZSt13__invoke_implIiRZ4mainEUliE_JiEET_St14__invoke_otherOT0_DpOT1_, .-_ZSt13__invoke_implIiRZ4mainEUliE_JiEET_St14__invoke_otherOT0_DpOT1_
	.type	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E8_M_cloneERSt9_Any_dataRKS3_St17integral_constantIbLb1EE, @function
_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E8_M_cloneERSt9_Any_dataRKS3_St17integral_constantIbLb1EE:
.LFB2441:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$24, %rsp
	.cfi_offset 3, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNKSt9_Any_data9_M_accessIZ4mainEUliE_EERKT_v
	movq	%rax, %rbx
	movq	-24(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessEv
	movq	%rax, %rsi
	movl	$4, %edi
	call	_ZnwmPv
	movl	(%rbx), %edx
	movl	%edx, (%rax)
	nop
	movq	-8(%rbp), %rbx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2441:
	.size	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E8_M_cloneERSt9_Any_dataRKS3_St17integral_constantIbLb1EE, .-_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E8_M_cloneERSt9_Any_dataRKS3_St17integral_constantIbLb1EE
	.type	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE, @function
_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE:
.LFB2442:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessIZ4mainEUliE_EERT_v
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2442:
	.size	_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE, .-_ZNSt14_Function_base13_Base_managerIZ4mainEUliE_E10_M_destroyERSt9_Any_dataSt17integral_constantIbLb1EE
	.section	.text._ZNSt9_Any_data9_M_accessIPFiiEEERT_v,"axG",@progbits,_ZNSt9_Any_data9_M_accessIPFiiEEERT_v,comdat
	.align 2
	.weak	_ZNSt9_Any_data9_M_accessIPFiiEEERT_v
	.type	_ZNSt9_Any_data9_M_accessIPFiiEEERT_v, @function
_ZNSt9_Any_data9_M_accessIPFiiEEERT_v:
.LFB2472:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessEv
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2472:
	.size	_ZNSt9_Any_data9_M_accessIPFiiEEERT_v, .-_ZNSt9_Any_data9_M_accessIPFiiEEERT_v
	.section	.text._ZNSt9_Any_data9_M_accessI14FunctionObjectEERT_v,"axG",@progbits,_ZNSt9_Any_data9_M_accessI14FunctionObjectEERT_v,comdat
	.align 2
	.weak	_ZNSt9_Any_data9_M_accessI14FunctionObjectEERT_v
	.type	_ZNSt9_Any_data9_M_accessI14FunctionObjectEERT_v, @function
_ZNSt9_Any_data9_M_accessI14FunctionObjectEERT_v:
.LFB2473:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessEv
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2473:
	.size	_ZNSt9_Any_data9_M_accessI14FunctionObjectEERT_v, .-_ZNSt9_Any_data9_M_accessI14FunctionObjectEERT_v
	.text
	.align 2
	.type	_ZNSt9_Any_data9_M_accessIZ4mainEUliE_EERT_v, @function
_ZNSt9_Any_data9_M_accessIZ4mainEUliE_EERT_v:
.LFB2474:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	%rax, %rdi
	call	_ZNSt9_Any_data9_M_accessEv
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2474:
	.size	_ZNSt9_Any_data9_M_accessIZ4mainEUliE_EERT_v, .-_ZNSt9_Any_data9_M_accessIZ4mainEUliE_EERT_v
	.section	.rodata
	.align 8
	.type	_ZTSZ4mainEUliE_, @object
	.size	_ZTSZ4mainEUliE_, 14
_ZTSZ4mainEUliE_:
	.string	"*Z4mainEUliE_"
	.section	.data.rel.ro,"aw"
	.align 8
	.type	_ZTIZ4mainEUliE_, @object
	.size	_ZTIZ4mainEUliE_, 16
_ZTIZ4mainEUliE_:
	.quad	_ZTVN10__cxxabiv117__class_type_infoE+16
	.quad	_ZTSZ4mainEUliE_
	.weak	_ZTI14FunctionObject
	.section	.data.rel.ro._ZTI14FunctionObject,"awG",@progbits,_ZTI14FunctionObject,comdat
	.align 8
	.type	_ZTI14FunctionObject, @object
	.size	_ZTI14FunctionObject, 16
_ZTI14FunctionObject:
	.quad	_ZTVN10__cxxabiv117__class_type_infoE+16
	.quad	_ZTS14FunctionObject
	.weak	_ZTS14FunctionObject
	.section	.rodata._ZTS14FunctionObject,"aG",@progbits,_ZTS14FunctionObject,comdat
	.align 16
	.type	_ZTS14FunctionObject, @object
	.size	_ZTS14FunctionObject, 17
_ZTS14FunctionObject:
	.string	"14FunctionObject"
	.weak	_ZTIPFiiE
	.section	.data.rel.ro._ZTIPFiiE,"awG",@progbits,_ZTIPFiiE,comdat
	.align 8
	.type	_ZTIPFiiE, @object
	.size	_ZTIPFiiE, 32
_ZTIPFiiE:
	.quad	_ZTVN10__cxxabiv119__pointer_type_infoE+16
	.quad	_ZTSPFiiE
	.long	0
	.zero	4
	.quad	_ZTIFiiE
	.weak	_ZTSPFiiE
	.section	.rodata._ZTSPFiiE,"aG",@progbits,_ZTSPFiiE,comdat
	.type	_ZTSPFiiE, @object
	.size	_ZTSPFiiE, 6
_ZTSPFiiE:
	.string	"PFiiE"
	.text
	.type	_Z41__static_initialization_and_destruction_0ii, @function
_Z41__static_initialization_and_destruction_0ii:
.LFB2517:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	%esi, -8(%rbp)
	cmpl	$1, -4(%rbp)
	jne	.L183
	cmpl	$65535, -8(%rbp)
	jne	.L183
	leaq	_ZStL8__ioinit(%rip), %rdi
	call	_ZNSt8ios_base4InitC1Ev@PLT
	leaq	__dso_handle(%rip), %rdx
	leaq	_ZStL8__ioinit(%rip), %rsi
	movq	_ZNSt8ios_base4InitD1Ev@GOTPCREL(%rip), %rax
	movq	%rax, %rdi
	call	__cxa_atexit@PLT
.L183:
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2517:
	.size	_Z41__static_initialization_and_destruction_0ii, .-_Z41__static_initialization_and_destruction_0ii
	.weak	_ZTIFiiE
	.section	.data.rel.ro._ZTIFiiE,"awG",@progbits,_ZTIFiiE,comdat
	.align 8
	.type	_ZTIFiiE, @object
	.size	_ZTIFiiE, 16
_ZTIFiiE:
	.quad	_ZTVN10__cxxabiv120__function_type_infoE+16
	.quad	_ZTSFiiE
	.weak	_ZTSFiiE
	.section	.rodata._ZTSFiiE,"aG",@progbits,_ZTSFiiE,comdat
	.type	_ZTSFiiE, @object
	.size	_ZTSFiiE, 5
_ZTSFiiE:
	.string	"FiiE"
	.text
	.type	_GLOBAL__sub_I__Z8functioni, @function
_GLOBAL__sub_I__Z8functioni:
.LFB2518:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movl	$65535, %esi
	movl	$1, %edi
	call	_Z41__static_initialization_and_destruction_0ii
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2518:
	.size	_GLOBAL__sub_I__Z8functioni, .-_GLOBAL__sub_I__Z8functioni
	.section	.init_array,"aw"
	.align 8
	.quad	_GLOBAL__sub_I__Z8functioni
	.hidden	DW.ref.__gxx_personality_v0
	.weak	DW.ref.__gxx_personality_v0
	.section	.data.rel.local.DW.ref.__gxx_personality_v0,"awG",@progbits,DW.ref.__gxx_personality_v0,comdat
	.align 8
	.type	DW.ref.__gxx_personality_v0, @object
	.size	DW.ref.__gxx_personality_v0, 8
DW.ref.__gxx_personality_v0:
	.quad	__gxx_personality_v0
	.hidden	__dso_handle
	.ident	"GCC: (GNU) 10.2.0"
	.section	.note.GNU-stack,"",@progbits
