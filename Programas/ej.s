.data
	.align 8
.align 1
.globl __stginit_Main
.type __stginit_Main, @object
__stginit_Main:
.globl __stginit_ZCMain
.type __stginit_ZCMain, @object
__stginit_ZCMain:
.section .data
	.align 8
.align 1
six_srt:
	.quad	ghczmprim_GHCziCString_unpackCStringzh_closure
.data
	.align 8
.align 1
six_closure:
	.quad	six_info
	.quad	0
	.quad	0
	.quad	0
.section .rodata
	.align 8
.align 1
ciJ_str:
	.byte	72
	.byte	111
	.byte	108
	.byte	97
	.byte	0
.text
	.align 8
	.long	six_srt-(six_info)+0
	.long	0
	.quad	0
	.quad	4294967318
six_info:
.LciN:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LciP
	addq $16,%r12
	cmpq 144(%r13),%r12
	ja .LciR
	movq $stg_CAF_BLACKHOLE_info,-8(%r12)
	movq 160(%r13),%rax
	movq %rax,0(%r12)
	movq %r13,%rdi
	movq %rbx,%rsi
	leaq -8(%r12),%rdx
	subq $8,%rsp
	movl $0,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .LciS
.LciT:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	leaq -8(%r12),%rax
	movq %rax,-8(%rbp)
	movl $ghczmprim_GHCziCString_unpackCStringzh_closure,%ebx
	movl $ciJ_str,%r14d
	addq $-16,%rbp
	jmp stg_ap_n_fast
.LciR:
	movq $16,192(%r13)
.LciP:
	jmp *-16(%r13)
.LciS:
	jmp *(%rbx)
	.size six_info, .-six_info
.section .data
	.align 8
.align 1
Main_main_srt:
	.quad	base_SystemziIO_putStrLn_closure
	.quad	six_closure
.data
	.align 8
.align 1
.globl Main_main_closure
.type Main_main_closure, @object
Main_main_closure:
	.quad	Main_main_info
	.quad	0
	.quad	0
	.quad	0
.text
	.align 8
	.long	Main_main_srt-(Main_main_info)+0
	.long	0
	.quad	0
	.quad	12884901910
.globl Main_main_info
.type Main_main_info, @object
Main_main_info:
.Lcjb:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .Lcjd
	addq $16,%r12
	cmpq 144(%r13),%r12
	ja .Lcjf
	movq $stg_CAF_BLACKHOLE_info,-8(%r12)
	movq 160(%r13),%rax
	movq %rax,0(%r12)
	movq %r13,%rdi
	movq %rbx,%rsi
	leaq -8(%r12),%rdx
	subq $8,%rsp
	movl $0,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .Lcjg
.Lcjh:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	leaq -8(%r12),%rax
	movq %rax,-8(%rbp)
	movl $base_SystemziIO_putStrLn_closure,%ebx
	movl $six_closure,%r14d
	addq $-16,%rbp
	jmp stg_ap_p_fast
.Lcjf:
	movq $16,192(%r13)
.Lcjd:
	jmp *-16(%r13)
.Lcjg:
	jmp *(%rbx)
	.size Main_main_info, .-Main_main_info
.section .data
	.align 8
.align 1
ZCMain_main_srt:
	.quad	base_GHCziTopHandler_runMainIO_closure
	.quad	Main_main_closure
.data
	.align 8
.align 1
.globl ZCMain_main_closure
.type ZCMain_main_closure, @object
ZCMain_main_closure:
	.quad	ZCMain_main_info
	.quad	0
	.quad	0
	.quad	0
.text
	.align 8
	.long	ZCMain_main_srt-(ZCMain_main_info)+0
	.long	0
	.quad	0
	.quad	12884901910
.globl ZCMain_main_info
.type ZCMain_main_info, @object
ZCMain_main_info:
.Lcjz:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcjB
	addq $16,%r12
	cmpq 144(%r13),%r12
	ja .LcjD
	movq $stg_CAF_BLACKHOLE_info,-8(%r12)
	movq 160(%r13),%rax
	movq %rax,0(%r12)
	movq %r13,%rdi
	movq %rbx,%rsi
	leaq -8(%r12),%rdx
	subq $8,%rsp
	movl $0,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je .LcjE
.LcjF:
	movq $stg_bh_upd_frame_info,-16(%rbp)
	leaq -8(%r12),%rax
	movq %rax,-8(%rbp)
	movl $base_GHCziTopHandler_runMainIO_closure,%ebx
	movl $Main_main_closure,%r14d
	addq $-16,%rbp
	jmp stg_ap_p_fast
.LcjD:
	movq $16,192(%r13)
.LcjB:
	jmp *-16(%r13)
.LcjE:
	jmp *(%rbx)
	.size ZCMain_main_info, .-ZCMain_main_info
.section .note.GNU-stack,"",@progbits
.ident "GHC 7.6.3"
