    .globl _scheme_entry
_scheme_entry:
    pushq %rbx
    pushq %rbp
    pushq %r12
    pushq %r13
    pushq %r14
    pushq %r15
    movq %rdi, %rbp
    movq %rsi, %rdx
    leaq _scheme_exit(%rip), %r15
    movq $3, %r8
    movq $10, %r9
L1:
    cmpq $1, %r8
    jne L9
L8:
    jmp L6
L9:
    cmpq $1000, %r9
    jle L7
L6:
    movq %r9, %rax
    jmp *%r15
L7:
    imulq $2, %r9
    movq %r8, %rax
    addq $1, %rax
    cmpq $0, %rax
    jne L4
L3:
    addq $1, %r9
    jmp L5
L4:
L5:
    sarq $1, %r8
    jmp L1
_scheme_exit:
    popq %r15
    popq %r14
    popq %r13
    popq %r12
    popq %rbp
    popq %rbx
    ret
