.data
argument: .word 7 # number to find
msg1: .string "Factorial value of"
msg2: .string " is "

.text
main:
    lw        a0, argument
    jal       ra, fact

    # print the result.
    mv        a1, a0
    lw        a0, argument
    jal       ra, printResult

    # exit program
    li        a0, 10
    ecall

fact:
    # push sp
    addi      sp, sp, -16
    sw        ra, 8(sp)
    sw        a0, 0(sp)

    addi      t0, a0, -1
    bge       t0, zero, nfact

    # restore sp
    addi      a0, zero, 1
    addi      sp, sp, 16
    jalr      x0, x1, 0

nfact:
    addi      a0, a0, -1
    jal       ra, fact

    addi      t1, a0, 0
    lw        a0, 0(sp)
    lw        ra, 8(sp)
    addi      sp, sp, 16

    mul       a0, a0, t1
    ret

printResult:
    mv        t0, a0
    mv        t1, a1

    la        a1, msg1
    li        a0, 4
    ecall

    mv        a1, t0
    li        a0, 1
    ecall

    la        a1, msg2
    li        a0, 4
    ecall

    mv        a1, t1
    li        a0, 1
    ecall

    ret




