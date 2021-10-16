#!/bin/bash

UI_INSTS=(sw lw add addi sub and andi or ori xor xori sll srl sra slli srli srai slt sltu slti sltiu beq bne blt bge bltu bgeu jal jalr lui auipc)
MI_INSTS=(csr scall)

WORK_DIR=/opt/riscv/riscv_chisel_cpu
RESULT_DIR=$WORK_DIR/results


function loop_show_result(){
    INSTS=${!1}
    for INST in ${INSTS[@]}
    do
        last=$(tail -n 1 $RESULT_DIR/$INST.txt)
        if [[ $last =~ ^\[success\]* ]] ;
        then
            printf "%-5s : passed\n" $INST
        else
            printf "%-5s : failed\n" $INST
        fi
    done
}

loop_show_result UI_INSTS[@] "ui"
loop_show_result MI_INSTS[@] "mi"