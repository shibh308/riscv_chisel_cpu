#!/bin/bash

UI_INSTS=(sw lw add addi sub and andi or ori xor xori sll srl sra slli srli srai slt sltu slti sltiu beq bne blt bge bltu bgeu jal jalr lui auipc)
MI_INSTS=(csr scall)

PACKAGE_NAME=riscv_test
WORK_DIR=/opt/riscv/riscv_chisel_cpu
RESULT_DIR=$WORK_DIR/results
READ_DIR=$WORK_DIR/src/templates
WRITE_DIR=$WORK_DIR/src/main/scala/auto_generate/$PACKAGE_NAME

mkdir -p $RESULT_DIR
mkdir -p $WRITE_DIR
cd $WORK_DIR

function loop_test(){
    INSTS=${!1}
    ISA=$2
    sed -e "s/{package}/$PACKAGE_NAME/" -e "s/{exit}/(me_reg_pc === 0x44.U(WORD_LEN.W))/" $READ_DIR/Core.scala > $WRITE_DIR/Core.scala
    sed -e "s/{package}/$PACKAGE_NAME/" $READ_DIR/Top.scala > $WRITE_DIR/Top.scala
    
    for INST in ${INSTS[@]}
    do
        LOAD_PATH=src/riscv-tests/rv32${ISA}-p-${INST}.hex
        sed -e "s/{package}/$PACKAGE_NAME/" -e "s%{load_path}%$LOAD_PATH%" $READ_DIR/Memory.scala > $WRITE_DIR/Memory.scala
        sbt "testOnly $PACKAGE_NAME.RiscvTest" > $RESULT_DIR/$INST.txt
        last=$(tail -n 1 $RESULT_DIR/$INST.txt)
        if [[ $last =~ ^\[success\]* ]] ;
        then
            printf "%-5s : passed\n" $INST
        else
            printf "%-5s : failed\n" $INST
        fi
    done
}

loop_test UI_INSTS[@] "ui"
loop_test MI_INSTS[@] "mi"
