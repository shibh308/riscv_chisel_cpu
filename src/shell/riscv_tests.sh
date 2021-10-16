#!/bin/bash

UI_INSTS=(sw lw add addi sub and andi or ori xor xori sll srl sra slli srli srai slt sltu slti sltiu beq bne blt bge bltu bgeu jal jalr lui auipc)
MI_INSTS=(csr scall)

WORK_DIR=/opt/riscv/riscv_chisel_cpu
RESULT_DIR=$WORK_DIR/results
mkdir -p $RESULT_DIR
mkdir -p $WORK_DIR/src/main/scala/auto_generate
cd $WORK_DIR

function loop_test(){
    INSTS=${!1}
    ISA=$2
    PACKAGE_NAME=mycpu
    sed -e "s/{package}/$PACKAGE_NAME/" $WORK_DIR/src/templates/Core.scala > $WORK_DIR/src/main/scala/auto_generate/Core.scala
    
    for INST in ${INSTS[@]}
    do
        echo $INST
        sed -e "s/{package}/$PACKAGE_NAME/" -e "s/{isa}/$ISA/" -e "s/{inst}/$INST/" $WORK_DIR/src/templates/Memory.scala > $WORK_DIR/src/main/scala/auto_generate/Memory.scala
        sbt "testOnly $PACKAGE_NAME.RiscvTest" > $RESULT_DIR/$INST.txt
    done
}

PACKAGE_NAME=$1
DIRECTORY_NAME=$2
loop_test UI_INSTS[@] "ui"
loop_test MI_INSTS[@] "mi"