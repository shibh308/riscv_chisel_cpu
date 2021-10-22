BASENAME=${1%.*}
echo $BASENAME

PACKAGE_NAME=ctest
WORK_DIR=/opt/riscv/riscv_chisel_cpu
RESULT_DIR=$WORK_DIR/results
READ_DIR=$WORK_DIR/src/templates
WRITE_DIR=$WORK_DIR/src/main/scala/auto_generate/$PACKAGE_NAME

LOAD_PATH=$BASENAME.hex
LD_PATH=src/c/link.ld

mkdir -p $WRITE_DIR
cd $WORK_DIR

sed -e "s/{package}/$PACKAGE_NAME/" -e "s/{exit}/(inst === UNIMP)/" $READ_DIR/Core.scala > $WRITE_DIR/Core.scala
sed -e "s/{package}/$PACKAGE_NAME/" $READ_DIR/Top.scala > $WRITE_DIR/Top.scala
sed -e "s/{package}/$PACKAGE_NAME/" $READ_DIR/Serial.scala > $WRITE_DIR/Serial.scala
sed -e "s/{package}/$PACKAGE_NAME/" -e "s%{load_path}%$LOAD_PATH%" $READ_DIR/Memory.scala > $WRITE_DIR/Memory.scala

riscv64-unknown-elf-gcc -march=rv32i -mabi=ilp32 -c -o $BASENAME.o $BASENAME.c
riscv64-unknown-elf-ld -b elf32-littleriscv $BASENAME.o -T $LD_PATH -o $BASENAME
riscv64-unknown-elf-objcopy -O binary $BASENAME $BASENAME.bin
od -An -tx1 -w1 -v $BASENAME.bin > $BASENAME.hex
riscv64-unknown-elf-objdump -b elf32-littleriscv -D $BASENAME > $BASENAME.dump

sbt "testOnly ctest.HexTest"