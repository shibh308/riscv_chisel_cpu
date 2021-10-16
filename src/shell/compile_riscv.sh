riscv64-unknown-elf-gcc -march=rv32i -mabi=ilp32 -c -o $1.o $1.c
riscv64-unknown-elf-ld -b elf32-littleriscv $1.o -T $2 -o $1
riscv64-unknown-elf-objcopy -O binary $1 $1.bin
od -An -tx1 -w1 -v $1.bin > $1.hex
riscv64-unknown-elf-objdump -b elf32-littleriscv -D $1 > $1.dump