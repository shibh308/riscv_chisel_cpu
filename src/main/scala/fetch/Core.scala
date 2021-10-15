package fetch

import chisel3._
import chisel3.util._

import common.Consts._

class Core extends Module {
    val io = IO(new Bundle {
        val imem = Flipped(new ImemPortIo())
        // 終了判定
        val exit = Output(Bool())
    })

    // レジスタは32本でそれぞれ32bit
    val regfile = Mem(NUM_REG, UInt(WORD_LEN.W))

    // プログラムカウンタ
    val pc_reg = RegInit(START_ADDR);

    pc_reg := pc_reg + 4.U(WORD_LEN.W)

    // アドレス指定して値を受け取る感じ(Topを見るとMemoryに繋がっている事が分かる)
    io.imem.addr := pc_reg
    val inst = io.imem.inst

    printf(p"pc_reg: 0x${Hexadecimal(pc_reg)}\n")
    printf(p"inst  : 0x${Hexadecimal(inst)}\n")
    printf("----------------\n")

    // 終了判定
    io.exit := (inst === 0x34333231.U(WORD_LEN.W))
}