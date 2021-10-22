package {package}

import chisel3._
import chisel3.util._

import common.Consts._


class Top extends Module {
    val io = IO(new Bundle {
        val gp = Output(UInt(WORD_LEN.W))
        val exit = Output(Bool())
    })

    // Moduleで囲むとハードウェア化
    val core = Module(new Core())
    val memory = Module(new Memory())
    val serial = Module(new Serial())

    // CoreとMemoryのimemを繋げてる
    core.io.imem <> memory.io.imem
    core.io.dmem <> memory.io.dmem

    memory.io.serial <> serial.io.input
    serial.io.clk := (core.io.cnt(3,0) === BitPat("b0000"))

    io.gp := core.io.gp
    io.exit := core.io.exit
}

object Elaborate extends App {
    val rtl = (new stage.ChiselStage).emitVerilog(new Top)
    print(rtl)
}