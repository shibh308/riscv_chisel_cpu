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

    // CoreとMemoryのimemを繋げてる
    core.io.imem <> memory.io.imem
    core.io.dmem <> memory.io.dmem

    io.gp := core.io.gp
    io.exit := core.io.exit
}