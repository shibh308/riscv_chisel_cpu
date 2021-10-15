package fetch

import chisel3._
import chisel3.util._


class Top extends Module {
    val io = IO(new Bundle {
        val exit = Output(Bool())
    })

    // Moduleで囲むとハードウェア化
    val core = Module(new Core())
    val memory = Module(new Memory())

    // CoreとMemoryのimemを繋げてる
    core.io.imem <> memory.io.imem

    io.exit := core.io.exit
}