package {package}

import chisel3._
import chisel3.util._

import common.Consts._

class ShiftReg extends Module {
    val io = IO(new Bundle {
        val input = Input(Bool())
        val output = Output(UInt(10.W))
        val clk = Input(Bool())
    })
    val reg = RegInit(UInt(10.W), "b1111111111".U)
    reg := Mux(io.clk, Cat(reg, io.input), reg)
    io.output := Mux(io.clk, reg, "b1111111111".U)
}

class Serial extends Module {
    val io = IO(new Bundle {
        val input = Input(UInt(8.W))
        val cnt = Input(UInt(COUNTER_LEN.W))
    })
    val reg = Module(new ShiftReg())

    when(io.input =/= 0.U(8.W)){
        printf(p"serial_output: ${io.input}\n")
    }

    // -------------------- 以下TODO ----------------
    reg.io.input := io.input(0) // TODO: ここの処理は8bitずつまとめて入れるべき

    // クロックは後でうまくやる
    reg.io.clk := (io.cnt(3,0) === BitPat("b0000"))

    val uart_res = reg.io.output

    when(uart_res === BitPat("b0????????1")) {
        val pat = uart_res(8, 1)
    }

}