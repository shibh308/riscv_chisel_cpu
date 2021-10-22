package {package}

import chisel3._
import chisel3.util._

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
        val input = Input(Bool())
        val clk = Input(Bool())
    })
    val reg = Module(new ShiftReg())
    reg.io.input := io.input
    reg.io.clk := io.clk

    val uart_res = reg.io.output

    when(uart_res === BitPat("b0????????1")) {
        val pat = uart_res(8, 1)
        printf(p"target  : ${Hexadecimal(pat)}\n")
    }

    printf(p"serial_input: ${io.input}\n")
}