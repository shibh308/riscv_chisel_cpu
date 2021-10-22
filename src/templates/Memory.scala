package {package}

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

import common.Consts._


// 命令取得用
class ImemPortIo extends Bundle {
    val addr = Input(UInt(WORD_LEN.W))
    val inst = Output(UInt(WORD_LEN.W))
}

// メモリ読み出し用
class DmemPortIo extends Bundle {
    val addr = Input(UInt(WORD_LEN.W))
    val wen = Input(Bool()) // 書き込み可否
    val rdata = Output(UInt(WORD_LEN.W)) // 読み込みデータ
    val wdata = Input(UInt(WORD_LEN.W)) // 書き込みデータ
    val len = Input(UInt(MEMLEN_LEN.W)) // 読み書きのビット数
}


class Memory extends Module {
    val io = IO(new Bundle{
        val imem = new ImemPortIo()
        val dmem = new DmemPortIo()
        val serial = Output(Bool())
    })

    val mem = Mem(MEM_SIZE, UInt(8.W))

    io.serial := mem(IO_START_ADDR)


    loadMemoryFromFile(mem, "{load_path}")

    // 命令読み込み
    io.imem.inst := Cat(
        mem(io.imem.addr + 3.U(WORD_LEN.W)),
        mem(io.imem.addr + 2.U(WORD_LEN.W)),
        mem(io.imem.addr + 1.U(WORD_LEN.W)),
        mem(io.imem.addr + 0.U(WORD_LEN.W))
    )
    // MEMLEN_Xの時はそもそも読まれない
    io.dmem.rdata := MuxCase(Cat(mem(io.dmem.addr)), Seq(
        (io.dmem.len === MEMLEN_16) -> Cat(
            mem(io.dmem.addr + 1.U(WORD_LEN.W)),
            mem(io.dmem.addr + 0.U(WORD_LEN.W))
        ),
        (io.dmem.len === MEMLEN_32) -> Cat(
            mem(io.dmem.addr + 3.U(WORD_LEN.W)),
            mem(io.dmem.addr + 2.U(WORD_LEN.W)),
            mem(io.dmem.addr + 1.U(WORD_LEN.W)),
            mem(io.dmem.addr + 0.U(WORD_LEN.W))
        )
    ))

    /*
    printf("----------------\n")
    printf(p"dmem.wen : 0x${io.dmem.wen}\n")
    printf(p"dmem.addr : 0x${Hexadecimal(io.dmem.addr)}\n")
    printf(p"dmem.wdata : 0x${Hexadecimal(io.dmem.wdata)}\n")
    printf(p"io_serial : 0x${Hexadecimal(mem(IO_START_ADDR))}\n")
    printf("----------------\n")
    */

    // メモリへの書き込み
    when(io.dmem.wen) {
        when(io.dmem.len =/= MEMLEN_X) {
            mem(io.dmem.addr + 0.U) := io.dmem.wdata(7, 0)
        }
        when(io.dmem.len === MEMLEN_16 || io.dmem.len === MEMLEN_32) {
            mem(io.dmem.addr + 1.U) := io.dmem.wdata(15, 8)
        }
        when(io.dmem.len === MEMLEN_32) {
            mem(io.dmem.addr + 2.U) := io.dmem.wdata(23, 16)
            mem(io.dmem.addr + 3.U) := io.dmem.wdata(31, 24)
        }
    }
}