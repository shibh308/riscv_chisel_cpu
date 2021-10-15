package mycpu

import chisel3._
import chisel3.util._

import common.Instructions._
import common.Consts._


class Core extends Module {
    val io = IO(new Bundle {
        val imem = Flipped(new ImemPortIo())
        val dmem = Flipped(new DmemPortIo())
        // 終了判定
        val exit = Output(Bool())
    })

    // レジスタは32本でそれぞれ32bit
    val regfile = Mem(NUM_REG, UInt(WORD_LEN.W))

    // プログラムカウンタ
    val pc_reg = RegInit(START_ADDR);


    pc_reg := pc_reg + 4.U(WORD_LEN.W)


    // --------------------- IF --------------------
    // アドレス指定して値を受け取る感じ(Topを見るとMemoryに繋がっている事が分かる)
    io.imem.addr := pc_reg


    // --------------------- ID --------------------
    val inst = io.imem.inst

    // レジスタ番号 (rs1, rs2, rd)
    val rs1_addr = inst(19, 15)
    val rs2_addr = inst(24, 20)
    val rd_addr = inst(11, 7)

    // 0番レジスタは常に0
    // マルチプレクサ要らないように見えるけど参照元コードで定義されてるので書いてます
    val rs1_data = Mux((rs1_addr === 0.U(WORD_LEN.U)), 0.U(WORD_LEN.W), regfile(rs1_addr))
    val rs2_data = Mux((rs2_addr === 0.U(WORD_LEN.U)), 0.U(WORD_LEN.W), regfile(rs2_addr))

    // I形式の即値
    val imm_i = inst(31, 20)
    val imm_i_sext = Cat(Fill(20, imm_i(11)), imm_i) // 最上位bitで埋める (12bitを32bitに拡張してる)

    // S形式の即値
    val imm_s = Cat(inst(31, 25), inst(11, 7))
    val imm_s_sext = Cat(Fill(20, imm_s(11)), imm_s)


    // --------------------- EX --------------------
    val alu_out = MuxCase(0.U(WORD_LEN.W), Seq(
        // switch-caseで計算
        (inst === LW) -> (rs1_data + imm_i_sext), // メモリアドレスの計算
        (inst === SW) -> (rs1_data + imm_s_sext)
    ))


    // --------------------- MEM --------------------
    io.dmem.addr := alu_out // LW以外の命令では使わない
    io.dmem.wdata := rs2_data // SW以外では使わない
    io.dmem.wen := (inst === SW) // SW命令の時のみ書き込むようにする


    // --------------------- WB --------------------
    val rd_data = io.dmem.rdata
    when(inst === LW) {
        regfile(rd_addr) := rd_data // メモリに書き出し
    }


    printf(p"pc_reg     : 0x${Hexadecimal(pc_reg)}\n")
    printf(p"inst       : 0x${Hexadecimal(inst)}\n")

    printf(p"rs1_addr   : $rs1_addr\n")
    printf(p"rs2_addr   : $rs2_addr\n")
    printf(p"rd_addr    : $rd_addr\n")

    printf(p"rs1_data   : 0x${Hexadecimal(rs1_data)}\n")
    printf(p"rs2_data   : 0x${Hexadecimal(rs2_data)}\n")
    printf(p"rd_data    : 0x${Hexadecimal(rd_data)}\n")
    printf(p"dmem.addr  : 0x${io.dmem.addr}\n")
    printf(p"dmem.wen   : 0x${io.dmem.wen}\n")
    printf(p"dmem.wdata : 0x${Hexadecimal(io.dmem.wdata)}\n")
    printf("----------------\n")

    // 終了判定
    io.exit := (inst === 0x00602823.U(WORD_LEN.W))
}