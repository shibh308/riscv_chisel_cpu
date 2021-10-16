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

    val csignals = ListLookup(inst,
                 List(ALU_X    , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X  ),
    Array(
        LW    -> List(ALU_ADD  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_MEM),
        SW    -> List(ALU_ADD  , OP1_RS1, OP2_IMS, MEN_S, REN_X, WB_X  ),
        ADD   -> List(ALU_ADD  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
        ADDI  -> List(ALU_ADD  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
        SUB   -> List(ALU_SUB  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
        AND   -> List(ALU_AND  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
        OR    -> List(ALU_OR   , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
        XOR   -> List(ALU_XOR  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
        ANDI  -> List(ALU_AND  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
        ORI   -> List(ALU_OR   , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
        XORI  -> List(ALU_XOR  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
    ))
    val exe_fun :: op1_sel :: op2_sel :: mem_wen :: rf_wel :: wb_sel :: Nil = csignals // unpackして受け取ってる

    val op1_data = MuxCase(0.U(WORD_LEN.W), Seq(
        (op1_sel === OP1_RS1) -> rs1_data,
    ))
    val op2_data = MuxCase(0.U(WORD_LEN.W), Seq(
        (op2_sel === OP2_RS2) -> rs2_data,
        (op2_sel === OP2_IMI) -> imm_i_sext,
        (op2_sel === OP2_IMS) -> imm_s_sext,
    ))


    // --------------------- EX --------------------
    val alu_out = MuxCase(0.U(WORD_LEN.W), Seq(
        (exe_fun === ALU_ADD) -> (op1_data + op2_data),
        (exe_fun === ALU_SUB) -> (op1_data - op2_data),
        (exe_fun === ALU_AND) -> (op1_data & op2_data),
        (exe_fun === ALU_OR) -> (op1_data | op2_data),
        (exe_fun === ALU_XOR) -> (op1_data ^ op2_data),
    ))


    // --------------------- MEM --------------------
    io.dmem.addr := alu_out // LW以外の命令では使わない
    io.dmem.wdata := rs2_data // SW以外では使わない
    io.dmem.wen := mem_wen // SW命令の時のみ書き込むようにする


    // --------------------- WB --------------------

    // write-backするdataを設定
    val wb_data = MuxCase(alu_out, Seq(
        (wb_sel === WB_MEM) -> io.dmem.rdata // メモリからの読み込み (LW命令時)
    ))

    when(rf_wel === REN_S) {
        regfile(rd_addr) := wb_data // データをレジスタに書き出し (SW以外の命令時)
    }


    printf(p"pc_reg     : 0x${Hexadecimal(pc_reg)}\n")
    printf(p"inst       : 0x${Hexadecimal(inst)}\n")

    printf(p"rs1_addr   : $rs1_addr\n")
    printf(p"rs2_addr   : $rs2_addr\n")
    printf(p"rd_addr    : $rd_addr\n")

    printf(p"rs1_data   : 0x${Hexadecimal(rs1_data)}\n")
    printf(p"rs2_data   : 0x${Hexadecimal(rs2_data)}\n")
    printf(p"wb_data    : 0x${Hexadecimal(wb_data)}\n")
    printf(p"dmem.addr  : 0x${io.dmem.addr}\n")
    printf(p"dmem.wen   : 0x${io.dmem.wen}\n")
    printf(p"dmem.wdata : 0x${Hexadecimal(io.dmem.wdata)}\n")
    printf("----------------\n")

    // 終了判定
    io.exit := (inst === 0x00602823.U(WORD_LEN.W))
}