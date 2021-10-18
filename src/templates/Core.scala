package {package}

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
        val gp = Output(UInt(WORD_LEN.W)) // x3のレジスタ
    })

    // レジスタは32本でそれぞれ32bit
    val regfile = Mem(NUM_REG, UInt(WORD_LEN.W))

    // CSR用のレジスタ
    val csr_regfile = Mem(NUM_CSR_REG, UInt(WORD_LEN.W))

    val stall_flg = Wire(Bool()) // データハザードでのストール

    val ex_reg_wb_addr = RegInit(0.U(ADDR_LEN.W))
    val ex_reg_rf_wen  = RegInit(0.U(REN_LEN.W))
    val me_reg_wb_addr = RegInit(0.U(ADDR_LEN.W))
    val me_reg_rf_wen  = RegInit(0.U(REN_LEN.W))
    val wb_reg_wb_addr = RegInit(0.U(ADDR_LEN.W))
    val wb_reg_rf_wen  = RegInit(0.U(ADDR_LEN.W))

    // 0x3 (グローバルポインタ) を指すようにする
    io.gp := regfile(3)

    // *********************************************

    val if_reg_pc = RegInit(START_ADDR);

    // --------------------- IF --------------------

    io.imem.addr := if_reg_pc
    val if_inst = io.imem.inst

    // ジャンプ関連のフラグ
    val ex_br_flg = Wire(Bool()) // 分岐判定がTrueになったかどうか
    val ex_jmp_flg = Wire(Bool())
    val ex_br_target = Wire(UInt(WORD_LEN.W)) // 分岐先アドレス

    val ex_alu_out = Wire(UInt(WORD_LEN.W))

    val me_wb_data = Wire(UInt(WORD_LEN.W))
    val wb_reg_wb_data = RegInit(0.U(ADDR_LEN.W))

    // 次の命令に行かずにジャンプをする場合がある
    // br_flgの代入がEXステージなので直感に反するんだけど、これで問題ないらしい
    val if_pc_next = MuxCase(if_reg_pc + 4.U(WORD_LEN.W), Seq(
        ex_br_flg -> ex_br_target, // ALUでは分岐判定が走ってて, br_targetはALUとは別に計算をしている
        ex_jmp_flg -> ex_alu_out, // ジャンプ先アドレスはALUで計算するため
        (if_inst === ECALL) -> csr_regfile(0x305), // 0x305(mtvec)がtrap_vectorアドレスで, OSだと例外時のシステムコールが書いてある
        stall_flg -> if_reg_pc, // ループさせる
    ))
    if_reg_pc := if_pc_next;

    // *********************************************

    val id_reg_pc   = RegInit(0.U(WORD_LEN.W))
    val id_reg_inst = RegInit(0.U(WORD_LEN.W))

    id_reg_pc := Mux(stall_flg, id_reg_pc, if_reg_pc);
    id_reg_inst := MuxCase(if_inst, Seq(
        (ex_br_flg || ex_jmp_flg) -> BUBBLE, // 分岐ハザード
        stall_flg -> id_reg_inst, // データハザードのストール
    ))

    // --------------------- ID --------------------

    val id_rs1_addr_b = id_reg_inst(19, 15) // ストール判定部分 (id_instの確定前に動かす)
    val id_rs2_addr_b = id_reg_inst(24, 20)
    val id_rs1_data_hazard = (id_rs1_addr_b =/= 0.U) && (id_rs1_addr_b === ex_reg_wb_addr)
    val id_rs2_data_hazard = (id_rs2_addr_b =/= 0.U) && (id_rs2_addr_b === ex_reg_wb_addr)
    stall_flg := (id_rs1_data_hazard || id_rs2_data_hazard) && (ex_reg_rf_wen === REN_S)

    val id_inst = Mux((ex_br_flg || ex_jmp_flg || stall_flg), BUBBLE, id_reg_inst) // 分岐ハザード

    // レジスタ番号 (rs1, rs2, rd)
    val id_rs1_addr = id_inst(19, 15)
    val id_rs2_addr = id_inst(24, 20)
    val id_wb_addr = id_inst(11, 7)

    // 0番レジスタは常に0
    // 実際は適当に値を書き込んじゃう場合があるので、マルチプレクサ挟んで必ず0にするようにしてる
    val id_rs1_data = MuxCase(regfile(id_rs1_addr), Seq(
        (id_rs1_addr === 0.U(WORD_LEN.U)) -> 0.U(WORD_LEN.W),
        ((id_rs1_addr === me_reg_wb_addr) && me_reg_rf_wen === REN_S) -> me_wb_data,
        ((id_rs1_addr === wb_reg_wb_addr) && wb_reg_rf_wen === REN_S) -> wb_reg_wb_data,
    ))
    val id_rs2_data = MuxCase(regfile(id_rs2_addr), Seq(
        (id_rs2_addr === 0.U(WORD_LEN.U)) -> 0.U(WORD_LEN.W),
        ((id_rs2_addr === me_reg_wb_addr) && me_reg_rf_wen === REN_S) -> me_wb_data,
        ((id_rs2_addr === wb_reg_wb_addr) && wb_reg_rf_wen === REN_S) -> wb_reg_wb_data,
    ))

    // I形式の即値
    val id_imm_i = id_inst(31, 20)
    val id_imm_i_sext = Cat(Fill(20, id_imm_i(11)), id_imm_i) // 最上位bitで埋める (12bitを32bitに拡張してる)

    // S形式の即値
    val id_imm_s = Cat(id_inst(31, 25), id_inst(11, 7))
    val id_imm_s_sext = Cat(Fill(20, id_imm_s(11)), id_imm_s)

    // B形式の即値
    val id_imm_b = Cat(id_inst(31), id_inst(7), id_inst(30, 25), id_inst(11, 8)) // 11bitがすごいバラけてる
    val id_imm_b_sext = Cat(Fill(19, id_imm_b(11)), id_imm_b, 0.U(1.U)) // 12bitの末尾1ケタを0で固定して11bitで表現みたいな事をしてる

    // J形式の即値
    val id_imm_j = Cat(id_inst(31), id_inst(19, 12), id_inst(20), id_inst(30, 21))
    val id_imm_j_sext = Cat(Fill(11, id_imm_j(19)), id_imm_j, 0.U(1.U)) // 最下位bitは0固定

    // U形式の即値
    val id_imm_u = id_inst(31, 12)
    val id_imm_u_shifted = Cat(id_imm_u, Fill(12, 0.U)) // 20bitを12個左シフトする

    // Z形式の即値
    val id_imm_z = id_inst(19, 15)
    val id_imm_z_uext = Cat(Fill(27, 0.U), id_imm_z) // そのまま0埋め

    val id_csignals = ListLookup(id_inst,
                //    op, arg1, arg2, memwrite, regwrite, regwrite target, csr cmd
                 List(ALU_X    , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X  , CSR_X),
    Array(
        LW    -> List(ALU_ADD  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_MEM, CSR_X),
        SW    -> List(ALU_ADD  , OP1_RS1, OP2_IMS, MEN_S, REN_X, WB_X  , CSR_X),

        ADD   -> List(ALU_ADD  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
        SUB   -> List(ALU_SUB  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
        ADDI  -> List(ALU_ADD  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),

        AND   -> List(ALU_AND  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
        OR    -> List(ALU_OR   , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
        XOR   -> List(ALU_XOR  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
        ANDI  -> List(ALU_AND  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
        ORI   -> List(ALU_OR   , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
        XORI  -> List(ALU_XOR  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),

        SLL   -> List(ALU_SLL  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X), // 論理シフト
        SRL   -> List(ALU_SRL  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),
        SRA   -> List(ALU_SRA  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X), // 算術シフト
        SLLI  -> List(ALU_SLL  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
        SRLI  -> List(ALU_SRL  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
        SRAI  -> List(ALU_SRA  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),

        SLT   -> List(ALU_SLT  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X), // Less(<)
        SLTU  -> List(ALU_SLTU , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X), // LessのUnsigned版
        SLTI  -> List(ALU_SLT  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
        SLTIU -> List(ALU_SLTU , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),

        BEQ   -> List(BR_BEQ   , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X), // 分岐関連
        BNE   -> List(BR_BNE   , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
        BLT   -> List(BR_BLT   , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
        BLTU  -> List(BR_BLTU  , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
        BGE   -> List(BR_BGE   , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
        BGEU  -> List(BR_BGEU  , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),

        JAL   -> List(ALU_ADD  , OP1_PC , OP2_IMJ, MEN_X, REN_S, WB_PC, CSR_X), // PCからの相対位置ジャンプ
        JALR  -> List(ALU_JALR , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_PC, CSR_X), // レジスタの値からの相対位置ジャンプ

        LUI   -> List(ALU_ADD  , OP1_X  , OP2_IMU, MEN_X, REN_S, WB_ALU, CSR_X), // 即値ロードなので, ALUでは0+即値を計算している
        AUIPC -> List(ALU_ADD  , OP1_PC , OP2_IMU, MEN_X, REN_S, WB_ALU, CSR_X), // PCに即値を足してレジスタに書き込む

        CSRRW -> List(ALU_COPY1, OP1_RS1, OP2_X  , MEN_X, REN_S, WB_CSR, CSR_W), // csr = x
        CSRRWI-> List(ALU_COPY1, OP1_IMZ, OP2_X  , MEN_X, REN_S, WB_CSR, CSR_W),
        CSRRS -> List(ALU_COPY1, OP1_RS1, OP2_X  , MEN_X, REN_S, WB_CSR, CSR_S), // csr | x
        CSRRSI-> List(ALU_COPY1, OP1_IMZ, OP2_X  , MEN_X, REN_S, WB_CSR, CSR_S),
        CSRRC -> List(ALU_COPY1, OP1_RS1, OP2_X  , MEN_X, REN_S, WB_CSR, CSR_C), // csr & ~x
        CSRRCI-> List(ALU_COPY1, OP1_IMZ, OP2_X  , MEN_X, REN_S, WB_CSR, CSR_C),

        ECALL -> List(ALU_X    , OP1_X  , OP2_X  , MEN_X, REN_X, WB_X  , CSR_E), // 例外を発生させる
    ))
    val id_exe_fun :: id_op1_sel :: id_op2_sel :: id_mem_wen :: id_rf_wen :: id_wb_sel :: id_csr_cmd :: Nil = id_csignals // unpackして受け取ってる

    // 書き換えるCSRのindex
    val id_csr_addr = Mux(id_csr_cmd === CSR_E, 0x342.U(CSR_ADDR_LEN.W), id_inst(31, 20)) // ECALLの時は0x342(mcauseレジスタ)に特定の値を書き込む

    val id_op1_data = MuxCase(0.U(WORD_LEN.W), Seq(
        (id_op1_sel === OP1_RS1) -> id_rs1_data,
        (id_op1_sel === OP1_PC) -> id_reg_pc,
        (id_op1_sel === OP1_IMZ) -> id_imm_z_uext,
    ))
    val id_op2_data = MuxCase(0.U(WORD_LEN.W), Seq(
        (id_op2_sel === OP2_RS2) -> id_rs2_data,
        (id_op2_sel === OP2_IMI) -> id_imm_i_sext,
        (id_op2_sel === OP2_IMS) -> id_imm_s_sext,
        (id_op2_sel === OP2_IMJ) -> id_imm_j_sext,
        (id_op2_sel === OP2_IMU) -> id_imm_u_shifted,
    ))

    // *********************************************

    val ex_reg_pc            = RegInit(0.U(WORD_LEN.W))
    val ex_reg_op1_data      = RegInit(0.U(WORD_LEN.W))
    val ex_reg_op2_data      = RegInit(0.U(WORD_LEN.W))
    val ex_reg_rs2_data      = RegInit(0.U(WORD_LEN.W))
    val ex_reg_exe_fun       = RegInit(0.U(EXE_FUN_LEN.W))
    val ex_reg_mem_wen       = RegInit(0.U(MEN_LEN.W))
    val ex_reg_wb_sel        = RegInit(0.U(WB_SEL_LEN.W))
    val ex_reg_csr_addr      = RegInit(0.U(CSR_ADDR_LEN.W))
    val ex_reg_csr_cmd       = RegInit(0.U(CSR_LEN.W))
    val ex_reg_imm_i_sext    = RegInit(0.U(WORD_LEN.W))
    val ex_reg_imm_s_sext    = RegInit(0.U(WORD_LEN.W))
    val ex_reg_imm_b_sext    = RegInit(0.U(WORD_LEN.W))
    val ex_reg_imm_u_shifted = RegInit(0.U(WORD_LEN.W))
    val ex_reg_imm_z_uext    = RegInit(0.U(WORD_LEN.W))

    ex_reg_pc            := id_reg_pc
    ex_reg_wb_addr       := id_wb_addr
    ex_reg_op1_data      := id_op1_data
    ex_reg_op2_data      := id_op2_data
    ex_reg_rs2_data      := id_rs2_data
    ex_reg_exe_fun       := id_exe_fun
    ex_reg_mem_wen       := id_mem_wen
    ex_reg_rf_wen        := id_rf_wen
    ex_reg_wb_sel        := id_wb_sel
    ex_reg_csr_addr      := id_csr_addr
    ex_reg_csr_cmd       := id_csr_cmd
    ex_reg_imm_i_sext    := id_imm_i_sext
    ex_reg_imm_s_sext    := id_imm_s_sext
    ex_reg_imm_b_sext    := id_imm_b_sext
    ex_reg_imm_u_shifted := id_imm_u_shifted
    ex_reg_imm_z_uext    := id_imm_z_uext

    // --------------------- EX --------------------

    ex_alu_out := MuxCase(0.U(WORD_LEN.W), Seq(
        (ex_reg_exe_fun === ALU_ADD) -> (ex_reg_op1_data + ex_reg_op2_data),
        (ex_reg_exe_fun === ALU_SUB) -> (ex_reg_op1_data - ex_reg_op2_data),
        (ex_reg_exe_fun === ALU_AND) -> (ex_reg_op1_data & ex_reg_op2_data),
        (ex_reg_exe_fun === ALU_OR) -> (ex_reg_op1_data | ex_reg_op2_data),
        (ex_reg_exe_fun === ALU_XOR) -> (ex_reg_op1_data ^ ex_reg_op2_data),
        (ex_reg_exe_fun === ALU_SRL) -> (ex_reg_op1_data << ex_reg_op2_data(4, 0))(31, 0), // シフトすると32bitでなくなるので下位32bitを取り出す
        (ex_reg_exe_fun === ALU_SLL) -> (ex_reg_op1_data >> ex_reg_op2_data(4, 0)).asUInt(), // 右シフトがBits型を返すので明示的に変換
        (ex_reg_exe_fun === ALU_SRA) -> (ex_reg_op1_data.asSInt() >> ex_reg_op2_data(4, 0)).asUInt(), // SIntに対するシフトは算術シフトになるらしい
        (ex_reg_exe_fun === ALU_SLT) -> (ex_reg_op1_data.asSInt() < ex_reg_op2_data.asSInt()).asUInt(),
        (ex_reg_exe_fun === ALU_SLTU) -> (ex_reg_op1_data < ex_reg_op2_data),
        (ex_reg_exe_fun === ALU_JALR) -> ((ex_reg_op1_data + ex_reg_op2_data) & ~1.U(WORD_LEN.W)), // 最下位bitを0にしている
        (ex_reg_exe_fun === ALU_COPY1) -> ex_reg_op1_data,
    ))

    // 分岐命令はALUを使わない
    ex_br_flg := MuxCase(false.B, Seq(
        (ex_reg_exe_fun === BR_BEQ)  ->  (ex_reg_op1_data === ex_reg_op2_data),
        (ex_reg_exe_fun === BR_BNE)  -> !(ex_reg_op1_data === ex_reg_op2_data),
        (ex_reg_exe_fun === BR_BLT)  ->  (ex_reg_op1_data.asSInt() < ex_reg_op2_data.asSInt()),
        (ex_reg_exe_fun === BR_BGE)  -> !(ex_reg_op1_data.asSInt() < ex_reg_op2_data.asSInt()),
        (ex_reg_exe_fun === BR_BLTU) ->  (ex_reg_op1_data < ex_reg_op2_data),
        (ex_reg_exe_fun === BR_BGEU) -> !(ex_reg_op1_data < ex_reg_op2_data),
    ))
    ex_br_target := ex_reg_pc + ex_reg_imm_b_sext
    ex_jmp_flg := (ex_reg_wb_sel === WB_PC)

    // *********************************************

    val me_reg_pc         = RegInit(0.U(WORD_LEN.W))
    val me_reg_op1_data   = RegInit(0.U(WORD_LEN.W))
    val me_reg_rs2_data   = RegInit(0.U(WORD_LEN.W))
    val me_reg_mem_wen    = RegInit(0.U(MEN_LEN.W))
    val me_reg_wb_sel     = RegInit(0.U(WB_SEL_LEN.W))
    val me_reg_csr_addr   = RegInit(0.U(CSR_ADDR_LEN.W))
    val me_reg_csr_cmd    = RegInit(0.U(CSR_LEN.W))
    val me_reg_imm_z_uext = RegInit(0.U(WORD_LEN.W))
    val me_reg_alu_out    = RegInit(0.U(WORD_LEN.W))

    me_reg_pc         := ex_reg_pc
    me_reg_wb_addr    := ex_reg_wb_addr
    me_reg_op1_data   := ex_reg_op1_data
    me_reg_rs2_data   := ex_reg_rs2_data
    me_reg_mem_wen    := ex_reg_mem_wen
    me_reg_rf_wen     := ex_reg_rf_wen
    me_reg_wb_sel     := ex_reg_wb_sel
    me_reg_csr_addr   := ex_reg_csr_addr
    me_reg_csr_cmd    := ex_reg_csr_cmd
    me_reg_imm_z_uext := ex_reg_imm_z_uext
    me_reg_alu_out    := ex_alu_out

    // --------------------- MEM --------------------

    io.dmem.addr := me_reg_alu_out // LW以外の命令では使わない
    io.dmem.wdata := me_reg_rs2_data // SW以外では使わない
    io.dmem.wen := me_reg_mem_wen // SW命令の時のみ書き込むようにする

    val csr_rdata = csr_regfile(me_reg_csr_addr)
    // CSRに書き込むデータ
    val csr_wdata = MuxCase(0.U(WORD_LEN.W), Seq(
        (me_reg_csr_cmd === CSR_W) -> me_reg_op1_data,
        (me_reg_csr_cmd === CSR_S) -> (csr_rdata | me_reg_op1_data),
        (me_reg_csr_cmd === CSR_C) -> (csr_rdata & ~me_reg_op1_data),
        (me_reg_csr_cmd === CSR_E) -> 11.U(WORD_LEN.W), // マシンモードからのECALLを表すらしい
    ))

    when(me_reg_csr_cmd =/= CSR_X) {
        csr_regfile(me_reg_csr_addr) := csr_wdata
    }

    // write-backするdataを設定
    me_wb_data := MuxCase(me_reg_alu_out, Seq(
        (me_reg_wb_sel === WB_MEM) -> io.dmem.rdata, // メモリからの読み込み (LW命令時)
        (me_reg_wb_sel === WB_PC) -> (me_reg_pc + 4.U(WORD_LEN.W)), // ジャンプから戻ってくる時のアドレス
        (me_reg_wb_sel === WB_CSR) -> csr_rdata,
    ))

    // *********************************************

    wb_reg_wb_addr := me_reg_wb_addr
    wb_reg_rf_wen := me_reg_rf_wen
    wb_reg_wb_data := me_wb_data

    // --------------------- WB --------------------

    when(wb_reg_rf_wen === REN_S) {
        regfile(wb_reg_wb_addr) := wb_reg_wb_data // データをレジスタに書き出し (SW以外の命令時)
    }

    printf(p"reg_pc     : 0x${Hexadecimal(id_reg_pc)}\n")

    /*
    printf(p"op1_data   : 0x${Hexadecimal(me_reg_op1_data)}\n")
    printf(p"op2_data   : 0x${Hexadecimal(me_reg_op2_data)}\n")
    printf(p"wb_data    : 0x${Hexadecimal(me_wb_data)}\n")
    */

    printf(p"gp         : 0x${regfile(3)}\n")
    printf("----------------\n")

    // 終了判定
    io.exit := {exit} // riscv-testsの終了アドレス
}