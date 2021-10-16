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

    // ALUの出力
    val alu_out = Wire(UInt(WORD_LEN.W))

    // フェッチされた命令
    val inst = Wire(UInt(WORD_LEN.W))

    // ジャンプ関連のフラグ
    val br_flg = Wire(Bool()) // 分岐判定がTrueになったかどうか
    val jmp_flg = (inst === JAL || inst === JALR) // ジャンプするかどうか

    // 分岐先アドレス
    val br_target = Wire(UInt(WORD_LEN.W))

    // 次の命令のアドレス
    // pc_nextに入れたり, ジャンプ命令の時にライトバックしたりする
    val pc_plus4 = pc_reg + 4.U(WORD_LEN.W)

    // 次の命令に行かずにジャンプをする場合がある
    // br_flgの代入がEXステージなので直感に反するんだけど、これで問題ないらしい
    val pc_next = MuxCase(pc_plus4, Seq(
        br_flg -> br_target, // ALUでは分岐判定が走ってて, br_targetはALUとは別に計算をしている
        jmp_flg -> alu_out, // ジャンプ先アドレスはALUで計算するため
    ))
    pc_reg := pc_next


    // --------------------- IF --------------------
    io.imem.addr := pc_reg
    inst := io.imem.inst


    // --------------------- ID --------------------

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

    // B形式の即値
    val imm_b = Cat(inst(31), inst(7), inst(30, 25), inst(11, 8)) // 11bitがすごいバラけてる
    val imm_b_sext = Cat(Fill(19, imm_s(11)), imm_s, 0.U(1.U)) // 12bitの末尾1ケタを0で固定して11bitで表現みたいな事をしてる

    // J形式の即値
    val imm_j = Cat(inst(31), inst(19, 12), inst(20), inst(30, 21))
    val imm_j_sext = Cat(Fill(11, imm_j(19)), imm_j, 0.U(1.U)) // 最下位bitは0固定

    // U形式の即値
    val imm_u = inst(31, 12)
    val imm_u_shifted = Cat(imm_u, Fill(12, 0.U)) // 20bitを12個左シフトする

    val csignals = ListLookup(inst,
                //    op, arg1, arg2, memwrite, regwrite, regwrite target
                 List(ALU_X    , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X  ),
    Array(
        LW    -> List(ALU_ADD  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_MEM),
        SW    -> List(ALU_ADD  , OP1_RS1, OP2_IMS, MEN_S, REN_X, WB_X  ),

        ADD   -> List(ALU_ADD  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
        SUB   -> List(ALU_SUB  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
        ADDI  -> List(ALU_ADD  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),

        AND   -> List(ALU_AND  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
        OR    -> List(ALU_OR   , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
        XOR   -> List(ALU_XOR  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
        ANDI  -> List(ALU_AND  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
        ORI   -> List(ALU_OR   , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
        XORI  -> List(ALU_XOR  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),

        SLL   -> List(ALU_SLL  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU), // 論理シフト
        SRL   -> List(ALU_SRL  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU),
        SRA   -> List(ALU_SRA  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU), // 算術シフト
        SLLI  -> List(ALU_SLL  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
        SRLI  -> List(ALU_SRL  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
        SRAI  -> List(ALU_SRA  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),

        SLT   -> List(ALU_SLT  , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU), // Less(<)
        SLTU  -> List(ALU_SLTU , OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU), // LessのUnsigned版
        SLTI  -> List(ALU_SLT  , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),
        SLTIU -> List(ALU_SLTU , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU),

        BEQ   -> List(BR_BEQ   , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X), // 分岐関連
        BNE   -> List(BR_BNE   , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X),
        BLT   -> List(BR_BLT   , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X),
        BLTU  -> List(BR_BLTU  , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X),
        BGE   -> List(BR_BGE   , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X),
        BGEU  -> List(BR_BGEU  , OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X),

        JAL   -> List(ALU_ADD  , OP1_PC , OP2_IMJ, MEN_X, REN_S, WB_PC), // PCからの相対位置ジャンプ
        JALR  -> List(ALU_JALR , OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_PC), // レジスタの値からの相対位置ジャンプ

        LUI   -> List(ALU_ADD  , OP1_X  , OP2_IMU, MEN_X, REN_S, WB_ALU), // 即値ロードなので, ALUでは0+即値を計算している
        AUIPC -> List(ALU_ADD  , OP1_PC , OP2_IMU, MEN_X, REN_S, WB_ALU), // PCに即値を足してレジスタに書き込む
    ))
    val exe_fun :: op1_sel :: op2_sel :: mem_wen :: rf_wel :: wb_sel :: Nil = csignals // unpackして受け取ってる

    val op1_data = MuxCase(0.U(WORD_LEN.W), Seq(
        (op1_sel === OP1_RS1) -> rs1_data,
        (op1_sel === OP1_PC) -> pc_reg,
    ))
    val op2_data = MuxCase(0.U(WORD_LEN.W), Seq(
        (op2_sel === OP2_RS2) -> rs2_data,
        (op2_sel === OP2_IMI) -> imm_i_sext,
        (op2_sel === OP2_IMS) -> imm_s_sext,
        (op2_sel === OP2_IMJ) -> imm_j_sext,
        (op2_sel === OP2_IMU) -> imm_u_shifted,
    ))


    // --------------------- EX --------------------
    alu_out := MuxCase(0.U(WORD_LEN.W), Seq(
        (exe_fun === ALU_ADD) -> (op1_data + op2_data),
        (exe_fun === ALU_SUB) -> (op1_data - op2_data),
        (exe_fun === ALU_AND) -> (op1_data & op2_data),
        (exe_fun === ALU_OR) -> (op1_data | op2_data),
        (exe_fun === ALU_XOR) -> (op1_data ^ op2_data),
        (exe_fun === ALU_SRL) -> (op1_data << op2_data(4, 0))(31, 0), // シフトすると32bitでなくなるので下位32bitを取り出す
        (exe_fun === ALU_SLL) -> (op1_data >> op2_data(4, 0)).asUInt(), // 右シフトがBits型を返すので明示的に変換
        (exe_fun === ALU_SRA) -> (op1_data.asSInt() >> op2_data(4, 0)).asUInt(), // SIntに対するシフトは算術シフトになるらしい
        (exe_fun === ALU_SLT) -> (op1_data.asSInt() < op2_data.asSInt()).asUInt(),
        (exe_fun === ALU_SLTU) -> (op1_data < op2_data),
        (exe_fun === ALU_JALR) -> ((op1_data + op2_data) & ~1.U(WORD_LEN.W)), // 最下位bitを0にしている
    ))

    // 分岐命令はALUを使わない
    br_flg := MuxCase(false.B, Seq(
        (exe_fun === BR_BEQ)  ->  (op1_data === op2_data),
        (exe_fun === BR_BNE)  -> !(op1_data === op2_data),
        (exe_fun === BR_BLT)  ->  (op1_data.asSInt() < op2_data.asSInt()),
        (exe_fun === BR_BGE)  -> !(op1_data.asSInt() < op2_data.asSInt()),
        (exe_fun === BR_BLTU) ->  (op1_data < op2_data),
        (exe_fun === BR_BGEU) -> !(op1_data < op2_data),
    ))
    br_target := pc_reg + imm_b_sext

    // --------------------- MEM --------------------
    io.dmem.addr := alu_out // LW以外の命令では使わない
    io.dmem.wdata := rs2_data // SW以外では使わない
    io.dmem.wen := mem_wen // SW命令の時のみ書き込むようにする


    // --------------------- WB --------------------

    // write-backするdataを設定
    val wb_data = MuxCase(alu_out, Seq(
        (wb_sel === WB_MEM) -> io.dmem.rdata, // メモリからの読み込み (LW命令時)
        (wb_sel === WB_PC) -> pc_plus4, // ジャンプから戻ってくる時のアドレス
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