package riscv_test
import chisel3._

import org.scalatest._
import chiseltest._

class RiscvTest extends FlatSpec with ChiselScalatestTester {
    "mycpu" should "workthrough hex" in { // テスト対象, 想定される振る舞い
        test(new Top) { c =>
            // exitがTrueになるまで動かす (PCがコード末尾まで来たらTrueになるはず)
            while (!c.io.exit.peek.litToBoolean) {
                c.clock.step(1)
            }
            c.io.gp.expect(1.U) // gpが1であって欲しい (riscv-testsのパス条件)
        }
    }
}