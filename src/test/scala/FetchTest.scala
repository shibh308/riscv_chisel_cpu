package fetch
import chisel3._

import org.scalatest._
import chiseltest._

class HexTest extends FlatSpec with ChiselScalatestTester {
    "mycpu" should "workthrough hex" in { // テスト対象, 想定される振る舞い
        test(new Top) { c =>
            // exitがTrueになるまで動かす (PCがコード末尾まで来たらTrueになるはず)
            while (!c.io.exit.peek.litToBoolean) {
                c.clock.step(1)
            }
        }
    }
}