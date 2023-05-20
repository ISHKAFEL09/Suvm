package chiseltester

import chisel3._
import circt.stage.{CIRCTTarget, CIRCTTargetAnnotation}
import firrtl.AnnotationSeq
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import treadle.WriteVcdAnnotation
import rocket2.config._
import rocket2._

class TLBTest extends AnyFlatSpec with ChiselTester with Matchers {
  behavior of "ChiselTester"

  class TestBase(implicit dut: TLB) {
    val reqAgent = new TLBReqAgent(dut.io.req, dut.clock)

    def run(): Unit = {}
  }

  class TestSmoke(implicit dut: TLB) extends TestBase {
    override def run(): Unit = {
      dut.io.ptw.status.vm.poke((1 << 3).U)
      reqAgent.drive("h5a5a00".U)
    }
  }

  it should "pass tlb test" in {

    implicit val p: Parameters = Parameters((site, here, up) => {
      case CoreKey => SimpleCoreParams
      case CacheKey => SimpleCacheParams
      case TLBEntries => 32
    })

    test(new TLB()) { c =>
      implicit val dut = c
//      new TestSmoke().run()
//      assert (c.io.resp.miss.peek().litToBoolean)
      var cnt = 10
      val a = fork("a") {
        while (cnt > -10) {
          cnt -= 1
          ->(1)
        }
      }

      val b = fork("b") {
        ->(cnt == 0)
        println(s"wait done!$cnt")
      }

      val e = fork("e") {
        ->(cnt == -3)
        println(s"wait done!$cnt")
      }

      val g = fork("g") {
        fork("tmp0") {
          ->(cnt == -4)
        }

        fork("tmp1") {
          while (true) {
            println("fork g:", cnt)
            ->(1)
          }
        }
      }

      val d = a ++ b ++ e
      d.joinAny()
      d.kill()
      g.kill()
      ->(3)
      println(a.done, b.done, d.done, e.done, g.done)
    }
  }
}
