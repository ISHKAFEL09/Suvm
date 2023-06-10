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
import uvm._

class TLBTest extends AnyFlatSpec with ChiselTester with Matchers {
  behavior of "ChiselTester"

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
          ~>(1)
        }
      }

      val b = fork("b") {
        ~>(cnt == 0)
        println(s"wait done!$cnt")
      }

      val e = fork("e") {
        ~>(cnt == -3)
        println(s"wait done!$cnt")
      }

      val g = fork("g") {
        fork("tmp0") {
          ~>(cnt == -4)
        }

        fork("tmp1") {
          while (true) {
            println("fork g:", cnt)
            ~>(1)
          }
        }
      }

      val d = a ++ b ++ e
      d.joinAny()
      d.kill()
      g.kill()
      ~>(3)
      println(a.done, b.done, d.done, e.done, g.done)
    }
  }

  it should "pass event test" in {

    implicit val p: Parameters = Parameters((site, here, up) => {
      case CoreKey => SimpleCoreParams
      case CacheKey => SimpleCacheParams
      case TLBEntries => 32
    })

    test(new TLB()) { c =>
      val event = Event("tlbEvent")

      println(s"current thread name: ${current.get.name}")

      val a = fork("trig") {
        ~>(20)
        event.trigger()
        println(s"current thread name: ${current.get.name}")
        ~>(10)
        event.trigger()
      }

      val b = fork("wait trig") {
        println(s"current thread name: ${current.get.name}")
        ~>(event)
        ~>(event)
      }

      (a ++ b).join()
    }
  }
}
