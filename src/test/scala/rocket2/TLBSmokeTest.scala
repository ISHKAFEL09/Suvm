package rocket2

import chisel3._
import firrtl.AnnotationSeq
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import rocket2.config._
import uvm._
import uvm.chiter._
import agents.tlbreq._

class TLBSmokeTest extends AnyFlatSpec with Matchers {
  behavior of "TLBSmokeTest"

  it should "pass smoke test" in {

    implicit val p: Parameters = Parameters((site, here, up) => {
      case CoreKey => SimpleCoreParams
      case CacheKey => SimpleCacheParams
      case TLBEntries => 32
    })

    class TLBHarness extends ChiterHarness {
      val tlb: TLB = Module(new TLB())
      val io = IO(tlb.io.cloneType)
      val clk: Clock = IO(Input(Clock()))
      io <> tlb.io

      tlb.clock := clk
      tlb.reset := reset

      clock(clk, 5)
    }

    class TLBReqSmokeSeq(name: String) extends UVMSequenceBase(name) {
      override def body(): Unit = {
        0 to 3000 foreach { i =>
          val item = TLBReqItem(s"tlb_req_item_$i", 0, i, passthrough = true, instruction = true, store = true)
          startItem(item)
          finishItem(item)
        }
      }
    }

    class TLBSmokeTest(name: String, cfg: TLBReqAgentConfig) extends UVMTest(name, None) {
      var tlbReqAgent: Option[TLBReqAgent] = None

      def tlbReqSqr: TLBReqSequencer = tlbReqAgent.get.sqr.get

      override def buildPhase(phase: UVMPhase): Unit = {
        tlbReqAgent = Some(create("tlbReqAgent", this) { case (s, p) =>
          new TLBReqAgent(s, p, cfg)
        })
      }

      override def runPhase(phase: UVMPhase): Unit = {
        phase.raiseObjection(phase)
        cfg.driverIF.clk.step(3)
        val s = new TLBReqSmokeSeq("mTLBReqSmokeSeq")
        s.start(tlbReqSqr)
        phase.dropObjection(phase)
      }
    }

    class TLBChiter extends Chiter[TLBHarness] with VerilatorBackend {
      override def harness(): TLBHarness =
        new TLBHarness

      override def annotations: AnnotationSeq = AnnotationSeq(Seq(
        TargetDirAnnotation("test_run_dir/TLBSmokeTest"),
        VerilatorFlags(Seq("--timescale", "1ns/1ns")))
      )

      override def top: TLBHarness => TLBSmokeTest = { c =>
        new TLBSmokeTest(
          "TLBSmokeTest",
          TLBReqAgentConfig(
            TLBReqIF(c.clk, c.io.req),
            TLBReqIF(c.clk, c.io.req)
          )
        )
      }
    }

    uvmRun(new TLBChiter {
//      override def compile: Boolean = true
      override def compile: Boolean = false
    })
  }
}
