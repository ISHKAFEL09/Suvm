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
import rocket2.agents.ptwslave._

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

      clock(clk, 100)

      tlb.io.ptw.status.mPrv := 0.U
      tlb.io.ptw.status.vm := 1.U << 3
    }

    class TLBReqSmokeSeq(name: String) extends UVMSequenceBase(name) {
      override def body(): Unit = {
        0 to 100 foreach { i =>
          val item = TLBReqItem(s"tlb_req_item_$i", 0, i / 2, passthrough = true, instruction = true, store = true)
          startItem(item)
          finishItem(item)
        }
      }
    }

    case class TLBTestConfig(ptwSlvCfg: PTWSlaveAgentConfig, tlbReqCfg: TLBReqAgentConfig)

    class TLBSmokeTest(name: String, cfg: TLBTestConfig) extends UVMTest(name, None) {
      var tlbReqAgent: Option[TLBReqAgent] = None
      var ptwSlaveAgent: Option[PTWSlaveAgent] = None

      def tlbReqSqr: TLBReqSequencer = tlbReqAgent.get.sqr.get

      override def buildPhase(phase: UVMPhase): Unit = {
        tlbReqAgent = Some(create("tlbReqAgent", this) { case (s, p) =>
          new TLBReqAgent(s, p, cfg.tlbReqCfg)
        })

        ptwSlaveAgent = Some(create("ptwSlaveAgent", this) { case (s, p) =>
          new PTWSlaveAgent(s, p, cfg.ptwSlvCfg)
        })
      }

      override def runPhase(phase: UVMPhase): Unit = {
        phase.raiseObjection(phase)
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
//        TimeoutValue(1000),
        VerilatorFlags(Seq("--timescale", "1ns/100ps")))
      )

      override def top: TLBHarness => TLBSmokeTest = { c =>
        val tlbReqCfg = TLBReqAgentConfig(
          TLBReqIF(c.clk, c.io.req),
          TLBReqIF(c.clk, c.io.req)
        )

        val ptwSlvCfg = PTWSlaveAgentConfig(PTWSlaveIF(
          c.clk, c.io.ptw
        ))

        new TLBSmokeTest(
          "TLBSmokeTest",
          TLBTestConfig(ptwSlvCfg, tlbReqCfg)
        )
      }
    }

    uvmRun(new TLBChiter {
      override def compile: Boolean = true
//      override def compile: Boolean = false
    })
  }
}
