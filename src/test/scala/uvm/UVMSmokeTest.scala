package uvm

import agents.decouple._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import firrtl.AnnotationSeq
import firrtl.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import rocket2._
import rocket2.config._
import treadle.WriteVcdAnnotation
import uvm.chiter._


class UVMSmokeTest extends AnyFlatSpec with Matchers {
  behavior of "UVMTest"

  it should "pass UVMObject test" in {
//    top.setReportVerbosityLevel(UVM_NONE)
    val obj = new UVMObject("obj") {
      val obj2 = new UVMObject("obj2") {
        uvmInfo("OBJ2", "this is obj2", UVM_LOW)
      }
    }
    println(obj.getUVMSeeding)
    obj.setUVMSeeding(false)
    println(obj.getUVMSeeding)
    uvmInfo("UVM TEST", "this is uvm test msg", UVM_LOW)
  }

  it should "pass UVMReportObject test" in {
    class Report(name: String) extends UVMReportObject(name) {
      setReportVerbosityLevel(UVM_HIGH)
//      uvmInfo("ReportObject", "this is a info test message", UVM_LOW)
//      uvmWarning("ReportObject", "this is a warn test message")
//      uvmFatal("ReportObject", "this is a fatal test message")
    }

    class Report1(name: String) extends Report(name)

    val report = create("MyReport") { s =>
      new Report(s)
    }
    val report1 = create("MyReport1") { s =>
      new Report1(s)
    }
    val report2 = create("MyReport2") { s =>
      new Report(s)
    }
    println(report, report1, report2)

    UVMCoreService().getFactory.setTypeOverrideByName(classOf[Report]) { s =>
      new Report1(s)
    }

    val report3 = create("OverrideReport") { s =>
      new Report(s)
    }
    println(report3)
  }

  it should "pass UVMComponent test" in {
    class Component(name: String) extends UVMComponent(name, None) {
      class Component1(name: String, parent: UVMComponent) extends UVMComponent(name, Some(parent))
      val comp1 = create("comp1", this) { case (s, p) =>
        new Component1(s, p)
      }
      val comp2 = create("", comp1) { case (s, p) =>
        new Component1(s, p)
      }
    }
    val uvmTest = new Component("uvmTest")
    println(uvmTest.getParent, uvmTest.comp1.getParent, uvmTest.comp2.getParent)
    println(uvmTest.getFullName, uvmTest.comp1.getFullName, uvmTest.comp2.getFullName)
    println(top.getChildren, uvmTest.getChildren, uvmTest.comp1.getChildren)
  }

  it should "pass run_test test" in {

    implicit val p: Parameters = Parameters((site, here, up) => {
      case CoreKey => SimpleCoreParams
      case CacheKey => SimpleCacheParams
      case TLBEntries => 32
    })

    class TLBHarness extends ChiterHarness {
      val tlb = Module(new TLB())
      val io = IO(tlb.io.cloneType)
      val clk = IO(Input(Clock()))
      io <> tlb.io

      tlb.clock := clk
      tlb.reset := reset

      clock(clk, 5)
    }

    case class TLBReqItem(asid: Int, vpn: Int, passthrough: Boolean, instruction: Boolean, store: Boolean)

    val sqr = new UVMSequencer[DecoupleSeqItem[TLBReqItem], UVMSequenceItem]("sqr", None)

    class TLBReqDriver(parent: UVMComponent, bus: DecoupledIO[TLBReq], clk: Clock) extends DecoupleDriver[TLBReqItem, TLBReq]("TLBReqDriver", Some(parent), bus)(clk) {
      override def driver(t: TLBReqItem): TLBReq = chiselTypeOf(bus.bits).Lit(
        _.vpn -> t.vpn.U,
        _.asid -> t.asid.U,
        _.passthrough -> t.passthrough.B,
        _.store -> t.store.B,
        _.instruction -> t.instruction.B
      )
    }

    class TLBReqMonitor(parent: UVMComponent, bus: DecoupledIO[TLBReq], clk: Clock) extends DecoupleMonitor[TLBReqItem, TLBReq]("TLBReqMonitor", Some(parent), bus)(clk) {
      override def monitor(): DecoupleSeqItem[TLBReqItem] = DecoupleSeqItem("monitor_item", TLBReqItem(
        bus.bits.vpn.peek().litValue.toInt,
        bus.bits.asid.peek().litValue.toInt,
        bus.bits.passthrough.peek().litToBoolean,
        bus.bits.instruction.peek().litToBoolean,
        bus.bits.store.peek().litToBoolean
      ))

      override def write(t: DecoupleSeqItem[TLBReqItem]): Unit =
        uvmInfo(getTypeName, s"monitor item: ${t.gen.asid}", UVM_NONE)
    }

    class Agent(name: String, parent: UVMComponent) extends UVMComponent(name, Some(parent)) {
      var driver: Option[TLBReqDriver] = None
      var monitor: Option[TLBReqMonitor] = None

      var driverIF: Option[DecoupledIO[TLBReq]] = None
      var monitorIF: Option[DecoupledIO[TLBReq]] = None
      var clk: Option[Clock] = None

      override def buildPhase(phase: UVMPhase): Unit = {
        driver = Some(create("driver", this) { case (s, p) => new TLBReqDriver(p, driverIF.get, clk.get) })
        monitor = Some(create("monitor", this) { case (s, p) => new TLBReqMonitor(p, monitorIF.get, clk.get) })
      }
    }

    class SequenceTest(name: String) extends UVMSequenceBase(name) {
      override def body(): Unit = {
        0 to 30 foreach { i =>
          val item = DecoupleSeqItem(s"item $i", TLBReqItem(0, i, true, true, true))
          startItem(item)
          finishItem(item)
        }
      }
    }

    case class TLBConfig(driverIF: DecoupledIO[TLBReq],
                         monitorIF: DecoupledIO[TLBReq],
                         clk: Clock)

    class TLBTest(name: String, cfg: TLBConfig) extends UVMTest(name, None) {
      var agent: Option[Agent] = None

      override def buildPhase(phase: UVMPhase): Unit = {
        agent = Some(create("agent", this) { case (s, p) => new Agent(s, p) })
        agent.get.driverIF = Some(cfg.driverIF)
        agent.get.monitorIF = Some(cfg.monitorIF)
        agent.get.clk = Some(cfg.clk)
      }

      override def connectPhase(phase: UVMPhase): Unit = {
        agent.get.driver.get.seqItemPort.connect(sqr.seqItemExport)
      }

      override def runPhase(phase: UVMPhase): Unit = {
        phase.raiseObjection(phase)
        val s = new SequenceTest("s")
        fork {
          for (i <- 0 to 100) {
            uvmInfo(getTypeName, s"api test: $i", UVM_NONE)
            ~>(1)
          }
//          finish()
//          uvmFatal(getTypeName, "api fatal test")
        }
        s.start(sqr)
        phase.dropObjection(phase)
      }
    }

    class TLBChiter extends Chiter[TLBHarness] with TreadleBackend {
      override def harness(): TLBHarness =
        new TLBHarness

      override def annotations: AnnotationSeq = AnnotationSeq(Seq(
        TargetDirAnnotation("test_run_dir/TLBTest"),
        WriteVcdAnnotation)
      )

      override def top: TLBHarness => TLBTest = { c =>
        new TLBTest("TLBTest", TLBConfig(c.io.req, c.io.req, c.clk))
      }
    }

    uvmRun(new TLBChiter)
//    ChiterVerilator.run()
  }
}
