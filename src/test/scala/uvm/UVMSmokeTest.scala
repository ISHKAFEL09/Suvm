package uvm

import chiseltester._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import rocket2._
import rocket2.config._

class UVMSmokeTest extends AnyFlatSpec with ChiselTester with Matchers {
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

    test(new TLB()) { c =>
      class Driver(name: String, parent: UVMComponent) extends UVMComponent(name, Some(parent)) {
        override def runPhase(phase: UVMPhase): Unit = {
          phase.raiseObjection(this)
          uvmInfo(getFullName, "test phase begin", UVM_NONE)
          ~>(20)
          uvmInfo(getFullName, "test phase done", UVM_NONE)
          uvmFatal(getTypeName, "fatal test")
          phase.dropObjection(this)
        }
      }

      class Monitor(name: String, parent: UVMComponent) extends UVMComponent(name, Some(parent)) {
        override def runPhase(phase: UVMPhase): Unit = {
          phase.raiseObjection(this)
          uvmInfo(getFullName, "test phase2 begin", UVM_NONE)
          ~>(30)
          uvmInfo(getFullName, "test phase2 done", UVM_NONE)
          phase.dropObjection(this)
        }
      }

      class Agent(name: String, parent: UVMComponent) extends UVMComponent(name, Some(parent)) {
        var driver: Option[Driver] = None
        var monitor: Option[Monitor] = None

        override def buildPhase(phase: UVMPhase): Unit = {
          driver = Some(create("driver", this) { case (s, p) => new Driver(s, p) })
          monitor = Some(create("monitor", this) { case (s, p) => new Monitor(s, p) })
        }

        override def runPhase(phase: UVMPhase): Unit = {
          while (true) {
            ~>(1)
            println("infinite loop test")
          }
        }
      }

      class UVMPhaseTest(name: String) extends UVMTest(name, None) {
        var agent: Option[Agent] = None

        override def buildPhase(phase: UVMPhase): Unit = {
          agent = Some(create("agent", this) { case (s, p) => new Agent(s, p) })
        }
      }

      uvmRunTest { case (s, _) =>
        new UVMPhaseTest(s)
      }
    }
  }
}
