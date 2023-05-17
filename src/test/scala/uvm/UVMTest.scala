package uvm

import chiseltester.ChiselTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UVMTest extends AnyFlatSpec with ChiselTester with Matchers {
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
}
