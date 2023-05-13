package uvm

import chiseltester.ChiselTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UVMTest extends AnyFlatSpec with ChiselTester with Matchers {
  behavior of "UVMTest"

  it should "pass UVMObject test" in {
//    UVMCoreService().getRoot.setReportVerbosityLevel(UVM_NONE)
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

    val report = create((s: String) => new Report(s), "MyReport")
    val report1 = create((s: String) => new Report1(s), "MyReport1")
    val report2 = create((s: String) => new Report(s), "MyReport2")
    println(report, report1, report2)

    UVMCoreService().getFactory.setTypeOverrideByName(classOf[Report], (s: String) => new Report1(s))
    val report3 = create((s: String) => new Report(s), "OverrideReport")
    println(report3)
  }
}
