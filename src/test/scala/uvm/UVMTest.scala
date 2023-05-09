package uvm

import chiseltester.ChiselTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UVMTest extends AnyFlatSpec with ChiselTester with Matchers {
  behavior of "UVMTest"

  it should "pass UVMObject test" in {
    val obj = new UVMObject("obj") {}
    println(obj.getUVMSeeding)
    obj.setUVMSeeding(false)
    println(obj.getUVMSeeding)
    UVMCoreService().getRoot.uvmInfo("UVM TEST", "this is uvm test msg", UVM_NONE)
  }

  it should "pass UVMReportObject test" in {
    class Report extends UVMReportObject("Report") {
      uvmInfo("ReportObject", "this is a info test message", UVM_LOW)
      uvmWarning("ReportObject", "this is a warn test message")
      uvmFatal("ReportObject", "this is a fatal test message")
    }
    new Report
  }
}
