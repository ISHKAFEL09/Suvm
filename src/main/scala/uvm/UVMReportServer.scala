package uvm

import ENUM_UVM_ACTION._

abstract class UVMReportServer(name: String = "UVMReportServer") extends UVMObject(name) {
  def processReportMessage(reportMessage: UVMReportMessage): Unit

  def composeReportMessage(reportMessage: UVMReportMessage, reportObjName: String = ""): String

  def executeReportMessage(reportMessage: UVMReportMessage, msg: String): Unit
}

class UVMDefaultReportServer(name: String = "UVMDefaultReportServer") extends UVMReportServer(name) {
  override def processReportMessage(reportMessage: UVMReportMessage): Unit = {
    val report: Boolean =
      if (reportMessage.action.get == Seq(UVM_NO_ACTION)) false else true

    // TODO: catcher

    if (report) {
      var m: String = ""
      val svr = UVMCoreService().getReportServer
      if (reportMessage.action.get.intersect(Seq(UVM_LOG, UVM_DISPLAY)).nonEmpty) {
        m = svr.composeReportMessage(reportMessage)
      }
      svr.executeReportMessage(reportMessage, m)
    }
  }

  override def composeReportMessage(reportMessage: UVMReportMessage, reportObjName: String): String = {
    // TODO:
    val objName = if (reportObjName == "") reportMessage.rh.get.getFullName else reportObjName
    s"${reportMessage.severity}(${reportMessage.verbosity}) ${reportMessage.trace}: $objName" +
      s"[${reportMessage.idx}] ${reportMessage.msg}"
  }

  override def executeReportMessage(reportMessage: UVMReportMessage, msg: String): Unit = {
    // TODO:
    if (reportMessage.action.get.contains(UVM_DISPLAY)) {
      import ENUM_UVM_SEVERITY._
      reportMessage.severity match {
        case UVM_INFO => println(s"${Console.GREEN}$msg${Console.RESET}")
        case UVM_WARNING | UVM_ERROR | UVM_FATAL => println(s"${Console.RED}$msg${Console.RESET}")
      }
    }

    if (reportMessage.action.get.contains(UVM_STOP))
      throw new InterruptedException("GOT UVM_STOP ACTION!")
  }
}
