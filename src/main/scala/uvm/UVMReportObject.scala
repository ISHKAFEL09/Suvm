package uvm

import ENUM_UVM_VERBOSITY._

class UVMReportObject(name: String) extends UVMObject(name) {
  def getReportVerbosityLevel(severity: uvmSeverity = UVM_INFO, id: String = ""): uvmVerbosity = {
    // TODO:
    UVM_FULL
  }

  def uvmReportEnabled(verbosity: uvmVerbosity, severity: uvmSeverity = UVM_INFO, id: String = ""): Boolean =
    getReportVerbosityLevel(severity, id) >= verbosity

  def uvmReport(severity: uvmSeverity,
                id: String,
                msg: String,
                _verbosity: uvmVerbosity = UVM_MEDIUM,
                trace: String = "",
                contextName: String = "",
                reportEnabledChecked: Boolean = false): Unit = {
    val verbosity = {
      if (Seq(UVM_ERROR, UVM_FATAL, UVM_WARNING) contains severity) {
        UVM_NONE
      } else {
        _verbosity
      }
    }

    // TODO:
    println(s"$trace\n[$severity@$id]: $msg")
  }

  def uvmReportInfo(id: String,
                    msg: String,
                    verbosity: uvmVerbosity = UVM_MEDIUM,
                    trace: String = "",
                    contextName: String = "",
                    reportEnabledChecked: Boolean = false): Unit = {
    uvmReport(UVM_INFO, id, msg, verbosity, trace, contextName, reportEnabledChecked)
  }

  def uvmReportWarning(id: String,
                       msg: String,
                       verbosity: uvmVerbosity = UVM_NONE,
                       trace: String = "",
                       contextName: String = "",
                       reportEnabledChecked: Boolean = false): Unit = {
    uvmReport(UVM_WARNING, id, msg, verbosity, trace, contextName, reportEnabledChecked)
  }

  def uvmReportFatal(id: String,
                     msg: String,
                     verbosity: uvmVerbosity = UVM_NONE,
                     trace: String = "",
                     contextName: String = "",
                     reportEnabledChecked: Boolean = false): Unit = {
    uvmReport(UVM_FATAL, id, msg, verbosity, trace, contextName, reportEnabledChecked)
  }
  
  def uvmInfo(id: String, msg: String, verbosity: uvmVerbosity): Unit = {
    if (uvmReportEnabled(verbosity, UVM_INFO, id)) {
      uvmReportInfo(id, msg, verbosity, getTrace(), "", reportEnabledChecked = true)
    }
  }

  def uvmWarning(id: String, msg: String): Unit = {
    if (uvmReportEnabled(UVM_NONE, UVM_WARNING, id)) {
      uvmReportWarning(id, msg, UVM_NONE, getTrace(), "", reportEnabledChecked = true)
    }
  }

  def uvmFatal(id: String, msg: String): Unit = {
    if (uvmReportEnabled(UVM_NONE, UVM_FATAL, id)) {
      uvmReportFatal(id, msg, UVM_NONE, getTrace(), "", reportEnabledChecked = true)
    }
  }
}

object UVMReportObject {
}
