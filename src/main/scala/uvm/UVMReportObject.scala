package uvm

import ENUM_UVM_VERBOSITY._

class UVMReportObject(name: String = "") extends UVMObject(name) {
  private var mReportHandler: Option[UVMReportHandler] = None
  
  protected val _traceLevel: Int = 2

  def setReportHandler(handler: UVMReportHandler): Unit = mReportHandler = Some(handler)

  def mReportHandlerInit(): Unit = {
    if (mReportHandler.isEmpty) {
      setReportHandler(create(getName) { s => new UVMReportHandler(s) })
    }
  }

  def getReportVerbosityLevel(severity: uvmSeverity = UVM_INFO, idx: String = ""): uvmVerbosity = {
    mReportHandlerInit()
    mReportHandler.get.getVerbosityLevel(severity, idx)
  }

  def setReportVerbosityLevel(verbosity: uvmVerbosity): Unit = {
    mReportHandlerInit()
    mReportHandler.get.setVerbosityLevel(verbosity)
  }

  def uvmReportEnabled(verbosity: uvmVerbosity, severity: uvmSeverity = UVM_INFO, idx: String = ""): Boolean =
    getReportVerbosityLevel(severity, idx) >= verbosity

  def uvmReport(severity: uvmSeverity,
                idx: String,
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

    val reportMessage: UVMReportMessage = new UVMReportMessage()
    if (severity != UVM_INFO || reportEnabledChecked || uvmReportEnabled(verbosity, severity, idx)) {
      reportMessage.setReportMessage(severity, idx, msg, verbosity, trace, contextName)
      uvmProcessReportMessage(reportMessage)
    }
  }

  def uvmReportInfo(idx: String,
                    msg: String,
                    verbosity: uvmVerbosity = UVM_MEDIUM,
                    trace: String = "",
                    contextName: String = "",
                    reportEnabledChecked: Boolean = false): Unit = {
    uvmReport(UVM_INFO, idx, msg, verbosity, trace, contextName, reportEnabledChecked)
  }

  def uvmReportWarning(idx: String,
                       msg: String,
                       verbosity: uvmVerbosity = UVM_NONE,
                       trace: String = "",
                       contextName: String = "",
                       reportEnabledChecked: Boolean = false): Unit = {
    uvmReport(UVM_WARNING, idx, msg, verbosity, trace, contextName, reportEnabledChecked)
  }

  def uvmReportFatal(idx: String,
                     msg: String,
                     verbosity: uvmVerbosity = UVM_NONE,
                     trace: String = "",
                     contextName: String = "",
                     reportEnabledChecked: Boolean = false): Unit = {
    uvmReport(UVM_FATAL, idx, msg, verbosity, trace, contextName, reportEnabledChecked)
  }

  def uvmProcessReportMessage(reportMessage: UVMReportMessage): Unit = {
    mReportHandlerInit()
    reportMessage.setReportObject(this)
    mReportHandler.get.processReportMessage(reportMessage)
  }

  def uvmInfo(idx: String, msg: String, verbosity: uvmVerbosity): Unit = {
    if (uvmReportEnabled(verbosity, UVM_INFO, idx)) {
      uvmReportInfo(idx, msg, verbosity, getTrace(_traceLevel), reportEnabledChecked = true)
    }
  }

  def uvmWarning(idx: String, msg: String): Unit = {
    if (uvmReportEnabled(UVM_NONE, UVM_WARNING, idx)) {
      uvmReportWarning(idx, msg, UVM_NONE, getTrace(_traceLevel), reportEnabledChecked = true)
    }
  }

  def uvmFatal(idx: String, msg: String): Unit = {
    if (uvmReportEnabled(UVM_NONE, UVM_FATAL, idx)) {
      uvmReportFatal(idx, msg, UVM_NONE, getTrace(_traceLevel), reportEnabledChecked = true)
    }
  }
}

object UVMReportObject {
}
