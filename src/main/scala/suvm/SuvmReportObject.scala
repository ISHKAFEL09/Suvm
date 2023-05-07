package suvm

import SuvmObjectGlobals._
import SuvmImplicits._
import java.io.File

/**
 *  The uvm_report_object provides an interface to the UVM reporting facility.
 *  Through this interface, components issue the various messages that occur
 *  during simulation. Users can configure what actions are taken and what
 *  file(s) are output for individual messages from a particular component
 *  or for all messages from all components in the environment. Defaults are
 *  applied where there is no explicit configuration.
 */
class SuvmReportObject(val name: String = "") extends SuvmObject {
  /**
   * reporting
   */
  def suvmGetReportObject: SuvmReportObject = this

  /**
   * if the configured verbosity for this severity/id is greater than or equal to ~verbosity~
   */
  def suvmReportEnabled(verbosity: Int,
                        severity: SuvmSeverity.Value = SuvmSeverity.UVM_INFO,
                        id: String = ""): Boolean =
    getReportVerbosityLevel(severity, id) >= verbosity

  /**
   * primary reporting methods
   */
  def suvmReport(severity: SuvmSeverity.Value, id: String, message: String,
                 verbosity: Int, filename: String = "", line: Int = 0,
                 contextName: String = "", reportEnabledChecked: Boolean = false): Unit = {
    val noReport = severity == SuvmSeverity.UVM_INFO && !reportEnabledChecked &&
      !suvmReportEnabled(verbosity, severity, id)
    if (!noReport) {
      val reportMessage = SuvmReportMessage.newReportMessage()
      reportMessage.setReportMessage(severity, id, message, verbosity, filename, line, contextName)
      suvmProcessReportMessage(reportMessage)
    }
  }

  def suvmReportInfo(id: String, message: String, verbosity: Int = SuvmVerbosity.UVM_MEDIUM,
                     filename: String = "", line: Int = 0, contextName: String = "",
                     reportEnabledChecked: Boolean = false): Unit =
    suvmReport(SuvmSeverity.UVM_INFO, id, message, verbosity, filename, line, contextName, reportEnabledChecked)

  def suvmReportWarning(id: String, message: String, verbosity: Int = SuvmVerbosity.UVM_NONE,
                        filename: String = "", line: Int = 0, contextName: String = "",
                        reportEnabledChecked: Boolean = false): Unit =
    suvmReport(SuvmSeverity.UVM_WARNING, id, message, verbosity, filename, line, contextName,
      reportEnabledChecked)

  def suvmReportError(id: String, message: String, verbosity: Int = SuvmVerbosity.UVM_NONE,
                      filename: String = "", line: Int = 0, contextName: String = "",
                      reportEnabledChecked: Boolean = false): Unit =
    suvmReport(SuvmSeverity.UVM_ERROR, id, message, verbosity, filename, line, contextName, reportEnabledChecked)

  def suvmReportFatal(id: String, message: String, verbosity: Int = SuvmVerbosity.UVM_NONE,
                      filename: String = "", line: Int = 0, contextName: String = "",
                      reportEnabledChecked: Boolean = false): Unit =
    suvmReport(SuvmSeverity.UVM_FATAL, id, message, verbosity, filename, line, contextName, reportEnabledChecked)

  def suvmProcessReportMessage(reportMessage: SuvmReportMessage): Unit = {
    mReportHandleInit()
    reportMessage.setReportObject(this)
    mReportHandle.processReportMessage(reportMessage)
  }

  /**
   * verbosity configuration
   */
  def getReportVerbosityLevel(severity: SuvmSeverity.Value = SuvmSeverity.UVM_INFO, id: String = ""): Int = {
    mReportHandleInit()
    mReportHandle.getVerbosityLevel(severity, id)
  }

  def getReportMaxVerbosityLevel: Int ={
    mReportHandleInit()
    mReportHandle.getMaxVerbosityLevel
  }

  def setReportVerbosityLevel(verbosity: Int) = {
    mReportHandleInit()
  }

  def setReportHandler(handler: SuvmReportHandler): Unit = mReportHandle = handler

  def getReportAction(severity: SuvmSeverity.Value, id: String): SuvmAction.Value =
    ???

  def setReportIdFile(id: String, file: Option[File]): Unit =
    ???

  def setReportIdAction(id: String, action: SuvmAction.Value): Unit =
    println(s"$id: $action")

  def suvmInfo(id: String, msg: String, verbosity: SuvmVerbosity.Value): Unit =
    SuvmMessage.suvmInfo(id, msg, verbosity, suvmReportEnabled, suvmReportInfo)

  def suvmWarning(id: String, msg: String): Unit =
    SuvmMessage.suvmWarning(id, msg, suvmReportEnabled, suvmReportWarning)

  private def mReportHandleInit(): Unit = {
    if (!mReportHandleSet) setReportHandler(SuvmReportHandler.typeId.create(getName))
  }

  private var mReportHandleSet: Boolean = _
  private var mReportHandle: SuvmReportHandler = _
}
