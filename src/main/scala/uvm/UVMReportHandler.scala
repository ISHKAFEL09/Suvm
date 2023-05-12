package uvm

import scala.reflect.io.File
import collection.mutable
import ENUM_UVM_ACTION._

class UVMReportHandler(name: String = "UVMReportHandler") extends UVMObject(name) {
  private var mMaxVerbosityLevel: uvmVerbosity = UVM_MEDIUM

  private var defaultFileHandle: Option[File] = None

  private var severity2actions = mutable.HashMap.empty[uvmSeverity, Seq[uvmAction]]

  private var severity2File = mutable.HashMap.empty[uvmSeverity, Option[File]]

  def setSeverityAction(severity: uvmSeverity, action: Seq[uvmAction]): Unit = {
    severity2actions(severity) = action
  }

  def setSeverityFile(severity: uvmSeverity, file: Option[File]): Unit = {
    severity2File(severity) = file
  }

  def getVerbosityLevel(severity: uvmSeverity = UVM_INFO, idx: String = ""): uvmVerbosity = {
    mMaxVerbosityLevel // TODO:  
  }

  def initialize(): Unit = {
    defaultFileHandle = None
    mMaxVerbosityLevel = UVM_MEDIUM

    setSeverityAction(UVM_INFO, Seq(UVM_DISPLAY))
    setSeverityAction(UVM_WARNING, Seq(UVM_DISPLAY))
    setSeverityAction(UVM_ERROR, Seq(UVM_DISPLAY, UVM_COUNT))
    setSeverityAction(UVM_FATAL, Seq(UVM_DISPLAY))

    setSeverityFile(UVM_INFO, defaultFileHandle)
    setSeverityFile(UVM_WARNING, defaultFileHandle)
    setSeverityFile(UVM_ERROR, defaultFileHandle)
    setSeverityFile(UVM_FATAL, defaultFileHandle)
  }
}
