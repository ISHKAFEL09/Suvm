package uvm

import scala.reflect.io.File

class UVMReportMessage(name: String = "UVMReportMessage") extends UVMObject(name) {
  var contextName: String = ""
  var trace: String = ""
  var severity: uvmSeverity = UVM_INFO
  var idx: String = ""
  var msg = ""
  var verbosity: uvmVerbosity = UVM_NONE
  var obj: Option[UVMReportObject] = None
  var file: Option[File] = None
  var rh: Option[UVMReportHandler] = None
  var action: Option[uvmAction] = None

  def setReportMessage(severity: uvmSeverity,
                       idx: String,
                       msg: String,
                       verbosity: uvmVerbosity,
                       trace: String,
                       contextName: String): Unit = {
    this.contextName = contextName
    this.trace = trace
    this.severity = severity
    this.idx = idx
    this.msg = msg
    this.verbosity = verbosity
  }

  def setReportObject(obj: UVMReportObject): Unit = {
    this.obj = Some(obj)
  }

  def setFile(file: File): Unit = this.file = Some(file)

  def getFile: File = this.file.getOrElse(throw new ExceptionInInitializerError("File not initialized"))

  def setReportHandler(uvmReportHandler: UVMReportHandler): Unit = rh = Some(uvmReportHandler)

  def setAction(action: uvmAction): Unit = this.action = Some(action)
}
