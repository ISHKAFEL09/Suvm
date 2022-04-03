package suvm

import suvm.SuvmObjectGlobals._

import java.io.File


abstract class SuvmReportObject extends SuvmObject {
  def setReportHandler(handler: SuvmReportHandler): Unit =
    ???

  def getReportAction(severity: SuvmSeverity.Value, id: String): SuvmActionType.Value =
    ???

  def setReportIdFile(id: String, file: File): Unit =
    ???

  def setReportIdAction(id: String, action: SuvmActionType.Value): Unit =
    ???

  def suvmReportEnabled(verbosity: SuvmVerbosity.Value,
                        severity: SuvmSeverity.Value,
                        id: String): Boolean =
    ???

  def suvmReportInfo(id: String,
                     msg: String,
                     verbosity: SuvmVerbosity.Value,
                     filename: String,
                     line: Int,
                     contextName: String,
                     enabled: Boolean): Unit =
    ???

  def suvmReportWarning(id: String,
                     msg: String,
                     verbosity: SuvmVerbosity.Value,
                     filename: String,
                     line: Int,
                     contextName: String,
                     enabled: Boolean): Unit =
    ???

  def suvmInfo(id: String, msg: String, verbosity: SuvmVerbosity.Value): Unit =
    SuvmMessage.suvmInfo(id, msg, verbosity, suvmReportEnabled, suvmReportInfo)

  def suvmWarning(id: String, msg: String): Unit =
    SuvmMessage.suvmWarning(id, msg, suvmReportEnabled, suvmReportWarning)
}
