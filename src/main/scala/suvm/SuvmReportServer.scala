package suvm

import SuvmObjectGlobals._
import SuvmImplicits._

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class SuvmReportServer(val name: String = "base") extends SuvmObject {
  /**
   * @return the currently configured max quit count
   */
  def getMaxQuitCount: Int

  /**
   * set the currently configured max quit count
   */
  def setMaxQuitCount(count: Int, overridable: Boolean = true): Unit

  /**
   * @return the current number of UVM_QUIT actions already passed through this server
   */
  def getQuitCount: Int

  /**
   * specify the current number of UVM_QUIT
   */
  def setQuitCount(quitCount: Int): Unit

  /**
   * @return the count of already passed messages with severity `severity`
   */
  def getSeverityCount(severity: SuvmSeverity.Value): Int

  /**
   * specify the count of already passed messages with severity `severity`
   */
  def setSeverityCount(severity: SuvmSeverity.Value, count: Int): Unit

  /**
   * @return the count of already passed messages with `id`
   */
  def getIdCount(id: String): Int

  /**
   * @return the set of ids already used by this `SuvmReportServer`
   */
  def getIdSet: Seq[String]

  /**
   * @return the set of severities already used by this `SuvmReportServer`
   */
  def getSeveritySet: Seq[SuvmSeverity.Value]

  /**
   * @return the `SuvmTrDatabase` used for recording messages
   */
  def getMessageDatabase: Option[SuvmTrDatabase]

  /**
   * specify the `SuvmTrDatabase` used for recording messages
   */
  def setMessageDatabase(database: Option[SuvmTrDatabase]): Unit

  override def doCopy(rhs: SuvmObject): Unit = rhs match {
    case rhs_ : SuvmReportServer =>
      rhs_.getSeveritySet.foreach(s => setSeverityCount(s, rhs_.getSeverityCount(s)))
      rhs_.getIdSet.foreach(i => setIdCount(i, rhs_.getIdCount(i)))
      setMessageDatabase(rhs_.getMessageDatabase)
      setMaxQuitCount(rhs_.getMaxQuitCount)
      setQuitCount(rhs_.getQuitCount)
    case _ =>
      suvmError("UVM/REPORT/SERVER/RPTCOPY", "cannot copy to server from the given datatype")
  }

  /**
   * main entry point for the `SuvmReportServer`, processing `SuvmReportMessage`
   */
  def processReportMessage(reportMessage: SuvmReportMessage): Unit

  /**
   * Intended to construct the actual string sent to the file or command line from the severity,
   * component name, report id, and the message itself.
   */
  def composeReportMessage(reportMessage: SuvmReportMessage, reportObjectName: String = ""): String

  /**
   * Processes the provided report_message per the actions contained within.
   */
  def executeReportMessage(reportMessage: SuvmReportMessage, composedMessage: String): Unit

  /**
   * output statistical information on the reports issued by this central report server.
   */
  def reportSummarize(f: Option[File] = None): Unit

  protected def setIdCount(id: String, count: Int): Unit
}

class SuvmDefaultReportServer(name: String = "SuvmReportServer") extends SuvmReportServer(name) {
  var enableReportIdCountSummary = true
  var recordAllMessages = false
  var showVerbosity = false
  var showTerminator = false

  override def doPrint(printer: SuvmPrinter): Unit = {
    printer.printField("QuitCount", mQuitCount, mQuitCount.bitLength, SuvmRadix.UVM_DEC,
      '.', "Int")
    printer.printField("MaxQuitCount", mMaxQuitCount, mMaxQuitCount.bitLength, SuvmRadix.UVM_DEC,
      '.', "Int")
    printer.printField("MaxQuitOverridable", Bool2Int(maxQuitOverridable), 1, SuvmRadix.UVM_BIN,
      '.', "Bit")
    if (mSeverityCount.nonEmpty) {
      printer.printArrayHeader("SeverityCount", mSeverityCount.size, "Severity counts")
      mSeverityCount.foreach { case(sev, cnt) =>
        printer.printField(s"[$sev]", cnt, 32, SuvmRadix.UVM_DEC)
      }
      printer.printArrayFooter()
    }
    if (mIdCount.nonEmpty) {
      printer.printArrayHeader("IdCount", mIdCount.size, "Id counts")
      mIdCount.foreach { case(id, cnt) =>
        printer.printField(s"[$id]", cnt, 32, SuvmRadix.UVM_DEC)
      }
      printer.printArrayFooter()
    }
    printer.printField("EnableReportIdCountSummary", Bool2Int(enableReportIdCountSummary), 1,
      SuvmRadix.UVM_BIN, '.', "Bit")
    printer.printField("RecordAllMessages", Bool2Int(recordAllMessages), 1,
      SuvmRadix.UVM_BIN, '.', "Bit")
    printer.printField("ShowVerbosity", Bool2Int(showVerbosity), 1,
      SuvmRadix.UVM_BIN, '.', "Bit")
    printer.printField("ShowTerminator", Bool2Int(showTerminator), 1,
      SuvmRadix.UVM_BIN, '.', "Bit")
  }

  /**
   * quit count
   */
  override def getMaxQuitCount: Int = mMaxQuitCount

  override def setMaxQuitCount(count: Int, overridable: Boolean): Unit =
    if (maxQuitOverridable) {
      maxQuitOverridable = overridable
      mMaxQuitCount = if (count < 0) 0 else count
    } else {
      suvmReportInfo("NOMAXQUITOVR", s"The max quit count setting of $mMaxQuitCount is not " +
        s"overridable to + $count due to a previous setting", SuvmVerbosity.UVM_NONE)
    }

  override def getQuitCount: Int = mQuitCount

  override def setQuitCount(quitCount: Int): Unit = mQuitCount = if (quitCount < 0) 0 else quitCount

  def incrQuitCount(): Unit = mQuitCount += 1

  def resetQuitCount(): Unit = mQuitCount = 0

  def isQuitCountReached: Boolean = mQuitCount >= mMaxQuitCount

  /**
   * severity count
   */
  override def getSeverityCount(severity: SuvmObjectGlobals.SuvmSeverity.Value): Int =
    mSeverityCount.getOrElse(severity, SuvmSeverity.UVM_INFO)

  override def setSeverityCount(severity: SuvmObjectGlobals.SuvmSeverity.Value, count: Int): Unit =
    mSeverityCount.update(severity, if (count < 0) 0 else count)

  def incrSeverityCount(severity: SuvmSeverity.Value): Unit = {
    val i = mSeverityCount.getOrElse(severity, 0) + 1
    mSeverityCount.update(severity, i)
  }

  def resetSeverityCounts(): Unit = mSeverityCount = mutable.HashMap.empty[SuvmSeverity.Value, Int]

  /**
   * id count
   */
  override def getIdCount(id: String): Int = mIdCount.getOrElse(id, 0)

  override def setIdCount(id: String, count: Int): Unit = mIdCount.update(id, if (count < 0) 0 else count)

  def incrIdCount(id: String): Unit = {
    val i = mIdCount.getOrElse(id, 0) + 1
    mIdCount.update(id, i)
  }

  /**
   * message recording
   */
  override def setMessageDatabase(database: Option[SuvmTrDatabase]): Unit = mMessageDb = database

  override def getMessageDatabase: Option[SuvmTrDatabase] = mMessageDb

  override def getSeveritySet: Seq[SuvmObjectGlobals.SuvmSeverity.Value] = mSeverityCount.keys.toSeq

  override def getIdSet: Seq[String] = mIdCount.keys.toSeq

  def fDisplay(file: Option[File], str: String): Unit = fWrite(file, str)

  /**
   * process report message
   */
  override def processReportMessage(reportMessage: SuvmReportMessage): Unit = {
    reportMessage.setReportServer(this)
    if (SuvmReportCatcher.processAllReportCatchers(reportMessage) &&
      reportMessage.getAction != SuvmAction.UVM_NO_ACTION.id) {
      val m =
        if ((reportMessage.getAction & (SuvmAction.UVM_DISPLAY | SuvmAction.UVM_LOG)) == 0) ""
        else SuvmCoreService.getReportServer.composeReportMessage(reportMessage)
      SuvmCoreService.getReportServer.executeReportMessage(reportMessage, m)
    }
  }

  override def executeReportMessage(reportMessage: SuvmReportMessage, composedMessage: String): Unit = {
    incrSeverityCount(reportMessage.getSeverity)
    incrIdCount(reportMessage.getId)

    def hasAction(act: SuvmAction.Value): Boolean = (reportMessage.getAction & act) != 0

    import SuvmAction._
    if (recordAllMessages)
      reportMessage.setAction(reportMessage.getAction | UVM_RM_RECORD)
    // UVM_RM_RECORD action
    if (hasAction(UVM_RM_RECORD)) {
      var s: Option[SuvmTrStream] = None
      val ro = reportMessage.getReportObject
      val rh = reportMessage.getReportHandle
      if (mStreams.contains(ro.getName) && mStreams(ro.getName).contains(rh.getName))
        s = mStreams(ro.getName)(rh.getName)
      if (s.isEmpty) {
        val db = getMessageDatabase.getOrElse(SuvmCoreService.getDefaultTrDatabase)
        s = db.openStream(ro.getName, rh.getName, "MESSAGES")
        mStreams(ro.getName)(rh.getName) = s
      }
      if (s.nonEmpty) {
        val recorder = s.get.openRecorder(reportMessage.getName, typeName = reportMessage.getTypeName)
        if (recorder.nonEmpty) {
          reportMessage.recordObj(recorder)
          recorder.get.free()
        }
      }
    }
    // DISPLAY action
    if (hasAction(UVM_DISPLAY)) println(composedMessage)
    // LOG action
    if (hasAction(UVM_LOG)) fWrite(reportMessage.getFile, composedMessage)
    // UVM_COUNT action
    if (hasAction(UVM_COUNT)) {
      if (getMaxQuitCount != 0) {
        incrQuitCount()
        if (isQuitCountReached) reportMessage.setAction(reportMessage.getAction | UVM_EXIT)
      }
    }
    // UVM_EXIT action
    if (hasAction(UVM_EXIT)) SuvmRoot.get.die()
    // UVM_STOP action
    if (hasAction(UVM_STOP)) sys.exit(0)
  }

  override def composeReportMessage(reportMessage: SuvmReportMessage, reportObjectName: String): String =  {
    val filenameLineString =
      if (reportMessage.getFilename.nonEmpty) s"${reportMessage.getFilename}(${reportMessage.getLine}) " else ""

    val contextStr = if (reportMessage.getContext.nonEmpty) s"@${reportMessage.getContext}" else ""

    val verbosityStr = if (showVerbosity) {
      SuvmVerbosity.find(reportMessage.getVerbosity) match {
        case Some(value) => s"($value)"
        case None => s"(${reportMessage.getVerbosity})"
      }
    } else ""

    val terminatorStr = if (showTerminator) s" -${reportMessage.getSeverity}" else ""

    val container = reportMessage.getElementContainer
    val suvmDefaultPrinter = SuvmPrinter.getDefault
    val prefix = suvmDefaultPrinter.getLinePrefix
    suvmDefaultPrinter.setLinePrefix(" +")
    val msgBodyStr = {
      if (container.size == 0) reportMessage.getMessage
      else s"${reportMessage.getMessage}\n${container.sPrintObj()}"
    }
    suvmDefaultPrinter.setLinePrefix(prefix)

    val roName = if (reportObjectName.isEmpty) reportMessage.getReportHandle.getFullName else reportObjectName

    s"${reportMessage.getSeverity} $verbosityStr $filenameLineString@ " +
      s"$realtime: $roName $contextStr [${reportMessage.getId}] $msgBodyStr$terminatorStr"
  }

  def reportSummarize(file: Option[File] = None): Unit = {
    SuvmReportCatcher.summarize(file)
    val q = ArrayBuffer.empty[String]
    q += "\n--- UVM Report Summary ---\n\n"

    if (mMaxQuitCount != 0) {
      if (mQuitCount >= mMaxQuitCount) {
        q += "Quit count reached!\n"
      }
      q += f"Quit count : $mQuitCount%5d of $mMaxQuitCount%5d\n"
    }

    q += "** Report counts by severity\n"
    mSeverityCount.keys.foreach(i => q += f"$i%s :${mSeverityCount(i)}%5d\n")

    if (enableReportIdCountSummary) {
      q += "** Report counts by id\n"
      mIdCount.keys.foreach(i => q += f"[$i] ${mIdCount(i)}%5d\n")
    }

    val pq = q.mkString

    if (file.isEmpty)
      suvmInfo("UVM/REPORT/SERVER", pq, SuvmVerbosity.UVM_NONE)
    else {
      val root = SuvmRoot.get
      val action = root.getReportAction(SuvmSeverity.UVM_INFO, "UVM/REPORT/SERVER")
      root.setReportIdAction("UVM/REPORT/SERVER", SuvmAction.UVM_LOG)
      root.setReportIdFile("UVM/REPORT/SERVER", file)
      suvmInfo("UVM/REPORT/SERVER", pq, SuvmVerbosity.UVM_NONE)
      root.setReportIdAction("UVM/REPORT/SERVER", action)
    }
  }

  private var mQuitCount: Int = _
  private var mMaxQuitCount: Int = _
  private var maxQuitOverridable: Boolean = true
  private var mSeverityCount = mutable.HashMap.empty[SuvmSeverity.Value, Int]
  private var mIdCount = mutable.HashMap.empty[String, Int]
  private var mMessageDb: Option[SuvmTrDatabase] = None
  private var mStreams = mutable.HashMap.empty[String, mutable.HashMap[String, Option[SuvmTrStream]]]

  setMaxQuitCount(0)
  resetQuitCount()
  resetSeverityCounts()
}

object SuvmReportServer {
  /**
   * @return the global report server used for reporting
   */
  def getServer: SuvmReportServer = SuvmCoreService.getReportServer

  /**
   * specifies the global report server to use for reporting
   */
  def setServer(server: SuvmReportServer): Unit = SuvmCoreService.setReportServer(server)
}