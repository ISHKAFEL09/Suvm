package suvm

import SuvmObjectGlobals._
import SuvmImplicits._
import suvm.SuvmReportHandler.formatAction

import java.io.File
import scala.collection.mutable.ArrayBuffer

class SuvmIdActionArray extends SuvmPool[String, SuvmAction.Value] {
  override def defaultValue(key: String): SuvmAction.Value = SuvmAction.UVM_NO_ACTION
}

class SuvmSevActionArray extends SuvmPool[SuvmSeverity.Value, SuvmAction.Value] {
  override def defaultValue(key: SuvmSeverity.Value): SuvmAction.Value = SuvmAction.UVM_NO_ACTION
}

class SuvmSevIdActionArray extends SuvmPool[SuvmSeverity.Value, SuvmIdActionArray] {
  override def defaultValue(key: SuvmSeverity.Value): SuvmIdActionArray = new SuvmIdActionArray
}

class SuvmIdFileArray extends SuvmPool[String, Option[File]] {
  override def defaultValue(key: String): Option[File] = None
}

class SuvmSevFileArray extends SuvmPool[SuvmSeverity.Value, Option[File]] {
  override def defaultValue(key: SuvmSeverity.Value): Option[File] = None
}

class SuvmSevIdFileArray extends SuvmPool[SuvmSeverity.Value, SuvmIdFileArray] {
  override def defaultValue(key: SuvmSeverity.Value): SuvmIdFileArray = new SuvmIdFileArray
}

class SuvmIdVerbArray extends SuvmPool[String, Int] {
  override def defaultValue(key: String): Int = SuvmVerbosity.UVM_NONE
}

class SuvmSevIdVerbArray extends SuvmPool[SuvmSeverity.Value, SuvmIdVerbArray] {
  override def defaultValue(key: SuvmSeverity.Value): SuvmIdVerbArray = new SuvmIdVerbArray
}

class SuvmSevOverrideArray extends SuvmPool[SuvmSeverity.Value, SuvmSeverity.Value] {
  override def defaultValue(key: SuvmSeverity.Value): SuvmSeverity.Value = SuvmSeverity.UVM_INFO
}

class SuvmIdSevOverrideArray extends SuvmPool[String, SuvmSevOverrideArray] {
  override def defaultValue(key: String): SuvmSevOverrideArray = new SuvmSevOverrideArray
}

/**
  * The uvm_report_handler is the class to which most methods in
  * <uvm_report_object> delegate. It stores the maximum verbosity, actions,
  * and files that affect the way reports are handled.
  * The relationship between <uvm_report_object> (a base class for uvm_component)
  * and uvm_report_handler is typically one to one, but it can be many to one
  * if several uvm_report_objects are configured to use the same
  * uvm_report_handler_object. See <uvm_report_object::set_report_handler>.
  *
  * The relationship between uvm_report_handler and <uvm_report_server> is many
  * to one.
 */
class SuvmReportHandler(val name: String = "SuvmReportHandler") extends SuvmObject {
  /**
   * The uvm_report_handler implements the <uvm_object::do_print()> such that
   * ~print~ method provides UVM printer formatted output
   * of the current configuration.
   */
  override def doPrint(printer: SuvmPrinter): Unit = {
    printMaxVerb(printer)
    printIdVerbs(printer)
    printSevIdVerbs(printer)
    printIdActions(printer)
    printSevActions(printer)
    printSevIdActions(printer)
    printSevOverrides(printer)
    printSevIdOverrides(printer)
    // default file handle
    printer.printField("DefaultFileHandle", defaultFileHandle, 32, 
      SuvmRadix.UVM_HEX, '.', "Int")
    printIdFiles(printer)
    printSevFiles(printer)
    printSevIdFiles(printer)
  }

  /**
   * verbosity configuration
   */
  def getVerbosityLevel(severity: SuvmSeverity.Value = SuvmSeverity.UVM_INFO, id: String = ""): Int = {
    if (severityIdVerbosities.exists(severity) && severityIdVerbosities.get(severity).exists(id)) {
      severityIdVerbosities.get(severity).get(id)
    }
    else if (idVerbosities.exists(id))
      idVerbosities.get(id)
    else
      mMaxVerbosityLevel
  }

  def setVerbosityLevel(l: Int): Unit = mMaxVerbosityLevel = l

  def setIdVerbosity(id: String, verbosity: Int): Unit = idVerbosities.add(id, verbosity)

  def setSeverityIdVerbosity(severity: SuvmSeverity.Value, id: String, verbosity: Int): Unit = {
    severityIdVerbosities.get(severity).add(id, verbosity)
  }

  /**
   * action configuration
   */
  def getAction(severity: SuvmSeverity.Value, id: String): SuvmAction.Value = {
    if (severityIdActions.exists(severity) && severityIdActions.get(severity).exists(id))
      severityIdActions.get(severity).get(id)
    else if (idActions.exists(id))
      idActions.get(id)
    else
      severityActions.get(severity)
  }

  def setSeverityIdAction(severity: SuvmSeverity.Value, id: String, action: SuvmAction.Value): Unit =
    severityIdActions.get(severity).add(id, action)

  def setIdAction(id: String, action: SuvmAction.Value): Unit =
    idActions.add(id, action)

  def setSeverityAction(severity: SuvmSeverity.Value, action: SuvmAction.Value): Unit =
    severityActions.add(severity, action)

  /**
   * file configuration
   */
  def getFileHandle(severity: SuvmSeverity.Value, id: String): Option[File] = {
    var ret = severityIdFileHandles.get(severity).get(id)
    if (ret.isEmpty)
      ret = idFileHandles.get(id)
    if (ret.isEmpty)
      ret = severityFileHandles.get(severity)
    if (ret.isEmpty) defaultFileHandle else ret
  }

  def setSeverityFile(severity: SuvmSeverity.Value, file: Option[File]): Unit =
    severityFileHandles.add(severity, file)

  def setIdFile(id: String, file: Option[File]): Unit =
    idFileHandles.add(id, file)

  def setSeverityIdFile(severity: SuvmSeverity.Value, id: String, file: Option[File]): Unit =
    severityIdFileHandles.get(severity).add(id, file)

  def setDefaultFile(f: Option[File]): Unit = defaultFileHandle = f

  /**
   * override configuration
   */
  def setSeverityOverride(curSeverity: SuvmSeverity.Value, newSeverity: SuvmSeverity.Value): Unit =
    sevOverrides.add(curSeverity, newSeverity)

  def setSeverityIdOverride(curSeverity: SuvmSeverity.Value, id: String, newSeverity: SuvmSeverity.Value): Unit =
    idSevOverrides.get(id).add(curSeverity, newSeverity)

  /**
   * message processing
   */
  def processReportMessage(reportMessage: SuvmReportMessage): Unit = {
    val srvr = SuvmReportServer.getServer
    val id = reportMessage.getId
    val severity = reportMessage.getSeverity
    if (idSevOverrides.exists(id) && idSevOverrides.get(id).exists(severity))
      reportMessage.setSeverity(idSevOverrides.get(id).get(severity))
    else if (sevOverrides.exists(severity))
      reportMessage.setSeverity(sevOverrides.get(severity))
    reportMessage.setFile(getFileHandle(severity, id))
    reportMessage.setReportHandle(this)
    reportMessage.setAction(getAction(severity, id))
    srvr.processReportMessage(reportMessage)
  }

  /**
   * @return max verbosity level
   */
  def getMaxVerbosityLevel: Int = mMaxVerbosityLevel

  /**
   * max verb
   */
  private def printMaxVerb(printer: SuvmPrinter): Unit = {
    SuvmVerbosity.values.find(_.id == mMaxVerbosityLevel) match {
      case Some(value) =>
        printer.printGeneric("MaxVerbosityLevel", "SuvmVerbosity", 32, value.toString)
      case _ =>
        printer.printField("MaxVerbosityLevel", mMaxVerbosityLevel, 32,
          SuvmRadix.UVM_DEC, '.', "Int")
    }
  }

  /**
   * id verbs
   */
  private def printIdVerbs(printer: SuvmPrinter): Unit = {
    if (idVerbosities.nonEmpty) {
      printer.printArrayHeader("IdVerbosities", idVerbosities.num, "SuvmPool")
      idVerbosities foreach { case(id, verb) =>
        SuvmVerbosity.find(verb) match {
          case Some(v) =>
            printer.printGeneric(s"[$id]", "SuvmVerbosity", 32, s"$v")
          case None =>
            printer.printGeneric(s"[$id]", "Int", 32, s"$verb")
        }
      }
      printer.printArrayFooter()
    }
  }

  /**
   * sev and id verbs
   */
  private def printSevIdVerbs(printer: SuvmPrinter): Unit = {
    if (severityIdVerbosities.nonEmpty) {
      printer.printArrayHeader("SeverityIdVerbosities", severityIdVerbosities.values.map(_.num).sum,
        "Array")
      severityIdVerbosities foreach { case (severity, idVerb) =>
        idVerb foreach { case (id, verb) =>
          SuvmVerbosity.values.find(_.id == idVerb.get(id)) match {
            case Some(value) =>
              printer.printGeneric(s"[$severity:$id]", "SuvmVerbosity", 32,
                value.toString)
            case None =>
              printer.printGeneric(s"[$severity:$id]", "SuvmVerbosity", 32,
                s"$verb")
          }
        }
      }
      printer.printArrayFooter()
    }
  }

  /**
   * id actions
   */
  private def printIdActions(printer: SuvmPrinter): Unit = {
    if (idActions.nonEmpty) {
      printer.printArrayHeader("IdActions", idActions.num, "SuvmPool")
      idActions foreach { case (id, act) =>
        printer.printGeneric(s"$id", "SuvmAction", 32, formatAction(act))
      }
      printer.printArrayFooter()
    }
  }

  /**
   * severity actions
   */
  private def printSevActions(printer: SuvmPrinter): Unit = {
    if (severityActions.nonEmpty) {
      printer.printArrayHeader("SeverityActions", 4, "Array")
      severityActions foreach { case (severity, action) =>
        printer.printGeneric(s"[$severity]", "SuvmAction", 32, formatAction(action))
      }
      printer.printArrayFooter()
    }
  }

  /**
   * sev and id actions
   */
  private def printSevIdActions(printer: SuvmPrinter): Unit = {
    if (severityIdActions.nonEmpty) {
      printer.printArrayHeader("SeverityIdActions", severityIdActions.values.map(_.num).sum,
        "Array")
      severityIdActions foreach { case (severity, ida) =>
        ida foreach { case(id, act) =>
          printer.printGeneric(s"$severity:$id", "SuvmAction", 32, formatAction(act))
        }
      }
      printer.printArrayFooter()
    }
  }

  /**
   * severity overrides
   */
  private def printSevOverrides(printer: SuvmPrinter): Unit = {
    if (sevOverrides.nonEmpty) {
      printer.printArrayHeader("SeverityOverrides", sevOverrides.num, "SuvmPool")
      sevOverrides foreach { case(sev, overSev) =>
        printer.printGeneric(s"[$sev]", "SuvmSeverity", 32, s"$overSev")
      }
      printer.printArrayFooter()
    }
  }

  /**
   * sev and id overrides
   */
  private def printSevIdOverrides(printer: SuvmPrinter): Unit = {
    if (idSevOverrides.nonEmpty) {
      printer.printArrayHeader("SeverityIdOverrides", idSevOverrides.values.map(_.num).sum,
        "Array")
      idSevOverrides foreach { case (id, sevOvers) =>
        sevOvers foreach { case (sev, sevOver) =>
          printer.printGeneric(s"$sev:$id", "SuvmSeverity", 32, s"$sevOver")
        }
      }
      printer.printArrayFooter()
    }
  }

  /**
   * id files
   */
  private def printIdFiles(printer: SuvmPrinter): Unit = {
    if (idFileHandles.nonEmpty) {
      printer.printArrayHeader("IdFileHandles", idFileHandles.num, "SuvmPool")
      idFileHandles foreach { case(id, file) =>
        printer.printField(s"$id", file, 32, SuvmRadix.UVM_HEX, '.',
          "UVM_FILE")
      }
      printer.printArrayFooter()
    }
  }

  /**
   * severity files
   */
  private def printSevFiles(printer: SuvmPrinter): Unit = {
    if (severityFileHandles.nonEmpty) {
      printer.printArrayHeader("SeverityFileHandles", 4, "Array")
      severityFileHandles foreach { case (severity, file) =>
        printer.printField(s"[$severity]", file, 32, SuvmRadix.UVM_HEX, '.',
          "UVM_FILE")
      }
      printer.printArrayFooter()
    }
  }

  /**
   * sev and id files
   */
  private def printSevIdFiles(printer: SuvmPrinter): Unit = {
    if (severityIdFileHandles.nonEmpty) {
      printer.printArrayHeader("SeverityIdFileHandles", severityIdFileHandles.values.map(_.num).sum,
        "Array")
      severityIdFileHandles foreach { case (severity, idx) =>
        idx foreach { case(id, file) =>
          printer.printField(s"$severity:$id", file, 32, SuvmRadix.UVM_HEX, '.',
            "UVM_FILE")
        }
      }
      printer.printArrayFooter()
    }
  }

  private var mMaxVerbosityLevel: Int = SuvmVerbosity.UVM_MEDIUM
  /**
   * actions
   */
  private val idActions = new SuvmIdActionArray
  private val severityActions = new SuvmSevActionArray
  private val severityIdActions = new SuvmSevIdActionArray
  /**
   * id verbosity settings : default and severity
   */
  private val idVerbosities = new SuvmIdVerbArray
  private val severityIdVerbosities = new SuvmSevIdVerbArray
  /**
   * file handles : default, severity, action, (severity,id)
   */
  private var defaultFileHandle: Option[File] = None
  private val idFileHandles = new SuvmIdFileArray
  private val severityFileHandles = new SuvmSevFileArray
  private val severityIdFileHandles = new SuvmSevIdFileArray
  /**
   * severity overrides
    */
  private val sevOverrides = new SuvmSevOverrideArray
  private val idSevOverrides = new SuvmIdSevOverrideArray

  import SuvmSeverity._, SuvmAction._
  setSeverityAction(UVM_INFO, UVM_DISPLAY)
  setSeverityAction(UVM_WARNING, UVM_DISPLAY)
  setSeverityAction(UVM_ERROR, UVM_DISPLAY | UVM_COUNT)
  setSeverityAction(UVM_FATAL, UVM_DISPLAY | UVM_EXIT)
  setSeverityFile(UVM_INFO, defaultFileHandle)
  setSeverityFile(UVM_WARNING, defaultFileHandle)
  setSeverityFile(UVM_ERROR, defaultFileHandle)
  setSeverityFile(UVM_FATAL, defaultFileHandle)
}

object SuvmReportHandler extends SuvmObjectUtils[SuvmReportHandler] {
  override def create: String => SuvmReportHandler = new SuvmReportHandler(_)

  def formatAction(action: SuvmAction.Value): String = {
    val s = ArrayBuffer.empty[String]
    if (action == SuvmAction.UVM_NO_ACTION) s += "NO ACTION"
    else {
      if (action.hasOp(SuvmAction.UVM_DISPLAY)) s += "DISPLAY"
      if (action.hasOp(SuvmAction.UVM_LOG)) s += "LOG"
      if (action.hasOp(SuvmAction.UVM_RM_RECORD)) s += "RM_RECORD"
      if (action.hasOp(SuvmAction.UVM_COUNT)) s += "COUNT"
      if (action.hasOp(SuvmAction.UVM_CALL_HOOK)) s += "CALL_HOOK"
      if (action.hasOp(SuvmAction.UVM_EXIT)) s += "EXIT"
      if (action.hasOp(SuvmAction.UVM_STOP)) s += "STOP"
    }
    s.mkString(" ")
  }
}

object SuvmReportHandlerTest extends App {
  implicit val config: SuvmConfig = SuvmConfig(true)
  SuvmRoot.init

  val r = SuvmReportHandler.typeId.create("r")
  val printer = new SuvmPrinter {
    override val name: String = "printer"
  }
  r.setVerbosityLevel(SuvmVerbosity.UVM_FULL)
  r.setIdVerbosity("ID1", SuvmVerbosity.UVM_LOW)
  r.setSeverityIdOverride(SuvmSeverity.UVM_INFO, "ID4", SuvmSeverity.UVM_FATAL)
  r.setSeverityIdVerbosity(SuvmSeverity.UVM_INFO, "ID3", 501)
  r.setIdAction("ACT_ID", SuvmAction.UVM_LOG | SuvmAction.UVM_DISPLAY)
  r.setSeverityAction(SuvmSeverity.UVM_INFO, SuvmAction.UVM_LOG | SuvmAction.UVM_DISPLAY)
  r.setDefaultFile(Some(new File("defaultFile")))
  r.doPrint(printer)

  val m = SuvmReportMessage.typeId.create("reportMessage")
  m.setReportMessage(SuvmSeverity.UVM_INFO, "r", "reportTest",
    SuvmVerbosity.UVM_NONE.id, "report", 0, "context")
  m.addInt("foo", 3, 32, SuvmRadix.UVM_HEX)
  m.addString("bar", "hi there")
  m.addObject("myObj", new SuvmFieldOp())

  r.processReportMessage(m)
}