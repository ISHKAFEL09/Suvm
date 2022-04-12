package suvm

import SuvmObjectGlobals._
import SuvmImplicits._

import java.io.File

/**
 * Base class for report message element. Defines common interface.
 */
sealed abstract class SuvmReportMessageElementBase {
  var _action: SuvmAction.Value = _
  var _name: String = _

  def getName: String = _name

  def setName(name: String): Unit = _name = name

  def getAction: SuvmAction.Value = _action

  def setAction(action: SuvmAction.Value): Unit = _action = action

  def rPrint(printer: SuvmPrinter): Unit = {
    if (_action.hasOp(SuvmAction.UVM_LOG | SuvmAction.UVM_DISPLAY))
      doPrint(printer)
  }

  def rRecord(recorder: SuvmRecorder): Unit = {
    if (_action.hasOp(SuvmAction.UVM_RM_RECORD))
      doRecord(recorder)
  }

  def rCopy(rhs: SuvmReportMessageElementBase): Unit = doCopy(rhs)

  def rClone(): SuvmReportMessageElementBase = doClone()

  def doPrint(printer: SuvmPrinter): Unit

  def doRecord(recorder: SuvmRecorder): Unit

  def doCopy(rhs: SuvmReportMessageElementBase): Unit

  def doClone(): SuvmReportMessageElementBase
}

/**
 * Get or set the value (integral type) of the element, with size and radix
 */
class SuvmReportMessageIntElement extends SuvmReportMessageElementBase {
  override def doPrint(printer: SuvmPrinter): Unit = printer.printField(_name, _val, _size, _radix)

  override def doRecord(recorder: SuvmRecorder): Unit = recorder.recordField(_name, _val, _size, _radix)

  override def doCopy(rhs: SuvmReportMessageElementBase): Unit = rhs match {
    case element: SuvmReportMessageIntElement =>
      _name = element._name
      _val = element._val
      _size = element._size
      _radix = element._radix
      _action = element._action
    case _ =>
  }

  override def doClone(): SuvmReportMessageElementBase = {
    val tmp = new SuvmReportMessageIntElement
    tmp.rCopy(this)
    tmp
  }

  def getValue: (Int, SuvmRadix.Value, SuvmBitstream) = (_size, _radix, _val)

  def setValue(value: SuvmBitstream, size: Int, radix: SuvmRadix.Value): Unit = {
    _size = size
    _radix = radix
    _val = value
  }

  private var _val: SuvmBitstream = _
  private var _size: Int = _
  private var _radix: SuvmRadix.Value = _
}

/**
 * Message element class for object type
 */
class SuvmReportMessageObjectElement extends SuvmReportMessageElementBase {
  override def doPrint(printer: SuvmPrinter): Unit = printer.printObject(_name, _val)

  override def doRecord(recorder: SuvmRecorder): Unit = recorder.recordObject(_name, _val)

  override def doCopy(rhs: SuvmReportMessageElementBase): Unit = rhs match {
    case element: SuvmReportMessageObjectElement =>
      _name = element._name
      _val = element._val
      _action = element._action
    case _ =>
  }

  override def doClone(): SuvmReportMessageElementBase = {
    val tmp = new SuvmReportMessageObjectElement
    tmp.rCopy(this)
    tmp
  }

  def getValue: SuvmObject = _val

  def setValue(value: SuvmObject): Unit = _val = value

  private var _val: SuvmObject = _
}

/**
 * Get or set the value (string type) of the element
 */
class SuvmReportMessageStringElement extends SuvmReportMessageElementBase {
  override def doPrint(printer: SuvmPrinter): Unit = printer.printString(_name, _val)

  override def doRecord(recorder: SuvmRecorder): Unit = recorder.recordString(_name, _val)

  override def doCopy(rhs: SuvmReportMessageElementBase): Unit = rhs match {
    case element: SuvmReportMessageStringElement =>
      _name = element._name
      _val = element._val
      _action = element._action
    case _ =>
  }

  override def doClone(): SuvmReportMessageElementBase = {
    val tmp = new SuvmReportMessageStringElement
    tmp.rCopy(this)
    tmp
  }

  def getValue: String = _val

  def setValue(value: String): Unit = _val = value

  private var _val: String = _
}

/**
 * A container used by report message to contain the dynamically added elements,
 * with APIs to add and delete the elements.
 */
class SuvmReportMessageElementContainer(val name: String = "ElementContainer") extends SuvmObject {
  import SuvmAction._

  override def doPrint(printer: SuvmPrinter): Unit = elements foreach(_.rPrint(printer))

  override def doRecord(recorder: SuvmRecorder): Unit = elements foreach(_.rRecord(recorder))

  override def doCopy(rhs: SuvmObject): Unit = rhs match {
    case container: SuvmReportMessageElementContainer =>
      deleteElements()
      container.getElements.foreach(i => elements.enqueue(i.rClone()))
  }

  def size: Int = elements.size

  def delete(index: Int): Unit = elements.remove(index)

  def deleteElements(): Unit = elements.removeAll()

  def getElements: List[SuvmReportMessageElementBase] = elements.toList

  /**
   *  This method adds an integral type of the name ~name~ and value ~value~ to
   *  the container.  The required ~size~ field indicates the size of ~value~.
   *  The required ~radix~ field determines how to display and
   *  record the field. The optional print/record bit is to specify whether
   *  the element will be printed/recorded.
   */
  def addInt(name: String, value: SuvmBitstream, size: Int, radix: SuvmRadix.Value,
             action: SuvmAction.Value = UVM_LOG | UVM_RM_RECORD): Unit = {
    val element = new SuvmReportMessageIntElement
    element.setName(name)
    element.setValue(value, size, radix)
    element.setAction(action)
    elements.enqueue(element)
  }

  /**
   *   This method adds a string of the name ~name~ and value ~value~ to the
   *   message. The optional print/record bit is to specify whether
   *   the element will be printed/recorded.
    */
  def addString(name: String, value: String, action: SuvmAction.Value = UVM_LOG | UVM_RM_RECORD): Unit = {
    val element = new SuvmReportMessageStringElement
    element.setName(name)
    element.setValue(value)
    element.setAction(action)
    elements.enqueue(element)
  }

  /**
   * This method adds a uvm_object of the name ~name~ and reference ~obj~ to
   * the message. The optional print/record bit is to specify whether
   * the element will be printed/recorded.
   */
  def addObject(name: String, obj: SuvmObject, action: SuvmAction.Value = UVM_LOG | UVM_RM_RECORD): Unit = {
    val element = new SuvmReportMessageObjectElement
    element.setName(name)
    element.setValue(obj)
    element.setAction(action)
    elements.enqueue(element)
  }

  private val elements = collection.mutable.Queue.empty[SuvmReportMessageElementBase]
}

object SuvmReportMessageElementContainer extends SuvmObjectUtils[SuvmReportMessageElementContainer] {
  override def create = new SuvmReportMessageElementContainer(_)
}

/**
 * The uvm_report_message is the basic UVM object message class.
 * It provides the fields that are common to all messages.
 */
class SuvmReportMessage(val name: String = "SuvmReportMessage") extends SuvmObject {
  override def doPrint(printer: SuvmPrinter): Unit = {
    printer.printGeneric("severity", "SuvmSeverity", _severity.id.bitLength, _severity.toString)
    printer.printString("id", _id)
    printer.printString("message", _message)
    SuvmVerbosity.values.find(_.id == _verbosity) match {
      case Some(value) =>
        printer.printGeneric("verbosity", "SuvmVerbosity", value.id.bitLength, value.toString)
      case None =>
    }
    printer.printField("verbosity", _verbosity, _verbosity.bitLength, SuvmRadix.UVM_HEX)
    printer.printString("filename", _filename)
    printer.printField("line", _line, _line.bitLength, SuvmRadix.UVM_UNSIGNED)
    printer.printString("contextName", _contextName)
    _reportMessageElementContainer.getElements.foreach(_.rPrint(printer))
  }

  /**
   * infrastructure references
   */
  def getReportObject: SuvmReportObject = _reportObject

  def setReportObject(ro: SuvmReportObject): Unit = _reportObject = ro

  def getReportHandle: SuvmReportHandler = _reportHandle

  def setReportHandle(rh: SuvmReportHandler): Unit = _reportHandle = rh

  def getReportServer: SuvmReportServer = _reportServer

  def setReportServer(rs: SuvmReportServer): Unit = _reportServer = rs

  /**
   * message fields
   */
  def getSeverity: SuvmSeverity.Value = _severity

  def setSeverity(sev: SuvmSeverity.Value): Unit = _severity = sev

  def getId: String = _id

  def setId(id: String): Unit = _id = id

  def getMessage: String = _message

  def setMessage(msg: String): Unit = _message = msg

  def getVerbosity: Int = _verbosity

  def setVerbosity(ver: Int): Unit = _verbosity = ver

  def getFilename: String = _filename

  def setFilename(fname: String): Unit = _filename = fname

  def getLine: Int = _line

  def setLine(ln: Int): Unit = _line = ln

  def getContext: String = _contextName

  def setContext(cn: String): Unit = _contextName = cn

  def getAction: Int = _action

  def setAction(act: Int): Unit = _action = act

  def getFile: Option[File] = _file

  def setFile(fl: Option[File]): Unit = _file = fl

  def setReportMessage(severity: SuvmSeverity.Value, id: String, message: String,
                       verbosity: Int, filename: String, line: Int, contextName: String): Unit = {
    _contextName = contextName
    _filename = filename
    _line = line
    _severity = severity
    _id = id
    _message = message
    _verbosity = verbosity
  }

  def getElementContainer: SuvmReportMessageElementContainer = _reportMessageElementContainer

  import SuvmAction._
  def addInt(name: String, value: SuvmBitstream, size: Int, radix: SuvmRadix.Value,
             action: SuvmAction.Value = UVM_LOG | UVM_RM_RECORD): Unit =
    _reportMessageElementContainer.addInt(name, value, size, radix, action)

  def addString(name: String, value: String, action: SuvmAction.Value = UVM_LOG | UVM_RM_RECORD): Unit =
    _reportMessageElementContainer.addString(name, value, action)

  def addObject(name: String, obj: SuvmObject, action: SuvmAction.Value = UVM_LOG | UVM_RM_RECORD): Unit =
    _reportMessageElementContainer.addObject(name, obj, action)

  /**
   * not documented
   */
  override def doRecord(recorder: SuvmRecorder): Unit = {
    mRecordCoreProperties(recorder)
    _reportMessageElementContainer.recordObj(Some(recorder))
  }

  private def mRecordMessage(recorder: SuvmRecorder): Unit = recorder.recordString("Message", _message)

  private def mRecordCoreProperties(recorder: SuvmRecorder): Unit = {
    if (_contextName.nonEmpty) recorder.recordString("ContextName", _contextName)
    recorder.recordString("Filename", _filename)
    recorder.recordField("Line", _line, _line.bitLength, SuvmRadix.UVM_UNSIGNED)
    recorder.recordString("Severity", _severity.toString)
    SuvmVerbosity.find(_verbosity) match {
      case Some(value) => recorder.recordString("Verbosity", value.toString)
      case None => recorder.recordString("Verbosity", _verbosity.toString)
    }
    recorder.recordString("Id", _id)
    mRecordMessage(recorder)
  }

  private var _reportObject: SuvmReportObject = _
  private var _reportHandle: SuvmReportHandler = _
  private var _reportServer: SuvmReportServer = _
  private var _severity: SuvmSeverity.Value = _
  private var _id: String = _
  private var _message: String = _
  private var _verbosity: Int = _
  private var _filename: String = _
  private var _line: Int = _
  private var _contextName: String = _
  private var _action: Int = _
  private var _file: Option[File] = _
  private val _reportMessageElementContainer = new SuvmReportMessageElementContainer()
}

object SuvmReportMessage extends SuvmObjectUtils[SuvmReportMessage] {
  override def create = new SuvmReportMessage(_)

  // TODO: keep the random stability?
  def newReportMessage(name: String = "SuvmReportMessage") = new SuvmReportMessage(name)
}

object SuvmReportMessageTest extends App {
  val printer = new SuvmPrinter {
    override val name: String = "printer"
  }

  val r = SuvmReportMessage.typeId.create("reportMessage")
  r.setReportMessage(SuvmSeverity.UVM_INFO, "r", "reportTest",
    SuvmVerbosity.UVM_NONE.id, "report", 0, "context")
  r.addInt("foo", 3, 32, SuvmRadix.UVM_HEX)
  r.addString("bar", "hi there")
  r.addObject("myObj", new SuvmFieldOp())
  r.doPrint(printer)
}
