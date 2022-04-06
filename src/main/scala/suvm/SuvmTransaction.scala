package suvm

import SuvmImplicits._
import SuvmObjectGlobals._

/**
 * root base class for SUVM transactions.
 * Inheriting all the methods of SuvmObject, SuvmTransaction adds a timing and recording interface.
 */
abstract class SuvmTransaction(val name: String = "", initiator: Option[SuvmComponent] = None) extends SuvmObject {
  var _initiator: Option[SuvmComponent] = initiator

  /**
   * Calling accept_tr indicates the transaction item has been received by a consumer component
   */
  def acceptTr(acceptTime: Time = 0.s): Unit = {
    this.acceptTime = if (acceptTime.value != 0) acceptTime else realtime
    doAcceptTr()
    events.get("accept").trigger() // TODO:
  }

  def doAcceptTr(): Unit = {}

  /**
   * indicates the transaction has been started.
   * Generally, a consumer component begins execution of a transactions it receives.
   */
  def beginTr(beginTime: Time = 0.s, parentHandle: Int = 0): Int = mBeginTr(beginTime, parentHandle)

//  def beginChildTr(beginTime: Time = 0.s, parentHandle: Int = 0): Int = mBeginTr(beginTime, parentHandle)

  def doBeginTr(): Unit = {}

  /**
   * indicates the transaction execution has ended
   */
  def endTr(endTime: Time = 0.s, freeHandle: Boolean = true): Unit = {
    this.endTime = if (endTime.value == 0) realtime else endTime
    doEndTr()
    if (isRecordingEnabled && trRecorder.nonEmpty) {
      recordObj(trRecorder)
      trRecorder.get.close(this.endTime)
      if (freeHandle) trRecorder.get.free()
    }
    trRecorder = None
    events.get("end").trigger()
  }

  def doEndTr(): Unit = {}

  def getTrHandle: Int = if (trRecorder.isEmpty) 0 else trRecorder.get.getHandle

  def disableRecording(): Unit = streamHandle = None

  def enableRecording(trStream: SuvmTrStream): Unit = streamHandle = Some(trStream)

  def isRecordingEnabled: Boolean = streamHandle.nonEmpty

  def isActive: Boolean = endTime.value == -1

  def getEventPool: SuvmEventPool = events

  def setInitiator(initiator: SuvmComponent): Unit = _initiator = Some(initiator)

  def getInitiator: Option[SuvmComponent] = _initiator

  def getAcceptTime: Time = acceptTime

  def getBeginTime: Time = beginTime

  def getEndTime: Time = endTime

  def setTransactionId(id: Int): Unit = mTransactionId = id

  def getTransactionId: Int = mTransactionId

  /**
   * Internal methods properties; do not use directly
   */
  override def doPrint(printer: Some[SuvmPrinter]): Unit = {
    if (printer.nonEmpty) {
      val p = printer.get
      if (acceptTime.value == -1) p.printTime("accept_time", acceptTime)
      if (beginTime.value == -1) p.printTime("begin_time", beginTime)
      if (endTime.value == -1) p.printTime("end_time", endTime)
      if (_initiator.nonEmpty) {
        val str = s"@${_initiator.get.getInstId}"
        p.printGeneric("initiator", _initiator.get.getTypeName, -1, str)
      }
    }
  }

  override def doRecord(recorder: Some[SuvmRecorder]): Unit = {
    if (recorder.nonEmpty) {
      val r = recorder.get
      if (acceptTime.value != -1)
        r.recordField("accept_time", acceptTime, acceptTime.bitLength, SuvmRadixEnum.UVM_TIME)
      if (_initiator.nonEmpty) {
        val p = r.getRecursionPolicy
        r.setRecursionPolicy(SuvmRecursionPolicy.UVM_REFERENCE)
        r.recordObject("initiator", _initiator.get)
        r.setRecursionPolicy(p)
      }
    }
  }

  override def doCopy(rhs: Some[SuvmObject]): Unit = {
    if (rhs.nonEmpty) {
      rhs.get match {
        case t: SuvmTransaction =>
          acceptTime = t.acceptTime
          beginTime = t.beginTime
          endTime = t.endTime
          _initiator = t._initiator
          streamHandle = t.streamHandle
          trRecorder = t.trRecorder
        case _ =>
      }
    }
  }

  private def mBeginTr(beginTime: Time = 0.s, parentHandle: Int = 0): Int = {
    val tmpTime: Time = if (beginTime.value == 0) realtime else beginTime
    val parentRecorder = SuvmRecorder.getRecorderFromHandle(parentHandle)
    if (trRecorder.nonEmpty) endTr(tmpTime)
    val ret = if (isRecordingEnabled) {
      val db: SuvmTrDatabase = streamHandle.get.getDb
      this.endTime = (-1).s
      this.beginTime = tmpTime
      this.trRecorder = parentRecorder match {
        case None => streamHandle.get.openRecorder(getTypeName, this.beginTime, "Begin_No_Parent, Link")
        case Some(r) =>
          val tr = streamHandle.get.openRecorder(getTypeName, this.beginTime, "Begin_End, Link")
          if (tr.nonEmpty) db.establishLink(SuvmParentChildLink.getLink(r, tr.get))
          tr
      }
      if (trRecorder.isEmpty) 0 else trRecorder.get.getHandle
    } else {
      this.trRecorder = None
      this.endTime = (-1).s
      this.beginTime = tmpTime
      0
    }
    doBeginTr()
    events.get("begin").trigger()
    ret
  }

  private val events = new SuvmEventPool("events")
  private var mTransactionId: Int = -1
  private var beginTime: Time = (-1).s
  private var endTime: Time = (-1).s
  private var acceptTime: Time = (-1).s
  private var streamHandle: Option[SuvmTrStream] = None
  private var trRecorder: Option[SuvmRecorder] = None
}

object SuvmTransactionTest extends App {
  val t = new SuvmTransaction(initiator = Some(new SuvmComponent {
    override val name: String = "component"
  }) ) {}

  t.doPrint(Some(new SuvmPrinter {
    override val name: String = "printer"
  }))

  val r = new SuvmRecorder {
    override val name: String = "recorder"
  }

  t.doRecord(Some(r))
  println(r.getRecursionPolicy)

//  t.beginTime = 3.s
//
//  val c = new SuvmTransaction() {}
//  c.doCopy(Some(t))
//  println(c.getInitiator)
//  println(c.beginTime)
//  c.doPrint(Some(new SuvmPrinter {
//    override val name: String = "printer"
//  }))
}