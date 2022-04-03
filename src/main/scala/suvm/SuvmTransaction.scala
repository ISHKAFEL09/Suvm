package suvm

abstract class SuvmTransaction(initiator: Option[SuvmComponent] = None) extends SuvmObject {
  import SuvmImplicits._

  var _initiator: Option[SuvmComponent] = initiator

  def doAcceptTr(): Unit = {}

  def doBeginTr(): Unit = {}

  def doEndTr(): Unit = {}

  final def acceptTr(acceptTime: Time = 0.s): Unit = {
    this.acceptTime = if (acceptTime.value != 0) acceptTime else realtime
    doAcceptTr()
    val e: SuvmEvent = events.get("accept")
    e.trigger() // TODO:
  }

  final def beginTr(beginTime: Time = 0.s, parentHandle: Int = 0): Int =
    mBeginTr(beginTime, parentHandle)

  final def beginChildTr(beginTime: Time = 0.s, parentHandle: Int = 0): Int =
    mBeginTr(beginTime, parentHandle)

  final def endTr(endTime: Time = 0.s, freeHandle: Int = 1): Unit = {
    this.endTime = if (endTime.value == 0) realtime else endTime
    doEndTr()
    if (isRecordingEnabled && trRecorder.nonEmpty) {
      recordObj(trRecorder)
      trRecorder.get.close(this.endTime)
      if (freeHandle != 0) trRecorder.get.free()
    }
    trRecorder = None
    events.get("end").trigger()
  }

  final def getTrHandle: Int = if (trRecorder.isEmpty) 0 else trRecorder.get.getHandle

  final def disableRecording(): Unit = streamHandle = None

  final def enableRecording(trStream: SuvmTrStream): Unit = streamHandle = Some(trStream)

  final def isRecordingEnabled: Boolean = streamHandle.nonEmpty

  final def isActive: Boolean = endTime.value == -1

  final def getEventPool: SuvmEventPool = events

  final def setInitiator(initiator: SuvmComponent): Unit = _initiator = Some(initiator)

  final def getInitiator: Option[SuvmComponent] = _initiator

  final def getAcceptTime: Time = acceptTime

  final def getBeginTime: Time = beginTime

  final def getEndTime: Time = endTime

  final def setTransactionId(id: Int): Unit = mTransactionId = id

  final def getTransactionId: Int = mTransactionId

  override def doPrint(printer: SuvmPrinter): Unit = {
    // TODO
  }

  override def doRecord(recorder: SuvmRecorder): Unit = {
    // TODO
  }

  override def doCopy(rhs: SuvmObject): Unit = {
    // TODO
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
