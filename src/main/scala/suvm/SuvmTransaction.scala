package suvm

abstract class SuvmTransaction(name: String,
                               private[this] var initiator: Option[SuvmComponent] = None) extends SuvmObject {
  def doAcceptTr(): Unit

  def doBeginTr(): Unit

  def doEndTr(): Unit

  final def acceptTr(acceptTime: Time = 0): Unit = {
    this.acceptTime = if (acceptTime != 0) acceptTime else realtime
    doAcceptTr()
    val e: SuvmEvent[SuvmObject] = events.get("accept")
    e.trigger() // TODO
  }

  final def beginTr(beginTime: Time = 0, parentHandle: Int = 0): Int =
    mBeginTr(beginTime, parentHandle)

  final def beginChildTr(beginTime: Time = 0, parentHandle: Int = 0): Int =
    mBeginTr(beginTime, parentHandle)

  final def endTr(endTime: Time = 0, freeHandle: Int = 1): Int

  final def getTrHandle: Int = if (trRecorder.isEmpty) 0 else trRecorder.get.getHandle

  final def disableRecording(): Unit = streamHandle = None

  final def enableRecording(trStream: SuvmTrStream): Unit = streamHandle = Some(trStream)

  final def isRecordingEnabled: Boolean = streamHandle.nonEmpty

  final def isActive: Boolean = endTime == -1

  final def getEventPool: SuvmEventPool = events

  final def setInitiator(initiator: SuvmComponent): Unit = this.initiator = Some(initiator)

  final def getInitiator: Option[SuvmComponent] = this.initiator

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

  private def mBeginTr(beginTime: Time = 0, parentHandle: Int = 0): Int

  private val events = new SuvmEventPool
  private[this] var mTransactionId: Int = -1
  private[this] var beginTime: Time = -1
  private[this] var endTime: Time = -1
  private[this] var acceptTime: Time = -1
  private[this] var streamHandle: Option[SuvmTrStream] = None
  private[this] var trRecorder: Option[SuvmRecorder] = None
}
