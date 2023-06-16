package uvm

import chiter._

class UVMSequencer[REQ <: UVMSequenceItem, RSP](name: String, parent: Option[UVMComponent])
  extends UVMSequencerBase(name, parent) with UVMTlmSeqBase[REQ, RSP] {

  private var sequenceItemRequested = false
  private var getNextItemCalled = false

  val seqItemExport = new UVMSeqItemImp[REQ, RSP]("seqItemExport", this)

  private val reqQueue = collection.mutable.Queue.empty[REQ]

  override def getNextItem: REQ = {
    if (getNextItemCalled)
      uvmError(getFullName, "Get_next_item called twice without item_done or get in between")

    if (!sequenceItemRequested)
      mSelectSequence()

    sequenceItemRequested = true
    getNextItemCalled = true
    uvmInfo(getTypeName, s"waiting item @ ${time()}", UVM_NONE)
    ~>(reqQueue.nonEmpty)
    uvmInfo(getTypeName, s"waiting item done @ ${time()}", UVM_NONE)
    reqQueue.head
  }

  override def itemDone(rsp: Option[RSP]): Unit = {
    sequenceItemRequested = false
    getNextItemCalled = false

    // TODO:

    assert(reqQueue.nonEmpty)
    val t = reqQueue.dequeue()
    mWaitForItemSequenceID = t.getSequenceID
    mWaitForItemTransactionID = t.getTransactionID
  }

  override def sendRequest[T <: UVMSequenceItem](seq: UVMSequenceBase, t: T): Unit = {
    t.setSequenceID(seq.mGetSqrSequenceID(mSequencerID, update = true))
    reqQueue.enqueue(t.asInstanceOf[REQ])
    uvmInfo(getTypeName, s"reqQueue empty: ${reqQueue.isEmpty} @${time()}", UVM_NONE)
  }
}
