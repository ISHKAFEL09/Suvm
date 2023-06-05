package uvm

import chiseltester._
import uvm.UVMSequencerBase.requestID

object ENUM_SEQ_REQ extends Enumeration {
  val SEQ_TYPE_REQ, SEQ_TYPE_LOCK = Value
}

case class UVMSequenceReq(grant: Boolean,
                          seqID: Int,
                          reqID: Int,
                          pri: Int,
                          thread: Option[AbstractTesterThread],
                          request: ENUM_SEQ_REQ.Value,
                          seq: UVMSequenceBase)

class UVMSequencerBase(name: String, parent: Option[UVMComponent]) extends UVMComponent(name, parent) {
  private val mSequencerID: Int = {
    UVMSequencerBase.sequencerID += 1
    UVMSequencerBase.sequencerID
  }

  private val regSequences = collection.mutable.HashMap.empty[Int, UVMSequenceBase]

  private val arbSequenceQueue = collection.mutable.ArrayBuffer.empty[UVMSequenceReq]

  private val arbCompleted = collection.mutable.Set.empty[Int]

  UVMSequencerBase.allSequencerInsts(mSequencerID) = this

  private def mRegisterSequence(seq: UVMSequenceBase): Int = {
    if (seq.mGetSqrSequenceID(mSequencerID, update = true) < 0) {
      UVMSequencerBase.sequenceID += 1
      seq.mSetSqrSequenceID(mSequencerID, UVMSequencerBase.sequenceID)
      regSequences(seq.getSequenceID) = seq
    }
    seq.getSequenceID
  }

  private def mWaitArb(reqID: Int): Unit = {
    ~>(arbCompleted.contains(reqID))
    arbCompleted -= reqID
  }

  private def mSetArb(reqID: Int): Unit = arbCompleted += reqID

  private def mChooseNextReq: Int = {
    if (arbSequenceQueue.isEmpty) -1
    else 0
  }

  private def mSelectSequence(): Unit = {
    ~>(arbSequenceQueue.nonEmpty)
    mSetArb(arbSequenceQueue.remove(0).reqID)
  }

  def waitForGrant(seq: UVMSequenceBase, pri: Int = -1, lockReq: Boolean = false): Unit = {
    val seqID = mRegisterSequence(seq)

    if (lockReq) {
      UVMSequencerBase.requestID += 1
      arbSequenceQueue += UVMSequenceReq(
        grant = false, seqID, UVMSequencerBase.requestID, pri, current, ENUM_SEQ_REQ.SEQ_TYPE_LOCK, seq)
    }

    UVMSequencerBase.requestID += 1
    arbSequenceQueue += UVMSequenceReq(
      grant = false, seqID, UVMSequencerBase.requestID, pri, current, ENUM_SEQ_REQ.SEQ_TYPE_REQ, seq)

    mWaitArb(UVMSequencerBase.requestID)
  }
}

object UVMSequencerBase {
  private var sequencerID: Int = 0
  private var sequenceID: Int = 0
  private var requestID: Int = 0
  private val allSequencerInsts = collection.mutable.HashMap.empty[Int, UVMSequencerBase]
}