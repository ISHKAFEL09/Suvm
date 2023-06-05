package uvm

case class UVMSequenceReq(grant: Boolean,
                          seqID: Int,
                          reqID: Int,
                          pri: Int)
class UVMSequencerBase(name: String, parent: Option[UVMComponent]) extends UVMComponent(name, parent) {
  private val mSequencerID: Int = {
    UVMSequencerBase.sequencerID += 1
    UVMSequencerBase.sequencerID
  }

  private var mLockArbSize: Int = -1

  UVMSequencerBase.allSequencerInsts(mSequencerID) = this

  def waitForGrant(seqPtr: UVMSequenceBase, pri: Int = -1, lockReq: Boolean = false): Unit = {

  }
}

object UVMSequencerBase {
  private var sequencerID: Int = 1
  private var sequenceID: Int = 1
  private var requestID: Int = 1
  private val allSequencerInsts = collection.mutable.HashMap.empty[Int, UVMSequencerBase]
}