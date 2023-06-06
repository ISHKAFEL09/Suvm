package uvm

abstract class UVMSequenceBase(name: String) extends UVMSequenceItem(name) {

  private val mSqrSeqIDs = collection.mutable.HashMap.empty[Int, Int]

  def mGetSqrSequenceID(sqrID: Int, update: Boolean): Int = {
    val i = mSqrSeqIDs.getOrElse(sqrID, -1)
    if (update) setSequenceID(i)
    i
  }

  def mSetSqrSequenceID(sqrID: Int, seqID: Int): Unit = {
    mSqrSeqIDs(sqrID) = seqID
    setSequenceID(seqID)
  }

  def body(): Unit

  def start(sqr: UVMSequencerBase): Unit = {
    setSequencer(sqr)
    body()
  }

  def startItem(item: UVMSequenceItem, pri: Int = -1, sqr: Option[UVMSequencerBase] = None): Unit = {
    val sequencer = sqr.getOrElse(mSequencer.get)
    item.setSequencer(sequencer)
    sequencer.waitForGrant(this, pri)
  }

  def finishItem(item: UVMSequenceItem, pri: Int = -1): Unit = {
    val sqr = item.getSequencer
    sqr.sendRequest(this, item)
    sqr.waitForItemDone(this, -1)
  }
}
