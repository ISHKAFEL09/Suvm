package uvm

class UVMSequenceBase(name: String) extends UVMSequenceItem(name) {

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
}
