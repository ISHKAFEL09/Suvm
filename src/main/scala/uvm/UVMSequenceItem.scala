package uvm

class UVMSequenceItem(name: String) extends UVMTransaction(name, None) {
  protected var mSequenceID: Int = -1
  protected var mTransactionID: Int = -1

  protected var mSequencer: Option[UVMSequencerBase] = None

  def setSequenceID(i: Int): Unit = mSequenceID = i

  def setSequencer(sqr: UVMSequencerBase): Unit = mSequencer = Some(sqr)

  def getSequencer: UVMSequencerBase = mSequencer.get

  def getSequenceID: Int = mSequenceID

  def getTransactionID: Int = mTransactionID
}
