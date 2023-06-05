package uvm

class UVMSequenceItem(name: String) extends UVMTransaction(name, None) {
  protected var mSequenceID: Int = -1

  def setSequenceID(i: Int): Unit = mSequenceID = i

  def getSequenceID: Int = mSequenceID
}
