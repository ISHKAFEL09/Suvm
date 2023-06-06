package uvm

trait UVMTlmIfBase[T1] {
  def write(t: T1): Unit
}

trait UVMTlmSeqBase[REQ, RSP] {
  def getNextItem: REQ

  def itemDone(rsp: Option[RSP]): Unit
}

trait UVMTlmIfExtend[T1, T2] {
  def put(t: T1): Unit

  def get: T2

  def peek: T2

  def tryPut(t: T1): Boolean

  def canPut: Boolean

  def tryGet: (Boolean, T2)

  def canGet: Boolean

  def tryPeek: (Boolean, T2)

  def canPeek: Boolean
}