package uvm

abstract class UVMDriver[REQ <: UVMSequenceItem, RSP](name: String, parent: Option[UVMComponent])
  extends UVMComponent(name, parent) {
  val seqItemPort = new UVMSeqItemPort[REQ, RSP]("seqItemPort", Some(this))
}
