package uvm

class UVMSeqItemPort[REQ, RSP](name: String, parent: Option[UVMComponent])
  extends UVMPortBase[REQ](name, parent, ENUM_PORT_TYPE.UVM_PORT)
    with UVMTlmSeqBase[REQ, RSP] {

  override def getTypeName: String = "UVMSeqItemPort"

  override def getNextItem: REQ =
    getIF().asInstanceOf[UVMTlmSeqBase[REQ, RSP]].getNextItem

  override def itemDone(rsp: Option[RSP] = None): Unit =
    getIF().asInstanceOf[UVMTlmSeqBase[REQ, RSP]].itemDone(rsp)
}

class UVMSeqItemImp[REQ, RSP](name: String, agent: UVMTlmSeqBase[REQ, RSP])
  extends UVMPortBase[REQ](name, None, ENUM_PORT_TYPE.UVM_IMPLEMENTATION)
    with UVMTlmSeqBase[REQ, RSP] {

  override def getTypeName: String = "UVMSeqItemImp"

  override def getNextItem: REQ = agent.getNextItem

  override def itemDone(rsp: Option[RSP]): Unit = agent.itemDone(rsp)
}
