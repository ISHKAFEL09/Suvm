package rocket2.agents.tlbreq

import agents.decouple._
import rocket2.TLBReq
import uvm._

class TLBReqMonitor(name: String, parent: UVMComponent, bus: TLBReqIF)
  extends DecoupleMonitor[TLBReqItem, TLBReq](name, Some(parent), bus.clk, bus) {

  override def monitor(): TLBReqItem = TLBReqItem(
    "tlbReqItem",
    vpn = bus.bits.vpn.peek().litValue.toInt,
    asid = bus.bits.asid.peek().litValue.toInt,
    passthrough = bus.bits.passthrough.peek().litToBoolean,
    instruction = bus.bits.instruction.peek().litToBoolean,
    store = bus.bits.store.peek().litToBoolean
  )

  override def write(t: TLBReqItem): Unit = {
    uvmInfo(getTypeName, s"Monitor got item: $t\n", UVM_NONE)
  }
}
