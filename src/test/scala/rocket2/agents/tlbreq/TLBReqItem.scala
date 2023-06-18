package rocket2.agents.tlbreq

import uvm._

case class TLBReqItem(itemName: String,
                      asid: Int,
                      vpn: Int,
                      passthrough: Boolean,
                      instruction: Boolean,
                      store: Boolean) extends UVMSequenceItem(itemName) {
  override def toString: String =
    s"$itemName(asid: $asid, vpn: $vpn, passthrough: $passthrough, instruction: $instruction, store: $store)"
}
